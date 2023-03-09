{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

/// <summary>Unit that holds common functionality relative to a generic URL Client (HTTP, FTP, ...) </summary>
unit System.Net.URLClient;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Generics.Defaults, System.Generics.Collections,
  System.SysUtils, System.Types, System.Net.Mime;

type
  /// <summary>Generic Exception class for System.Net exceptions</summary>
  ENetException = class(Exception);
  /// <summary>Exception class for Credentials related exceptions.</summary>
  ENetCredentialException = class(ENetException);
  /// <summary>Exception class for URI related exceptions.</summary>
  ENetURIException = class(ENetException);
  /// <summary>Exception class for URI related exceptions.</summary>
  ENetURIClientException = class(ENetException);
  /// <summary>Exception class for URI related exceptions.</summary>
  ENetURIRequestException = class(ENetException);
  /// <summary>Exception class for URI related exceptions.</summary>
  ENetURIResponseException = class(ENetException);

// -------------------------------------------------------------------------------- //
type
  /// <summary>Record to manage a Name-Value pair</summary>
  TNameValuePair = record
    /// <summary>Name part of a Name-Value pair</summary>
    Name: string;
    /// <summary>Value part of a Name-Value pair</summary>
    Value: string;
    /// <summary>Initializes a Name-Value pair</summary>
    constructor Create(const AName, AValue: string);
  end;
  TNameValueArray = TArray<TNameValuePair>;
  //TNameValueArray = array of TNameValuePair;

  /// <summary>Alias for a URI Parameter</summary>
  TURIParameter = TNameValuePair;

  /// <summary> Array of URI Parameters. </summary>
  TURIParameters = TNameValueArray;

  // Forwarded class declaration;
  TURLClient = class;
// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Record type to help compose and decompose a URI from/into its parts. </summary>
  /// <remarks>It is a record to ease its use and avoid manage its life cycle.</remarks>
  TURI = record
  private
    FScheme: string;
    FUsername: string;
    FPassword: string;
    FHost: string;
    FPort: Integer;
    FPath: string;
    FQuery: string;
    FParams: TURIParameters;
    FFragment: string;
  private type
    TEncType = (URLEnc, FormEnc);
  private type
    TSchemeDecomposeProc = procedure(const AURIStr: string;
      Pos, Limit, SlashCount: Integer) of object;
  private
    procedure DecomposeBaseScheme(const AURIStr: string;
      Pos, Limit, SlashCount: Integer);
    procedure DecomposeNoAuthorityScheme(const AURIStr: string;
      Pos, Limit, SlashCount: Integer);
  private
    function IsMailtoScheme: Boolean;
    function IsSchemeNoAuthority: Boolean;
    function IsValidPort: Boolean;
    function GetDefaultPort(const AScheme: string): Integer;

    procedure ParseParams(Encode: Boolean = False);
    function FindParameterIndex(const AName: string): Integer;
    function GetParameter(const I: Integer): TURIParameter;
    function GetParameterByName(const AName: string): string;
    procedure SetParameter(const I: Integer; const Value: TURIParameter);
    procedure SetParameterByName(const AName: string; const Value: string);
    function GetQuery: string;

    /// <summary>Decompose a string into its parts</summary>
    procedure DecomposeURI(const AURIStr: string; ARaiseNoSchema: Boolean);
    procedure SetScheme(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetQuery(const Value: string);
    procedure SetParams(const Value: TURIParameters);
  public const
    SCHEME_HTTP = 'http'; // DO NOT LOCALIZE
    SCHEME_HTTPS = 'https'; // DO NOT LOCALIZE
    SCHEME_MAILTO = 'mailto'; // DO NOT LOCALIZE
    SCHEME_NEWS = 'news'; // DO NOT LOCALIZE
    SCHEME_TEL = 'tel'; // DO NOT LOCALIZE
    SCHEME_URN = 'urn'; // DO NOT LOCALIZE
  public
    /// <summary>Initializes a TURI from a string</summary>
    constructor Create(const AURIStr: string);

    /// <summary>Generate a urlencoded string from the URI parts</summary>
    /// <returns>A string representing the URI</returns>
    function ToString: string;
    /// <summary>Generate a URI from the string parts</summary>
    procedure ComposeURI(const AScheme, AUsername, APassword, AHostname: string; APort: Integer; const APath: string;
      const AParams: TURIParameters; const AFragment: string);
    /// <summary>Adds a Parameter to the URI</summary>
    procedure AddParameter(const AName, AValue: string); overload;
    procedure AddParameter(const AParameter: TURIParameter); overload; inline;
    /// <summary>Removes a Parameter from the URI</summary>
    procedure DeleteParameter(AIndex: Integer); overload;
    procedure DeleteParameter(const AName: string); overload;

    /// <summary>URL percent encoding of a text.</summary>
    /// <param name="AValue">The text to be encoded.</param>
    /// <param name="SpacesAsPlus">If true the spaces are translated as '+' instead %20.</param>
    class function URLEncode(const AValue: string; SpacesAsPlus: Boolean = False): string; static; deprecated 'Use TNetEncoding.URL.Encode';
    /// <summary>URL percent decoding of a text.</summary>
    /// <param name="AValue">The text to be decoded.</param>
    /// <param name="PlusAsSpaces">If true the character '+' is translated as space.</param>
    class function URLDecode(const AValue: string; PlusAsSpaces: Boolean = False): string; static; deprecated 'Use TNetEncoding.URL.Decode';

    function Encode: string;

    /// <summary>Converts a Unicode hostname into it's ASCII equivalent using IDNA</summary>
    class function UnicodeToIDNA(const AHostName: string): string; static;
    /// <summary>Converts a IDNA encoded ASCII hostname into it's Unicode equivalent</summary>
    class function IDNAToUnicode(const AHostName: string): string; static;

    /// <summary>Normalize a relative path using a given URI as base.</summary>
    /// <remarks>This function will remove '.' and '..' from path and returns an absolute URL.</remarks>
    class function PathRelativeToAbs(const RelPath: string; const Base: TURI): string; static;
    /// <summary>
    /// Clean up/fix the given URL. Add a possibly missing protocol (http is default), remove trailing white spaces.
    /// Ensures no trailing slash exists.
    /// </summary>
    /// <example>
    /// <see href="http://www.example.com">www.example.com</see> -&gt; <see href="http://www.example.com/" /><br />
    /// <see href="http://www.example.com/some/path">www.example.com/some/path</see> -&gt;
    /// <see href="http://www.example.com/some/path" />
    /// </example>
    class function FixupForREST(const AURL: string): string; static;

    /// <summary>Property to obtain/set a parameter by it's index</summary>
    property Parameter[const I: Integer]: TURIParameter read GetParameter write SetParameter;
    /// <summary>Property to obtain/set a parameter value by it's Name</summary>
    property ParameterByName[const AName: string]: string read GetParameterByName write SetParameterByName;

    /// <summary>Scheme part of the URI.</summary>
    property Scheme: string read FScheme write SetScheme;
    /// <summary>Username part of the URI.</summary>
    property Username: string read FUsername write SetUserName;
    /// <summary>Password part of the URI.</summary>
    property Password: string read FPassword write SetPassword;
    /// <summary>Host part of the URI.</summary>
    property Host: string read FHost write SetHost;
    /// <summary>Port part of the URI.</summary>
    property Port: Integer read FPort write FPort;
    /// <summary>Path part of the URI.</summary>
    property Path: string read FPath write SetPath;
    /// <summary>Query part of the URI.</summary>
    property Query: string read FQuery write SetQuery;
    /// <summary>Params part of the URI.</summary>
    property Params: TURIParameters read FParams write SetParams;
    /// <summary>Fragment part of the URI.</summary>
    property Fragment: string read FFragment write FFragment;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Enumeration defining different persistences of a credential</summary>
  TAuthPersistenceType = (Request, Client);

  /// <summary>Different types of authentication targets for the credential</summary>
  TAuthTargetType = (Proxy, Server);

  /// <summary>Storage for credentials used in TURLClient and TURLRequest</summary>
  TCredentialsStorage = class
  public type
    /// <summary>Different types of Authorization Schemes</summary>
    TAuthSchemeType = (Basic, Digest, NTLM, Negotiate);

    /// <summary>Callback to request user credentials when server asks for them</summary>
    TCredentialAuthCallback = procedure(const Sender: TObject; AnAuthTarget: TAuthTargetType;
      const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;
      var Persistence: TAuthPersistenceType);
    /// <summary>Callback to request user credentials when server asks for them</summary>
    TCredentialAuthEvent = procedure(const Sender: TObject; AnAuthTarget: TAuthTargetType;
      const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;
      var Persistence: TAuthPersistenceType) of object;

    /// <summary>Record to manage all the data from a single credential.</summary>
    TCredential = record
      /// <summary> Target type for the credential</summary>
      AuthTarget: TAuthTargetType;
      /// <summary> Realm where the credential can be used</summary>
      Realm: string;
      /// <summary> URL that defines the scope of the credential </summary>
      URL: string; // Scope? Comment it...
      /// <summary> UserName of the credential</summary>
      UserName: string;
      /// <summary> Password associated to the UserName </summary>
      Password: string;
      /// <summary>Initializes a Credential.</summary>
      constructor Create(AnAuthTarget: TAuthTargetType; const ARealm, AURL, AUserName, APassword: string);
      /// <summary> Returns True when both Password and UserName are empty </summary>
      function IsEmpty: Boolean;
    end;
    /// <summary>Type alias for an array of TCredential</summary>
    TCredentialArray = TArray<TCredential>;

    /// <summary>Comparer class used to sort credentials</summary>
    TCredentialComparer = class(TComparer<TCredential>)
    public
      /// <summary>Function that compares two credentials</summary>
      function Compare(const Left, Right: TCredential): Integer; override;
    end;
    ICredComparer = IComparer<TCredential>;

  private
    function GetCredentials: TCredentialArray; inline;
  protected
    /// <summary>List that stores current instance credentials</summary>
    FCredentials: TList<TCredential>;
  protected
    class var
    /// <summary>Global comparer instance used to sort credentials</summary>
    FCredComparer: TCredentialComparer;
  public
    /// <summary>Initializes class Variables</summary>
    class constructor Create;
    /// <summary>Cleanup class Variables</summary>
    class destructor Destroy;

    /// <summary>Creates a credential storage</summary>
    constructor Create;
    destructor Destroy; override;

    /// <summary>Clears the credentials stored</summary>
    procedure ClearCredentials;

    /// <summary>Adds a credential to the credential storage.</summary>
    /// <param name="ACredential"> Credential to be added.</param>
    /// <returns>True if the Add operation was successful.</returns>
    function AddCredential(const ACredential: TCredential): Boolean;

    /// <summary>Removes a credential from the credential storage.</summary>
    /// <returns>True if the Remove operation was successful.</returns>
    function RemoveCredential(AnAuthTargetType: TAuthTargetType; const ARealm: string; const AURL: string = '';
      const AUser: string = ''): Boolean;

    /// <summary>Finds credentials inside the storage filtered by the parameters</summary>
    function FindCredentials(AnAuthTargetType: TAuthTargetType; const ARealm: string; const AURL: string = '';
      const AUser: string = ''): TCredentialArray;

    /// <summary>Finds the fittest credential inside the storage filtered by the parameters</summary>
    function FindAccurateCredential(AnAuthTargetType: TAuthTargetType; const ARealm: string; const AURL: string = '';
      const AUser: string = ''): TCredential;

    /// <summary>Property to access the credentials of the credential storage</summary>
    property Credentials: TCredentialArray read GetCredentials;

    /// <summary>Sorts the given credentials array.</summary>
    class function SortCredentials(const ACredentials: TCredentialArray): TCredentialArray;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Stores the settings of a proxy to be used by the URLClient</summary>
  TProxySettings = record
  private
    FHost: string;
    FPort: Integer;
    FScheme: string;
    FUserName: string;
    FPassword: string;
    FCredential: TCredentialsStorage.TCredential;
    function GetCredential: TCredentialsStorage.TCredential;
  public
    /// <summary>Creates the proxy settings from the given arguments.</summary>
    constructor Create(const AURL: string); overload;
    constructor Create(const AHost: string; APort: Integer; const AUserName: string = ''; const APassword: string = ''; const AScheme: string = ''); overload;

    /// <summary> Host to be used as a proxy</summary>
    property Host: string read FHost write FHost;
    /// <summary> Port used to communicate to the proxy</summary>
    property Port: Integer read FPort write FPort;
    /// <summary> Scheme to be used with the proxy</summary>
    property Scheme: string read FScheme write FScheme;
    /// <summary> UserName needed to be authenticated to the proxy</summary>
    property UserName: string read FUserName write FUserName;
    /// <summary> PassWord needed to be authenticated to the proxy</summary>
    property Password: string read FPassword write FPassword;
    /// <summary> Credential information constructed with the ProxySettings</summary>
    property Credential: TCredentialsStorage.TCredential read GetCredential;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Record to manage a Name-Value pair</summary>
  TNetHeader = TNameValuePair;

  /// <summary>Array of Name-Value pairs</summary>
  TNetHeaders = TNameValueArray;

  /// <summary>This class encapsulates the URL custom headers and provides
  /// fluent style API to manage the header values.</summary>
  TURLHeaders = class(TPersistent)
  public type
    TEnumerator = record
    private
      FHeaders: TURLHeaders;
      FIndex: Integer;
      function GetCurrent: TNetHeader; inline;
      procedure Create(const AHeaders: TURLHeaders);
    public
      function MoveNext: Boolean; inline;
      property Current: TNetHeader read GetCurrent;
    end;

    /// <summary>This class encapsulates the THeaderValueList instance and provides
    /// fluent style API to build Authorization and similar headers. </summary>
    TValueList = class(TObject)
    private
      FList: THeaderValueList;
      FHeaders: TURLHeaders;
      FName: string;
      constructor Create(AHeaders: TURLHeaders; const AName: string);
    public
      destructor Destroy; override;
      /// <summary>Sets subject of header value</summary>
      function SetSubject(const AValue: string): TValueList; inline;
      /// <summary>Clear current header value</summary>
      function Clear: TValueList; inline;
      /// <summary>Deletes header value item with specified AIndex</summary>
      function Delete(AIndex: Integer): TValueList; inline;
      /// <summary>Adds header value item with specified AName</summary>
      function Add(const AName: string): TValueList; overload; inline;
      /// <summary>Adds header value item with specified AName and AValue</summary>
      function Add(const AName, AValue: string; AQuoteVal: Boolean = True): TValueList; overload; inline;
      /// <summary>Finishes editing of header value and destroys this THeaderValueList instance</summary>
      function &End: TURLHeaders; inline;
      /// <summary>Reference to wrapped THeaderValueList instance</summary>
      property List: THeaderValueList read FList;
    end;

    /// <summary>This class encapsulates the TAcceptValueList instance and provides
    /// fluent style API to build Accept and similar headers. </summary>
    TAcceptList = class(TObject)
    private
      FList: TAcceptValueList;
      FHeaders: TURLHeaders;
      FName: string;
      constructor Create(AHeaders: TURLHeaders; const AName: string);
    public
      destructor Destroy; override;
      /// <summary>Clear current header value</summary>
      function Clear: TAcceptList; inline;
      /// <summary>Deletes header value item with specified AIndex</summary>
      function Delete(AIndex: Integer): TAcceptList; inline;
      /// <summary>Adds header value item with specified AName and AWeight</summary>
      function Add(const AName: string; AWeight: Double = 1; AExtra: TStrings = nil): TAcceptList; inline;
      /// <summary>Finishes editing of header value and destroys this TAcceptList instance</summary>
      function &End: TURLHeaders; inline;
      /// <summary>Reference to wrapped TAcceptValueList instance</summary>
      property List: TAcceptValueList read FList;
    end;

  private
    FHeaders: TNetHeaders;
    function GetCount: Integer;
    procedure CheckRange(AIndex: Integer);
    function GetNames(AIndex: Integer): string;
    function GetValues(AIndex: Integer): string;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    function ToString: string; override;
    /// <summary>Assigns ASource to headers collection</summary>
    procedure Assign(ASource: TPersistent); overload; override;
    /// <summary>Assigns AHeaders to headers collection</summary>
    procedure Assign(const AHeaders: TNetHeaders); reintroduce; overload;
    /// <summary>Finds a header by the AName and returns it index</summary>
    function FindItem(const AName: string): Integer;
    /// <summary>Adds new AHeader to headers collection</summary>
    function Add(const AHeader: TNetHeader): TURLHeaders; overload;
    /// <summary>Adds new AHeader to headers collection</summary>
    function Add(const AName, AValue: string): TURLHeaders; overload;
    /// <summary>Appends AHeaders to headers collection</summary>
    function Append(const AHeaders: TNetHeaders): TURLHeaders; overload;
    /// <summary>Appends AHeaders to headers collection</summary>
    function Append(const AHeaders: TURLHeaders): TURLHeaders; overload;
    /// <summary>Clear headers collection</summary>
    function Clear: TURLHeaders;
    /// <summary>Delete a header by the index from headers collection</summary>
    function Delete(AIndex: Integer): TURLHeaders; overload;
    /// <summary>Delete a header by the name from headers collection</summary>
    function Delete(const AName: string): TURLHeaders; overload;
    /// <summary>Returns TValueList object associated with the header by it name.
    /// Corresponding header will be updated when TValueList is destroyed. This
    /// may be done explicitly or by TValueList.&End call. Use method to build
    /// Authorization and similar headers.</summary>
    function ValueList(const AName: string): TValueList;
    /// <summary>Returns TAcceptList object associated with the header by it name.
    /// Corresponding header will be updated when TAcceptList is destroyed. This
    /// may be done explicitly or by TAcceptList.&End call. Use method to build
    /// Accept and similar headers.</summary>
    function AcceptList(const AName: string): TAcceptList;
    /// <summary>Returns enumerator for headers collection</summary>
    function GetEnumerator: TEnumerator; inline;
    /// <summary>Returns dynamic array representing headers collection</summary>
    property Headers: TNetHeaders read FHeaders;
    /// <summary>Returns number of headers in headers collection</summary>
    property Count: Integer read GetCount;
    /// <summary>Returns header name by the index</summary>
    property Names[AIndex: Integer]: string read GetNames;
    /// <summary>Returns header value by the index</summary>
    property Values[AIndex: Integer]: string read GetValues;
    /// <summary>Gets or sets header value by the name</summary>
    property Value[const AName: string]: string read GetValue write SetValue; default;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>This class encapsulates the URL request and all of it's parameters</summary>
  IURLRequest = interface(IInterface)
    /// <summary>Getter for the Credential property</summary>
    function GetCredential: TCredentialsStorage.TCredential;
    /// <summary>Setter for the Credential property</summary>
    procedure SetCredential(const ACredential: TCredentialsStorage.TCredential); overload;
    procedure SetCredential(const AUserName, APassword: string); overload;
    /// <summary>Property that exposes the Credential object that has the URLRequest object</summary>
    property Credential: TCredentialsStorage.TCredential read GetCredential write SetCredential;

    /// <summary>Getter for the URL property</summary>
    function GetURL: TURI;
    /// <summary>Setter for the URL property</summary>
    procedure SetURL(const AValue: TURI);
    /// <summary>Property that exposes the URI object that generated the current URLRequest object</summary>
    property URL: TURI read GetURL write SetURL;

    /// <summary>Getter for the Method property</summary>
    function GetMethodString: string;
    /// <summary>Setter for the Method property</summary>
    procedure SetMethodString(const AValue: string);
    /// <summary>The request method that is going to be Executed</summary>
    property MethodString: string read GetMethodString write SetMethodString;

    /// <summary>Getter for the SourceStream property</summary>
    function GetSourceStream: TStream;
    /// <summary>Setter for the SourceStream property</summary>
    procedure SetSourceStream(const ASourceStream: TStream);
    /// <summary>Property to Get/Set the SourceStream of a Request</summary>
    /// <returns>The source stream associated to the Request.</returns>
    property SourceStream: TStream read GetSourceStream write SetSourceStream;

    /// <summary>Cancels request</summary>
    procedure Cancel;
    /// <summary>Getter for the Canceled property</summary>
    function GetIsCancelled: Boolean;
    /// <summary>Property indicating when the request was canceled</summary>
    property IsCancelled: Boolean read GetIsCancelled;
  end;

  /// <summary>This class encapsulates the URL Request and all of it's related data</summary>
  TURLRequest = class(TInterfacedObject, IURLRequest)
  private
    FConnectionTimeout,
    FSendTimeout,
    FResponseTimeout: Integer;
    FSendTimeoutChanged: Boolean;
    function GetSendTimeout: Integer;
  protected
    /// <summary>URI To be accessed by the request</summary>
    FURL: TURI;
    /// <summary>Method to be executed by the request</summary>
    FMethodString: string;
    /// <summary>Credential given to the Request in the Userinfo part of the request URI</summary>
    FLocalCredential: TCredentialsStorage.TCredential;
    /// <summary>Client associated with the request object</summary>
    [Weak] FClient: TURLClient;
    /// <summary>Stream with data to be sent by the request</summary>
    [Weak] FSourceStream: TStream;
    /// <summary>Stores the cancelation status of the current request</summary>
    FCancelled: Boolean;

    {IURLRequest}
    function GetCredential: TCredentialsStorage.TCredential;
    procedure SetCredential(const ACredential: TCredentialsStorage.TCredential); overload; virtual;
    procedure SetCredential(const AUserName, APassword: string); overload; virtual;
    function GetURL: TURI;
    procedure SetURL(const AValue: TURI);
    function GetMethodString: string;
    procedure SetMethodString(const AValue: string);

    function GetSourceStream: TStream; virtual;
    procedure SetSourceStream(const ASourceStream: TStream); virtual;

    function GetIsCancelled: Boolean;
    procedure DoCancel; virtual;
    procedure DoResetCancel; virtual;
    procedure Cancel;

    /// <summary>Creates the URLRequest from the given parameters.</summary>
    /// <remarks>This constructor is protected because is not intended to be used explicitly.
    /// If an user wants an URLRequest needs to use specific Client.GetRequest (Example: THTTPClient.GetREquest)</remarks>
    constructor Create(const AClient: TURLClient; const AMethodString: string; const AURI: TURI);

    /// <summary> Setter for the ConnectionTimeout property.</summary>
    procedure SetConnectionTimeout(const Value: Integer); virtual;
    /// <summary> Setter for the SendTimeout property.</summary>
    procedure SetSendTimeout(const Value: Integer); virtual;
    /// <summary> Setter for the ResponseTimeout property.</summary>
    procedure SetResponseTimeout(const Value: Integer); virtual;
  public
    destructor Destroy; override;

    /// <summary> ConnectionTimeout property. Value is in milliseconds.</summary>
    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    /// <summary> SendTimeout property. Value is in milliseconds.</summary>
    property SendTimeout: Integer read GetSendTimeout write SetSendTimeout;
    /// <summary> ResponseTimeout property. Value is in milliseconds.</summary>
    property ResponseTimeout: Integer read FResponseTimeout write SetResponseTimeout;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Common interface for an URL response and all of it's related data</summary>
  IURLResponse = interface(IInterface)
    ['{5D687C75-5C36-4302-B0AB-989DDB7558FE}']
    /// <summary>Getter for the Headers property</summary>
    function GetHeaders: TNetHeaders;
    /// <summary>Property to retrieve all Headers from the response</summary>
    /// <returns>An Array of TNetHeader containing all Headers with theirs respective values.</returns>
    property Headers: TNetHeaders read GetHeaders;
    /// <summary>Getter for the MimeType property</summary>
    function GetMimeType: string;
    /// <summary>Property to retrieve the MimeType Header Value from the response</summary>
    /// <returns>A string containing the MimeType value.</returns>
    property MimeType: string read GetMimeType;
    /// <summary>Getter for the ContentStream property</summary>
    function GetContentStream: TStream;
    /// <summary>Property to retrieve the ContentStream from the response</summary>
    /// <returns>A stream whith the content of the response.</returns>
    property ContentStream: TStream read GetContentStream;
    /// <summary>Function that transforms the ContentStream into a string</summary>
    function ContentAsString(const AnEncoding: TEncoding = nil): string;
    /// <summary>Getter for the AsyncResult property</summary>
    function GetAsyncResult: IAsyncResult;
    /// <summary>A reference to IAsyncResult controlling asynchronous execution of corresponding request</summary>
    property AsyncResult: IAsyncResult read GetAsyncResult;
  end;

  /// <summary>This class encapsulates the URL response and all of it's related data</summary>
  TURLResponse = class(TBaseAsyncResult, IURLResponse)
  private
    FAsyncCallback: TAsyncCallback;
    FAsyncCallbackEvent: TAsyncCallbackEvent;
    FProc: TProc;

  protected
     /// <summary>Request that originate the response</summary>
    FRequest: IURLRequest;

    /// <summary>Field that holds a temporary stream when user do not provide a response stream</summary>
    FInternalStream: TStream;
    /// <summary>Field intended as a common stream to be used internally</summary>
    /// <remarks>This field will point to an external Stream or FInternalStream. It's a mere pointer</remarks>
    [Weak] FStream: TStream;

    constructor Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback;
      const AAsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: IURLRequest; const AContentStream: TStream); overload;
    constructor Create(const AContext: TObject; const ARequest: IURLRequest; const AContentStream: TStream); overload;

    /// <summary>Contructs InternalStream Field. Can be overrided to create a different kind of stream</summary>
    function DoCreateInternalStream: TStream; virtual;

    procedure AsyncDispatch; override;
    procedure Complete; override;
    procedure Schedule; override;

    function DoCancel: Boolean; override;

    function GetAsyncResult: IAsyncResult;

  public
    destructor Destroy; override;

    {IURLResponse}
    /// <summary>Implementation of IURLResponse Getter for the ContentStream property</summary>
    function GetContentStream: TStream;

    /// <summary>Implementation of IURLResponse Getter for the MimeType property</summary>
    function GetMimeType: string; virtual; abstract;
    /// <summary>Implementation of IURLResponse Getter for the Headers property</summary>
    /// <remarks>Must be overriden in platform Implementation.</remarks>
    function GetHeaders: TNetHeaders; virtual; abstract;
    /// <summary>Implementation of IURLResponse ContentAsString</summary>
    /// <remarks>If you do not provide AEncoding then the system will try to get it from the response</remarks>
    function ContentAsString(const AnEncoding: TEncoding = nil): string; virtual; abstract;
  end;
// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Class that encapsulates the managing of a common URL client.
  /// A derivative of this class can be a THTTPClient.</summary>
  TURLClient = class
  public const
    DefaultConnectionTimeout = 60000;
    DefaultSendTimeout = 60000;
    DefaultResponseTimeout = 60000;
  var
    FInstances: TObjectDictionary<string, TURLClient>;
    FAuthCallback: TCredentialsStorage.TCredentialAuthCallback;
    FAuthEvent: TCredentialsStorage.TCredentialAuthEvent;
    FInternalCredentialsStorage: TCredentialsStorage;
    FProxySettings: TProxySettings;
    [Weak] FCredentialsStorage: TCredentialsStorage;

    FConnectionTimeout: Integer;
    FSendTimeout: Integer;
    FResponseTimeout: Integer;

    function GetInternalInstance(const AScheme: string): TURLClient;
    function GetUserAgent: string;
    procedure SetUserAgent(const Value: string);
  protected

    /// <summary> CustomHeaders to be used by the client.</summary>
    FCustomHeaders: TURLHeaders;

    /// <summary> Setter for the ConnectionTimeout property.</summary>
    procedure SetConnectionTimeout(const Value: Integer);
    /// <summary> Setter for the SendTimeout property.</summary>
    procedure SetSendTimeout(const Value: Integer);
    /// <summary> Setter for the ResponseTimeout property.</summary>
    procedure SetResponseTimeout(const Value: Integer);

    /// <summary> Getter for the CustomHeaders property.</summary>
    function GetCustomHeaderValue(const Name: string): string;
    /// <summary> Setter for the CustomHeaders property.</summary>
    procedure SetCustomHeaderValue(const Name, Value: string);

    /// <summary>Function that transforms the ContentStream into a string</summary>
    function SupportedSchemes: TArray<string>; virtual;

    /// <summary>Function that executes a Request and obtains a response</summary>
    /// <remarks>This function must be overriden by specific clients (HTTP, FTP, ...)</remarks>
    function DoExecute(const ARequestMethod: string; const AURI: TURI;
      const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IURLResponse; virtual;
    /// <summary>Function that executes a Request and obtains a response</summary>
    /// <remarks>This function must be overriden by specific clients (HTTP, FTP, ...)</remarks>
    function DoExecuteAsync(const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequestMethod: string; const AURI: TURI;
      const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders; AOwnsSourceStream: Boolean = False): IAsyncResult; virtual;

    /// <summary>Function that obtains a Response instance</summary>
    /// <remarks>This function must be overriden by specific clients (HTTP, FTP, ...)</remarks>
    function DoGetResponseInstance(const AContext: TObject; const AProc: TProc; const AsyncCallback: TAsyncCallback;
      const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: IURLRequest; const AContentStream: TStream): IAsyncResult; virtual;

    /// <summary>Function that obtains a Request instance</summary>
    /// <remarks>This function must be overriden by specific clients (HTTP, FTP, ...)</remarks>
    function DoGetRequestInstance(const ARequestMethod: string; const AURI: TURI): IURLRequest; virtual;

    /// <summary> Function used to invoke the Authorization callback or the Authorization envent.</summary>
    procedure DoAuthCallback(AnAuthTarget: TAuthTargetType; const ARealm, AURL: string;
      var AUserName, APassword: string; var AbortAuth: Boolean; var Persistence: TAuthPersistenceType); virtual;

    /// <summary>Create a Client instance</summary>
    class function CreateInstance: TURLClient; virtual;
    /// <summary>To obtain a Client instance</summary>
    /// <remarks></remarks>
    /// <param name="AScheme">The scheme that is going to be used with the Client</param>
    /// <returns>The GetInstance method returns the protocol dependant TURLClient object associated to the given Scheme</returns>
    class function GetInstance(const AScheme: string): TURLClient; static;

    /// <summary>Setter for CredentialsStorage Property</summary>
    procedure SetCredentialsStorage(const Value: TCredentialsStorage); virtual;
    /// <summary>Get the compatible credentials with the given parameters</summary>
    function GetCredentials(AuthTarget: TAuthTargetType; const ARealm, URL: string): TCredentialsStorage.TCredentialArray; virtual;
    /// <summary>Setter for ProxySettings Property</summary>
    procedure SetProxySettings(const Value: TProxySettings); virtual;
  public
    /// <summary>To obtain a Client instance</summary>
    constructor Create;
    destructor Destroy; override;

    /// <summary>Function that obtains a Request instance</summary>
    function GetRequest(const ARequestMethod: string; const AURI: TURI): IURLRequest; overload;

    /// <summary>Function that obtains a Request instance</summary>
    function GetRequest(const ARequestMethod, AURI: string): IURLRequest; overload; inline;

    /// <summary>You have to use this function to Execute a Request</summary>
    /// <remarks></remarks>
    /// <param name="ARequestMethod">The request method that is going to be Executed</param>
    /// <param name="AURI">The URI that contains the information for the request that is going to be Executed</param>
    /// <param name="ASourceStream">The stream to provide the request data.</param>
    /// <param name="AContentStream">The stream to store the response data. If provided the user is responsible
    /// of releasing it. If not provided will be created internally and released when not needed.</param>
    /// <param name="AHeaders">Additional headers to be passed to the request that is going to be Executed</param>
    /// <returns>The platform dependant response object associated to the given request. It's an Interface and It's
    /// released automatically.</returns>
    function Execute(const ARequestMethod: string; const AURI: TURI; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IURLResponse; overload;

    /// <summary>You have to use this function to Execute a given Request</summary>
    /// <remarks></remarks>
    /// <param name="ARequestMethod">The request method that is going to be Executed</param>
    /// <param name="AURIStr">The URI string that contains the information for the request that is going to be Executed</param>
    /// <param name="ASourceStream">The stream to provide the request data.</param>
    /// <param name="AContentStream">The stream to store the response data. If provided the user is responsible
    /// of releasing it. If not provided will be created internally and released when not needed.</param>
    /// <param name="AHeaders">Additional headers to be passed to the request that is going to be Executed</param>
    /// <returns>The platform dependant response object associated to the given request. It's an Interface and It's
    /// released automatically.</returns>
    function Execute(const ARequestMethod: string; const AURIStr: string; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IURLResponse; overload;

    /// <summary>You have to use this function to Execute a Request</summary>
    /// <remarks></remarks>
    /// <param name="ARequestMethod">The request method that is going to be Executed</param>
    /// <param name="AURI">The URI that contains the information for the request that is going to be Executed</param>
    /// <param name="ASourceStream">The stream to provide the request data.</param>
    /// <param name="AContentStream">The stream to store the response data. If provided the user is responsible
    /// of releasing it. If not provided will be created internally and released when not needed.</param>
    /// <param name="AHeaders">Additional headers to be passed to the request that is going to be Executed</param>
    /// <returns>The platform dependant response object associated to the given request. It's an Interface and It's
    /// released automatically.</returns>
    function BeginExecute(const ARequestMethod: string; const AURI: TURI; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult; overload;
    function BeginExecute(const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequestMethod: string; const AURI: TURI; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult; overload;
    function BeginExecute(const AsyncCallback: TAsyncCallback; const ARequestMethod: string; const AURI: TURI; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult; overload;

    /// <summary>You have to use this function to Execute a given Request</summary>
    /// <remarks></remarks>
    /// <param name="ARequestMethod">The request method that is going to be Executed</param>
    /// <param name="AURIStr">The URI string that contains the information for the request that is going to be Executed</param>
    /// <param name="ASourceStream">The stream to provide the request data.</param>
    /// <param name="AContentStream">The stream to store the response data. If provided the user is responsible
    /// of releasing it. If not provided will be created internally and released when not needed.</param>
    /// <param name="AHeaders">Additional headers to be passed to the request that is going to be Executed</param>
    /// <returns>The platform dependant response object associated to the given request. It's an Interface and It's
    /// released automatically.</returns>
    function BeginExecute(const ARequestMethod: string; const AURIStr: string; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult; overload;
    function BeginExecute(const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequestMethod: string; const AURIStr: string; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult; overload;
    function BeginExecute(const AsyncCallback: TAsyncCallback; const ARequestMethod: string; const AURIStr: string; const ASourceStream: TStream = nil;
      const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult; overload;

    /// <summary>You have to use this function to Wait for the result of a given Request</summary>
    /// <remarks>You must use this class function to ensure that any pending exceptions are raised in the proper context</remarks>
    /// <returns>The platform dependant response object associated to the given request. It's an Interface and It's
    /// released automatically.</returns>
    class function EndAsyncURL(const AAsyncResult: IAsyncResult): IURLResponse; overload;
    class function EndAsyncURL(const AAsyncResult: IURLResponse): IURLResponse; overload;

    /// <summary> Property to set/get the ConnectionTimeout. Value is in milliseconds.
    ///  -1 - Infinite timeout. 0 - platform specific timeout. Supported by Windows, Linux, Android platforms. </summary>
    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
    /// <summary> Property to set/get the SendTimeout. Value is in milliseconds.
    ///  -1 - Infinite timeout. 0 - platform specific timeout. Supported by Windows, macOS platforms. </summary>
    property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
    /// <summary> Property to set/get the ResponseTimeout. Value is in milliseconds.
    ///  -1 - Infinite timeout. 0 - platform specific timeout. Supported by all platforms. </summary>
    property ResponseTimeout: Integer read FResponseTimeout write SetResponseTimeout;

    /// <summary> Property to set the UserAgent sent with the request </summary>
    property UserAgent: string read GetUserAgent write SetUserAgent;
    /// <summary> Authorization CallBack to ask for user and password</summary>
    /// <remarks> If the AuthCallback is defined, the AuthEvent is not fired</remarks>
    property AuthCallback: TCredentialsStorage.TCredentialAuthCallback read FAuthCallback write FAuthCallback;
    /// <summary> Authorization Event to ask for user and password</summary>
    /// <remarks> If the AuthCallback is defined, the AuthEvent is not fired</remarks>
    property AuthEvent: TCredentialsStorage.TCredentialAuthEvent read FAuthEvent write FAuthEvent;
    /// <summary> Credential Storage to be used by the client</summary>
    /// <remarks> By default the client has his own CredentialStorage</remarks>
    property CredentialsStorage: TCredentialsStorage read FCredentialsStorage write SetCredentialsStorage;
    /// <summary> Proxy Settings to be used by the client.</summary>
    property ProxySettings: TProxySettings read FProxySettings write SetProxySettings;
    /// <summary> CustomHeaders to be used by the client.</summary>
    property CustomHeaders[const AName: string]: string read GetCustomHeaderValue write SetCustomHeaderValue;
    /// <summary> CustHeaders is TURLHeaders object managing custom headers to be used by the client.</summary>
    property CustHeaders: TURLHeaders read FCustomHeaders;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary> Record to store the information of a certificate.</summary>
  TCertificate = record
    /// <summary> Name of the certificate</summary>
    CertName: string;
    /// <summary> Serial number of the certificate</summary>
    SerialNum: string;
    /// <summary> Expiry date of the certificate</summary>
    Expiry: TDateTime;
    /// <summary> Start date of the certificate</summary>
    Start: TDateTime;
    /// <summary> Subject of the certificate</summary>
    Subject: string;
    /// <summary> Issuer of the certificate</summary>
    Issuer: string;
    /// <summary> ProtocolName of the certificate</summary>
    ProtocolName: string;
    /// <summary> Algorithm Signature of the certificate</summary>
    AlgSignature: string;
    /// <summary> Algorithm Encryption of the certificate</summary>
    AlgEncryption: string;
    /// <summary> Encryption's KeySize of the certificate</summary>
    KeySize: Integer;
    /// <summary> RSA public key modulus</summary>
    PublicKey: string;
    /// <summary> Returns True when all key fields are empty</summary>
    function IsEmpty: Boolean;
  end;

  /// <summary> List of Certificates.</summary>
  TCertificateList = class(TList<TCertificate>);

  ///  <summary> Certificate selection callback signature</summary>
  TNeedClientCertificateCallback = procedure(const Sender: TObject; const ARequest: TURLRequest; const ACertificateList: TCertificateList;
    var AnIndex: Integer);

  ///  <summary> Certificate selection Event signature</summary>
  TNeedClientCertificateEvent = procedure(const Sender: TObject; const ARequest: TURLRequest; const ACertificateList: TCertificateList;
    var AnIndex: Integer) of object;

  ///  <summary> Certificate validation callback signature</summary>
  ///  <param name="Sender"> HTTPClient that invoked the callback</param>
  ///  <param name="ARequest">Request that invoked the callback</param>
  ///  <param name="Certificate">Certificate to validate</param>
  ///  <param name="Accepted">boolean, expected to be true if the certificate is valid</param>
  TValidateCertificateCallback = procedure(const Sender: TObject; const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);

  ///  <summary> Certificate validation Event signature</summary>
  ///  <param name="Sender"> HTTPClient that invoked the Event</param>
  ///  <param name="ARequest">Request that invoked the Event</param>
  ///  <param name="Certificate">Certificate to validate</param>
  ///  <param name="Accepted">boolean, expected to be true if the certificate is valid</param>
  TValidateCertificateEvent = procedure(const Sender: TObject; const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean) of object;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Factory for creating objects associated to currently registered schemes</summary>
  TURLSchemes = class
  private type
    /// <summary>Metaclass used to store platform dependant TURLClient classes associated to schemes</summary>
    TURLClientClass = class of TURLClient;
  private
    class var FSchemeClients: TDictionary<string, TURLClientClass>;
  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>Register a platform URLClient class associated to a given scheme</summary>
    class procedure RegisterURLClientScheme(const AURLClientClass: TURLClientClass; const AScheme: string);
    /// <summary>Unregister a given scheme and it's associated platform URLClient class</summary>
    class procedure UnRegisterURLClientScheme(const AScheme: string);
    /// <summary>Obtain a platform URLClient object instance associated to a given scheme</summary>
    /// <remarks>Obtained instance must be released by the user.</remarks>
    /// <param name="AScheme"></param>
    /// <returns>The platform dependant client object valid for the given scheme.</returns>
    class function GetURLClientInstance(const AScheme: string): TURLClient;
  end;

// -------------------------------------------------------------------------------- //
// -------------------------------------------------------------------------------- //

  /// <summary>Class that implements a stream that is populated asynchronously.
  /// TAsyncReadStream supports only reading operations. Any writing operation
  /// outside of APopulator will raise an exception. Any reading operation will
  /// be blocked until APopulator is finished. </summary>
  TAsyncReadStream = class(TMemoryStream, IAsyncResult)
  protected type
    TStreamAsyncResult = class(TBaseAsyncResult)
    private
      function GetStream: TAsyncReadStream; inline;
    protected
      procedure Schedule; override;
      procedure AsyncDispatch; override;
      procedure Complete; override;
      function DoCancel: Boolean; override;
      property Stream: TAsyncReadStream read GetStream;
    end;
  public type
    TStreamer = reference to procedure (AStream: TStream);
    TCanceler = reference to function (AStream: TStream): Boolean;
  private
    FAsyncResultObj: TStreamAsyncResult;
    FAsyncResult: IAsyncResult;
    FOwningThread: TThreadID;
    FPopulator: TStreamer;
    FProvider: TStreamer;
    FCanceler: TCanceler;
    FSynchronizeProvide: Boolean;
    FFreeOnCompletion: Boolean;
    FReadingStream: TStream;
    procedure Populate;
    procedure Provide;
    procedure WaitForCompletion;
    procedure CheckWriting;
    class var FActiveStreams: TThreadList;
    class constructor Create;
    class destructor Destroy;
  protected
    /// <summary> Method used to invoke the APopulator callback, which will populate the stream.</summary>
    procedure DoPopulate; virtual;
    /// <summary> Method used to invoke the AProvider callback, which is called when stream population is finished.</summary>
    procedure DoProvide; virtual;
    /// <summary> Method used to invoke the ACanceler callback, which is called when stream population must be canceled.</summary>
    function DoCancel: Boolean; virtual;
    /// <summary> Method which sets a stream, which is used as internal content provider of this stream.</summary>
    procedure SetReadingStream(AReadingStream: TStream);
  public
    /// <summary> Creates TAsyncReadStream instance.</summary>
    /// <param name="APopulator">A callback which will populate the stream</param>
    /// <param name="AProvider">A callback which is called when stream population is finished</param>
    /// <param name="ACanceler">A callback which is called when stream population must be canceled</param>
    /// <param name="ASynchronizeProvide">Specifies when AProvider must be called in context of main thread</param>
    /// <param name="AFreeOnCompletion">Specifies when this instance must be destroyed after finishing</param>
    constructor Create(const APopulator, AProvider: TStreamer; const ACanceler: TCanceler = nil;
      ASynchronizeProvide: Boolean = True; AFreeOnCompletion: Boolean = True);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    procedure SaveToStream(Stream: TStream); override;
    /// <summary> Property to get IAsyncResult interface for this stream</summary>
    property AsyncResult: IAsyncResult read FAsyncResult implements IAsyncResult;
  end;

  /// <summary>Class that implements a stream providing a content of specified URL.
  /// The supported URL schemas are HTTP, HTTPS, FILE, RES. </summary>
  TURLStream = class(TAsyncReadStream)
  public type
    TSyncReqExecutor = reference to function (AClient: TURLClient; const ARequest: IURLRequest;
      const AContentStream: TStream; const AHeaders: TNetHeaders): IURLRequest;
  private
    FClient: TURLClient;
    FRequest: IURLRequest;
    FModule: THandle;
    class var FSyncReqExecutors: TDictionary<string, TSyncReqExecutor>;
    class constructor Create;
    class destructor Destroy;
  protected
    procedure DoPopulate; override;
    function DoCancel: Boolean; override;
    /// <summary> Method resolves an URL with FILE scheme and returns corresponding file stream.</summary>
    function GetFileSchemeStream(const AURI: TURI): TStream; virtual;
    /// <summary> Method resolves an URL with RES scheme and returns corresponding resource stream.</summary>
    function GetResSchemeStream(const AURI: TURI): TStream; virtual;
    /// <summary> Method resolves an URL with non standard schemes and returns corresponding stream.</summary>
    function GetOtherSchemeStream(const AURI: TURI): TStream; virtual;
  public
    /// <summary> Creates TURLStream instance.</summary>
    /// <param name="AURL">A URL which provides content of this stream.
    /// The supported URL schemas are HTTP, HTTPS, FILE, RES. </param>
    /// <param name="AProvider">A callback which is called when stream population is finished</param>
    /// <param name="ASynchronizeProvide">Specifies when AProvider must be called in context of main thread</param>
    /// <param name="AFreeOnCompletion">Specifies when this instance must be destroyed after finishing</param>
    constructor Create(const AURL: string; const AProvider: TAsyncReadStream.TStreamer;
      ASynchronizeProvide: Boolean = True; AFreeOnCompletion: Boolean = True);
    destructor Destroy; override;
    class procedure RegisterSyncReqExecutor(const AScheme: string; const AFunc: TSyncReqExecutor);
    class procedure UnRegisterSyncReqExecutor(const AScheme: string);
  end;

implementation

uses
{$IFDEF MACOS}
  Macapi.CoreFoundation,
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.RTLConsts, System.NetConsts, System.Threading, System.NetEncoding,
  System.StrUtils;


{ TPunyCode }

type
  TPunyCode = class
  private
  const
    BASE: Cardinal = 36;
    TMIN: Cardinal =  1;
    TMAX: Cardinal = 26;
    SKEW: Cardinal = 38;
    DAMP: Cardinal = 700;
    INITIAL_BIAS: Cardinal = 72;
    INITIAL_N: Cardinal = 128;
    MAX_INT: Cardinal = 4294967295;
    Delimiter = '-';
  private
    class function GetMinCodePoint(const AMinLimit: Cardinal; const AString: UCS4String): Cardinal;
    class function IsBasic(const AString: UCS4String; const AIndex, AMinLimit: Cardinal): Boolean; inline;
    class function Adapt(const ADelta, ANumPoints: Cardinal; const FirstTime: Boolean): Cardinal;
    class function Digit2Codepoint(const ADigit: Cardinal): Cardinal;
    class function Codepoint2Digit(const ACodePoint: Cardinal): Cardinal;
    class function DoEncode(const AString: UCS4String): UCS4String; overload;
    class function DoDecode(const AString: UCS4String): UCS4String; overload;
    class function DoEncode(const AString: string): string; overload;
    class function DoDecode(const AString: string): string; overload;
  public
    class function Encode(const AString: string): string;
    class function Decode(const AString: string): string;
  end;


{ TNameValuePair }

constructor TNameValuePair.Create(const AName, AValue: string);
begin
  Name := AName;
  Value := AValue;
end;


{ TURI }

procedure TURI.AddParameter(const AParameter: TURIParameter);
begin
  AddParameter(AParameter.Name, AParameter.Value);
end;

function TURI.GetQuery: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Params) - 1 do
    Result := Result + Params[I].Name + '=' + Params[I].Value + '&';
  if Result <> '' then
    Result := Result.Substring(0, Result.Length - 1); //Remove last '&'
end;

procedure TURI.AddParameter(const AName, AValue: string);
var
  Len: Integer;
begin
  Len := Length(Params);
  SetLength(FParams, Len + 1);
  FParams[Len].Name := TNetEncoding.URL.EncodeQuery(AName);
  FParams[Len].Value := TNetEncoding.URL.EncodeQuery(AValue);
  FQuery := GetQuery;
end;

procedure TURI.ComposeURI(const AScheme, AUsername, APassword, AHostname: string; APort: Integer; const APath: string;
  const AParams: TURIParameters; const AFragment: string);
begin
  FScheme := AScheme.ToLower;
  FUsername := TNetEncoding.URL.EncodeQuery(AUsername);
  FPassword := TNetEncoding.URL.EncodeQuery(APassword);
  FHost := AHostname;
  FPort := APort;
  FPath := TNetEncoding.URL.EncodePath(APath);
  Params := AParams;
  FFragment := TNetEncoding.URL.EncodeQuery(AFragment);
end;

constructor TURI.Create(const AURIStr: string);
begin
  DecomposeURI(AURIStr, True);
end;

procedure TURI.DecomposeBaseScheme(const AURIStr: string; Pos, Limit, SlashCount: Integer);

  function PortColonOffset(Pos, Limit: Integer): Integer;
  begin
    while Pos <= Limit do
    begin
      case AURIStr.Chars[Pos] of
        '[':
          repeat
            Inc(Pos);
          until (Pos >= Limit) or (AURIStr.Chars[Pos] = ']');

        ':':
          Exit(Pos);
      end;
      Inc(Pos);
    end;
    Result := Limit; // No colon
  end;

  function ParsePort(Pos, Limit: Integer): Integer;
  begin
    Result := AURIStr.Substring(Pos, Limit - Pos).ToInteger;
  end;

var
  LCompDelimiterOffset: Integer;
  LPasswordColonOffset: Integer;
  LPortColonOffset: Integer;
  LQueryDelimiterOffset: Integer;
  LPathDelimiterOffset: Integer;
  C: Char;
  LHasPassword: Boolean;
  LHasUsername: Boolean;
  LEncodedUsername: string;
begin
  // Read an authority if either:
  //  * The input starts with 2 or more slashes. These follow the scheme if it exists.
  //  * The input scheme exists and is different from the base URL's scheme.
  //
  // The structure of an authority is:
  //   username:password@host:port
  //
  // Username, password and port are optional.
  //   [username[:password]@]host[:port]
  if (SlashCount >= 2) then
    Inc(Pos, 2);//work around: Only remove the first 2 slashes to be able to omit the authority.

  LHasPassword := False;
  LHasUsername := False;

  while True do
  begin
    LCompDelimiterOffset := AURIStr.IndexOfAny(['@', '/', '\', '?', '#'], Pos, Limit + 1 - Pos);
    if LCompDelimiterOffset = -1 then
      LCompDelimiterOffset := Limit + 1;

    if LCompDelimiterOffset <> Limit + 1 then
      C := AURIStr.Chars[LCompDelimiterOffset]
    else
      C := Char(-1);

    case C of
      '@':
        begin
          if not LHasPassword then
          begin
            LPasswordColonOffset := AURIStr.IndexOf(':', Pos, LCompDelimiterOffset - Pos);
            if LPasswordColonOffset = -1 then
              LPasswordColonOffset := LCompDelimiterOffset;

            LEncodedUsername := TNetEncoding.URL.EncodeAuth(AURIStr.Substring(Pos, LPasswordColonOffset - Pos));
            if LHasUsername then
              Username := Username + '%40' + LEncodedUsername
            else
              Username := LEncodedUsername;

            if LPasswordColonOffset <> LCompDelimiterOffset then
            begin
              LHasPassword := True;
              Password := TNetEncoding.URL.Encode(AURIStr.Substring(LPasswordColonOffset + 1,
                LCompDelimiterOffset - (LPasswordColonOffset + 1)));
            end;
            LHasUsername := True;
          end
          else
            Password := Password + '%40' + TNetEncoding.URL.Encode(AURIStr.Substring(Pos, LCompDelimiterOffset - Pos));

          Pos := LCompDelimiterOffset + 1;
        end;

      Char(-1), '/', '\', '?', '#':
        begin
          LPortColonOffset := PortColonOffset(Pos, LCompDelimiterOffset);
          Host := AURIStr.Substring(Pos, LPortColonOffset - Pos);
          if LPortColonOffset + 1 < LCompDelimiterOffset then
            try
              Port := ParsePort(LPortColonOffset + 1, LCompDelimiterOffset)
            except
              raise ENetURIException.CreateResFmt(@SNetUriInvalid, [AURIStr]);
            end
          else
            Port := GetDefaultPort(Scheme);

           //It is only wrong if host was not omitted.
          if (Host = '') and (SlashCount = 2) then
            raise ENetURIException.CreateResFmt(@SNetUriInvalid, [AURIStr]);

          Pos := LCompDelimiterOffset;
          Break;
        end;
    end;

  end;

  LPathDelimiterOffset := AURIStr.IndexOfAny(['?', '#'], Pos, Limit); // PathDelimiterOffset
  if LPathDelimiterOffset = -1 then
    LPathDelimiterOffset := Limit + 1;
  Path := TNetEncoding.URL.EncodePath(AURIStr.Substring(Pos, LPathDelimiterOffset - Pos));
  Pos := LPathDelimiterOffset;

  // Query
  if (Pos < Limit) and (AURIStr.Chars[Pos] = '?') then
  begin
    LQueryDelimiterOffset := AURIStr.IndexOf('#', Pos, Limit + 1 - Pos);
    if LQueryDelimiterOffset = -1 then
      LQueryDelimiterOffset := Limit + 1;
    Query := TNetEncoding.URL.EncodeQuery(AURIStr.Substring(Pos + 1, LQueryDelimiterOffset - (Pos + 1)));
    Pos := LQueryDelimiterOffset;
  end;

  // Fragment
  if (Pos < Limit) and (AURIStr.Chars[Pos] = '#') then
    Fragment := TNetEncoding.URL.Encode(AURIStr.Substring(Pos + 1, Limit + 1 - (Pos + 1)), [], []);

  ParseParams(True);
end;

procedure TURI.DecomposeNoAuthorityScheme(const AURIStr: string; Pos, Limit, SlashCount: Integer);
begin
  // scheme without authority have 2 parts: <scheme name>:<path>
  // Example urn:oasis:names:specification:docbook:dtd:xml:4.1.2
  //      scheme = urn
  //      path = oasis:names:specification:docbook:dtd:xml:4.1.2
  Path := AURIStr.Substring(Pos);
  Port := GetDefaultPort(Scheme);
end;

procedure TURI.DecomposeURI(const AURIStr: string; ARaiseNoSchema: Boolean);

  function SchemeDelimiterOffset(Pos, Limit: Integer): Integer;
  var
    I: Integer;
  begin
    if Limit - Pos > 0 then
      for I := Pos to Limit do
        case AURIStr.Chars[I] of
          // Scheme character. Keep going.
          '0'..'9', 'a'..'z', 'A'..'Z', '+', '-', '.':
            {continue};
          ':':
            Exit(I);
        else
          // Non-scheme character before the first ':'.
          Exit(-1);
        end;

    // No ':'; doesn't start with a scheme.
    Result := -1;
  end;

  function SlashCount(Pos, Limit: Integer): Integer;
  begin
    Result := 0;
    while Pos < Limit do
      if (AURIStr.Chars[Pos] = '\') or (AURIStr.Chars[Pos] = '/') then
      begin
        Inc(Result);
        Inc(Pos);
      end
      else
        Break;
  end;

  function GetSchemeDecomposProc(const AScheme: string; ASlashcount: Integer): TSchemeDecomposeProc;
  begin
    Result := nil;
    // Typical scheme with "://" delimiters
    if ASlashCount >= 2 then
      Result := DecomposeBaseScheme
    else

    // Mailto scheme. This scheme is "No autority". But it should decompose
    // by base decomposer for detect host, user name
    if IsMailtoScheme then
      Result := DecomposeBaseScheme
    else

    // News, tel, urn schemes
    if IsSchemeNoAuthority then
      Result := DecomposeNoAuthorityScheme;
  end;

var
  PosScheme: Integer;
  Pos: Integer;
  Limit: Integer;
  LSlashCount: Integer;
  SchemeDecomposeProc: TSchemeDecomposeProc;
begin
  Self := Default(TURI);

  // Skip leading white spaces
  Pos := 0;
  while (Pos < High(AURIStr)) and (
          (AURIStr.Chars[Pos] = ' ') or
          (AURIStr.Chars[Pos] = #9) or
          (AURIStr.Chars[Pos] = #10) or
          (AURIStr.Chars[Pos] = #13) or
          (AURIStr.Chars[Pos] = #12)) do
    Inc(Pos);

  // Skip trailing white spaces
  //Limit := High(AURIStr);
  Limit := AURIStr.Length - 1;
  while (Limit > 0) and (
          (AURIStr.Chars[Limit] = ' ') or
          (AURIStr.Chars[Limit] = #9) or
          (AURIStr.Chars[Limit] = #10) or
          (AURIStr.Chars[Limit] = #13) or
          (AURIStr.Chars[Limit] = #12)) do
    Dec(Limit);

  PosScheme := SchemeDelimiterOffset(Pos, Limit);

  if PosScheme = -1 then
    if ARaiseNoSchema then
      raise ENetURIException.CreateResFmt(@SNetUriInvalid, [AURIStr])
    else
      Exit;

  Scheme := AURIStr.Substring(Pos, PosScheme - Pos);
  Pos := Pos + Scheme.Length + 1;

  LSlashCount := SlashCount(Pos, Limit);
  SchemeDecomposeProc := GetSchemeDecomposProc(Scheme, LSlashCount);
  if Assigned(SchemeDecomposeProc) then
    SchemeDecomposeProc(AURIStr, Pos, Limit, LSlashCount)
  else
    raise ENetURIException.CreateResFmt(@SNetUriInvalid, [AURIStr]);
end;

procedure TURI.DeleteParameter(const AName: string);
var
  LIndex: Integer;
begin
  LIndex := FindParameterIndex(AName);
  if LIndex >= 0 then
  begin
    repeat
      DeleteParameter(LIndex);
      LIndex := FindParameterIndex(AName);
    until LIndex = -1;
  end
  else
    raise ENetURIException.CreateResFmt(@SNetUriParamNotFound, [AName]);
end;

function TURI.FindParameterIndex(const AName: string): Integer;
var
  I: Integer;
  LName: string;
begin
  Result := -1;
  LName := TNetEncoding.URL.EncodeQuery(AName);
  for I := 0 to Length(Params) - 1 do
    if Params[I].Name = LName then
      Exit(I);
end;

procedure TURI.DeleteParameter(AIndex: Integer);
begin
  if (AIndex >= Low(Params)) and (AIndex <= High(Params)) then
  begin
    if AIndex < High(Params) then
    begin
      Finalize(Params[AIndex]);
      System.Move(Params[AIndex + 1], Params[AIndex], (Length(Params) - AIndex - 1) * SizeOf(TURIParameter));
      FillChar(Params[High(Params)], SizeOf(TURIParameter), 0);
    end;
    SetLength(FParams, Length(FParams) - 1);
    FQuery := GetQuery;
  end
  else
    raise ENetURIException.CreateResFmt(@SNetUriIndexOutOfRange, [AIndex, Low(Params), High(Params)]);
end;

function TURI.GetDefaultPort(const AScheme: string): Integer;
begin
  if AScheme.Equals(SCHEME_HTTP) then
    Result := 80
  else if AScheme.Equals(SCHEME_HTTPS) then
    Result := 443
  else
    Result := -1;
end;

function TURI.GetParameter(const I: Integer): TURIParameter;
begin
  if (I >= Low(Params)) and (I <= High(Params)) then
    Result := Params[I]
  else
    raise ENetURIException.CreateResFmt(@SNetUriIndexOutOfRange, [I, Low(Params), High(Params)]);
end;

function TURI.GetParameterByName(const AName: string): string;
var
  LIndex: Integer;
begin
  LIndex := FindParameterIndex(AName);
  if LIndex >= 0 then
    Result := GetParameter(LIndex).Value
  else
    raise ENetURIException.CreateResFmt(@SNetUriParamNotFound, [AName]);
end;

class function TURI.IDNAToUnicode(const AHostName: string): string;
var
  I: Integer;
  LDecoded: string;
  LPart: string;
  LParts: TArray<string>;
  LIsEncoded: Boolean;
begin
  LIsEncoded := False;
  LParts := AHostName.Split(['.']);
  for I := Low(LParts) to High(LParts) do
    if LParts[I].StartsWith('xn--') then
    begin
      LPart := LParts[I].Substring('xn--'.Length);
      if LPart.IndexOf('-') = -1 then
        LPart := '-' + LPart;
      LDecoded := TPunyCode.Decode(LPart);
      if LDecoded <> LPart then
      begin
        LParts[I] := LDecoded;
        LIsEncoded := True;
      end;
    end;
  if LIsEncoded then
    Result := string.Join('.', LParts)
  else
    Result := AHostName;
end;

function TURI.IsMailtoScheme: Boolean;
begin
  Result := Scheme.Equals(SCHEME_MAILTO);
end;

function TURI.IsSchemeNoAuthority: Boolean;
begin
  Result := Scheme.Equals(SCHEME_MAILTO) or
            Scheme.Equals(SCHEME_NEWS) or
            Scheme.Equals(SCHEME_TEL) or
            Scheme.Equals(SCHEME_URN);
end;

function TURI.IsValidPort: Boolean;
begin
  Result := (Port <> 0) and (Port <> -1);
end;

procedure TURI.ParseParams(Encode: Boolean);
var
  LParts: TArray<string>;
  I: Integer;
  Pos: Integer;
begin
  LParts := Query.Split([Char(';'), Char('&')]);
  SetLength(FParams, Length(LParts));
  for I := 0 to Length(LParts) - 1 do
  begin
    Pos := LParts[I].IndexOf(Char('='));
    if Pos > 0 then
    begin
      if Encode then
      begin
        FParams[I].Name := TNetEncoding.URL.EncodeQuery(LParts[I].Substring(0, Pos));
        FParams[I].Value := TNetEncoding.URL.EncodeQuery(LParts[I].Substring(Pos + 1));
      end
      else
      begin
        FParams[I].Name := LParts[I].Substring(0, Pos);
        FParams[I].Value := LParts[I].Substring(Pos + 1);
      end;
    end
    else
    begin
      if Encode then
        FParams[I].Name := TNetEncoding.URL.EncodeQuery(LParts[I])
      else
        FParams[I].Name := LParts[I];
      FParams[I].Value := '';
    end;
  end;
end;

class function TURI.PathRelativeToAbs(const RelPath: string; const Base: TURI): string;

  function GetURLBase(const AURI: TURI): string;
  begin
    if AURI.GetDefaultPort(AURI.Scheme) = AURI.Port then
      Result := AURI.Scheme + '://' + AURI.Host
    else
    begin
      if AURI.IsSchemeNoAuthority then
        Result := AURI.Scheme + ':' + AURI.Host
      else
        Result := AURI.Scheme + '://' + AURI.Host;
      if AURI.IsValidPort then
        Result := Result + ':' + AURI.Port.ToString;
    end;
  end;

  function GetURLPath(const AURI: TURI): string;
  begin
    Result := AURI.Path;
    Result := Result.Substring(0, Result.LastIndexOf(Char('/')) + 1);
  end;

var
  List: TStringList;
  Path: string;
  I, K: Integer;
begin
  if RelPath.Trim.IsEmpty then
    Exit(Base.ToString);

  I := RelPath.IndexOf(':');
  K := RelPath.IndexOf('/');

  if (I = -1) or (K <= I) then // not found ':' before '/' so there is no scheme, it's a relative path
  begin

    if K = 0 then // Begins with '/'
      Result := GetURLBase(Base) + RelPath
    else
    begin
      Path := GetURLPath(Base) + RelPath;

      List := TStringList.Create;
      try
        List.LineBreak := '/';
        List.Text := Path;
        I := 0;
        while I < List.Count do
        begin
          if (List[I] = '.') then
            List.Delete(I)
          else
          if (I > 0) and (List[I] = '..') then
          begin
            List.Delete(I);
            if I <= 1 then
              raise ENetURIException.CreateResFmt(@SNetUriInvalidRelPath, [RelPath]);
            Dec(I);
            List.Delete(I);
          end
          else Inc(I);
        end;
        Result := '';
        for I := 0 to List.Count - 1 do
        begin
          if List[I] = '' then
            Result := Result + '/'
          else
            Result := Result + List[I] + '/';
        end;
        if RelPath[High(RelPath)] <> '/' then
          Result := Result.Substring(0, Result.Length - 1); // remove last '/'
        Result := GetURLBase(Base) + Result;
      finally
        List.Free;
      end;
    end;
  end
  else
    Result := RelPath;
end;

procedure TURI.SetHost(const Value: string);
begin
  FHost := UnicodeToIDNA(Value);
end;

procedure TURI.SetParameter(const I: Integer; const Value: TURIParameter);
begin
  if (I >= Low(Params)) and (I <= High(Params)) then
  begin
    Params[I].Name := TNetEncoding.URL.EncodeQuery(Value.Name);
    Params[I].Value := TNetEncoding.URL.EncodeQuery(Value.Value);
    FQuery := GetQuery;
  end
  else
    raise ENetURIException.CreateResFmt(@SNetUriIndexOutOfRange, [I, Low(Params), High(Params)]);
end;

procedure TURI.SetParameterByName(const AName: string; const Value: string);
var
  LIndex: Integer;
begin
  LIndex := FindParameterIndex(AName);
  if LIndex >= 0 then
  begin
    Params[LIndex].Value := TNetEncoding.URL.EncodeQuery(Value);
    FQuery := GetQuery;
  end
  else
    raise ENetURIException.CreateResFmt(@SNetUriParamNotFound, [AName]);
end;

procedure TURI.SetParams(const Value: TURIParameters);
var
  I: Integer;
begin
  FParams := Value;
  for I := Low(FParams) to High(FParams) do
  begin
    FParams[I].Name := TNetEncoding.URL.EncodeQuery(FParams[I].Name);
    FParams[I].Value := TNetEncoding.URL.EncodeQuery(FParams[I].Value);
  end;
  FQuery := GetQuery;
end;

procedure TURI.SetPassword(const Value: string);
begin
  FPassword := TNetEncoding.URL.EncodeAuth(Value);
end;

procedure TURI.SetPath(const Value: string);
begin
  if Value = '' then
    FPath := '/'
  else
    FPath := TNetEncoding.URL.EncodePath(Value);

  if IsSchemeNoAuthority and FPath.StartsWith('/') then
    FPath := FPath.Remove(0, 1);
end;

procedure TURI.SetQuery(const Value: string);
begin
  FQuery := Value;
  ParseParams(True);
end;

procedure TURI.SetScheme(const Value: string);
begin
  FScheme := Value.ToLower;
end;

procedure TURI.SetUserName(const Value: string);
begin
  FUsername := TNetEncoding.URL.EncodeAuth(Value);
end;

function TURI.ToString: string;
var
  LAuth: string;
begin
  if Username <> '' then
    if Password <> '' then
      LAuth := Username + ':' + Password + '@'
    else
      LAuth := Username + '@'
  else
    LAuth := '';
  if Scheme <> '' then
    if IsSchemeNoAuthority then
      Result := Scheme + ':'
    else
      Result := Scheme + '://'
  else
    Result := '';
  Result := Result + LAuth + Host;
  if IsValidPort and (GetDefaultPort(Scheme) <> Port) then
    Result := Result + ':' + Port.ToString;
  Result := Result + Path;
  if Length(Params) > 0 then
    Result := Result + '?' + Query;
  if Fragment <> '' then
    Result := Result + '#' + Fragment;
end;

class function TURI.UnicodeToIDNA(const AHostName: string): string;
  procedure FixHostNamePart(var APart: string); inline;
  begin
    // This function is for solving issues related to: Internationalized Domain Names (IDN) in .museum - Orthographic issues
    // See: http://about.museum/idn/issues.html
    // U+00DF LATIN SMALL LETTER SHARP S
    APart := APart.Replace(#$00DF, 'ss', [rfReplaceAll]);
    // U+03C2 GREEK SMALL LETTER FINAL SIGMA
    // U+03C3 GREEK SMALL LETTER SIGMA
    APart := APart.Replace(#$03C2, #$03C3, [rfReplaceAll]);
    // case 0x200c:  // Ignore/remove ZWNJ.
    APart := APart.Replace(#$200C, '', [rfReplaceAll]);
    // case 0x200d:  // Ignore/remove ZWJ.
    APart := APart.Replace(#$200D, '', [rfReplaceAll]);
  end;
var
  I: Integer;
  LNormalizedHostName: string;
  LEncoded: string;
  LParts: TArray<string>;
begin
  // Normalize HostName. We do a LowerCase conversion. This manages casing issues.
  LNormalizedHostName := AHostName.ToLower;

  LParts := LNormalizedHostName.Split(['.']);
  for I := Low(LParts) to High(LParts) do
  begin
    FixHostNamePart(LParts[I]);

    LEncoded := TPunyCode.Encode(LParts[I]);
    if LEncoded <> LParts[I] then
    begin
      if LEncoded.StartsWith('-') then
        LParts[I] := 'xn-' + LEncoded
      else
        LParts[I] := 'xn--' + LEncoded;
    end;
  end;
  Result := string.Join('.', LParts)
end;

class function TURI.URLDecode(const AValue: string; PlusAsSpaces: Boolean): string;

const
  H2BConvert: array[Ord('0')..Ord('f')] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);

  function IsHexChar(C: Byte): Boolean;
  begin
    case C of
      Ord('0')..Ord('9'), Ord('a')..Ord('f'), Ord('A')..Ord('F'):  Result := True;
    else
      Result := False;
    end;
  end;


var
  ValueBuff: TBytes;
  Buff: TBytes;
  Cnt: Integer;
  Pos: Integer;
  Len: Integer;
begin
  Cnt := 0;
  Pos := 0;
  ValueBuff := TEncoding.UTF8.GetBytes(AValue);
  Len := Length(ValueBuff);
  SetLength(Buff, Len);
  while Pos < Len do
  begin
    if (ValueBuff[Pos] = Ord('%')) and ((Pos + 2) < Len)  and IsHexChar(ValueBuff[Pos + 1]) and IsHexChar(ValueBuff[Pos + 2]) then
    begin
      Buff[Cnt] := (H2BConvert[ValueBuff[Pos + 1]]) shl 4 or H2BConvert[ValueBuff[Pos + 2]];
      Inc(Pos, 3);
    end
    else
    begin
      if (ValueBuff[Pos] = Ord('+')) and PlusAsSpaces then
        Buff[Cnt] := Ord(' ')
      else
        Buff[Cnt] := ValueBuff[Pos];
      Inc(Pos);
    end;
    Inc(Cnt);
  end;
  Result := TEncoding.UTF8.GetString(Buff, 0 , Cnt);

end;

class function TURI.URLEncode(const AValue: string; SpacesAsPlus: Boolean): string;
const
  FormUnsafeChars: TURLEncoding.TUnsafeChars = [Ord('!'), Ord('"'), Ord('#'), Ord('$'), Ord('%'), Ord('&'), Ord(''''),
    Ord('('), Ord(')'), Ord('*'), Ord('+'), Ord(','), Ord('"'), Ord('/'), Ord(':'), Ord(';'), Ord('<'), Ord('>'),
    Ord('='), Ord('?'), Ord('@'), Ord('['), Ord(']'), Ord('\'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|')];
var
  LEncoding: TURLEncoding;
  LOptions: TURLEncoding.TEncodeOptions;
begin
  LEncoding := TURLEncoding.Create;
  try
    if SpacesAsPlus then
      LOptions := [TURLEncoding.TEncodeOption.EncodePercent, TURLEncoding.TEncodeOption.SpacesAsPlus]
    else
      LOptions := [TURLEncoding.TEncodeOption.EncodePercent];
    Result := LEncoding.Encode(AValue, FormUnsafeChars, LOptions);
  finally
    LEncoding.Free;
  end;
end;

class function TURI.FixupForREST(const AURL: string): string;
var
  LUri: TURI;
begin
  Result := Trim(AURL);
  if Result <> '' then
  begin
    LUri.DecomposeURI(Result, False);
    if LUri.Scheme = '' then
      Result := SCHEME_HTTP + '://' + Result;
    if Result[Length(Result)] = '/' then
      Delete(Result, Length(Result), 1);
  end;
end;

function TURI.Encode: string;
begin
  ComposeURI(FScheme, FUsername, FPassword, FHost, FPort, FPath, FParams, FFragment);
  Result := ToString;
end;

{ TProxySettings }

constructor TProxySettings.Create(const AURL: string);
var
  LURL: TURI;
begin
  LURL := TURI.Create(AURL);
  FScheme := LURL.Scheme;
  FHost := LURL.Host;
  FPort := LURL.Port;
  FUserName := LURL.Username;
  FPassword := LURL.Password;
  FCredential := TCredentialsStorage.TCredential.Create(TAuthTargetType.Proxy, '', '', FUserName, FPassword);
end;

constructor TProxySettings.Create(const AHost: string; APort: Integer; const AUserName, APassword, AScheme: string);
begin
  FScheme := AScheme;
  FHost := AHost;
  FPort := APort;
  FUserName := AUsername;
  FPassword := APassword;
  FCredential := TCredentialsStorage.TCredential.Create(TAuthTargetType.Proxy, '', '', FUserName, FPassword);
end;

function TProxySettings.GetCredential: TCredentialsStorage.TCredential;
begin
  Result := TCredentialsStorage.TCredential.Create(TAuthTargetType.Proxy, '',  '', FUserName, FPassword);
end;

{ TURLHeaders.TEnumerator }

procedure TURLHeaders.TEnumerator.Create(const AHeaders: TURLHeaders);
begin
  FHeaders := AHeaders;
  FIndex := -1;
end;

function TURLHeaders.TEnumerator.GetCurrent: TNetHeader;
begin
  Result := FHeaders.FHeaders[FIndex];
end;

function TURLHeaders.TEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < Length(FHeaders.FHeaders);
end;

{ TURLHeaders.TValueList }

constructor TURLHeaders.TValueList.Create(AHeaders: TURLHeaders; const AName: string);
begin
  inherited Create;
  FHeaders := AHeaders;
  FList := THeaderValueList.Create(FHeaders.Value[AName]);
  FName := AName;
end;

destructor TURLHeaders.TValueList.Destroy;
begin
  FHeaders.Value[FName] := FList.ToString();
  FList.Free;
  inherited Destroy;
end;

function TURLHeaders.TValueList.SetSubject(const AValue: string): TValueList;
begin
  FList.Subject := AValue;
  Result := Self;
end;

function TURLHeaders.TValueList.Clear: TValueList;
begin
  FList.Clear;
  Result := Self;
end;

function TURLHeaders.TValueList.Delete(AIndex: Integer): TValueList;
begin
  FList.Delete(AIndex);
  Result := Self;
end;

function TURLHeaders.TValueList.Add(const AName: string): TValueList;
begin
  FList.Add(AName);
  Result := Self;
end;

function TURLHeaders.TValueList.Add(const AName, AValue: string;
  AQuoteVal: Boolean): TValueList;
begin
  FList.Add(AName, AValue, AQuoteVal);
  Result := Self;
end;

function TURLHeaders.TValueList.&End: TURLHeaders;
begin
  Result := FHeaders;
  Destroy;
end;

{ TURLHeaders.TAcceptList }

constructor TURLHeaders.TAcceptList.Create(AHeaders: TURLHeaders;
  const AName: string);
begin
  inherited Create;
  FHeaders := AHeaders;
  FList := TAcceptValueList.Create(FHeaders.Value[AName]);
  FName := AName;
end;

destructor TURLHeaders.TAcceptList.Destroy;
begin
  FHeaders.Value[FName] := FList.ToString();
  FList.Free;
  inherited Destroy;
end;

function TURLHeaders.TAcceptList.Clear: TAcceptList;
begin
  FList.Clear;
  Result := Self;
end;

function TURLHeaders.TAcceptList.Delete(AIndex: Integer): TAcceptList;
begin
  FList.Delete(AIndex);
  Result := Self;
end;

function TURLHeaders.TAcceptList.Add(const AName: string; AWeight: Double;
  AExtra: TStrings): TAcceptList;
begin
  FList.Add(AName, AWeight, AExtra);
  Result := Self;
end;

function TURLHeaders.TAcceptList.&End: TURLHeaders;
begin
  Result := FHeaders;
  Destroy;
end;

{ TURLHeaders }

procedure TURLHeaders.CheckRange(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FHeaders)) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange) at ReturnAddress;
end;

procedure TURLHeaders.AssignTo(ADest: TPersistent);
var
  LPair: TNetHeader;
begin
  if ADest is TStrings then
  begin
    TStrings(ADest).BeginUpdate;
    try
      TStrings(ADest).Clear;
      for LPair in FHeaders do
        TStrings(ADest).AddPair(LPair.Name, LPair.Value);
    finally
      TStrings(ADest).EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TURLHeaders.Assign(ASource: TPersistent);
var
  i: Integer;
begin
  if ASource is TURLHeaders then
    Assign(TURLHeaders(ASource).FHeaders)
  else if ASource is TStrings then
  begin
    Clear;
    for i := 0 to TStrings(ASource).Count - 1 do
      Value[TStrings(ASource).Names[i]] := TStrings(ASource).ValueFromIndex[i];
  end
  else
    inherited;
end;

procedure TURLHeaders.Assign(const AHeaders: TNetHeaders);
begin
  FHeaders := Copy(AHeaders);
end;

function TURLHeaders.FindItem(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(FHeaders) - 1 do
    if string.CompareText(FHeaders[I].Name, AName) = 0 then
      Exit(I);
  Exit(-1);
end;

function TURLHeaders.Add(const AHeader: TNetHeader): TURLHeaders;
begin
  Value[AHeader.Name] := AHeader.Value;
  Result := Self;
end;

function TURLHeaders.Add(const AName, AValue: string): TURLHeaders;
begin
  Result := Add(TNetHeader.Create(AName, AValue));
end;

function TURLHeaders.Append(const AHeaders: TNetHeaders): TURLHeaders;
begin
  FHeaders := FHeaders + AHeaders;
  Result := Self;
end;

function TURLHeaders.Append(const AHeaders: TURLHeaders): TURLHeaders;
begin
  Result := Append(AHeaders.Headers);
end;

function TURLHeaders.Clear: TURLHeaders;
begin
  SetLength(FHeaders, 0);
  Result := Self;
end;

function TURLHeaders.Delete(const AName: string): TURLHeaders;
var
  I: Integer;
begin
  repeat
    I := FindItem(AName);
    if I >= 0 then
      Delete(I);
  until I < 0;
  Result := Self;
end;

function TURLHeaders.Delete(AIndex: Integer): TURLHeaders;
begin
  CheckRange(AIndex);
  System.Delete(FHeaders, AIndex, 1);
  Result := Self;
end;

function TURLHeaders.GetCount: Integer;
begin
  Result := Length(FHeaders);
end;

function TURLHeaders.GetNames(AIndex: Integer): string;
begin
  CheckRange(AIndex);
  Result := FHeaders[AIndex].Name;
end;

function TURLHeaders.GetValues(AIndex: Integer): string;
begin
  CheckRange(AIndex);
  Result := FHeaders[AIndex].Value;
end;

function TURLHeaders.GetValue(const AName: string): string;
var
  I: Integer;
begin
  I := FindItem(AName);
  if I >= 0 then
    Result := FHeaders[I].Value
  else
    Result := '';
end;

procedure TURLHeaders.SetValue(const AName, AValue: string);
var
  I: Integer;
begin
  I := FindItem(AName);
  if I < 0 then
  begin
    I := Length(FHeaders);
    SetLength(FHeaders, I + 1);
    FHeaders[I].Name := AName;
  end;
  FHeaders[I].Value := AValue;
end;

function TURLHeaders.ValueList(const AName: string): TValueList;
begin
  Result := TValueList.Create(Self, AName);
end;

function TURLHeaders.AcceptList(const AName: string): TAcceptList;
begin
  Result := TAcceptList.Create(Self, AName);
end;

function TURLHeaders.GetEnumerator: TEnumerator;
begin
  Result.Create(Self);
end;

function TURLHeaders.ToString: string;
var
  LItem: TNetHeader;
begin
  Result := '';
  for LItem in Self do
    Result := Result + LItem.Name + ': ' + LItem.Value + #13#10;
end;

{ TURLClient }

constructor TURLClient.Create;
begin
  inherited;
  FCustomHeaders := TURLHeaders.Create;
  FCustomHeaders.Add(TNetHeader.Create(sUserAgent, DefaultUserAgent));
  FInternalCredentialsStorage := TCredentialsStorage.Create;
  FInstances := TObjectDictionary<string, TURLClient>.Create;
  FCredentialsStorage := FInternalCredentialsStorage;
  FConnectionTimeout := DefaultConnectionTimeout;
  FSendTimeout := DefaultSendTimeout;
  FResponseTimeout := DefaultResponseTimeout;
end;

function TURLClient.GetCredentials(AuthTarget: TAuthTargetType;
  const ARealm, URL: string): TCredentialsStorage.TCredentialArray;
begin
  Result := TCredentialsStorage.SortCredentials(FCredentialsStorage.FindCredentials(AuthTarget, ARealm, URL));
end;

function TURLClient.GetCustomHeaderValue(const Name: string): string;
begin
  Result := FCustomHeaders.Value[Name];
end;

class function TURLClient.GetInstance(const AScheme: string): TURLClient;
begin
  Result := TURLSchemes.GetURLClientInstance(AScheme);
end;

function TURLClient.GetInternalInstance(const AScheme: string): TURLClient;

  function IsSupportedScheme(const AScheme: string): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to High(SupportedSchemes) do
      if string.CompareText(SupportedSchemes[I], AScheme) = 0 then
        Exit(True);
    Result := False;
  end;

begin
  if IsSupportedScheme(AScheme) then
    Result := Self
  else
  begin
    TMonitor.Enter(FInstances);
    try
      if not FInstances.TryGetValue(AScheme, Result) then
      begin
        Result := GetInstance(AScheme);
        if Result <> nil then
          FInstances.Add(AScheme, Result)
        else
          raise ENetURIClientException.CreateResFmt(@SSchemeNotRegistered, [AScheme]);
      end;
    finally
      TMonitor.Exit(FInstances);
    end;
  end;
end;

function TURLClient.GetUserAgent: string;
begin
  Result := GetCustomHeaderValue(sUserAgent);
end;

class function TURLClient.CreateInstance: TURLClient;
begin
  raise ENetURIClientException.CreateRes(@SNetPlatformFunctionNotImplemented);
end;

destructor TURLClient.Destroy;
begin
  FInstances.Free;
  FInternalCredentialsStorage.Free;
  FCustomHeaders.Free;
  inherited;
end;

procedure TURLClient.DoAuthCallback(AnAuthTarget: TAuthTargetType; const ARealm, AURL: string;
  var AUserName, APassword: string; var AbortAuth: Boolean; var Persistence: TAuthPersistenceType);
begin
  if Assigned(FAuthCallback) then
    FAuthCallback(Self, AnAuthTarget, ARealm, AURL, AUserName, APassword, AbortAuth, Persistence)
  else if Assigned(FAuthEvent) then
    FAuthEvent(Self, AnAuthTarget, ARealm, AURL, AUserName, APassword, AbortAuth, Persistence);
end;

function TURLClient.DoExecute(const ARequestMethod: string; const AURI: TURI; const ASourceStream,
  AContentStream: TStream; const AHeaders: TNetHeaders): IURLResponse;
begin
  raise ENetURIClientException.CreateRes(@SNetSchemeFunctionNotImplemented);
end;

function TURLClient.DoExecuteAsync(const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent;
  const ARequestMethod: string; const AURI: TURI; const ASourceStream, AContentStream: TStream;
  const AHeaders: TNetHeaders; AOwnsSourceStream: Boolean): IAsyncResult;
begin
  raise ENetURIClientException.CreateRes(@SNetSchemeFunctionNotImplemented);
end;

function TURLClient.DoGetRequestInstance(const ARequestMethod: string; const AURI: TURI): IURLRequest;
begin
  raise ENetURIClientException.CreateRes(@SNetSchemeFunctionNotImplemented);
end;

function TURLClient.GetRequest(const ARequestMethod: string; const AURI: TURI): IURLRequest;
begin
  Result := DoGetRequestInstance(ARequestMethod, AURI);
end;

function TURLClient.GetRequest(const ARequestMethod, AURI: string): IURLRequest;
begin
  Result := DoGetRequestInstance(ARequestMethod, TURI.Create(AURI));
end;

function TURLClient.DoGetResponseInstance(const AContext: TObject; const AProc: TProc; const AsyncCallback: TAsyncCallback;
  const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: IURLRequest; const AContentStream: TStream): IAsyncResult;
begin
  raise ENetURIClientException.CreateRes(@SNetSchemeFunctionNotImplemented);
end;

class function TURLClient.EndAsyncURL(const AAsyncResult: IAsyncResult): IURLResponse;
begin
  (AAsyncResult as TURLResponse).WaitForCompletion;
  Result := AAsyncResult as IURLResponse;
end;

class function TURLClient.EndAsyncURL(const AAsyncResult: IURLResponse): IURLResponse;
begin
  (AAsyncResult as TURLResponse).WaitForCompletion;
  Result := AAsyncResult;
end;

function TURLClient.Execute(const ARequestMethod: string; const AURI: TURI; const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IURLResponse;
begin
  Result := GetInternalInstance(AURI.Scheme).DoExecute(ARequestMethod, AURI, ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.Execute(const ARequestMethod, AURIStr: string; const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IURLResponse;
begin
  Result := Execute(ARequestMethod, TURI.Create(AURIStr), ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.BeginExecute(const AsyncCallback: TAsyncCallback; const ARequestMethod: string; const AURI: TURI;
  const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IAsyncResult;
begin
  Result := GetInternalInstance(AURI.Scheme).DoExecuteAsync(AsyncCallback, nil, ARequestMethod, AURI, ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.BeginExecute(const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequestMethod: string;
  const AURI: TURI; const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IAsyncResult;
begin
  Result := GetInternalInstance(AURI.Scheme).DoExecuteAsync(nil, AsyncCallbackEvent, ARequestMethod, AURI, ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.BeginExecute(const AsyncCallback: TAsyncCallback; const ARequestMethod, AURIStr: string;
  const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IAsyncResult;
begin
  Result := BeginExecute(AsyncCallback, ARequestMethod, TURI.Create(AURIStr), ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.BeginExecute(const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequestMethod, AURIStr: string;
  const ASourceStream, AContentStream: TStream; const AHeaders: TNetHeaders): IAsyncResult;
begin
  Result := BeginExecute(AsyncCallbackEvent, ARequestMethod, TURI.Create(AURIStr), ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.BeginExecute(const ARequestMethod: string; const AURI: TURI; const ASourceStream: TStream = nil;
    const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult;
begin
  Result := GetInternalInstance(AURI.Scheme).DoExecuteAsync(nil, nil, ARequestMethod, AURI, ASourceStream, AContentStream, AHeaders);
end;

function TURLClient.BeginExecute(const ARequestMethod: string; const AURIStr: string; const ASourceStream: TStream = nil;
  const AContentStream: TStream = nil; const AHeaders: TNetHeaders = nil): IAsyncResult;
begin
  Result := BeginExecute(ARequestMethod, TURI.Create(AURIStr), ASourceStream, AContentStream, AHeaders);
end;

procedure TURLClient.SetCustomHeaderValue(const Name, Value: string);
begin
  FCustomHeaders.Value[Name] := Value;
end;

procedure TURLClient.SetConnectionTimeout(const Value: Integer);
begin
  FConnectionTimeout := Value;
end;

procedure TURLClient.SetCredentialsStorage(const Value: TCredentialsStorage);
begin
  if Value = nil then
  begin
    FCredentialsStorage := FInternalCredentialsStorage;
    //FCredentialsStorage.ClearCredentials; //TODO -ojulianm: decide if we clear the cached credentials
  end
  else
    FCredentialsStorage := Value;
end;

procedure TURLClient.SetProxySettings(const Value: TProxySettings);
var
  LCredential: TCredentialsStorage.TCredential;
  LURI: TURI;
begin
  FProxySettings := Value;

  // If there is a user information, we create the credential needed for authenticating, and add it to the storage.
  if Value.Username <> '' then
  begin
    LURI.FHost := Value.FHost;
    LURI.FPort := Value.FPort;
    LURI.FScheme := Value.FScheme.ToLower;
    if LURI.FScheme = '' then
      LURI.FScheme := TURI.SCHEME_HTTP;
    LCredential := TCredentialsStorage.TCredential.Create(TAuthTargetType.Proxy, '', LURI.ToString,
      Value.FUserName, Value.FPassword);
    FCredentialsStorage.AddCredential(LCredential);
  end;
end;

procedure TURLClient.SetResponseTimeout(const Value: Integer);
begin
  FResponseTimeout := Value;
end;

procedure TURLClient.SetSendTimeout(const Value: Integer);
begin
  FSendTimeout := Value;
end;

procedure TURLClient.SetUserAgent(const Value: string);
begin
  SetCustomHeaderValue(sUserAgent, Value);
end;

function TURLClient.SupportedSchemes: TArray<string>;
begin
  Result := [];
end;


{ TURLClientSchemes }

class constructor TURLSchemes.Create;
begin
  FSchemeClients := TDictionary<string, TURLClientClass>.Create;
end;

class destructor TURLSchemes.Destroy;
begin
  FSchemeClients.Free;
end;

class function TURLSchemes.GetURLClientInstance(const AScheme: string): TURLClient;
var
  LClientClass: TURLClientClass;
begin
  Result := nil;
  FSchemeClients.TryGetValue(AScheme.ToUpper, LClientClass);
  if LClientClass <> nil then
    Result := LClientClass.CreateInstance;
end;

class procedure TURLSchemes.RegisterURLClientScheme(const AURLClientClass: TURLClientClass; const AScheme: string);
var
  LScheme: string;
begin
  LScheme := AScheme.ToUpper;
  if FSchemeClients.ContainsKey(LScheme) then
    raise ENetException.CreateResFmt(@SSchemeAlreadyRegistered, [LScheme, 'Client']); // Do not translate
  FSchemeClients.Add(LScheme, AURLClientClass);
end;

class procedure TURLSchemes.UnRegisterURLClientScheme(const AScheme: string);
begin
  FSchemeClients.Remove(AScheme.ToUpper);
end;

{ TURLRequest }

constructor TURLRequest.Create(const AClient: TURLClient; const AMethodString: string; const AURI: TURI);
begin
  inherited Create;
  FURL := AURI;
  FClient := AClient;
  FMethodString := AMethodString;

  FConnectionTimeout := FClient.FConnectionTimeout;
  FSendTimeout := FClient.FSendTimeout;
  FResponseTimeout := FClient.FResponseTimeout;

  FLocalCredential := TCredentialsStorage.TCredential.Create(TAuthTargetType.Server, '',
    AURI.Host, AURI.Username, AUri.Password);
end;

destructor TURLRequest.Destroy;
begin
  inherited;
end;

function TURLRequest.GetCredential: TCredentialsStorage.TCredential;
begin
  Result := FLocalCredential;
end;

function TURLRequest.GetMethodString: string;
begin
  Result := FMethodString;
end;

function TURLRequest.GetSourceStream: TStream;
begin
  Result := FSourceStream;
end;

function TURLRequest.GetURL: TURI;
begin
  Result := FURL;
end;

procedure TURLRequest.SetCredential(const ACredential: TCredentialsStorage.TCredential);
begin
  FLocalCredential := ACredential;
end;

procedure TURLRequest.SetConnectionTimeout(const Value: Integer);
begin
  FConnectionTimeout := Value;
end;

procedure TURLRequest.SetCredential(const AUserName, APassword: string);
begin
  SetCredential(TCredentialsStorage.TCredential.Create(TAuthTargetType.Server, '', FURL.Host, AUserName, APassword));
end;

procedure TURLRequest.SetMethodString(const AValue: string);
begin
  if FMethodString <> '' then
    raise ENetURIRequestException.CreateRes(@SNetUriMethodAlreadyAssigned);
  FMethodString := AValue.ToUpper;
end;

procedure TURLRequest.SetResponseTimeout(const Value: Integer);
begin
  FResponseTimeout := Value;
end;

procedure TURLRequest.SetSendTimeout(const Value: Integer);
begin
  if FSendTimeout <> Value then
  begin
    FSendTimeout := Value;
    FSendTimeoutChanged := True;
  end;
end;

function TURLRequest.GetSendTimeout: Integer;
begin
  if FSendTimeoutChanged then
    Result := FSendTimeout
  else
    Result := ResponseTimeout;
end;

procedure TURLRequest.SetSourceStream(const ASourceStream: TStream);
begin
  FSourceStream := ASourceStream;
end;

procedure TURLRequest.SetURL(const AValue: TURI);
begin
  if FURL.Host <> '' then
    raise ENetURIRequestException.CreateRes(@SNetUriURLAlreadyAssigned);
  FURL := AValue;
end;

function TURLRequest.GetIsCancelled: Boolean;
begin
  Result := FCancelled;
end;

procedure TURLRequest.DoCancel;
begin
  // nothing
end;

procedure TURLRequest.DoResetCancel;
begin
  FCancelled := False;
end;

procedure TURLRequest.Cancel;
begin
  if not FCancelled then
  begin
    FCancelled := True;
    DoCancel;
  end;
end;

{ TCredentialsStorage.TCredential }

constructor TCredentialsStorage.TCredential.Create(AnAuthTarget: TAuthTargetType;
  const ARealm, AURL, AUserName, APassword: string);
begin
  AuthTarget := AnAuthTarget;
  Realm := ARealm;
  URL := AURL;
  UserName := AUserName;
  Password := APassword;
end;

function TCredentialsStorage.TCredential.IsEmpty: Boolean;
begin
  Result := (UserName = '') and (Password = '');
end;

{ TCredentialsStorage }

function TCredentialsStorage.AddCredential(const ACredential: TCredential): Boolean;
var
  LCredentials: TCredentialArray;
begin
  if (ACredential.AuthTarget = TAuthTargetType.Server) and ACredential.IsEmpty then
    raise ENetCredentialException.CreateRes(@SCredentialInvalidUserPassword);
  LCredentials := FindCredentials(ACredential.AuthTarget, ACredential.Realm, ACredential.URL, ACredential.UserName);
  if Length(LCredentials) = 0 then
  begin
    TMonitor.Enter(FCredentials);
    try
      FCredentials.Add(ACredential);
      Result := True;
    finally
      TMonitor.Exit(FCredentials);
    end;
  end
  else
    Result := False;
end;

function TCredentialsStorage.FindCredentials(AnAuthTargetType: TAuthTargetType;
  const ARealm, AURL, AUser: string): TCredentialArray;
var
  LCredential: TCredential;
  LValid: Boolean;
//  LValidURL: Boolean;
  LHost: string;
  LCredentialHost: string;
begin
                                                                                             
  Result := [];
  TMonitor.Enter(FCredentials);
  try
    if AURL = '' then
      LHost := ''
    else
      LHost := TURI.Create(AURL).Host;
    for LCredential in FCredentials do
    begin
      if LCredential.URL = '' then
        LCredentialHost := ''
      else
        LCredentialHost := TURI.Create(LCredential.URL).Host;
      LValid := AnAuthTargetType = LCredential.AuthTarget;
      LValid := LValid and ((ARealm = '') or ((ARealm <> '') and ((LCredential.Realm = '') or
                                                                  ((LCredential.Realm <> '') and
                                                                   (string.CompareText(LCredential.Realm, ARealm) = 0)
                                                                   )
                                                                  )));
//      LValidURL := ((LCredential.URL <> '') and (AURL.StartsWith(LCredential.URL, True)));
//      LValid := LValid and ((AURL   = '') or ((AURL   <> '') and ((LCredential.URL = '') or
//                                                                  ((LCredential.URL <> '') and (AURL.StartsWith(LCredential.URL, True)))
//                                                                  )));
      LValid := LValid and ((AURL   = '') or ((AURL   <> '') and ((LCredentialHost = '') or
                                                                  ((LCredentialHost <> '') and (LHost.StartsWith(LCredentialHost, True)))
                                                                  )));
      LValid := LValid and ((AUser  = '') or (string.CompareText(LCredential.UserName, AUser) = 0));

      if LValid then
        Result := Result + [LCredential];
    end;
  finally
    TMonitor.Exit(FCredentials);
  end;
end;

class constructor TCredentialsStorage.Create;
begin
  FCredComparer := TCredentialComparer.Create;
end;

class destructor TCredentialsStorage.Destroy;
begin
  FCredComparer.Free;
end;

procedure TCredentialsStorage.ClearCredentials;
begin
  TMonitor.Enter(FCredentials);
  try
    FCredentials.Clear;
  finally
    TMonitor.Exit(FCredentials);
  end;
end;

constructor TCredentialsStorage.Create;
begin
  inherited;
  FCredentials := TList<TCredential>.Create;
end;

destructor TCredentialsStorage.Destroy;
begin
  FCredentials.Free;
  inherited;
end;

function TCredentialsStorage.FindAccurateCredential(AnAuthTargetType: TAuthTargetType;
  const ARealm, AURL, AUser: string): TCredential;
var
  LCredential: TCredential;
  LValid: Boolean;
begin
  Result := Default(TCredential);
  TMonitor.Enter(FCredentials);
  try
    for LCredential in FCredentials do
    begin
      LValid := AnAuthTargetType = LCredential.AuthTarget;
      LValid := LValid and ((ARealm = '') or (string.CompareText(LCredential.Realm, ARealm) = 0));
      LValid := LValid and ((AURL   = '') or AURL.StartsWith(LCredential.URL));
      LValid := LValid and ((AUser  = '') or (string.CompareText(LCredential.UserName, AUser) = 0));

      if LValid and (Result.URL.Length < LCredential.URL.Length) then
        Result := LCredential;
    end;
  finally
    TMonitor.Exit(FCredentials);
  end;
end;

function TCredentialsStorage.GetCredentials: TCredentialArray;
begin
  Result := FCredentials.ToArray;
end;

function TCredentialsStorage.RemoveCredential(AnAuthTargetType: TAuthTargetType;
  const ARealm, AURL, AUser: string): Boolean;
var
  I: Integer;
  LEqual: Boolean;
begin
  Result := False;
  TMonitor.Enter(FCredentials);
  try
    for I := FCredentials.Count - 1 downto 0 do
    begin
      LEqual := AnAuthTargetType = FCredentials[I].AuthTarget;
      LEqual := LEqual and (string.CompareText(FCredentials[I].Realm, ARealm) = 0);
      LEqual := LEqual and (string.CompareText(FCredentials[I].URL, AURL) = 0);
      LEqual := LEqual and (string.CompareText(FCredentials[I].UserName, AUser) = 0);
      if LEqual then
      begin
        FCredentials.Delete(I);
        Result := True;
      end;
    end;
  finally
    TMonitor.Exit(FCredentials);
  end;
end;

class function TCredentialsStorage.SortCredentials(
  const ACredentials: TCredentialsStorage.TCredentialArray): TCredentialsStorage.TCredentialArray;
begin
  Result := ACredentials;
  TArray.Sort<TCredentialsStorage.TCredential>(Result, FCredComparer);
end;

{ TCredentialsStorage.TCredentialComparer }

function TCredentialsStorage.TCredentialComparer.Compare(const Left, Right: TCredential): Integer;
begin
  Result := 0;
  if (Left.Realm <> '')  then
  begin
    if (Right.Realm = '') then
      Result := -1
    else
      Result := string.CompareText(Left.Realm, Right.Realm);
  end
  else
  begin
    if (Right.Realm <> '') then
      Result := 1;
  end;

  if Result = 0 then
    Result := - string.CompareText(Left.URL, Right.URL);
  if Result = 0 then
    Result := string.CompareText(Left.UserName, Right.UserName);
end;

{ TURLResponse }

procedure TURLResponse.AsyncDispatch;
begin
  if Assigned(FProc) then
    FProc;
end;

procedure TURLResponse.Complete;
begin
  inherited Complete;
  if IsCancelled and (FInvokingException <> nil) then
    FreeAndNil(FInvokingException);
  try
    if Assigned(FAsyncCallback) then
      FAsyncCallback(Self as IAsyncResult)
    else if Assigned(FAsyncCallbackEvent) then
      FAsyncCallbackEvent(Self as IAsyncResult);
  finally
    FProc := nil;
    FAsyncCallback := nil;
  end;
end;

constructor TURLResponse.Create(const AContext: TObject; const ARequest: IURLRequest; const AContentStream: TStream);
begin
  inherited Create(AContext);
  FRequest := ARequest;
  if AContentStream <> nil then
    FStream := AContentStream
  else
  begin
    FInternalStream := DoCreateInternalStream;
    FStream := FInternalStream;
  end;
end;

constructor TURLResponse.Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback;
  const AAsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: IURLRequest; const AContentStream: TStream);
begin
  Create(AContext, ARequest, AContentStream);
  FAsyncCallback := AAsyncCallback;
  FAsyncCallbackEvent := AAsyncCallbackEvent;
  FProc := AProc;
end;

destructor TURLResponse.Destroy;
begin
  inherited;
  FInternalStream.Free;
end;

function TURLResponse.DoCancel: Boolean;
begin
  Result := FRequest <> nil;
  if Result then
    FRequest.Cancel;
end;

function TURLResponse.DoCreateInternalStream: TStream;
begin
  Result := TMemoryStream.Create;
end;

function TURLResponse.GetContentStream: TStream;
begin
  Result := FStream;
end;

procedure TURLResponse.Schedule;
begin
  // If FProc is assigned, this is an asynchronous response. Otherwise process it sequentially
  if Assigned(FProc) then
    TTask.Run(DoAsyncDispatch)
  else
    DoAsyncDispatch;
end;

function TURLResponse.GetAsyncResult: IAsyncResult;
begin
  Result := Self;
end;

{ TPunyCode }

class function TPunyCode.DoEncode(const AString: UCS4String): UCS4String;
var
  N, LDelta, LBias, b, q, m, k, t: Cardinal;
  I: Integer;
  h: Integer;
  LenAStr: Integer;

begin
  Result := nil;
  if AString = nil then
    Exit;
  try
    N := INITIAL_N;
    LBias := INITIAL_BIAS;
    LenAStr := Length(AString) - 1;
    for I := 0 to LenAStr - 1 do
      if IsBasic(AString, I, INITIAL_N) then
        Result := Result + [AString[I]];
    b := Length(Result);
    if (Length(Result) < LenAStr) and (Length(Result) >= 0) then
      Result := Result + [Cardinal(Delimiter)];
    h := b;
    LDelta := 0;
    while h < LenAStr do
    begin
      m := GetMinCodePoint(N, AString);
      LDelta := LDelta + (m - N) * Cardinal(h + 1);
      N := m;
      for I := 0 to LenAStr - 1 do
      begin
        if IsBasic(AString, I, N) then
          Inc(LDelta)
        else if AString[I] = N then
        begin
          q := LDelta;
          k := BASE;
          while k <= MAX_INT do
          begin
            if k <= (LBias + TMIN) then
              t := TMIN
            else if k >= (LBias + TMAX) then
              t := TMAX
            else
              t := k - LBias;
            if q < t then
              break;
            Result := Result + [Digit2Codepoint(t + ((q - t) mod (BASE - t)))];
            q := (q - t) div (BASE - t);
            Inc(k, BASE);
          end;
          Result := Result + [Digit2Codepoint(q)];
          LBias := Adapt(LDelta, h + 1, Cardinal(h) = b);
          LDelta := 0;
          Inc(h);
        end;
      end;
      Inc(LDelta);
      Inc(n);
    end;
    Result := Result + [0];
  except
    Result := AString;
  end;
end;

class function TPunyCode.DoDecode(const AString: UCS4String): UCS4String;
  function LastIndexOf(const ADelimiter: UCS4Char; const AString: UCS4String): Integer;
  var
    Pos: Integer;
  begin
    Result := -1;
    for Pos := Length(AString) - 1 downto 0 do
      if AString[Pos] = ADelimiter then
      begin
        Result := Pos;
        Break;
      end;
  end;

var
  J: Integer;
  I, OldI, N, LBias, w, k, t: Cardinal;
  LPos, textLen, LCurrentLen: Integer;
  LDigit: Cardinal;
  c: UCS4Char;
begin
  Result := [];
  if Length(AString) = 0 then
    Exit;
  try
    N := INITIAL_N;
    LBias := INITIAL_BIAS;
    LPos := LastIndexOf(UCS4Char(Delimiter), AString);
    if LPos < 0 then
      Exit(AString);
    for J := 0 to (LPos - 1) do
      if AString[J] >= INITIAL_N then
        Exit;
    Result := Copy(AString, 0, LPos);// + 1);
    I := 0;
    Inc(LPos);
    textLen := Length(AString) - 1;
    while LPos < textLen do
    begin
      OldI := I;
      w := 1;
      k := BASE;
      while (k <= MAX_INT) and (LPos < textLen) do
      begin
        c := AString[LPos];
        Inc(LPos);
        LDigit := Codepoint2Digit(c);
        if ((LDigit >= BASE) or (LDigit > ((MAX_INT - i) / w))) then
          Exit(Result);
        I := I + LDigit * w;
        if k <= LBias then
          t := TMIN
        else
          if k >= (LBias + TMAX) then
            t := TMAX
          else
            t := k - LBias;
        if LDigit < t then
          break;
        if w > (MAX_INT / (base - t)) then
          Exit(nil);
        w := w * (BASE - t);
        Inc(k, BASE);
      end;
      LCurrentLen := Length(Result) + 1;
      LBias := Adapt(I - OldI, LCurrentLen, OldI = 0);
      if (I / LCurrentLen) > (MAX_INT - N) then
        Exit(nil);
      N := N + I div Cardinal(LCurrentLen);
      I := I mod Cardinal(LCurrentLen);
      if IsBasic([N], 0, INITIAL_N) then
        Exit(nil);
      Result := Copy(Result, 0, I) + [N] + Copy(Result, I, Length(Result) - Integer(I));
      Inc(I);
    end;
    Result := Result + [0];
  except
    Result := AString;
  end;
end;

class function TPunyCode.DoEncode(const AString: string): string;
var
  LStr, LResult: UCS4String;
begin
  LStr := UnicodeStringToUCS4String(AString);
  LResult := DoEncode(LStr);
  if LResult <> nil then
    Result := UCS4StringToUnicodeString(LResult)
  else
    Result := AString;
end;

class function TPunyCode.DoDecode(const AString: string): string;
var
  LStr, LResult: UCS4String;
begin
  LStr := UnicodeStringToUCS4String(AString);
  LResult := DoDecode(LStr);
  if LResult <> nil then
    Result := UCS4StringToUnicodeString(LResult)
  else
    Result := AString;
end;

class function TPunyCode.IsBasic(const AString: UCS4String; const AIndex, AMinLimit: Cardinal): Boolean;
begin
  Result := AString[AIndex] < AMinLimit;
end;

class function TPunyCode.Adapt(const ADelta, ANumPoints: Cardinal; const FirstTime: Boolean): Cardinal;
var
  k, dt: Cardinal;
begin
  if FirstTime = True then
    dt := ADelta div DAMP
  else
    dt := ADelta div 2;
  dt := dt + (dt div ANumPoints);
  k := 0;
  while dt > (((BASE - TMIN) * TMAX) div 2) do
  begin
    dt := dt div (BASE - TMIN);
    Inc(k, BASE);
  end;
  Result := k + (((BASE - TMIN + 1) * dt) div (dt + SKEW));
end;

class function TPunyCode.Encode(const AString: string): string;
var
  Aux: string;
begin
  Result := DoEncode(AString);
  Aux := DoDecode(Result);
  if Aux <> AString then
    Result := AString;
end;

class function TPunyCode.Decode(const AString: string): string;
var
  Aux: string;
begin
  Result := DoDecode(AString);
  Aux := DoEncode(Result);
  if Aux <> AString then
    Result := AString;
end;

class function TPunyCode.Digit2Codepoint(const ADigit: Cardinal): Cardinal;
begin
  Result := 0;
  if ADigit < 26 then
    Result := ADigit + 97
  else if ADigit < 36 then
    Result := ADigit - 26 + 48;
end;

class function TPunyCode.Codepoint2Digit(const ACodePoint: Cardinal): Cardinal;
begin
  Result := BASE;
  if (ACodePoint - 48) < 10 then
    Result := ACodePoint - 22
  else if (ACodePoint - 65) < 26 then
    Result := ACodePoint - 65
  else if (ACodePoint - 97) < 26 then
    Result := ACodePoint - 97;
end;

class function TPunyCode.GetMinCodePoint(const AMinLimit: Cardinal; const AString: UCS4String): Cardinal;
var
  I: Integer;
  LMinCandidate: Cardinal;
begin
  Result := Cardinal.MaxValue;
  for I := 0 to Length(AString) - 1 do
  begin
    LMinCandidate := AString[I];
    if (LMinCandidate >= AMinLimit) and (LMinCandidate < Result) then
      Result := LMinCandidate;
  end;
end;

{ TCertificate }

function TCertificate.IsEmpty: Boolean;
begin
  Result := (Expiry = 0) and (Subject = '') and (Issuer = '') and
    (CertName = '') and (SerialNum = '') and (PublicKey = '');
end;

{ TAsyncReadStream.TStreamAsyncResult }

function TAsyncReadStream.TStreamAsyncResult.GetStream: TAsyncReadStream;
begin
  Result := Context as TAsyncReadStream;
end;

procedure TAsyncReadStream.TStreamAsyncResult.Schedule;
begin
  TTask.Run(DoAsyncDispatch);
end;

procedure TAsyncReadStream.TStreamAsyncResult.AsyncDispatch;
begin
  Stream.Populate;
end;

procedure TAsyncReadStream.TStreamAsyncResult.Complete;
begin
  inherited Complete;
  Stream.Provide;
end;

function TAsyncReadStream.TStreamAsyncResult.DoCancel: Boolean;
begin
  Result := Stream.DoCancel;
end;

{ TAsyncReadStream }

class constructor TAsyncReadStream.Create;
begin
  FActiveStreams := TThreadList.Create;
end;

class destructor TAsyncReadStream.Destroy;
var
  LList: TList;
  LStr: Pointer;
begin
  LList := FActiveStreams.LockList;
  try
    for LStr in LList do
    begin
      TAsyncReadStream(LStr).FAsyncResult.Cancel;
      TThread.RemoveQueuedEvents(TAsyncReadStream(LStr).DoProvide);
    end;
  finally
    FActiveStreams.UnlockList;
  end;
  FreeAndNil(FActiveStreams);
end;

constructor TAsyncReadStream.Create(const APopulator, AProvider: TStreamer;
  const ACanceler: TCanceler; ASynchronizeProvide, AFreeOnCompletion: Boolean);
begin
  inherited Create;
  FPopulator := APopulator;
  FProvider := AProvider;
  FCanceler := ACanceler;
  FSynchronizeProvide := ASynchronizeProvide;
  FFreeOnCompletion := AFreeOnCompletion;
  FAsyncResultObj := TStreamAsyncResult.Create(Self);
  FAsyncResult := FAsyncResultObj as IAsyncResult;
  FActiveStreams.Add(Self);
  FOwningThread := TThread.Current.ThreadID;
end;

procedure TAsyncReadStream.AfterConstruction;
begin
  inherited AfterConstruction;
  FAsyncResultObj.Invoke;
end;

destructor TAsyncReadStream.Destroy;
begin
  if FActiveStreams <> nil then
    FActiveStreams.Remove(Self);
  if FAsyncResultObj <> nil then
  begin
    FAsyncResultObj.Cancel;
    FAsyncResultObj.WaitForCompletion;
  end;
  SetReadingStream(nil);
  inherited Destroy;
end;

procedure TAsyncReadStream.SetReadingStream(AReadingStream: TStream);
begin
  FreeAndNil(FReadingStream);
  FReadingStream := AReadingStream;
end;

procedure TAsyncReadStream.Populate;
begin
  try
    inherited SetSize(Int64(0));
    DoPopulate;
    inherited Seek(0, soBeginning);
  except
    if (FAsyncResultObj <> nil) and not FAsyncResultObj.IsCancelled then
      raise;
  end;
end;

procedure TAsyncReadStream.DoPopulate;
begin
  if Assigned(FPopulator) then
    FPopulator(Self);
end;

function TAsyncReadStream.DoCancel: Boolean;
begin
  FProvider := nil;
  if Assigned(FCanceler) then
    Result := FCanceler(Self)
  else
    Result := True;
end;

procedure TAsyncReadStream.Provide;
begin
  try
    if FAsyncResultObj.FInvokingException <> nil then
    begin
      if FSynchronizeProvide then
        // This will raise exception in main thread
        TThread.Synchronize(nil, FAsyncResultObj.WaitForCompletion);
    end
    else if not FAsyncResultObj.IsCancelled then
    begin
      if FSynchronizeProvide then
        TThread.Synchronize(nil, DoProvide)
      else
        DoProvide;
    end;
  finally
    if FFreeOnCompletion then
      Destroy;
  end;
end;

procedure TAsyncReadStream.DoProvide;
begin
  if Assigned(FProvider) then
    FProvider(Self);
end;

procedure TAsyncReadStream.WaitForCompletion;
begin
  if (FOwningThread = TThread.Current.ThreadID) and (FAsyncResultObj <> nil) then
    FAsyncResultObj.WaitForCompletion;
end;

function TAsyncReadStream.Read(var Buffer; Count: Longint): Longint;
begin
  WaitForCompletion;
  if FReadingStream <> nil then
    Result := FReadingStream.Read(Buffer, Count)
  else
    Result := inherited Read(Buffer, Count);
end;

procedure TAsyncReadStream.SaveToStream(Stream: TStream);
begin
  WaitForCompletion;
  if FReadingStream <> nil then
    Stream.CopyFrom(FReadingStream, FReadingStream.Size)
  else
    inherited SaveToStream(Stream);
end;

function TAsyncReadStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  WaitForCompletion;
  if FReadingStream <> nil then
    Result := FReadingStream.Seek(Offset, Origin)
  else
    Result := inherited Seek(Offset, Origin);
end;

procedure TAsyncReadStream.CheckWriting;
begin
  if (FOwningThread = TThread.Current.ThreadID) or (FReadingStream <> nil) then
    raise EWriteError.CreateRes(@SWriteError);
end;

procedure TAsyncReadStream.SetSize(const NewSize: Int64);
begin
  CheckWriting;
  inherited SetSize(NewSize);
end;

procedure TAsyncReadStream.SetSize(NewSize: Longint);
begin
  CheckWriting;
  inherited SetSize(NewSize);
end;

function TAsyncReadStream.Write(const Buffer; Count: Longint): Longint;
begin
  CheckWriting;
  Result := inherited Write(Buffer, Count);
end;

{ TURLStream }

class constructor TURLStream.Create;
begin
  FSyncReqExecutors := TDictionary<string, TSyncReqExecutor>.Create;
end;

class destructor TURLStream.Destroy;
begin
  FreeAndNil(FSyncReqExecutors);
end;

class procedure TURLStream.RegisterSyncReqExecutor(const AScheme: string;
  const AFunc: TSyncReqExecutor);
var
  LScheme: string;
begin
  LScheme := AScheme.ToUpper;
  FSyncReqExecutors.AddOrSetValue(LScheme, AFunc);
end;

class procedure TURLStream.UnRegisterSyncReqExecutor(const AScheme: string);
var
  LScheme: string;
begin
  LScheme := AScheme.ToUpper;
  FSyncReqExecutors.Remove(LScheme);
end;

constructor TURLStream.Create(const AURL: string; const AProvider: TAsyncReadStream.TStreamer;
  ASynchronizeProvide, AFreeOnCompletion: Boolean);
var
  LStream: TStream;
  LURI: TURI;
begin
  if FileExists(AURL) then
    LStream := TFileStream.Create(AURL, fmOpenRead or fmShareDenyWrite)
  else
  begin
    LURI := TURI.Create(AURL);
    if SameText(LURI.Scheme, 'file') then
      LStream := GetFileSchemeStream(LURI)
    else if SameText(LURI.Scheme, 'res') then
      LStream := GetResSchemeStream(LURI)
    else
    begin
      LStream := GetOtherSchemeStream(LURI);
      if (LStream = nil) and FSyncReqExecutors.ContainsKey(LURI.Scheme.ToUpper) then
      begin
        FClient := TURLSchemes.GetURLClientInstance(LURI.Scheme);
        if FClient <> nil then
          FRequest := FClient.GetRequest('GET', LURI);
      end;
    end;
  end;
  if (LStream = nil) and (FRequest = nil) then
    raise ENetURIException.CreateResFmt(@SNetUriInvalid, [AURL]);
  inherited Create(nil, AProvider, nil, ASynchronizeProvide, AFreeOnCompletion);
  SetReadingStream(LStream);
end;

destructor TURLStream.Destroy;
begin
  if (FModule <> 0) and (FModule <> HInstance) then
    FreeLibrary(FModule);
  FRequest := nil;
  FClient.Free;
  inherited Destroy;
end;

function TURLStream.DoCancel: Boolean;
begin
  if FRequest <> nil then
  begin
    FRequest.Cancel;
    Result := FRequest.IsCancelled;
  end
  else
    Result := True;
end;

procedure TURLStream.DoPopulate;
var
  LExec: TSyncReqExecutor;
begin
  if FClient <> nil then
  begin
    LExec := FSyncReqExecutors[FRequest.URL.Scheme.ToUpper];
    LExec(FClient, FRequest, Self, nil);
  end;
end;

function TURLStream.GetFileSchemeStream(const AURI: TURI): TStream;
var
  LFileName: string;
begin
  LFileName := TNetEncoding.URL.URLDecode(AURI.Host);
  if (LFileName <> '') and (LFileName <> '.') and (LFileName <> '..') then
    LFileName := LFileName + ':';
  LFileName := LFileName + TNetEncoding.URL.URLDecode(AURI.Path);
{$IFDEF MSWINDOWS}
  if (LFileName <> '') and (LFileName[1] = '/') then
    LFileName := Copy(LFileName, 2);
{$ENDIF}
  Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
end;

function TURLStream.GetResSchemeStream(const AURI: TURI): TStream;
var
  LStrs: TArray<string>;
  LResMod, LResName: string;
  LResType: NativeUInt;
  LCode: Integer;
begin
  Result := nil;
  LResMod := '';
  LResName := '';
  LResType := NativeUInt(RT_RCDATA);
  LStrs := TNetEncoding.URL.URLDecode(AURI.Path).Split(['/'], TStringSplitOptions.ExcludeLastEmpty);
  if Length(LStrs) = 1 then
    LResName := TNetEncoding.URL.URLDecode(AURI.Host)
  else if Length(LStrs) = 2 then
  begin
    LResMod := TNetEncoding.URL.URLDecode(AURI.Host);
    LResName := LStrs[1];
  end
  else if Length(LStrs) = 3 then
  begin
    LResMod := TNetEncoding.URL.URLDecode(AURI.Host);
    if (LStrs[1] <> '') and (LStrs[1][1] = '#') then
      LStrs[1] := Copy(LStrs[1], 2);
    Val(LStrs[1], LResType, LCode);
    if LCode <> 0 then
      LResType := NativeUInt(PChar(LStrs[1]));
    LResName := LStrs[2];
  end;
  if LResName <> '' then
  begin
    if (LResMod <> '') and (LResMod <> '.') then
    begin
{$IFDEF MSWINDOWS}
      FModule := LoadLibraryEx(PChar(LResMod), 0, LOAD_LIBRARY_AS_DATAFILE);
{$ELSE}
      FModule := LoadLibrary(PChar(LResMod));
{$ENDIF}
      if FModule = 0 then
        RaiseLastOSError;
    end
    else
      FModule := HInstance;
    Result := TResourceStream.Create(FModule, LResName, PChar(LResType));
  end;
end;

function TURLStream.GetOtherSchemeStream(const AURI: TURI): TStream;
begin
  Result := nil;
end;

end.
