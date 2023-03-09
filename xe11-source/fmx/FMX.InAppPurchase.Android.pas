{*******************************************************}
{                                                       }
{           Delphi FireMonkey Mobile Services           }
{                                                       }
{     Implementation of interface for in-app purchases  }
{                                                       }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

// Reference on Android developer site: https://developer.android.com/google/play/billing

unit FMX.InAppPurchase.Android;

interface

{$SCOPEDENUMS ON}

procedure RegisterInAppPurchaseService;
procedure UnRegisterInAppPurchaseService;

implementation

uses
  System.TypInfo, System.Classes, System.SysUtils, System.Generics.Collections, System.DateUtils,
  Androidapi.Consts, Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Embarcadero, Androidapi.JNI.InAppBilling, Androidapi.JNI.App,
  FMX.Platform, FMX.Platform.Android, FMX.Consts, FMX.Helpers.Android, FMX.InAppPurchase;

type
  TPurchase = class;
  TInventory = class;

  TOrderId = string;
  TPurchaseState = (psUnspecified = 0, psPurchased = 1, psPending = 2);

  TSkuId = string;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Alias for type string for localized prices. A price string is the
  ///	  formatted price (exclusive of tax) of the item, including its currency
  ///	  sign.
  ///	</summary>
  {$ENDREGION}
  TSkuPrice = string;

  TProductKind = (Unknown, InApp, Subscription);

  TIABResponseCode = (Ok, UserCancelled, BillingUnavailable, ItemUnavailable, DeveloperError, Error, ItemAlreadyOwned,
    ItemNotOwned, RemoteException, BadResponse, VerificationFailed, SendIntentFailed, UnknownPurchaseResponse,
    MissingToken, UnknownError, SubscriptionsNotAvailable, InvalidConsumption, FeatureNotSupported, ServiceDisconnected,
    ServiceTimeout, ServiceUnavailable);

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents the result of an in-app billing operation. A result is
  ///	  composed of a response code (an enum) and possibly a message (string).
  ///	  You can get those through the Response and Message prroperties,
  ///	  respectively. You can also inquire whether a result is a success or a
  ///	  failure via the IsSuccess and isFailure properties.
  ///	</summary>
  {$ENDREGION}
  TIabResult = record
  private
    FResponse: TIABResponseCode;
    FMessage: string;
    function GetIsSuccess: Boolean;
    function GetIsFailure: Boolean;
  public
    class function GetResponseDesc(Code: TIABResponseCode): string; static;
    class function IntToResponseCode(const Response: Integer): TIABResponseCode; static;
  public
    constructor Create(const AResponse: TIABResponseCode; const AMsg: string); overload;
    constructor Create(const ACode: Integer; const AMsg: string); overload;
    constructor Create(const ABillingResult: JBillingResult); overload;
    function ToString: string;
    property Response: TIABResponseCode read FResponse;
    property Message: string read FMessage;
    property IsSuccess: Boolean read GetIsSuccess;
    property IsFailure: Boolean read GetIsFailure;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents an in-app product listing details.
  ///	</summary>
  {$ENDREGION}
  TSkuDetails = class
  private
    FDescription: string;
    FNativeSkuDetails: JSkuDetails;
    FPrice: TSkuPrice;
    FProductKind: TProductKind;
    FSku: TSkuId;
    FType: string;
    FTitle: string;
  public
    constructor Create(const ANativeSkuDetails: JSkuDetails); overload;
    property Sku: TSkuId read FSku;
    property ProductKind: TProductKind read FProductKind;
    property Price: TSkuPrice read FPrice;
    property Title: string read FTitle;
    property Description: string read FDescription;
    property NativeSkuDetails: JSkuDetails read FNativeSkuDetails;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents an in-app billing purchase.
  ///	</summary>
  {$ENDREGION}
  TPurchase = class
  private
    FItemKind: TProductKind;
    FNativePurchase: JPurchase;
    FOrderId: TOrderId;
    FPackageName: string;
    FSku: string;
    FPurchaseTime: TDateTime;
    FPurchaseState: TPurchaseState;
    FDeveloperPayload: string;
    FToken: string;
    FSignature: string;
  public
    class function GetSku(const ANativePurchase: JPurchase): string;
    class function GetPurchaseState(const ANativePurchase: JPurchase): TPurchaseState;
  public
    constructor Create(const AProductKind: TProductKind; const ANativePurchase: JPurchase);
    property ItemKind: TProductKind read FItemKind;
    property OrderId: TOrderId read FOrderId;
    property PackageName: string read FPackageName;
    property Sku: TSkuId read FSku;
    property PurchaseTime: TDateTime read FPurchaseTime;
    property PurchaseState: TPurchaseState read FPurchaseState;
    property DeveloperPayload: string read FDeveloperPayload;
    property NativePurchase: JPurchase read FNativePurchase;
    property Token: string read FToken;
    property Signature: string read FSignature;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Represents a block of information about in-app items.
  ///	</summary>
  {$ENDREGION}
  TInventory = class
  private
    FSkuMap: TDictionary<TSkuId, TSkuDetails>;
    FPurchaseMap: TDictionary<TSkuId, TPurchase>;
    function GetSkuDetails(const ASkuId: TSkuId): TSkuDetails;
    // Returns purchase information for a given product, or nil if there is no purchase.
    function GetPurchase(const ASkuId: TSkuId): TPurchase;
  public
    constructor Create;
    function IsPurchased(const ASkuId: TSkuId): Boolean;
    function HasDetails(const ASkuId: TSkuId): Boolean;
    // Erase a purchase (locally) from the inventory, given its product ID. This just
    // modifies the Inventory object locally and has no effect on the server! This is
    // useful when you have an existing Inventory object which you know to be up to date,
    // and you have just consumed an item successfully, which means that erasing its
    // purchase data from the Inventory you already have is quicker than querying for
    // a new Inventory.
    procedure ErasePurchase(const ASkuId: TSkuId);
    procedure AddPurchase(const APurchase: TPurchase);
    procedure AddSkuDetails(const ASkuDetails: TSkuDetails);
    function GetAllSkus: TList<TSkuId>; overload;
    function GetAllOwnedSkus: TList<TSkuId>; overload;
    function GetAllOwnedSkus(const AItemKind: TProductKind): TList<TSkuId>; overload;
    function GetAllPurchases: TList<TPurchase>;
    property SkuDetails[const ASkuId: TSkuId]: TSkuDetails read GetSkuDetails;
    property Purchases[const ASkuId: TSkuId]: TPurchase read GetPurchase;
  end;

  TBillingClientFeature = (InAppItemsOnVR, PriceChangeConfirmation, Subscriptions, SubscriptionsOnVR, SubscriptionsUpdate);

  TBillingClientFeatures = set of TBillingClientFeature;

  TAndroidInAppPurchaseService = class;

  TBillingClientStateListener = class(TJavaLocal, JBillingClientStateListener)
  private
    FService: TAndroidInAppPurchaseService;
  public
    { JBillingClientStateListener }
    procedure onBillingServiceDisconnected; cdecl;
    procedure onBillingSetupFinished(billingResult: JBillingResult); cdecl;
  public
    constructor Create(const AService: TAndroidInAppPurchaseService);
  end;

  TPurchasesUpdatedListener = class(TJavaLocal, JPurchasesUpdatedListener)
  private
    FService: TAndroidInAppPurchaseService;
  public
    { JPurchasesUpdatedListener }
    procedure onPurchasesUpdated(billingResult: JBillingResult; list: JList); cdecl;
  public
    constructor Create(const AService: TAndroidInAppPurchaseService);
  end;

  TPurchasesResponseListener = class(TJavaLocal, JPurchasesResponseListener)
  private
    FService: TAndroidInAppPurchaseService;
  public
    { JPurchasesResponseListener }
    procedure onQueryPurchasesResponse(billingResult: JBillingResult; list: JList); cdecl;
  public
    constructor Create(const AService: TAndroidInAppPurchaseService);
  end;

  TSkuDetailsResponseListener = class(TJavaLocal, JSkuDetailsResponseListener)
  private
    FService: TAndroidInAppPurchaseService;
    FProductIds: TArray<string>;
    FProductKind: TProductKind;
  public
    { JSkuDetailsResponseListener }
    procedure onSkuDetailsResponse(billingResult: JBillingResult; list: JList); cdecl;
  public
    constructor Create(const AService: TAndroidInAppPurchaseService);
    procedure QueryProducts(const AProductKind: TProductKind; const AProductIds: TArray<string>);
  end;

  TConsumeProductRequest = class(TObject)
  private
    FConsumeResponseListener: JConsumeResponseListener;
    FService: TAndroidInAppPurchaseService;
    FSku: TSkuId;
  protected
    procedure ConsumeResponse(const ABillingResult: JBillingResult; const APurchaseToken: JString);
  public
    constructor Create(const AService: TAndroidInAppPurchaseService; const ASkuId: TSkuId);
    property ConsumeResponseListener: JConsumeResponseListener read FConsumeResponseListener;
    property Sku: TSkuId read FSku;
  end;

  TConsumeResponseListener = class(TJavaLocal, JConsumeResponseListener)
  private
    FRequest: TConsumeProductRequest;
  public
    { JConsumeResponseListener }
    procedure onConsumeResponse(billingResult: JBillingResult; purchaseToken: JString); cdecl;
  public
    constructor Create(const ARequest: TConsumeProductRequest);
  end;

  TAcknowledgePurchaseRequest = class(TObject)
  private
    FAcknowledgePurchaseResponseListener: JAcknowledgePurchaseResponseListener;
    FService: TAndroidInAppPurchaseService;
    FNativePurchase: JPurchase;
  protected
    procedure AcknowledgePurchaseResponse(const ABillingResult: JBillingResult);
  public
    constructor Create(const AService: TAndroidInAppPurchaseService; const ANativePurchase: JPurchase);
    property AcknowledgePurchaseResponseListener: JAcknowledgePurchaseResponseListener read FAcknowledgePurchaseResponseListener;
    property NativePurchase: JPurchase read FNativePurchase;
  end;

  TAcknowledgePurchaseResponseListener = class(TJavaLocal, JAcknowledgePurchaseResponseListener)
  private
    FRequest: TAcknowledgePurchaseRequest;
  public
    { JAcknowledgePurchaseResponseListener }
    procedure onAcknowledgePurchaseResponse(billingResult: JBillingResult); cdecl;
  public
    constructor Create(const ARequest: TAcknowledgePurchaseRequest);
  end;

  TAndroidInAppPurchaseService = class(TInterfacedObject, IFMXInAppPurchaseService)
  private
    FAcknowledgePurchaseRequests: TList<TAcknowledgePurchaseRequest>;
    FApplicationLicenseKey: string;
    FBase64PublicKey: JString;
    FBillingClient: JBillingClient;
    FBillingClientFeatures: TBillingClientFeatures;
    FBillingClientStateListener: JBillingClientStateListener;
    FConsumeProductRequests: TList<TConsumeProductRequest>;
    FPurchasesResponseListener: JPurchasesResponseListener;
    FPurchasesUpdatedListener: JPurchasesUpdatedListener;
    FSkuDetailsResponseListener: TSkuDetailsResponseListener;
    FComponents: TList<TCustomInAppPurchase>;
    FComponentsPendingSetupCompleteEvent: TList<TCustomInAppPurchase>;
    FSetupIsComplete: Boolean;
    FInventory: TInventory;
    FTransactionPayload: string;
    procedure AcknowledgePurchase(const ANativePurchase: JPurchase);
    function CanSupportSubscriptions: Boolean;
    procedure CheckApplicationLicenseKey;
    procedure ConsumeProductsAsync(const AProductIDs: TArray<string>);
    procedure DoError(const AFailureKind: TFailureKind; const AErrorMessage: string);
    procedure DoConsumeFinished(const AIabResult: TIabResult; const ASkuId: TSkuId);
    procedure DoProductsRequestResponse(const AProducts: TIAPProductList; const AInvalidProductIDs: TStrings);
    procedure DoPurchaseCompleted(const AProductID: string; const ANewTransaction: Boolean);
    procedure DoPurchaseFinished(const AIabResult: TIabResult; const ANativePurchase: JPurchase);
    procedure DoQueryInventoryFinished(const IabResult: TIabResult);
    procedure DoRecordTransaction(const AProductID, ATransactionID: string; const ATransactionDate: TDateTime);
    procedure DoSetupComplete(const AIabResult: TIabResult);
    function DoVerifyPayload(const APayload: string): Boolean;
    procedure HandlePurchases(const APurchasesList: JList);
    procedure InternalDoError(const AFailureKind: TFailureKind; const AErrorMessage: string);
    function IsFeatureSupported(const AFeatureType: JString): Boolean;
    function IsMainThread: Boolean;
    procedure PerformPostSetup(const AIabResult: TIabResult);
    procedure ProcessPurchases(const AProductKind: TProductKind; const APurchasesResult: JPurchase_PurchasesResult);
    procedure QueryFeatures;
    procedure QueryInventory;
    procedure QueryPurchases(const AProductKind: TProductKind);
  protected
    procedure AcknowledgePurchaseResponse(const ARequest: TAcknowledgePurchaseRequest; const ABillingResult: JBillingResult);
    procedure BillingServiceDisconnected;
    procedure BillingSetupFinished(const ABillingResult: JBillingResult);
    procedure ConsumeResponse(const ARequest: TConsumeProductRequest; const ABillingResult: JBillingResult; const APurchaseToken: JString);
    procedure InternalQueryProducts(const AProductKind: TProductKind; const AProductIDs: TArray<string>);
    procedure PurchasesUpdated(const ABillingResult: JBillingResult; const APurchasesList: JList);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure QueryPurchasesResponse(const ABillingResult: JBillingResult; const APurchasesList: JList);
    procedure SkuDetailsResponse(const AProductKind: TProductKind; const AProductIds: TArray<string>;
      const ABillingResult: JBillingResult; const ASkuDetailsList: JList);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXInAppPurchaseService }
    procedure AddComponent(const Component: TCustomInAppPurchase);
    function CanMakeInAppPurchases: Boolean;
    procedure ConsumeProduct(const AProductID: string);
    procedure ConsumeProducts(const AProductIDs: TStrings);
    function GetApplicationLicenseKey: string;
    function GetTransactionPayload: string;
    function IsProductPurchased(const AProductID: string): Boolean;
    procedure PurchaseProduct(const AProductID: string);
    procedure QueryProducts(const ProductIDs: TStrings);
    procedure RemoveComponent(const Component: TCustomInAppPurchase);
    procedure RestorePurchasedProducts;
    procedure SetApplicationLicenseKey(const Value: string);
    procedure SetTransactionPayload(const Value: string);
    procedure SetupInAppPurchase(Component: TCustomInAppPurchase);
    property ApplicationLicenseKey: string read GetApplicationLicenseKey write SetApplicationLicenseKey;
    property TransactionPayload: string read GetTransactionPayload write SetTransactionPayload;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Exception thrown when something went wrong with in-app billing. An
  ///	  EIabException has an associated IabResult (an error). To get the IAB
  ///	  result that caused this exception to be thrown, use <see cref="Result" />
  ///	</summary>
  {$ENDREGION}
  EIabException = class(Exception)
  private
    FResult: TIabResult;
  public
    constructor Create(const R: TIabResult); overload;
    constructor Create(const Response: TIABResponseCode; const AMessage: String); overload;
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  The result that caused this exception to be thrown
    ///	</summary>
    {$ENDREGION}
    property Result: TIabResult read FResult;
  end;
  EIllegalStateException = class(EIabException);
  ERuntimeException = class(EIabException);

const
  // Exception class names as picked up by EJNIException
  // The JNI exception class name extraction uses the exception class's
  // toString() method, which inserts the class prefix in
  SRemoteException = 'class android.os.RemoteException';  // do not localize
  SNoSuchAlgorithmException = 'class java.security.NoSuchAlgorithmException'; // do not localize
  SInvalidKeySpecException = 'class java.security.spec.InvalidKeySpecException'; // do not localize
  SInvalidKeyException = 'class java.security.InvalidKeyException'; // do not localize
  SSignatureException = 'class java.security.SignatureException'; // do not localize

var
  InAppPurchaseService: TAndroidInAppPurchaseService;

function ProductKindToStr(Kind: TProductKind): string;
const
  // Do not localize
  Strs: array[TProductKind] of string = ('unknown', 'inapp', 'subs'); // do not localize
begin
  Result := Strs[Kind];
end;

function SkuDetailsToProduct(const SkuDetails: TSkuDetails): TProduct;
begin
  Result := TProduct.Create(SkuDetails.Sku, TProduct.PriceNotAvailable,
    SkuDetails.Price, SkuDetails.Title, SkuDetails.Description, False, nil, '');
end;

{ TSkuDetails }

constructor TSkuDetails.Create(const ANativeSkuDetails: JSkuDetails);
begin
  FNativeSkuDetails := ANativeSkuDetails;
  FSku := JStringToString(FNativeSkuDetails.getSku);
  FType := JStringToString(FNativeSkuDetails.getType);
  FPrice := JStringToString(FNativeSkuDetails.getPrice);
  FTitle := JStringToString(FNativeSkuDetails.getTitle);
  FDescription := JStringToString(FNativeSkuDetails.getDescription);
  if FNativeSkuDetails.getType.equals(TJBillingClient_SkuType.JavaClass.INAPP) then
    FProductKind := TProductKind.InApp
  else if FNativeSkuDetails.getType.equals(TJBillingClient_SkuType.JavaClass.SUBS) then
    FProductKind := TProductKind.Subscription
  else
    FProductKind := TProductKind.Unknown;
end;

{ TPurchase }

constructor TPurchase.Create(const AProductKind: TProductKind; const ANativePurchase: JPurchase);
begin
  FItemKind := ItemKind;
  FNativePurchase := ANativePurchase;
  FOrderId := JStringToString(FNativePurchase.getOrderId);
  FPackageName := JStringToString(FNativePurchase.getPackageName);
  FSku := GetSku(ANativePurchase);
  FPurchaseTime := IncMilliSecond(UnixDateDelta, FNativePurchase.getPurchaseTime);
  FPurchaseState := GetPurchaseState(FNativePurchase);
  FDeveloperPayload := JStringToString(FNativePurchase.getDeveloperPayload);
  FToken := JStringToString(FNativePurchase.getPurchaseToken);
  FSignature := JStringToString(FNativePurchase.getSignature);
end;

class function TPurchase.GetPurchaseState(const ANativePurchase: JPurchase): TPurchaseState;
begin
  Result := TPurchaseState(ANativePurchase.getPurchaseState);
end;

class function TPurchase.GetSku(const ANativePurchase: JPurchase): string;
begin
  if ANativePurchase.getSkus.size > 0 then
    Result := JStringToString(TJString.Wrap(TAndroidHelper.JObjectToID(ANativePurchase.getSkus.get(0))))
  else
    Result := '';
end;

{ TInventory }

constructor TInventory.Create;
begin
  inherited;
  FSkuMap := TDictionary<TSkuId, TSkuDetails>.Create;
  FPurchaseMap := TDictionary<TSkuId, TPurchase>.Create;
end;

procedure TInventory.AddPurchase(const APurchase: TPurchase);
begin
  if not IsPurchased(APurchase.Sku) then
    FPurchaseMap.Add(APurchase.Sku, APurchase);
end;

procedure TInventory.AddSkuDetails(const ASkuDetails: TSkuDetails);
begin
  if not HasDetails(ASkuDetails.Sku) then
    FSkuMap.Add(ASkuDetails.Sku, ASkuDetails);
end;

procedure TInventory.ErasePurchase(const ASkuId: TSkuId);
begin
  if FPurchaseMap.ContainsKey(ASkuId) then
    FPurchaseMap.Remove(ASkuId);
end;

function TInventory.GetAllOwnedSkus(const AItemKind: TProductKind): TList<TSkuId>;
var
  LPurchase: TPurchase;
begin
  Result := TList<TSkuId>.Create;
  for LPurchase in FPurchaseMap.values do
  begin
    if LPurchase.ItemKind = AItemKind then
      Result.Add(LPurchase.Sku);
  end;
end;

function TInventory.GetAllSkus: TList<TSkuId>;
var
  LSkuID: TSkuID;
begin
  Result := TList<TSkuId>.Create;
  for LSkuID in FSkuMap.Keys do
    Result.Add(LSkuID);
end;

function TInventory.GetAllOwnedSkus: TList<TSkuId>;
var
  LKey: TSkuId;
begin
  Result := TList<TSkuId>.Create;
  for LKey in FPurchaseMap.Keys do
    Result.Add(LKey);
end;

function TInventory.GetAllPurchases: TList<TPurchase>;
var
  LPurchase: TPurchase;
begin
  Result := TList<TPurchase>.Create;
  for LPurchase in FPurchaseMap.Values do
    Result.Add(LPurchase);
end;

function TInventory.GetPurchase(const ASkuId: TSkuId): TPurchase;
begin
  FPurchaseMap.TryGetValue(ASkuId, Result);
end;

function TInventory.GetSkuDetails(const ASkuId: TSkuId): TSkuDetails;
begin
  FSkuMap.TryGetValue(ASkuId, Result);
end;

function TInventory.HasDetails(const ASkuId: TSkuId): Boolean;
begin
  Result := FSkuMap.ContainsKey(ASkuId);
end;

function TInventory.IsPurchased(const ASkuId: TSkuId): Boolean;
begin
  Result := FPurchaseMap.ContainsKey(ASkuId);
end;

{ TIabResult }

constructor TIabResult.Create(const AResponse: TIABResponseCode; const AMsg: string);
begin
  FResponse := AResponse;
  if AMsg.Trim.IsEmpty then
    FMessage := GetResponseDesc(Response)
  else
    FMessage := AMsg + ' (response: ' + GetResponseDesc(AResponse) + ')';
end;

constructor TIabResult.Create(const ACode: Integer; const AMsg: string);
begin
  Create(IntToResponseCode(ACode), AMsg);
end;

constructor TIabResult.Create(const ABillingResult: JBillingResult);
begin
  Create(ABillingResult.getResponseCode, JStringToString(ABillingResult.getDebugMessage));
end;

class function TIabResult.GetResponseDesc(Code: TIABResponseCode): string;
const
  SIABMessages: array[TIabResponseCode] of String = (
    SOk,
    SUserCanceled,
    SBillingUnavailable,
    SItemUnavailable,
    SDeveloperError,
    SError,
    SItemAlreadyOwned,
    SItemNotOwned,
    SRemoteExceptionOccurred,
    SBadResponse,
    SVerificationFailed,
    SSendIntentFailed,
    SUnknownPurchaseResponse,
    SMissingToken,
    SUnknownError,
    SSubscriptionsNotAvailable,
    SInvalidConsumption,
    SIABFeatureNotSupported,
    SIABServiceDisconnected,
    SIABServiceTimeout,
    SIABServiceUnavailable
  );
begin
  Result := SIABMessages[Code];
end;

function TIabResult.GetIsFailure: Boolean;
begin
  Result := not IsSuccess;
end;

function TIabResult.GetIsSuccess: Boolean;
begin
  Result := FResponse = TIABResponseCode.Ok;
end;

class function TIabResult.IntToResponseCode(const Response: Integer): TIABResponseCode;
begin
  Result := TIABResponseCode.UnknownError;
  if Response = TJBillingClient_BillingResponseCode.JavaClass.BILLING_UNAVAILABLE then
    Result := TIABResponseCode.BillingUnavailable
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.DEVELOPER_ERROR then
    Result := TIABResponseCode.DeveloperError
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.ERROR then
    Result := TIABResponseCode.Error
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.FEATURE_NOT_SUPPORTED then
    Result := TIABResponseCode.FeatureNotSupported
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.ITEM_ALREADY_OWNED then
    Result := TIABResponseCode.ItemAlreadyOwned
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.ITEM_NOT_OWNED then
    Result := TIABResponseCode.ItemNotOwned
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.ITEM_UNAVAILABLE then
    Result := TIABResponseCode.ItemUnavailable
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.OK then
    Result := TIABResponseCode.OK
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.SERVICE_DISCONNECTED then
    Result := TIABResponseCode.ServiceDisconnected
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.SERVICE_TIMEOUT then
    Result := TIABResponseCode.ServiceTimeout
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.SERVICE_UNAVAILABLE then
    Result := TIABResponseCode.ServiceUnavailable
  else if Response = TJBillingClient_BillingResponseCode.JavaClass.USER_CANCELED then
    Result := TIABResponseCode.UserCancelled;
end;

function TIabResult.ToString: String;
begin
  Result := 'IabResult: ' + Message;
end;

{ EIabException }

constructor EIabException.Create(const R: TIabResult);
begin
  inherited Create(R.Message);
end;

constructor EIabException.Create(const Response: TIABResponseCode; const AMessage: String);
begin
  Create(TIabResult.Create(Response, AMessage));
end;

{ TBillingClientStateListener }

constructor TBillingClientStateListener.Create(const AService: TAndroidInAppPurchaseService);
begin
  inherited Create;
  FService := AService;
end;

procedure TBillingClientStateListener.onBillingServiceDisconnected;
begin
  FService.BillingServiceDisconnected;
end;

procedure TBillingClientStateListener.onBillingSetupFinished(billingResult: JBillingResult);
begin
  FService.BillingSetupFinished(billingResult);
end;

{ TPurchasesResponseListener }

constructor TPurchasesResponseListener.Create(const AService: TAndroidInAppPurchaseService);
begin
  inherited Create;
  FService := AService;
end;

procedure TPurchasesResponseListener.onQueryPurchasesResponse(billingResult: JBillingResult; list: JList);
begin
  FService.QueryPurchasesResponse(billingResult, list);
end;

{ TPurchasesUpdatedListener }

constructor TPurchasesUpdatedListener.Create(const AService: TAndroidInAppPurchaseService);
begin
  inherited Create;
  FService := AService;
end;

procedure TPurchasesUpdatedListener.onPurchasesUpdated(billingResult: JBillingResult; list: JList);
begin
  FService.PurchasesUpdated(billingResult, list);
end;

{ TSkuDetailsResponseListener }

constructor TSkuDetailsResponseListener.Create(const AService: TAndroidInAppPurchaseService);
begin
  inherited Create;
  FService := AService;
end;

procedure TSkuDetailsResponseListener.onSkuDetailsResponse(billingResult: JBillingResult; list: JList);
begin
  FService.SkuDetailsResponse(FProductKind, FProductIds, billingResult, list);
end;

procedure TSkuDetailsResponseListener.QueryProducts(const AProductKind: TProductKind; const AProductIds: TArray<string>);
begin
  FProductKind := AProductKind;
  FProductIds := AProductIds;
  FService.InternalQueryProducts(FProductKind, FProductIds);
end;

{ TConsumeProductRequest }

constructor TConsumeProductRequest.Create(const AService: TAndroidInAppPurchaseService; const ASkuId: TSkuId);
begin
  inherited Create;
  FService := AService;
  FSku := ASkuId;
  FConsumeResponseListener := TConsumeResponseListener.Create(Self);
end;

procedure TConsumeProductRequest.ConsumeResponse(const ABillingResult: JBillingResult; const APurchaseToken: JString);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FService.ConsumeResponse(Self, ABillingResult, APurchaseToken);
    end
  );
end;

{ TConsumeResponseListener }

constructor TConsumeResponseListener.Create(const ARequest: TConsumeProductRequest);
begin
  inherited Create;
  FRequest := ARequest;
end;

procedure TConsumeResponseListener.onConsumeResponse(billingResult: JBillingResult; purchaseToken: JString);
begin
  FRequest.ConsumeResponse(billingResult, purchaseToken);
end;

{ TAcknowledgePurchaseRequest }

constructor TAcknowledgePurchaseRequest.Create(const AService: TAndroidInAppPurchaseService; const ANativePurchase: JPurchase);
begin
  inherited Create;
  FService := AService;
  FNativePurchase := ANativePurchase;
  FAcknowledgePurchaseResponseListener := TAcknowledgePurchaseResponseListener.Create(Self);
end;

procedure TAcknowledgePurchaseRequest.AcknowledgePurchaseResponse(const ABillingResult: JBillingResult);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FService.AcknowledgePurchaseResponse(Self, ABillingResult);
    end
  );
end;

{ TAcknowledgePurchaseResponseListener }

constructor TAcknowledgePurchaseResponseListener.Create(const ARequest: TAcknowledgePurchaseRequest);
begin
  inherited Create;
  FRequest := ARequest;
end;

procedure TAcknowledgePurchaseResponseListener.onAcknowledgePurchaseResponse(billingResult: JBillingResult);
begin
  FRequest.AcknowledgePurchaseResponse(billingResult);
end;

{ TAndroidInAppPurchaseService }

constructor TAndroidInAppPurchaseService.Create;
begin
  inherited;
  FInventory := TInventory.Create;
  FComponents := TList<TCustomInAppPurchase>.Create;
  FAcknowledgePurchaseRequests := TList<TAcknowledgePurchaseRequest>.Create;
  FConsumeProductRequests := TList<TConsumeProductRequest>.Create;
  FBillingClientStateListener := TBillingClientStateListener.Create(Self);
  FPurchasesResponseListener := TPurchasesResponseListener.Create(Self);
  FPurchasesUpdatedListener := TPurchasesUpdatedListener.Create(Self);
  FSkuDetailsResponseListener := TSkuDetailsResponseListener.Create(Self);
  FBillingClient := TJBillingClient.JavaClass.newBuilder(TAndroidHelper.Context)
    .setListener(FPurchasesUpdatedListener)
    .enablePendingPurchases
    .build;
  FBillingClient.startConnection(FBillingClientStateListener);
end;

destructor TAndroidInAppPurchaseService.Destroy;
begin
  if FBillingClient.isReady then
    FBillingClient.endConnection;
  FInventory.Free;
  FAcknowledgePurchaseRequests.Free;
  FConsumeProductRequests.Free;
  while FComponents.Count > 0 do
    FComponents.Delete(0);
  FComponents.Free;
  if FComponentsPendingSetupCompleteEvent <> nil then
  begin
    while FComponentsPendingSetupCompleteEvent.Count > 0 do
      FComponentsPendingSetupCompleteEvent.Delete(0);
  end;
  FComponentsPendingSetupCompleteEvent.Free;
  inherited;
end;

function TAndroidInAppPurchaseService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := FBillingClient.QueryInterface(IID, Obj);
end;

procedure TAndroidInAppPurchaseService.AddComponent(const Component: TCustomInAppPurchase);
begin
  if not FComponents.Contains(Component) then
    FComponents.Add(Component);
end;

procedure TAndroidInAppPurchaseService.RemoveComponent(const Component: TCustomInAppPurchase);
begin
  if Component <> nil then
    FComponents.Remove(Component);
end;

procedure TAndroidInAppPurchaseService.SetupInAppPurchase(Component: TCustomInAppPurchase);
begin
  // If setup has already been done just trigger the event, otherwise add
  // component into a pending list to notify later on setup completion
  if Supports(Component, IInAppPurchaseCallbacks) then
  begin
    if FSetupIsComplete then
      IInAppPurchaseCallbacks(Component).DoSetupComplete
    else
    begin
      if FComponentsPendingSetupCompleteEvent = nil then
        FComponentsPendingSetupCompleteEvent := TList<TCustomInAppPurchase>.Create;
      FComponentsPendingSetupCompleteEvent.Add(Component);
    end
  end;
end;

function TAndroidInAppPurchaseService.IsFeatureSupported(const AFeatureType: JString): Boolean;
begin
  Result := FBillingClient.isFeatureSupported(AFeatureType).getResponseCode = TJBillingClient_BillingResponseCode.JavaClass.OK;
end;

function TAndroidInAppPurchaseService.IsMainThread: Boolean;
begin
  Result := TThread.CurrentThread.ThreadID = MainThreadID;
end;

procedure TAndroidInAppPurchaseService.QueryFeatures;
begin
  FBillingClientFeatures := [];
  if IsFeatureSupported(TJBillingClient_FeatureType.JavaClass.SUBSCRIPTIONS) then
    Include(FBillingClientFeatures, TBillingClientFeature.Subscriptions);
end;

procedure TAndroidInAppPurchaseService.QueryInventory;
begin
  QueryPurchases(TProductKind.InApp);
  if CanSupportSubscriptions then
    QueryPurchases(TProductKind.Subscription);
end;

procedure TAndroidInAppPurchaseService.DoSetupComplete(const AIabResult: TIabResult);
var
  Component: TCustomInAppPurchase;
begin
  if AIabResult.IsFailure then
    InternalDoError(TFailureKind.Purchase, Format('%s: %s', [SIAPSetupProblem, AIabResult.ToString]))
  else
  begin
    FSetupIsComplete := True;
    if Assigned(FComponentsPendingSetupCompleteEvent) then
    begin
      for Component in FComponentsPendingSetupCompleteEvent do
        if Supports(Component, IInAppPurchaseCallbacks) then
          IInAppPurchaseCallbacks(Component).DoSetupComplete;
      while FComponentsPendingSetupCompleteEvent.Count > 0 do
        FComponentsPendingSetupCompleteEvent.Delete(0);
      FComponentsPendingSetupCompleteEvent := nil;
    end;
  end;
end;

procedure TAndroidInAppPurchaseService.PerformPostSetup(const AIabResult: TIabResult);
begin
  QueryFeatures;
  QueryInventory;
  TThread.Synchronize(nil,
    procedure
    begin
      DoSetupComplete(AIabResult);
    end
  );
end;

procedure TAndroidInAppPurchaseService.BillingServiceDisconnected;
begin
  // New - may need to add "retry" handling
end;

procedure TAndroidInAppPurchaseService.BillingSetupFinished(const ABillingResult: JBillingResult);
var
  LIabResult: TIabResult;
begin
  LIabResult := TIabResult.Create(ABillingResult);
  if LIabResult.IsSuccess then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        PerformPostSetup(LIabResult)
      end
    ).Start;
  end
  else
    DoSetupComplete(LIabResult);
end;

procedure TAndroidInAppPurchaseService.AcknowledgePurchaseResponse(const ARequest: TAcknowledgePurchaseRequest; const ABillingResult: JBillingResult);
begin
  try
    FAcknowledgePurchaseRequests.Remove(ARequest);
    DoPurchaseFinished(TIabResult.Create(ABillingResult), ARequest.NativePurchase);
  finally
    ARequest.Free;
  end;
end;

procedure TAndroidInAppPurchaseService.AcknowledgePurchase(const ANativePurchase: JPurchase);
var
  LRequest: TAcknowledgePurchaseRequest;
  LParams: JAcknowledgePurchaseParams;
begin
  LRequest := TAcknowledgePurchaseRequest.Create(Self, ANativePurchase);
  FAcknowledgePurchaseRequests.Add(LRequest);
  LParams := TJAcknowledgePurchaseParams.JavaClass.newBuilder
    .setPurchaseToken(ANativePurchase.getPurchaseToken)
    .build;
  FBillingClient.acknowledgePurchase(LParams, LRequest.AcknowledgePurchaseResponseListener);
end;

procedure TAndroidInAppPurchaseService.HandlePurchases(const APurchasesList: JList);
var
  I: Integer;
  LNativePurchase: JPurchase;
  LBase64PublicKey: JString;
begin
  LBase64PublicKey := StringToJString(FApplicationLicenseKey);
  for I := 0 to APurchasesList.size - 1 do
  begin
    LNativePurchase := TJPurchase.Wrap(TAndroidHelper.JObjectToID(APurchasesList.get(I)));
    case TPurchase.GetPurchaseState(LNativePurchase) of
      TPurchaseState.psPurchased:
      begin
        if TJIAPSecurity.JavaClass.verifyPurchase(LBase64PublicKey, LNativePurchase.getOriginalJson, LNativePurchase.getSignature) then
        begin
          if LNativePurchase.isAcknowledged then
            DoPurchaseFinished(TIabResult.Create(TIABResponseCode.Ok, SSuccess), LNativePurchase)
          else
            AcknowledgePurchase(LNativePurchase);
        end
        else
          DoPurchaseFinished(TIabResult.Create(TIABResponseCode.VerificationFailed, Format(SSignatureFailureFmt, [TPurchase.GetSku(LNativePurchase)])), nil);
      end;
    end;
  end;
end;

procedure TAndroidInAppPurchaseService.DoPurchaseFinished(const AIabResult: TIabResult; const ANativePurchase: JPurchase);
var
  LSkuDetails: TSkuDetails;
  LPurchase: TPurchase;
  LPayload: string;
begin
  if AIabResult.IsSuccess then
  begin
    // If we've a payload on the purchase, ensure it checks out before completing
    LPayload := JStringToString(ANativePurchase.getDeveloperPayload);
    if not LPayload.IsEmpty then
    begin
      if not DoVerifyPayload(LPayload) then
      begin
        InternalDoError(TFailureKind.Purchase, SIAPPayloadVerificationFailed);
        Exit;
      end;
    end;
    LSkuDetails := FInventory.GetSkuDetails(TPurchase.GetSku(ANativePurchase));
    if LSkuDetails <> nil then
      LPurchase := TPurchase.Create(LSkuDetails.ProductKind, ANativePurchase)
    else
      LPurchase := TPurchase.Create(TProductKind.Unknown, ANativePurchase);
    FInventory.AddPurchase(LPurchase);
    DoPurchaseCompleted(LPurchase.Sku, True);
    DoRecordTransaction(LPurchase.Sku, LPurchase.Token, LPurchase.PurchaseTime);
  end
  else
    InternalDoError(TFailureKind.Purchase, AIabResult.ToString);
end;

procedure TAndroidInAppPurchaseService.DoPurchaseCompleted(const AProductID: string; const ANewTransaction: Boolean);
var
  LComponent: TCustomInAppPurchase;
begin
  for LComponent in FComponents do
    if Supports(LComponent, IInAppPurchaseCallbacks) then
      IInAppPurchaseCallbacks(LComponent).DoPurchaseCompleted(AProductID, ANewTransaction);
end;

procedure TAndroidInAppPurchaseService.DoRecordTransaction(const AProductID, ATransactionID: string; const ATransactionDate: TDateTime);
var
  LComponent: TCustomInAppPurchase;
begin
  for LComponent in FComponents do
  begin
    if Supports(LComponent, IInAppPurchaseCallbacks) then
      IInAppPurchaseCallbacks(LComponent).DoRecordTransaction(AProductID, ATransactionID, ATransactionDate);
  end;
end;

function TAndroidInAppPurchaseService.DoVerifyPayload(const APayload: string): Boolean;
var
  LComponent: TCustomInAppPurchase;
begin
  Result := True;
  for LComponent in FComponents do
  begin
    if Supports(LComponent, IInAppPurchaseCallbacks) then
    begin
      IInAppPurchaseCallbacks(LComponent).DoVerifyPayload(APayload, Result);
      if not Result then
        Break;
    end;
  end;
end;

procedure TAndroidInAppPurchaseService.PurchasesUpdated(const ABillingResult: JBillingResult; const APurchasesList: JList);
var
  LIabResult: TIabResult;
begin
  LIabResult := TIabResult.Create(ABillingResult);
  if LIabResult.IsSuccess then
  begin
    if APurchasesList <> nil then
      HandlePurchases(APurchasesList);
  end
  else if LIabResult.Response = TIABResponseCode.ItemAlreadyOwned then
    raise EIAPException.Create(SIAPAlreadyPurchased)
  else
    InternalDoError(TFailureKind.Purchase, LIabResult.Message);
end;

procedure TAndroidInAppPurchaseService.QueryPurchasesResponse(const ABillingResult: JBillingResult; const APurchasesList: JList);
var
  LIabResult: TIabResult;
begin
  LIabResult := TIabResult.Create(ABillingResult);
  if LIabResult.IsSuccess then
  begin
    // Process products previously purchased
    if APurchasesList <> nil then
      HandlePurchases(APurchasesList);
  end
  else
    InternalDoError(TFailureKind.Purchase, LIabResult.Message);
end;

procedure TAndroidInAppPurchaseService.DoProductsRequestResponse(const AProducts: TIAPProductList; const AInvalidProductIDs: TStrings);
begin
  TThread.Synchronize(nil,
    procedure
    var
      LComponent: TCustomInAppPurchase;
    begin
      for LComponent in FComponents do
        if Supports(LComponent, IInAppPurchaseCallbacks) then
          IInAppPurchaseCallbacks(LComponent).DoProductsRequestResponse(AProducts, AInvalidProductIDs);
    end
  );
end;

procedure TAndroidInAppPurchaseService.InternalDoError(const AFailureKind: TFailureKind; const AErrorMessage: string);
begin
  if not IsMainThread then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        DoError(AFailureKind, AErrorMessage);
      end
    );
  end
  else
    DoError(AFailureKind, AErrorMessage);
end;

procedure TAndroidInAppPurchaseService.DoError(const AFailureKind: TFailureKind; const AErrorMessage: string);
var
  LComponent: TCustomInAppPurchase;
begin
  for LComponent in FComponents do
  begin
    if Supports(LComponent, IInAppPurchaseCallbacks) then
      IInAppPurchaseCallbacks(LComponent).DoError(AFailureKind, AErrorMessage);
  end;
end;

procedure TAndroidInAppPurchaseService.DoQueryInventoryFinished(const IabResult: TIabResult);
var
  LSkuId: TSkuId;
  LSkuIds: TList<TSkuId>;
  LProducts: TIAPProductList;
begin
  if IabResult.IsSuccess then
  begin
    LSkuIds := FInventory.GetAllSkus;
    LProducts := TIAPProductList.Create;
    try
      for LSkuId in LSkuIds do
        LProducts.Add(SkuDetailsToProduct(FInventory.SkuDetails[LSkuId]));
      DoProductsRequestResponse(LProducts, nil);
    finally
      LProducts.Free;
    end;
  end
  else
    InternalDoError(TFailureKind.ProductsRequest, IabResult.ToString);
end;

procedure TAndroidInAppPurchaseService.SkuDetailsResponse(const AProductKind: TProductKind; const AProductIds: TArray<string>;
  const ABillingResult: JBillingResult; const ASkuDetailsList: JList);
var
  LIabResult: TIabResult;
  I: Integer;
  LNativeSkuDetails: JSkuDetails;
begin
  LIabResult := TIabResult.Create(ABillingResult);
  if LIabResult.IsSuccess then
  begin
    if ASkuDetailsList <> nil then
    begin
      for I := 0 to ASkuDetailsList.size - 1 do
      begin
        LNativeSkuDetails := TJSkuDetails.Wrap(TAndroidHelper.JObjectToID(ASkuDetailsList.get(I)));
        FInventory.AddSkuDetails(TSkuDetails.Create(LNativeSkuDetails));
      end;
    end;
  end;
  if (AProductKind = TProductKind.InApp) and CanSupportSubscriptions then
    FSkuDetailsResponseListener.QueryProducts(TProductKind.Subscription, AProductIds)
  else
    DoQueryInventoryFinished(LIabResult);
end;

function TAndroidInAppPurchaseService.CanMakeInAppPurchases: Boolean;
begin
  Result := FSetupIsComplete;
end;

function TAndroidInAppPurchaseService.CanSupportSubscriptions: Boolean;
begin
  Result := TBillingClientFeature.Subscriptions in FBillingClientFeatures;
end;

procedure TAndroidInAppPurchaseService.CheckApplicationLicenseKey;
begin
  if ApplicationLicenseKey.IsEmpty then
    raise EIAPNoLicenseKey.Create(SIAPNoLicenseKey);
end;

function TAndroidInAppPurchaseService.GetApplicationLicenseKey: string;
begin
  Result := FApplicationLicenseKey;
end;

function TAndroidInAppPurchaseService.GetTransactionPayload: string;
begin
  Result := FTransactionPayload;
end;

function TAndroidInAppPurchaseService.IsProductPurchased(const AProductID: string): Boolean;
begin
  Result := False;
  if FInventory <> nil then
    Result := FInventory.IsPurchased(AProductID);
end;

procedure TAndroidInAppPurchaseService.PurchaseProduct(const AProductID: string);
var
  LParams: JBillingFlowParams;
  LIabResult: TIabResult;
  LSkuDetails:  TSkuDetails;
begin
  CheckApplicationLicenseKey;
  LSkuDetails := FInventory.GetSkuDetails(AProductID);
  if LSkuDetails <> nil then
  begin
    LParams := TJBillingFlowParams.JavaClass.newBuilder
      .setSkuDetails(LSkuDetails.NativeSkuDetails)
      .build;
    // launchBillingFlow if successful will trigger onPurchasesUpdated in FPurchasesUpdatedListener
    LIabResult := TIabResult.Create(FBillingClient.launchBillingFlow(TAndroidHelper.Activity, LParams));
    if not LIabResult.IsSuccess then
      InternalDoError(TFailureKind.Purchase, LIabResult.Message);
  end
  else
    raise EIAPException.CreateFmt(SIAPProductNotInInventory, []);
end;

procedure TAndroidInAppPurchaseService.ConsumeProduct(const AProductID: string);
begin
  CheckApplicationLicenseKey;
  if IsProductPurchased(AProductID) then
    ConsumeProductsAsync([AProductID]);
end;

procedure TAndroidInAppPurchaseService.ConsumeProducts(const AProductIDs: TStrings);
var
  LProductIDs: TArray<string>;
  I: Integer;
begin
  CheckApplicationLicenseKey;
  LProductIDs := AProductIDs.ToStringArray;
  for I := High(LProductIDs) downto Low(LProductIDs) do
  begin
    if not IsProductPurchased(LProductIDs[I]) then
      Delete(LProductIDs, I, 1);
  end;
  if Length(LProductIDs) > 0 then
    ConsumeProductsAsync(LProductIDs);
end;

procedure TAndroidInAppPurchaseService.ConsumeProductsAsync(const AProductIDs: TArray<string>);
var
  LProductID: string;
  LRequest: TConsumeProductRequest;
  LParams: JConsumeParams;
  LPurchase: TPurchase;
begin
  for LProductID in AProductIDs do
  begin
    LPurchase := FInventory.GetPurchase(LProductID);
    if LPurchase <> nil then
    begin
      LRequest := TConsumeProductRequest.Create(Self, LPurchase.Sku);
      FConsumeProductRequests.Add(LRequest);
      LParams := TJConsumeParams.JavaClass.newBuilder
        .setPurchaseToken(LPurchase.NativePurchase.getPurchaseToken)
        .build;
      FBillingClient.consumeAsync(LParams, LRequest.ConsumeResponseListener);
    end;
  end;
end;

procedure TAndroidInAppPurchaseService.ConsumeResponse(const ARequest: TConsumeProductRequest; const ABillingResult: JBillingResult;
  const APurchaseToken: JString);
var
  LSku: string;
begin
  try
    LSku := ARequest.Sku;
    FConsumeProductRequests.Remove(ARequest);
  finally
    ARequest.Free;
  end;
  DoConsumeFinished(TIabResult.Create(ABillingResult), LSku);
end;

procedure TAndroidInAppPurchaseService.DoConsumeFinished(const AIabResult: TIabResult; const ASkuId: TSkuId);
var
  LComponent: TCustomInAppPurchase;
begin
  // Once consumed, the product is no longer considered owned,
  // and so can be purchased again
  if FInventory <> nil then
    FInventory.ErasePurchase(ASkuId);
  //Now tell all the components it has been consumed
  for LComponent in FComponents do
  begin
    if Supports(LComponent, IInAppPurchaseCallbacks) then
    begin
      if AIabResult.IsSuccess then
        IInAppPurchaseCallbacks(LComponent).DoConsumeCompleted(ASkuId)
      else
        IInAppPurchaseCallbacks(LComponent).DoConsumeFailed(ASkuId, AIabResult.ToString);
    end;
  end;
end;

procedure TAndroidInAppPurchaseService.InternalQueryProducts(const AProductKind: TProductKind; const AProductIDs: TArray<string>);
var
  LProductID: string;
  LParams: JSkuDetailsParams;
  LSkuList: JArrayList;
  LType: JString;
begin
  if AProductKind = TProductKind.InApp then
    LType := TJBillingClient_SkuType.JavaClass.INAPP
  else
    LType := TJBillingClient_SkuType.JavaClass.SUBS;
  LSkuList := TJArrayList.Create;
  for LProductID in AProductIDs do
    LSkuList.add(StringToJString(LProductID));
  LParams := TJSkuDetailsParams.JavaClass.newBuilder
    .setSkusList(TJList.Wrap(LSkuList))
    .setType(LType)
    .build;
  FBillingClient.querySkuDetailsAsync(LParams, FSkuDetailsResponseListener);
end;

procedure TAndroidInAppPurchaseService.QueryProducts(const ProductIDs: TStrings);
begin
  FSkuDetailsResponseListener.QueryProducts(TProductKind.InApp, ProductIds.ToStringArray);
end;

procedure TAndroidInAppPurchaseService.ProcessPurchases(const AProductKind: TProductKind; const APurchasesResult: JPurchase_PurchasesResult);
var
  LIabResult: TIabResult;
  I: Integer;
  LPurchase: JPurchase;
begin
  LIabResult := TIabResult.Create(APurchasesResult.getBillingResult);
  if LIabResult.IsSuccess then
  begin
    I := 0;
    while (APurchasesResult.getPurchasesList <> nil) and (I < APurchasesResult.getPurchasesList.size) do
    begin
      LPurchase := TJPurchase.Wrap(TAndroidHelper.JObjectToID(APurchasesResult.getPurchasesList.get(I)));
      FInventory.AddPurchase(TPurchase.Create(AProductKind, LPurchase));
      Inc(I);
    end;
  end;
end;

procedure TAndroidInAppPurchaseService.QueryPurchases(const AProductKind: TProductKind);
var
  LType: JString;
begin
  if AProductKind = TProductKind.InApp then
    LType := TJBillingClient_SkuType.JavaClass.INAPP
  else
    LType := TJBillingClient_SkuType.JavaClass.SUBS;
  ProcessPurchases(AProductKind, FBillingClient.queryPurchases(LType));
end;

procedure TAndroidInAppPurchaseService.RestorePurchasedProducts;
begin
  CheckApplicationLicenseKey;
  // On Android the restore functionality is irrelevant thanks to the inventory
  // system automatically identifying which products have been purchased
end;

procedure TAndroidInAppPurchaseService.SetApplicationLicenseKey(const Value: string);
begin
  FApplicationLicenseKey := Value;
  FBase64PublicKey := StringToJString(Value);
end;

procedure TAndroidInAppPurchaseService.SetTransactionPayload(const Value: string);
begin
  FTransactionPayload := Value;
end;

procedure RegisterInAppPurchaseService;
begin
  InAppPurchaseService := TAndroidInAppPurchaseService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXInAppPurchaseService, InAppPurchaseService);
end;

procedure UnRegisterInAppPurchaseService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXInAppPurchaseService);
  InAppPurchaseService := nil;
end;

end.
