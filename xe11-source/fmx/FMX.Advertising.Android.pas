{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
unit FMX.Advertising.Android;

interface

uses System.SysUtils;

{$SCOPEDENUMS ON}

procedure RegisterAdvertisingService;
procedure UnregisterAdvertisingService;

type
  ETestModeException = Exception;

implementation

uses
  System.Classes, System.Types, System.Generics.Collections, FMX.Controls, FMX.Types, FMX.Forms, System.Messaging,
  Androidapi.Helpers, FMX.Platform, FMX.Platform.Android, FMX.Advertising, FMX.Helpers.Android, Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Embarcadero, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.AdMob,
  Androidapi.JNI.Os, Androidapi.JNI.Widget, Androidapi.JNI.App, FMX.Consts, FMX.ZOrder.Android;

function IsPortrait: Boolean;
var
  ScreenService: IFMXScreenService;
begin
  Result := True;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    Result := ScreenService.GetScreenOrientation in [TScreenOrientation.Portrait,
      TScreenOrientation.InvertedPortrait];
end;

type
  TAndroidBannerAd = class;

  TAdViewListener = class(TJavaLocal, JIAdListener)
  private
    [Weak] FBanner : TAndroidBannerAd;
  public
    constructor Create(const ABanner: TAndroidBannerAd);
    destructor Destroy; override;
    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(adError: JLoadAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
  end;

  TAndroidAdvertisingService = class(TInterfacedObject, IFMXAdvertisingService, IFMXAdvertisingTestModeService)
  private
    FTestDeviceID: string;
    FUseTestAds: Boolean;
  public
    constructor Create;
    { IFMXAdvertisingService }
    function CreateBannerAd(const AOwner: TCustomBannerAd): IBannerAd;
    procedure RemoveAd(const Ad: ICommonAd);
    { IFMXAdvertisingTestModeService }
    procedure SetTestMode(AValue: Boolean);
    function GetTestMode: Boolean;
    procedure SetTestModeDeviceID(const ADeviceID: string);
  end;

  TAndroidBannerAd = class(TInterfacedObject, IBannerAd)
  private
    FAdView : JAdView;
    FAdViewContainer: JViewGroup;
    FChildrenContainer: JViewGroup;
    FAdRequest: JAdRequest;
    FAdViewListener: TAdViewListener;
    FAdControl: TCustomBannerAd;
    FAdUnitID: string;
    FOrientationChangedId: Integer;
    FActionIsInProgress: Boolean;
    FLoaded: Boolean;
    procedure InitAd;
    procedure FreeAd;
    procedure OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
    function GetZOrderManager: TAndroidZOrderManager;
  protected
    function GetParent: TFmxObject;
    function GetVisible: Boolean;
    procedure Show;
    procedure Hide;
    procedure UpdateControlMetrics;
    procedure UpdateContentFromControl;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(const AOwner: TCustomBannerAd);
    destructor Destroy; override;

    destructor Free;
    { ICommonAd }
    procedure CancelAction;
    function IsActionInProgress: Boolean;
    function IsLoaded: Boolean;
    function GetAdUnitID: string;
    procedure SetAdUnitID(const AValue: string);
    procedure LoadAd;
    { IBannerAd }
    procedure SetBannerAdControl(const AValue: TCustomBannerAd);

    property ZOrderManager: TAndroidZOrderManager read GetZOrderManager;
  end;

var
  AdvertisingService : TAndroidAdvertisingService;

{ TAndroidAdvertisingService }

constructor TAndroidAdvertisingService.Create;
begin
  inherited Create;
  FUseTestAds := False;
end;

function TAndroidAdvertisingService.CreateBannerAd(const AOwner: TCustomBannerAd): IBannerAd;
begin
  Result := TAndroidBannerAd.Create(AOwner);
end;

procedure TAndroidAdvertisingService.RemoveAd(const Ad: ICommonAd);
begin
end;

procedure TAndroidAdvertisingService.SetTestModeDeviceID(const ADeviceID: string);
begin
  FTestDeviceID := ADeviceID;
end;

procedure TAndroidAdvertisingService.SetTestMode(AValue: Boolean);
begin
  if AValue then
  begin
    if FTestDeviceID = '' then
      FTestDeviceID := JStringToString(MainActivity.getDeviceID);
    if FTestDeviceID = '' then
      raise ETestModeException.Create(SCannotGetDeviceIDForTestAds);
  end;
  FUseTestAds := AValue;
end;

function TAndroidAdvertisingService.GetTestMode: Boolean;
begin
  Result := FUseTestAds;
end;

{ TAndroidBannerAd }

constructor TAndroidBannerAd.Create(const AOwner: TCustomBannerAd);
begin
  inherited Create;
  FAdControl := AOwner;
  FOrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage,
    OrientationChangedHandler);
end;

destructor TAndroidBannerAd.Destroy;
begin
  FreeAd;
  inherited;
end;

destructor TAndroidBannerAd.Free;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, FOrientationChangedId);
  if FAdUnitId <> '' then
    FreeAd;
  inherited Destroy;
end;

procedure TAndroidBannerAd.CancelAction;
begin
  //Android doesn't offer a way to do this
end;

procedure TAndroidBannerAd.FreeAd;
begin
  if ZOrderManager <> nil then
    ZOrderManager.RemoveLink(FAdControl);

  FAdRequest := nil;

  if FAdView <> nil then
  begin  
    FAdView.setAdListener(nil);
    FAdView.destroy;
    FAdView := nil;

    FAdViewListener := nil;
  end;

  FLoaded := False;
end;

function TAndroidBannerAd.GetAdUnitID: string;
begin
  Result := FAdUnitID;
end;

function TAndroidBannerAd.GetParent: TFmxObject;
begin
  Result := nil;
  if FAdControl <> nil then
    Result := FAdControl.Parent;
end;

function TAndroidBannerAd.GetVisible: Boolean;
begin
  Result := False;
  if FAdControl <> nil then
    Result := FAdControl.Visible;
end;

function TAndroidBannerAd.GetZOrderManager: TAndroidZOrderManager;
var
  Form: TCommonCustomForm;
begin
  Result := nil;
  if (FAdControl <> nil) and (FAdControl.Root is TCommonCustomForm) then
  begin
    Form := TCommonCustomForm(FAdControl.Root);
    if Form.IsHandleAllocated then
      Result := WindowHandleToPlatform(Form.Handle).ZOrderManager;
  end;
end;

procedure TAndroidBannerAd.Hide;
begin
  FAdViewContainer.setVisibility(TJView.JavaClass.INVISIBLE);
end;

procedure TAndroidBannerAd.InitAd;

  function BannerAdSizeToAdSize(const AdSize: TBannerAdSize): JAdSize;
  begin
    case AdSize of
      TBannerAdSize.Auto: Result := TJAdSize.JavaClass.SMART_BANNER;
      TBannerAdSize.Small: Result := TJAdSize.JavaClass.BANNER;
      TBannerAdSize.Medium: Result := TJAdSize.JavaClass.FULL_BANNER;
      TBannerAdSize.Large: Result := TJAdSize.JavaClass.LEADERBOARD;
    end
  end;

var
  AdSize: JAdSize;
  TestDeviceId: JString;
  TestDeviceIds: JArrayList;
  RequestConfiguration: JRequestConfiguration;
  RequestConfigurationBuilder: JRequestConfiguration_Builder;
  AdRequestBuilder: JAdRequest_Builder;
  LayoutParams: JRelativeLayout_LayoutParams;
begin
  if FAdControl <> nil then
    AdSize := BannerAdSizeToAdSize(FAdControl.AdSize)
  else
    AdSize := TJAdSize.JavaClass.SMART_BANNER;

  FAdViewListener := TAdViewListener.Create(Self);

  FAdView := TJAdView.JavaClass.init(TAndroidHelper.Activity);
  FAdView.setAdUnitId(StringToJString(FAdUnitID));
  FAdView.setAdSize(AdSize);
  FAdView.setAdListener(TJAdListenerAdapter.JavaClass.init(FAdViewListener));

  if AdvertisingService.FUseTestAds then
  begin
    RequestConfiguration := TJMobileAds.JavaClass.getRequestConfiguration;

    TestDeviceId := StringToJString(AdvertisingService.FTestDeviceID);

    TestDeviceIds := TJArrayList.JavaClass.init(RequestConfiguration.getTestDeviceIds);

    if not TestDeviceIds.contains(TestDeviceId) then
      TestDeviceIds.add(TestDeviceId);

    RequestConfigurationBuilder := RequestConfiguration.toBuilder;
    RequestConfigurationBuilder.setTestDeviceIds(TJList.Wrap(TestDeviceIds));

    RequestConfiguration := RequestConfigurationBuilder.build;

    TJMobileAds.JavaClass.setRequestConfiguration(RequestConfiguration);
  end;

  AdRequestBuilder := TJAdRequest_Builder.JavaClass.init;

  FAdRequest := AdRequestBuilder.build;

  FAdViewContainer := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
  FChildrenContainer := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FAdViewContainer.addView(FAdView, LayoutParams);
  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FAdViewContainer.addView(FChildrenContainer, LayoutParams);

  FAdView.loadAd(FAdRequest);
end;

function TAndroidBannerAd.IsActionInProgress: Boolean;
begin
  Result := FActionIsInProgress;
end;

function TAndroidBannerAd.IsLoaded: Boolean;
begin
  Result := FLoaded;
end;

procedure TAndroidBannerAd.LoadAd;
begin
  FLoaded := False;
  if FAdView <> nil then
    FreeAd;
  InitAd;
end;

procedure TAndroidBannerAd.OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
var
  OldControl: TCustomBannerAd;
begin
  if (FAdControl <> nil) and FAdControl.Visible and FLoaded then
  begin
    //Avoid issue with portrait ad still being visible when form has rotated to landscape
    Hide;
    //Kill off existing ad and recreate it
    LoadAd;
    //Kill off old layout and recreate it
    OldControl := FAdControl;
    SetBannerAdControl(nil);
    SetBannerAdControl(OldControl);
    UpdateContentFromControl;
  end;
end;

function TAndroidBannerAd.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FAdView <> nil) and (Result <> S_OK) then
    Result := FAdView.QueryInterface(IID, Obj);
  if (FAdRequest <> nil) and (Result <> S_OK) then
    Result := FAdRequest.QueryInterface(IID, Obj);
  if (FAdViewListener <> nil) and (Result <> S_OK) then
    Result := FAdViewListener.QueryInterface(IID, Obj);
end;

procedure TAndroidBannerAd.SetAdUnitID(const AValue: string);
begin
  FAdUnitID := AValue;
end;

procedure TAndroidBannerAd.SetBannerAdControl(const AValue: TCustomBannerAd);
begin
  FAdControl := AValue;
  UpdateControlMetrics;
end;

procedure TAndroidBannerAd.Show;
begin
  FAdViewContainer.setVisibility(TJView.JavaClass.VISIBLE);
end;

procedure TAndroidBannerAd.UpdateContentFromControl;
begin
  if (ZOrderManager = nil) or (FAdControl = nil) or not IsLoaded then
    Exit;

  ZOrderManager.AddOrSetLink(FAdControl, FAdView, nil);
  ZOrderManager.UpdateOrderAndBounds(FAdControl);
end;

procedure TAndroidBannerAd.UpdateControlMetrics;
var
  Frame: TRectF;
  Form: TCommonCustomForm;
const
  AdHeightPortrait = 90;
  AdHeightLandscape = 90;
  AdHeightPhonePortrait = 50;
  AdHeightPhoneLandscape = 32;
  AndroidSW600DP = 600;
  AndroidSW600DPLandscape = 552;
  AdMobBannerHeight = 50;
  AdMobFullBannerHeight = 60;
  AdMobLeaderboardHeight = 90;
begin
                                                                                             
  if (FAdControl <> nil) and not Assigned(FAdControl.OnResize) then
  begin
    case FAdControl.AdSize of
      TBannerAdSize.Auto:
        begin
          if Screen.Size.Height > Screen.Size.Width then
            if Screen.Size.Width < AndroidSW600DP then
              FAdControl.Height := AdHeightPhonePortrait
            else
              FAdControl.Height := AdHeightPortrait
          else
            if Screen.Size.Height < AndroidSW600DPLandscape then
              FAdControl.Height := AdHeightPhoneLandscape
            else
              FAdControl.Height := AdHeightLandscape;
        end;
      TBannerAdSize.Small:
        FAdControl.Height := AdMobBannerHeight;
      TBannerAdSize.Medium:
        FAdControl.Height := AdMobFullBannerHeight;
      TBannerAdSize.Large:
        FAdControl.Height := AdMobLeaderboardHeight;
    end;

    if Screen.ActiveForm <> nil then
      Form := Screen.ActiveForm
    else if Application.MainForm <> nil then
      Form := Application.MainForm
    else
      Form := nil;
    if Form <> nil then
    begin
      Frame := WindowHandleToPlatform(Form.Handle).Bounds;
      FAdControl.Width := Round(Frame.Width);
    end;
  end;
end;

{ TAdViewListener }

constructor TAdViewListener.Create(const ABanner: TAndroidBannerAd);
begin
  inherited Create;
  FBanner := ABanner;
end;

destructor TAdViewListener.Destroy;
begin
  FBanner := nil;
  inherited;
end;

procedure TAdViewListener.onAdClicked;
begin
  // Not applicable.
end;

procedure TAdViewListener.onAdClosed;
begin
  FBanner.FActionIsInProgress := False;

  if FBanner.FAdControl <> nil then
    FBanner.FAdControl.DoActionDidFinish;
end;

procedure TAdViewListener.onAdFailedToLoad(adError: JLoadAdError);
begin
  if FBanner.FAdControl <> nil then
    FBanner.FAdControl.DoDidFail(JStringToString(adError.getMessage));
end;

procedure TAdViewListener.onAdImpression;
begin
  // Not applicable.
end;

procedure TAdViewListener.onAdLoaded;
begin
  FBanner.FLoaded := True;

  if FBanner.FAdControl <> nil then
    FBanner.FAdControl.DoDidLoad;
end;

procedure TAdViewListener.onAdOpened;
var
  WillLeaveApplication: Boolean;
begin
  FBanner.FActionIsInProgress := True;

  if FBanner.FAdControl <> nil then
  begin
    WillLeaveApplication := True;

    FBanner.FAdControl.DoActionCanBegin(WillLeaveApplication);
  end;
end;

procedure RegisterAdvertisingService;
begin
  AdvertisingService := TAndroidAdvertisingService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXAdvertisingService, AdvertisingService);
  TPlatformServices.Current.AddPlatformService(IFMXAdvertisingTestModeService, AdvertisingService);
end;

procedure UnregisterAdvertisingService;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXAdvertisingService);
    TPlatformServices.Current.RemovePlatformService(IFMXAdvertisingTestModeService);
  end;
end;

end.
