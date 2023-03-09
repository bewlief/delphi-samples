{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Advertising.Firebase.iOS;

interface

implementation

uses
  FMX.Types,
  System.Types, System.Classes, System.Math, System.TypInfo, System.Messaging, System.SysUtils,
  Macapi.Helpers, Macapi.ObjectiveC,
  iOSapi.GoogleMobileAds, iOSapi.FirebaseCommon, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.Helpers,
  FMX.Advertising.Firebase, FMX.Presentation.iOS, FMX.Presentation.Messages, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation,
  FMX.Controls.Model, FMX.Platform.iOS;

const
  cFirebaseBannerAdTestAdUnitID = 'ca-app-pub-3940256099942544/2934735716';

type
  IGADBannerView = interface(UIView)
    ['{7595F980-6EE8-4B8B-9A02-864433DD4C2E}']
  end;

  TiOSFirebaseBannerAd = class;

  TGADBannerViewDelegate = class(TOCLocal, GADBannerViewDelegate)
  private
    FBannerAd: TiOSFirebaseBannerAd;
  public
    { GADBannerViewDelegate }
    procedure adView(bannerView: GADBannerView; error: GADRequestError); cdecl;
    procedure adViewDidDismissScreen(bannerView: GADBannerView); cdecl;
    procedure adViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure adViewWillDismissScreen(bannerView: GADBannerView); cdecl;
    procedure adViewWillLeaveApplication(bannerView: GADBannerView); cdecl;
    procedure adViewWillPresentScreen(bannerView: GADBannerView); cdecl;
  public
    constructor Create(const ABannerAd: TiOSFirebaseBannerAd);
  end;

  TiOSFirebaseBannerAd = class(TiOSNativeControl)
  private
    FBannerView: GADBannerView;
    FDelegate: TGADBannerViewDelegate;
    procedure AdSizeChanged;
    procedure AdUnitIdChanged;
    function GetAdControl: TCustomFirebaseBannerAd;
    function GetAdSize: GADAdSize;
    function GetModel: TCustomFirebaseBannerAdModel;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure MMAdSizeChanged(var AMessage: TDispatchMessage); message MM_FIREBASE_ADSIZE_CHANGED;
    procedure MMAdUnitIdChanged(var AMessage: TDispatchMessage); message MM_FIREBASE_ADUNITID_CHANGED;
    procedure MMLoadAd(var AMessage: TDispatchMessage); message MM_FIREBASE_LOADAD;
    procedure MMTestModeChanged(var AMessage: TDispatchMessage); message MM_FIREBASE_TESTMODE_CHANGED;
    procedure PMGetAdjustedSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_ADJUST_SIZE;
    procedure PMGetAdjustedType(var AMessage: TDispatchMessageWithValue<TAdjustType>); message PM_GET_ADJUST_TYPE;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
    property AdControl: TCustomFirebaseBannerAd read GetAdControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomFirebaseBannerAdModel read GetModel;
    property BannerView: GADBannerView read FBannerView;
  end;

{ TGADBannerViewDelegate }

constructor TGADBannerViewDelegate.Create(const ABannerAd: TiOSFirebaseBannerAd);
begin
  inherited Create;
  FBannerAd := ABannerAd;
end;

procedure TGADBannerViewDelegate.adView(bannerView: GADBannerView; error: GADRequestError);
begin
  FBannerAd.AdControl.DoDidFailToReceiveAd(NSStrToStr(error.localizedDescription));
end;

procedure TGADBannerViewDelegate.adViewDidDismissScreen(bannerView: GADBannerView);
begin
  FBannerAd.AdControl.DoDidDismissScreen;
end;

procedure TGADBannerViewDelegate.adViewDidReceiveAd(bannerView: GADBannerView);
begin
  FBannerAd.AdControl.DoDidReceiveAd;
end;

procedure TGADBannerViewDelegate.adViewWillDismissScreen(bannerView: GADBannerView);
begin
  FBannerAd.AdControl.DoDidDismissScreen;
end;

procedure TGADBannerViewDelegate.adViewWillLeaveApplication(bannerView: GADBannerView);
begin
  FBannerAd.AdControl.DoWillLeaveApplication;
end;

procedure TGADBannerViewDelegate.adViewWillPresentScreen(bannerView: GADBannerView);
begin
  FBannerAd.AdControl.DoWillPresentScreen;
end;

{ TiOSFirebaseBannerAd }

constructor TiOSFirebaseBannerAd.Create;
var
  Constraint: NSLayoutConstraint;
begin
  TGADMobileAds.OCClass.sharedInstance.startWithCompletionHandler(nil);
  inherited;

  FBannerView := TGADBannerView.Create;
  // GADBannerView has a own system of alignment of its content and some adjusting issue with it. For example,
  // For example, the position of the content is stuck inside the banner. Therefore, when we dynamically change
  // the banner size, the content is not aligned and is always linked to the same position point.
  // It's important to pass size of zero size, it sets this position content point to (0, 0) and we can later just
  // change parent container view size.
  FBannerView.initWithAdSize(GADAdSizeFromCGSize(CGSizeMake(0, 0)));
  FBannerView.setTranslatesAutoresizingMaskIntoConstraints(False);
  FDelegate := TGADBannerViewDelegate.Create(Self);
  BannerView.setDelegate(FDelegate.GetObjectID);
  BannerView.setRootViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController);

  View.addSubview(BannerView);
  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(
    NSObjectToID(BannerView),
    NSLayoutAttributeTop,
    NSLayoutRelationEqual,
    NSObjectToID(view),
    NSLayoutAttributeTop,
    1,
    0));
  Constraint.setActive(True);

  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(
    NSObjectToID(BannerView),
    NSLayoutAttributeBottom,
    NSLayoutRelationEqual,
    NSObjectToID(view),
    NSLayoutAttributeBottom,
    1,
    0));
  Constraint.setActive(True);

  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(
    NSObjectToID(BannerView),
    NSLayoutAttributeLeft,
    NSLayoutRelationEqual,
    NSObjectToID(view),
    NSLayoutAttributeLeft,
    1,
    0));
  Constraint.setActive(True);

  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(
    NSObjectToID(BannerView),
    NSLayoutAttributeRight,
    NSLayoutRelationEqual,
    NSObjectToID(view),
    NSLayoutAttributeRight,
    1,
    0));
  Constraint.setActive(True);

  View.updateConstraints;
end;

destructor TiOSFirebaseBannerAd.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TiOSFirebaseBannerAd.DefineModelClass: TDataModelClass;
begin
  Result := TCustomFirebaseBannerAdModel;
end;

function TiOSFirebaseBannerAd.GetAdControl: TCustomFirebaseBannerAd;
begin
  Result := TCustomFirebaseBannerAd(Control);
end;

function TiOSFirebaseBannerAd.GetAdSize: GADAdSize;
begin
  case Model.AdSize of
    TFirebaseBannerAdSize.Banner:
      Result := kGADAdSizeBanner;
    TFirebaseBannerAdSize.LargeBanner:
      Result := kGADAdSizeLargeBanner;
    TFirebaseBannerAdSize.MediumRectangle:
      Result := kGADAdSizeMediumRectangle;
    TFirebaseBannerAdSize.FullBanner:
      Result := kGADAdSizeFullBanner;
    TFirebaseBannerAdSize.LeaderBoard:
      Result := kGADAdSizeLeaderboard
  else
    Result := kGADAdSizeBanner;
  end;
  Result := GADAdSizeFromCGSize(Result.size);
end;

function TiOSFirebaseBannerAd.GetModel: TCustomFirebaseBannerAdModel;
begin
  Result := inherited GetModel<TCustomFirebaseBannerAdModel>;
end;

function TiOSFirebaseBannerAd.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IGADBannerView);
end;

procedure TiOSFirebaseBannerAd.MMAdSizeChanged(var AMessage: TDispatchMessage);
begin
  AdSizeChanged;
end;

procedure TiOSFirebaseBannerAd.MMAdUnitIdChanged(var AMessage: TDispatchMessage);
begin
  AdUnitIdChanged;
end;

procedure TiOSFirebaseBannerAd.AdSizeChanged;
begin
  Control.Size.Size := TSizeF.Create(GetAdSize.size.width, GetAdSize.size.height);
  // GADBannerView looks at the actual view size and fits banner to its size.
  // So if we use small view size and set large banner, large banner will be ignored. For avoiding it, we have to force
  // layout view after settings size. Otherwise, the banner uses the old size.
  View.layoutSubviews;
  BannerView.setNeedsLayout;
  BannerView.layoutIfNeeded;
  BannerView.setAdSize(GetAdSize);
end;

procedure TiOSFirebaseBannerAd.AdUnitIdChanged;
begin
  if Model.TestMode then
    BannerView.setAdUnitID(StrToNSStr(cFirebaseBannerAdTestAdUnitID))
  else
    BannerView.setAdUnitID(StrToNSStr(Model.AdUnitID));
end;

procedure TiOSFirebaseBannerAd.MMLoadAd(var AMessage: TDispatchMessage);
begin
  BannerView.loadRequest(TGADRequest.Create);
end;

procedure TiOSFirebaseBannerAd.MMTestModeChanged(var AMessage: TDispatchMessage);
begin
  AdUnitIdChanged;
end;

procedure TiOSFirebaseBannerAd.PMGetAdjustedSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  AMessage.Value := GetAdSize.size.ToSizeF;
end;

procedure TiOSFirebaseBannerAd.PMGetAdjustedType(var AMessage: TDispatchMessageWithValue<TAdjustType>);
begin
  AMessage.Value := TAdjustType.FixedSize;
end;

procedure TiOSFirebaseBannerAd.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
end;

initialization
  TPresentationProxyFactory.Current.Register(TFirebaseBannerAd, TControlType.Platform, TiOSPresentationProxy<TiOSFirebaseBannerAd>);

finalization
  TPresentationProxyFactory.Current.Unregister(TFirebaseBannerAd, TControlType.Platform, TiOSPresentationProxy<TiOSFirebaseBannerAd>);

end.

