{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Advertising.Firebase;

interface

uses
  System.Classes, System.Types,
  FMX.Types, FMX.Controls.Presentation, FMX.Controls.Model;

const
  MM_FIREBASE_ADUNITID_CHANGED = MM_USER + 1;
  MM_FIREBASE_LOADAD = MM_USER + 2;
  MM_FIREBASE_TESTMODE_CHANGED = MM_USER + 3;
  MM_FIREBASE_ADSIZE_CHANGED = MM_USER + 4;

type
  TFirebaseBannerAdSize = (Banner, LargeBanner, MediumRectangle, FullBanner, Leaderboard);

  TDidFailToReceiveAdEvent = procedure(Sender: TObject; const Error: string) of object;

  TCustomFirebaseBannerAdModel = class(TDataModel)
  private
    FAdSize: TFirebaseBannerAdSize;
    FAdUnitID: string;
    FTestMode: Boolean;
    procedure SetAdSize(const Value: TFirebaseBannerAdSize);
    procedure SetAdUnitID(const Value: string);
    procedure SetTestMode(const Value: Boolean);
  public
    procedure LoadAd;
    property AdSize: TFirebaseBannerAdSize read FAdSize write SetAdSize;
    property AdUnitID: string read FAdUnitID write SetAdUnitID;
    property TestMode: Boolean read FTestMode write SetTestMode;
  end;

  TCustomFirebaseBannerAd = class(TPresentedControl)
  private
    FOnDidDismissScreen: TNotifyEvent;
    FOnDidFailToReceiveAd: TDidFailToReceiveAdEvent;
    FOnDidReceiveAd: TNotifyEvent;
    FOnWillDismissScreen: TNotifyEvent;
    FOnWillLeaveApplication: TNotifyEvent;
    FOnWillPresentScreen: TNotifyEvent;
    function GetAdSize: TFirebaseBannerAdSize;
    function GetAdUnitID: string;
    function GetModel: TCustomFirebaseBannerAdModel; overload;
    function GetTestMode: Boolean;
    procedure SetAdSize(const Value: TFirebaseBannerAdSize);
    procedure SetAdUnitID(const Value: string);
    procedure SetTestMode(const Value: Boolean);
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure Paint; override;
  public
    procedure DoDidDismissScreen;
    procedure DoDidFailToReceiveAd(const AError: string);
    procedure DoDidReceiveAd;
    procedure DoWillDismissScreen;
    procedure DoWillLeaveApplication;
    procedure DoWillPresentScreen;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadAd;
    property AdSize: TFirebaseBannerAdSize read GetAdSize write SetAdSize;
    property AdUnitID: string read GetAdUnitID write SetAdUnitID;
    property Model: TCustomFirebaseBannerAdModel read GetModel;
    property TestMode: Boolean read GetTestMode write SetTestMode default False;
    property OnDidDismissScreen: TNotifyEvent read FOnDidDismissScreen write FOnDidDismissScreen;
    property OnDidFailToReceiveAd: TDidFailToReceiveAdEvent read FOnDidFailToReceiveAd write FOnDidFailToReceiveAd;
    property OnDidReceiveAd: TNotifyEvent read FOnDidReceiveAd write FOnDidReceiveAd;
    property OnWillDismissScreen: TNotifyEvent read FOnWillDismissScreen write FOnWillDismissScreen;
    property OnWillLeaveApplication: TNotifyEvent read FOnWillLeaveApplication write FOnWillLeaveApplication;
    property OnWillPresentScreen: TNotifyEvent read FOnWillPresentScreen write FOnWillPresentScreen;
  end;

  [ComponentPlatformsAttribute(pfidiOS)]
  TFirebaseBannerAd = class(TCustomFirebaseBannerAd)
  published
    property Align;
    property Anchors;
    property Height;
    property Margins;
    property Position;
    property TestMode;
    property Visible default True;
    property Width;
    property OnDidDismissScreen;
    property OnDidFailToReceiveAd;
    property OnDidReceiveAd;
    property OnResize;
    property OnResized;
    property OnWillDismissScreen;
    property OnWillLeaveApplication;
    property OnWillPresentScreen;
  end;

implementation

uses
  System.SysUtils,
  {$IF Defined(IOS)}
  FMX.Advertising.Firebase.iOS,
  {$ENDIF}
  FMX.Platform, FMX.Graphics, FMX.Controls;

{ TCustomFirebaseBannerAdModel }

procedure TCustomFirebaseBannerAdModel.LoadAd;
begin
  SendMessage(MM_FIREBASE_LOADAD);
end;

procedure TCustomFirebaseBannerAdModel.SetAdSize(const Value: TFirebaseBannerAdSize);
begin
  if Value <> FAdSize then
  begin
    FAdSize := Value;
    SendMessage(MM_FIREBASE_ADSIZE_CHANGED);
  end;
end;

procedure TCustomFirebaseBannerAdModel.SetAdUnitID(const Value: string);
begin
  if Value <> FAdUnitID then
  begin
    FAdUnitID := Value;
    SendMessage(MM_FIREBASE_ADUNITID_CHANGED);
  end;
end;

procedure TCustomFirebaseBannerAdModel.SetTestMode(const Value: Boolean);
begin
  if Value <> FTestMode then
  begin
    FTestMode := Value;
    SendMessage(MM_FIREBASE_TESTMODE_CHANGED);
  end;
end;

{ TCustomFirebaseBannerAd }

constructor TCustomFirebaseBannerAd.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
end;

function TCustomFirebaseBannerAd.DefineModelClass: TDataModelClass;
begin
  Result := TCustomFirebaseBannerAdModel;
end;

procedure TCustomFirebaseBannerAd.DoDidDismissScreen;
begin
  if Assigned(FOnDidDismissScreen) then
    FOnDidDismissScreen(Self);
end;

procedure TCustomFirebaseBannerAd.DoDidFailToReceiveAd(const AError: string);
begin
  if Assigned(FOnDidFailToReceiveAd) then
    FOnDidFailToReceiveAd(Self, AError);
end;

procedure TCustomFirebaseBannerAd.DoDidReceiveAd;
begin
  if Assigned(FOnDidReceiveAd) then
    FOnDidReceiveAd(Self);
end;

procedure TCustomFirebaseBannerAd.DoWillDismissScreen;
begin
  if Assigned(FOnWillDismissScreen) then
    FOnWillDismissScreen(Self);
end;

procedure TCustomFirebaseBannerAd.DoWillLeaveApplication;
begin
  if Assigned(FOnWillLeaveApplication) then
    FOnWillLeaveApplication(Self);
end;

procedure TCustomFirebaseBannerAd.DoWillPresentScreen;
begin
  if Assigned(FOnWillPresentScreen) then
    FOnWillPresentScreen(Self);
end;

function TCustomFirebaseBannerAd.GetAdSize: TFirebaseBannerAdSize;
begin
  Result := Model.AdSize;
end;

function TCustomFirebaseBannerAd.GetAdUnitID: string;
begin
  Result := Model.AdUnitId;
end;

function TCustomFirebaseBannerAd.GetModel: TCustomFirebaseBannerAdModel;
begin
  Result := GetModel<TCustomFirebaseBannerAdModel>;
end;

function TCustomFirebaseBannerAd.GetTestMode: Boolean;
begin
  Result := Model.TestMode;
end;

procedure TCustomFirebaseBannerAd.LoadAd;
begin
  Model.LoadAd;
end;

procedure TCustomFirebaseBannerAd.Paint;
begin
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure TCustomFirebaseBannerAd.SetAdSize(const Value: TFirebaseBannerAdSize);
begin
  Model.AdSize := Value;
end;

procedure TCustomFirebaseBannerAd.SetAdUnitID(const Value: string);
begin
  Model.AdUnitID := Value;
end;

procedure TCustomFirebaseBannerAd.SetTestMode(const Value: Boolean);
begin
  Model.TestMode := Value;
end;

end.
