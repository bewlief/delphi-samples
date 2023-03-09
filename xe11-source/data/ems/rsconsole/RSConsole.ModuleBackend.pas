{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
unit RSConsole.ModuleBackend;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Backend.ServiceTypes,
  FMX.Grid, System.Generics.Collections, FMX.ListBox,
  REST.Backend.MetaTypes, REST.Backend.EMSServices, System.JSON, REST.Client,
  REST.Backend.EndPoint, REST.Backend.Providers, REST.Backend.ServiceComponents,
  REST.Backend.EMSProvider, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Backend.BindSource, RSSetUp.Config, RSConsole.Types,
  REST.Backend.PushTypes;

type
  TBackendDM = class(TDataModule)
    BackendAuth1: TBackendAuth;
    EMSProvider: TEMSProvider;
    BackendUsers1: TBackendUsers;
    BackendGroups1: TBackendGroups;
    BackendEndpoint1: TBackendEndpoint;
    BackendQuery1: TBackendQuery;
    BackendPush1: TBackendPush;
  private
    FSessionID: string;
    FClosing: Boolean;
    { Private declarations }
  public
    { Public declarations }
    procedure SetProviderConnectionInfo(ARegistry: TEMSManagementRegistry);
    procedure SetProviderCredentials(ARegistry: TEMSManagementRegistry);
    function Login(UseUser: Boolean): Boolean;
    function Logout: Boolean;
    procedure AppHandshake;
    property SessionID: string read FSessionID write FSessionID;
    property Closing: Boolean read FClosing write FClosing;
  end;

var
  BackendDM: TBackendDM;

implementation

uses FMX.Dialogs, IdHTTP, RSConsole.Consts, REST.Types,
  System.UITypes, REST.Backend.EMSMetaTypes,
  REST.Backend.EMSApi;

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}
{ TDataModule1 }

procedure TBackendDM.AppHandshake;
begin
  EMSProvider.AppHandshake(nil);
  ShowMessage(strConnectionSucc);
end;

function TBackendDM.Login(UseUser: Boolean): Boolean;
begin
  Result := False;
  BackendAuth1.Login;

  if BackendAuth1.LoggedIn then
  begin
    ShowMessage(strLoginSucc + #10#13 + strUserName + ': ' +
      BackendAuth1.UserName + #10#13'Token: ' + BackendAuth1.LoggedInToken);
    FSessionID := BackendAuth1.LoggedInToken;
    Result := True;
  end
  else
  begin
    MessageDlg(strUserName + ': ' + BackendAuth1.UserName + ' ' +
      strUserNotloggedIn, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    FSessionID := '';
  end;
end;

function TBackendDM.Logout: Boolean;
begin
  Result := False;
  BackendAuth1.Logout;
  if not BackendAuth1.LoggedIn then
  begin
    ShowMessage(strUser + ' ' + BackendAuth1.UserName + ' ' + strLoggedOut);
    Result := True;
    FSessionID := '';
  end;
end;

procedure TBackendDM.SetProviderConnectionInfo(ARegistry: TEMSManagementRegistry);
begin
  EMSProvider.URLHost := ARegistry.Connection.Host;
  EMSProvider.URLPort := ARegistry.Connection.Port;
  EMSProvider.URLProtocol := ARegistry.Connection.Protocol;
  EMSProvider.URLBasePath := ARegistry.Connection.BaseURL;
  BackendEndpoint1.Timeout := ARegistry.Connection.Timeout;
  EMSProvider.ProxyServer := ARegistry.Connection.ProxyServer;
  EMSProvider.ProxyPort := ARegistry.Connection.ProxyPort;
end;

procedure TBackendDM.SetProviderCredentials(ARegistry: TEMSManagementRegistry);
begin
  EMSProvider.UserName := ARegistry.Credentials.ServerUserName;
  EMSProvider.Password := ARegistry.Credentials.ServerPassword;
  BackendAuth1.UserName := ARegistry.Credentials.ServerUserName;
  BackendAuth1.Password := ARegistry.Credentials.ServerPassword;
  EMSProvider.MasterSecret := ARegistry.Credentials.MasterSecret;
  EMSProvider.AppSecret := ARegistry.Credentials.AppSecret;
  EMSProvider.ApplicationId := ARegistry.Credentials.ApplicationId;
  EMSProvider.TenantId := ARegistry.Credentials.TenantId;
  EMSProvider.TenantSecret := ARegistry.Credentials.TenantSecret;
  if ARegistry.Credentials.UseMasterSecret then
    BackendAuth1.Authentication := TBackendAuthentication.Root
  else
  begin
    EMSProvider.MasterSecret := '';
    if FSessionID = '' then
      BackendAuth1.Authentication := TBackendAuthentication.Default
    else
      BackendAuth1.Authentication := TBackendAuthentication.Session;
  end;
  if (ARegistry.Credentials.AppSecret <> '') and
    not(ARegistry.Credentials.UseMasterSecret) then
    BackendAuth1.Authentication := TBackendAuthentication.Application;
end;

end.
