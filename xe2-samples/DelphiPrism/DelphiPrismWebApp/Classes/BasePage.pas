
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
namespace DelphiPrismWebApp;

interface

uses
  System.Web.UI,
  Borland.Data.*;

type
  TBasePage = public class(System.Web.UI.Page)
  protected
     FAdoDbxConnection :  TAdoDbxConnection;

    procedure OnPreInit( e: EventArgs);override;
    procedure OnLoadComplete(e: EventArgs); override;
    method getAdoDbxConnection: TAdoDbxConnection;
    method CloseDbConnection;

  public
    constructor Create;
    property AdoDbxConnection : TAdoDbxConnection read getAdoDbxConnection;

    finalizer Finalize;
  end;

implementation

constructor TBasePage.Create;
begin
  inherited Create;
  // TODO: Add any constructor code here
end;

procedure TBasePage.OnPreInit(e: EventArgs);
var
  u : TUser;
begin
  inherited OnPreInit(e);
  Page.MasterPageFile := Session[MyMasterPageKey].ToString;
  u := (Session.Item[UserKey] as TUser);
  if Assigned(u) then
  begin
     Page.Theme := u.Theme;
  end;

end;

finalizer TBasePage.Finalize;
begin
  CloseDbConnection;
end;

method TBasePage.getAdoDbxConnection: TAdoDbxConnection;
begin
  if not assigned(FAdoDbxConnection) then
     FAdoDbxConnection := TDBConnection.Create.MyAdoDbxConnection;

  result := FAdoDbxConnection;
end;

procedure TBasePage.OnLoadComplete(e: EventArgs);
begin
  inherited OnLoadComplete(e);
  CloseDbConnection;
end;

method TBasePage.CloseDbConnection;
begin
  if assigned(AdoDbxConnection) then
  begin
    AdoDbxConnection.Close;
  end;  

end;

end.