
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
  System,
  System.Data,
  System.Configuration,
  System.Web,
  System.Web.Security,
  System.Web.SessionState,
  System.Web.UI,
  System.Web.UI.WebControls,
  System.Web.UI.WebControls.WebParts,
  System.Web.UI.HtmlControls;

type
  ucThemeMasterPage = public partial class(System.Web.UI.UserControl)
  protected
    method lbMasterPage_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    method lbTheme_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    method Page_Load(sender: Object; e: EventArgs);
  end;

implementation

method ucThemeMasterPage.Page_Load(sender: Object; e: EventArgs);
begin

  lbTheme.Visible := HttpContext.Current.User.Identity.IsAuthenticated;
  lbMasterPage.Visible := HttpContext.Current.User.Identity.IsAuthenticated;

  if lbTheme.Visible then
  begin
     if (not isPostBack ) and Assigned(Session[UserKey]) then
     begin
        lbTheme.SelectedValue := (Session[UserKey] as TUser).Theme;
        lbMasterPage.SelectedValue := Session[MyMasterPageKey].ToString;
     end;
  end;

end;

method ucThemeMasterPage.lbTheme_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
begin
  if Assigned(Session[UserKey]) then
  begin
     (Session[UserKey] as TUser).Theme := lbTheme.SelectedValue;
     Response.Redirect(Request.Url.AbsolutePath);
  end;  
end;

method ucThemeMasterPage.lbMasterPage_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
begin
  Session[MyMasterPageKey] := lbMasterPage.SelectedValue;
  Response.Redirect('Pictures.aspx');  
end;

end.
