
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
  System.Collections.Generic,
  System.Web,
  System.Web.UI,
  System.Web.UI.WebControls,
  System.Web.Security;

type
  _Default1 = public partial class(TBasePage)
  protected
    method Button2_Click(sender: System.Object; e: System.EventArgs);
    method Page_Load(sender : object; e : EventArgs);

    method ValidateLogin: Boolean;
    method doValidate;

  end;

implementation
  
method _Default1.Page_Load(sender : object; e : EventArgs);
begin
  tbUser.Focus;
  if IsPostBack then
  begin
    Page.Validate;
    doValidate;
  end;  
end;
  
method _Default1.Button2_Click(sender: System.Object; e: System.EventArgs);
begin
  
end;

method _Default1.ValidateLogin: Boolean;
var
  user : TUser;
begin

  user := TUser.Create( tbUser.Text, tbPassword.Text, lbTheme.SelectedValue);
  Result := user.isValid;
  if Result then
  begin
    user.GenerateTicket;
    Session.Add(UserKey, user);
  end;
end;

method _Default1.doValidate;
var
  user : string;
begin
  user := tbUser.Text;

  if Page.IsValid and ValidateLogin then
  begin
    TApplicationCounter(Application.Item[AppCounterKey]).LoginUser;
    LBWarning.Visible := false;
    if HttpContext.Current.Request.QueryString['ReturnUrl'] = nil then
    begin
      FormsAuthentication.SetAuthCookie(user, False);
      HttpContext.Current.Response.Redirect('Pictures.aspx');
    end
    else
      FormsAuthentication.RedirectFromLoginPage(user, False);
  end
  else
  begin
    LBWarning.Visible := true;
  end;

end;

end.