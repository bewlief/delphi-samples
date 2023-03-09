
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
  System.Web,
  System.Web.Security;

type
  Logoff = public partial class(System.Web.UI.Page)
  protected
    method Page_Load(sender: Object; e: EventArgs);
  end;

implementation

method Logoff.Page_Load(sender: Object; e: EventArgs);
begin
  FormsAuthentication.SignOut;
  HttpContext.Current.Session.Abandon;
  Response.Redirect('~/Default.aspx');
end;

end.
