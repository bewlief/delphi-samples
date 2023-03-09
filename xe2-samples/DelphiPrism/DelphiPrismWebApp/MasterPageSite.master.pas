
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
  System.Web.UI.WebControls;

type
  MasterPageSite = public partial class(System.Web.UI.MasterPage)
  protected 
    method Page_Load(sender : object; e : EventArgs);
  end;

implementation

method MasterPageSite.Page_Load(sender : object; e : EventArgs);
begin
  hlStatistics.Visible := HttpContext.Current.User.Identity.IsAuthenticated;
  hlLogoff.Visible := HttpContext.Current.User.Identity.IsAuthenticated;
  
end;


end.
