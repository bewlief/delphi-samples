
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
  ucStatistics = public partial class(System.Web.UI.UserControl)
  protected
    method Page_Load(sender: Object; e: EventArgs);
    method DataBind(raiseOnDataBinding: Boolean); override;
  end;

implementation

method ucStatistics.Page_Load(sender: Object; e: EventArgs);
begin

end;

method ucStatistics.DataBind(raiseOnDataBinding: Boolean);
Const
  msgsession = 'Page access count: {0}';
  msguseron = 'Total logged-in users: {0}';
  msgapp = 'Web Site access count: {0} - Date/Time last visit: {1}';
var
  appc: TApplicationCounter;
begin
  appc := TApplicationCounter(Application.Item[AppCounterKey]);

  if not Request.Url.AbsolutePath.toString.Contains('Default.aspx') then
    if (not HttpContext.Current.User.Identity.IsAuthenticated) then
      Response.Redirect('Default.aspx');

  lbAppCounter.Text := System.String.Format(msgapp, appc.Count.ToString, appc.LastVisit.ToString);
  lbPageCounter.Text := System.String.Format(msgsession, appc.Pages.Item[Request.Url.AbsolutePath].toString);
  lbUserOn.Text := System.String.Format(msguseron, appc.CountUserOn.toString);
end;

end.