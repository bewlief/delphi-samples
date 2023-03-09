
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
  Statistics = public partial class(System.Web.UI.Page)
  protected
    method dgCounter_RowCreated(sender: System.Object; e: System.Web.UI.WebControls.GridViewRowEventArgs);
    method GridView1_PageIndexChanging(sender: System.Object; e: System.Web.UI.WebControls.GridViewPageEventArgs);
    method Page_Load(sender: Object; e: EventArgs);
  end;

implementation

method Statistics.Page_Load(sender: Object; e: EventArgs);
begin
  dgCounter.DataSource := TApplicationCounter(Application.Item['ApplicationCounter']).Pages;
  dgCounter.DataBind;
end;

method Statistics.GridView1_PageIndexChanging(sender: System.Object; e: System.Web.UI.WebControls.GridViewPageEventArgs);
begin
  dgCounter.PageIndex := e.NewPageIndex;  
end;

method Statistics.dgCounter_RowCreated(sender: System.Object; e: System.Web.UI.WebControls.GridViewRowEventArgs);
var
  o : System.Collections.DictionaryEntry;
  appc : TApplicationCounter;
begin

  case e.Row.RowType of
      DataControlRowType.DataRow:
              begin
                  
                  o := e.Row.DataItem as System.Collections.DictionaryEntry;
                  (e.Row.FindControl('lbPage') as Label ).Text  := o.Key.ToString;
                  (e.Row.FindControl('lbTotal') as Label ).Text := o.Value.ToString;
              end;
      DataControlRowType.Footer: 
              begin
                  appc :=  TApplicationCounter(Application.Item['ApplicationCounter']);
                  (e.Row.FindControl('lbVisit') as Label ).Text  := System.&String.Format('Last Visit: {0} ', appc.LastVisit.ToString );
              end;
  end; // case

end;

end.
