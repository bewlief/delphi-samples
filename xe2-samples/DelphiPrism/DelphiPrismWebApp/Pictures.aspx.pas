
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
  System.Collections.Generic,
  System.Web,
  System.Web.UI,
  System.Web.UI.WebControls,
  Borland.Data;

type
  Pictures = public partial class(TBasePage)
  protected
    method GridView1_PageIndexChanging(sender: System.Object; e: System.Web.UI.WebControls.GridViewPageEventArgs);
    method Button1_Click(sender: System.Object; e: System.EventArgs);
    method GridView1_PageIndexChanged(sender: System.Object; e: System.EventArgs);
    method GridView1_RowCommand(sender: System.Object; e: System.Web.UI.WebControls.GridViewCommandEventArgs);
    method GridView1_RowCreated(sender: System.Object; e: System.Web.UI.WebControls.GridViewRowEventArgs);
    method GridView1_DataBinding(sender: System.Object; e: System.EventArgs);
    method Page_Load(sender : object; e : EventArgs);
  end;

implementation
  
method Pictures.Page_Load(sender : object; e : EventArgs);
Const
  Sql : String = 'SELECT C.ID_CITY, C.CITY, C.STATE, C.COUNTRY , ' +
                 'P.PICTURE_NAME, P.ID_PICTURE ' +
                 'FROM PICTURES P, CITY C ' +
                 'Where C.ID_CITY = P.ID_CITY ' +
                 ' Order By C.COUNTRY, C.STATE, C.CITY';
  SqlCity : String = 'SELECT ID_CITY, CITY, STATE, COUNTRY, '+
                 ' (COUNTRY || ''-'' || CITY || ''-'' || STATE) as DESCLIST ' +
                 'FROM CITY ' +
                 ' Order By COUNTRY, STATE, CITY';

var
  cmd : TAdoDbxCommand;
begin


  // TODO: Put user code to initialize the page here
  // Using AdoDbxClient
  if not IsPostBack then
  begin
    cmd := AdoDbxConnection.CreateCommand as TAdoDbxCommand;
    cmd.CommandText := SqlCity;
    lbCity.DataSource := cmd.ExecuteReader;
  end;

  DataBind;
  
end;
  
method Pictures.GridView1_DataBinding(sender: System.Object; e: System.EventArgs);
Const
  Sql : String = 'SELECT C.ID_CITY, C.CITY, C.STATE, C.COUNTRY , ' +
                 'P.PICTURE_NAME, P.ID_PICTURE ' +
                 'FROM PICTURES P, CITY C ' +
                 'Where C.ID_CITY = P.ID_CITY ' +
                 ' Order By C.COUNTRY, C.STATE, C.CITY';

  Key : Array[0..0] of String = ['ID_PICTURE'];
var
  sqladat : TAdoDbxDataAdapter;
  dt : System.Data.DataTable;
begin

  sqladat := new TAdoDbxDataAdapter(Sql, AdoDbxConnection);
  dt := new System.Data.DataTable;
  sqladat.Fill(dt);
  GridView1.DataSource := dt;
  
end;

method Pictures.GridView1_RowCreated(sender: System.Object; e: System.Web.UI.WebControls.GridViewRowEventArgs);
var
   drv : System.Data.DataRowView;
begin

  if (e.Row.RowType = DataControlRowType.DataRow) then
  begin
    drv := e.Row.DataItem as DataRowView;

    if Assigned(drv) then
    begin
      (e.Row.FindControl('lbDelete') as LinkButton ).CommandArgument := drv.Item['ID_PICTURE'].ToString;
      e.Row.Attributes.Add('onmouseover', 'this.className=''hightlighrow''');

      if (e.row.RowState = DataControlRowState.Normal ) then
         e.Row.Attributes.Add('onmouseout', 'this.className=''item''')
      else
         e.Row.Attributes.Add('onmouseout', 'this.className=''altitem''');
    end;
  end;
  
end;

method Pictures.GridView1_RowCommand(sender: System.Object; e: System.Web.UI.WebControls.GridViewCommandEventArgs);
Const
  sql : String = 'Delete From Pictures Where ID_PICTURE = {0}';
var
  cmddelete : TAdoDbxCommand;
  id : String;
begin
  id := e.CommandArgument.ToString;
  if ( e.CommandName = 'DeleteRow' ) and ( id <> '' )then
  begin

  // Using AdoDbxClient

    cmddelete := AdoDbxConnection.CreateCommand as TAdoDbxCommand;
    cmddelete.CommandText := System.String.Format(sql, [id]);
    cmddelete.ExecuteNonQuery;

    DataBind;

  end;
  
end;

method Pictures.GridView1_PageIndexChanged(sender: System.Object; e: System.EventArgs);
begin
end;

method Pictures.Button1_Click(sender: System.Object; e: System.EventArgs);
Const
  sql : String = 'Insert Into Pictures ( ID_PICTURE, ID_CITY, PICTURE_NAME, PATH) ' +
                           'Values ( {0}, {1}, ''{2}'', ''{3}'' )';
var
  cmdinsert : TAdoDbxCommand;
  cmd : TAdoDbxCommand;
  nextid : Integer;
begin

  // TODO: Put user code to initialize the page here
  // Using AdoDbxClient

  if fuUpload.HasFile then
  begin
    fuUpload.SaveAs(Server.MapPath('images/' + fuUpload.FileName));

    cmd := AdoDbxConnection.CreateCommand as TAdoDbxCommand;
    cmd.CommandText := 'Select Max(ID_PICTURE) From Pictures';
    nextid := (System.Convert.ToInt32(cmd.ExecuteScalar) + 1);

    cmdinsert := AdoDbxConnection.CreateCommand as TAdoDbxCommand;
    cmdinsert.CommandText := System.String.Format(sql, [nextid.ToString, lbCity.SelectedValue,
                                                        tbDescription.Text,
                                                        '~/images/' + fuUpload.FileName  ]);
    cmdinsert.ExecuteNonQuery;

    DataBind;
  end;
  
end;

method Pictures.GridView1_PageIndexChanging(sender: System.Object; e: System.Web.UI.WebControls.GridViewPageEventArgs);
begin
  gridView1.PageIndex := e.NewPageIndex;
  DataBind;  
  
end;

end.