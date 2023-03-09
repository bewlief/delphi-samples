
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
  System.Data.Common,
  System.Collections.Generic,
  System.Web,
  System.Web.UI,
  System.Web.UI.WebControls,
  Borland.Data;

type
  Detail = public partial class(TBasePage)
  protected
    method Page_Load(sender : object; e : EventArgs);
  end;

implementation
  
method Detail.Page_Load(sender : object; e : EventArgs);
var
  cmd : System.Data.Common.DbCommand;
  dr : TAdoDbxDataReader;
  param : String;
begin

  param := Request.QueryString.Item['id'];
  // TODO: Put user code to initialize the page here
  if param = nil then
     Response.Redirect('~/Pictures.aspx')
  else
  begin
    param := Request.QueryString.Item['id'];

    cmd := AdoDbxConnection.CreateCommand() as TAdoDbxCommand;
    cmd.CommandText := 'SELECT C.ID_CITY, C.CITY, C.STATE, C.COUNTRY , P.PICTURE_NAME, P.ID_PICTURE ' +
                       ',P.PATH FROM PICTURES P, CITY C ' +
                       'Where C.ID_CITY = P.ID_CITY AND P.ID_PICTURE = ' + param;

    try
      dr := cmd.ExecuteReader as TAdoDbxDataReader;

      if dr.Read then
      begin

        lbPicName.Text := dr.GetString(dr.GetOrdinal('PICTURE_NAME'));
        lbState.Text := dr.GetString(dr.GetOrdinal('STATE'));
        lbCountry.Text := dr.GetString(dr.GetOrdinal('COUNTRY'));
        lbCity.Text := dr.GetString(dr.GetOrdinal('CITY'));
        lbImage.ImageUrl := dr.GetString(dr.GetOrdinal('PATH'));

      end;
      dr.Close;
      DataBind;
    finally
      AdoDbxConnection.Close;
    end;
  end;
  
end;
  
end.