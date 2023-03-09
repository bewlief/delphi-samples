
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

uses System.Data,System.Collections,
     Borland.Data.*,
     System.Web,
     System.Web.Security;

type
  StringArray = array of String;
  TUser = class
  private
    FLogin: String;
    FisValid : Boolean;
    FTheme: String;
    FConnection : TAdoDbxConnection;

    procedure Validate(sPassword : String );
    function GetConnection : TAdoDbxConnection;
  public
    constructor Create( sLogin : String );
    constructor Create( sLogin, sPassword, sTheme : String );

    procedure GenerateTicket;
    procedure set_Theme(const Value: String);
    property Login : String read FLogin;
    property isValid : boolean read FisValid;
    property Theme : String read FTheme write set_Theme;
    function GetRoles : StringArray;
  end;

implementation
  

constructor TUser.Create(sLogin, sPassword, sTheme : String);
begin
  inherited Create;
  // TODO: Add any constructor code here
  FLogin := sLogin;
  FisValid := False;
  FTheme := sTheme;

  Validate(sPassword);

  if isValid then
     GetRoles;

end;

constructor TUser.Create(sLogin: String);
begin
  inherited Create;
  FLogin := sLogin;
end;

procedure TUser.GenerateTicket;
var
  authTicket: FormsAuthenticationTicket;
  authCookie: HttpCookie;
  encryptedTicket: string;
begin
    authTicket := FormsAuthenticationTicket.Create(
      1,                            // version
      FLogin,      // name
      DateTime.Now,                 // creation
      DateTime.Now.AddMinutes(HttpContext.Current.Session.Timeout),  // expiration
      False,                        // persistent,
      FLogin,                 // user data
      FormsAuthentication.FormsCookiePath);


    encryptedTicket := FormsAuthentication.Encrypt(authTicket);
    authCookie := HttpCookie.Create(FormsAuthentication.FormsCookieName, encryptedTicket);
    HttpContext.Current.Response.Cookies.Add(authCookie);

end;

function TUser.GetConnection: TAdoDbxConnection;
begin
  if not Assigned(FConnection) then
     FConnection := TDBConnection.Create.MyAdoDbxConnection;

  Result := FConnection;
end;

function TUser.GetRoles : StringArray;
Const
  sql : String = 'SELECT ROLES.ROLE_NAME ' +
                 'FROM ROLES ' +
                 'INNER JOIN USER_ROLE ON (ROLES.ID_ROLE = USER_ROLE.ID_ROLE) ' +
                 'INNER JOIN USERS ON (USER_ROLE.ID_USER = USERS.ID_USER) ' +
                 'WHERE USERS.USER_LOGIN = ''{0}''';
var
  roles: ArrayList;
  i:integer;
  dr: TAdoDBXDataReader;
  cmdRole : System.Data.Common.DbCommand;
begin

  roles := ArrayList.Create;
  cmdRole := GetConnection.CreateCommand as TAdoDbxCommand;
  try

    cmdRole.CommandText := System.&String.Format(sql, [FLogin]);
    dr := cmdRole.ExecuteReader as TAdoDbxDataReader;

    { Tratamento específico para ArrayList }
    while dr.Read do
      roles.Add( dr.GetString(0));

    dr.Close;
  finally
     GetConnection.Close;
  end;

  result := new StringArray(roles.Count);
  //new Array of String;
  // review
  // SetLength(result, roles.Count);
  for i := 0 to roles.Count-1 do
  begin
    result[i] := roles[i].ToString;
  end;

end;

procedure TUser.set_Theme(const Value: String);
begin
  FTheme := Value;
end;

procedure TUser.Validate(sPassword : String );
Const
 sql : String = 'SELECT ID_USER, NAME, USER_LOGIN, USER_PASSWORD ' +
            'FROM USERS WHERE USER_LOGIN = ''{0}'' and USER_PASSWORD = ''{1}''';
var
  dr: TAdoDbxDataReader;
  cmdUser : TAdoDbxCommand;
begin


  FisValid := False;
  cmdUser := GetConnection.CreateCommand() as TAdoDbxCommand;
  try
    cmdUser.CommandText := System.&String.Format(sql, [FLogin, sPassword]);
    cmdUser.Prepare;

    dr := CmdUser.ExecuteReader as TAdoDbxDataReader;

    if Assigned( dr ) then
    begin
      if dr.Read then
      begin
        GenerateTicket;
        FisValid := True;
      end;
    end;
    dr.Close;
  finally
    GetConnection.Close;
  end;
end;


end.