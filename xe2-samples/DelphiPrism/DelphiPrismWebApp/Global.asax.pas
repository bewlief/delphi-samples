
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
  System.Collections,
  System.Web, System.Web.SessionState,
  System.Security,
  System.Security.Principal,
  System.Configuration,
  System.Web.Security,
  System.Diagnostics,
  System.Net.Mail;

type
   Global_asax = public class(System.Web.HttpApplication)
   protected
     beginrequesttime : DateTime;

     method Application_Start(sender: Object; e: EventArgs);
     method Session_Start(sender: Object; e: EventArgs);
     method Application_BeginRequest(sender: Object; e: EventArgs);
     method Application_EndRequest(sender: Object; e: EventArgs);
     method Application_AuthenticateRequest(sender: Object; e: EventArgs);
     method Application_Error(sender: Object; e: EventArgs);
     method Session_End(sender: System.Object; e: EventArgs);

   end;

implementation

method Global_asax.Application_Start(sender: Object; e: EventArgs);
var
  appCounter : TApplicationCounter;
begin

  // Create TApplicationCounter object and put in Application scope
  appCounter := TApplicationCounter.Create;
  Application.Add(AppCounterKey, appCounter);
  
end;

method Global_asax.Session_Start(sender: Object; e: EventArgs);
var
  sessionCounter : TSessionCounter;
begin

  // Create TSessionCounter object and put in Session scope
  sessionCounter := TSessionCounter.Create;
  Session.Add(SessionCounterKey, sessionCounter);

  Session[MyMasterPageKey] := 'MasterPageSite.master';

end;

method Global_asax.Application_BeginRequest(sender: Object; e: EventArgs);
begin

  beginrequesttime := DateTime.Now;

  try
    // blocks other clients from modifying the variables stored
    // in the Application object, ensuring that only one client at a time
    // can alter or access the Application variables
    Application.Lock;

    // exclude statistics.aspx from statistics
    if not Request.Url.AbsolutePath.Contains('statistics.aspx') then
    begin
      TApplicationCounter(Application.Item[AppCounterKey]).Inc;
      TApplicationCounter(Application.Item[AppCounterKey]).IncPages(Request.Url.AbsolutePath);
    end;

  finally
    Application.UnLock
  end;

end;

method Global_asax.Application_EndRequest(sender: Object; e: EventArgs);
const
  msg : string = '<span style="color:wHITE;font-family:Verdana;font-size:12px;font-weight:bold;">Elapsed request time (ms) = {0}</span><br/>';
var
  difftime : TimeSpan;
begin

  // calculate request time
  difftime := DateTime.Now - beginrequesttime;

  if ConfigurationManager.AppSettings.Get('TraceTimeRequest') = 'on' then
  begin
     Response.Write(System.String.Format(msg, [difftime.Milliseconds]));
  end;
end;

method Global_asax.Application_AuthenticateRequest(sender: Object; e: EventArgs);
var
  authCookie: HttpCookie;
  authTicket: System.Web.Security.FormsAuthenticationTicket;
  identity: FormsIdentity;
  principal: GenericPrincipal;
begin

  authCookie := Context.Request.Cookies[FormsAuthentication.FormsCookieName];

  // Default.aspx generate authentication cookie
  if Assigned(authCookie) then
  begin
    authTicket := FormsAuthentication.Decrypt(authCookie.Value);

    // TUser object generate ticket information
    if Assigned(authTicket) then
    begin
      identity := FormsIdentity.Create(authTicket);

      // Define new IPrincipal and user roles
      // You can use Roles to define access in web site
      principal := new GenericPrincipal(identity, new TUser(authTicket.Name).GetRoles);
      Context.User := principal;
    end;
  end;
end;

method Global_asax.Application_Error(sender: Object; e: EventArgs);
var
  objError: Exception;
  msgerror : String;
  mail : MailMessage;
  smtp : SmtpClient;
begin

  if ( ConfigurationManager.AppSettings.Get('SMTPServer').Trim <> '' ) and
     ( ConfigurationManager.AppSettings.Get('MailNofication').Trim <> '' ) then
  begin
    objError := Server.GetLastError.GetBaseException;

    msgerror := objError.Message +  ' - ' + objError.StackTrace;

    // Prepare email
    mail :=  MailMessage.Create;
    mail.&To.Add(ConfigurationManager.AppSettings.Get('MailNofication'));
    mail.Subject := 'Error in the Site';
    mail.Priority := MailPriority.High;
//    mail.BodyFormat := MailFormat.Text;
    mail.Body := msgerror;

    // Send email
    smtp := SmtpClient.Create;
    smtp.Host := ConfigurationManager.AppSettings.Get('SMTPServer');
    smtp.Send(mail);
  end;

end;

method Global_asax.Session_End(sender: System.Object; e: EventArgs);
begin
  // Decrease number of user connected in web site
  TApplicationCounter(Application.Item[AppCounterKey]).LogoffUser;
end;

end.
    