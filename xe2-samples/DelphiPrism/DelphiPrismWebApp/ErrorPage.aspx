<%@ Page Language="Oxygene" AutoEventWireup="true" CodeBehind="ErrorPage.aspx.pas"
    Inherits="DelphiPrismWebApp.ErrorPage" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>Untitled Page</title>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <p>
            <table class="cpq" width="100%">
                <tbody>
                    <tr>
                        <th class="titulo" align="left" colspan="3">
                            <h2>
                                <asp:Label ID="lbError" runat="server" Text="Error on Server"></asp:Label><br/>
                                <br/>
                                <asp:HyperLink ID="HyperLink1" runat="server" NavigateUrl="~/Default.aspx">HomePage
                                </asp:HyperLink>
                            </h2>
                        </th>
                        <p>
                        </p>
                    </tr>
                </tbody>
            </table>
            <br/>
        </p>
    </div>
    </form>
</body>
</html>
