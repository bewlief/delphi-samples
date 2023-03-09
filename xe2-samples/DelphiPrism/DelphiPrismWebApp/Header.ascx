<%@ Control Language="Oxygene" AutoEventWireup="true" CodeBehind="Header.ascx.pas" Inherits="DelphiPrismWebApp.Header" %>

<script language="JavaScript">

    var popUpWin = 0;
    function popUpWindow(URLStr, left, top, width, height) {
        if (popUpWin) {
            if (!popUpWin.closed)
                popUpWin.close();
        }
        popUpWin = open(URLStr, 'popUpWin', 'toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbar=yes,resizable=yes,copyhistory=yes,width=' + width + ',height=' + height + ',left=' + left + ', top=' + top + ',screenX=' + left + ',screenY=' + top + '');
    }


</script>
<table class="tableheader" cellspacing="0" cellpadding="10" width="100%" border="0">
  <tbody>
    <tr>
      <td align="middle" colspan="2">
        <asp:Label id="lbHeader" cssclass="header" runat="server">Pictures from the World</asp:Label></td>
      <td align="right" rowspan="3"><img id="IMG1"
          hspace="0" src="~/images/powerbydelphismall.JPG" border="0"
          name="IMG1" runat="Server"></td>
    </tr>
    <tr>
      <td align="right" colspan="2">
        <asp:Label id="lbAppCounter" cssclass="label" runat="server"></asp:Label></td>
    </tr>
    <tr>
      <td>
        <asp:HyperLink id="hlHome" cssclass="link" runat="server"
          navigateurl="~/Pictures.aspx">Home Page</asp:HyperLink><br/>
        <asp:HyperLink id="hlStatistics" cssclass="link" runat="server"
          navigateurl="JavaScript:popUpWindow('statistics.aspx', 0, 0, 400, 300)">Statistics</asp:HyperLink><br/>
        <asp:HyperLink id="hlLogoff" cssclass="link" runat="server"
          navigateurl="~/Logoff.aspx">Logoff</asp:HyperLink></td>
      <td align="right">
        <asp:Label id="lbPageCounter" cssclass="label" runat="server"></asp:Label><br/>
        <asp:Label id="lbUserOn" cssclass="label" runat="server"></asp:Label></td>
    </tr>
  </tbody>
</table>
