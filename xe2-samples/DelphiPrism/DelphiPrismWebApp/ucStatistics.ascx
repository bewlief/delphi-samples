<%@ Control Language="Oxygene" AutoEventWireup="true" CodeBehind="ucStatistics.ascx.pas" Inherits="DelphiPrismWebApp.ucStatistics" %>
    <table cellspacing="0" cellpadding="4" width="100%" border="0">
        <tr>
          <td align="right">
            <asp:Label id="lbAppCounter" runat="server" skinid="StatisticSkin"></asp:Label></td>
        </tr>
        <tr>
          <td align="right">
            <asp:Label id="lbPageCounter" runat="server" skinid="StatisticSkin"></asp:Label>&nbsp;&nbsp;&nbsp; 
            <asp:Label id="lbUserOn" runat="server" skinid="StatisticSkin"></asp:Label></td>
        </tr>
    </table>
 