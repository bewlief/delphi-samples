<%@ Control Language="Oxygene" AutoEventWireup="true" CodeBehind="ucThemeMasterPage.ascx.pas" Inherits="DelphiPrismWebApp.ucThemeMasterPage" %>
                <asp:ListBox id="lbTheme" runat="server" rows="1" 
    autopostback="True" onselectedindexchanged="lbTheme_SelectedIndexChanged">
  <asp:ListItem value="Winter">Theme - Winter</asp:ListItem>
  <asp:ListItem value="Summer">Theme - Summer</asp:ListItem>
                </asp:ListBox>
&nbsp;
                <asp:ListBox id="lbMasterPage" runat="server" rows="1" 
    autopostback="True" onselectedindexchanged="lbMasterPage_SelectedIndexChanged">
  <asp:ListItem selected="True" value="MasterPageSite.master">MasterPage - Default</asp:ListItem>
  <asp:ListItem value="MasterPageMenu.master">MasterPage - Menu</asp:ListItem>
                </asp:ListBox>
