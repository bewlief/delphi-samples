<%@ Page Title="" Language="Oxygene" MasterPageFile="~/MasterPageSite.master" AutoEventWireup="true" CodeBehind="Default.aspx.pas" Inherits="DelphiPrismWebApp._Default1" %>

	<%@ Register TagPrefix="asp" Namespace="System.Web.UI" Assembly="System.Web.Extensions, Version=1.0.61025.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35" %>
<asp:Content id="Content1" runat="server" contentplaceholderid="ContentPlaceHolder1">&nbsp; <br/>
  <asp:UpdatePanel id="UpdatePanel1" runat="server">
    <ContentTemplate>
    <div class="centered">
      <table style="margin-right: auto; margin-left: auto;" cellspacing="0" cellpadding="8" width="500"  border="0" >
          <tr>
            <td>
              <asp:Label id="Label2" runat="server">Login </asp:Label></td>
            <td>
              <asp:TextBox id="tbUser" runat="server"></asp:TextBox>
              <asp:RequiredFieldValidator id="RequiredFieldValidator2" runat="server" font-size="X-Large" controltovalidate="tbUser" errormessage="*"></asp:RequiredFieldValidator></td>
          </tr>
          <tr>
            <td>
              <asp:Label id="Label4" runat="server">Password </asp:Label></td>
            <td>
              <asp:TextBox id="tbPassword" runat="server" textmode="Password"></asp:TextBox>
              <asp:RequiredFieldValidator id="RequiredFieldValidator3" runat="server" font-size="X-Large" controltovalidate="tbPassword" errormessage="*"></asp:RequiredFieldValidator></td>
          </tr>
          <tr>
            <td>
              <asp:Label id="Label3" runat="server">Theme</asp:Label></td>
            <td>
              <asp:ListBox id="lbTheme" runat="server" rows="1">
                <asp:ListItem>Winter</asp:ListItem>
                <asp:ListItem selected="True">Summer</asp:ListItem>
              </asp:ListBox></td>
          </tr>
          <tr>
            <td style="HEIGHT: 46px">
              <asp:Button id="Button2" runat="server" font-size="Medium" text="Login >>" 
                    onclick="Button2_Click"></asp:Button></td>
            <td style="HEIGHT: 46px">
              <asp:Label id="LBWarning" runat="server" skinid="ErrorMessageSkin" forecolor="Red" visible="False">User
            and Password invalid!!! </asp:Label><br/>
              <br/>
              <asp:Label id="Label1" runat="server" text="Use login: embt and password: delphi   "></asp:Label></td>
          </tr>
      </table>
      </div>
    </ContentTemplate>
  </asp:UpdatePanel>
</asp:Content>
