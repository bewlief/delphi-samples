<%@ Page Title="" Language="Oxygene" MasterPageFile="~/MasterPageSite.master" AutoEventWireup="true"
    CodeBehind="Detail.aspx.pas" Inherits="DelphiPrismWebApp.Detail" %>

<asp:Content ID="Content1" runat="server" ContentPlaceHolderID="ContentPlaceHolder1">
    &nbsp;
    <div class="centered">
        <table class="" cellspacing="0" cellpadding="5" border="0">
            <tr>
                <td class="">
                    <asp:Label ID="lbPicName" runat="server" SkinID="TitleSkin" Text="lbPicName"></asp:Label>
                </td>
            </tr>
            <tr>
                <td class="altitem" style="height: 23px">
                    City:
                    <asp:Label ID="lbCity" runat="server" Text="lbCity"></asp:Label>&nbsp;&nbsp;&nbsp;
                    State:&nbsp;
                    <asp:Label ID="lbState" runat="server" Text="lbState"></asp:Label>&nbsp; &nbsp;-
                    &nbsp;Country:
                    <asp:Label ID="lbCountry" runat="server" Text="lbCountry"></asp:Label>
                </td>
            </tr>
            <tr>
                <td>
                    <asp:Image ID="lbImage" runat="server"></asp:Image>
                </td>
            </tr>
        </table>
    </div>
</asp:Content>
