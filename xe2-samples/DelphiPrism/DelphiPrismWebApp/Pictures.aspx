<%@ Page Title="" Language="Oxygene" MasterPageFile="~/MasterPageSite.master" AutoEventWireup="true" CodeBehind="Pictures.aspx.pas" Inherits="DelphiPrismWebApp.Pictures" %>

<asp:Content id="Content1" runat="server" contentplaceholderid="ContentPlaceHolder1">&nbsp; 

  <table cellspacing="0" cellpadding="10" width="100%" border="0" 
    style="background-image: inherit">
      <tr>
        <td align="center">
          <asp:UpdatePanel id="UpdatePanel1" runat="server">
            <ContentTemplate>
              <asp:GridView id="GridView1" runat="server" allowpaging="True" 
                    autogeneratecolumns="False" cellpadding="5" 
                    ondatabinding="GridView1_DataBinding" 
                    onpageindexchanged="GridView1_PageIndexChanged" 
                    onpageindexchanging="GridView1_PageIndexChanging" 
                    onrowcommand="GridView1_RowCommand" onrowcreated="GridView1_RowCreated">
                <FooterStyle cssclass="footer"></FooterStyle>
                <EmptyDataRowStyle cssclass="item"></EmptyDataRowStyle>
                <Columns>
                  <asp:BoundField datafield="COUNTRY" sortexpression="COUNTRY" headertext="Country"></asp:BoundField>
                  <asp:BoundField datafield="STATE" sortexpression="STATE" headertext="State"></asp:BoundField>
                  <asp:BoundField datafield="CITY" sortexpression="CITY" headertext="Name of City"></asp:BoundField>
                  <asp:BoundField datafield="PICTURE_NAME" sortexpression="PICTURE_NAME" headertext="Photo Description"></asp:BoundField>
                  <asp:HyperLinkField headertext="Pictures" text="Show" datanavigateurlformatstring="Detail.aspx?id={0}" datanavigateurlfields="ID_PICTURE"></asp:HyperLinkField>
                  <asp:TemplateField insertvisible="False" showheader="False">
                    <ItemTemplate>
                      <asp:LinkButton id="lbDelete" runat="server" text="Delete" causesvalidation="False" commandname="DeleteRow"></asp:LinkButton>
                    </ItemTemplate>
                  </asp:TemplateField>
                </Columns>
                <RowStyle cssclass="item"></RowStyle>
                <HeaderStyle cssclass="header"></HeaderStyle>
                <AlternatingRowStyle cssclass="altitem"></AlternatingRowStyle>
              </asp:GridView>
            </ContentTemplate>
          </asp:UpdatePanel></td>
        <td valign="top">
          <table class="" cellspacing="0" cellpadding="5" border="0">
              <tr>
                <td class="" style="HEIGHT: 26px" align="center" colspan="2">
                  <asp:Label id="Label1" runat="server" text="Upload new picture"></asp:Label></td>
              </tr>
              <tr>
                <td class="item">Country / City / State</td>
                <td>
                  <asp:ListBox id="lbCity" runat="server" rows="1" datatextfield="DESCLIST" datavaluefield="ID_CITY"></asp:ListBox></td>
              </tr>
              <tr>
                <td class="item">Photo Description</td>
                <td>
                  <asp:TextBox id="tbDescription" runat="server"></asp:TextBox>
                  <asp:RequiredFieldValidator id="RequiredFieldValidator1" runat="server" controltovalidate="tbDescription" errormessage="*"></asp:RequiredFieldValidator></td>
              </tr>
              <tr>
                <td class="item">File</td>
                <td>
                  <asp:FileUpload id="fuUpload" runat="server"></asp:FileUpload>
                  <asp:RequiredFieldValidator id="RequiredFieldValidator2" runat="server" controltovalidate="fuUpload" errormessage="*"></asp:RequiredFieldValidator></td>
              </tr>
              <tr>
                <td class="item"></td>
                <td>
                  <asp:Button id="Button1" runat="server" text="New Picture..." borderstyle="None" 
                        onclick="Button1_Click"></asp:Button></td>
              </tr>
              <tr>
                <td></td>
                <td></td>
              </tr>
          </table></td>
      </tr>
  </table>
 </asp:Content>
