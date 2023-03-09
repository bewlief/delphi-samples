<%@ Page Language="Oxygene" AutoEventWireup="true" CodeBehind="Statistics.aspx.pas" Inherits="DelphiPrismWebApp.Statistics" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="serveR">
    <title>Statistics</title>

  </head>
  <body style="text-align: center">
    <form id="Form1" runat="server">
      <p>
          <asp:GridView ID="dgCounter" runat="server" AutoGenerateColumns="False" 
              CellPadding="4" ForeColor="#333333" GridLines="None" 
              onpageindexchanging="GridView1_PageIndexChanging" 
              onrowcreated="dgCounter_RowCreated" style="text-align: center" 
              ShowFooter="True">
              <AlternatingRowStyle BackColor="White" />
              <Columns>
                  <asp:TemplateField HeaderText="Page">
                      <ItemTemplate>
                          <asp:Label ID="lbPage" runat="server" Text="lbPage"></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="Total">
                      <FooterTemplate>
                          <asp:Label ID="lbVisit" runat="server" Text="lbVisit"></asp:Label>
                      </FooterTemplate>
                      <ItemTemplate>
                          <asp:Label ID="lbTotal" runat="server" Text="lbTotal"></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
              </Columns>
              <FooterStyle BackColor="#990000" Font-Bold="True" ForeColor="White" />
              <HeaderStyle BackColor="#990000" Font-Bold="True" ForeColor="White" />
              <PagerStyle BackColor="#FFCC66" ForeColor="#333333" HorizontalAlign="Center" />
              <RowStyle BackColor="#FFFBD6" ForeColor="#333333" />
              <SelectedRowStyle BackColor="#FFCC66" Font-Bold="True" ForeColor="Navy" />
              <SortedAscendingCellStyle BackColor="#FDF5AC" />
              <SortedAscendingHeaderStyle BackColor="#4D0000" />
              <SortedDescendingCellStyle BackColor="#FCF6C0" />
              <SortedDescendingHeaderStyle BackColor="#820000" />
          </asp:GridView>
      </p>
    </form>
  </body>
</html>