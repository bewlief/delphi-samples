<?xml version="1.0" encoding="utf-8"?>
<Project DefaultTargets="Build" xmlns="http://schemas.microsoft.com/developer/msbuild/2003" ToolsVersion="4.0">
  <PropertyGroup>
    <ProductVersion>3.5</ProductVersion>
    <ProjectTypeGuids>{349c5851-65df-11da-9384-00065b846f21};{656346D9-4656-40DA-A068-22D5425D4639}</ProjectTypeGuids>
    <OutputType>Library</OutputType>
    <RootNamespace>InterBaseASPNETApp</RootNamespace>
    <AssemblyName>InterBaseASPNETApp</AssemblyName>
    <Configuration Condition="'$(Configuration)' == ''">Release</Configuration>
    <TargetFrameworkVersion>v4.0</TargetFrameworkVersion>
    <Name>InterBaseASPNETApp</Name>
    <ProjectGuid>{23427f1f-7abf-4946-8aa7-93c5907d23cf}</ProjectGuid>
    <AllowLegacyOutParams>
    </AllowLegacyOutParams>
    <AllowLegacyCreate>
    </AllowLegacyCreate>
    <AllowGlobals>
    </AllowGlobals>
    <InternalAssemblyName />
    <StartupClass />
    <DefaultUses />
    <ApplicationIcon />
  </PropertyGroup>
  <PropertyGroup Condition=" '$(Configuration)' == 'Debug' ">
    <DefineConstants>DEBUG;TRACE;</DefineConstants>
    <OutputPath>./bin</OutputPath>
    <GeneratePDB>True</GeneratePDB>
    <GenerateMDB>True</GenerateMDB>
  </PropertyGroup>
  <PropertyGroup Condition=" '$(Configuration)' == 'Release' ">
    <OutputPath>./bin</OutputPath>
    <EnableAsserts>False</EnableAsserts>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="Borland.Data.AdoDbxClient">
      <HintPath>C:\Program Files (x86)\Common Files\CodeGear Shared\Delphi Prism\Shared Assemblies\8.0\Borland.Data.AdoDBXClient.dll</HintPath>
    </Reference>
    <Reference Include="Borland.Data.DbxClientDriver">
      <HintPath>C:\Program Files (x86)\Common Files\CodeGear Shared\Delphi Prism\Shared Assemblies\8.0\Borland.Data.DBXClientDriver.dll</HintPath>
    </Reference>
    <Reference Include="Borland.Data.DbxCommonDriver">
      <HintPath>C:\Program Files (x86)\Common Files\CodeGear Shared\Delphi Prism\Shared Assemblies\8.0\Borland.Data.DbxCommonDriver.dll</HintPath>
    </Reference>
    <Reference Include="Borland.Delphi">
      <HintPath>C:\Program Files (x86)\Common Files\CodeGear Shared\Delphi Prism\Shared Assemblies\8.0\Borland.Delphi.dll</HintPath>
    </Reference>
    <Reference Include="Borland.VclDbRtl">
      <HintPath>C:\Program Files (x86)\Common Files\CodeGear Shared\Delphi Prism\Shared Assemblies\8.0\Borland.VclDbRtl.dll</HintPath>
    </Reference>
    <Reference Include="Borland.VclRtl">
      <HintPath>C:\Program Files (x86)\Common Files\CodeGear Shared\Delphi Prism\Shared Assemblies\8.0\Borland.VclRtl.dll</HintPath>
    </Reference>
    <Reference Include="mscorlib">
      <HintPath>mscorlib.dll</HintPath>
    </Reference>
    <Reference Include="System" />
    <Reference Include="System.Configuration" />
    <Reference Include="System.Data" />
    <Reference Include="System.Drawing" />
    <Reference Include="System.EnterpriseServices" />
    <Reference Include="System.Web" />
    <Reference Include="System.Web.Extensions" />
    <Reference Include="System.Web.Mobile" />
    <Reference Include="System.Web.Services" />
    <Reference Include="System.Xml" />
    <Reference Include="System.Core">
      <RequiredTargetFramework>3.5</RequiredTargetFramework>
    </Reference>
    <Reference Include="System.Xml.Linq">
      <RequiredTargetFramework>3.5</RequiredTargetFramework>
    </Reference>
    <Reference Include="System.Data.DataSetExtensions">
      <RequiredTargetFramework>3.5</RequiredTargetFramework>
    </Reference>
  </ItemGroup>
  <ItemGroup>
    <Compile Include="Classes\ApplicationCounter.pas" />
    <Compile Include="Classes\PCConsts.pas" />
    <Compile Include="Classes\Counter.pas" />
    <Compile Include="Classes\BasePage.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DesignableClassName>DelphiPrismWebApp.TBasePage</DesignableClassName>
    </Compile>
    <Compile Include="Classes\DBConnection.pas" />
    <Compile Include="Classes\User.pas" />
    <Compile Include="Default.aspx.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Default.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp._Default1</DesignableClassName>
    </Compile>
    <Compile Include="Default.aspx.pas">
      <DependentUpon>Default.aspx</DependentUpon>
      <SubType>ASPXCodeBehind</SubType>
      <DesignableClassName>DelphiPrismWebApp._Default1</DesignableClassName>
    </Compile>
    <Compile Include="Detail.aspx.designer.pas">
      <DependentUpon>Detail.aspx</DependentUpon>
      <SubType>ASPXCodeBehind</SubType>
      <DesignableClassName>DelphiPrismWebApp.Detail</DesignableClassName>
    </Compile>
    <Compile Include="Detail.aspx.pas">
      <DependentUpon>Detail.aspx</DependentUpon>
      <SubType>ASPXCodeBehind</SubType>
      <DesignableClassName>DelphiPrismWebApp.Detail</DesignableClassName>
    </Compile>
    <Compile Include="ErrorPage.aspx.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>ErrorPage.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.ErrorPage</DesignableClassName>
    </Compile>
    <Compile Include="ErrorPage.aspx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>ErrorPage.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.ErrorPage</DesignableClassName>
    </Compile>
    <Compile Include="Header.ascx.Designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Header.ascx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Header</DesignableClassName>
    </Compile>
    <Compile Include="Header.ascx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Header.ascx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Header</DesignableClassName>
    </Compile>
    <Compile Include="Logoff.aspx.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Logoff.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Logoff</DesignableClassName>
    </Compile>
    <Compile Include="Logoff.aspx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Logoff.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Logoff</DesignableClassName>
    </Compile>
    <Compile Include="MasterPageMenu.Master.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>MasterPageMenu.Master</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.MasterPageMenu</DesignableClassName>
    </Compile>
    <Compile Include="MasterPageMenu.Master.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>MasterPageMenu.Master</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.MasterPageMenu</DesignableClassName>
    </Compile>
    <Compile Include="MasterPageSite.master.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>MasterPageSite.master</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.MasterPageSite</DesignableClassName>
    </Compile>
    <Compile Include="MasterPageSite.master.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>MasterPageSite.master</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.MasterPageSite</DesignableClassName>
    </Compile>
    <Compile Include="Pictures.aspx.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Pictures.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Pictures</DesignableClassName>
    </Compile>
    <Compile Include="Pictures.aspx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Pictures.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Pictures</DesignableClassName>
    </Compile>
    <Compile Include="properties\Resources.Designer.pas" />
    <Compile Include="properties\Settings.Designer.pas" />
    <Compile Include="Statistics.aspx.designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Statistics.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Statistics</DesignableClassName>
    </Compile>
    <Compile Include="Statistics.aspx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>Statistics.aspx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.Statistics</DesignableClassName>
    </Compile>
    <Compile Include="ucStatistics.ascx.Designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>ucStatistics.ascx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.ucStatistics</DesignableClassName>
    </Compile>
    <Compile Include="ucStatistics.ascx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>ucStatistics.ascx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.ucStatistics</DesignableClassName>
    </Compile>
    <Compile Include="ucThemeMasterPage.ascx.Designer.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>ucThemeMasterPage.ascx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.ucThemeMasterPage</DesignableClassName>
    </Compile>
    <Compile Include="ucThemeMasterPage.ascx.pas">
      <SubType>ASPXCodeBehind</SubType>
      <DependentUpon>ucThemeMasterPage.ascx</DependentUpon>
      <DesignableClassName>DelphiPrismWebApp.ucThemeMasterPage</DesignableClassName>
    </Compile>
    <Content Include="App_Themes\Summer\SkinFile.skin">
      <SubType>Content</SubType>
    </Content>
    <Content Include="App_Themes\Summer\StyleSheet.css">
      <SubType>Content</SubType>
    </Content>
    <Content Include="App_Themes\Winter\SkinFile.skin">
      <SubType>Content</SubType>
    </Content>
    <Content Include="App_Themes\Winter\StyleSheet.css">
      <SubType>Content</SubType>
    </Content>
    <Content Include="dbxconnections.ini">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Default.aspx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Detail.aspx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="ErrorPage.aspx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Global.asax" />
    <Compile Include="Global.asax.pas">
      <DependentUpon>Global.asax</DependentUpon>
    </Compile>
    <Content Include="Header.ascx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\Alcatraz1.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\Alcatraz2.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\BocachicaBeach.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\DelphiPrism.png">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\elephant.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\Floripa1.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\Floripa2.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\Floripa3.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\foto.JPG">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\gorila.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\n601942333_1068549_1179.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\RD2.JPG">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\SanConradoBeach.JPG">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\SanFrancisco1.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\SanFrancisco2.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\SantaCruz1.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\SantaCruz2.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\SantaCruz3.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\sppenguin.JPG">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\spturtle.jpg">
      <SubType>Content</SubType>
    </Content>
    <Content Include="images\StreetBeach.JPG">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Logoff.aspx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="MasterPageMenu.Master">
      <SubType>Content</SubType>
    </Content>
    <Content Include="MasterPageSite.master">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Pictures.aspx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="properties\Settings.settings">
      <SubType>Content</SubType>
      <Generator>SettingsSingleFileGenerator</Generator>
    </Content>
    <Content Include="Readme.html">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Statistics.aspx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="ucStatistics.ascx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="ucThemeMasterPage.ascx">
      <SubType>Content</SubType>
    </Content>
    <Content Include="Web.config" />
    <Content Include="Web.sitemap">
      <SubType>Content</SubType>
    </Content>
  </ItemGroup>
  <ItemGroup>
    <Folder Include="App_Themes\" />
    <Folder Include="App_Themes\Winter" />
    <Folder Include="App_Themes\Summer" />
    <Folder Include="Classes" />
    <Folder Include="images" />
    <Folder Include="Properties\" />
  </ItemGroup>
  <ItemGroup>
    <EmbeddedResource Include="properties\Resources.resx" />
  </ItemGroup>
  <ProjectExtensions>
    <VisualStudio>
      <FlavorProperties Guid="{349c5851-65df-11da-9384-00065b846f21}">
        <WebProjectProperties>
          <UseIIS>False</UseIIS>
          <AutoAssignPort>True</AutoAssignPort>
          <DevelopmentServerPort>10111</DevelopmentServerPort>
          <DevelopmentServerVPath>/</DevelopmentServerVPath>
          <IISUrl>
          </IISUrl>
          <NTLMAuthentication>False</NTLMAuthentication>
          <UseCustomServer>False</UseCustomServer>
          <CustomServerUrl>
          </CustomServerUrl>
          <SaveServerSettingsInUserFile>False</SaveServerSettingsInUserFile>
        </WebProjectProperties>
      </FlavorProperties>
      <FlavorProperties Guid="{349c5851-65df-11da-9384-00065b846f21}" User="">
        <WebProjectProperties>
          <StartPageUrl>Default.aspx</StartPageUrl>
          <StartAction>SpecificPage</StartAction>
          <AspNetDebugging>True</AspNetDebugging>
          <SilverlightDebugging>False</SilverlightDebugging>
          <NativeDebugging>False</NativeDebugging>
          <SQLDebugging>False</SQLDebugging>
          <ExternalProgram>
          </ExternalProgram>
          <StartExternalURL>
          </StartExternalURL>
          <StartCmdLineArguments>
          </StartCmdLineArguments>
          <StartWorkingDirectory>
          </StartWorkingDirectory>
          <EnableENC>False</EnableENC>
          <AlwaysStartWebServerOnDebug>True</AlwaysStartWebServerOnDebug>
        </WebProjectProperties>
      </FlavorProperties>
    </VisualStudio>
  </ProjectExtensions>
  <Import Project="$(MSBuildExtensionsPath)\RemObjects Software\Oxygene\RemObjects.Oxygene.targets" />
  <Import Project="$(MSBuildExtensionsPath)\Microsoft\VisualStudio\v10.0\WebApplications\Microsoft.WebApplication.targets" />
</Project>