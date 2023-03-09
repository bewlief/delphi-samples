{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Winapi.EdgeUtils;

interface

uses
  Winapi.Windows, Winapi.WebView2;

function CheckWebView2Loaded: Boolean;
function IsEdgeAvailable: Boolean;

// WebView2 loader DLL
function CreateCoreWebView2EnvironmentWithOptions(
  BrowserExecutableFolder, UserDataFolder: LPCWSTR; const EnvironmentOptions: ICoreWebView2EnvironmentOptions;
  const Environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;

function CreateCoreWebView2Environment(
  const Environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;

function GetCoreWebView2BrowserVersionString(BrowserExecutableFolder: LPCWSTR; var VersionInfo: LPWSTR): HRESULT; stdcall;

function CompareBrowserVersions(Version1, Version2: LPCWSTR; var AResult: Integer): HRESULT; stdcall;

implementation

uses
  System.SysUtils;

type
  TCreateCoreWebView2EnvironmentWithOptions = function(
    browserExecutableFolder, userDataFolder: LPCWSTR; const environmentOptions: ICoreWebView2EnvironmentOptions;
    const environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
  TCreateCoreWebView2Environment = function(
    const environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
  TGetCoreWebView2BrowserVersionInfo = function(browserExecutableFolder: LPCWSTR;
    var versionInfo: LPWSTR): HRESULT; stdcall;
  TCompareBrowserVersions = function (version1, version2: LPCWSTR; var result: Integer): HRESULT; stdcall;

var
  hWebView2: THandle;
  _CreateCoreWebView2EnvironmentWithOptions: TCreateCoreWebView2EnvironmentWithOptions;
  _CreateCoreWebView2Environment: TCreateCoreWebView2Environment;
  _GetCoreWebView2BrowserVersionString: TGetCoreWebView2BrowserVersionInfo;
  _CompareBrowserVersions: TCompareBrowserVersions;

function CheckWebView2Loaded: Boolean;
begin
  if hWebView2 = 0 then
  begin
    hWebView2 := LoadLibrary('WebView2Loader.dll');
    if hWebView2 = 0 then
      Exit(False);

    @_CreateCoreWebView2EnvironmentWithOptions := GetProcAddress(hWebView2, 'CreateCoreWebView2EnvironmentWithOptions');
    @_CreateCoreWebView2Environment := GetProcAddress(hWebView2, 'CreateCoreWebView2Environment');
    @_GetCoreWebView2BrowserVersionString := GetProcAddress(hWebView2, 'GetCoreWebView2BrowserVersionString');
    @_CompareBrowserVersions := GetProcAddress(hWebView2, 'CompareBrowserVersions');
  end;
  Result := True;
end;

function IsEdgeAvailable: Boolean;
begin
  Result := TOSVersion.Check(6, 1) and CheckWebView2Loaded;
end;

function CreateCoreWebView2EnvironmentWithOptions(
  BrowserExecutableFolder, UserDataFolder: LPCWSTR; const EnvironmentOptions: ICoreWebView2EnvironmentOptions;
  const Environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
begin
  if CheckWebView2Loaded then
    Result := _CreateCoreWebView2EnvironmentWithOptions(
      BrowserExecutableFolder, UserDataFolder, EnvironmentOptions, Environment_created_handler)
  else
    Result := E_FAIL;
end;

function CreateCoreWebView2Environment(
  const Environment_created_handler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
begin
  if CheckWebView2Loaded then
    Result := _CreateCoreWebView2Environment(Environment_created_handler)
  else
    Result := E_FAIL;
end;

function GetCoreWebView2BrowserVersionString(BrowserExecutableFolder: LPCWSTR; var VersionInfo: LPWSTR): HRESULT; stdcall;
begin
  if CheckWebView2Loaded then
    Result := _GetCoreWebView2BrowserVersionString(BrowserExecutableFolder, VersionInfo)
  else
    Result := E_FAIL;
end;

function CompareBrowserVersions(Version1, Version2: LPCWSTR; var AResult: Integer): HRESULT; stdcall;
begin
  if CheckWebView2Loaded then
    Result := _CompareBrowserVersions(Version1, Version2, AResult)
  else
    Result := E_FAIL;
end;

initialization
  FSetExceptMask(femALLEXCEPT);

finalization
  if hWebView2 <> 0 then
  begin
    FreeLibrary(hWebView2);
    hWebView2 := 0;
  end;

end.
