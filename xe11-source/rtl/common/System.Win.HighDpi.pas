{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Win.HighDpi;

interface

function IsDpiAware: Boolean;
function SetHighDpiAware: Boolean;

implementation

uses
  Winapi.Windows, Winapi.ShellScaling, System.SysUtils;

function SetHighDpiAware: Boolean;
begin
  if CheckWin32Version(10, 0) and (TOSversion.Build >= 15063) then // Windows 10 1703 has 15063 as the Build number
    Result := SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2)
  else
  if CheckWin32Version(6, 3) then
    Result := SetProcessDpiAwareness(TProcessDpiAwareness.PROCESS_PER_MONITOR_DPI_AWARE) = S_OK
  else
    Result := SetProcessDpiAware;
end;

function IsDpiAware: Boolean;
var
  LLevel: TProcessDpiAwareness;
begin
  // GetProcessDpiAwareness works for Windows 10 also
  // This function returns true if PerMonitor V2 is used
  if CheckWin32Version(6, 3) then
  begin
    GetProcessDpiAwareness(GetCurrentProcess, LLevel);
    Result := LLevel = TProcessDpiAwareness.PROCESS_PER_MONITOR_DPI_AWARE;
  end
  else
{$WARN SYMBOL_DEPRECATED OFF}
    Result := IsProcessDPIAware;
{$WARN SYMBOL_DEPRECATED DEFAULT}
end;

end.
