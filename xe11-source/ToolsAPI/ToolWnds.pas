{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit ToolWnds;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DesignWindows, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.ComCtrls, System.IniFiles, Vcl.ActnList, Vcl.Menus, Vcl.ActnPopup, System.Actions,
  Vcl.PlatformDefaultStyleActnCtrls;

type
  TToolbarDesignWindow = class(TDesignWindow)
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    PopupMenu1: TPopupActionBar;
    ActionList1: TActionList;
    ToolbarCmd: TAction;
    TextLabelsCmd: TAction;
    Toolbar2: TMenuItem;
    PopupMenu2: TPopupActionBar;
    TextLabels1: TMenuItem;
    HelpCmd: TAction;
    procedure FormCreate(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ToolbarCmdExecute(Sender: TObject);
    procedure TextLabelsCmdExecute(Sender: TObject);
    procedure ToolbarCmdUpdate(Sender: TObject);
    procedure TextLabelsCmdUpdate(Sender: TObject);
    procedure HelpCmdExecute(Sender: TObject);
    procedure HelpCmdUpdate(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FLargeButtons: Boolean;
    procedure SetLargeButtons(Value: Boolean);
    function GetToolbarVisible: boolean;
    procedure SetToolbarVisible(const Value: boolean);
    procedure UpdateToolbarVisible(Visible: boolean);
  protected
    procedure ResizeButtons(Large: Boolean); virtual;
  public
    property ToolbarVisible: boolean read GetToolbarVisible write SetToolbarVisible;
    property LargeButtons: Boolean read FLargeButtons write SetLargeButtons;
  end;

implementation

uses
  BrandingAPI, IDETheme.Utils;

{$R *.dfm}

{
  Simple Intl fix to display longer captions on localized versions of the
  product.        
  InitFromResources is called in initialization section.
  See bug #105175
}

resourcestring
  sSmallToolbarSize = '32';
  sSmallButtonHeight = '24';
  sSmallButtonWidth = '24';
  sLargeToolbarSize = '44';
  sLargeButtonHeight = '36';
  sLargeButtonWidth = '56';

var
  SmallToolbarSize: Integer;
  SmallButtonHeight: Integer;
  SmallButtonWidth: Integer;
  LargeToolbarSize: Integer;
  LargeButtonHeight: Integer;
  LargeButtonWidth: Integer;

procedure InitFromResources;
begin
  SmallToolbarSize := StrToIntDef(sSmallToolbarSize, 32);
  SmallButtonHeight := StrToIntDef(sSmallButtonHeight, 24);
  SmallButtonWidth := StrToIntDef(sSmallButtonWidth, 24);
  LargeToolbarSize := StrToIntDef(sLargeToolbarSize, 44);
  LargeButtonHeight := StrToIntDef(sLargeButtonHeight, 36);
  LargeButtonWidth := StrToIntDef(sLargeButtonWidth, 56);
end;

procedure TToolbarDesignWindow.FormCreate(Sender: TObject);
begin
  { Toggle to force update }
  ResizeButtons(False);
  SmallToolbarSize := ConvertToDefaultPPI(Self, ToolBar1.Height);
  ResizeButtons(True);
  LargeToolbarSize := ConvertToDefaultPPI(Self, ToolBar1.Height);
  ResizeButtons(FLargeButtons);
  UpdateToolbarVisible(True);
  if TIDEThemeMetrics.Font.Enabled then
  begin
    Font.Assign(TIDEThemeMetrics.Font.GetFont());
    TIDEThemeMetrics.Font.AdjustDPISize(Font, TIDEThemeMetrics.Font.DefaultSize, FCurrentPPI);
  end;
end;

procedure TToolbarDesignWindow.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  if TIDEThemeMetrics.Font.Enabled then
    TIDEThemeMetrics.Font.AdjustDPISize(Font, TIDEThemeMetrics.Font.DefaultSize, FCurrentPPI);
end;

procedure TToolbarDesignWindow.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  with Toolbar1 do
    if (Height >= LargeToolbarSize) then
      if (NewSize <= SmallToolbarSize) then
        NewSize := SmallToolbarSize
      else
        NewSize := LargeToolbarSize
    else
      if(NewSize >= LargeToolbarSize) then
        NewSize := LargeToolbarSize
      else
        NewSize := SmallToolbarSize;
end;

procedure TToolbarDesignWindow.Splitter1Moved(Sender: TObject);
begin
  LargeButtons := (ToolBar1.Height >= LargeToolbarSize);
end;

function TToolbarDesignWindow.GetToolbarVisible: boolean;
begin
  Result := ToolBar1.Visible;
end;

procedure TToolbarDesignWindow.HelpCmdExecute(Sender: TObject);
begin
  if (HelpType = htContext) and (HelpContext <> 0) then
    Application.HelpContext(HelpContext)
  else if (HelpKeyword <> '') then
    Application.HelpKeyword(HelpKeyword);
end;

procedure TToolbarDesignWindow.HelpCmdUpdate(Sender: TObject);
begin
  if Sender is TAction then
  begin
    TAction(Sender).Visible := (((HelpType = htContext) and (HelpContext <> 0)) or (HelpKeyword <> ''));
  end;
end;

procedure TToolbarDesignWindow.SetToolbarVisible(const Value: boolean);
begin
  if Value <> ToolbarVisible  then
    UpdateToolbarVisible(Value);
end;

procedure TToolbarDesignWindow.UpdateToolbarVisible(Visible: boolean);
begin
  DisableAlign;
  try
    ToolBar1.Visible := Visible;
    Splitter1.Top := Toolbar1.Top + Toolbar1.Height;
    Splitter1.Visible := Visible;
  finally
    EnableAlign;
  end;
end;

procedure TToolbarDesignWindow.ToolbarCmdExecute(Sender: TObject);
begin
  ToolbarVisible := not ToolbarVisible;
end;

procedure TToolbarDesignWindow.ToolbarCmdUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := ToolbarVisible;
end;


procedure TToolbarDesignWindow.SetLargeButtons(Value: Boolean);
begin
  if Value <> FLargeButtons then
  begin
    ResizeButtons(Value);
    FLargeButtons := Value;
  end;
end;

procedure TToolbarDesignWindow.ResizeButtons(Large: Boolean);
var
  NewLargeWidth, NewLargeHeight: Integer;
begin
    with ToolBar1 do
    begin
      ToolBar1.LockDrawing;
      try
        if Large then
        begin
          NewLargeWidth := ScaleValue(LargeButtonWidth);
          NewLargeHeight := ScaleValue(LargeButtonHeight);
          { Large buttons }
          ShowCaptions := True;
          ButtonWidth := NewLargeWidth;
          ButtonHeight := NewLargeHeight;
          Height := ButtonHeight + ScaleValue(TIDEThemeMetrics.AlignFactor * 2);//!LargeToolbarSize;
          ShowHint := False;
        end
        else
        begin
          { Small buttons }
          ShowCaptions := False;
          ButtonWidth := ScaleValue(SmallButtonWidth);
          ButtonHeight := ScaleValue(SmallButtonHeight);
          Height := ScaleValue(SmallToolbarSize);
          ShowHint := True;
        end;
      finally
        ToolBar1.UnlockDrawing;
        Invalidate;
      end;
    end;
end;

procedure TToolbarDesignWindow.TextLabelsCmdExecute(Sender: TObject);
begin
  LargeButtons := not LargeButtons;
end;

procedure TToolbarDesignWindow.TextLabelsCmdUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := LargeButtons;
end;

initialization
  InitFromResources;

end.
