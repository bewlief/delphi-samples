{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC Base dialog form                }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.VCLUI.OptsBase;

interface

uses
  Winapi.Messages,
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, System.IniFiles;

type
  TfrmFDGUIxFormsOptsBase = class(TForm)
    pnlTop: TPanel;
    Image2: TImage;
    lblPrompt: TLabel;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    Bevel2: TBevel;
    Bevel3: TBevel;
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
   function ConvertToDefaultPPI(const AValue: Integer): Integer;
  protected
    procedure LoadFormState(AIni: TCustomIniFile); virtual;
    procedure SaveFormState(AIni: TCustomIniFile); virtual;
  public
    procedure LoadState;
    procedure SaveState;
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmFDGUIxFormsOptsBase: TfrmFDGUIxFormsOptsBase;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Vcl.Consts, System.Math,
{$IFDEF MSWINDOWS}
  System.Win.Registry,
{$ENDIF}
  FireDAC.Stan.Consts, FireDAC.Stan.Util;

{------------------------------------------------------------------------------}
{ TfrmFDGUIxFormsOptsBase                                                      }
{------------------------------------------------------------------------------}
const
  cImageSize = 32;

constructor TfrmFDGUIxFormsOptsBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  btnOk.Caption := '&' + SOKButton;
  if btnCancel.Caption = '&Cancel' then
    btnCancel.Caption := '&' + SCancelButton;
  Image2.Width := ScaleValue(cImageSize);
  Image2.Height := ScaleValue(cImageSize);
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  Image2.Width := ScaleValue(cImageSize);
  Image2.Height := ScaleValue(cImageSize);
end;

{------------------------------------------------------------------------------}
function TfrmFDGUIxFormsOptsBase.ConvertToDefaultPPI(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, Screen.DefaultPixelsPerInch, Self.CurrentPPI);
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.LoadFormState(AIni: TCustomIniFile);
var
  eWinState: TWindowState;
  rArea: TRect;
  iTop: Integer;
  iLeft: Integer;
begin
  eWinState := TWindowState(AIni.ReadInteger(Name, 'State', Integer(WindowState)));
  if eWinState = wsNormal then begin
    Position := poDesigned;
    Width := ScaleValue(AIni.ReadInteger(Name, 'Width', ConvertToDefaultPPI(Width)));
    Height := ScaleValue(AIni.ReadInteger(Name, 'Height', ConvertToDefaultPPI(Height)));
    iTop := ScaleValue(AIni.ReadInteger(Name, 'Top', ConvertToDefaultPPI(Top)));
    iLeft := ScaleValue(AIni.ReadInteger(Name, 'Left', ConvertToDefaultPPI(Left)));
    if Application.MainForm <> nil then
      rArea := Application.MainForm.Monitor.WorkareaRect
    else
      rArea := Monitor.WorkareaRect;
    if iTop < 0 then
      iTop := (rArea.Height - Height) div 2;
    if iTop > rArea.Bottom then
      iTop := Max(rArea.Bottom - Height, rArea.Top);
    if iLeft < 0 then
      iLeft := (rArea.Width - Width) div 2;
    if iLeft > rArea.Right then
      iLeft := Max(rArea.Right - Width, rArea.Left);
    Top := iTop;
    Left := iLeft;
  end
  else if eWinState = wsMaximized then
    WindowState := wsMaximized;
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.SaveFormState(AIni: TCustomIniFile);
begin
  AIni.WriteInteger(Name, 'State', Integer(WindowState));
  if WindowState = wsNormal then begin
    AIni.WriteInteger(Name, 'Width', ConvertToDefaultPPI(Width));
    AIni.WriteInteger(Name, 'Height', ConvertToDefaultPPI(Height));
    AIni.WriteInteger(Name, 'Top', ConvertToDefaultPPI(Top));
    AIni.WriteInteger(Name, 'Left', ConvertToDefaultPPI(Left));
  end;
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.LoadState;
var
  oIni: TCustomIniFile;
begin
  oIni := TFDConfigFile.Create(True);
  try
    Position := poMainFormCenter;
    if oIni.SectionExists(Name) then
      LoadFormState(oIni);
  except
  end;
  FDFree(oIni);
end;

{------------------------------------------------------------------------------}
procedure TfrmFDGUIxFormsOptsBase.SaveState;
var
  oIni: TCustomIniFile;
begin
  oIni := TFDConfigFile.Create(False);
  try
    SaveFormState(oIni);
  except
  end;
  FDFree(oIni);
end;

end.
