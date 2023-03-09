{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC GUIx Forms memo                }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.VCLUI.Memo;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages, System.Win.Registry,
{$ENDIF}
  System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls,
{$IFDEF FireDAC_SynEdit}
  FireDAC.VCLUI.SynMemo,
{$ENDIF}
  FireDAC.Stan.Intf;

type
  {----------------------------------------------------------------------------}
  { TFDGUIxFormsMemo                                                           }
  {----------------------------------------------------------------------------}
{$IFDEF FireDAC_SynEdit}
  TFDGUIxFormsMemo = class(TFDGUIxFormsSynMemo)
  end;
{$ELSE}
  TFDGUIxFormsMemo = class(TMemo)
  private
    FRDBMSKind: TFDRDBMSKind;
  public
    constructor Create(AOwner: TComponent); override;
    property RDBMSKind: TFDRDBMSKind read FRDBMSKind write FRDBMSKind;
  end;
{$ENDIF}

implementation

uses
  System.SysUtils, System.TypInfo,
  FireDAC.Stan.Util;

{------------------------------------------------------------------------------}
{$IFNDEF FireDAC_SynEdit}
constructor TFDGUIxFormsMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Font.Name := 'Courier New';
  WordWrap := False;
  ScrollBars := ssBoth;
end;
{$ENDIF}

end.
