{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DBClientActnRes;

interface

uses
  SysUtils, Classes, ActnList, DBClientActns, DBActns, ImgList, Controls,
  Vcl.VirtualImageList, System.ImageList, System.Actions;

type
  TClientDatasetActions = class(TDataModule)
    ActionList1: TActionList;
    ClientDataSetApply1: TClientDataSetApply;
    ClientDataSetRevert1: TClientDataSetRevert;
    ClientDataSetUndo1: TClientDataSetUndo;
    ImageList1: TVirtualImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClientDatasetActions: TClientDatasetActions;

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}

end.
