
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit AddFilesFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdActns, Vcl.StdCtrls;

type
  TAddFilesFrame = class(TFrame)
    Label1: TLabel;
    Button1: TButton;
    ActionList1: TActionList;
    FileOpen1: TFileOpen;
    Memo1: TMemo;
    procedure FileOpen1Accept(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    FFileNames: TArray<string>;
    FUpdateNeeded: Boolean;
    function GetFileNames: TArray<string>;
    procedure UpdateFileNames;
    { Private declarations }
  public
    { Public declarations }
    procedure Validate;
    property FileNames: TArray<string> read GetFileNames;
  end;

implementation

{$R *.dfm}

procedure TAddFilesFrame.Edit1Change(Sender: TObject);
begin
  FUpdateNeeded := True;
end;

procedure TAddFilesFrame.FileOpen1Accept(Sender: TObject);
begin
  if Memo1.Text <> '' then
    Memo1.Text := Memo1.Text + '; ';
   Memo1.Text := Memo1.Text + FileOpen1.Dialog.FileName;
end;

function TAddFilesFrame.GetFileNames: TArray<string>;
begin
  UpdateFileNames;
  Result := FFileNames;
end;

procedure TAddFilesFrame.Memo1Change(Sender: TObject);
begin
  FUpdateNeeded := True;
end;

procedure TAddFilesFrame.UpdateFileNames;
var
  LStrings: TStrings;
begin
  if FUpdateNeeded then
  begin
    FUpdateNeeded := False;
    LStrings := TStringList.Create;
    try
      LStrings.Delimiter := ';';
      LStrings.DelimitedText := Memo1.Text;
      FFileNames := LStrings.ToStringArray;
    finally
      LStrings.Free;
    end;

  end;

end;
procedure TAddFilesFrame.Validate;
var
  S: string;
begin
  for S in FileNames do
    if not FileExists(S) then
      raise Exception.CreateFmt('File not found: "%s"', [S]);


end;

end.
