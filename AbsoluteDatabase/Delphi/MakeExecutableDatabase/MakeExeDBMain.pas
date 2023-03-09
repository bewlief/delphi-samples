unit MakeExeDBMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ABSMain, ShellApi;

type
  TForm1 = class(TForm)
    edOutput: TEdit;
    edStub: TEdit;
    edDatabase: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    Button4: TButton;
    dbExecutable: TABSDatabase;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if dlgOpen.Execute then
    edStub.Text := dlgOpen.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if dlgOpen.Execute then
    eddatabase.Text := dlgOpen.FileName;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if dlgSave.Execute then
    edOutput.Text := dlgSave.FileName;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  dbExecutable.DatabaseFileName := edDatabase.Text;
  if (AnsiCompareText(ExtractFileExt(edOutput.Text), '.exe') <> 0) then
    edOutput.Text := edOutput.Text + '.exe';
  dbExecutable.MakeExecutableDatabase(edStub.Text, edOutput.Text);
  ShellExecute(Handle, 'open', PChar(edOutput.Text), nil, nil, SW_SHOWDEFAULT)

end;

end.
