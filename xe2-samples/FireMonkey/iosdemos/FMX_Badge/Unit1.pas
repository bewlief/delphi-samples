
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Unit1;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, SharedApplication;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ImageControl1: TImageControl;
    iOSIconBadge1: TiOSIconBadge;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  ButtonText : String;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Text := ButtonText+' -> '+IntToStr(iOSIconBadge1.Number);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  iOSIconBadge1.Number := iOSIconBadge1.Number+1;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  iOSIconBadge1.Number := iOSIconBadge1.Number*2;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  iOSIconBadge1.Number := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ButtonText := Button1.Text;
end;

end.

