
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Unit2;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, FMX_Types3D, FMX_Layers3D, FMX_Objects, FMX_Objects3D, FMX_Ani,
  FMX_Effects, FMX_Filter_Effects;

type
  TForm1 = class(TForm)
    Image1: TImage;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Viewport3D1: TViewport3D;
    StyleBook2: TStyleBook;
    Text3D1: TText3D;
    ReflectionEffect1: TReflectionEffect;
    Viewport3D2: TViewport3D;
    Image3D1: TImage3D;
    TrackBar3: TTrackBar;
    FloatAnimation1: TFloatAnimation;
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Text3D1.RotationAngle.y := TrackBar1.Value;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
var tempangle: integer;
begin
  Image3D1.RotationAngle.x := TrackBar2.Value;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  Image1.RotationAngle := TrackBar3.Value;
end;

end.
