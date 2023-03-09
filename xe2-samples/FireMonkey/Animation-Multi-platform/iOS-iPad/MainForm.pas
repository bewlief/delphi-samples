
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainForm;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls,
  FMX_Forms,
  FMX_Dialogs, FMX_Layouts, FMX_Ani, FMX_Types3D, FMX_Layers3D, FMX_Objects;

type
  TForm1 = class(TForm)
    bbRun: TButton;
    Image1: TImage;
    BitmapAnimation1: TBitmapAnimation;
    Image2: TImage;
    Viewport3D1: TViewport3D;
    Layer3D1: TLayer3D;
    Image3: TImage;
    Layer3D2: TLayer3D;
    Image4: TImage;
    Layer3D3: TLayer3D;
    Image5: TImage;
    procedure bbRunClick(Sender: TObject);
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

procedure TForm1.bbRunClick(Sender: TObject);
var
  i: Integer;
begin

  i := 280;

  // Image1 Animation
  Image1.AnimateFloat('Position.y', Image1.Position.Y + i, 5.0);
  Image1.AnimateFloat('RotationAngle', 12, 4.5);

  Image1.AnimateFloatDelay('Position.y', bbRun.Height + 20, 5.0, 5.0);
  Image1.AnimateFloatDelay('RotationAngle', 0, 4.5, 5.0);

  // Image2 Animation
  Image2.AnimateFloat('Position.y', Image2.Position.Y + i, 5.0);
  Image2.AnimateFloat('RotationAngle', -42, 4.5);

  Image2.AnimateFloatDelay('RotationCenter.Y', 6, 2, 2.5);
  Image2.AnimateFloatDelay('RotationCenter.Y', 0.5, 2, 5.0);

  Image2.AnimateFloatDelay('Position.y', bbRun.Height + 20, 5.0, 5.0);
  Image2.AnimateFloatDelay('RotationAngle', 0, 4.5, 5.0);

  // Icon Animation
  Layer3D2.Position.X := -12;
  Layer3D2.AnimateFloat('Position.X', 12, 10.0);
  Layer3D2.AnimateFloat('RotationAngle.Y', 720, 10.0);


  Layer3D2.AnimateFloat('Position.Y', 7.5, 1.0);
  Layer3D2.AnimateFloatDelay('Position.Y', 4.0, 1.0, 2.0);
  Layer3D2.AnimateFloatDelay('Position.Y', 7.5, 2.0, 4.0);

  // FireMonkey logo Animation
  Layer3D1.AnimateFloat('RotationAngle.Y', 360, 5.0);

  Layer3D1.AnimateFloat('RotationAngle.Y', 360, 4.0);

  Layer3D1.AnimateFloat('RotationAngle.X', 360, 2, TAnimationType.atInOut,
    TInterpolationType.itBack);

  Layer3D1.AnimateFloat('Position.Z', 200, 7.1);
  Layer3D1.AnimateFloatDelay('Position.Z', 0, 3, 7.1);


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AniFrameRate := 60;
end;

end.
