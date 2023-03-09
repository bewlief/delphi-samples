
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit anidemofrm;

interface

uses
  System.SysUtils, System.Classes,
  FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Objects3D, FMX.Ani,
  FMX.Layers3D, FMX.Controls, FMX.Types, FMX.Objects, FMX.Listbox,
  FMX.TreeView, FMX.TabControl, FMX.Layouts, FMX.Memo, FMX.Effects;

type
  TfrmAniDemo = class(TForm3D)
    Sphere1: TSphere;
    Light1: TLight;
    Text1: TTextLayer3D;
    ColorAnimation1: TColorAnimation;
    ColorAnimation2: TColorAnimation;
    Sphere2: TSphere;
    Text2: TTextLayer3D;
    FloatAnimation1: TFloatAnimation;
    StrokeCube1: TStrokeCube;
    Text3: TTextLayer3D;
    FloatAnimation2: TFloatAnimation;
    Cube1: TCube;
    Plane1: TPlane;
    RoundCube1: TRoundCube;
    FloatAnimation3: TFloatAnimation;
    Text4: TTextLayer3D;
    Text3D1: TText3D;
    FloatAnimation4: TFloatAnimation;
    ObjectLayer1: TLayer3D;
    RoundRect1: TRoundRect;
    PathAni: TPathAnimation;
    Path1: TPath;
    Background1: TLayout;
    Text5: TText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAniDemo: TfrmAniDemo;

implementation

{$R *.fmx}

end.
