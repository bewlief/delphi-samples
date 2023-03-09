
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MV.CameraLookAt;

interface

uses
  MV.Utils,  FMX.Types, FMX.Types3D, System.Types;

type
  TCameraLookAt = class
  private
    FRotation:TPointf;
    FDistance: Single;
    FTarget: TVector3D;
    FEye: TVector3D;
    procedure SetRotation(const ARotation:TPointf);
    procedure SetDistance(const ADistance:Single);
  public
    constructor Create();
    function GetCameraMatrix(const AContext :TContext3D): TMatrix3D;
    function GetLoacalVector(const AMove: TVector3D):TVector3D;

    property Eye: TVector3D read FEye;
    property Rotation: TPointf read FRotation write SetRotation;
    property Distance: Single read FDistance write SetDistance;
    property Target: TVector3D read FTarget write FTarget;

    procedure Zoom(const ADelta: Single);
    procedure Rotate(const Ax, Ay: Single);
    procedure Move(const ADir: TVector3D);

  end;

implementation

procedure TCameraLookAt.Rotate(const Ax, Ay: Single);
begin
  Rotation := PointF( Rotation.X + Ax,  Rotation.Y + Ay);
end;

procedure TCameraLookAt.Move(const ADir: TVector3D);
begin
  Target := Vector3dAdd(Target, GetLoacalVector(ADir));
end;

procedure TCameraLookAt.Zoom(const ADelta: Single);
begin
  Distance := Distance + ADelta;
end;

function TCameraLookAt.GetLoacalVector(const AMove: TVector3D):TVector3D;
var
  LDir, LUp, LSide: TVector3D;
begin
  LUp :=  Vector3D(0,0,1);
  LDir := Vector3DNormalize(SphericalToRect(FRotation, FDistance));
  LSide := Vector3DNormalize(Vector3DCrossProduct(LUp, LDir));
  LUp := Vector3DCrossProduct(LDir, LSide);

  result := Vector3DScale(LSide,AMove.x );
  result := Vector3DAdd(result, Vector3DScale(LUp , -AMove.y ));
  result := Vector3DAdd(result, Vector3DScale(LDir ,AMove.z ));

end;

procedure TCameraLookAt.SetRotation(const ARotation:TPointf);
begin
  FRotation:= ARotation;
    if FRotation.Y > 140 then
      FRotation.Y := 140;
    if FRotation.Y < 1 then
      FRotation.Y := 1;
end;

procedure TCameraLookAt.SetDistance(const ADistance:Single);
begin
  FDistance := ADistance;
  if FDistance  < 1 then
    FDistance := 1
  else if FDistance > 250 then
    FDistance := 250;
end;

constructor TCameraLookAt.Create();
begin
  FRotation.X := 45;
  FRotation.Y := 70;
  FDistance := 80;

  FTarget := Vector3D(0, 0, 15);
end;

function TCameraLookAt.GetCameraMatrix(const AContext :TContext3D): TMatrix3D;
var
  LTransform: Tmatrix3D;
begin
  FEye := Vector3DAdd(FTarget, SphericalToRect(FRotation, FDistance));
  LTransform := MatrixLookAtRH(FEye, FTarget, Vector3D(0, 0, 1));
  result := Matrix3DMultiply(LTransform, AContext.CurrentPojectionMatrix);
end;

end.
