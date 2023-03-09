
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit ufMain2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Layers3D,
  FMX.TabControl, FMX.Colors, FMX.ListBox,
  MV.ColladaImport, MV.CameraLookAt, uShaders, FMX.Layouts;

type
  TForm1 = class(TForm3D)
    LayerGUI: TLayer3D;
    LayerRender: TLayer3D;
    Viewport: TViewport3D;
    Dummy1: TDummy;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ButtonLoadFromFile: TButton;
    GroupBox1: TGroupBox;
    cpAmbientColor: TColorPanel;
    cpLightColor: TColorPanel;
    Label1: TLabel;
    Label2: TLabel;
    cbLightPosAsCameraPos: TCheckBox;
    cbAnimatedLight: TCheckBox;
    GroupBox2: TGroupBox;
    rbMeshModeFrame: TRadioButton;
    rbMeshModeSolid: TRadioButton;
    GroupBox3: TGroupBox;
    dOpen: TOpenDialog;
    Label3: TLabel;
    lbMeshes: TListBox;
    lblTriangleCount: TLabel;
    lblVerticesCount: TLabel;
    rbAmbientMap: TRadioButton;
    rbDiffuseMap: TRadioButton;
    rbNormalMap: TRadioButton;
    rbSpecularMap: TRadioButton;
    rbEffects: TRadioButton;
    rbComposition: TRadioButton;
    procedure Dummy1Render(Sender: TObject; Context: TContext3D);
    procedure ButtonLoadFromFileClick(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DDestroy(Sender: TObject);
    procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ViewportMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure lbMeshesChange(Sender: TObject);
    procedure cpAmbientColorChange(Sender: TObject);
    procedure cbLightPosAsCameraPosChange(Sender: TObject);
    procedure rbMeshModeFrameChange(Sender: TObject);
    procedure rbMeshModeSolidChange(Sender: TObject);
    procedure rbAmbientMapChange(Sender: TObject);
    procedure rbDiffuseMapChange(Sender: TObject);
    procedure rbNormalMapChange(Sender: TObject);
    procedure rbSpecularMapChange(Sender: TObject);
    procedure rbEffectsChange(Sender: TObject);
    procedure rbCompositionChange(Sender: TObject);
  private
    FModel: TModel;
    FCamera: TCameraLookAt;
    FShaders: TShaders;

    FMousePosition: TPointf;
    FContextState:  TContextState;
    FRotSpeed: Single;
    FMoveSpeed: Single;
    FZoomSpeed: Single;

    FPixelShader: TContextShader;
    FVertexShader: TContextShader;

    FSunPosition: TVector3D;
    FAnimationTime: Single;
    FLightSize: Single;
  public
    procedure FormIdle(Sender: TObject; var Done: boolean);
  end;

var
  Form1: TForm1;

implementation

uses
  MV.Utils;

{$R *.fmx}

procedure TForm1.Form3DCreate(Sender: TObject);
begin
  dOpen.InitialDir := MODELS_PATH;

  // load and set active shaders
  FShaders:= TShaders.Create(Viewport.Context);
  FPixelShader := FShaders.PixelMode[pmComposition];
  FVertexShader := FShaders.VertexMode;

  // load default textures
  DefaultDiffuse:= TBitmap.CreateFromFile(TEXTURES_PATH + 'default_diff.bmp');
  DefaultNormal:= TBitmap.CreateFromFile(TEXTURES_PATH + 'default_nml.bmp');

  // loop event
  Application.OnIdle := FormIdle;
  FSunPosition := Vector3D(45, 70, 80);

  // create camera nad speed settings
  FCamera := TCameraLookAt.Create;
  FCamera.Target := Vector3d(0,0,30);
  FRotSpeed := 1/4;
  FMoveSpeed := 0.5;
  FZoomSpeed := -1/10;
end;

procedure TForm1.Form3DDestroy(Sender: TObject);
begin
  if Assigned(FModel) then
    FModel.Free;
  FCamera.Free;
  FShaders.Free;
  DefaultDiffuse.Free;
  DefaultNormal.Free;
end;

procedure TForm1.FormIdle(Sender: TObject; var Done: boolean);
begin
  LayerRender.Invalidate;
  Invalidate;
  Done := False;
end;

procedure TForm1.lbMeshesChange(Sender: TObject);
var
 LMesh: TMesh;
begin
  if Assigned(lbMeshes.Selected) and (lbMeshes.Selected.Index < FModel.MeshCount) then
  begin
    LMesh := FModel.MeshList[lbMeshes.Selected.Index];
    lblTriangleCount.Text := 'Triangle Count: ' + IntToStr(LMesh.TriangleCount);
    lblVerticesCount.Text := 'Vertices Count: ' + IntToStr(LMesh.VertexCount);
  end
  else
  begin
    lblTriangleCount.Text := '';
    lblVerticesCount.Text := '';
  end;
end;

procedure TForm1.rbMeshModeSolidChange(Sender: TObject);
begin
  if rbMeshModeSolid.IsChecked then
    FContextState := TContextState.csSolid;
end;

procedure TForm1.rbNormalMapChange(Sender: TObject);
begin
  FPixelShader := FShaders.PixelMode[pmNormal];
end;

procedure TForm1.rbSpecularMapChange(Sender: TObject);
begin
  FPixelShader := FShaders.PixelMode[pmSpecular];
end;

procedure TForm1.rbAmbientMapChange(Sender: TObject);
begin
  FPixelShader := FShaders.PixelMode[pmAmbient];
end;

procedure TForm1.rbCompositionChange(Sender: TObject);
begin
  FPixelShader := FShaders.PixelMode[pmComposition];
end;

procedure TForm1.rbDiffuseMapChange(Sender: TObject);
begin
  FPixelShader := FShaders.PixelMode[pmDiffuse];
end;

procedure TForm1.rbEffectsChange(Sender: TObject);
begin
  FPixelShader := FShaders.PixelMode[pmEffects];
end;

procedure TForm1.rbMeshModeFrameChange(Sender: TObject);
begin
  if rbMeshModeFrame.IsChecked then
    FContextState := TContextState.csFrame;
end;

procedure TForm1.ViewportMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  LMouseDelta: TPointf;
begin
  // Calculate delta between prevoius and actual position of mouse
  LMouseDelta := PointF(FMousePosition.X - X, FMousePosition.Y - Y);

  // Save mouse position as previous
  FMousePosition := PointF(X, Y);

  if ssLeft in Shift then
    FCamera.Rotate(LMouseDelta.X * FRotSpeed, LMouseDelta.Y * FRotSpeed)
  else if ssRight in Shift then
    FCamera.Move(Vector3D(LMouseDelta.X * FMoveSpeed, LMouseDelta.Y * FMoveSpeed, 0));
end;

procedure TForm1.ViewportMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  FCamera.Zoom(WheelDelta * FZoomSpeed);
end;

procedure TForm1.ButtonLoadFromFileClick(Sender: TObject);
var
  i:Integer;
  LItem: TListBoxItem;
begin
  if not dOpen.Execute then
    Exit;

  if Assigned(FModel) then
    FModel.Free;

  FModel := TModel.CreateFromFile(dOpen.FileName);

  lblTriangleCount.Text := '';
  lblVerticesCount.Text := '';

  lbMeshes.Clear;
  lbMeshes.BeginUpdate;
  for i := 0 to FModel.MeshCount - 1 do
  begin
    LItem := TListBoxItem.Create(lbMeshes);
    LItem.Text := FModel.MeshList[i].Name;
    lbMeshes.AddObject(LItem);
  end;
  lbMeshes.EndUpdate;
end;

procedure TForm1.cbLightPosAsCameraPosChange(Sender: TObject);
begin
  cbAnimatedLight.Enabled := not cbLightPosAsCameraPos.IsChecked;
end;

procedure TForm1.cpAmbientColorChange(Sender: TObject);
begin
  Viewport.Color := cpAmbientColor.Color;
end;

procedure TForm1.Dummy1Render(Sender: TObject; Context: TContext3D);
begin
  if cbAnimatedLight.IsChecked then
  begin
    FAnimationTime := FAnimationTime + 0.1;
    if (FAnimationTime > 2*pi) then FAnimationTime := 0;
  end;

  if cbLightPosAsCameraPos.IsChecked then
  begin
    FSunPosition := FCamera.Eye;
    FSunPosition.w := 1/500;  // Large light radius
  end else
    FSunPosition := Vector3D(sin(FAnimationTime)*50, cos(FAnimationTime)*50, 60, 1/150); // Vector3d(50,50,140);

  Context.SetContextState(TContextState.csBackFace);
  Context.SetContextState(TContextState.csTexLinear);
  Context.SetContextState(TContextState.csTexDisable);
  Context.SetContextState(TContextState.csLightOff);
  Context.SetPixelShader(FPixelShader);

  Context.SetVertexShader(FVertexShader);
  Context.SetContextState(FContextState);

  // prepare camera*projection matrix for shaders
  Context.SetVertexShaderMatrix(0, FCamera.GetCameraMatrix(Context));

  Context.SetPixelShaderVector(0, FSunPosition);
  Context.SetPixelShaderVector(1, ColorToVector3D(cpAmbientColor.Color));
  Context.SetPixelShaderVector(2, ColorToVector3D(cpLightColor.Color));
  Context.SetVertexShaderVector(7, FSunPosition);
  Context.SetVertexShaderVector(8,  FCamera.Eye);

  if Assigned(FModel) then
    FModel.Render(Context);

  Context.SetPixelShader(0);
  Context.SetVertexShader(0);

end;

end.
