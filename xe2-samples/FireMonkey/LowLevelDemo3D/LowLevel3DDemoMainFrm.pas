
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit LowLevel3DDemoMainFrm;

interface

uses
  SysUtils, Variants, Classes, Math, FMX.Types, FMX.Forms, FMX.Types3D, FMX.Layers3D,
  FMX.Objects, System.Types;

type
  TfmxLowLevel3DDemoMain = class(TForm3D)
    InputBitmap: TBitmapObject;
    SecondBitmap: TBitmapObject;
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
    procedure Form1Render(Sender: TObject; const Context: TContext3D);
    procedure Timer1(Sender: TObject);
  private
    { Private declarations }
    FShader: TContextShader;
    FVertexShader: TContextShader;
  public
    { Public declarations }
  end;

var
  fmxLowLevel3DDemoMain: TfmxLowLevel3DDemoMain;

implementation

{$R *.fmx}

procedure TfmxLowLevel3DDemoMain.Form1Create(Sender: TObject);
const
  DX9PS2BIN: array [0..359] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $25, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $6A, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $63, $00, $00, $00,
    $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $01, $00, $01, $00, $06, $00, $4C, $00, $00, $00, $00, $00, $00, $00,
    $69, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $73, $65, $63, $6F, $6E, $64, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69,
    $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $AB, $51, $00, $00, $05,
    $00, $00, $0F, $A0, $9A, $99, $99, $BE, $9A, $99, $19, $BF, $00, $00, $80, $3F, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90,
    $00, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90, $01, $08, $0F, $A0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $B0, $01, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80,
    $00, $00, $E4, $B0, $00, $08, $E4, $A0, $02, $00, $00, $03, $02, $00, $08, $80, $00, $00, $00, $B0, $00, $00, $55, $A0, $01, $00, $00, $02, $03, $00, $03, $80, $00, $00, $E4, $B0, $01, $00, $00, $02,
    $03, $00, $0C, $80, $00, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $FF, $80, $03, $00, $E4, $80, $00, $00, $E4, $80, $02, $00, $00, $03, $02, $00, $01, $80, $00, $00, $00, $B0,
    $00, $00, $00, $A0, $58, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $00, $80, $00, $00, $E4, $80, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00

  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[1] = { { 1, 0, 0.30000001, 0.60000002 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'TEMP R2;'#13+
    'SLT R2.y, fragment.texcoord[0].x, c[0].w;'#13+
    'SLT R2.x, fragment.texcoord[0], c[0].z;'#13+
    'ABS R2.x, R2;'#13+
    'CMP R2.x, -R2, c[0].y, c[0];'#13+
    'TEX R0, fragment.texcoord[0], texture[1], 2D;'#13+
    'MUL R2.z, R2.y, R2.x;'#13+
    'TEX R1, fragment.texcoord[0], texture[0], 2D;'#13+
    'CMP R1, -R2.z, R0, R1;'#13+
    'ABS R2.w, R2.y;'#13+
    'CMP R2.y, -R2.w, c[0], c[0].x;'#13+
    'MUL R2.x, R2.y, R2;'#13+
    'MOV R0.xy, fragment.texcoord[0];'#13+
    'MOV R0.zw, c[0].x;'#13+
    'CMP result.color, -R2.x, R0, R1;'#13+
    'END';
  GLES_GLSLF: PAnsiChar =
    'precision mediump float;'+
    'vec4 _ret_0;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform sampler2D texture1;'#13+
    'void main()'#13+
    '{'#13+
    '    if (TEX0.x < 3.00000012E-001) { '#13+
    '        _ret_0 = texture2D(texture0, TEX0.xy);'#13+
    '        gl_FragColor = _ret_0;'#13+
    '        return;'#13+
    '    } else {'#13+
    '        if (TEX0.x < 6.00000024E-001) { '#13+
    '            _ret_0 = texture2D(texture1, TEX0.xy);'#13+
    '            gl_FragColor = _ret_0;'#13+
    '            return;'#13+
    '        } else {'#13+
    '            _ret_0 = vec4(TEX0.x, TEX0.y, 1.00000000E+000, 1.00000000E+000);'#13+
    '            gl_FragColor = _ret_0;'#13+
    '            return;'#13+
    '        } // end if'#13+
    '    } // end if'#13+
    '    gl_FragColor = _ret_0;'#13+
    '} ';
begin
  // First param - It is a compiled HLSL - DirectX binary format.
  // Second param - In Mac OS X must be uses ARBFP1 shader-asm string.
  // Third param - In iOS must be uses OpenGL ES 2.0 GLSL string.
  FShader := Context.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLES_GLSLF);
end;

procedure TfmxLowLevel3DDemoMain.Form1Destroy(Sender: TObject);
begin
  Context.DestroyPixelShader(FShader);
  Context.DestroyVertexShader(FVertexShader);
end;

procedure TfmxLowLevel3DDemoMain.Form1Render(Sender: TObject; const Context: TContext3D);
var
  M: TMatrix3D;
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
begin
  // Set Vertices
  Ver := TVertexBuffer.Create([TVertexFormat.vfVertex, TVertexFormat.vfTexCoord0], 4);
  Ver.Vertices[0] := Point3D(-5, 0, -5);
  Ver.TexCoord0[0] := PointF(0, 0);
  Ver.Vertices[1] := Point3D( 5, 0, -5);
  Ver.TexCoord0[1] := PointF(1, 0);
  Ver.Vertices[2] := Point3D( 5, 0,  5);
  Ver.TexCoord0[2] := PointF(1, 1);
  Ver.Vertices[3] := Point3D(-5, 0,  5);
  Ver.TexCoord0[3] := PointF(0, 1);
  // Set Indices
  Idx := TIndexBuffer.Create(4);
  Idx[0] := 0;
  Idx[1] := 1;
  Idx[2] := 2;
  Idx[3] := 3;
  // Set matrix
  M := CreateRotationMatrix3D(Vector3D(1, 0, 0), DegToRad(Tag));
  Context.SetMatrix(M);
  // set states
  Context.SetContextState(TContextState.csAllFace);
  Context.SetContextState(TContextState.csTexLinear);
  Context.SetContextState(TContextState.csLightOff);
  Context.SetContextState(TContextState.csTexReplace);
  Context.SetColor(TMaterialColor.mcDiffuse, $FFFFFFFF);
  // bitmap
  Context.SetTextureUnit(0, InputBitmap.Bitmap);
  Context.SetTextureUnit(1, SecondBitmap.Bitmap);
  // shader
  Context.SetPixelShader(FShader);
  // render
  Context.DrawTrianglesFan(Ver, Idx, 1);
  // dispose
  Ver.Free;
  Idx.Free;
end;

procedure TfmxLowLevel3DDemoMain.Timer1(Sender: TObject);
begin
  Tag := Tag + 1;
  Invalidate;
end;

end.
