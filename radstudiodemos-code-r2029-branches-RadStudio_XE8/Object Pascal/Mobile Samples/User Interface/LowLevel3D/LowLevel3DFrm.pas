unit LowLevel3DFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Materials,
  FMX.Forms3D, System.Math, FMX.Graphics;

type
  TForm1 = class(TForm3D)
    InputBitmap: TBitmapObject;
    SecondBitmap: TBitmapObject;
    procedure Form1Render(Sender: TObject; const Context: TContext3D);
    procedure Timer1(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
  System.Math.Vectors;

type

  TMyMaterial = class(TCustomMaterial)
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
    class function DoGetMaterialProperty(const Prop: TMaterial.TProperty): string; override;
  public
  end;

{ TMyMaterial }

class function TMyMaterial.DoGetMaterialProperty(const Prop: TMaterial.TProperty): string;
begin
  case Prop of
    TProperty.ModelViewProjection: Result := 'MVPMatrix';
  else
    Result := '';
  end;
end;

procedure TMyMaterial.DoInitialize;
begin
  inherited;
  FVertexShader := TShaderManager.RegisterShaderFromData('hw.fvs', TContextShaderKind.VertexShader,
    'float4x4 MVPMatrix;'#13+
    ' '#13+
    'void main(float3 a_position : POSITION,'#13+
    '  float2 a_texcoord0: TEXCOORD0,'#13+
    '  out float4 o_pos : POSITION,'#13+
    '  out float2 o_texcoord0 : TEXCOORD0)'#13+
    '{'#13+
    '  o_pos = mul(MVPMatrix, float4(a_position, 1.0));'#13+
    '  o_texcoord0 = a_texcoord0;  '#13+
    '}', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FE, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FE, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $76, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $00, $80, $01, $00, $0F, $90, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $55, $90, $01, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $A0, $00, $00, $00, $90, $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80,
      $02, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $C0, $00, $00, $E4, $80, $03, $00, $E4, $A0, $01, $00, $00, $02, $00, $00, $03, $E0, $01, $00, $E4, $90,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX10, [
      $44, $58, $42, $43, $F6, $28, $77, $77, $FE, $25, $25, $7B, $89, $F2, $CC, $B0, $54, $09, $A0, $15, $01, $00, $00, $00, $30, $03, $00, $00, $05, $00, $00, $00, $34, $00, $00, $00, $04, $01, $00, $00,
      $58, $01, $00, $00, $B0, $01, $00, $00, $B4, $02, $00, $00, $52, $44, $45, $46, $C8, $00, $00, $00, $01, $00, $00, $00, $48, $00, $00, $00, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FE, $FF,
      $00, $11, $00, $00, $94, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $AB, $AB, $3C, $00, $00, $00, $01, $00, $00, $00, $60, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $78, $00, $00, $00,
      $00, $00, $00, $00, $40, $00, $00, $00, $02, $00, $00, $00, $84, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C,
      $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $4C, $00, $00, $00, $02, $00, $00, $00, $08, $00, $00, $00, $38, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $07, $07, $00, $00, $41, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00,
      $03, $03, $00, $00, $50, $4F, $53, $49, $54, $49, $4F, $4E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $4F, $53, $47, $4E, $50, $00, $00, $00, $02, $00, $00, $00, $08, $00, $00, $00,
      $38, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $44, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $01, $00, $00, $00, $03, $0C, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $53, $48, $44, $52, $FC, $00, $00, $00,
      $40, $00, $01, $00, $3F, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00, $5F, $00, $00, $03, $72, $10, $10, $00, $00, $00, $00, $00, $5F, $00, $00, $03,
      $32, $10, $10, $00, $01, $00, $00, $00, $67, $00, $00, $04, $F2, $20, $10, $00, $00, $00, $00, $00, $01, $00, $00, $00, $65, $00, $00, $03, $32, $20, $10, $00, $01, $00, $00, $00, $68, $00, $00, $02,
      $01, $00, $00, $00, $38, $00, $00, $08, $F2, $00, $10, $00, $00, $00, $00, $00, $56, $15, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $10, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $A6, $1A, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $00, $00, $00, $08,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $36, $00, $00, $05, $32, $20, $10, $00, $01, $00, $00, $00,
      $46, $10, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $06, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 64)]
    ),
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63,
      $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72,
      $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30, $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61,
      $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76,
      $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69,
      $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72,
      $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50,
      $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28,
      $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20,
      $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31,
      $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F,
      $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A,
      $20, $20, $20, $20, $67, $6C, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('hw.fps', TContextShaderKind.PixelShader,
    '    sampler2D texture0;'#13+
    '    sampler2D texture1;'#13+
    '    float4 MyColor;'#13+
    ''#13+
    '    struct PSInput'#13+
    '    {'#13+
    '        float4 Pos       	: POSITION;'#13+
    '        float2 Tex0      	: TEXCOORD0;'#13+
    '    };'#13+
    '    '#13+
    '    float4 main( PSInput input ): COLOR'#13+
    '    {'#13+
    '      if (input.Tex0.x < 0.25) '#13+
    '        return tex2D(texture1, input.Tex0);'#13+
    ''#13+
    '      if (input.Tex0.x < 0.5) '#13+
    '        return tex2D(texture0, input.Tex0);'#13+
    ''#13+
    '      if (input.Tex0.x < 0.75) '#13+
    '        return float4(input.Tex0, 0, 1.0);'#13+
    ''#13+
    '      return MyColor; '#13+
    '    }', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $32, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $9C, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $95, $00, $00, $00,
      $58, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $60, $00, $00, $00, $00, $00, $00, $00, $70, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00,
      $8C, $00, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $4D, $79, $43, $6F, $6C, $6F, $72, $00, $01, $00, $03, $00, $01, $00, $04, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $31,
      $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69,
      $6C, $65, $72, $20, $00, $AB, $AB, $AB, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $80, $BE, $00, $00, $00, $BF, $00, $00, $40, $BF, $00, $00, $00, $00, $51, $00, $00, $05, $02, $00, $0F, $A0,
      $00, $00, $00, $00, $00, $00, $80, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0,
      $1F, $00, $00, $02, $00, $00, $00, $90, $01, $08, $0F, $A0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $B0,
      $01, $08, $E4, $A0, $02, $00, $00, $03, $02, $00, $08, $80, $00, $00, $00, $B0, $01, $00, $AA, $A0, $01, $00, $00, $02, $03, $00, $03, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $03, $00, $04, $80,
      $02, $00, $00, $A0, $01, $00, $00, $02, $03, $00, $08, $80, $02, $00, $55, $A0, $58, $00, $00, $04, $02, $00, $0F, $80, $02, $00, $FF, $80, $00, $00, $E4, $A0, $03, $00, $E4, $80, $02, $00, $00, $03,
      $03, $00, $01, $80, $00, $00, $00, $B0, $01, $00, $55, $A0, $58, $00, $00, $04, $00, $00, $0F, $80, $03, $00, $00, $80, $02, $00, $E4, $80, $00, $00, $E4, $80, $02, $00, $00, $03, $02, $00, $01, $80,
      $00, $00, $00, $B0, $01, $00, $00, $A0, $58, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $00, $80, $00, $00, $E4, $80, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('MyColor', TContextShaderVariableKind.Vector, 0, 1),
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX10, [
      $44, $58, $42, $43, $39, $0D, $73, $2B, $78, $34, $48, $B8, $20, $97, $D3, $58, $BB, $29, $D3, $A6, $01, $00, $00, $00, $38, $04, $00, $00, $05, $00, $00, $00, $34, $00, $00, $00, $90, $01, $00, $00,
      $E8, $01, $00, $00, $1C, $02, $00, $00, $BC, $03, $00, $00, $52, $44, $45, $46, $54, $01, $00, $00, $01, $00, $00, $00, $D8, $00, $00, $00, $05, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF,
      $00, $11, $00, $00, $20, $01, $00, $00, $BC, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $C5, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $C5, $00, $00, $00, $02, $00, $00, $00,
      $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $BC, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00,
      $FF, $FF, $FF, $FF, $01, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $CE, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $74, $65, $78, $74, $75, $72, $65, $31, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $CE, $00, $00, $00,
      $01, $00, $00, $00, $F0, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $01, $00, $00, $00, $00, $00, $00, $10, $00, $00, $00, $02, $00, $00, $00, $10, $01, $00, $00,
      $00, $00, $00, $00, $4D, $79, $43, $6F, $6C, $6F, $72, $00, $01, $00, $03, $00, $01, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52,
      $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB,
      $49, $53, $47, $4E, $50, $00, $00, $00, $02, $00, $00, $00, $08, $00, $00, $00, $38, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00,
      $44, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $03, $03, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $00, $54, $45, $58, $43,
      $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB, $53, $48, $44, $52, $98, $01, $00, $00, $40, $00, $00, $00, $66, $00, $00, $00, $59, $00, $00, $04,
      $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $01, $00, $00, $00, $58, $18, $00, $04,
      $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $01, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $01, $00, $00, $00,
      $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $01, $00, $00, $00, $31, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $00, $01, $00, $00, $00,
      $01, $40, $00, $00, $00, $00, $80, $3E, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $01, $00, $00, $00,
      $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $15, $00, $00, $01, $31, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $00,
      $01, $00, $00, $00, $01, $40, $00, $00, $00, $00, $00, $3F, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00,
      $01, $00, $00, $00, $46, $7E, $10, $00, $01, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $3E, $00, $00, $01, $15, $00, $00, $01, $31, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00,
      $0A, $10, $10, $00, $01, $00, $00, $00, $01, $40, $00, $00, $00, $00, $40, $3F, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05, $32, $20, $10, $00, $00, $00, $00, $00,
      $46, $10, $10, $00, $01, $00, $00, $00, $36, $00, $00, $08, $C2, $20, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $3F,
      $3E, $00, $00, $01, $15, $00, $00, $01, $36, $00, $00, $06, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54,
      $74, $00, $00, $00, $12, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
      ], [

      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('MyColor', TContextShaderVariableKind.Vector, 0, 16)]
    ),
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0A, $0D, $2F, $2F, $20, $67, $6C, $73, $6C, $66, $20, $6F, $75, $74, $70, $75, $74, $20, $62, $79, $20, $43,
      $67, $20, $63, $6F, $6D, $70, $69, $6C, $65, $72, $0A, $2F, $2F, $20, $63, $67, $63, $20, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $30, $31, $36, $2C, $20, $62, $75, $69, $6C,
      $64, $20, $64, $61, $74, $65, $20, $46, $65, $62, $20, $31, $31, $20, $32, $30, $31, $31, $0A, $2F, $2F, $20, $63, $6F, $6D, $6D, $61, $6E, $64, $20, $6C, $69, $6E, $65, $20, $61, $72, $67, $73, $3A,
      $20, $2D, $71, $20, $2D, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $20, $2D, $65, $6E, $74, $72, $79, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $20, $73, $6F, $75, $72, $63, $65,
      $20, $66, $69, $6C, $65, $3A, $20, $68, $77, $2E, $66, $70, $73, $0A, $2F, $2F, $76, $65, $6E, $64, $6F, $72, $20, $4E, $56, $49, $44, $49, $41, $20, $43, $6F, $72, $70, $6F, $72, $61, $74, $69, $6F,
      $6E, $0A, $2F, $2F, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $2E, $31, $36, $0A, $2F, $2F, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $0A, $2F, $2F, $70,
      $72, $6F, $67, $72, $61, $6D, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $74, $65, $78, $74, $75, $72, $65, $30, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74,
      $69, $63, $20, $74, $65, $78, $74, $75, $72, $65, $31, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $4D, $79, $43, $6F, $6C, $6F, $72, $0A, $2F, $2F, $76, $61, $72, $20, $73, $61, $6D,
      $70, $6C, $65, $72, $32, $44, $20, $74, $65, $78, $74, $75, $72, $65, $30, $20, $3A, $20, $20, $3A, $20, $5F, $74, $65, $78, $74, $75, $72, $65, $30, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A,
      $2F, $2F, $76, $61, $72, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $74, $65, $78, $74, $75, $72, $65, $31, $20, $3A, $20, $20, $3A, $20, $5F, $74, $65, $78, $74, $75, $72, $65, $31, $20,
      $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $34, $20, $4D, $79, $43, $6F, $6C, $6F, $72, $20, $3A, $20, $20, $3A, $20, $5F, $4D, $79, $43, $6F,
      $6C, $6F, $72, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $32, $20, $69, $6E, $70, $75, $74, $2E, $54, $65, $78, $30, $20, $3A, $20, $24,
      $76, $69, $6E, $2E, $54, $45, $58, $43, $4F, $4F, $52, $44, $30, $20, $3A, $20, $54, $45, $58, $43, $4F, $4F, $52, $44, $30, $20, $3A, $20, $30, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20,
      $66, $6C, $6F, $61, $74, $34, $20, $6D, $61, $69, $6E, $20, $3A, $20, $24, $76, $6F, $75, $74, $2E, $43, $4F, $4C, $4F, $52, $20, $3A, $20, $43, $4F, $4C, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31,
      $0A, $0A, $73, $74, $72, $75, $63, $74, $20, $50, $53, $49, $6E, $70, $75, $74, $20, $7B, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $54, $65, $78, $30, $3B, $0A, $7D, $3B, $0A, $0A, $76,
      $65, $63, $34, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $74, $65, $78, $74, $75, $72, $65, $30, $3B,
      $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $74, $65, $78, $74, $75, $72, $65, $31, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65,
      $63, $34, $20, $5F, $4D, $79, $43, $6F, $6C, $6F, $72, $3B, $0A, $0A, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $70, $72, $6F, $63, $65, $64, $75, $72, $65, $2C, $20, $74, $68, $65, $20, $6F, $72,
      $69, $67, $69, $6E, $61, $6C, $20, $6E, $61, $6D, $65, $20, $77, $61, $73, $20, $6D, $61, $69, $6E, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0A, $7B, $0A, $0A, $0A, $20, $20, $20,
      $20, $69, $66, $20, $28, $54, $45, $58, $30, $2E, $78, $20, $3C, $20, $32, $2E, $35, $30, $30, $30, $30, $30, $30, $30, $45, $2D, $30, $30, $31, $29, $20, $7B, $20, $2F, $2F, $20, $69, $66, $20, $62,
      $65, $67, $69, $6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $74, $65, $78, $74, $75, $72, $65,
      $31, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74,
      $5F, $30, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0A, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0A, $20, $20, $20, $20,
      $69, $66, $20, $28, $54, $45, $58, $30, $2E, $78, $20, $3C, $20, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2D, $30, $30, $31, $29, $20, $7B, $20, $2F, $2F, $20, $69, $66, $20, $62, $65,
      $67, $69, $6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $74, $65, $78, $74, $75, $72, $65, $30,
      $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74, $5F,
      $30, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0A, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0A, $20, $20, $20, $20, $69,
      $66, $20, $28, $54, $45, $58, $30, $2E, $78, $20, $3C, $20, $37, $2E, $35, $30, $30, $30, $30, $30, $30, $30, $45, $2D, $30, $30, $31, $29, $20, $7B, $20, $2F, $2F, $20, $69, $66, $20, $62, $65, $67,
      $69, $6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D, $20, $76, $65, $63, $34, $28, $54, $45, $58, $30, $2E, $78, $2C, $20, $54, $45, $58, $30, $2E, $79, $2C,
      $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $3B, $0A, $20, $20, $20, $20,
      $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $72, $65, $74, $75, $72,
      $6E, $3B, $0A, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $4D,
      $79, $43, $6F, $6C, $6F, $72, $3B, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0A, $7D, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $65, $6E, $64, $0A], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('MyColor', TContextShaderVariableKind.Vector, 0, 1)]
    )
  ]);
end;

procedure TMyMaterial.DoApply(const Context: TContext3D);
begin
  inherited;

  Context.SetShaderVariable('texture0', Context.BitmapToTexture(Form1.InputBitmap.Bitmap));
  Context.SetShaderVariable('texture1', Context.BitmapToTexture(Form1.SecondBitmap.Bitmap));
  Context.SetShaderVariable('MyColor', [ Vector3D(random, random, random, 1)]);
end;

{ TForm1 }

procedure TForm1.Form1Render(Sender: TObject; const Context: TContext3D);
var
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TMyMaterial;
begin
  // Set Vertices
  Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
  Ver.Vertices[0] := Point3D(-6, 0, -5);
  Ver.TexCoord0[0] := PointF(0, 0);
  Ver.Vertices[1] := Point3D( 6, 0, -5);
  Ver.TexCoord0[1] := PointF(1, 0);
  Ver.Vertices[2] := Point3D( 6, 0,  5);
  Ver.TexCoord0[2] := PointF(1, 1);
  Ver.Vertices[3] := Point3D(-6, 0,  5);
  Ver.TexCoord0[3] := PointF(0, 1);
  // Set Indices
  Idx := TIndexBuffer.Create(6);
  Idx[0] := 0;
  Idx[1] := 1;
  Idx[2] := 3;
  Idx[3] := 3;
  Idx[4] := 1;
  Idx[5] := 2;
  // Set matrix
  Context.SetMatrix(TMatrix3D.CreateRotation(Point3D(1,0,0), DegToRad(Tag)));
  // Set states
  Context.SetContextState(TContextState.csAllFace);
  // Create material
  Mat := TMyMaterial.Create;
  // render
  Context.DrawTriangles(Ver, Idx, Mat, 1);
  // dispose
  Mat.Free;
  Ver.Free;
  Idx.Free;
end;

procedure TForm1.Timer1(Sender: TObject);
begin
  Tag := Tag + 1;
  Invalidate;
end;

end.
