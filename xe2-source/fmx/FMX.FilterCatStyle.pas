{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.FilterCatStyle;

interface

{$I FMX.Defines.inc}

uses
  System.UITypes, FMX.Filter, FMX.Types3D;

type

  TPixelateFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TEmbossFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TToonFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TSharpenFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TSepiaFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TPaperSketchFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TPencilStrokeFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

  TGlowFilter = class(TShaderFilter)
  protected
    procedure LoadShader; override;
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  
  TInnerGlowFilter = class(TShaderFilter)
  protected
    procedure LoadShader; override;
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  
  TReflectionFilter = class(TShaderFilter)
  protected
    class function FilterAttr: TFilterRec; override;
  public
    constructor Create; override;
  end;

implementation

{ TPixelateFilter }

constructor TPixelateFilter.Create;
const
  DX9PS2BIN: array [0..347] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2A, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7F, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $78, $00, $00, $00,
    $44, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $50, $00, $00, $00, $00, $00, $00, $00, $60, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $68, $00, $00, $00, $00, $00, $00, $00,
    $42, $6C, $6F, $63, $6B, $43, $6F, $75, $6E, $74, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00,
    $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53,
    $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $00, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $05, $00, $00, $03, $00, $00, $03, $80, $00, $00, $E4, $B0, $00, $00, $00, $A0,
    $13, $00, $00, $02, $01, $00, $03, $80, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $03, $80, $00, $00, $E4, $80, $01, $00, $E4, $81, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0,
    $05, $00, $00, $03, $00, $00, $08, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $03, $80, $00, $00, $E4, $80, $00, $00, $AA, $80, $00, $00, $FF, $80, $42, $00, $00, $03,
    $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[2] = { program.local[0],'#13+
    '		{ 0.5 } };'#13+
    'TEMP R0;'#13+
    'RCP R0.z, c[0].x;'#13+
    'MUL R0.xy, fragment.texcoord[0], c[0].x;'#13+
    'MUL R0.w, R0.z, c[1].x;'#13+
    'FLR R0.xy, R0;'#13+
    'MAD R0.xy, R0, R0.z, R0.w;'#13+
    'TEX result.color, R0, texture[0], 2D;'#13+
    'END';
  GLSLF: PAnsiChar =
    'vec4 _ret_0;'#13+
    'vec2 _x0003;'#13+
    'varying vec4 TEX0;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform sampler2D texture0;'#13+
    'float _BlockSize;'#13+
    'void main()'#13+
    '{'#13+
    '    vec2 _blockPos;'#13+
    '    vec2 _blockCenter;'#13+
    '    _BlockSize = 1.00000000E+000/PSParam0.x;'#13+
    '    _x0003 = TEX0.xy*PSParam0.x;'#13+
    '    _blockPos = floor(_x0003);'#13+
    '    _blockCenter = _blockPos*_BlockSize + _BlockSize*5.00000000E-001;'#13+
    '    _ret_0 = texture2D(texture0, _blockCenter);'#13+
    '    gl_FragColor = _ret_0;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TPixelateFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Pixelate', '', [
    FilterValueRec('BlockCount', 'The number of pixel blocks.', TShaderValueType.vtFloat, 25, 1, 1000)
  ]);
end;

{ TEmbossFilter }

constructor TEmbossFilter.Create;
const
  DX9PS2BIN: array [0..483] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $33, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $A3, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $9C, $00, $00, $00,
    $58, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $60, $00, $00, $00, $00, $00, $00, $00, $70, $00, $00, $00, $02, $00, $01, $00, $01, $00, $06, $00, $60, $00, $00, $00, $00, $00, $00, $00,
    $76, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $8C, $00, $00, $00, $00, $00, $00, $00, $41, $6D, $6F, $75, $6E, $74, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00,
    $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $69, $6D, $70, $6C, $69, $63, $69, $74, $49, $6E, $70, $75, $74, $53, $61, $6D, $70, $6C, $65, $72, $00, $AB, $04, $00, $0C, $00, $01, $00, $01, $00,
    $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
    $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $02, $00, $0F, $A0, $6F, $12, $83, $3A, $AB, $AA, $AA, $3E, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05,
    $03, $00, $0F, $A0, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $80, $3F, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90,
    $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $08, $80, $02, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $03, $80, $01, $00, $00, $A0, $00, $00, $FF, $81, $00, $00, $E4, $B0, $04, $00, $00, $04,
    $01, $00, $03, $80, $01, $00, $00, $A0, $00, $00, $FF, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80,
    $01, $00, $E4, $80, $00, $08, $E4, $A0, $01, $00, $00, $02, $02, $00, $08, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $80, $02, $00, $FF, $81, $03, $00, $E4, $A0,
    $04, $00, $00, $04, $00, $00, $0F, $80, $01, $00, $E4, $80, $00, $00, $00, $A0, $00, $00, $E4, $80, $02, $00, $00, $03, $01, $00, $01, $80, $00, $00, $55, $80, $00, $00, $00, $80, $02, $00, $00, $03,
    $01, $00, $01, $80, $00, $00, $AA, $80, $01, $00, $00, $80, $05, $00, $00, $03, $00, $00, $07, $80, $01, $00, $00, $80, $02, $00, $55, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
    $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[3] = { program.local[0..1],'#13+
    '		{ 0.001, 0.33333334, 0.5, 1 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'MOV R0.x, c[1];'#13+
    'MAD R0.zw, R0.x, c[2].x, fragment.texcoord[0].xyxy;'#13+
    'TEX R1, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.xy, R0.x, -c[2].x, fragment.texcoord[0];'#13+
    'MUL R1, R1, c[0].x;'#13+
    'TEX R0, R0, texture[0], 2D;'#13+
    'MAD R0, -R0, c[0].x, R1;'#13+
    'ADD R0, R0, c[2].zzzw;'#13+
    'ADD R0.x, R0, R0.y;'#13+
    'ADD R0.x, R0, R0.z;'#13+
    'MUL result.color.xyz, R0.x, c[2].y;'#13+
    'MOV result.color.w, R0;'#13+
    'END';
  GLSLF: PAnsiChar =
    'vec4 _TMP1;'#13+
    'vec4 _TMP0;'#13+
    'vec2 _c0006;'#13+
    'vec2 _c0008;'#13+
    'varying vec4 TEX0;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform vec4 PSParam1;'#13+
    'uniform sampler2D texture0;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _outC;'#13+
    '    _c0006 = TEX0.xy - PSParam1.x/1.00000000E+003;'#13+
    '    _TMP0 = texture2D(texture0, _c0006);'#13+
    '    _outC = vec4( 5.00000000E-001, 5.00000000E-001, 5.00000000E-001, 1.00000000E+000) - _TMP0*PSParam0.x;'#13+
    '    _c0008 = TEX0.xy + PSParam1.x/1.00000000E+003;'#13+
    '    _TMP1 = texture2D(texture0, _c0008);'#13+
    '    _outC = _outC + _TMP1*PSParam0.x;'#13+
    '    _outC.xyz = vec3((_outC.x + _outC.y + _outC.z)/3.00000000E+000, (_outC.x + _outC.y + _outC.z)/3.00000000E+000, (_outC.x + _outC.y + _outC.z)/3.00000000E+000);'#13+
    '    gl_FragColor = _outC;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TEmbossFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Emboss', 'An effect that embosses the input.', [
    FilterValueRec('Amount', 'The amplitude of the embossing.', TShaderValueType.vtFloat, 0.5, 0, 1),
    FilterValueRec('Width', 'The separation between samples (as a fraction of input size).', TShaderValueType.vtFloat, 3, 0, 10)
  ]);
end;

{ TToonFilter }

constructor TToonFilter.Create;
const
  DX9PS2BIN: array [0..387] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2D, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $8B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $84, $00, $00, $00,
    $44, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $74, $00, $00, $00, $00, $00, $00, $00,
    $4C, $65, $76, $65, $6C, $73, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6D, $70, $6C, $69, $63, $69, $74, $49, $6E, $70, $75, $74, $53, $61, $6D,
    $70, $6C, $65, $72, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74,
    $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
    $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $13, $00, $00, $02, $01, $00, $08, $80, $00, $00, $00, $A0,
    $02, $00, $00, $03, $01, $00, $01, $80, $01, $00, $FF, $81, $00, $00, $00, $A0, $06, $00, $00, $02, $01, $00, $02, $80, $00, $00, $FF, $80, $05, $00, $00, $03, $02, $00, $07, $80, $00, $00, $E4, $80,
    $01, $00, $55, $80, $05, $00, $00, $03, $02, $00, $07, $80, $01, $00, $00, $80, $02, $00, $E4, $80, $06, $00, $00, $02, $02, $00, $08, $80, $01, $00, $00, $80, $13, $00, $00, $02, $01, $00, $07, $80,
    $02, $00, $E4, $80, $02, $00, $00, $03, $01, $00, $07, $80, $02, $00, $E4, $80, $01, $00, $E4, $81, $05, $00, $00, $03, $01, $00, $07, $80, $02, $00, $FF, $80, $01, $00, $E4, $80, $05, $00, $00, $03,
    $00, $00, $07, $80, $00, $00, $FF, $80, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[1] = { program.local[0] };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'TEX R0, fragment.texcoord[0], texture[0], 2D;'#13+
    'RCP R1.y, R0.w;'#13+
    'FLR R1.x, c[0];'#13+
    'MUL R0.xyz, R0, R1.y;'#13+
    'MUL R0.xyz, R0, R1.x;'#13+
    'RCP R1.x, R1.x;'#13+
    'FLR R0.xyz, R0;'#13+
    'MUL R0.xyz, R0, R1.x;'#13+
    'MUL result.color.xyz, R0, R0.w;'#13+
    'MOV result.color.w, R0;'#13+
    'END';
  GLSLF: PAnsiChar =
    'float _TMP0;'#13+
    'varying vec4 TEX0;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform sampler2D texture0;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _color;'#13+
    '    int _levels;'#13+
    '    _color = texture2D(texture0, TEX0.xy);'#13+
    '    _color.xyz = _color.xyz/_color.www;'#13+
    '    _TMP0 = floor(PSParam0.x);'#13+
    '    _levels = int(_TMP0);'#13+
    '    _color.xyz = _color.xyz*vec3(float(_levels), float(_levels), float(_levels));'#13+
    '    _color.xyz = floor(_color.xyz);'#13+
    '    _color.xyz = _color.xyz/vec3(float(_levels), float(_levels), float(_levels));'#13+
    '    _color.xyz = _color.xyz*_color.www;'#13+
    '    gl_FragColor = _color;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TToonFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Toon', 'An effect that applies cartoon-like shading (posterization).', [
    FilterValueRec('Levels', 'The number of color levels to use.', TShaderValueType.vtFloat, 5, 3, 15)
  ]);
end;

{ TSharpenFilter }

constructor TSharpenFilter.Create;
const
  DX9PS2BIN: array [0..371] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2D, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $8B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $84, $00, $00, $00,
    $44, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $74, $00, $00, $00, $00, $00, $00, $00,
    $41, $6D, $6F, $75, $6E, $74, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6D, $70, $6C, $69, $63, $69, $74, $49, $6E, $70, $75, $74, $53, $61, $6D,
    $70, $6C, $65, $72, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74,
    $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $A6, $9B, $44, $BB,
    $A6, $9B, $44, $3B, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $02, $00, $00, $03,
    $00, $00, $03, $80, $00, $00, $E4, $B0, $01, $00, $55, $A0, $02, $00, $00, $03, $01, $00, $03, $80, $00, $00, $E4, $B0, $01, $00, $00, $A0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80,
    $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $04, $00, $00, $04,
    $01, $00, $07, $80, $01, $00, $E4, $80, $00, $00, $00, $A0, $02, $00, $E4, $80, $04, $00, $00, $04, $02, $00, $07, $80, $00, $00, $E4, $80, $00, $00, $00, $A1, $01, $00, $E4, $80, $01, $00, $00, $02,
    $00, $08, $0F, $80, $02, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[2] = { program.local[0],'#13+
    '		{ 0.003 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'TEMP R2;'#13+
    'TEX R0, fragment.texcoord[0], texture[0], 2D;'#13+
    'ADD R1.xy, fragment.texcoord[0], -c[1].x;'#13+
    'TEX R1.xyz, R1, texture[0], 2D;'#13+
    'MAD R0.xyz, R1, c[0].x, R0;'#13+
    'ADD R2.xy, fragment.texcoord[0], c[1].x;'#13+
    'TEX R1.xyz, R2, texture[0], 2D;'#13+
    'MAD result.color.xyz, -R1, c[0].x, R0;'#13+
    'MOV result.color.w, R0;'#13+
    'END';
  GLSLF: PAnsiChar =
    'vec4 _TMP1;'#13+
    'vec4 _TMP0;'#13+
    'vec2 _c0007;'#13+
    'vec2 _c0009;'#13+
    'varying vec4 TEX0;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform sampler2D texture0;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _color;'#13+
    '    _color = texture2D(texture0, TEX0.xy);'#13+
    '    _c0007 = TEX0.xy - 3.00000003E-003;'#13+
    '    _TMP0 = texture2D(texture0, _c0007);'#13+
    '    _color.xyz = _color.xyz + _TMP0.xyz*PSParam0.x;'#13+
    '    _c0009 = TEX0.xy + 3.00000003E-003;'#13+
    '    _TMP1 = texture2D(texture0, _c0009);'#13+
    '    _color.xyz = _color.xyz - _TMP1.xyz*PSParam0.x;'#13+
    '    gl_FragColor = _color;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TSharpenFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Sharpen', 'An effect that sharpens the input.', [
    FilterValueRec('Amount', 'The amount of sharpening.', TShaderValueType.vtFloat, 1, 0, 2)
  ]);
end;

{ TSepiaFilter }

constructor TSepiaFilter.Create;
const
  DX9PS2BIN: array [0..419] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $29, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $74, $00, $00, $00,
    $44, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $64, $00, $00, $00, $00, $00, $00, $00,
    $66, $61, $63, $74, $6F, $72, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00,
    $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
    $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $80, $3F, $89, $88, $88, $3E, $89, $88, $08, $3E, $89, $88, $88, $3D, $1F, $00, $00, $02,
    $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $02, $00, $00, $03,
    $01, $00, $08, $80, $00, $00, $55, $80, $00, $00, $00, $80, $02, $00, $00, $03, $01, $00, $01, $80, $00, $00, $AA, $80, $01, $00, $FF, $80, $05, $00, $00, $03, $01, $00, $01, $80, $01, $00, $00, $80,
    $00, $00, $00, $A0, $01, $00, $00, $02, $02, $00, $08, $80, $01, $00, $00, $A0, $02, $00, $00, $03, $01, $00, $02, $80, $02, $00, $FF, $80, $00, $00, $00, $A1, $05, $00, $00, $03, $01, $00, $04, $80,
    $00, $00, $00, $80, $01, $00, $55, $80, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $00, $80, $01, $00, $55, $A0, $01, $00, $AA, $80, $05, $00, $00, $03, $01, $00, $06, $80, $00, $00, $1B, $80,
    $01, $00, $55, $80, $04, $00, $00, $04, $00, $00, $02, $80, $01, $00, $00, $80, $01, $00, $AA, $A0, $01, $00, $AA, $80, $04, $00, $00, $04, $00, $00, $04, $80, $01, $00, $00, $80, $01, $00, $FF, $A0,
    $01, $00, $55, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[3] = { program.local[0],'#13+
    '		{ 0.33333334, 0.2, 1, 0.40000001 },'#13+
    '		{ 0.80000001 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'TEX R0, fragment.texcoord[0], texture[0], 2D;'#13+
    'ADD R1.y, R0.x, R0;'#13+
    'ADD R1.y, R1, R0.z;'#13+
    'MOV R1.x, c[1].z;'#13+
    'ADD R1.x, R1, -c[0];'#13+
    'MUL R1.y, R1, c[1].x;'#13+
    'MUL R1.y, R1, c[0].x;'#13+
    'MUL R0.z, R0, R1.x;'#13+
    'MUL R0.y, R0, R1.x;'#13+
    'MUL R0.x, R0, R1;'#13+
    'MAD result.color.z, R1.y, c[1].y, R0;'#13+
    'MAD result.color.y, R1, c[1].w, R0;'#13+
    'MAD result.color.x, R1.y, c[2], R0;'#13+
    'MOV result.color.w, R0;'#13+
    'END';
  GLSLF: PAnsiChar =
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _clr;'#13+
    '    float _avg;'#13+
    '    _clr = texture2D(texture0, TEX0.xy);'#13+
    '    _avg = (_clr.x + _clr.y + _clr.z)/3.00000000E+000;'#13+
    '    _clr.x = 8.00000012E-001*_avg*PSParam0.x + _clr.x*(1.00000000E+000 - PSParam0.x);'#13+
    '    _clr.y = 4.00000006E-001*_avg*PSParam0.x + _clr.y*(1.00000000E+000 - PSParam0.x);'#13+
    '    _clr.z = 2.00000003E-001*_avg*PSParam0.x + _clr.z*(1.00000000E+000 - PSParam0.x);'#13+
    '    gl_FragColor = _clr;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TSepiaFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Sepia', 'Sepia effect.', [
    FilterValueRec('Amount', 'The amount of sharpening.', TShaderValueType.vtFloat, 0.5, 0, 1)
  ]);
end;

{ TPaperSketchFilter }

constructor TPaperSketchFilter.Create;
const
  DX9PS2BIN: array [0..743] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2A, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7F, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $78, $00, $00, $00,
    $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $68, $00, $00, $00, $00, $00, $00, $00,
    $49, $6D, $61, $67, $65, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $62, $72, $75, $73, $68, $53, $69, $7A, $65, $00, $AB, $AB, $00, $00, $03, $00,
    $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53,
    $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $AE, $47, $E1, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $00, $00, $6F, $12, $83, $3A, $3D, $0A, $17, $3F, $9A, $99, $99, $3E, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $00, $00, $6F, $12, $83, $BA,
    $00, $00, $80, $C0, $00, $00, $80, $3F, $51, $00, $00, $05, $04, $00, $0F, $A0, $6F, $12, $83, $BA, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $12, $83, $3A, $1F, $00, $00, $02, $00, $00, $00, $80,
    $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $08, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $03, $80, $00, $00, $FF, $80,
    $02, $00, $E4, $A0, $00, $00, $E4, $B0, $04, $00, $00, $04, $01, $00, $03, $80, $00, $00, $FF, $80, $04, $00, $1B, $A0, $00, $00, $E4, $B0, $04, $00, $00, $04, $02, $00, $03, $80, $00, $00, $FF, $80,
    $04, $00, $E4, $A0, $00, $00, $E4, $B0, $04, $00, $00, $04, $03, $00, $03, $80, $00, $00, $FF, $80, $03, $00, $E4, $A0, $00, $00, $E4, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80,
    $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
    $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $02, $80, $04, $00, $00, $80,
    $03, $00, $AA, $A0, $03, $00, $00, $80, $02, $00, $00, $03, $00, $00, $02, $80, $02, $00, $00, $80, $00, $00, $55, $80, $02, $00, $00, $03, $00, $00, $02, $80, $01, $00, $00, $80, $00, $00, $55, $80,
    $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $55, $80, $06, $00, $00, $02, $00, $00, $01, $80, $00, $00, $00, $80, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $81,
    $03, $00, $FF, $A0, $02, $00, $00, $03, $00, $00, $02, $80, $00, $00, $00, $81, $03, $00, $FF, $A0, $05, $00, $00, $03, $01, $00, $01, $80, $00, $00, $00, $80, $03, $00, $FF, $A0, $05, $00, $00, $03,
    $01, $00, $08, $80, $04, $00, $55, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $01, $00, $08, $80, $04, $00, $00, $80, $02, $00, $FF, $A0, $01, $00, $FF, $80, $04, $00, $00, $04, $01, $00, $02, $80,
    $04, $00, $AA, $80, $01, $00, $00, $A0, $01, $00, $FF, $80, $01, $00, $00, $02, $01, $00, $04, $80, $04, $00, $FF, $80, $58, $00, $00, $04, $01, $00, $01, $80, $00, $00, $55, $80, $01, $00, $55, $80,
    $01, $00, $00, $80, $58, $00, $00, $04, $01, $00, $02, $80, $00, $00, $55, $80, $01, $00, $AA, $80, $01, $00, $AA, $80, $01, $00, $00, $02, $00, $00, $07, $80, $01, $00, $00, $80, $01, $00, $00, $02,
    $00, $00, $08, $80, $01, $00, $55, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[4] = { program.local[0],'#13+
    '		{ 1, -4, 0.001, 0 },'#13+
    '		{ 0, -1, 0.11, 0.30000001 },'#13+
    '		{ 0.58999997 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'TEMP R2;'#13+
    'TEX R2, fragment.texcoord[0], texture[0], 2D;'#13+
    'MOV R0.x, c[0];'#13+
    'MUL R1.y, R0.x, c[1].z;'#13+
    'MAD R0.xy, R1.y, c[2], fragment.texcoord[0];'#13+
    'MAD R0.zw, R1.y, c[2].xyyx, fragment.texcoord[0].xyxy;'#13+
    'TEX R0.x, R0, texture[0], 2D;'#13+
    'TEX R1.x, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.x, R2, c[1].y, R0;'#13+
    'ADD R1.z, R0.x, R1.x;'#13+
    'MAD R0.zw, R1.y, c[1].xywx, fragment.texcoord[0].xyxy;'#13+
    'MAD R0.xy, R1.y, c[1].xwzw, fragment.texcoord[0];'#13+
    'TEX R0.x, R0, texture[0], 2D;'#13+
    'MUL R0.y, R2, c[3].x;'#13+
    'TEX R1.x, R0.zwzw, texture[0], 2D;'#13+
    'ADD R0.x, R1.z, R0;'#13+
    'ADD R0.x, R0, R1;'#13+
    'MAD R0.y, R2.x, c[2].w, R0;'#13+
    'RCP R0.x, R0.x;'#13+
    'ADD R0.x, -R0, c[1];'#13+
    'MAD R1.xyz, R2.z, c[2].z, R0.y;'#13+
    'MUL R0.z, R0.x, c[3].x;'#13+
    'SLT R0.y, c[1].x, R0.x;'#13+
    'ABS R0.w, R0.y;'#13+
    'CMP R2.x, -R0.w, c[1].w, c[1];'#13+
    'MAD R0.z, R0.x, c[2].w, R0;'#13+
    'MAD R0.xyz, R0.x, c[2].z, R0.z;'#13+
    'MOV R0.w, R2;'#13+
    'MOV R1.w, R2;'#13+
    'CMP result.color, -R2.x, R1, R0;'#13+
    'END';
  GLSLF: PAnsiChar =
    'vec4 _ret_0;'#13+
    'vec4 _TMP1;'#13+
    'float _TMP4;'#13+
    'vec2 _c0010;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _color;'#13+
    '    vec4 _laplace;'#13+
    '    vec4 _complement;'#13+
    '    float _gray1;'#13+
    '    float _gray2;'#13+
    '    _TMP4 = PSParam0.x/1.00000000E+003;'#13+
    '    _color = texture2D(texture0, TEX0.xy);'#13+
    '    _laplace = -4.00000000E+000*_color;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( 0.0, -1.00000000E+000);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _laplace = _laplace + _TMP1;'#13+
    '    _laplace.y = _laplace.x;'#13+
    '    _laplace.z = _laplace.x;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( -1.00000000E+000, 0.0);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _laplace = _laplace + _TMP1;'#13+
    '    _laplace.y = _laplace.x;'#13+
    '    _laplace.z = _laplace.x;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( 1.00000000E+000, 0.0);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _laplace = _laplace + _TMP1;'#13+
    '    _laplace.y = _laplace.x;'#13+
    '    _laplace.z = _laplace.x;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( 0.0, 1.00000000E+000);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _laplace = _laplace + _TMP1;'#13+
    '    _laplace.y = _laplace.x;'#13+
    '    _laplace.z = _laplace.x;'#13+
    '    _laplace = 1.00000000E+000/_laplace;'#13+
    '    _complement.xyz = 1.00000000E+000 - _laplace.xyz;'#13+
    '    _complement.w = _color.w;'#13+
    '    if (_complement.x > 1.00000000E+000) { '#13+
    '        _gray1 = _complement.x*3.00000012E-001 + _complement.y*5.89999974E-001 + _complement.z*1.09999999E-001;'#13+
    '        _complement.x = _gray1;'#13+
    '        _complement.y = _gray1;'#13+
    '        _complement.z = _gray1;'#13+
    '        gl_FragColor = _complement;'#13+
    '        return;'#13+
    '    } else {'#13+
    '        _gray2 = _color.x*3.00000012E-001 + _color.y*5.89999974E-001 + _color.z*1.09999999E-001;'#13+
    '        _color.x = _gray2;'#13+
    '        _color.y = _gray2;'#13+
    '        _color.z = _gray2;'#13+
    '        gl_FragColor = _color;'#13+
    '        return;'#13+
    '    } // end if'#13+
    '    gl_FragColor = _ret_0;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TPaperSketchFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('PaperSketch', 'An paper sketch effect.', [
    FilterValueRec('BrushSize', 'The brush size of the sketch effect.', TShaderValueType.vtFloat, 3, 0.6, 10)
  ]);
end;

{ TPencilStrokeFilter }

constructor TPencilStrokeFilter.Create;
const
  DX9PS2BIN: array [0..543] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2A, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7F, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $78, $00, $00, $00,
    $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $68, $00, $00, $00, $00, $00, $00, $00,
    $49, $6D, $61, $67, $65, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $62, $72, $75, $73, $68, $53, $69, $7A, $65, $00, $AB, $AB, $00, $00, $03, $00,
    $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53,
    $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $00, $00, $0A, $D7, $23, $BC, $00, $00, $80, $C0, $00, $00, $80, $3F,
    $51, $00, $00, $05, $02, $00, $0F, $A0, $0A, $D7, $23, $BC, $00, $00, $00, $00, $00, $00, $00, $00, $0A, $D7, $23, $3C, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $00, $00, $0A, $D7, $23, $3C,
    $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $08, $80,
    $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $03, $80, $00, $00, $FF, $80, $02, $00, $E4, $A0, $00, $00, $E4, $B0, $04, $00, $00, $04, $01, $00, $03, $80, $00, $00, $FF, $80, $01, $00, $E4, $A0,
    $00, $00, $E4, $B0, $04, $00, $00, $04, $02, $00, $03, $80, $00, $00, $FF, $80, $02, $00, $1B, $A0, $00, $00, $E4, $B0, $04, $00, $00, $04, $03, $00, $03, $80, $00, $00, $FF, $80, $03, $00, $E4, $A0,
    $00, $00, $E4, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
    $04, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80,
    $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $02, $80, $04, $00, $00, $80, $01, $00, $AA, $A0, $01, $00, $00, $80, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $55, $80,
    $02, $00, $00, $03, $00, $00, $01, $80, $02, $00, $00, $80, $00, $00, $00, $80, $02, $00, $00, $03, $00, $00, $01, $80, $03, $00, $00, $80, $00, $00, $00, $80, $02, $00, $00, $03, $04, $00, $07, $80,
    $00, $00, $00, $81, $01, $00, $FF, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $04, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar =
    '!!ARBfp1.0'#13+
    'PARAM c[3] = { program.local[0],'#13+
    '		{ 0.0099999998, 1, -4, 0 },'#13+
    '		{ 0, -1 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'TEMP R2;'#13+
    'MOV R0.x, c[0];'#13+
    'MUL R0.z, R0.x, c[1].x;'#13+
    'MAD R0.xy, R0.z, c[2].yxzw, fragment.texcoord[0];'#13+
    'MAD R1.xy, R0.z, c[2], fragment.texcoord[0];'#13+
    'TEX R2.xw, fragment.texcoord[0], texture[0], 2D;'#13+
    'TEX R0.x, R0, texture[0], 2D;'#13+
    'TEX R1.x, R1, texture[0], 2D;'#13+
    'MAD R0.y, R2.x, c[1].z, R1.x;'#13+
    'ADD R1.y, R0, R0.x;'#13+
    'MAD R0.xy, R0.z, c[1].ywzw, fragment.texcoord[0];'#13+
    'MAD R0.zw, R0.z, c[1].xywy, fragment.texcoord[0].xyxy;'#13+
    'TEX R0.x, R0, texture[0], 2D;'#13+
    'TEX R1.x, R0.zwzw, texture[0], 2D;'#13+
    'ADD R0.x, R1.y, R0;'#13+
    'ADD R0.x, R0, R1;'#13+
    'ADD result.color.xyz, -R0.x, c[1].y;'#13+
    'MOV result.color.w, R2;'#13+
    'END';
  GLSLF: PAnsiChar =
    'vec4 _TMP1;'#13+
    'float _TMP4;'#13+
    'vec2 _c0010;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _color;'#13+
    '    vec4 _complement;'#13+
    '    _TMP4 = PSParam0.x/1.00000000E+002;'#13+
    '    _color = texture2D(texture0, TEX0.xy);'#13+
    '    _complement = -4.00000000E+000*_color;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( 0.0, -1.00000000E+000);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _complement = _complement + _TMP1;'#13+
    '    _complement.y = _complement.x;'#13+
    '    _complement.z = _complement.x;'#13+
    '    _complement.w = _color.w;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( -1.00000000E+000, 0.0);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _complement = _complement + _TMP1;'#13+
    '    _complement.y = _complement.x;'#13+
    '    _complement.z = _complement.x;'#13+
    '    _complement.w = _color.w;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( 1.00000000E+000, 0.0);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _complement = _complement + _TMP1;'#13+
    '    _complement.y = _complement.x;'#13+
    '    _complement.z = _complement.x;'#13+
    '    _complement.w = _color.w;'#13+
    '    _c0010 = TEX0.xy + _TMP4*vec2( 0.0, 1.00000000E+000);'#13+
    '    _TMP1 = texture2D(texture0, _c0010);'#13+
    '    _complement = _complement + _TMP1;'#13+
    '    _complement.y = _complement.x;'#13+
    '    _complement.z = _complement.x;'#13+
    '    _complement.w = _color.w;'#13+
    '    _complement.xyz = 1.00000000E+000 - _complement.xyz;'#13+
    '    gl_FragColor = _complement;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TPencilStrokeFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('PencilStroke', 'An pencil stroke effect.', [
    FilterValueRec('BrushSize', 'The brush size of the sketch effect.', TShaderValueType.vtFloat, 5, 1, 19)
  ]);
end;

{ TGlowFilter }

constructor TGlowFilter.Create;
const
  DX9PS2BIN: array [0..1327] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2B, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $83, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $7C, $00, $00, $00, 
    $44, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $6C, $00, $00, $00, $00, $00, $00, $00, 
    $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6E, $70, $75, $74, $54, $65, $78, $74, $75, $72, $65, $00, $AB, $AB, $AB, 
    $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, 
    $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, 
    $93, $77, $0D, $3D, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $2C, $71, $C6, $3D, 
    $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05, $04, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, 
    $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, 
    $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, 
    $01, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, 
    $02, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $AA, $A0, $00, $00, $00, $B0, 
    $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, 
    $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, 
    $06, $00, $11, $80, $00, $00, $AA, $81, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, 
    $08, $00, $11, $80, $00, $00, $AA, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, 
    $00, $00, $01, $80, $09, $00, $FF, $80, $01, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $02, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $04, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $06, $00, $FF, $80, $03, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $08, $00, $FF, $80, $03, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A1, 
    $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, 
    $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, 
    $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0, 
    $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $12, $80, 
    $00, $00, $55, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $01, $00, $AA, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $08, $80, $06, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $00, $00, $07, $80, $03, $00, $AA, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, 
    $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar = 
    '!!ARBfp1.0'#13+
    'PARAM c[8] = { program.local[0..1],'#13+
    '		{ 0, -7, 0.034537863, -6 },'#13+
    '		{ 0.048116904, -5, 0.059957355, -4 },'#13+
    '		{ 0.071781985, -3, 0.082568936, -2 },'#13+
    '		{ 0.091252789, 0.096895546, 0.09885297, 2 },'#13+
    '		{ 3, 4, 5, 6 },'#13+
    '		{ 7 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'RCP R0.x, c[0].x;'#13+
    'MAD_SAT R0.z, R0.x, c[2].w, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MUL R0.y, R0.w, c[3].x;'#13+
    'MAD_SAT R0.z, R0.x, c[2].y, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[2].z, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[3].y, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].z, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[3].w, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].x, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[4].y, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].z, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[4].w, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].x, R0;'#13+
    'ADD_SAT R0.z, fragment.texcoord[0].x, -R0.x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'ADD_SAT R0.z, fragment.texcoord[0].x, R0.x;'#13+
    'MAD R0.y, R0.w, c[5], R0;'#13+
    'MOV_SAT R1.xy, fragment.texcoord[0];'#13+
    'TEX R0.w, R1, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].z, R0;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5], R0;'#13+
    'MAD_SAT R0.z, R0.x, c[5].w, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].x, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6].x, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].z, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6].y, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].x, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6], fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].z, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6].w, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.z, R0.w, c[3].x, R0.y;'#13+
    'MOV_SAT R0.y, fragment.texcoord[0];'#13+
    'MAD_SAT R0.x, R0, c[7], fragment.texcoord[0];'#13+
    'TEX R0.w, R0, texture[0], 2D;'#13+
    'MOV result.color.xyz, c[2].x;'#13+
    'MAD result.color.w, R0, c[2].z, R0.z;'#13+
    'END';
  GLSLF: PAnsiChar = 
    'vec4 _ret_0;'#13+
    'vec4 _TMP1;'#13+
    'vec2 _TMP2;'#13+
    'vec2 _TMP7;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'void main()'#13+
    '{'#13+
    '    float _a;'#13+
    '    vec2 _newCoord;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -7.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _TMP1.w*3.45378630E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -6.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -5.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -4.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -3.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -2.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -1.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 0.0/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.88529697E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 1.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 2.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 3.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 4.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 5.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 6.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 7.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*3.45378630E-002;'#13+
    '    _ret_0 = vec4(0.0, 0.0, 0.0, _a);'#13+
    '    gl_FragColor = _ret_0;'#13+
    '    return;'#13+
    '} ';
  DX9PS2BIN2: array [0..1379] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $37, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $B3, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $AC, $00, $00, $00, 
    $58, $00, $00, $00, $02, $00, $01, $00, $01, $00, $06, $00, $64, $00, $00, $00, $00, $00, $00, $00, $74, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $7C, $00, $00, $00, $00, $00, $00, $00, 
    $8C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $9C, $00, $00, $00, $00, $00, $00, $00, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $00, $AB, $AB, $01, $00, $03, $00, $01, $00, $04, $00, 
    $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6E, $70, $75, $74, $54, $65, $78, 
    $74, $75, $72, $65, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, 
    $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $E0, $C0, 
    $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, 
    $04, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, 
    $BE, $E2, $BA, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, 
    $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $01, $00, $11, $80, 
    $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, 
    $02, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, 
    $03, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, 
    $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, 
    $00, $00, $00, $B0, $02, $00, $00, $03, $06, $00, $12, $80, $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $11, $80, 
    $00, $00, $00, $B0, $02, $00, $00, $03, $08, $00, $12, $80, $00, $00, $AA, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, 
    $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, 
    $00, $00, $AA, $80, $05, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A1, 
    $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, 
    $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, 
    $02, $00, $55, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, 
    $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, 
    $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, 
    $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, 
    $02, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, 
    $01, $00, $E4, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP12: PAnsiChar = 
    '!!ARBfp1.0'#13+
    'PARAM c[7] = { program.local[0..1],'#13+
    '		{ -7, 0.034537863, -6, 0.048116904 },'#13+
    '		{ -5, 0.059957355, -4, 0.071781985 },'#13+
    '		{ -3, 0.082568936, -2, 0.091252789 },'#13+
    '		{ 0.096895546, 0.09885297, 2, 3 },'#13+
    '		{ 4, 5, 6, 7 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'RCP R0.x, c[0].x;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[2].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MUL R0.y, R0.w, c[2].w;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[2].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[2], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[3].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[3].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[4].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[4].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'ADD_SAT R0.w, fragment.texcoord[0].y, -R0.x;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].x, R0;'#13+
    'MOV_SAT R1.xy, fragment.texcoord[0];'#13+
    'TEX R0.w, R1, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'ADD_SAT R0.w, fragment.texcoord[0].y, R0.x;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].x, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[5].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[5], fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[6].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[6].y, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[6].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[2].w, R0;'#13+
    'MAD_SAT R0.w, R0.x, c[6], fragment.texcoord[0].y;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.x, R0.w, c[2].y, R0.y;'#13+
    'MUL result.color, R0.x, c[1];'#13+
    'END';
  GLSLF2: PAnsiChar = 
    'vec4 _ret_0;'#13+
    'vec4 _TMP1;'#13+
    'vec2 _TMP2;'#13+
    'vec2 _TMP7;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform vec4 PSParam1;'#13+
    'void main()'#13+
    '{'#13+
    '    float _a;'#13+
    '    vec2 _newCoord;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -7.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _TMP1.w*3.45378630E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -6.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -5.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -4.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -3.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -2.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -1.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 0.0/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.88529697E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 1.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 2.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 3.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 4.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 5.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 6.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 7.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*3.45378630E-002;'#13+
    '    _ret_0 = PSParam1*_a;'#13+
    '    gl_FragColor = _ret_0;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[1] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
  FShaders[2] := ShaderDevice.CreatePixelShader(@DX9PS2BIN2, ARBFP12, GLSLF2);
  FPassCount := 2;
end;

destructor TGlowFilter.Destroy;
begin
  inherited;
end;

procedure TGlowFilter.LoadShader;
const
  Scale = {$IFNDEF FPC} 1.5; {$ELSE} 2.6; {$ENDIF}
var
  C: TAlphaColor;
begin
  ShaderDevice.SetPixelShader(FShaders[FPass]);
  if FPass = 1 then
    ShaderDevice.SetPixelShaderVector(0, Vector3D(FInput.Width / Values['BlurAmount'] / Scale, 0, 0, 0));
  if FPass = 2 then
    ShaderDevice.SetPixelShaderVector(0, Vector3D(FInput.Height / Values['BlurAmount'] / Scale, 0, 0, 0));
  {$IFNDEF FPC}
  c := FValues[1].Value;
  {$ELSE}
  c := Cardinal(FValues[1].Value);
  {$ENDIF}
  ShaderDevice.SetPixelShaderVector(1, Vector3D(TAlphaColorRec(c).r / $FF,
    TAlphaColorRec(c).g / $FF, TAlphaColorRec(c).b / $FF, TAlphaColorRec(c).a / $FF));
end;

class function TGlowFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Glow', 'An effect that add glow.', [
    FilterValueRec('BlurAmount', 'The blur factor.', TShaderValueType.vtFloat, 0.7, 0.01, 10),
    FilterValueRec('Color', 'The glow color.', TShaderValueType.vtColor, $FFFFD700, 0, 0)
  ]);
end;

{ TInnerGlowFilter }

constructor TInnerGlowFilter.Create;
const
  DX9PS2BIN: array [0..1343] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $2B, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $83, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $7C, $00, $00, $00, 
    $44, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $6C, $00, $00, $00, $00, $00, $00, $00, 
    $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6E, $70, $75, $74, $54, $65, $78, $74, $75, $72, $65, $00, $AB, $AB, $AB, 
    $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, 
    $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, 
    $93, $77, $0D, $3D, $51, $00, $00, $05, $02, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $80, $3F, $00, $00, $00, $00, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0, 
    $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, 
    $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, 
    $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, 
    $01, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, 
    $03, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $00, $B0, 
    $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, 
    $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, 
    $06, $00, $11, $80, $00, $00, $AA, $81, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, 
    $08, $00, $11, $80, $00, $00, $AA, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, 
    $00, $00, $01, $80, $09, $00, $FF, $80, $01, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $04, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $06, $00, $FF, $80, $02, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $08, $00, $FF, $80, $02, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A1, 
    $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, 
    $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, 
    $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0, 
    $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $12, $80, 
    $00, $00, $55, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $01, $00, $AA, $A0, $00, $00, $00, $80, 
    $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $00, $81, $02, $00, $AA, $A0, $01, $00, $00, $02, 
    $00, $00, $07, $80, $02, $00, $FF, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar = 
    '!!ARBfp1.0'#13+
    'PARAM c[8] = { program.local[0..1],'#13+
    '		{ 0, 1, -7, 0.034537863 },'#13+
    '		{ -6, 0.048116904, -5, 0.059957355 },'#13+
    '		{ -4, 0.071781985, -3, 0.082568936 },'#13+
    '		{ -2, 0.091252789, 0.096895546, 0.09885297 },'#13+
    '		{ 2, 3, 4, 5 },'#13+
    '		{ 6, 7 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'RCP R0.x, c[0].x;'#13+
    'MAD_SAT R0.z, R0.x, c[3].x, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MUL R0.y, R0.w, c[3];'#13+
    'MAD_SAT R0.z, R0.x, c[2], fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[2].w, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[3], fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].w, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[4].x, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4], R0;'#13+
    'MAD_SAT R0.z, R0.x, c[4], fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].w, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[5].x, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5], R0;'#13+
    'ADD_SAT R0.z, fragment.texcoord[0].x, -R0.x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'ADD_SAT R0.z, fragment.texcoord[0].x, R0.x;'#13+
    'MAD R0.y, R0.w, c[5].z, R0;'#13+
    'MOV_SAT R1.xy, fragment.texcoord[0];'#13+
    'TEX R0.w, R1, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].w, R0;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].z, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6].x, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5], R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6].y, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].w, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6], fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4], R0;'#13+
    'MAD_SAT R0.z, R0.x, c[6].w, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].w, R0;'#13+
    'MAD_SAT R0.z, R0.x, c[7].x, fragment.texcoord[0].x;'#13+
    'MOV_SAT R0.w, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.z, R0.w, c[3].y, R0.y;'#13+
    'MAD_SAT R0.x, R0, c[7].y, fragment.texcoord[0];'#13+
    'MOV_SAT R0.y, fragment.texcoord[0];'#13+
    'TEX R0.w, R0, texture[0], 2D;'#13+
    'MAD R0.x, R0.w, c[2].w, R0.z;'#13+
    'MOV result.color.xyz, c[2].x;'#13+
    'ADD result.color.w, -R0.x, c[2].y;'#13+
    'END';
  GLSLF: PAnsiChar = 
    'vec4 _ret_0;'#13+
    'vec4 _TMP1;'#13+
    'vec2 _TMP2;'#13+
    'vec2 _TMP7;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'void main()'#13+
    '{'#13+
    '    float _a;'#13+
    '    vec2 _newCoord;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -7.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _TMP1.w*3.45378630E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -6.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -5.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -4.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -3.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -2.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + -1.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 0.0/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.88529697E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 1.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 2.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 3.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 4.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 5.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 6.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.x = TEX0.x + 7.00000000E+000/PSParam0.x;'#13+
    '    _TMP2 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP7 = max(vec2( 0.0, 0.0), _TMP2);'#13+
    '    _TMP1 = texture2D(texture0, _TMP7);'#13+
    '    _a = _a + _TMP1.w*3.45378630E-002;'#13+
    '    _ret_0 = vec4(0.0, 0.0, 0.0, 1.00000000E+000 - _a);'#13+
    '    gl_FragColor = _ret_0;'#13+
    '    return;'#13+
    '} ';
  DX9PS2BIN2: array [0..1459] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $40, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $D7, $00, $00, $00, $00, $02, $FF, $FF, $04, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $D0, $00, $00, $00, 
    $6C, $00, $00, $00, $02, $00, $01, $00, $01, $00, $06, $00, $78, $00, $00, $00, $00, $00, $00, $00, $88, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $90, $00, $00, $00, $00, $00, $00, $00, 
    $A0, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $B0, $00, $00, $00, $00, $00, $00, $00, $C0, $00, $00, $00, $03, $00, $01, $00, $01, $00, $06, $00, $B0, $00, $00, $00, $00, $00, $00, $00, 
    $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $00, $AB, $AB, $01, $00, $03, $00, $01, $00, $04, $00, $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, 
    $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $69, $6E, $70, $75, $74, $54, $65, $78, $74, $75, $72, $65, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, 
    $00, $00, $00, $00, $6F, $72, $69, $67, $69, $6E, $61, $6C, $54, $65, $78, $74, $75, $72, $65, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, 
    $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, 
    $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, 
    $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, 
    $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90, $01, $08, $0F, $A0, $01, $00, $00, $02, 
    $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A0, $00, $00, $55, $B0, 
    $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, 
    $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, 
    $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, 
    $05, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A0, $00, $00, $55, $B0, 
    $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $06, $00, $12, $80, $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, 
    $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $08, $00, $12, $80, $00, $00, $AA, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, 
    $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, 
    $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, 
    $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, 
    $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, 
    $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, 
    $00, $00, $AA, $80, $05, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, 
    $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, 
    $05, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, 
    $05, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, 
    $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, 
    $07, $00, $0F, $80, $00, $00, $E4, $B0, $01, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $02, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $02, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, 
    $06, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $07, $00, $FF, $80, 
    $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP12: PAnsiChar = 
    '!!ARBfp1.0'#13+
    'PARAM c[7] = { program.local[0..1],'#13+
    '		{ -7, 0.034537863, -6, 0.048116904 },'#13+
    '		{ -5, 0.059957355, -4, 0.071781985 },'#13+
    '		{ -3, 0.082568936, -2, 0.091252789 },'#13+
    '		{ 0.096895546, 0.09885297, 2, 3 },'#13+
    '		{ 4, 5, 6, 7 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'RCP R0.x, c[0].x;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[2].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MUL R0.y, R0.w, c[2].w;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[2].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[2], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[3].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[3].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[4].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[4].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].w, R0;'#13+
    'MOV_SAT R1.xy, fragment.texcoord[0];'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'ADD_SAT R0.w, fragment.texcoord[0].y, -R0.x;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].x, R0;'#13+
    'TEX R0.w, R1, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'ADD_SAT R0.w, fragment.texcoord[0].y, R0.x;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[5].x, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[5].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[5], fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[4], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[6].x, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3].w, R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[6].y, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[3], R0;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'MAD_SAT R0.w, R0.x, c[6].z, fragment.texcoord[0].y;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.y, R0.w, c[2].w, R0;'#13+
    'MAD_SAT R0.w, R0.x, c[6], fragment.texcoord[0].y;'#13+
    'MOV_SAT R0.z, fragment.texcoord[0].x;'#13+
    'TEX R0.w, R0.zwzw, texture[0], 2D;'#13+
    'MAD R0.x, R0.w, c[2].y, R0.y;'#13+
    'TEX R0.w, fragment.texcoord[0], texture[1], 2D;'#13+
    'MUL R1, R0.x, c[1];'#13+
    'MUL result.color, R1, R0.w;'#13+
    'END';
  GLSLF2: PAnsiChar = 
    'vec4 _ret_0;'#13+
    'vec4 _TMP2;'#13+
    'vec4 _TMP1;'#13+
    'vec2 _TMP3;'#13+
    'vec2 _TMP9;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform sampler2D texture1;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform vec4 PSParam1;'#13+
    'void main()'#13+
    '{'#13+
    '    float _a;'#13+
    '    vec2 _newCoord;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -7.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _TMP1.w*3.45378630E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -6.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -5.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -4.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -3.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -2.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + -1.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 0.0/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*9.88529697E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 1.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*9.68955457E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 2.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*9.12527889E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 3.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*8.25689360E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 4.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*7.17819855E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 5.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*5.99573553E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 6.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*4.81169038E-002;'#13+
    '    _newCoord = TEX0.xy;'#13+
    '    _newCoord.y = TEX0.y + 7.00000000E+000/PSParam0.x;'#13+
    '    _TMP3 = min(vec2( 1.00000000E+000, 1.00000000E+000), _newCoord);'#13+
    '    _TMP9 = max(vec2( 0.0, 0.0), _TMP3);'#13+
    '    _TMP1 = texture2D(texture0, _TMP9);'#13+
    '    _a = _a + _TMP1.w*3.45378630E-002;'#13+
    '    _TMP2 = texture2D(texture1, TEX0.xy);'#13+
    '    _ret_0 = PSParam1*_a*_TMP2.w;'#13+
    '    gl_FragColor = _ret_0;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[1] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
  FShaders[2] := ShaderDevice.CreatePixelShader(@DX9PS2BIN2, ARBFP12, GLSLF2);
  FPassCount := 2;
end;

destructor TInnerGlowFilter.Destroy;
begin
  inherited;
end;

procedure TInnerGlowFilter.LoadShader;
const
  Scale = {$IFNDEF FPC} 1.5; {$ELSE} 2.6; {$ENDIF}
var
  C: TAlphaColor;
begin
  ShaderDevice.SetPixelShader(FShaders[FPass]);
  if FPass = 1 then
    ShaderDevice.SetPixelShaderVector(0, Vector3D(FInput.Width / Values['BlurAmount'] / Scale, 0, 0, 0));
  if FPass = 2 then
  begin
    ShaderDevice.SetPixelShaderVector(0, Vector3D(FInput.Height / Values['BlurAmount'] / Scale, 0, 0, 0));
    ShaderDevice.SetTextureUnit(1, FInput);
  end;
  {$IFNDEF FPC}
  c := FValues[1].Value;
  {$ELSE}
  c := Cardinal(FValues[1].Value);
  {$ENDIF}
  ShaderDevice.SetPixelShaderVector(1, Vector3D(TAlphaColorRec(c).r / $FF,
    TAlphaColorRec(c).g / $FF, TAlphaColorRec(c).b / $FF, TAlphaColorRec(c).a / $FF));
end;

class function TInnerGlowFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('InnerGlow', 'An effect that add glow.', [
    FilterValueRec('BlurAmount', 'The blur factor.', TShaderValueType.vtFloat, 0.7, 0.01, 10),
    FilterValueRec('Color', 'The glow color.', TShaderValueType.vtColor, $FFFFD700, 0, 0)
  ]);
end;

{ TReflectionFilter }

constructor TReflectionFilter.Create;
const
  DX9PS2BIN: array [0..375] of byte = (
    $00, $02, $FF, $FF, $FE, $FF, $30, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $97, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $90, $00, $00, $00, 
    $58, $00, $00, $00, $02, $00, $01, $00, $01, $00, $06, $00, $60, $00, $00, $00, $00, $00, $00, $00, $70, $00, $00, $00, $02, $00, $00, $00, $01, $00, $02, $00, $60, $00, $00, $00, $00, $00, $00, $00, 
    $78, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $80, $00, $00, $00, $00, $00, $00, $00, $4C, $65, $6E, $67, $74, $68, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, 
    $00, $00, $00, $00, $4F, $70, $61, $63, $69, $74, $79, $00, $69, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, 
    $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, 
    $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $BF, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, 
    $00, $00, $00, $90, $00, $08, $0F, $A0, $06, $00, $00, $02, $00, $00, $08, $80, $01, $00, $00, $A0, $05, $00, $00, $03, $00, $00, $11, $80, $00, $00, $FF, $80, $00, $00, $55, $B0, $02, $00, $00, $03, 
    $00, $00, $01, $80, $00, $00, $00, $81, $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $01, $00, $03, $80, $00, $00, $E4, $B0, 
    $02, $00, $C9, $A0, $02, $00, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $80, 
    $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00
  );
  ARBFP1: PAnsiChar = 
    '!!ARBfp1.0'#13+
    'PARAM c[3] = { program.local[0..1],'#13+
    '		{ 1 } };'#13+
    'TEMP R0;'#13+
    'TEMP R1;'#13+
    'RCP R0.x, c[1].x;'#13+
    'MUL_SAT R0.x, fragment.texcoord[0].y, R0;'#13+
    'ADD R0.x, -R0, c[2];'#13+
    'MUL R1.x, R0, c[0];'#13+
    'ADD R0.y, -fragment.texcoord[0], c[2].x;'#13+
    'MOV R0.x, fragment.texcoord[0];'#13+
    'TEX R0, R0, texture[0], 2D;'#13+
    'MUL result.color, R0, R1.x;'#13+
    'END';
  GLSLF: PAnsiChar = 
    'float _TMP1;'#13+
    'vec2 _c0006;'#13+
    'float _TMP7;'#13+
    'float _x0008;'#13+
    'varying vec4 TEX0;'#13+
    'uniform sampler2D texture0;'#13+
    'uniform vec4 PSParam0;'#13+
    'uniform vec4 PSParam1;'#13+
    'void main()'#13+
    '{'#13+
    '    vec4 _color;'#13+
    '    _c0006 = vec2(TEX0.x, 1.00000000E+000 - TEX0.y);'#13+
    '    _color = texture2D(texture0, _c0006);'#13+
    '    _x0008 = TEX0.y/PSParam1.x;'#13+
    '    _TMP1 = min(1.00000000E+000, _x0008);'#13+
    '    _TMP7 = max(0.0, _TMP1);'#13+
    '    _color = _color*vec4((1.00000000E+000 - _TMP7)*PSParam0.x, (1.00000000E+000 - _TMP7)*PSParam0.x, (1.00000000E+000 - _TMP7)*PSParam0.x, (1.00000000E+000 - _TMP7)*PSParam0.x);'#13+
    '    gl_FragColor = _color;'#13+
    '    return;'#13+
    '} ';
begin
  inherited;
  FShaders[FPassCount] := ShaderDevice.CreatePixelShader(@DX9PS2BIN, ARBFP1, GLSLF);
end;

class function TReflectionFilter.FilterAttr: TFilterRec;
begin
  Result := FilterRec('Reflection', 'An reflection effect.', [
    FilterValueRec('Opacity', 'The opacity of result image.', TShaderValueType.vtFloat, 1, 0, 1),
    FilterValueRec('Length', 'The length of reflection.', TShaderValueType.vtFloat, 0.5, 0, 1)
  ]);
end;

initialization
  RegisterFilter('Style', TPixelateFilter);
  RegisterFilter('Style', TEmbossFilter);
  RegisterFilter('Style', TSharpenFilter);
  RegisterFilter('Style', TToonFilter);
  RegisterFilter('Style', TSepiaFilter);
  RegisterFilter('Style', TPaperSketchFilter);
  RegisterFilter('Style', TPencilStrokeFilter);
  RegisterFilter('Style', TGlowFilter);
  RegisterFilter('Style', TInnerGlowFilter);
  RegisterFilter('Style', TReflectionFilter);
end.