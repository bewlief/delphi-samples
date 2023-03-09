
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uShaders;

interface
uses
 FMX.Types3D, MV.Utils, IOUtils;


type
  TPixelMode = (pmNoTexture, pmDiffuse, pmNormal, pmSpecular, pmAmbient, pmEffects, pmComposition);
const
  TPixelModeNames: array [TPixelMode] of String =
  ('notexture','diffuse', 'normal', 'specular',  'ambient', 'effects', 'composition' );

type
  TPixelModeList = array [TPixelMode] of TContextShader;

  TShaders = class
  private
    FContext: TContext3D;
    FPixelMode: TPixelModeList;
    FVertexMode: TContextShader;
  public
    property PixelMode: TPixelModeList read FPixelMode;
    property VertexMode: TContextShader read FVertexMode;
    constructor Create(const AContext: TContext3D);
    destructor Destroy; override;
  end;

implementation


constructor TShaders.Create(const AContext: TContext3D);
var
  LMode: TPixelMode;
  LFileName: String;
//  LFileBytes: pointer;
begin
  FContext := AContext;

  // load shaders
  for LMode := Low(TPixelMode) to High(TPixelMode) do
  begin
    LFileName := SHADERS_PATH + TPixelModeNames[LMode] + '.ps.fxo';
//    LFileBytes := TFile.ReadAllBytes(LFileName);
    FPixelMode[LMode] := AContext.CreatePixelShader(TFile.ReadAllBytes(LFileName), nil, nil)
  end;

  FVertexMode := AContext.CreateVertexShader(
    TFile.ReadAllBytes(SHADERS_PATH + 'composition.vs.fxo'), nil);
end;


destructor TShaders.Destroy;
var
  LMode: TPixelMode;
begin
  for LMode := Low(TPixelMode) to High(TPixelMode) do
    FContext.DestroyPixelShader(FPixelMode[LMode]);

  FContext.DestroyVertexShader(FVertexMode);
end;

end.
