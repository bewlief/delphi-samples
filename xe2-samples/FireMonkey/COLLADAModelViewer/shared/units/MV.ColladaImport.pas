
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MV.ColladaImport;

interface

uses
  SysUtils, Classes, Types, FMX.Types3D, FMX.Types, collada_schema_1_5,
  MV.Utils, MV.TgaBitmap, System.UITypes;

{.$DEFINE CONSOLE_DEBUG}

type
  TVertexSource = record
  private
    Id: String;
    Data: TSingleDynArray;
    function GetPoint3D(const AIndex: Integer): TPoint3D;
    function GetPoint2D(const AIndex: Integer): TPointF;
  end;

  TImage = class
  private
    FId: String;
    FBitmap: TBitmap;
  public
    destructor Destroy(); override;
  end;

  TMap = record
    Image: TImage;
  end;

  TPhongMapMode  = (pmEmission, pmAmbient, pmDiffuse, pmSpecular,
  pmShininess, pmReflective, pmReflectivity, pmTransparent, mpTransparency);

  TEffect = class
  private
    FId: String;
    FMaps: array [TPhongMapMode] of TMap;
  end;

  TMaterial = class
  private
    FId: String;
    FEffect: TEffect;
    procedure Bind(const AContext: TContext3D);
  public
    destructor Destroy(); override;
  end;

  TMeshBuffer = record
  private
    Material: TMaterial;
    VetexData: TVertexBuffer;
    IndexData: TIndexBuffer;
    MaterialName: String;
    procedure GenerateTangentSpace();
    procedure Render(const AContext: TContext3D);
  end;

  TController = class;
  TMesh = class
  private
    FId : String;
    FSubMesh: TArray<TMeshBuffer>;
    procedure LoadPolygons(const AMesh: IXMLMesh_type; const ASources: TArray<TVertexSource>; const AController: TController);
    procedure LoadTriangles(const AMesh: IXMLMesh_type; const ASources: TArray<TVertexSource>; const AController: TController);
    procedure Render(const AContext: TContext3D);
    function GetTriangleCount: Integer;
    function GetVertexCount: Integer;
    function GetName: String;
  public
    property TriangleCount: Integer read  GetTriangleCount;
    property VertexCount: Integer read  GetVertexCount;
    property Name: String read GetName;
    destructor Destroy(); override;
  end;

  TVisualNode = class
  private
    FTransformation: TMatrix3D;
    FMesh : TMesh;
//    FController : TController;
    procedure Render(const AContext: TContext3D);
  end;

  TVisualScene = class
  private
    FNodes: TArray<TVisualNode>;
    procedure Render(const AContext: TContext3D);
  public
    destructor Destroy(); override;
  end;

  TNodeBone = class
  private
//    FAnimations: TMatrix3D;
  end;

  TControllerBone = record
    FId : String;
    FPose: TMatrix3D;
    FNode: TNodeBone;
  end;

  TSkinVerticle = array of record
    FBoneId: Integer;
    FWeight: Single;
  end;

  TController = class
  private
    FId : String;
    FMeshName : String;
    FTransformation: TMatrix3D;
    FMesh : TMesh;
    FBones: TArray<TControllerBone>;
    FVerticles : TArray<TSkinVerticle>;
    procedure Render(const AContext: TContext3D);
    procedure OptimizeWeights(const AMaxCount: Byte);
  end;


  TModel = class
  private
    FFileName: String;
    FImageList: TArray<TImage>;
    FMeshList: TArray<TMesh>;
    FMaterialList: TArray<TMaterial>;
    FEffectList: TArray<TEffect>;
    FSceneList: TArray<TVisualScene>;
    FControllerList: TArray<TController>;
    function GetMesh(const AIndex:Integer): TMesh; overload;
    function GetMeshCount(): Integer;
    function FindMaterialById(const AName:String): TMaterial;
    function FindEffectById(const AName:String): TEffect;
    function FindImageById(const AName:String): TImage;
    function FindMeshById(const AName:String): TMesh;
    function FindControllerByMeshId(const AName:String): TController;
    function FindControllerById(const AName:String): TController;
    function GetMesh(const AGeometry: IXMLGeometry_type): TMesh;  overload;
    function GetImage(const AImage: IXMLImage_type): TImage;
    function GetMaterial(const AMaterial: IXMLMaterial_type): TMaterial;
    function GetEffect(const AEffect: IXMLEffect_type): TEffect;
    function GetScene(const AScene: IXMLVisual_scene_type): TVisualScene;
    function GetNode(const ANode: IXMLNode_type): TVisualNode;
    function GetController(const AController: IXMLController_type): TController;
    function ImportFromCot(const AMap:IXMLFx_common_color_or_texture_type):TMap;
    procedure ImportFromGeometryLib(const AGeometryLib: IXMLLibrary_geometries_type);
    procedure ImportFromImageLib(const AImageLib: IXMLLibrary_images_type);
    procedure ImportFromMaterialLib(const AMaterialLib: IXMLLibrary_materials_type);
    procedure ImportFromEffectLib(const AEffectLib: IXMLLibrary_effects_type);
    procedure ImportFromSceneLib(const ASceneLib: IXMLLibrary_visual_scenes_type);
    procedure ImportFromControllerLib(const AControllerLib: IXMLLibrary_controllers_type);
  public
    property MeshList[const Index: Integer]: TMesh read GetMesh;
    property MeshCount:Integer read GetMeshCount;

    procedure Render(const AContext: TContext3D);
    constructor CreateFromFile(const AFileName: String);
    destructor Destroy(); override;
  end;

var
 DefaultDiffuse:   TBitmap;
 DefaultNormal:  TBitmap;

implementation

type
  TSimpleVertex = record
    Pos: TPoint3D;
    Tex: TPointF;
    Nor: TPoint3D;
    Tan: TPoint3D;
    Bon: TVector3D;
    Use: Word;
  end;

  TSimpleVertexArray = TArray<TSimpleVertex>;

function TMesh.GetName: String;
begin
  if length(self.FId) > 5 then
    Result := Copy(self.FId, 2, length(self.FId)-5);
end;

function TMesh.GetTriangleCount: Integer;
var
  i, LCount :Integer;
begin
  LCount := 0;
  for i := 0 to High(FSubMesh) do
    Inc(LCount, FSubMesh[i].IndexData.Length);

  Result :=  LCount div 3;
end;

function TMesh.GetVertexCount: Integer;
var
  i ,count :Integer;
begin
  count := 0;
  for i := 0 to High(FSubMesh) do
    Inc(count, FSubMesh[i].VetexData.Length);
  Result :=  count;
end;

procedure TMeshBuffer.GenerateTangentSpace();
var
  j, i: Integer;
  v1,v2,v3: Integer;
  FTangentList: TArray<TVector3D>;
  FSmthTangentList: TArray<TVector3D>;
  LTangent, LNormal, LBinormal: TVector3D;
  sdir: TVector3D;
  x1, x2, y1, y2, z1, z2, s1, s2, t1, t2, r: Single;
  v1Pos, v2Pos, v3Pos: TPoint3D;
  v1Tex0, v2Tex0, v3Tex0: TPointf;
  v1Tangent, v2Tangent, v3Tangent: TVector3D;

  LTangentColor: TColorRec;
  LDenum:Single;
begin

  // clean table for tangents
  SetLength(FTangentList, VetexData.Length);
  for i := 0 to VetexData.Length - 1 do
    FTangentList[i] := NullVector3D;

  // calculate tangent space based on texture coordinates
  for i := 0 to (IndexData.Length div 3) - 1 do
  begin
    v1 := IndexData[i*3];
    v2 := IndexData[i*3+1];
    v3 := IndexData[i*3+2];

    v1Pos := VetexData.Vertices[v1];
    v2Pos := VetexData.Vertices[v2];
    v3Pos := VetexData.Vertices[v3];

    v1Tex0 := VetexData.TexCoord0[v1];
    v2Tex0 := VetexData.TexCoord0[v2];
    v3Tex0 := VetexData.TexCoord0[v3];

    v1Tangent := FTangentList[v1];
    v2Tangent := FTangentList[v2];
    v3Tangent := FTangentList[v3];

    x1 := v2Pos.x - v1Pos.x;
    x2 := v3Pos.x - v1Pos.x;
    y1 := v2Pos.Y - v1Pos.Y;
    y2 := v3Pos.Y - v1Pos.Y;
    z1 := v2Pos.Z - v1Pos.Z;
    z2 := v3Pos.Z - v1Pos.Z;

    s1 := v2Tex0.x - v1Tex0.x;
    s2 := v3Tex0.x - v1Tex0.x;
    t1 := v2Tex0.Y - v1Tex0.Y;
    t2 := v3Tex0.Y - v1Tex0.Y;

    LDenum := (s1 * t2 - s2 * t1);

    if LDenum = 0 then
      LDenum := 1.0;

    r := 1.0 / LDenum;
    sdir := Vector3D((t2 * x1 - t1 * x2) * r, (t2 * y1 - t1 * y2) * r, (t2 * z1 - t1 * z2) * r);

    FTangentList[v1] := Vector3DAdd(sdir , v1Tangent);
    FTangentList[v2] := Vector3DAdd(sdir , v2Tangent);
    FTangentList[v3] := Vector3DAdd(sdir , v3Tangent);
  end;


  // make smooth tangent for separated vertex with teh same normal
  SetLength(FSmthTangentList, VetexData.Length);
  for i := 0 to VetexData.Length - 1 do
  begin
    FSmthTangentList[i] := NullVector3D;
    for j := 0 to VetexData.Length - 1 do
    begin
      if (PointDistance2(VetexData.Vertices[i], VetexData.Vertices[j]) < 0.0001) and
        (PointDistance2(VetexData.Normals[i], VetexData.Normals[j]) < 0.0001) then
        begin
           FSmthTangentList[i] := Vector3DAdd(FSmthTangentList[i], FTangentList[j]);
        end;
    end;
  end;

  for i := 0 to VetexData.Length - 1 do
  begin

    LTangent := FSmthTangentList[i];
    NormalizeVector3D(LTangent);

    LTangent.W :=  FTangentList[i].W ;
    LNormal := Vector3D(
      VetexData.Normals[i].x,
      VetexData.Normals[i].y,
      VetexData.Normals[i].z, 0.0);

    LBinormal := Vector3DCrossProduct(LTangent, LNormal);
    LTangent := Vector3DCrossProduct(LNormal,LBinormal);

    // tangen vector compresion to integer
    LTangentColor.r := round((LTangent.X + 1.0) * 127);
    LTangentColor.g := round((LTangent.Y + 1.0) * 127);
    LTangentColor.b := round((LTangent.Z + 1.0) * 127);
    LTangentColor.a := round((LTangent.W + 1.0) * 127);

    VetexData.Specular[i] := LTangentColor.Color;
  end;
end;

function GetOptimizedVertexArrayData(const APolygons: TSimpleVertexArray)
  : TMeshBuffer;
var
  v, f: Integer;
  LNum: Integer;
  LVetexData: TVertexBuffer;
  LIndexData: TIndexBuffer;
  LFormats: TVertexFormats;
begin
  LIndexData := TIndexBuffer.Create(Length(APolygons));
  LNum := 0;
  for v := 0 to Length(APolygons) - 1 do
  begin
    APolygons[v].Use := v;
    for f := 0 to LNum - 1 do
      if (PointDistance2(APolygons[v].Pos, APolygons[f].Pos) < 0.00001) and
        (PointDistance2(APolygons[v].Nor, APolygons[f].Nor) < 0.00001) and
        (PointDistance2(APolygons[v].Tex, APolygons[f].Tex) < 0.00001) then
      begin
        APolygons[v].Use := f;
        LIndexData[v] := f;
        break;
      end;

    if (APolygons[v].Use = v) then
    begin
      APolygons[LNum] := APolygons[v];
      APolygons[LNum].Use := LNum;
      APolygons[v].Use := LNum;
      LIndexData[v] := LNum;
      Inc(LNum);
    end;
  end;
  LFormats := [
    TVertexFormat.vfVertex,
    TVertexFormat.vfNormal,
    TVertexFormat.vfTexCoord0,
    TVertexFormat.vfSpecular
  ];
  LVetexData := TVertexBuffer.Create(LFormats, LNum);

  for v := 0 to LNum - 1 do
  begin
    LVetexData.Vertices[v] := APolygons[v].Pos;
    LVetexData.Normals[v] := APolygons[v].Nor;
    LVetexData.TexCoord0[v] := APolygons[v].Tex;
  end;

  Result.VetexData := LVetexData;
  Result.IndexData := LIndexData;
  Result.GenerateTangentSpace;
end;

type
  TSourceMode = (smVertex, smNormal, smTexCoord);

  TSourceSemantic = record
    FMode: TSourceMode;
    FSource: TVertexSource;
    FOffset: Integer;
  end;

  TSourceSemanticList = array [TSourceMode] of TSourceSemantic;

function TVertexSource.GetPoint3D(const AIndex: Integer): TPoint3D;
var
  Id: Integer;
begin
  Id := AIndex * 3;
  Result.x := Data[Id];
  Result.y := Data[Id + 1];
  Result.Z := Data[Id + 2];
end;

function TVertexSource.GetPoint2D(const AIndex: Integer): TPointF;
var
  Id: Integer;
begin
  Id := AIndex * 2;
  Result.x := Data[Id];
  Result.y := Data[Id + 1];
end;

function GetSemantic(const Input: IXMLInput_local_offset_typeList;
  const ASources: TArray<TVertexSource>): TArray<TSourceSemantic>;
var
  j: Integer;
  function FindSource(ASourceName: String): TVertexSource;
  var
    i: Integer;
  begin
    for i := 0 to High(ASources) do
    begin
      if SameText(ASourceName, '#' + ASources[i].Id) then
      begin
        Result := ASources[i];
        exit;
      end;
    end;
    Result := ASources[0]; // if not found Result POSITION attribute
  end;

begin
  SetLength(Result, Input.Count);
  for j := 0 to Input.Count - 1 do
  begin
    Result[j].FOffset := Input[j].Offset;
    Result[j].FSource := FindSource(Input[j].Source);
    if SameText(Input[j].Semantic, 'VERTEX') then
      Result[j].FMode := smVertex
    else if SameText(Input[j].Semantic, 'NORMAL') then
      Result[j].FMode := smNormal
    else if SameText(Input[j].Semantic, 'TEXCOORD') then
      Result[j].FMode := smTexCoord;
  end;
end;

procedure TMesh.LoadPolygons(const AMesh: IXMLMesh_type;
  const ASources: TArray<TVertexSource>; const AController: TController);
var
  i, j, k, l, id: Integer;
  LList: TSimpleVertexArray;
  LSubMesh: IXMLPolygons_type;
  LPolygon: TIntegerDynArray;
  LSemantics: TArray<TSourceSemantic>;
  LSemantic: TSourceSemantic;
  LVertexId:Integer;
begin
  if AMesh.Polygons.Count > 0 then
  SetLength(FSubMesh, AMesh.Polygons.Count);
  for i := 0 to AMesh.Polygons.Count - 1 do
  begin

    LSubMesh := AMesh.Polygons[i];
    LSemantics := GetSemantic(LSubMesh.Input, ASources);

    SetLength(LList, LSubMesh.Count * 3);
    for j := 0 to LSubMesh.P.Count - 1 do
    begin
      LPolygon := IntStringsToIntegerDynArray(LSubMesh.P[j].GetText);

      for k := 0 to High(LSemantics) do
      begin
        LSemantic := LSemantics[k];

        case LSemantic.FMode of
          smVertex:
            begin
              for l := 0 to 2 do
              begin
                LVertexId := LPolygon[LSemantic.FOffset + l * 3];
                id := j * 3 + l;
                LList[id].Pos := LSemantic.FSource.GetPoint3D(LVertexId);

                if Assigned(AController) then
                begin
                  LList[id].Bon  :=
                    Vector3D(
                    AController.FVerticles[LVertexId][0].FWeight,
                    AController.FVerticles[LVertexId][1].FWeight,
                    AController.FVerticles[LVertexId][0].FBoneId,
                    AController.FVerticles[LVertexId][1].FBoneId);

                end;
              end;


            end;
          smTexCoord:
            begin
              for l := 0 to 2 do
              begin
                LList[j * 3 + l].Tex := LSemantic.FSource.GetPoint2D
                  (LPolygon[LSemantic.FOffset + l * 3]);
                  LList[j * 3 + l].Tex.Y := 1-LList[j * 3 + l].Tex.Y;
              end;
            end;
          smNormal:
            begin
              for l := 0 to 2 do
              begin
                LList[j * 3 + l].Nor := LSemantic.FSource.GetPoint3D
                  (LPolygon[LSemantic.FOffset + l * 3]);
              end;
            end;
        end;
      end;
    end;
    FSubMesh[i] := GetOptimizedVertexArrayData(LList);
    FSubMesh[i].MaterialName := LSubMesh.Material;
  end;
end;

procedure TMesh.LoadTriangles(const AMesh: IXMLMesh_type;
  const ASources: TArray<TVertexSource>; const  AController: TController);
var
  i, j, k, l, LVertexId, id: Integer;
  LList: TSimpleVertexArray;
  LSubMesh: IXMLTriangles_type;
  LPolygon: TIntegerDynArray;
  LSemantics: TArray<TSourceSemantic>;
  LSemantic: TSourceSemantic;
  LTriangleCount: Integer;
  LJump: Integer;
begin

  if AMesh.Triangles.Count > 0 then
    SetLength(FSubMesh, AMesh.Triangles.Count);

  for i := 0 to AMesh.Triangles.Count - 1 do
  begin

    LSubMesh := AMesh.Triangles[i];
    LSemantics := GetSemantic(LSubMesh.Input, ASources);

    SetLength(LList, LSubMesh.Count * 3);
    LPolygon := IntStringsToIntegerDynArray(LSubMesh.P.GetText);

    LJump := 0;
    LTriangleCount := Length(LPolygon) div LSubMesh.Count;
    for j := 0 to LSubMesh.Count-1 do
    begin

      for k := 0 to High(LSemantics) do
      begin
        LSemantic := LSemantics[k];

        case LSemantic.FMode of
         smVertex:
            begin
              for l := 0 to 2 do
              begin
                LVertexId := LPolygon[LJump + LSemantic.FOffset + l * 3];
                id := j * 3 + l;
                LList[id].Pos := LSemantic.FSource.GetPoint3D(LVertexId);

                if Assigned(AController) then
                begin
                  LList[id].Bon :=
                    Vector3D(
                    AController.FVerticles[LVertexId][0].FWeight,
                    AController.FVerticles[LVertexId][1].FWeight,
                    AController.FVerticles[LVertexId][0].FBoneId,
                    AController.FVerticles[LVertexId][1].FBoneId);

                end;
              end;
            end;
          smTexCoord:
            begin
              for l := 0 to 2 do
              begin
                LList[j * 3 + l].Tex := LSemantic.FSource.GetPoint2D
                  (LPolygon[LJump + LSemantic.FOffset + l * 3]);
                  LList[j * 3 + l].Tex.Y := 1-LList[j * 3 + l].Tex.Y;
              end;
            end;
          smNormal:
            begin
              for l := 0 to 2 do
              begin
                LList[j * 3 + l].Nor := LSemantic.FSource.GetPoint3D
                  (LPolygon[LJump + LSemantic.FOffset + l * 3]);
              end;
            end;
        end;
      end;
      Inc(LJump, LTriangleCount);
    end;

    FSubMesh[i] := GetOptimizedVertexArrayData(LList);
    FSubMesh[i].MaterialName := LSubMesh.Material;
  end;
end;

function TModel.GetMesh(const AGeometry: IXMLGeometry_type): TMesh;
var
  i : Integer;
  LSource: IXMLSource_type;
  LSources: TArray<TVertexSource>;
  LMesh:TMesh;
  LController: TController;
  MeshType: IXMLMesh_type;
begin
  MeshType:=  AGeometry.Mesh;
  LMesh := TMesh.Create;
  LMesh.FId := '#'+ AGeometry.Id;

  SetLength(LSources, MeshType.Source.Count);

  for i := 0 to MeshType.Source.Count - 1 do
  begin
    LSource := MeshType.Source[i];
    LSources[i].Id := LSource.Id;
    LSources[i].Data := FloatStringsToSingleDynArray(LSource.Float_array.Text);

    Assert(Length(LSources[i].Data) = LSource.Float_array.Count,
      Format('Bad source length: %d <> %d', [Length(LSources[i].Data),
      LSource.Float_array.Count]));
  end;

  LController := FindControllerById(LMesh.FId);

  LMesh.LoadTriangles(MeshType, LSources, LController);
  LMesh.LoadPolygons(MeshType, LSources, LController);

  for i := 0 to High(LMesh.FSubMesh) do
      LMesh.FSubMesh[i].Material := FindMaterialById(LMesh.FSubMesh[i].MaterialName);

{$IFDEF CONSOLE_DEBUG}
  for i := 0 to High(LMesh.FSubMesh) do
  begin
    WriteLn(' Sumbesh: ',i);
    WriteLn('  Triangles: ',LMesh.FSubMesh[i].IndexData.Length div 3);
    WriteLn('  Verticles: ',LMesh.FSubMesh[i].VetexData.Length);
  end;
{$ENDIF}

  Result := LMesh;
end;

function TModel.FindMaterialById(const AName:String): TMaterial;
var
  i: Integer;
begin
  for i := 0 to High(FMaterialList) do
  begin
    Result := FMaterialList[i];
    if SameText(Result.FId, AName) then
      exit;
  end;
  Result := nil;
  Assert(false,'Material id '+AName+' not found');
end;

function TModel.FindEffectById(const AName:String): TEffect;
var
  i: Integer;
begin
  for i := 0 to High(FEffectList) do
  begin
    Result := FEffectList[i];
    if SameText(Result.FId, AName) then
      exit;
  end;
  Result := nil;
  Assert(false,'Effect id '+AName+' not found');
end;

function TModel.FindImageById(const AName:String): TImage;
var
  i: Integer;
begin
  for i := 0 to High(FImageList) do
  begin
    Result := FImageList[i];
    if SameText(Result.FId, AName) then
      exit;
  end;
  Result := nil;

end;

function TModel.FindMeshById(const AName:String): TMesh;
var
  i: Integer;
begin
  for i := 0 to High(FMeshList) do
  begin
    Result := FMeshList[i];
    if SameText(Result.FId, AName) then
      exit;
  end;
  Result := nil;
  Assert(false,'Mesh id '+AName+' not found');
end;

function TModel.FindControllerByMeshId(const AName : String): TController;
var
  i: Integer;
begin
{$IFDEF CONSOLE_DEBUG}
  WriteLn('FindControllerById: ' + AName);
{$ENDIF}
  for i := 0 to High(FControllerList) do
  begin
    Result := FControllerList[i];
    if SameText(Result.FMeshName, AName) then
      exit;
  end;
  Result := nil;
 // Assert(false,'Controller id '+AName+' not found');
end;

function TModel.FindControllerById(const AName : String): TController;
var
  i: Integer;
begin
{$IFDEF CONSOLE_DEBUG}
  WriteLn('FindControllerById: ' + AName);
{$ENDIF}
  for i := 0 to High(FControllerList) do
  begin
    Result := FControllerList[i];
    if SameText(Result.FId, AName) then
      exit;
  end;
  Result := nil;
 // Assert(false,'Controller id '+AName+' not found');
end;

procedure TController.Render(const AContext: TContext3D);
begin
 self.
  FMesh.Render(AContext);
end;

procedure TMeshBuffer.Render(const AContext: TContext3D);
begin
  if Assigned(Material) then
    Material.Bind(AContext);

  AContext.DrawTrianglesList(VetexData, IndexData, 1);
end;

procedure TMesh.Render(const AContext: TContext3D);
var
  i: Integer;
begin
  for i := 0 to High(FSubMesh) do
    FSubMesh[i].Render(AContext);
end;

procedure TModel.Render(const AContext: TContext3D);
var
  i: Integer;
begin
  for i := 0 to High(FSceneList) do
    FSceneList[i].Render(AContext);

  AContext.SetTextureUnit(2, nil);
  AContext.SetTextureUnit(1, nil);
  AContext.SetTextureUnit(0, nil);
end;

procedure TVisualNode.Render(const AContext: TContext3D);
var
  V : TVector3D;
begin
  if Assigned(FMesh) then
  begin
    V := Vector3D(FTransformation.m11, FTransformation.m21, FTransformation.m31, FTransformation.m41);
    AContext. SetVertexShaderVector(4 + 0, V);
    V := Vector3D(FTransformation.m12, FTransformation.m22, FTransformation.m32, FTransformation.m42);
    AContext. SetVertexShaderVector(4 + 1, V);
    V := Vector3D(FTransformation.m13, FTransformation.m23, FTransformation.m33, FTransformation.m43);
    AContext. SetVertexShaderVector(4 + 2, V);
    FMesh.Render(AContext);
  end;
end;

procedure TVisualScene.Render(const AContext: TContext3D);
var
  i: Integer;
begin
  for i := 0 to High(FNodes) do
    FNodes[i].Render(AContext);
end;


procedure TModel.ImportFromGeometryLib(const AGeometryLib
  : IXMLLibrary_geometries_type);
var
  i: Integer;
begin
  SetLength(FMeshList, AGeometryLib.Geometry.Count);
  for i := 0 to AGeometryLib.Geometry.Count - 1 do
  begin
{$IFDEF CONSOLE_DEBUG}
    WriteLn('Mesh: ' + AGeometryLib.Geometry[i].Id);
{$ENDIF}
    FMeshList[i] := GetMesh(AGeometryLib.Geometry[i]);
  end;
end;


function TModel.GetImage(const AImage: IXMLImage_type): TImage;
var
  LFileName, LModelTextureFile : String;
  LLen:Integer;
const
  LFilePred = 'file://';
begin
  Result := TImage.Create();
  Result.FId := AImage.Id;
  // TODO function for cut local file://
  LFilename := AImage.Init_from.Text;
  LLen := Length(LFilePred);
  LFileName := Copy(LFileName, LLen + 1 , Length(LFileName) - LLen);
  LModelTextureFile := ExtractFileDir(FFileName) +'\'+ ExtractFileName(LFileName);
{$IFDEF CONSOLE_DEBUG}
  WriteLn(LModelTextureFile);
{$ENDIF}
  if SameText(ExtractFileExt(LModelTextureFile), '.TGA') then
    Result.FBitmap := TTgaBitmap.CreateFromFile(LModelTextureFile)
  else
    Result.FBitmap := TBitmap.CreateFromFile(LModelTextureFile);
end;

procedure TModel.ImportFromImageLib(const AImageLib
  : IXMLLibrary_images_type);
var
  i: Integer;
begin
  SetLength(FImageList, AImageLib.Image.Count);
  for i := 0 to AImageLib.Image.Count - 1 do
    FImageList[i] := GetImage(AImageLib.Image[i]);
end;

function TModel.GetMaterial(const AMaterial: IXMLMaterial_type): TMaterial;
begin
  Result := TMaterial.Create();
  Result.FId := AMaterial.Id;
  Result.FEffect := FindEffectById(AMaterial.Instance_effect.Url);
end;

procedure TModel.ImportFromMaterialLib(const AMaterialLib: IXMLLibrary_materials_type);
var
  i: Integer;
begin
  SetLength(FMaterialList, AMaterialLib.Material.Count);
  for i := 0 to AMaterialLib.Material.Count - 1 do
    FMaterialList[i] := GetMaterial(AMaterialLib.Material[i]);
end;

function TModel.ImportFromCot(const AMap:IXMLFx_common_color_or_texture_type):TMap;
begin
  Result.Image := FindImageById(AMap.Texture.Texture);
 //   function Get_Color: IXMLFx_common_color_or_texture_type_color;
 //   function Get_Param: IXMLFx_common_color_or_texture_type_param;
 //   function Get_Texture: IXMLFx_common_color_or_texture_type_texture;
end;

function TModel.GetEffect(const AEffect: IXMLEffect_type): TEffect;
var
 LPhong: IXMLProfile_common_type_technique_phong;
begin
  Result := TEffect.Create;
  Result.FId := '#'+AEffect.Id;    // id instance has #
  if SameText(AEffect.Profile_COMMON.Technique.Sid, 'standard') then
  begin
    LPhong := AEffect.Profile_COMMON.Technique.Phong;
 //   Result.FMaps[pmEmission] := ImportFromCot(LPhong.Emission);
    Result.FMaps[pmAmbient] := ImportFromCot(LPhong.Ambient);
    Result.FMaps[pmDiffuse] := ImportFromCot(LPhong.Diffuse);
    Result.FMaps[pmSpecular] := ImportFromCot(LPhong.Specular);

 //   Result.FMaps[pmShininess] := ImportFromCot(LPhong.Shininess);
 //   Result.FMaps[pmReflective] := ImportFromCot(LPhong.Reflective);
 //   Result.FMaps[pmReflectivity] := ImportFromCot(LPhong.Reflectivity);
 //   Result.FMaps[pmTransparent] := ImportFromCot(LPhong.Transparent);
 //   Result.FMaps[mpTransparency] := ImportFromCot(LPhong.Transparency);
  end;
end;

function CreateMatrix3d():TMatrix3d;
begin

end;

function TModel.GetNode(const ANode: IXMLNode_type): TVisualNode;
var
  i : Integer;
  LRot: array [0..2] of Single;
  LArr:TSingleDynArray;
  procedure UpdateMatrix(const AMatrix: TMatrix3D);
  begin
     Result.FTransformation := Matrix3DMultiply(AMatrix, Result.FTransformation);
  end;
begin
  Result := TVisualNode.Create;
  if ANode.Instance_geometry.Count > 0 then
    Result.FMesh := FindMeshById(ANode.Instance_geometry[0].Url)
  else
    Result.FMesh := nil;

  Result.FTransformation := IdentityMatrix3D;

  LRot[0] := 0;
  LRot[1] := 0;
  LRot[2] := 0;
  for i := 0 to ANode.Rotate.Count - 1  do
  begin
    LArr := FloatStringsToSingleDynArray(ANode.Rotate[i].Text);
    LRot[Round(LArr[0]+LArr[1]*2+LArr[2]*3)-1] := LArr[3];
  end;
  UpdateMatrix(CreateRotMatrix3D(LRot[0],LRot[1],LRot[2]));

  for i := 0 to ANode.Translate.Count - 1 do
  begin
    LArr := FloatStringsToSingleDynArray(ANode.Translate[i].Text);
    Result.FTransformation.m41 := LArr[0];
    Result.FTransformation.m42 := LArr[1];
    Result.FTransformation.m43 := LArr[2];
  end;

  for i := 0 to ANode.Scale.Count - 1 do
  begin
    LArr := FloatStringsToSingleDynArray(ANode.Scale[i].Text);
    UpdateMatrix(CreateScaleMatrix3D(Vector3D(LArr[0],LArr[1],LArr[2])));
  end;

  for i := 0 to ANode.Matrix.Count - 1 do
    Result.FTransformation := Matrix3DA(FloatStringsToSingleDynArray(ANode.Matrix[i].Text));

end;

function TModel.GetScene(const AScene: IXMLVisual_scene_type): TVisualScene;
var
  i : Integer;
begin
  Result := TVisualScene.Create;
  SetLength(Result.FNodes, AScene.Node.Count);
  for i := 0 to AScene.Node.Count - 1 do
    Result.FNodes[i] := GetNode(AScene.Node[i]);
end;

procedure TModel.ImportFromEffectLib(const AEffectLib: IXMLLibrary_effects_type);
var
  i: Integer;
begin
  SetLength(FEffectList, AEffectLib.Effect.Count);
  for i := 0 to AEffectLib.Effect.Count - 1 do
    FEffectList[i] := GetEffect(AEffectLib.Effect[i]);
end;

procedure TModel.ImportFromSceneLib(const ASceneLib: IXMLLibrary_visual_scenes_type);
var
  i: Integer;
begin
  SetLength(FSceneList, ASceneLib.Visual_scene.Count);
  for i := 0 to ASceneLib.Visual_scene.Count - 1 do
    FSceneList[i] := GetScene(ASceneLib.Visual_scene[i]);
end;

procedure TController.OptimizeWeights(const AMaxCount: Byte);
begin

end;

function FlofatArrayToMatrixArray(const AArr: TSingleDynArray):TArray<TMatrix3D>;
var
  i , id: Integer;
begin
  SetLength(Result, length(AArr) div 16 );
  for i := 0 to High(Result) do
  begin
    id := i * 16;
    Result[i] := Matrix3D(
      AArr[0+id],AArr[4+id],AArr[8+id],AArr[12+id],
      AArr[1+id],AArr[5+id],AArr[9+id],AArr[13+id],
      AArr[2+id],AArr[6+id],AArr[10+id],AArr[14+id],
      AArr[3+id],AArr[7+id],AArr[11+id],AArr[15+id]);
  end;
end;


function TModel.GetController(const AController: IXMLController_type): TController;
var
  i, j, id : Integer;
  LSkin: IXMLSkin_type;
  LSource: IXMLSource_type;

  LTemp: TSingleDynArray;
  LWeights: TSingleDynArray;
  LMatrices: TArray<TMatrix3D>;
  LJoints: TStringDynArray;

  LCount: TIntegerDynArray;
  LIndex: TIntegerDynArray;
  LBone:TControllerBone;
begin

  LSkin := AController.Skin;
  Result := TController.Create;
  Result.FId := AController.Id;
  Result.FMeshName := LSkin.Source;
  Result.FTransformation := Matrix3DA(FloatStringsToSingleDynArray(LSkin.Bind_shape_matrix));
  Result.FMesh := nil;

  for i := 0 to LSkin.SourceList.Count-1 do
  begin
     LSource := LSkin.SourceList[i];
{$IFDEF CONSOLE_DEBUG}
     WriteLn(AController.Id + '-Joints');
{$ENDIF}
     if Sametext(LSource.Id, AController.Id + '-Joints')  then
     begin
       LJoints := StringsToStringDynArray(LSource.Name_array.Text);
     end else
     if Sametext(LSource.Id, AController.Id + '-Matrices')  then
     begin
       LTemp := FloatStringsToSingleDynArray(LSource.Float_array.Text);
       LMatrices := FlofatArrayToMatrixArray(LTemp);
     end else
     if Sametext(LSource.Id, AController.Id + '-Weights')  then
     begin
       LWeights := FloatStringsToSingleDynArray(LSource.Float_array.Text);
     end;
  end;

{$IFDEF CONSOLE_DEBUG}
  WriteLn('Joints: ', Length(LJoints));
{$ENDIF}
  SetLength(Result.FBones, Length(LJoints));
  for i := 0 to High(LJoints) do
  begin
{$IFDEF CONSOLE_DEBUG}
    WriteLn('  '+LJoints[i]);
{$ENDIF}
    LBone.FId := LJoints[i];
    LBone.FPose := LMatrices[i];
    Result.FBones[i] := LBone;
  end;

  SetLength(Result.FVerticles, LSkin.Vertex_weights.Count);
  LCount := IntStringsToIntegerDynArray(LSkin.Vertex_weights.Vcount);
  LIndex := IntStringsToIntegerDynArray(LSkin.Vertex_weights.V);

  id := 0;
  for i := 0 to High(Result.FVerticles) do
  begin
    Setlength(Result.FVerticles[i], LCount[i]);
    for j := 0 to LCount[i]-1 do
    begin
      Result.FVerticles[i][j].FBoneId := LIndex[id];
      Result.FVerticles[i][j].FWeight := LWeights[LIndex[id+1]];
      Inc(id,2);
    end;
  end;
end;

procedure TModel.ImportFromControllerLib(const AControllerLib: IXMLLibrary_controllers_type);
var
  i: Integer;
begin
  SetLength(FControllerList, AControllerLib.Controller.Count);
  for i := 0 to AControllerLib.Controller.Count - 1 do
    FControllerList[i] := GetController(AControllerLib.Controller[i]);
end;

constructor TModel.CreateFromFile(const AFileName: String);
var
  LModel: IXMLCOLLADA;
begin
  FFileName := AFileName;
  LModel := LoadCOLLADA(AFileName);

  if (LModel.Library_images.Count > 0) then
     ImportFromImageLib(LModel.Library_images[0]);

  if (LModel.Library_effects.Count > 0) then
     ImportFromEffectLib(LModel.Library_effects[0]);

  if (LModel.Library_materials.Count > 0) then
     ImportFromMaterialLib(LModel.Library_materials[0]);

//  if (LModel.Library_controllers.Count > 0) then
//     ImportFromControllerLib(LModel.Library_controllers[0]);

  if (LModel.Library_geometries.Count > 0) then
     ImportFromGeometryLib(LModel.Library_geometries[0]);

  if (LModel.Library_visual_scenes.Count > 0) then
     ImportFromSceneLib(LModel.Library_visual_scenes[0]);

end;

procedure TMaterial.Bind(const AContext: TContext3D);
begin

  if Assigned(FEffect.FMaps[pmDiffuse].Image) then
    AContext.SetTextureUnit(0, FEffect.FMaps[pmDiffuse].Image.FBitmap)
  else
    AContext.SetTextureUnit(0, DefaultDiffuse);

  if Assigned(FEffect.FMaps[pmSpecular].Image) then
    AContext.SetTextureUnit(1, FEffect.FMaps[pmSpecular].Image.FBitmap)
  else
    AContext.SetTextureUnit(1, DefaultNormal);

  if Assigned(FEffect.FMaps[pmAmbient].Image) then
    AContext.SetTextureUnit(2, FEffect.FMaps[pmAmbient].Image.FBitmap)
  else
    AContext.SetTextureUnit(2, nil);
end;

function TModel.GetMeshCount(): Integer;
begin
  Result := Length(FMeshList);
end;

function TModel.GetMesh(const AIndex:Integer): TMesh;
begin
  Result := FMeshList[AIndex];
end;


destructor TVisualScene.Destroy();
var
  i:Integer;
begin
  for i := 0 to High(FNodes) do
    FNodes[i].Free;
end;

destructor TImage.Destroy();
begin
  if Assigned(FBitmap) then FBitmap.Free;
end;

destructor TMaterial.Destroy();
begin

end;

destructor TMesh.Destroy();
var
  i:Integer;
  buf: ^TMeshBuffer;
begin
  for i := 0 to High(FSubMesh) do
  begin
    buf := @FSubMesh[i];
    buf.VetexData.Free;
    buf.IndexData.Free;
  end;
end;

destructor TModel.Destroy();
var
  i:Integer;
begin
  for i := 0 to High(FImageList) do
    FImageList[i].Free;

  for i := 0 to High(FMeshList) do
    FMeshList[i].Free;

  for i := 0 to High(FMaterialList) do
    FMaterialList[i].Free;

  for i := 0 to High(FEffectList) do
    FEffectList[i].Free;

  for i := 0 to High(FSceneList) do
    FSceneList[i].Free;

  for i := 0 to High(FControllerList) do
    FControllerList[i].Free;
end;

end.
