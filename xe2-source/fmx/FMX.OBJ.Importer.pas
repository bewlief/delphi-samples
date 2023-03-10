{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.OBJ.Importer;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  System.Classes, System.SysUtils, FMX.Types3D, FMX.Import, FMX.Objects3D;

type
  TOBJModelImporter = class(TModelImporter)
  public
    function GetDescription: string; override;
    function GetExt: string; override;

    function LoadFromFile(const AFileName: string;
      out AMesh: TMeshDynArray; AOwner: TComponent): boolean; override;
  end;

implementation

uses
  FMX.OBJ.Model;

{ TOBJModelImporter }

function TOBJModelImporter.GetDescription: string;
begin
  Result := 'Wavefront object';
end;

function TOBJModelImporter.GetExt: string;
begin
  Result := 'OBJ';
end;

function TOBJModelImporter.LoadFromFile(const AFileName: string;
  out AMesh: TMeshDynArray; AOwner: TComponent): Boolean;
var
  i, j, idx : Integer;
  LOBJMesh : TOBJMesh;
  LOBJModel : TOBJModel;
begin
  LOBJModel := TOBJModel.Create();
  LOBJModel.LoadFromFile(AFileName);

  LOBJModel.Materials.LoadImages(ExtractFilePath(AFileName));

  AMesh := nil;
  idx := 0;

  for i := 0 to High(LOBJModel.Meshes) do
  begin
    LOBJMesh := LOBJModel.Meshes[i];

    SetLength(AMesh, Length(AMesh) + Length(LOBJMesh.SubMeshes));
    for j := 0 to High(LOBJMesh.SubMeshes) do
    begin
      AMesh[idx] := LOBJMesh.SubMeshes[j].CreateMesh(AOwner, IdentityMatrix3D,
        LOBJModel.Materials.Materials);
      Inc(idx);
    end;
  end;
  LOBJModel.Free;

  Result := True;
end;

var
  OBJImporterId: Integer;
initialization
  OBJImporterId := TModelImportServices.RegisterImporter(TOBJModelImporter.Create);
finalization
  TModelImportServices.UnregisterImporter(OBJImporterId);
end.
