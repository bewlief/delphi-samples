{*******************************************************}
{                                                       }
{               Delphi DBX Framework                    }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Data.DBXDb2MetaDataWriter;

interface

uses
  Data.DBXMetaDataWriter;

type
  TDBXDb2MetaDataWriter = class(TDBXBaseMetaDataWriter)
  public
    constructor Create;
    procedure Open; override;
  protected
    function IsSerializedIsolationSupported: Boolean; override;
    function IsIndexNamesGlobal: Boolean; override;
    function IsDescendingIndexConstraintsSupported: Boolean; override;
    function GetSqlAutoIncrementKeyword: string; override;
  end;

implementation

uses
  Data.DBXDb2MetaDataReader;

constructor TDBXDb2MetaDataWriter.Create;
begin
  inherited Create;
  Open;
end;

procedure TDBXDb2MetaDataWriter.Open;
begin
  if FReader = nil then
    FReader := TDBXDb2MetaDataReader.Create;
end;

function TDBXDb2MetaDataWriter.IsSerializedIsolationSupported: Boolean;
begin
  Result := False;
end;

function TDBXDb2MetaDataWriter.IsIndexNamesGlobal: Boolean;
begin
  Result := True;
end;

function TDBXDb2MetaDataWriter.IsDescendingIndexConstraintsSupported: Boolean;
begin
  Result := False;
end;

function TDBXDb2MetaDataWriter.GetSqlAutoIncrementKeyword: string;
begin
  Result := 'GENERATED BY DEFAULT AS IDENTITY';
end;

end.
