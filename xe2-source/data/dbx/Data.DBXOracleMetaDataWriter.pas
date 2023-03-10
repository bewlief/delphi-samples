{*******************************************************}
{                                                       }
{               Delphi DBX Framework                    }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Data.DBXOracleMetaDataWriter;

interface

uses
  Data.DBXMetaDataWriter;

type
  TDBXOracleMetaDataWriter = class(TDBXBaseMetaDataWriter)
  public
    constructor Create;
    procedure Open; override;
  protected
    function IsCatalogsSupported: Boolean; override;
    function IsSchemasSupported: Boolean; override;
    function IsMultipleStatementsSupported: Boolean; override;
    function IsDescendingIndexConstraintsSupported: Boolean; override;
    function IsIndexNamesGlobal: Boolean; override;
    function GetSqlRenameTable: UnicodeString; override;
  end;

implementation

uses
  Data.DBXOracleMetaDataReader;

constructor TDBXOracleMetaDataWriter.Create;
begin
  inherited Create;
  Open;
end;

procedure TDBXOracleMetaDataWriter.Open;
begin
  if FReader = nil then
    FReader := TDBXOracleMetaDataReader.Create;
end;

function TDBXOracleMetaDataWriter.IsCatalogsSupported: Boolean;
begin
  Result := False;
end;

function TDBXOracleMetaDataWriter.IsSchemasSupported: Boolean;
begin
  Result := True;
end;

function TDBXOracleMetaDataWriter.IsMultipleStatementsSupported: Boolean;
begin
  Result := False;
end;

function TDBXOracleMetaDataWriter.IsDescendingIndexConstraintsSupported: Boolean;
begin
  Result := False;
end;

function TDBXOracleMetaDataWriter.IsIndexNamesGlobal: Boolean;
begin
  Result := True;
end;

function TDBXOracleMetaDataWriter.GetSqlRenameTable: UnicodeString;
begin
  Result := 'RENAME :TABLE_NAME TO :NEW_TABLE_NAME';
end;

end.
