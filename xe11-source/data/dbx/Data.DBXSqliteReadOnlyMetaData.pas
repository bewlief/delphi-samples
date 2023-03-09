{*******************************************************}
{                                                       }
{               Delphi DBX Framework                    }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$HPPEMIT LINKUNIT}
unit Data.DBXSqliteReadOnlyMetaData;

interface

uses
  Data.DBXMetaDataCommandFactory, Data.DBXMetaDataReader;

type
  TDBXSqliteMetaDataCommandFactory = class(TDBXMetaDataCommandFactory)
  public
    function CreateMetaDataReader: TDBXMetaDataReader; override;
  end;

implementation

uses
  Data.DBXSqliteMetaDataReader;

function TDBXSqliteMetaDataCommandFactory.CreateMetaDataReader: TDBXMetaDataReader;
begin
  Result := TDBXSqliteMetaDataReader.Create;
end;

initialization
  TDBXMetaDataCommandFactory.RegisterMetaDataCommandFactory(TDBXSqliteMetaDataCommandFactory);
finalization
  TDBXMetaDataCommandFactory.UnRegisterMetaDataCommandFactory(TDBXSqliteMetaDataCommandFactory);

end.
