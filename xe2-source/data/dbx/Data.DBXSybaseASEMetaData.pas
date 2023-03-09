{*******************************************************}
{                                                       }
{               Delphi DBX Framework                    }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{$HPPEMIT '#pragma link "Data.DBXSybaseASEMetaData"'}    {Do not Localize}
unit Data.DBXSybaseASEMetaData;

interface

uses
  Data.DBXMetaDataWriterFactory,
  Data.DBXSybaseASEMetaDataWriter
  ;


implementation

initialization
  TDBXMetaDataWriterFactory.RegisterWriter('Sybase SQL Server', TDBXSybaseASEMetaDataWriter); {Do not localize}
finalization
  TDBXMetaDataWriterFactory.UnRegisterWriter('Sybase SQL Server', TDBXSybaseASEMetaDataWriter); {Do not localize}
end.
