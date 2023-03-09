{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.WinRTMetadata;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows,
  Winapi.WinRT,
  Winapi.Cor,
  Winapi.CorHdr;

///// rometadataresolution.h

type
  PIMetaDataImport2 = ^IMetaDataImport2;
  {$EXTERNALSYM PIMetaDataImport2}

function RoGetMetaDataFile(const name: HSTRING; const metaDataDispenser: IMetaDataDispenserEx;
  metaDataFilePath: PHSTRING; metaDataImport: PIMetaDataImport2; mdTypeDef: pmdTypeDef): HRESULT; stdcall;
{$EXTERNALSYM RoGetMetaDataFile}

function RoParseTypeName(typename: HSTRING; out partsCount: Cardinal; typeNameParts: PPHSTRING): HRESULT; stdcall;
{$EXTERNALSYM RoParseTypeName}

function RoResolveNamespace(name: HSTRING; windowsMetaDataDir: HSTRING; packageGraphDirsCount: Cardinal;
  packageGraphDirs: PHSTRING; metaDataFilePathsCount: PCardinal; metaDataFilePaths: PPHSTRING;
  subNamespacesCount: PCardinal; subNamespaces: PPHSTRING): HRESULT; stdcall;
{$EXTERNALSYM RoResolveNamespace}

///// rometadata.h

type
  TCLSID = TGUID;
  {$EXTERNALSYM TCLSID}
  TIID = TGUID;
  {$EXTERNALSYM TIID}

function MetaDataGetDispenser(const clsid: TCLSID; const iid: TIID; out pv): HRESULT; stdcall;
{$EXTERNALSYM MetaDataGetDispenser}

implementation

const
  rotyperesolution  = 'api-ms-win-ro-typeresolution-l1-1-0.dll';
  rometadata        = 'rometadata.dll';

function RoGetMetaDataFile; external rotyperesolution name 'RoGetMetaDataFile' delayed;
function RoParseTypeName; external rotyperesolution name 'RoParseTypeName' delayed;
function RoResolveNamespace; external rotyperesolution name 'RoResolveNamespace' delayed;
function MetaDataGetDispenser; external rometadata name 'MetaDataGetDispenser' delayed;

end.
