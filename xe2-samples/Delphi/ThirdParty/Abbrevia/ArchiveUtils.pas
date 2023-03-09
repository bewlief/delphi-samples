
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit ArchiveUtils;

interface

uses
  SysUtils, Classes, AbZipKit, AbArcTyp, DB, AbUtils, AbCabTyp, AbZipTyp;

type
  EArchiveException = class(Exception);

  ///	<summary>Wrapper class for various archive types</summary>
  TCustomArchive = class(TAbArchive)
  private
    FArcType: TAbArchiveType;

    {$REGION 'Method to test the integrity of every item in an archive.'}
    ///	<summary>Method to test the integrity of every item in an
    ///	archive.</summary>
    ///	<param name="Sender">Archive object</param>
    ///	<param name="Item">Item to test</param>
    ///	<remarks>Calls <c>AbTestZipItem</c> to test each item</remarks>
    {$ENDREGION}
    procedure TestZipItemProc(Sender: TObject; Item: TAbArchiveItem);

    {$REGION 'Routine to unzip an item to the provided file name'}
    ///	<summary>Routine to unzip an item to the provided file name</summary>
    ///	<param name="Sender">Archive object</param>
    ///	<param name="Item">Archive item to extract</param>
    ///	<param name="NewName">Name to use for the extracted item</param>
    ///	<remarks>Calls <c>AbUnzip</c> to extract the item</remarks>
    {$ENDREGION}
    procedure UnzipProc(Sender: TObject; Item: TAbArchiveItem;
      const NewName: string);

    {$REGION 'Unzips an item to a stream'}
    ///	<summary>Unzips an item to a stream</summary>
    ///	<param name="Sender">Archive object</param>
    ///	<param name="Item">Archive item to extract</param>
    ///	<param name="OutStream">Stream to receive extraction of item</param>
    ///	<remarks>Calls AbUnzipToStream</remarks>
    {$ENDREGION}
    procedure UnzipToStreamProc(Sender: TObject; Item: TAbArchiveItem;
      OutStream: TStream);
    function GetItems(Index: integer): TAbArchiveItem;
    procedure SetItems(Index: integer; const Value: TAbArchiveItem);
  protected
    procedure LoadArchive; override;
  public

    {$REGION 'Additional constructor supporting creating from a stream instead of a file'}
    ///	<summary>Additional constructor supporting creating from a stream
    ///	instead of a file</summary>
    ///	<param name="AStream">Input stream for archive</param>
    ///	<param name="AArchiveName">Name of the archive. If type cannot be
    ///	automatically determined, the extension of the archive name is used to
    ///	determine archive type.</param>
    {$ENDREGION}
    constructor CreateFromStream(AStream: TStream; const AArchiveName: string);
      override;

    ///	<summary>Property accessor for items in the archive</summary>
    ///	<param name="Index">Ordinal position of item to retrieve</param>
    property Items[Index:integer]: TAbArchiveItem read GetItems write SetItems;

    {$REGION 'Initializes internal method for the archive based on the archive type'}
    ///	<summary>Initializes internal method for the archive based on the
    ///	archive type</summary>
    ///	<param name="AType"><b>Optional</b>. The archive type will be
    ///	determined by examining the stream if the archive type is not
    ///	explicitly provided.</param>
    {$ENDREGION}
    procedure InitArchive(AType: TAbArchiveType = atUnknown);

  end;

  TArchive = class(TCustomArchive); //class(TAbZipKit);
  TZipArchive = class(TAbZipArchive);
  TArchiveClass = class of TAbArchive;

  /// <summary>Custom TAbCabArchive descendant that allows creation from a TStream</summary>
  TEDNCabArchive = class(TAbCabArchive)
  private
    FDeleteFile: Boolean;
    FFileName: string;
  public
    constructor CreateFromStream(AInput: TStream; const AArchiveName: string); override;
    destructor Destroy; override;
  end;

const
  STempZip = 'temp.zip';
  SDefaultName = 'ArchiveItem';
  SFileName = 'FileName';
  SPath = 'Path';
  SFileSize = 'FileSize';
  SFileDate = 'FileDate';
  SAttachment = 'Attachment';

{$REGION 'Convert a stream into an item of the provided archive type'}
///	<summary>Convert a stream into an item of the provided archive
///	type</summary>
///	<param name="AInput">Stream to compress as an item of an archive</param>
///	<param name="AName"><b>Optional</b>. Default name for archive, can be used
///	to determine archive type.</param>
///	<param name="AType"><b>Optional</b>. Type of archive to create. Defaults to
///	zip.</param>
///	<returns>Returns an archive of the requested type with the input stream
///	compressed as the only item</returns>
{$ENDREGION}
function Compress(AInput: TStream; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip): TArchive;

{$REGION 'Compress an input stream into a stream of the provided archive type'}
///	<summary>Convert a stream into an item of the provided archive
///	type</summary>
///	<param name="AInput">Stream to compress</param>
///	<param name="ADest">Destination stream for the compressed output</param>
///	<param name="AName"><b>Optional</b>. Default name for archive, can be used
///	to determine archive type.</param>
///	<param name="AType"><b>Optional</b>. Type of archive to create. Defaults to
///	zip.</param>
///	<returns>Returns an archive of the requested type with the input stream
///	compressed as the only item</returns>
{$ENDREGION}
procedure CompressToStream(AInput, ADest: TStream; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip);

{$REGION 'Overload for compressing and encoding content into a remotable string'}
///	<summary>Overload for compressing and encoding content into a remotable
///	string</summary>
///	<param name="AInput">Input string to compress and encode</param>
///	<param name="AName"><b>Optional</b>. Name of archive to create. Defaults to
///	a zip file.</param>
///	<param name="AType"><b>Optional</b>. Type of archive to create. Defaults to
///	zip.</param>
///	<returns>Returns the input string compressed into an archive of the
///	requested type, and encoded to a remotable string</returns>
{$ENDREGION}
function CompressAndEncode(const AInput: string; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip): string; overload;

{$REGION 'Overloaded function to compress and encode an input stream into a remotable a...'}
///	<summary>Overloaded function to compress and encode an input stream into a
///	remotable archive stream</summary>
///	<param name="AInput">Input stream to compress</param>
///	<param name="AName"><b>Optional</b>. Name of archive type to encode.
///	Defaults to a zip file.</param>
///	<param name="AType"><b>Optional</b>. Type of archive to create.</param>
///	<returns>Returns a remotable stream of the item compressed into the
///	requested archive type</returns>
{$ENDREGION}
function CompressAndEncode(const AInput: TStream; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip): string; overload;

{$REGION 'Convert a string to its encoded representation'}
///	<summary>Convert a string to its encoded representation</summary>
///	<param name="AInput">String to encode</param>
///	<returns>The encoded string</returns>
{$ENDREGION}
function Encode(const AInput: string): string; overload;

{$REGION 'Convert a stream to an encoded string'}
///	<summary>Convert a stream to an encoded string</summary>
///	<param name="AInput">Input stream to encode</param>
///	<returns>The stream encoded into a remotable string</returns>
{$ENDREGION}
function Encode(const AInput: TStream): string; overload;

{$REGION 'Decodes an archive and extracts the item contained in the archive'}
///	<summary>Decodes an archive and extracts the item contained in the
///	archive</summary>
///	<param name="AInput">Input stream</param>
///	<returns>Returns the decoded and uncompressed item as a string</returns>
{$ENDREGION}
function DecodeAndUnCompress(const AInput: string): string; overload;

{$REGION 'Decodes an uncompresses the first item from an archive'}
///	<summary>Decodes an uncompresses the first item from an archive</summary>
///	<param name="AInput">Input stream</param>
///	<returns>Returns a string with the content of the first item in the encoded
///	archive</returns>
{$ENDREGION}
function DecodeAndUnCompress(const AInput: TStream): string; overload;

{$REGION 'Extracts the first item in a byte array representing a compressed archive'}
///	<summary>Extracts the first item in a byte array representing a compressed
///	archive</summary>
///	<param name="AInput">Byte array of archive</param>
///	<returns>Uncompresses string of the first item in the archive</returns>
{$ENDREGION}
function UnCompress(const AInput: TBytes): string; overload;

{$REGION 'Extracts the first item in a stream representing a compressed archive'}
///	<summary>Extracts the first item in a stream representing a compressed
///	archive</summary>
///	<returns>Returns the first item as an uncompresses string</returns>
{$ENDREGION}
function UnCompress(const AInput: TStream): string; overload;

{$REGION 'Decodes an encoded (remotable) string'}
///	<summary>Decodes an encoded (remotable) string</summary>
///	<param name="input">Encoded string</param>
///	<returns>Returns the decoded string</returns>
{$ENDREGION}
function Decode(const input: string): string;

{$REGION 'Decodes an encoded string into a byte array'}
///	<summary>Decodes an encoded string into a byte array</summary>
///	<param name="input">Encoded string</param>
///	<returns>Returns the decoded string as a byte array</returns>
{$ENDREGION}
function DecodeBytes(const input: string): TBytes;

{$REGION 'Create an archive object instance from the input stream'}
///	<summary>Create an archive object instance from the input stream</summary>
///	<param name="AInput">Input stream representing archive</param>
///	<param name="AName"><b>Optional</b>. Type of archive to create. Archive
///	type will be determined from the stream if not specified.</param>
///	<returns>Returns an archive using the provided stream as its
///	contents</returns>
{$ENDREGION}
function CreateFromStream(const AInput: TStream;
  AName: string = STempZip): TArchive;

{$REGION 'Checks an achive for validity'}
///	<summary>Checks an achive for validity</summary>
///	<param name="AInput">Input string representing the archive</param>
///	<param name="AName"><b>Optional</b>. Name used to determine archive type.
///	Archive type will be determined from the input string if name is
///	skipped.</param>
///	<exception cref="EArchiveException">If the archive is not valid or is
///	empty</exception>
{$ENDREGION}
procedure CheckArchive(const AInput: string;
  AName: string = STempZip); overload;
{$REGION 'Checks an achive for validity'}
///	<summary>Checks an achive for validity</summary>
///	<param name="AInput">Input stream representing the archive</param>
///	<param name="AName"><b>Optional</b>. Name used to determine archive type.
///	Archive type will be determined from the input string if name is
///	skipped.</param>
///	<exception cref="EArchiveException">If the archive is not valid or is
///	empty</exception>
{$ENDREGION}
procedure CheckArchive(const AInput: TStream;
  AName: string = STempZip); overload;

{$REGION 'Open an archive object from a blob field reference'}
///	<summary>Open an archive object from a blob field reference</summary>
///	<param name="AField">Blob field object</param>
///	<param name="AName"><b>Optional</b>. Name of archive file.</param>
///	<returns>Returns an archive object from the TField blob stream</returns>
{$ENDREGION}
function OpenArchive(const AField: TField;
  AName: string = STempZip) : TArchive; overload;

{$REGION 'Open an archive from the file name'}
///	<summary>Open an archive from the file name</summary>
///	<param name="AFile">Name of file to open as an archive</param>
///	<param name="AType"><b>Optional</b>. Type of archive to create. If not
///	specified, archive type is determined.</param>
{$ENDREGION}
function OpenArchive(const AFile: string; AType:
  TAbArchiveType = atUnknown) : TArchive; overload;

{$REGION 'Determine the archive type of a file'}
///	<summary>Determine the archive type of a file</summary>
///	<param name="AFileName">Name of file to analyze</param>
///	<returns>Returns the archive type of the file</returns>
{$ENDREGION}
function ArchiveFileType(const AFileName: string): TAbArchiveType;

{$REGION 'Retrieve the names of the files contained in an archive (overloaded)'}
///	<summary>Retrieve the names of the files contained in an archive
///	(overloaded)</summary>
///	<param name="AField">Blob field to convert to an archive</param>
///	<param name="AIncludePath"><b>Optional</b>. <c>False</c> to omit file paths
///	from the file names returned. <c>True</c> includes file paths. Defaults to
///	<c>True</c>.</param>
///	<param name="AName"><b>Optional</b>. Name of archive. Leave empty to
///	automatically determine the archive type.</param>
///	<returns>Returns an array file names found in the archive</returns>
{$ENDREGION}
function GetArchiveFileNames(const AField: TField; AIncludePath: boolean = true;
  AName: string = STempZip): TArray<string>; overload;

{$REGION 'Retrieve the names of the files contained in an archive (overloaded)'}
///	<summary>Retrieve the names of the files contained in an archive
///	(overloaded)</summary>
///	<param name="AFile">Name of archive file</param>
///	<param name="AIncludePath"><b>Optional</b>. <c>False</c> to omit file paths
///	from the file names returned. <c>True</c> includes file paths. Defaults to
///	<c>True</c>.</param>
///	<param name="AType"><b>Optional</b>. Type of archive. Omit to
///	automatically determine the archive type.</param>
///	<returns>Returns an array file names found in the archive</returns>
{$ENDREGION}
function GetArchiveFileNames(const AFile: string; AIncludePath: boolean = true;
  AType: TAbArchiveType = atZip): TArray<string>; overload;

{$REGION 'Retrieve the names of the files contained in an archive (overloaded)'}
///	<summary>Retrieve the names of the files contained in an archive
///	(overloaded)</summary>
///	<param name="AArchive">Archive object</param>
///	<param name="AIncludePath"><b>Optional</b>. <c>False</c> to omit file paths
///	from the file names returned. <c>True</c> includes file paths. Defaults to
///	<c>True</c>.</param>
///	<returns>Returns an array file names found in the archive</returns>
{$ENDREGION}
function GetArchiveFileNames(const AArchive: TArchive;
  AIncludePath: boolean = true): TArray<string>; overload;

function ArchiveList(const AField: TField;
  AName: string = STempZip): TDataSet; overload;
function ArchiveList(const AFile: string;
  AType: TAbArchiveType = atZip): TDataSet; overload;
function ArchiveToDataSet(const AField: TField; AName: string = STempZip;
  ALoad: boolean = true; AFiles: string = ''): TDataSet; overload;
function ArchiveToDataSet(const AArchive: TArchive; ALoad: boolean = true;
  AFiles: string = ''): TDataSet; overload;

function ExtractArchiveFile(const AInput: TStream; AFileName: string) : string;
function FileExtFromArchiveType(AType: TAbArchiveType): string;

implementation

uses
  EncdDecd, Zlib, AbBrowse, AbTarTyp, AbGzTyp, AbUnzPrc,
  AbBzip2Typ, AbZipper, Windows, DBClient;

resourcestring
  StrUnknownArchiveType = 'Unable to determine archive type';

function GetTempDirectory: string;
var
  tempFolder: array[0..MAX_PATH-1] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;

function CreateTempFile(const AExt:string = ''): string;
// Creates a temporal file and returns its path name
var
  TempFileName: array [0..MAX_PATH-1] of Char;
begin
  if GetTempFileName(PChar(GetTempDirectory), '~', 0, TempFileName) = 0 then
    raise EArchiveException.Create(SysErrorMessage(GetLastError));
  Result := TempFileName;

  if (AExt <> '') then
  begin
    //Because GetTempFileName actually creates the file, ensure we rename it
    //if we are applying an extension
    Result := ChangeFileExt(Result, AExt);
    SysUtils.RenameFile(TempFileName, Result);
  end;
end;


function ArchiveClassFromType(const AType: TAbArchiveType): TArchiveClass;
begin
  case AType of
    atZip,
    atSpannedZip,
    atSelfExtZip: Result := TAbZipArchive;
    atTar:        Result := TAbTarArchive;
    atGzip,
    atGzippedTar: Result := TAbGzipArchive;
    atCab:        Result := TEDNCabArchive;
    atBzip2,
    atBzippedTar: Result := TAbBzip2Archive;
  else
    raise EArchiveException.Create(StrUnknownArchiveType);
  end;
end;

function InitArchive(AStream: TStream = nil; AFile: string = ''; AType: TAbArchiveType = atZip): TArchive;
begin
  Result := TArchive(ArchiveClassFromType(AType).CreateFromStream(AStream, AFile));
  Result.InitArchive(AType);
  Result.Load;
end;

function ArchiveFileType(const AFileName: string): TAbArchiveType;
var
  s: TFileStream;
begin
  s := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := AbDetermineArcType(s);
  finally
    s.Free;
  end;
end;

function CreateFromStream(const AInput: TStream; AName: string = STempZip): TArchive;
var
  lType: TAbArchiveType;
begin
  lType := AbDetermineArcType(AInput);
  Result := InitArchive(AInput, AName, lType);
end;

function OpenArchive(const AField: TField; AName: string = STempZip): TArchive;
var
  input: TStream;
  buf: TArray<byte>;
begin
  input := TMemoryStream.Create;
  try
    buf := AField.AsBytes;
    if Length(buf) > 0 then
    begin
      input.Write(buf[0], Length(buf));
      SetLength(buf, 0);
      Result := CreateFromStream(input, AName);
      Result.OwnsStream := True; // Freeing archive will free input stream
    end
    else
    begin
      Result := nil;
      FreeAndNil(input);
    end;
  except
    FreeAndNil(input);
    FreeAndNil(Result);
    raise;
  end;
end;

function OpenArchive(const AFile: string; AType: TAbArchiveType = atUnknown): TArchive;
begin
//  Result := ArchiveClassFromType(AType).Create(AFile, fmOpenRead) as TArchive;
  Result := InitArchive(nil, AFile, AType);
end;

function GetArchiveFileNames(const AField: TField; AIncludePath: boolean = true;
  AName: string = STempZip): TArray<string>;
var
  a: TArchive;
begin
  a := OpenArchive(AField, AName);
  try
    Result := GetArchiveFileNames(a, AIncludePath);
  finally
    FreeAndNil(a);
  end;
end;

function GetArchiveFileNames(const AFile: string;
  AIncludePath: boolean = true; AType: TAbArchiveType = atZip): TArray<string>;
var
  a: TArchive;
begin
  a := OpenArchive(AFile, AType);
  try
    Result := GetArchiveFileNames(a, AIncludePath);
  finally
    FreeAndNil(a);
  end;
end;

function GetArchiveFileNames(const AArchive: TArchive;
  AIncludePath: boolean = true): TArray<string>;
var
  i: integer;
  item: TAbArchiveItem;
  names: TStrings;
begin
  names := TStringList.Create;
  try
    for i := 0 to Pred(AArchive.Count) do
    begin
      item := AArchive.Items[i];
      if (Length(item.FileName) > 0) then
      begin
        if AIncludePath then
          names.Add(item.DiskFileName)
        else
          names.Add(item.FileName);
      end;
    end;
    Result := Names.ToStringArray;
  finally
    FreeAndNil(names);
  end;
end;

function ArchiveList(const AField: TField; AName: string = STempZip): TDataSet;
begin
  Result := ArchiveToDataSet(AField, AName, False);
end;

function ArchiveList(const AFile: string; AType: TAbArchiveType = atZip): TDataSet;
var
  a: TArchive;
begin
  a := OpenArchive(AFile, AType);
  try
    Result := ArchiveToDataSet(a, false);
  finally
    FreeAndNil(a);
  end;
end;

function ArchiveToDataSet(const AField: TField; AName: string = STempZip;
  ALoad: boolean = True; AFiles: string = ''): TDataSet;
var
  a: TArchive;
begin
  a := OpenArchive(AField, AName);
  try
    Result := ArchiveToDataSet(a, ALoad, AFiles);
  finally
    FreeAndNil(a);
  end;
end;

function ExtractArchiveFile(const AInput: TStream; AFileName: string) : string;
var
  zip : TAbZipKit;
  stream: TStringStream;
begin
  zip := TAbZipKit.Create(nil);
  try
    zip.TarAutoHandle := True;
    zip.Stream := AInput;
    stream := TStringStream.Create;
    try
      zip.ExtractToStream(zip.Items[0].FileName, stream);
      Result := stream.DataString;
    finally
      stream.Free;
    end;
  finally
    zip.Free;
  end;
end;

function ArchiveToDataSet(const AArchive: TArchive; ALoad: boolean = true;
  AFiles: string = ''): TDataSet;
var
  i: Integer;
  item: TAbArchiveItem;
  F: TStringList;
  FldFileName,
  FldPath,
  FldFileSize,
  FldFileDate: TField;
  FldAttachment: TBlobField;

  Attachment: TMemoryStream;

  procedure AddDef(cds: TClientDataSet; AName: string; ADataType: TFieldType;
    ASize: integer = 0);
  begin
    with cds.FieldDefs.AddFieldDef do
    begin
      Name := AName;
      DataType := ADataType;
      if ASize > 0 then
        Size := ASize;
    end;
  end;

  function CreateArchiveCDS: TClientDataSet;
  begin
    Result := TClientDataSet.Create(nil);
    AddDef(Result, SFileName, ftString, 125);
    AddDef(Result, SPath, ftString, 125);
    AddDef(Result, SFileSize, ftLargeInt);
    AddDef(Result, SFileDate, ftDateTime);
    if ALoad then
      AddDef(Result, SAttachment, ftBlob);
    Result.CreateDataSet;
    Result.LogChanges := False;
    FldFileName := Result.FieldByName(SFileName);
    FldPath := Result.FieldByName(SPath);
    FldFileSize := Result.FieldByName(SFileSize);
    FldFileDate := Result.FieldByName(SFileDate);
    if ALoad then
      FldAttachment := Result.FieldByName(SAttachment) as TBlobField;
  end;

  function AddFile(AFile: string): boolean;
  begin
    Result := (not Assigned(F)) or (F.IndexOf(AFile) > -1);
  end;

begin

  Result := CreateArchiveCDS;
  try
    if AFiles <> '' then // Return only the requested files
    begin
      F := TStringList.Create;
      F.CaseSensitive := false;
      F.Sorted := true;
      F.Delimiter := ';';
      F.DelimitedText := AFiles;
    end
    else
      F := nil;

    Attachment := TMemoryStream.Create;
    //for item in archive.ItemList.Items do
    for i := 0 to Pred(AArchive.Count) do
    begin
      item := AArchive.Items[i];
      if (Length(item.FileName) > 0) and (not item.IsDirectory) and AddFile(item.FileName) then
      begin
        Result.Append;
        FldFileName.AsString := item.FileName;
        FldPath.AsString := item.DiskPath;
        FldFileSize.Value := item.UncompressedSize;
        FldFileDate.AsDateTime := item.LastModTimeAsDateTime;
        if ALoad then
        begin
          Attachment.Clear;
          AArchive.ExtractToStream(item.FileName, Attachment);
          FldAttachment.LoadFromStream(Attachment);
        end;
        Result.Post;
      end;
    end;
    Result.First;
  finally
    FreeAndNil(F);
    FreeAndNil(Attachment);
  end;
end;

procedure CheckArchive(const AInput: string; AName: string = STempZip);
var
  ZipStream : TStringStream;
  DecodedStream: TMemoryStream;
begin
  ZipStream := TStringStream.Create(AInput);
  try
    DecodedStream := TMemoryStream.Create;
    try
      DecodeStream(ZipStream, DecodedStream);
      DecodedStream.Position := 0;
      CheckArchive(DecodedStream, AName);
    finally
      DecodedStream.Free;
    end;
  finally
    ZipStream.Free;
  end;
end;

procedure CheckArchive(const AInput: TStream; AName: string = STempZip);
var
  zip: TArchive;
begin
  zip := CreateFromStream(AInput, AName);
  try
    if (zip.Count <= 0) then
      raise EArchiveException.CreateFmt('%s cannot be empty', [ AName ]);
  finally
    zip.Free;
  end;
end;

function CompressAndEncode(const AInput: string; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip): string;
var
  lStringStream: TStringStream;
begin
  lStringStream := TStringStream.Create(AInput);
  try
    Result := CompressAndEncode(lStringStream, AName, AType);
  finally
    lStringStream.Free;
  end;
end;

function Compress(AInput: TStream; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip): TArchive;
var
  lMemStr: TMemoryStream;
begin
  //TArchive will own the stream, so we don't need to worry about freeing it
  lMemStr := TMemoryStream.Create;
  CompressToStream(AInput, lMemStr, AName, AType);
  Result := CreateFromStream(lMemStr);
end;

procedure CompressToStream(AInput, ADest: TStream; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip);
var
  lZipKit: TAbZipKit;
  lStr: TFileStream;
  lFileName: string;
begin
  lFileName := CreateTempFile(FileExtFromArchiveType(AType));
  try
    lZipKit := TAbZipKit.Create(nil);
    try
      lZipKit.ArchiveType := AType;
      lZipKit.ForceType := True;
      lZipKit.TarAutoHandle := True;
      if (ADest.Size > 0) and (AbDetermineArcType(ADest) <> atUnknown) then
      begin
        lStr := TFileStream.Create(lFileName, fmOpenReadWrite);
        try
          ADest.Position := 0;
          lStr.CopyFrom(ADest, ADest.Size);
        finally
          lStr.Free;
        end;
        lZipKit.OpenArchive(lFileName);
      end
      else
        lZipKit.FileName := lFileName;
      lZipKit.AddFromStream(AName, AInput);
      lZipKit.Save;
    finally
      lZipKit.Free;
    end;
    lStr := TFileStream.Create(lFileName, fmOpenReadWrite);
    try
      ADest.Position := 0;
      ADest.CopyFrom(lStr, lStr.Size);
      ADest.Position := 0;
    finally
      lStr.Free;
    end;
  finally
    SysUtils.DeleteFile(lFileName);
  end;
end;

function CompressAndEncode(const AInput: TStream; AName: string = SDefaultName;
  AType: TAbArchiveType = atZip): string;
var
  lStringStr: TStringStream;
  lArchive: TArchive;
begin
  lArchive := Compress(AInput, AName, AType);
  lStringStr := TStringStream.Create('');
  try
    lArchive.FStream.Position := 0;
    EncodeStream(lArchive.FStream, lStringStr);
    Result := lStringStr.DataString;
  finally
    lStringStr.Free;
  end;
end;

function UnCompress(const AInput: TStream): string;
var
  lStringStream: TStringStream;
  //lZipKit: TAbZipKit;
  lArc: TArchive;
begin
  lArc := CreateFromStream(AInput);
  try
    lStringStream := TStringStream.Create;
    try
      lArc.ExtractItemToStreamAt(0, lStringStream);
      Result := lStringStream.DataString;
    finally
      lStringStream.Free;
    end;
  finally
    lArc.Free;
  end;
//  lZipKit := TAbZipKit.Create(nil);
//  try
//    if (AType <> atUnknown) then
//    begin
//      lZipKit.ArchiveType := AType;
//      lZipKit.ForceType := True;
//    end;
//    AInput.Position := 0;
//    lZipKit.TarAutoHandle := True;
//    lZipKit.Stream := AInput;
//    lStringStream := TStringStream.Create('');
//    try
//      lZipKit.ExtractToStream(lZipKit.Items[0].FileName, lStringStream);
//      Result := lStringStream.DataString;
//    finally
//      lStringStream.Free;
//    end;
//  finally
//    lZipKit.Free;
//  end;
end;

function UnCompress(const AInput: TBytes): string;
var
  lStream: TBytesStream;
begin
  lStream := TBytesStream.Create(AInput);
  try
    Result := UnCompress(lStream);
  finally
    lStream.Free;
  end;
end;

function DecodeAndUnCompress(const AInput: TStream): string;
var
  lMemStream: TMemoryStream;
begin
  lMemStream := TMemoryStream.Create;
  try
    DecodeStream(AInput, lMemStream);
    lMemStream.Position := 0;
    Result := Uncompress(lMemStream);
  finally
    lMemStream.Free;
  end;
end;

function DecodeAndUnCompress(const AInput: string): string;
var
  lStringStream: TStringStream;
begin
  lStringStream := TStringStream.Create(AInput);
  try
    Result := DecodeAndUncompress(lStringStream);
  finally
    lStringStream.Free;
  end;
end;

function Encode(const AInput: string): string; overload;
var
  fCompressedStream: TStringStream;
  fStringStream: TStringStream;
begin
  fCompressedStream := TStringStream.create(AInput);
  try
    fStringStream := TStringStream.create('');
    try
      fCompressedStream.Position := 0;
      EncodeStream(fCompressedStream, fStringStream);
      Result := fStringStream.datastring;
    finally
      FreeAndNil(fStringStream);
    end;
  finally
    FreeAndNil(fCompressedStream);
  end;
end;

function Encode(const AInput: TStream): string; overload;
var
  fStringStream: TStringStream;
begin
  fStringStream := TStringStream.create('');
  try
    EncodeStream(AInput, fStringStream);
    Result := fStringStream.datastring;
  finally
    FreeAndNil(fStringStream);
  end;
end;

function Decode(const input: string): string;
var
  fWorkStream: TMemoryStream;
  fEncodedStream: TMemoryStream;
begin
  fWorkStream := TMemoryStream.create;
  try
    fWorkStream.size := length(input);
    move(input[1], fWorkStream.memory^, fWorkStream.size);
    fWorkStream.position := 0;
    fEncodedStream := TMemoryStream.create;
    try
      DecodeStream(fWorkStream, fEncodedStream);
      fEncodedStream.position := 0;
      SetLength(Result, fEncodedStream.size);
      move(fEncodedStream.memory^, Result[1], fEncodedStream.size);
    finally
      FreeAndnil(fEncodedStream);
    end;
  finally
    FreeAndNil(fWorkStream);
  end;
end;

function DecodeBytes(const input: string): TBytes;
var
  lInput: TStringStream;
  lOutput: TBytesStream;
begin
  lInput := TStringStream.Create;
  try
    lInput.WriteString(input);
    lInput.Position := 0;
    lOutput := TBytesStream.Create;
    try
      DecodeStream(lInput, lOutput);
      Result := lOutput.Bytes;
      SetLength(Result, lOutput.Size);
    finally
      lOutput.Free;
    end;
  finally
    lInput.Free;
  end;
end;

{ TEDNCabArchive }

constructor TEDNCabArchive.CreateFromStream(AInput: TStream;
  const AArchiveName: string);
var
  lFileStream: TFileStream;
begin
  FDeleteFile := False;
  if (AInput is TFileStream) then
    FFileName := TFileStream(AInput).FileName
  else
  begin
    FFileName := CreateTempFile;
    lFileStream := TFileStream.Create(FFileName, fmCreate);
    try
      lFileStream.CopyFrom(AInput, AInput.Size);
    finally
      lFileStream.Free;
    end;
    FDeleteFile := True;
  end;
  Create(FFileName, fmOpenReadWrite or fmShareDenyNone);
end;

destructor TEDNCabArchive.Destroy;
begin
  if FDeleteFile then
    SysUtils.DeleteFile(FFileName);
  inherited;
end;

function FileExtFromArchiveType(AType: TAbArchiveType): string;
begin
  case AType of
    atZip,
    atSpannedZip: Result := '.zip';
    atSelfExtZip: Result := '.exe';
    atTar:        Result := '.tar';
    atGzip:       Result := '.gz';
    atGzippedTar: Result := '.tgz';
    atCab:        Result := '.cab';
    atBzip2:      Result := '.bz2';
    atBzippedTar: Result := '.tbz';
  else
    Result := '';
  end;
end;

{ TCustomArchive }

constructor TCustomArchive.CreateFromStream(AStream: TStream;
  const AArchiveName: string);
begin
  inherited;
  FArcType := AbBrowse.AbDetermineArcType(AStream);
end;

function TCustomArchive.GetItems(Index: integer): TAbArchiveItem;
begin
  Result := ItemList.Items[Index];
end;

procedure TCustomArchive.InitArchive(AType: TAbArchiveType = atUnknown);
begin
  inherited;
  if AType = atUnknown then
    AType := AbBrowse.AbDetermineArcType(FInStream);
  case AType of
    atUnknown: ;
    atZip,
    atSpannedZip,
    atSelfExtZip:
      begin
        TAbZipArchive(self).ExtractHelper         := UnzipProc;
        TAbZipArchive(self).ExtractToStreamHelper := UnzipToStreamProc;
        TAbZipArchive(self).TestHelper            := TestZipItemProc;
      end;
    atTar: ;
    atGzip: ;
    atGzippedTar:
      begin
        TAbGzipArchive(self).TarAutoHandle := True;
        TAbGzipArchive(self).IsGzippedTar := True;
      end;
    atCab: ;
    atBzip2: ;
    atBzippedTar:
      begin
        TAbBzip2Archive(self).TarAutoHandle := True;
        TAbBzip2Archive(self).IsBzippedTar := True;
      end;
  end;
end;

type
  TZipAcc = class(TAbZipArchive);
  TTarAcc = class(TAbTarArchive);
  TGZipAcc = class(TAbGzipArchive);
  TCabAcc = class(TAbCabArchive);
  TBzip2Acc = class(TAbBzip2Archive);

procedure TCustomArchive.LoadArchive;
begin
  inherited;
//  case FArcType of
//    atUnknown: ;
//    atZip,
//    atSpannedZip,
//    atSelfExtZip: TZipAcc(self).LoadArchive;
//    atTar: TTarAcc(self).LoadArchive;
//    atGzip: TGzipAcc(self).LoadArchive;
//    atGzippedTar: ;
//    atCab: TCabAcc(self).LoadArchive;
//    atBzip2: TBzip2Acc(self).LoadArchive;
//    atBzippedTar: ;
//  end;

end;

procedure TCustomArchive.UnzipProc(Sender: TObject; Item: TAbArchiveItem;
  const NewName: string );
begin
  AbUnzip(TAbZipArchive(Sender), TAbZipItem(Item), NewName);
end;

procedure TCustomArchive.UnzipToStreamProc(Sender: TObject; Item: TAbArchiveItem;
  OutStream: TStream);
begin
  AbUnzipToStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;

procedure TCustomArchive.SetItems(Index: integer; const Value: TAbArchiveItem);
begin
  ItemList.Items[Index] := Value;
end;

procedure TCustomArchive.TestZipItemProc(Sender: TObject; Item: TAbArchiveItem);
begin
  AbTestZipItem(TAbZipArchive(Sender), TAbZipItem(Item));
end;

end.
