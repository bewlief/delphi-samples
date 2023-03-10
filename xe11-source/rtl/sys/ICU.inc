{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

type
  UBool = ShortInt;
  UChar = Char;
  UChar32 = Int32;
  PUChar = PChar;
  UErrorCode = Int32;
  PUErrorCode = ^UErrorCode;
  TByteArray = array of Byte;
  TStarterArray = array [0..255] of UBool;
  PStarterArray = ^TStarterArray;
  UConverter = record end;
  PUConverter = ^UConverter;

  PUCollator = ^UCollator;
  UCollator = record end;

  UCollationResult = (UCOL_LESS = -1, UCOL_EQUAL = 0, UCOL_GREATER = 1);

  UConverterCallbackReason = (
    UCNV_CB_UNASSIGNED = 0,  // The code point is unassigned. The error code U_INVALID_CHAR_FOUND will be set.
    UCNV_CB_ILLEGAL = 1,     // The code point is illegal. The error code U_ILLEGAL_CHAR_FOUND will be set.
    UCNV_CB_IRREGULAR = 2,   // The codepoint is not a regular sequence in the encoding. The error code U_INVALID_CHAR_FOUND will be set.
    UCNV_CB_RESET = 3,       // The callback is called with this reason when a 'reset' has occured. Callback should reset all state.
    UCNV_CB_CLOSE = 4,       // Called when the converter is closed. The callback should release any allocated memory.
    UCNV_CB_CLONE = 5        // Called when ucnv_safeClone() is called on the converter. the pointer available as the
                             // 'context' is an alias to the original converters' context pointer. If the context must be owned
                             // by the new converter, the callback must clone the data and call ucnv_setFromUCallback
                             // (or setToUCallback) with the correct pointer.
  );

  PUConverterToUnicodeArgs = ^UConverterToUnicodeArgs;
  UConverterToUnicodeArgs = record end;

  PUConverterFromUnicodeArgs = ^UConverterFromUnicodeArgs;
  UConverterFromUnicodeArgs = record end;

  UConverterFromUCallback = procedure(const context: Pointer; fromArgs: PUConverterFromUnicodeArgs;
              const codeUnits: PUChar; length: Int32; codePoint: UChar32; reason: UConverterCallbackReason; var err: UErrorCode);cdecl;

  UConverterToUCallback = procedure(const context: Pointer; toArgs: PUConverterToUnicodeArgs;
              const codeUnits: MarshaledAString; length: Int32; reason: UConverterCallbackReason; var err: UErrorCode);cdecl;

const
  U_ZERO_ERROR             = 0;
  UCNV_PRV_ESCAPE_ICU      = 0;
  UCNV_PRV_ESCAPE_C        = Ord('C');
  UCNV_PRV_ESCAPE_XML_DEC  = Ord('D');
  UCNV_PRV_ESCAPE_XML_HEX  = Ord('X');
  UCNV_PRV_ESCAPE_JAVA     = Ord('J');
  UCNV_PRV_ESCAPE_UNICODE  = Ord('U');
  UCNV_PRV_ESCAPE_CSS2     = Ord('S');
  UCNV_PRV_STOP_ON_ILLEGAL = Ord('i');

var
{ ICU ustring.h File Reference }
{ http://www.icu-project.org/apiref/icu4c/ustring_8h.html }
  // Convert a UTF-16 string to UTF-8.
  u_strToUTF8: function(dest: MarshaledAString; destCapacity: Int32; var DestLength: Int32;
      const src: PUChar; srcLength: Int32; var ErrorCode: UErrorCode): MarshaledAString; cdecl;

  //Convert a UTF-16 string to UTF-8 with Sub.
  u_strToUTF8WithSub: function(dest: MarshaledAString; destCapacity: Int32; var DestLength: Int32;
      const src: PUChar; srcLength: Int32; subchar: UChar32; var NumSubstitutions: Int32;
      var ErrorCode: UErrorCode): PUChar; cdecl;

  //Convert a UTF-8 string to UTF-16.
  u_strFromUTF8: function(dest: PUChar; destCapacity: Int32; var DestLength: Int32;
      const src: MarshaledAString; srcLength: Int32; var ErrorCode: UErrorCode): PUChar; cdecl;

  //Convert a UTF-8 string to UTF-16 with Sub.
  u_strFromUTF8WithSub: function(dest: PUChar; destCapacity: Int32; var DestLength: Int32;
      const src: MarshaledAString; srcLength: Int32; subchar: UChar32; var NumSubstitutions: Int32;
      var ErrorCode: UErrorCode): PUChar; cdecl;


  // Compare two strings case-insensitively using full case folding.
  u_strcasecmp: function(const s1:PUChar; const s2:PUChar; options: Int32): Int32; cdecl;


{ ICU ucnv.h File Reference }
{ http://www.icu-project.org/apiref/icu4c/ucnv_8h.html }
  //Creates a UConverter object with the name of a coded character set specified as a C string.
  ucnv_open: function(const converterName: MarshaledAString; var err: UErrorCode): PUConverter; cdecl;

  //Resets the state of a converter to the default state.
  ucnv_reset: procedure(converter: PUConverter); cdecl;

  //Deletes the unicode converter and releases resources associated with just this instance.
  ucnv_close: procedure(converter: PUConverter); cdecl;

 	//Convert the Unicode string into a codepage string using an existing UConverter.
  ucnv_fromUChars: function(cnv: PUConverter; dest: MarshaledAString; destCapacity: Int32;
                      const src: PUChar; srcLength: Int32; var ErrorCode: UErrorCode): Int32; cdecl;

  //Convert the codepage string into a Unicode string using an existing UConverter.
	ucnv_toUChars: function(cnv: PUConverter; dest: PUChar; destCapacity: Int32;
                      const src: MarshaledAString ; srcLength: Int32; var ErrorCode: UErrorCode): Int32; cdecl;

  //Sets the substitution chars when converting from unicode to a codepage.
//  ucnv_setSubstChars: procedure(converter: PUConverter; subChars: MarshaledAString; len: word; var err: UErrorCode); cdecl;

  //Gets the "starter" (lead) bytes for converters of type MBCS.
  ucnv_getStarters: procedure(converter: PUConverter; var starters: TStarterArray; var err: UErrorCode); cdecl;

  //Changes the callback function used by the converter when an illegal or invalid sequence is found.
  ucnv_setFromUCallBack: procedure(cnv: PUConverter; newAction: UConverterFromUCallback; const newContext: Pointer;
                      oldAction: UConverterFromUCallback; const oldContext: PPointer; var ErrorCode: UErrorCode); cdecl;

  //Changes the callback function used by the converter when an illegal or invalid sequence is found.
  ucnv_setToUCallBack: procedure(cnv: PUConverter; newAction: UConverterToUCallback; const newContext: Pointer;
                      oldAction: UConverterToUCallback; const oldContext: PPointer; var ErrorCode: UErrorCode); cdecl;

  //Writes out the specified byte output bytes to the target byte buffer or to converter internal buffers.
	ucnv_cbFromUWriteBytes: procedure(args: PUConverterFromUnicodeArgs; const source: MarshaledAString;
                      length: Int32; offsetIndex: Int32; var ErrorCode: UErrorCode); cdecl;

  //This function will write out the specified characters to the target UChar buffer.
	ucnv_cbToUWriteUChars: procedure(args: PUConverterToUnicodeArgs; const source: PUChar; length: Int32;
                      offsetIndex: Int32; var ErrorCode: UErrorCode); cdecl;


  { ICU ucol.h File Reference}
  { http://www.icu-project.org/apiref/icu4c/ucol_8h.html }

  // Open a UCollator for comparing strings.
  ucol_open: function(const loc: MarshaledAString; var status: UErrorCode): PUCollator; cdecl;

 	// Close a UCollator.
  ucol_close: procedure(coll: PUCollator); cdecl;

  // 	Compare two strings in UTF-8. ICU 5.0
//  ucol_strcollUTF8: function(const coll: PUCollator; const source: PUChar; sourceLength: Int32;
//      const target: PUChar; targetLength: Int32; var status: UErrorCode): UCollationResult; cdecl;


function InitICU: Boolean;
var
  CStr: MarshaledAString;

  //We haven't Marshall yet
  function CStrToStr(const Str: MarshaledAString; len: Byte; var Dest: string): string;  // Only ASCII chars
  type
    PWordArray = ^TWordArray;
    TWordArray = array[0..127] of Word;
    var
    I:Byte;
    P: PWordArray;
  begin
    SetLength(Dest, len);
    P := PWordArray(Dest);
    for I := 0 to len - 1 do
    begin
      P^[I] := Word((PByte(Str)+I)^);
    end;
    Result := Dest;
  end;

  function StrToCStr(const Str: string; Dest: MarshaledAString): MarshaledAString;  // Only ASCII chars
  var
    I: Integer;
  begin
    Result := Dest;
    for I := Low(Str) to High(Str) do
    begin
      Byte(Dest^) := Byte(Str[I]);
      Inc(Dest);
    end;
    Byte(Dest^) := 0;
  end;

  function SStrToCStr(const Str: _ShortStr; Dest: MarshaledAString): MarshaledAString;  // Only ASCII chars
  var
    I: Integer;
  begin
    Result := Dest;
    for I := Low(Str) to High(Str) do
    begin
      Byte(Dest^) := Byte(Str[I]);
      Inc(Dest);
    end;
    Byte(Dest^) := 0;
  end;

  var
    ICUVersionNumber: Integer;

  function GetICUVersion(hICU: NativeUInt): string;
  const
    APINAME = 'u_strToUTF8';
  var
    I: Integer;
    VersionName: string;

    function FoundExpectedVersionName(const VersionNumber: Integer): Boolean;
    var
      TempName: string;
      TempShortStr: _ShortStr;
    begin
      TempShortStr := _Str0Int64(VersionNumber);
      CStrToStr(MarshaledAString(@TempShortStr[1]), Byte(TempShortStr[0]), TempName);
      VersionName := '_' + TempName;
      if dlsym(hICU, StrToCStr(APINAME + VersionName, CStr)) <> nil then
        Exit(True);
      VersionName := '_' + TempName[Low(string)] + '_' + TempName[Low(string) + 1];
      if dlsym(hICU, StrToCStr(APINAME + VersionName, CStr)) <> nil then
        Exit(True);
      VersionName := '';
      Result := False;
    end;

  begin
    VersionName := '';
    Result := '';
    if dlsym(hICU, StrToCStr(APINAME, CStr)) <> nil then Exit('');  // Apple has no version
    if ICUVersionNumber > 0 then
      if FoundExpectedVersionName(ICUVersionNumber) then Exit(VersionName);
    for I := 99 downto 40 do
      if FoundExpectedVersionName(I) then Exit(VersionName);
    VersionName := 'error';
    Result := VersionName;
  end;

  function GetFncAddress(Handle: NativeUInt; FunctionName: string): Pointer;
  begin
    Result := dlsym(Handle, StrToCStr(FunctionName + LibICUSuffix, CStr));
  end;

{$IF defined(LINUX)}
  function OpenICU_Lib(const AName: _ShortStr): NativeUInt;
  var
    LCStr: MarshaledAString;
    I: Integer;
    Temp: _ShortStr;
  begin
    LCStr := System.AllocMem(1024);

    if ICUVersionNumber > 0 then
    begin
      Temp := AName + '.' + _Str0Int64(ICUVersionNumber) + #$00;
      Result := dlopen(_PAnsiChr(@Temp[1]), RTLD_LAZY);
      System.FreeMem(LCStr);
      Exit;
    end;

    Temp := AName + #$00;
    Result := dlopen(_PAnsiChr(@Temp[1]), RTLD_LAZY);
    if Result > 0 then
    begin
      ICUVersionNumber := 0;
      System.FreeMem(LCStr);
      Exit;
    end;

    for I := 99 downto 40 do
    begin
      Temp := AName + '.' + _Str0Int64(I) + #$00;
      Result := dlopen(_PAnsiChr(@Temp[1]), RTLD_LAZY);
      if Result > 0 then
      begin
        System.FreeMem(LCStr);
        ICUVersionNumber := I;
        Exit;
      end
    end;
    Result := 0;
  end;
{$ENDIF}


begin
  ICUVersionNumber := -1;
  Result := False;
{$IFDEF MACOS}
  HICUUC := dlopen('/usr/lib/libicucore.dylib', RTLD_LAZY);
  HICUI18N := HICUUC;
{$ELSEIF defined(LINUX)}
  HICUUC := OpenICU_Lib('libicuuc.so');
  HICUI18N := OpenICU_Lib('libicui18n.so');
{$ELSE defined(ANDROID)}
  HICUUC := dlopen('libicuuc.so', RTLD_LAZY);
  HICUI18N := dlopen('libicui18n.so', RTLD_LAZY);
{$ENDIF}
  if HICUUC <> 0 then
  begin
    CStr := System.AllocMem(256);
    LibICUSuffix := GetICUVersion(HICUUC);
    if LibICUSuffix = 'error' then
    begin
      System.FreeMem(CStr);
      Exit;
    end;

    @u_strToUTF8 := GetFncAddress(HICUUC, 'u_strToUTF8');
    @u_strToUTF8WithSub := GetFncAddress(HICUUC, 'u_strToUTF8WithSub');
    @u_strFromUTF8 := GetFncAddress(HICUUC, 'u_strFromUTF8');
    @u_strFromUTF8WithSub := GetFncAddress(HICUUC, 'u_strFromUTF8WithSub');
    @u_strcasecmp := GetFncAddress(HICUUC, 'u_strcasecmp');

    @ucnv_open := GetFncAddress(HICUUC, 'ucnv_open');
    @ucnv_reset := GetFncAddress(HICUUC, 'ucnv_reset');
    @ucnv_close := GetFncAddress(HICUUC, 'ucnv_close');
    @ucnv_fromUChars := GetFncAddress(HICUUC, 'ucnv_fromUChars');
    @ucnv_toUChars := GetFncAddress(HICUUC, 'ucnv_toUChars');
//    @ucnv_setSubstChars := GetFncAddress(System.HICUUC, 'ucnv_setSubstChars');
    @ucnv_getStarters := GetFncAddress(HICUUC, 'ucnv_getStarters');
    @ucnv_setToUCallBack := GetFncAddress(HICUUC, 'ucnv_setToUCallBack');
    @ucnv_setFromUCallBack := GetFncAddress(HICUUC, 'ucnv_setFromUCallBack');
    @ucnv_cbToUWriteUChars := GetFncAddress(HICUUC, 'ucnv_cbToUWriteUChars');
    @ucnv_cbFromUWriteBytes := GetFncAddress(HICUUC, 'ucnv_cbFromUWriteBytes');

    @ucol_open := GetFncAddress(HICUI18N, 'ucol_open');
    @ucol_close := GetFncAddress(HICUI18N, 'ucol_close');
//    @ucol_strcollUTF8 := GetFncAddress(HICUI18N, 'ucol_strcollUTF8');

    System.FreeMem(CStr);
    Result := True;
  end
  else
    if HICUI18N <> 0 then
    begin
      dlclose(HICUI18N);
      HICUI18N := 0;
    end;
end;
