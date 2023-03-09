{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit System.AnsiStrings;

interface


{$IFDEF POSIX}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

{$IFDEF CPUX86}
  {$DEFINE X86ASM}
{$ELSE !CPUX86}
  {$DEFINE PUREPASCAL}
{$ENDIF !CPUX86}

uses
  System.SysUtils, System.StrUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.String_,
{$ENDIF}
  System.SysConst;

{ UpperCase converts all ASCII characters in the given AnsiString to upper case.
  The conversion affects only 7-bit ASCII characters between 'a' and 'z'. To
  convert 8-bit international characters, use AnsiUpperCase. }

function UpperCase(const S: AnsiString): AnsiString; overload;
function UpperCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString; overload; inline;

{ LowerCase converts all ASCII characters in the given AnsiString to lower case.
  The conversion affects only 7-bit ASCII characters between 'A' and 'Z'. To
  convert 8-bit international characters, use AnsiLowerCase. }

function LowerCase(const S: AnsiString): AnsiString; overload;
function LowerCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString; overload; inline;

{ CompareStr compares S1 to S2, with case-sensitivity. The return value is
  less than 0 if S1 < S2, 0 if S1 = S2, or greater than 0 if S1 > S2. The
  compare operation is based on the 8-bit ordinal value of each character
  and is not affected by the current user locale. }

function CompareStr(const S1, S2: AnsiString): Integer; overload;
function CompareStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Integer; overload;

{ SameStr compares S1 to S2, with case-sensitivity. Returns true if
  S1 and S2 are the equal, that is, if CompareStr would return 0. }

function SameStr(const S1, S2: AnsiString): Boolean; overload;
function SameStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Boolean; overload;

{ CompareText compares S1 to S2, without case-sensitivity. The return value
  is the same as for CompareStr. The compare operation is based on the 8-bit
  ordinal value of each character, after converting 'a'..'z' to 'A'..'Z',
  and is not affected by the current user locale. }

function CompareText(const S1, S2: AnsiString): Integer; overload;
function CompareText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Integer; overload;

{ SameText compares S1 to S2, without case-sensitivity. Returns true if
  S1 and S2 are the equal, that is, if CompareText would return 0. SameText
  has the same 8-bit limitations as CompareText }

function SameText(const S1, S2: AnsiString): Boolean; overload;
function SameText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Boolean; overload;

{ AnsiUpperCase converts all characters in the given AnsiString to upper case.
  The conversion uses the current user locale. }

function AnsiUpperCase(const S: AnsiString): AnsiString; overload;

{ AnsiLowerCase converts all characters in the given AnsiString to lower case.
  The conversion uses the current user locale. }

function AnsiLowerCase(const S: AnsiString): AnsiString; overload;

{ AnsiCompareStr compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is the same as for CompareStr. }

function AnsiCompareStr(const S1, S2: AnsiString): Integer; inline; overload;

{ AnsiSameStr compares S1 to S2, with case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is True if AnsiCompareStr would have returned 0. }

function AnsiSameStr(const S1, S2: AnsiString): Boolean; inline; overload;

{ AnsiCompareText compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is the same as for CompareStr. }

function AnsiCompareText(const S1, S2: AnsiString): Integer; inline; overload;

{ AnsiSameText compares S1 to S2, without case-sensitivity. The compare
  operation is controlled by the current user locale. The return value
  is True if AnsiCompareText would have returned 0. }

function AnsiSameText(const S1, S2: AnsiString): Boolean; inline; overload;

{ AnsiLastChar returns a pointer to the last full character in the AnsiString.
  This function supports multibyte characters  }

function AnsiLastChar(const S: AnsiString): PAnsiChar; overload;

{ Trim trims leading and trailing spaces and control characters from the
  given AnsiString. }

function Trim(const S: AnsiString): AnsiString; overload;

{ TrimLeft trims leading spaces and control characters from the given
  AnsiString. }

function TrimLeft(const S: AnsiString): AnsiString; overload;

{ TrimRight trims trailing spaces and control characters from the given
  AnsiString. }

function TrimRight(const S: AnsiString): AnsiString; overload;

{ QuotedStr returns the given AnsiString as a quoted AnsiString. A single quote
  character is inserted at the beginning and the end of the AnsiString, and
  for each single quote character in the AnsiString, another one is added. }

function QuotedStr(const S: AnsiString): AnsiString; overload;

{ AnsiQuotedStr returns the given AnsiString as a quoted AnsiString, using the
  provided Quote character.  A Quote character is inserted at the beginning
  and end of the AnsiString, and each Quote character in the AnsiString is doubled.
  This function supports multibyte character AnsiStrings (MBCS). }

function AnsiQuotedStr(const S: AnsiString; Quote: AnsiChar): AnsiString; overload;

{ AnsiDequotedStr is a simplified version of AnsiExtractQuotedStr }

function AnsiDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString; overload;

{ CharLength returns the number of bytes required by the character starting
  at bytes S[Index].  }

function CharLength(const S: AnsiString; Index: Integer): Integer; overload;

{ NextCharIndex returns the byte index of the first byte of the character
  following the character starting at S[Index].  }

function NextCharIndex(const S: AnsiString; Index: Integer): Integer; overload;

{ AnsiCompareFileName supports DOS file name comparison idiosyncracies
  in Far East locales (Zenkaku) on Windows.
  In non-MBCS locales on Windows, AnsiCompareFileName is identical to
  AnsiCompareText (case insensitive).
  On Linux, AnsiCompareFileName is identical to AnsiCompareStr (case sensitive).
  For general purpose file name comparisions, you should use this function
  instead of AnsiCompareText. }

function AnsiCompareFileName(const S1, S2: AnsiString): Integer; {$IFNDEF MACOS}inline;{$ENDIF} overload;

function SameFileName(const S1, S2: AnsiString): Boolean; inline; overload;

{ AnsiLowerCaseFileName is identical to AnsiLowerCase. }

function AnsiLowerCaseFileName(const S: AnsiString): AnsiString; overload; deprecated 'Use AnsiLowerCase instead';

{ AnsiUpperCaseFileName is identical to AnsiUpperCase. }

function AnsiUpperCaseFileName(const S: AnsiString): AnsiString; overload; deprecated 'Use AnsiUpperCase instead';

{ AnsiPos:  Same as Pos but supports MBCS AnsiStrings }

function AnsiPos(const Substr, S: AnsiString): Integer; overload;

{ From StrUtils }

{ AnsiContainsText returns true if the subtext is found, without
  case-sensitivity, in the given text }

function ContainsText(const AText, ASubText: AnsiString): Boolean; inline; overload;
function AnsiContainsText(const AText, ASubText: AnsiString): Boolean; overload;

{ AnsiStartsText & AnsiEndText return true if the leading or trailing part
  of the given text matches, without case-sensitivity, the subtext }

function StartsText(const ASubText, AText: AnsiString): Boolean; inline; overload;
function AnsiStartsText(const ASubText, AText: AnsiString): Boolean; overload;

function EndsText(const ASubText, AText: AnsiString): Boolean; inline; overload;
function AnsiEndsText(const ASubText, AText: AnsiString): Boolean; overload;

{ AnsiReplaceText will replace all occurrences of a substring, without
  case-sensitivity, with another substring (recursion substring replacement
  is not supported) }

function ReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString; inline; overload;
function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString; overload;

{ AnsiMatchText & AnsiIndexText provide case like function for dealing with
  strings }

function MatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;
function AnsiMatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;

function IndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;
function AnsiIndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;

{ These function are similar to some of the above but are case-sensitive }

function ContainsStr(const AText, ASubText: AnsiString): Boolean; inline; overload;
function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean; overload;

function StartsStr(const ASubText, AText: AnsiString): Boolean; inline; overload;
function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean; overload;

function EndsStr(const ASubText, AText: AnsiString): Boolean; inline; overload;
function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean; overload;

function ReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString; inline; overload;
function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString; overload;

function MatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;
function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;

function IndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;
function AnsiIndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;

{ DupeString will return N copies of the given AnsiString }

function DupeString(const AText: AnsiString; ACount: Integer): AnsiString; overload;

{ ReverseString simply reverses the given AnsiString }

function ReverseString(const AText: AnsiString): AnsiString; overload;
function AnsiReverseString(const AText: AnsiString): AnsiString; overload;

{ StuffString replaces a segment of a AnsiString with another one }

function StuffString(const AText: AnsiString; AStart, ALength: Cardinal;
  const ASubText: AnsiString): AnsiString; overload;

{ RandomFrom will randomly return one of the given strings }

function RandomFrom(const AValues: array of AnsiString): AnsiString; overload;

function StringReplace(const S, OldPattern, NewPattern: AnsiString;
  Flags: TReplaceFlags): AnsiString; overload;


{ ChangeFileExt changes the extension of a filename. FileName specifies a
  filename with or without an extension, and Extension specifies the new
  extension for the filename. The new extension can be a an empty AnsiString or
  a period followed by up to three characters. }

function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString; overload;

{ ChangeFilePath changes the path of a filename. FileName specifies a
  filename with or without an extension, and Path specifies the new
  path for the filename. The new path is not required to contain the trailing
  path delimiter. }

function ChangeFilePath(const FileName, Path: AnsiString): AnsiString; overload;

{ ExtractFilePath extracts the drive and directory parts of the given
  filename. The resulting AnsiString is the leftmost characters of FileName,
  up to and including the colon or backslash that separates the path
  information from the name and extension. The resulting AnsiString is empty
  if FileName contains no drive and directory parts. }

function ExtractFilePath(const FileName: AnsiString): AnsiString; overload;

{ ExtractFileDir extracts the drive and directory parts of the given
  filename. The resulting AnsiString is a directory name suitable for passing
  to SetCurrentDir, CreateDir, etc. The resulting AnsiString is empty if
  FileName contains no drive and directory parts. }

function ExtractFileDir(const FileName: AnsiString): AnsiString; overload;

{ ExtractFileDrive extracts the drive part of the given filename.  For
  filenames with drive letters, the resulting AnsiString is '<drive>:'.
  For filenames with a UNC path, the resulting AnsiString is in the form
  '\\<servername>\<sharename>'.  If the given path contains neither
  style of filename, the result is an empty AnsiString. }

function ExtractFileDrive(const FileName: AnsiString): AnsiString; overload;

{ ExtractFileName extracts the name and extension parts of the given
  filename. The resulting AnsiString is the leftmost characters of FileName,
  starting with the first character after the colon or backslash that
  separates the path information from the name and extension. The resulting
  AnsiString is equal to FileName if FileName contains no drive and directory
  parts. }

function ExtractFileName(const FileName: AnsiString): AnsiString; overload;

{ ExtractFileExt extracts the extension part of the given filename. The
  resulting AnsiString includes the period character that separates the name
  and extension parts. The resulting AnsiString is empty if the given filename
  has no extension. }

function ExtractFileExt(const FileName: AnsiString): AnsiString; overload;

{ ExpandFileName expands the given filename to a fully qualified filename.
  The resulting AnsiString consists of a drive letter, a colon, a root relative
  directory path, and a filename. Embedded '.' and '..' directory references
  are removed. }

function ExpandFileName(const FileName: AnsiString): AnsiString; overload;

{ ExpandFilenameCase returns a fully qualified filename like ExpandFilename,
  but performs a case-insensitive filename search looking for a close match
  in the actual file system, differing only in uppercase versus lowercase of
  the letters.  This is useful to convert lazy user input into useable file
  names, or to convert filename data created on a case-insensitive file
  system (Win32) to something useable on a case-sensitive file system (Linux).

  The MatchFound out parameter indicates what kind of match was found in the
  file system, and what the function result is based upon:

  ( in order of increasing difficulty or complexity )
  mkExactMatch:  Case-sensitive match.  Result := ExpandFileName(FileName).
  mkSingleMatch: Exactly one file in the given directory path matches the
        given filename on a case-insensitive basis.
        Result := ExpandFileName(FileName as found in file system).
  mkAmbiguous: More than one file in the given directory path matches the
        given filename case-insensitively.
        In many cases, this should be considered an error.
        Result := ExpandFileName(First matching filename found).
  mkNone:  File not found at all.  Result := ExpandFileName(FileName).

  Note that because this function has to search the file system it may be
  much slower than ExpandFileName, particularly when the given filename is
  ambiguous or does not exist.  Use ExpandFilenameCase only when you have
  a filename of dubious orgin - such as from user input - and you want
  to make a best guess before failing.  }

type
  TFilenameCaseMatch = (mkNone, mkExactMatch, mkSingleMatch, mkAmbiguous);

function ExpandFileNameCase(const FileName: AnsiString;
  out MatchFound: TFilenameCaseMatch): AnsiString; overload;

{ ExpandUNCFileName expands the given filename to a fully qualified filename.
  This function is the same as ExpandFileName except that it will return the
  drive portion of the filename in the format '\\<servername>\<sharename> if
  that drive is actually a network resource instead of a local resource.
  Like ExpandFileName, embedded '.' and '..' directory references are
  removed. }

function ExpandUNCFileName(const FileName: AnsiString): AnsiString; overload;

{ ExtractRelativePath will return a file path name relative to the given
  BaseName.  It strips the common path dirs and adds '..\' on Windows,
  and '../' on Linux for each level up from the BaseName path. }

function ExtractRelativePath(const BaseName, DestName: AnsiString): AnsiString; overload;

{$IFDEF MSWINDOWS}
{ ExtractShortPathName will convert the given filename to the short form
  by calling the GetShortPathName API.  Will return an empty AnsiString if
  the file or directory specified does not exist }

function ExtractShortPathName(const FileName: AnsiString): AnsiString; overload;
{$ENDIF}

{ LastDelimiter returns the byte index in S of the rightmost whole
  character that matches any character in Delimiters (except null (#0)).
  S may contain multibyte characters; Delimiters must contain only single
  byte non-null characters.
  Example: LastDelimiter('\.:', 'c:\filename.ext') returns 12. }

function LastDelimiter(const Delimiters, S: AnsiString): Integer; overload;


{ IsPathDelimiter returns True if the character at byte S[Index]
  is a PathDelimiter ('\' or '/'), and it is not a MBCS lead or trail byte. }

function IsPathDelimiter(const S: AnsiString; Index: Integer): Boolean; overload;

{ IsDelimiter returns True if the character at byte S[Index] matches any
  character in the Delimiters AnsiString, and the character is not a MBCS lead or
  trail byte.  S may contain multibyte characters; Delimiters must contain
  only single byte characters. }

function IsDelimiter(const Delimiters, S: AnsiString; Index: Integer): Boolean; overload;

{ IncludeTrailingPathDelimiter returns the path with a PathDelimiter
  ('/' or '\') at the end.  This function is MBCS enabled. }

function IncludeTrailingPathDelimiter(const S: AnsiString): AnsiString; overload;

{ IncludeTrailingBackslash is the old name for IncludeTrailingPathDelimiter. }

function IncludeTrailingBackslash(const S: AnsiString): AnsiString; platform; overload; inline;

{ ExcludeTrailingPathDelimiter returns the path without a PathDelimiter
  ('\' or '/') at the end.  This function is MBCS enabled. }

function ExcludeTrailingPathDelimiter(const S: AnsiString): AnsiString; overload;

{ ExcludeTrailingBackslash is the old name for ExcludeTrailingPathDelimiter. }

function ExcludeTrailingBackslash(const S: AnsiString): AnsiString; platform; overload; inline;

function PosEx(const SubStr, S: AnsiString; Offset: Integer = 1): Integer; overload;

{ String formatting routines }

{ The Format routine formats the argument list given by the Args parameter
  using the format string given by the Format parameter.

  Format strings contain two types of objects--plain characters and format
  specifiers. Plain characters are copied verbatim to the resulting string.
  Format specifiers fetch arguments from the argument list and apply
  formatting to them.

  Format specifiers have the following form:

    "%" [index ":"] ["-"] [width] ["." prec] type

  A format specifier begins with a % character. After the % come the
  following, in this order:

  -  an optional argument index specifier, [index ":"]
  -  an optional left-justification indicator, ["-"]
  -  an optional width specifier, [width]
  -  an optional precision specifier, ["." prec]
  -  the conversion type character, type

  The following conversion characters are supported:

  d  Decimal. The argument must be an integer value. The value is converted
     to a string of decimal digits. If the format string contains a precision
     specifier, it indicates that the resulting string must contain at least
     the specified number of digits; if the value has less digits, the
     resulting string is left-padded with zeros.

  u  Unsigned decimal.  Similar to 'd' but no sign is output.

  e  Scientific. The argument must be a floating-point value. The value is
     converted to a string of the form "-d.ddd...E+ddd". The resulting
     string starts with a minus sign if the number is negative, and one digit
     always precedes the decimal point. The total number of digits in the
     resulting string (including the one before the decimal point) is given
     by the precision specifer in the format string--a default precision of
     15 is assumed if no precision specifer is present. The "E" exponent
     character in the resulting string is always followed by a plus or minus
     sign and at least three digits.

  f  Fixed. The argument must be a floating-point value. The value is
     converted to a string of the form "-ddd.ddd...". The resulting string
     starts with a minus sign if the number is negative. The number of digits
     after the decimal point is given by the precision specifier in the
     format string--a default of 2 decimal digits is assumed if no precision
     specifier is present.

  g  General. The argument must be a floating-point value. The value is
     converted to the shortest possible decimal string using fixed or
     scientific format. The number of significant digits in the resulting
     string is given by the precision specifier in the format string--a
     default precision of 15 is assumed if no precision specifier is present.
     Trailing zeros are removed from the resulting string, and a decimal
     point appears only if necessary. The resulting string uses fixed point
     format if the number of digits to the left of the decimal point in the
     value is less than or equal to the specified precision, and if the
     value is greater than or equal to 0.00001. Otherwise the resulting
     string uses scientific format.

  n  Number. The argument must be a floating-point value. The value is
     converted to a string of the form "-d,ddd,ddd.ddd...". The "n" format
     corresponds to the "f" format, except that the resulting string
     contains thousand separators.

  m  Money. The argument must be a floating-point value. The value is
     converted to a string that represents a currency amount. The conversion
     is controlled by the CurrencyString, CurrencyFormat, NegCurrFormat,
     ThousandSeparator, DecimalSeparator, and CurrencyDecimals global
     variables, all of which are initialized from locale settings provided
     by the operating system.  For example, Currency Format preferences can be
     set in the International section of the Windows Control Panel. If the format
     string contains a precision specifier, it overrides the value given
     by the CurrencyDecimals global variable.

  p  Pointer. The argument must be a pointer value. The value is converted
     to a string of the form "XXXX:YYYY" where XXXX and YYYY are the
     segment and offset parts of the pointer expressed as four hexadecimal
     digits.

  s  String. The argument must be a character, a string, or a PChar value.
     The string or character is inserted in place of the format specifier.
     The precision specifier, if present in the format string, specifies the
     maximum length of the resulting string. If the argument is a string
     that is longer than this maximum, the string is truncated.

  x  Hexadecimal. The argument must be an integer value. The value is
     converted to a string of hexadecimal digits. If the format string
     contains a precision specifier, it indicates that the resulting string
     must contain at least the specified number of digits; if the value has
     less digits, the resulting string is left-padded with zeros.

  Conversion characters may be specified in upper case as well as in lower
  case--both produce the same results.

  For all floating-point formats, the actual characters used as decimal and
  thousand separators are obtained from the DecimalSeparator and
  ThousandSeparator global variables.

  Index, width, and precision specifiers can be specified directly using
  decimal digit string (for example "%10d"), or indirectly using an asterisk
  charcater (for example "%*.*f"). When using an asterisk, the next argument
  in the argument list (which must be an integer value) becomes the value
  that is actually used. For example "Format('%*.*f', [8, 2, 123.456])" is
  the same as "Format('%8.2f', [123.456])".

  A width specifier sets the minimum field width for a conversion. If the
  resulting string is shorter than the minimum field width, it is padded
  with blanks to increase the field width. The default is to right-justify
  the result by adding blanks in front of the value, but if the format
  specifier contains a left-justification indicator (a "-" character
  preceding the width specifier), the result is left-justified by adding
  blanks after the value.

  An index specifier sets the current argument list index to the specified
  value. The index of the first argument in the argument list is 0. Using
  index specifiers, it is possible to format the same argument multiple
  times. For example "Format('%d %d %0:d %d', [10, 20])" produces the string
  '10 20 10 20'.

  The Format function can be combined with other formatting functions. For
  example

    S := Format('Your total was %s on %s', [
      FormatFloat('$#,##0.00;;zero', Total),
      FormatDateTime('mm/dd/yy', Date)]);

  which uses the FormatFloat and FormatDateTime functions to customize the
  format beyond what is possible with Format.

  Each of the string formatting routines that uses global variables for
  formatting (separators, decimals, date/time formats etc.), has an
  overloaded equivalent requiring a parameter of type TFormatSettings. This
  additional parameter provides the formatting information rather than the
  global variables. For more information see the notes at TFormatSettings.  }

function Format(const Format: AnsiString;
  const Args: array of const): AnsiString; overload;
function Format(const Format: AnsiString; const Args: array of const;
  const AFormatSettings: TFormatSettings): AnsiString; overload;

{ FmtStr formats the argument list given by Args using the format string
  given by Format into the string variable given by Result. For further
  details, see the description of the Format function. }

procedure FmtStr(var Result: AnsiString; const Format: AnsiString;
  const Args: array of const); overload;
procedure FmtStr(var Result: AnsiString; const Format: AnsiString;
  const Args: array of const; const AFormatSettings: TFormatSettings); overload;

{ FormatBuf formats the argument list given by Args using the format string
  given by Format and FmtLen into the buffer given by Buffer and BufLen.
  The Format parameter is a reference to a buffer containing FmtLen
  characters, and the Buffer parameter is a reference to a buffer of BufLen
  characters. The returned value is the number of characters actually stored
  in Buffer. The returned value is always less than or equal to BufLen. For
  further details, see the description of the Format function. }

function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal; overload;
function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const AFormatSettings: TFormatSettings): Cardinal; overload;

function AnsiLeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
function AnsiRightStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; overload;

implementation

{$IFDEF MACOS}
{ Mac OS/X ABI requires stack to be aligned to 16 bytes at the
  point of a function call. }
{$DEFINE ALIGN_STACK}
{$ENDIF}

{$IFDEF MACOS}
uses
  Macapi.CoreFoundation, Posix.Dirent;
{$ENDIF MACOS}


function Format(const Format: AnsiString; const Args: array of const): AnsiString;
begin
  Result := System.AnsiStrings.Format(Format, Args, FormatSettings);
end;

function Format(const Format: AnsiString; const Args: array of const;
  const AFormatSettings: TFormatSettings): AnsiString;
begin
  FmtStr(Result, Format, Args, AFormatSettings);
end;

procedure FmtStr(var Result: AnsiString; const Format: AnsiString;
  const Args: array of const);
begin
  FmtStr(Result, Format, Args, FormatSettings);
end;

procedure FmtStr(var Result: AnsiString; const Format: AnsiString;
  const Args: array of const; const AFormatSettings: TFormatSettings);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of AnsiChar;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := AnsiFormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format),
      Args, AFormatSettings)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := AnsiFormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
        Length(Format), Args, AFormatSettings);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;


function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
begin
  Result := AnsiFormatBuf(Buffer, BufLen, Format, FmtLen, Args, FormatSettings);
end;

function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const AFormatSettings: TFormatSettings): Cardinal;
begin
  Result := FormatBuf(Buffer, BufLen, Format, FmtLen, Args, AFormatSettings);
end;

function StringReplace(const S, OldPattern, NewPattern: AnsiString;
  Flags: TReplaceFlags): AnsiString;
var
  SearchStr, Patt, NewStr: AnsiString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function UpperCase(const S: AnsiString): AnsiString;
{$IFDEF PUREPASCAL}
var
  L, I: Integer;
begin
  L := Length(S);
  SetLength(Result, L);
  SetCodePage(RawByteString(Result), StringCodePage(S), False);

  for I := 1 to L do
    if S[I] in ['a' .. 'z'] then
      Result[I] := AnsiChar(Byte(S[I]) - $20)
    else
      Result[I] := S[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function UpperCase is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
asm {Size = 134 Bytes}
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  movzx   ecx, word ptr [esi-12] {Set code page}
  call    system.@LStrSetLength  {Create Result AnsiString}
  mov     edi, [edi]             {@Result}
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of AnsiString}
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $7B7B7B7B         {Set High Bit if Original <= Ord('z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('a')}
  and     eax, edx               {80h if Orig in 'a'..'z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  sub     ecx, eax               {Clear Bit 5 if Original in 'a'..'z'}
  mov     [edi+ebx-4], ecx
  sub     ebx, 1
  and     ebx, -4
  jmp     @@CheckDone
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Loop:                          {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $7B7B7B7B         {Set High Bit if Original <= Ord('z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('a')}
  and     eax, edx               {80h if Orig in 'a'..'z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  sub     ecx, eax               {Clear Bit 5 if Original in 'a'..'z'}
  mov     [edi+ebx], ecx
@@CheckDone:
  sub     ebx, 4
  jnc     @@Loop
  pop     esi
  pop     edi
  pop     ebx
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function UpperCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString;
begin
  if LocaleOptions = loUserLocale then
    Result := AnsiUpperCase(S)
  else
    Result := UpperCase(S);
end;

function LowerCase(const S: AnsiString): AnsiString;
{$IFDEF PUREPASCAL}
var
  L, I: Integer;
begin
  L := Length(S);
  SetLength(Result, L);
  SetCodePage(RawByteString(Result), StringCodePage(S), False);

  for I := 1 to L do
    if S[I] in ['A' .. 'Z'] then
      Result[I] := AnsiChar(Byte(S[I]) + $20)
    else
      Result[I] := S[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function LowerCase is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
asm {Size = 134 Bytes}
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  movzx   ecx, word ptr [esi-12] {Set code page}
  call    system.@LStrSetLength  {Create Result AnsiString}
  mov     edi, [edi]             {@Result}
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of AnsiString}
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $5B5B5B5B         {Set High Bit if Original <= Ord('Z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('A')}
  and     eax, edx               {80h if Orig in 'A'..'Z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  add     ecx, eax               {Set Bit 5 if Original in 'A'..'Z'}
  mov     [edi+ebx-4], ecx
  sub     ebx, 1
  and     ebx, -4
  jmp     @@CheckDone
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Loop:                          {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $5B5B5B5B         {Set High Bit if Original <= Ord('Z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('A')}
  and     eax, edx               {80h if Orig in 'A'..'Z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  add     ecx, eax               {Set Bit 5 if Original in 'A'..'Z'}
  mov     [edi+ebx], ecx
@@CheckDone:
  sub     ebx, 4
  jnc     @@Loop
  pop     esi
  pop     edi
  pop     ebx
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function LowerCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString;
begin
  if LocaleOptions = loUserLocale then
    Result := AnsiLowerCase(S)
  else
    Result := LowerCase(S);
end;

function CompareStr(const S1, S2: AnsiString): Integer;
{$IFDEF PUREPASCAL}
var
  P1, P2: PAnsiChar;
  I: Integer;
  L1, L2: Integer;
begin
  { Length and PChar of S1 }
  L1 := Length(S1);
  P1 := PAnsiChar(S1);

  { Length and PChar of S2 }
  L2 := Length(S2);
  P2 := PAnsiChar(S2);

  { Continue the loop until the end of one string is reached. }
  I := 0;
  while (I < L1) and (I < L2) do
  begin
    if (P1^ <> P2^) then
      Exit(Ord(P1^) - Ord(P2^));

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;

  { If chars were not different return the difference in length }
  Result := L1 - L2;
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function CompareStr is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2007
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Pierre le Riche
 *
 * ***** END LICENSE BLOCK ***** *)
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in eax:
       0 if S1 = S2,
       > 0 if S1 > S2,
       < 0 if S1 < S2
   Code size:
     101 bytes}
  cmp eax, edx
  je @SameAnsiString
  {Is either of the AnsiStrings perhaps nil?}
  test eax, edx
  jz @PossibleNilAnsiString
  {Compare the first four characters (there has to be a trailing #0). In random
   AnsiString compares this can save a lot of CPU time.}
@BothNonNil:
  {Compare the first character}
  movzx ecx, byte ptr [edx]
  cmp cl, [eax]
  je @FirstCharacterSame
  {First character differs}
  movzx eax, byte ptr [eax]
  sub eax, ecx
  ret
@FirstCharacterSame:
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  xor ecx, ecx
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) < length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  add ecx, 4
  js @CompareLoop
  {All characters match - return the difference in length}
@MatchUpToLength:
  pop eax
  pop ebx
  ret
@Mismatch:
  bsf ebx, ebx
  shr ebx, 3
  add ecx, ebx
  jns @MatchUpToLength
  movzx eax, byte ptr [eax + ecx]
  movzx edx, byte ptr [edx + ecx]
  sub eax, edx
  pop ebx
  pop ebx
  ret
  {It is the same AnsiString}
@SameAnsiString:
  xor eax, eax
  ret
  {Good possibility that at least one of the AnsiStrings are nil}
@PossibleNilAnsiString:
  test eax, eax
  jz @FirstAnsiStringNil
  test edx, edx
  jnz @BothNonNil
  {Return first AnsiString length: second AnsiString is nil}
  mov eax, [eax - 4]
  ret
@FirstAnsiStringNil:
  {Return 0 - length(S2): first AnsiString is nil}
  sub eax, [edx - 4]
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function CompareStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Integer;
begin
  if LocaleOptions = loUserLocale then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := CompareStr(S1, S2);
end;

function CompareStrProxy(const S1, S2: AnsiString): Integer; inline;
begin
  Result := CompareStr(S1, S2);
end;

function SameStr(const S1, S2: AnsiString): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := CompareStr(S1, S2) = 0;
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
asm //StackAligned
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        CALL    CompareStrProxy
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function SameStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Boolean;
begin
  if LocaleOptions = loUserLocale then
    Result := AnsiSameStr(S1, S2)
  else
    Result := SameStr(S1, S2);
end;

function CompareText(const S1, S2: AnsiString): Integer;
{$IFDEF PUREPASCAL}
var
  P1, P2: PAnsiChar;
  I: Integer;
  C1, C2: AnsiChar;
  L1, L2: Integer;
begin
  { Length and PChar of S1 }
  L1 := Length(S1);
  P1 := PAnsiChar(S1);

  { Length and PChar of S2 }
  L2 := Length(S2);
  P2 := PAnsiChar(S2);

  { Continue the loop until the end of one string is reached. }
  I := 0;
  while (I < L1) and (I < L2) do
  begin
    if P1^ in ['a'..'z'] then
      C1 := AnsiChar(Byte(P1^) xor $20)
    else
      C1 := P1^;

    if P2^ in ['a'..'z'] then
      C2 := AnsiChar(Byte(P2^) xor $20)
    else
      C2 := P2^;

    if (C1 <> C2) then
      Exit(Ord(C1) - Ord(C2));

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;

  { If chars were not different return the difference in length }
  Result := L1 - L2;
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function CompareText is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
asm
        TEST   EAX, EAX
        JNZ    @@CheckS2
        TEST   EDX, EDX
        JZ     @@Ret
        MOV    EAX, [EDX-4]
        NEG    EAX
@@Ret:
        RET
@@CheckS2:
        TEST   EDX, EDX
        JNZ    @@Compare
        MOV    EAX, [EAX-4]
        RET
@@Compare:
        PUSH   EBX
        PUSH   EBP
        PUSH   ESI
        MOV    EBP, [EAX-4]     // length(S1)
        MOV    EBX, [EDX-4]     // length(S2)
        SUB    EBP, EBX         // Result if All Compared Characters Match
        SBB    ECX, ECX
        AND    ECX, EBP
        ADD    ECX, EBX         // min(length(S1),length(S2)) = Compare Length
        LEA    ESI, [EAX+ECX]   // Last Compare Position in S1
        ADD    EDX, ECX         // Last Compare Position in S2
        NEG    ECX
        JZ     @@SetResult      // Exit if Smallest Length = 0
@@Loop:                         // Load Next 2 Chars from S1 and S2
                                // May Include Null Terminator}
        MOVZX  EAX, WORD PTR [ESI+ECX]
        MOVZX  EBX, WORD PTR [EDX+ECX]
        CMP    EAX, EBX
        JE     @@Next           // Next 2 Chars Match
        CMP    AL, BL
        JE     @@SecondPair     // First AnsiChar Matches
        MOV    AH, 0
        MOV    BH, 0
        CMP    AL, 'a'
        JL     @@UC1
        CMP    AL, 'z'
        JG     @@UC1
        SUB    EAX, 'a'-'A'
@@UC1:
        CMP    BL, 'a'
        JL     @@UC2
        CMP    BL, 'z'
        JG     @@UC2
        SUB    EBX, 'a'-'A'
@@UC2:
        SUB    EAX, EBX         // Compare Both Uppercase Chars
        JNE    @@Done           // Exit with Result in EAX if Not Equal
        MOVZX  EAX, WORD PTR [ESI+ECX] // Reload Same 2 Chars from S1
        MOVZX  EBX, WORD PTR [EDX+ECX] // Reload Same 2 Chars from S2
        CMP    AH, BH
        JE     @@Next           // Second AnsiChar Matches
@@SecondPair:
        SHR    EAX, 8
        SHR    EBX, 8
        CMP    AL, 'a'
        JL     @@UC3
        CMP    AL, 'z'
        JG     @@UC3
        SUB    EAX, 'a'-'A'
@@UC3:
        CMP    BL, 'a'
        JL     @@UC4
        CMP    BL, 'z'
        JG     @@UC4
        SUB    EBX, 'a'-'A'
@@UC4:
        SUB    EAX, EBX         // Compare Both Uppercase Chars
        JNE    @@Done           // Exit with Result in EAX if Not Equal
@@Next:
        ADD    ECX, 2
        JL     @@Loop           // Loop until All required Chars Compared
@@SetResult:
        MOV    EAX, EBP         // All Matched, Set Result from Lengths
@@Done:
        POP    ESI
        POP    EBP
        POP    EBX
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function CompareText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Integer;
begin
  if LocaleOptions = loUserLocale then
    Result := AnsiCompareText(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

function CompareTextProxy(const S1, S2: AnsiString): Integer; inline;
begin
  Result := CompareText(S1, S2);
end;

function SameText(const S1, S2: AnsiString): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := CompareText(S1, S2) = 0;
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
asm //StackAligned
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        CALL    CompareTextProxy
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function SameText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Boolean;
begin
  if LocaleOptions = loUserLocale then
    Result := AnsiSameText(S1, S2)
  else
    Result := SameText(S1, S2);
end;

function AnsiUpperCase(const S: AnsiString): AnsiString;
var
  Len: Integer;
{$IFDEF MSWINDOWS}
begin
  Len := Length(S);
  SetString(Result, PAnsiChar(S), Len);
  if Len > 0 then CharUpperBuffA(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Len := Length(S);
  SetString(Result, PAnsiChar(S), Len);
  if Len > 0 then Result := AnsiStrUpper(PAnsiChar(Result));
end;
{$ENDIF}

function AnsiLowerCase(const S: AnsiString): AnsiString;
var
  Len: Integer;
{$IFDEF MSWINDOWS}
begin
  Len := Length(S);
  SetString(Result, PAnsiChar(S), Len);
  if Len > 0 then CharLowerBuffA(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Len := Length(S);
  SetString(Result, PAnsiChar(S), Len);
  if Len > 0 then Result := AnsiStrLower(PAnsiChar(Result));
end;
{$ENDIF}

function AnsiCompareStr(const S1, S2: AnsiString): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, 0, PAnsiChar(S1), Length(S1),
    PAnsiChar(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF POSIX}
  Result := strcoll_l(PAnsiChar(UTF8String(S1)), PAnsiChar(UTF8String(S2)), UTF8CompareLocale);
{$ENDIF}
end;

function AnsiSameStr(const S1, S2: AnsiString): Boolean;
begin
  Result := AnsiCompareStr(S1, S2) = 0;
end;

function AnsiCompareText(const S1, S2: AnsiString): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PAnsiChar(S1),
    Length(S1), PAnsiChar(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF POSIX}
  Result := System.SysUtils.AnsiCompareText(string(S1), string(S2));
{$ENDIF}
end;

function AnsiSameText(const S1, S2: AnsiString): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;

function AnsiLastChar(const S: AnsiString): PAnsiChar;
var
  LastByte: Integer;
begin
  LastByte := Length(S);
  if LastByte <> 0 then
  begin
    while ByteType(S, LastByte) = mbTrailByte do Dec(LastByte);
    Result := @S[LastByte];
  end
  else
    Result := nil;
end;

function Trim(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  if (L > 0) and (S[I] > ' ') and (S[L] > ' ') then Exit(S);
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Exit('');
  while S[L] <= ' ' do Dec(L);
  Result := Copy(S, I, L - I + 1);
end;

function TrimLeft(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I = 1 then Exit(S);
  Result := Copy(S, I, Maxint);
end;

function TrimRight(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := Length(S);
  if (I > 0) and (S[I] > ' ') then Exit(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function QuotedStr(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '''' then Insert('''', Result, I);
  Result := '''' + Result + '''';
end;

function AnsiQuotedStr(const S: AnsiString; Quote: AnsiChar): AnsiString;
var
  P, Src, Dest: PAnsiChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := AnsiStrScan(PAnsiChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := AnsiStrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := AnsiStrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := AnsiStrScan(Src, Quote);
  until P = nil;
  P := StrEnd(Src);
  Move(Src^, Dest^, P - Src);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function AnsiDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString;
var
  LText: PAnsiChar;
begin
  LText := PAnsiChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if ((Result = '') or (LText^ = #0)) and
     (Length(S) > 0) and ((S[1] <> AQuote) or (System.AnsiStrings.AnsiLastChar(S)^ <> AQuote)) then
    Result := S;
end;

function AdjustLineBreaks(const S: AnsiString; Style: TTextLineBreakStyle): AnsiString;
var
  Source, SourceEnd, Dest: PAnsiChar;
  DestLen: Integer;
  L: Integer;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  DestLen := Length(S);
  while Source < SourceEnd do
  begin
    case Source^ of
      #10:
        if Style = tlbsCRLF then
          Inc(DestLen);
      #13:
        if Style = tlbsCRLF then
          if Source[1] = #10 then
            Inc(Source)
          else
            Inc(DestLen)
        else
          if Source[1] = #10 then
            Dec(DestLen);
    else
      if Source^ in LeadBytes then
      begin
        Source := StrNextChar(Source);
        continue;
      end;
    end;
    Inc(Source);
  end;
  if DestLen = Length(Source) then
    Result := S
  else
  begin
    Source := Pointer(S);
    SetString(Result, nil, DestLen);
    Dest := Pointer(Result);
    while Source < SourceEnd do
      case Source^ of
        #10:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := #13;
              Inc(Dest);
            end;
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
          end;
        #13:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := #13;
              Inc(Dest);
            end;
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
            if Source^ = #10 then Inc(Source);
          end;
      else
        if Source^ in LeadBytes then
        begin
          L := StrCharLength(Source);
          Move(Source^, Dest^, L);
          Inc(Dest, L);
          Inc(Source, L);
          continue;
        end;
        Dest^ := Source^;
        Inc(Dest);
        Inc(Source);
      end;
  end;
end;

function IsValidIdent(const Ident: AnsiString; AllowDots: Boolean): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
  AlphaNumericDot = AlphaNumeric + ['.'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then Exit;
  if AllowDots then
    for I := 2 to Length(Ident) do
      begin
        if not (Ident[I] in AlphaNumericDot) then Exit
      end
  else
    for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;

function CharLength(const S: AnsiString; Index: Integer): Integer;
begin
  Result := 1;
  assert((Index > 0) and (Index <= Length(S)));
  if SysLocale.FarEast and (S[Index] in LeadBytes) then
    Result := StrCharLength(PAnsiChar(S) + Index - 1);
end;

function NextCharIndex(const S: AnsiString; Index: Integer): Integer;
begin
  Result := Index + 1;
  assert((Index > 0) and (Index <= Length(S)));
  if SysLocale.FarEast and (S[Index] in LeadBytes) then
    Result := Index + StrCharLength(PAnsiChar(S) + Index - 1);
end;

{$IFDEF MACOS}
function StringToFileSystemString(const Source: AnsiString;
  var Dest: array of AnsiChar): Boolean;
var
  LEncoding: CFStringEncoding;
  StringRef: CFStringRef;
begin
  // Find an appropriate CFString encoding based on the code page of the string.
  // If no match is found, convert the source string to Unicode.
  LEncoding := CFStringConvertWindowsCodepageToEncoding(StringCodePage(Source));
  if LEncoding <> kCFStringEncodingInvalidId then
    StringRef := CFStringCreateWithCString(kCFAllocatorDefault,
      PAnsiChar(Source), LEncoding)
  else
    StringRef := CFStringCreateWithCharacters(kCFAllocatorDefault,
      PChar(UnicodeString(Source)), Length(Source));

  if StringRef = nil then
    Exit(False);
  try
    Result := CFStringGetFileSystemRepresentation(StringRef, Dest, NAME_MAX + 1);
  finally
    CFRelease(StringRef);
  end;
end;
{$ENDIF MACOS}

function AnsiCompareFileName(const S1, S2: AnsiString): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := CompareStr(AnsiLowerCase(S1), AnsiLowerCase(S2));
end;
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}
var
  Buf1, Buf2: array[0..NAME_MAX + 1] of AnsiChar;
begin
  if StringToFileSystemString(AnsiLowerCase(S1), Buf1) and
     StringToFileSystemString(AnsiLowerCase(S2), Buf2) then
    Result := strcoll_l(Buf1, Buf2, UTF8CompareLocale)
  else
    Result := -1;
end;
{$ENDIF MACOS}
{$IFDEF LINUX}
begin
  Result := AnsiCompareStr(S1, S2);
end;
{$ENDIF LINUX}

function SameFileName(const S1, S2: AnsiString): Boolean;
begin
  Result := AnsiCompareFileName(S1, S2) = 0;
end;

function AnsiLowerCaseFileName(const S: AnsiString): AnsiString;
begin
  Result := AnsiLowerCase(S);
end;

function AnsiUpperCaseFileName(const S: AnsiString): AnsiString;
begin
  Result := AnsiUpperCase(S);
end;

function AnsiPos(const Substr, S: AnsiString): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  P := AnsiStrPos(PAnsiChar(S), PAnsiChar(SubStr));
  if P <> nil then
    Result := IntPtr(P) - IntPtr(PAnsiChar(S)) + 1;
end;

function ContainsText(const AText, ASubText: AnsiString): Boolean;
begin
  Result := AnsiContainsText(AText, ASubText);
end;

function AnsiContainsText(const AText, ASubText: AnsiString): Boolean;
begin
  Result := AnsiPos(AnsiUppercase(ASubText), AnsiUppercase(AText)) > 0;
end;

function StartsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result := AnsiStartsText(ASubText, AText);
end;

function AnsiStartsText(const ASubText, AText: AnsiString): Boolean;
var
{$IFDEF MSWINDOWS}
  P: PAnsiChar;
{$ENDIF}
  L, L2: Integer;
begin
{$IFDEF MSWINDOWS}
  P := PAnsiChar(AText);
{$ENDIF}
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then
    Result := False
  else
{$IFDEF MSWINDOWS}
    Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      P, L, PAnsiChar(ASubText), L) = 2;
{$ENDIF}
{$IFDEF POSIX}
    Result := AnsiSameText(ASubText, Copy(AText, 1, L));
{$ENDIF}
end;

function EndsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result := AnsiEndsText(ASubText, AText);
end;

function AnsiEndsText(const ASubText, AText: AnsiString): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') and
     (ByteType(AText, SubTextLocation) <> mbTrailByte) then
    Result := AnsiStrIComp(Pointer(ASubText), PAnsiChar(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;

function ReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result := AnsiReplaceStr(AText, AFromText, AToText);
end;

function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function ReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result := AnsiReplaceText(AText, AFromText, AToText);
end;

function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

function MatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result := AnsiMatchText(AText, AValues);
end;

function AnsiMatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result := AnsiIndexText(AText, AValues) <> -1;
end;

function IndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer;
begin
  Result := AnsiIndexText(AText, AValues);
end;

function AnsiIndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
    if AnsiSameText(AText, AValues[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function ContainsStr(const AText, ASubText: AnsiString): Boolean;
begin
  Result := AnsiContainsStr(AText, ASubText);
end;

function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean;
begin
  Result := AnsiPos(ASubText, AText) > 0;
end;

function StartsStr(const ASubText, AText: AnsiString): Boolean;
begin
  Result := AnsiStartsStr(ASubText, AText);
end;

function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean;
begin
  Result := AnsiSameStr(ASubText, Copy(AText, 1, Length(ASubText)));
end;

function EndsStr(const ASubText, AText: AnsiString): Boolean;
begin
  Result := AnsiEndsStr(ASubText, AText);
end;

function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') and
     (ByteType(AText, SubTextLocation) <> mbTrailByte) then
    Result := AnsiStrComp(Pointer(ASubText), PAnsiChar(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;

function MatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result := AnsiMatchStr(AText, AValues);
end;

function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  Result := AnsiIndexStr(AText, AValues) <> -1;
end;

function IndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer;
begin
  Result := AnsiIndexStr(AText, AValues);
end;

function AnsiIndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
    if AnsiSameStr(AText, AValues[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function DupeString(const AText: AnsiString; ACount: Integer): AnsiString;
var
  P: PAnsiChar;
  C: Integer;
begin
  C := Length(AText);
  SetLength(Result, C * ACount);
  P := Pointer(Result);
  if P = nil then Exit;
  while ACount > 0 do
  begin
    Move(Pointer(AText)^, P^, C);
    Inc(P, C);
    Dec(ACount);
  end;
end;

function ReverseString(const AText: AnsiString): AnsiString;
var
  I: Integer;
  P: PAnsiChar;
begin
  SetLength(Result, Length(AText));
  P := PAnsiChar(Result);
  for I := Length(AText) downto 1 do
  begin
    P^ := AText[I];
    Inc(P);
  end;
end;

function AnsiReverseString(const AText: AnsiString): AnsiString;
var
  I: Integer;
  Len, CharByteLen: Integer;
  L, R, RR, Tail: PAnsiChar;
begin
  Len := Length(AText);
  SetLength(Result, Len);
  if AText = '' then exit;
  L := PAnsiChar(AText);
  Tail := L+Len;
  R := PAnsiChar(Result)+Len;

  while L < Tail do
  begin
    CharByteLen := 1;
    if L^ in LeadBytes then
      CharByteLen := StrCharLength(L);
    RR := R - CharByteLen;
    R := RR;
    for I := 0 to CharByteLen - 1 do
    begin
      RR^ := L^;
      Inc(L);
      Inc(RR);
    end;
  end;
end;

function StuffString(const AText: AnsiString; AStart, ALength: Cardinal;
  const ASubText: AnsiString): AnsiString;
begin
  Result := Copy(AText, 1, AStart - 1) +
            ASubText +
            Copy(AText, AStart + ALength, MaxInt);
end;

function RandomFrom(const AValues: array of AnsiString): AnsiString;
begin
  Result := AValues[Random(High(AValues) + 1)];
end;

function PosEx(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
{$IFDEF PUREPASCAL}
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PAnsiChar;
begin
  { Calculate the number of possible iterations. Not valid if Offset < 1. }
  LIterCnt := Length(S) - Offset - Length(SubStr) + 1;

  { Only continue if the number of iterations is positive or zero (there is space to check) }
  if (Offset > 0) and (LIterCnt >= 0) then
  begin
    L := Length(SubStr);
    PSubStr := PAnsiChar(SubStr);
    PS := PAnsiChar(S);
    Inc(PS, Offset - 1);

    for I := 0 to LIterCnt do
    begin
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        if (PS + I + J)^ = (PSubStr + J)^ then
          Inc(J)
        else
          J := -1;
      end;
      if J >= L then
        Exit(I + Offset);
    end;

    //for I := 0 to LIterCnt do
    //  if StrLComp(PS + I, PSubStr, L) = 0 then
    //    Exit(I + Offset);
  end;

  Result := 0;
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function PosEx is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first AnsiChar at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last AnsiChar at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last AnsiChar of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last AnsiChar of Substr

@Loop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTestT:
       add   edx, 4
       cmp   edx, [esp+8]
       jb   @Continue
@EndLoop:
       add   edx, -2
       cmp   edx, [esp+8]
       jb    @Loop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret
@Continue:
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jnz   @Loop
@Test1:
       add   edx,  1
@Test2:
       add   edx, -2
@Test0:
       add   edx, -1
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found
@AnsiString:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTestT
       cmp   esi, -2
       jge   @Found
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTestT
       add   esi, 4
       jl    @AnsiString
@Found:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter(AnsiString('.' + PathDelim + DriveDelim),Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ChangeFilePath(const FileName, Path: AnsiString): AnsiString;
begin
  Result := IncludeTrailingPathDelimiter(Path) + ExtractFileName(FileName);
end;

function ExtractFilePath(const FileName: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter(AnsiString(PathDelim + DriveDelim), FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDir(const FileName: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter(AnsiString(PathDelim + DriveDelim), Filename);
  if (I > 1) and (FileName[I] = PathDelim) and
    (not IsDelimiter( AnsiString(PathDelim + DriveDelim), FileName, I-1)) then 
    Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: AnsiString): AnsiString;
{$IFDEF MSWINDOWS}
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := '';  // Linux doesn't support drive letters
end;
{$ENDIF}

function ExtractFileName(const FileName: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter(AnsiString(PathDelim + DriveDelim), FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter(AnsiString('.' + PathDelim + DriveDelim), FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function ExpandFileName(const FileName: AnsiString): AnsiString;
{$IFDEF MSWINDOWS}
var
  FName: PAnsiChar;
  Buffer: array[0..MAX_PATH - 1] of AnsiChar;
  Len: Integer;
begin
  Len := GetFullPathNameA(PAnsiChar(FileName), Length(Buffer), Buffer, FName);
  if Len <= Length(Buffer) then
    SetString(Result, Buffer, Len)
  else if Len > 0 then
  begin
    SetLength(Result, Len);
    Len := GetFullPathNameA(PAnsiChar(FileName), Len, PAnsiChar(Result), FName);
    if Len < Length(Result) then
      SetLength(Result, Len);
  end;
end;
{$ENDIF}

{$IFDEF POSIX}
function ExpandTilde(const InString: AnsiString): AnsiString;
begin
  Result := AnsiString(GetHomePath()) + Copy(InString, 2, Length(InString));
end;

var
  I, J: Integer;
  LastWasPathDelim: Boolean;
  TempName: AnsiString;
begin
  { Special case for root path. '/', '/.' and '/..' refer to the same directory - '/' }
  if (Filename = PathDelim) or (Filename = PathDelim + '.') or (Filename = PathDelim + '..') then
    Exit(PathDelim)
  else
    Result := '';

  if Length(Filename) = 0 then Exit;

  if FileName[1] = PathDelim then
    TempName := FileName
  else
  begin
    TempName := FileName;
    if FileName[1] = '~' then
      TempName := ExpandTilde(TempName)
    else
      TempName := IncludeTrailingPathDelimiter(GetCurrentDir) + TempName;
  end;

  I := 1;
  J := 1;

  LastWasPathDelim := False;

  while I <= Length(TempName) do
  begin
    case TempName[I] of
      PathDelim:
        if J < I then
        begin
          // Check for consecutive 'PathDelim' characters and skip them if present
          if (I = 1) or (TempName[I - 1] <> PathDelim) then
            Result := Result + Copy(TempName, J, I - J);
          J := I;
          // Set a flag indicating that we just processed a path delimiter
          LastWasPathDelim := True;
        end;
      '.':
        begin
          // If the last character was a path delimiter then this '.' is
          // possibly a relative path modifier
          if LastWasPathDelim then
          begin
            // Check if the path ends in a '.'
            if I < Length(TempName) then
            begin
              // If the next characters are './' then this is a relative path
              // Otherwise simply treat this as just another filename.
              if (TempName[I + 1] = '.') and
                ((I + 1 = Length(TempName)) or (TempName[I + 2] = PathDelim)) then
              begin
                // Don't attempt to backup past the Root dir
                if Length(Result) > 1 then
                  // For the purpose of this excercise, treat the last dir as a
                  // filename so we can use this function to remove it
                  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
                J := I;
              end
              // Simply skip over and ignore any 'current dir' constrcucts, './'
              // or the remaining './' from a ../ constrcut.
              else if TempName[I + 1] = PathDelim then
              begin
                Result := IncludeTrailingPathDelimiter(Result);
                if TempName[I] in LeadBytes then
                  Inc(I, StrCharLength(PAnsiChar(@TempName[I])))
                else
                  Inc(I);
                J := I + 1;
              end else
                // If any of the above tests fail, then this is not a 'current dir' or
                // 'parent dir' construct so just clear the state and continue.
                LastWasPathDelim := False;
            end else
            begin
              // Don't let the expanded path end in a 'PathDelim' character
              Result := ExcludeTrailingPathDelimiter(Result);
              J := I + 1;
            end;
          end;
        end;
    else
      LastWasPathDelim := False;
    end;
    if TempName[I] in LeadBytes then
      Inc(I, StrCharLength(PAnsiChar(@TempName[I])))
    else
      Inc(I);
  end;
  // This will finally append what is left
  if (I - J > 1) then
    Result := Result + Copy(TempName, J, I - J);
end;
{$ENDIF}

function ExpandFileNameCase(const FileName: AnsiString;
  out MatchFound: TFilenameCaseMatch): AnsiString;
var
  SR: TSearchRec;
  FullPath, Name: AnsiString;
  Temp: Integer;
  FoundOne: Boolean;
{$IFDEF POSIX}
  Scans: Byte;
  FirstLetter, TestLetter: AnsiString;
{$ENDIF}
begin
  Result := ExpandFileName(FileName);
  MatchFound := mkNone;

  if FileName = '' then
    Exit;

  FullPath := ExtractFilePath(Result);
  Name := ExtractFileName(Result);


  // if FullPath is not the root directory  (portable)
  if not SameFileName(FullPath, IncludeTrailingPathDelimiter(ExtractFileDrive(FullPath))) then
  begin  // Does the path need case-sensitive work?
    Temp := FindFirst(string(ExcludeTrailingPathDelimiter(FullPath)), faAnyFile, SR);
    System.SysUtils.FindClose(SR);   // close search before going recursive
    if Temp <> 0 then
    begin
      FullPath := ExcludeTrailingPathDelimiter(FullPath);
      FullPath := ExpandFileNameCase(FullPath, MatchFound);
      if MatchFound = mkNone then
        Exit;    // if we can't find the path, we certainly can't find the file!
      FullPath := IncludeTrailingPathDelimiter(FullPath);
    end;
  end;

  // Path is validated / adjusted.  Now for the file itself
  try
    if FindFirst(string(FullPath + Name), faAnyFile, SR)= 0 then    // exact match on filename
    begin
      if not (MatchFound in [mkSingleMatch, mkAmbiguous]) then  // path might have been inexact
        MatchFound := mkExactMatch;
      Result := FullPath + AnsiString(SR.Name);
      Exit;
    end;
  finally
    System.SysUtils.FindClose(SR);
  end;

  FoundOne := False; // Windows should never get to here except for file-not-found

{$IFDEF POSIX}

{ Scan the directory.
  To minimize the number of filenames tested, scan the directory
  using upper/lowercase first letter + wildcard.
  This results in two scans of the directory (particularly on Linux) but
  vastly reduces the number of times we have to perform an expensive
  locale-charset case-insensitive AnsiString compare.  }

  // First, scan for lowercase first letter
  FirstLetter := AnsiLowerCase(Name[1]);
  for Scans := 0 to 1 do
  begin
    Temp := FindFirst(FullPath + FirstLetter + '*', faAnyFile, SR);
    while Temp = 0 do
    begin
      if AnsiSameText(SR.Name, Name) then
      begin
        if FoundOne then
        begin  // this is the second match
          MatchFound := mkAmbiguous;
          Exit;
        end
        else
        begin
          FoundOne := True;
          Result := FullPath + SR.Name;
        end;
      end;
      Temp := FindNext(SR);
    end;
    FindClose(SR);
    TestLetter := AnsiUpperCase(Name[1]);
    if TestLetter = FirstLetter then Break;
    FirstLetter := TestLetter;
  end;
{$ENDIF}

  if MatchFound <> mkAmbiguous then
  begin
    if FoundOne then
      MatchFound := mkSingleMatch
    else
      MatchFound := mkNone;
  end;
end;

{$IFDEF MSWINDOWS}
function GetUniversalName(const FileName: AnsiString): AnsiString;
type
  PNetResourceArray = ^TNetResourceArrayA;
  TNetResourceArrayA = array[0..MaxInt div SizeOf(TNetResource) - 1] of TNetResourceA;
var
  I, BufSize, NetResult: Integer;
  Count, Size: LongWord;
  Drive: AnsiChar;
  NetHandle: THandle;
  NetResources: PNetResourceArray;
  RemoteNameInfo: array[0..1023] of Byte;
begin
  Result := FileName;
  if (Win32Platform <> VER_PLATFORM_WIN32_WINDOWS) or (Win32MajorVersion > 4) then
  begin
    Size := SizeOf(RemoteNameInfo);
    if WNetGetUniversalNameA(PAnsiChar(FileName), UNIVERSAL_NAME_INFO_LEVEL,
      @RemoteNameInfo, Size) <> NO_ERROR then Exit;
    Result := AnsiString(string(PRemoteNameInfo(@RemoteNameInfo).lpUniversalName));
  end else
  begin
  { The following works around a bug in WNetGetUniversalName under Windows 95 }
    Drive := UpCase(FileName[1]);
    if (Drive < 'A') or (Drive > 'Z') or (Length(FileName) < 3) or
      (FileName[2] <> ':') or (FileName[3] <> '\') then
      Exit;
    if WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0, nil,
      NetHandle) <> NO_ERROR then Exit;
    try
      BufSize := 50 * SizeOf(TNetResource);
      GetMem(NetResources, BufSize);
      try
        while True do
        begin
          Count := $FFFFFFFF;
          Size := BufSize;
          NetResult := WNetEnumResourceA(NetHandle, Count, NetResources, Size);
          if NetResult = ERROR_MORE_DATA then
          begin
            BufSize := Size;
            ReallocMem(NetResources, BufSize);
            Continue;
          end;
          if NetResult <> NO_ERROR then Exit;
          for I := 0 to Count - 1 do
            with NetResources^[I] do
              if (lpLocalName <> nil) and (Drive = UpCase(lpLocalName[0])) then
              begin
                Result := lpRemoteName + Copy(FileName, 3, Length(FileName) - 2);
                Exit;
              end;
        end;
      finally
        FreeMem(NetResources, BufSize);
      end;
    finally
      WNetCloseEnum(NetHandle);
    end;
  end;
end;

function ExpandUNCFileName(const FileName: AnsiString): AnsiString;
begin
  { First get the local resource version of the file name }
  Result := ExpandFileName(FileName);
  if (Length(Result) >= 3) and (Result[2] = ':') and (Upcase(Result[1]) >= 'A')
    and (Upcase(Result[1]) <= 'Z') then
    Result := GetUniversalName(Result);
end;
{$ENDIF}

{$IFDEF POSIX}
function ExpandUNCFileName(const FileName: AnsiString): AnsiString;
begin
  Result := ExpandFileName(FileName);
end;
{$ENDIF}

function ExtractRelativePath(const BaseName, DestName: AnsiString): AnsiString;
var
  BasePath, DestPath: AnsiString;
  BaseLead, DestLead: PAnsiChar;
  BasePtr, DestPtr: PAnsiChar;

  function ExtractFilePathNoDrive(const FileName: AnsiString): AnsiString;
  begin
    Result := ExtractFilePath(FileName);
    Delete(Result, 1, Length(ExtractFileDrive(FileName)));
  end;

  function Next(var Lead: PAnsiChar): PAnsiChar;
  begin
    Result := Lead;
    if Result = nil then Exit;
    Lead := AnsiStrScan(Lead, PathDelim);
    if Lead <> nil then
    begin
      Lead^ := #0;
      Inc(Lead);
    end;
  end;

begin
  if SameFilename(ExtractFileDrive(BaseName), ExtractFileDrive(DestName)) then
  begin
    BasePath := ExtractFilePathNoDrive(BaseName);
    UniqueString(BasePath);
    DestPath := ExtractFilePathNoDrive(DestName);
    UniqueString(DestPath);
    BaseLead := Pointer(BasePath);
    BasePtr := Next(BaseLead);
    DestLead := Pointer(DestPath);
    DestPtr := Next(DestLead);
    while (BasePtr <> nil) and (DestPtr <> nil) and SameFilename(BasePtr, DestPtr) do
    begin
      BasePtr := Next(BaseLead);
      DestPtr := Next(DestLead);
    end;
    Result := '';
    while BaseLead <> nil do
    begin
      Result := Result + '..' + PathDelim;             { Do not localize }
      Next(BaseLead);
    end;
    if (DestPtr <> nil) and (DestPtr^ <> #0) then
      Result := Result + DestPtr + PathDelim;
    if DestLead <> nil then
      Result := Result + DestLead;     // destlead already has a trailing backslash
    Result := Result + ExtractFileName(DestName);
  end
  else
    Result := DestName;
end;

{$IFDEF MSWINDOWS}
function ExtractShortPathName(const FileName: AnsiString): AnsiString;
var
  Buffer: array[0..MAX_PATH - 1] of AnsiChar;
  Len: Integer;
begin
  Len := GetShortPathNameA(PAnsiChar(FileName), Buffer, Length(Buffer));
  if Len <= Length(Buffer) then
    SetString(Result, Buffer, Len)
  else
    if Len > 0 then
    begin
      SetLength(Result, Len);
      Len := GetShortPathNameA(PAnsiChar(FileName), PAnsiChar(Result), Len);
      if Len < Length(Result) then
        SetLength(Result, Len);
    end;
end;
{$ENDIF}

function LastDelimiter(const Delimiters, S: AnsiString): Integer;
var
  P: PAnsiChar;
begin
  Result := Length(S);
  P := PAnsiChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
      if (ByteType(S, Result) = mbTrailByte) then
        Dec(Result)
      else
        Exit;
    Dec(Result);
  end;
end;

function IsPathDelimiter(const S: AnsiString; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = PathDelim)
    and (ByteType(S, Index) = mbSingleByte);
end;

function IsDelimiter(const Delimiters, S: AnsiString; Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Length(S)) or (ByteType(S, Index) <> mbSingleByte) then exit;
  Result := StrScan(PAnsiChar(Delimiters), S[Index]) <> nil;
end;

function IncludeTrailingBackslash(const S: AnsiString): AnsiString;
begin
  Result := IncludeTrailingPathDelimiter(S);
end;

function IncludeTrailingPathDelimiter(const S: AnsiString): AnsiString;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;

function ExcludeTrailingBackslash(const S: AnsiString): AnsiString;
begin
  Result := ExcludeTrailingPathDelimiter(S);
end;

function ExcludeTrailingPathDelimiter(const S: AnsiString): AnsiString;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function AnsiCountElems(const AText: AnsiString; AStartIndex, ACharCount: Integer): Integer;
var
  S, P: PAnsiChar;
  Len, CharLen: Integer;
begin
  Result := 0;
  if AStartIndex <= 0 then
    AStartIndex := 1;
  Len := Length(AText);
  if (Len > 0) and (AStartIndex <= Len) then
  begin
    P := PAnsiChar(AText) + AStartIndex - 1;
    S := P;
    while (Len > 0) and (ACharCount > 0) do
    begin
      CharLen := 1;
      if IsLeadChar(P^) then
        CharLen := StrCharLength(P);
      if CharLen > Len then
        CharLen := Len;
      Inc(P, CharLen);
      Dec(Len, CharLen);
      Dec(ACharCount);
    end;
    Result := P - S;
  end;
end;

function AnsiCountChars(const AText: AnsiString): Integer;
var
  Len, CharLen: Integer;
  P: PAnsiChar;
begin
  Result := 0;
  Len := Length(AText);
  if Len > 0 then
  begin
    P := PAnsiChar(AText);
    while Len > 0 do
    begin
      CharLen := 1;
      if IsLeadChar(P^) then
        CharLen := StrCharLength(P);
      if CharLen > Len then
        CharLen := Len;
      Inc(P, CharLen);
      Dec(Len, CharLen);
      Inc(Result);
    end;
  end;
end;

function AnsiLeftStr(const AText: AnsiString; const ACount: Integer): AnsiString;
var
  Count: Integer;
begin
  Result := '';
  if (ACount > 0) and (Length(AText) > 0) then
  begin
    Count := AnsiCountElems(AText, 1, ACount);
    Result := Copy(AText, 1, Count);
  end;
end;

function AnsiRightStr(const AText: AnsiString; const ACount: Integer): AnsiString;
var
  CharCount: Integer;
begin
  Result := '';
  CharCount := AnsiCountChars(AText);
  if CharCount > 0 then
    Result := AnsiMidStr(AText, CharCount - ACount + 1, ACount);
end;

function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString;
var
  Len, Start, Count: Integer;
begin
  Result := '';
  if ACount > 0 then
  begin
    Len := Length(AText);
    if Len > 0 then
    begin
      Start := 1;
      if AStart > 0 then
        Start := AnsiCountElems(AText, 1, AStart - 1) + 1;
      if Len >= Start then
      begin
        Count := AnsiCountElems(AText, Start, ACount);
        Result := Copy(AText, Start, Count);
      end;
    end;
  end;
end;

end.

