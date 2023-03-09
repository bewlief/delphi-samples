{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2021-2022 Embarcadero Technologies, Inc. }
{        All rights reserved                            }
{                                                       }
{*******************************************************}

unit Xml.Internal.XmlInputSource;

{$HPPEMIT NOUSINGNAMESPACE}

interface

uses
  System.Classes, System.SysUtils,
  Xml.Internal.CodecUtilsWin32, Xml.Internal.XmlRulesUtils,
  Xml.Internal.ParserUtilsWin32;

type
  EDomException = class(Exception);

  ENot_Supported_Err = class(EdomException);

  TDomXMLDeclType = ( DT_XML_DECLARATION,
                      DT_TEXT_DECLARATION,
                      DT_XML_OR_TEXT_DECLARATION,
                      DT_UNSPECIFIED );

  TDomStandalone = ( STANDALONE_YES,
                     STANDALONE_NO,
                     STANDALONE_UNSPECIFIED );


  TXmlInputSource = class(TUtilsUCS4Reader)
  private
    FDeclType: TDomXMLDeclType;
    FHasMalformedDecl: Boolean;

    FXmlEncoding: string;
    FXmlStandalone: TDomStandalone;
    FXmlVersion: string;

    procedure CheckEncoding(const Encoding: string);
    function EvaluateXmlOrTextDecl(out DeclType: TDomXMLDeclType;
                                   out Version,
                                       EncName: string;
                                   out Standalone: TDomStandalone): Boolean;

    constructor Create(const Stream: TStream;
                       const ABufSize: Integer;
                       const AEncoding: string;
                       const InclDecl: Boolean;
                       const InitialByteCount,
                             InitialCharCount,
                             InitialCharsInLine,
                             InitialTabsInLine,
                             InitialLine: Int64);
  public
    class procedure GetXMLProlog(const Stream: TStream;
                                 out Encoding: string;
                                 out Standalone: TDomStandalone;
                                 out Version: string);
  end;

implementation

{ TXmlInputSource }

procedure TXmlInputSource.CheckEncoding(const Encoding: string);
// Calculates the codec class as specified in the XML or text declaration.
const
  UTF16_STR: string = 'UTF-16';
  UTF16BE_STR: string = 'UTF-16BE';
begin
  if Encoding = '' then Exit;

  if HasByteOrderMark and
     ( ( (Codec is TUTF16BECodec) and
         (TUTF16BECodec.AliasIndexOf(Encoding) = -1) and
         (TUCS4BECodec.AliasIndexOf(Encoding) = -1) )
       or
       ( (Codec is TUTF16LECodec) and
         (TUTF16LECodec.AliasIndexOf(Encoding) = -1) and
         (TUCS4LECodec.AliasIndexOf(Encoding) = -1) )
       or
       ( (Codec is TUTF8Codec) and
         (TUTF8Codec.AliasIndexOf(Encoding) = -1) ) ) then
    raise EConvertError.Create('Declared encoding does not match byte order mark.');

  if CompareText(Encoding, UTF16_STR) = 0 then begin // UTF-16 specified.
    if not HasByteOrderMark then begin
      SetEncoding(UTF16BE_STR);
      // Cf. RFC 2781: "UTF-16, an encoding of ISO 10646", sec. 4.3:
      //   If the first two octets of the text is not 0xFE followed by
      //   0xFF, and is not 0xFF followed by 0xFE, then the text SHOULD be
      //   interpreted as being big-endian.
    end else if not ( (Codec is TUTF16BECodec) or (Codec is TUTF16LECodec) ) then
      raise EConvertError.Create('Declared encoding does not match byte order mark.');
    Exit;
  end;

  try
    SetEncoding(Encoding);
  except
    on EParserUtilsException do
      raise ENot_Supported_Err.Create('Encoding not supported error.');
  end;
end;

constructor TXmlInputSource.Create(const Stream: TStream;
                                   const ABufSize: Integer;
                                   const AEncoding: string;
                                   const InclDecl: Boolean;
                                   const InitialByteCount,
                                         InitialCharCount,
                                         InitialCharsInLine,
                                         InitialTabsInLine,
                                         InitialLine: Int64);
begin
  inherited Create(Stream, ABufSize, AEncoding, InitialByteCount,
      InitialCharCount, InitialCharsInLine, InitialTabsInLine,
      InitialLine);

  FHasMalformedDecl := not EvaluateXmlOrTextDecl(FDeclType, FXmlVersion,
                             FXmlEncoding, FXmlStandalone);

  if AEncoding = '' then
    CheckEncoding(FXmlEncoding);

  if not InclDecl then
    InitialUCS4CharData := CurrentCharInfo;

  Reset;
end;

function TXmlInputSource.EvaluateXmlOrTextDecl(out DeclType: TDomXMLDeclType;
  out Version, EncName: string; out Standalone: TDomStandalone): Boolean;

  function IsXmlVersionNumCharCodePoint(const UCS4: Longint): Boolean;
  begin
    case UCS4 of
      // [.] , [0..9]
      $002E, $0030..$0039:
        Result := True
    else
      Result := False;
    end;
  end;

var
  QM: UCS4Char;
  WhitespaceSkipped: Boolean;
begin
  Result := True;
  DeclType := DT_UNSPECIFIED;
  EncName := '';
  Version := '1.0';  // Version 1.0 is the default.  Cf. XML 1.1, sec. 4.3.4.
  Standalone := STANDALONE_UNSPECIFIED;
  try
    if Match('<?xml') then begin // Does the stream start with '<?xml'?
      DeclType := DT_XML_OR_TEXT_DECLARATION;

      WhitespaceSkipped := SkipNext(GetXmlWhitespaceWideString) > 0;

      // version:
      if CurrentCharInfo.CodePoint = $0076 then begin // 'v'
        if not WhitespaceSkipped then begin
          Result := False;
          Exit;
        end;
        if Match('ersion') then begin
          SkipNext(GetXmlWhitespaceWideString);
          if not ( CurrentCharInfo.CodePoint = $003D ) then begin  // '='
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
          if not ( ( CurrentCharInfo.CodePoint = $0022 ) or
                   ( CurrentCharInfo.CodePoint = $0027 ) ) then begin  // '"' or '''
            Result := False;
            Exit;
          end;
          QM := CurrentCharInfo.CodePoint;
          Next;
          if IsXmlVersionNumCharCodePoint(CurrentCharInfo.CodePoint) then begin
            Version := string(Char(CurrentCharInfo.CodePoint));
          end else begin
            Result := False;
            Exit;
          end;
          Next;
          while IsXmlVersionNumCharCodePoint(CurrentCharInfo.CodePoint) do begin
            Version := Concat(Version, string(Char(CurrentCharInfo.CodePoint)));
            Next;
          end;
          if CurrentCharInfo.CodePoint <> QM then begin  // Is the first quotation mark of the same type as the second?
            Result := False;
            Exit;
          end;
          WhitespaceSkipped := SkipNext(GetXmlWhitespaceWideString) > 0;
        end else begin
          Result := False;
          Exit;
        end; {if ... else ...}
      end else DeclType := DT_TEXT_DECLARATION;

      // EncodingDecl:
      if CurrentCharInfo.CodePoint = $0065 then begin // 'e'
        if not WhitespaceSkipped then begin
          Result := False;
          Exit;
        end;
        if Match('ncoding') then begin
          SkipNext(GetXmlWhitespaceWideString);
          if not ( CurrentCharInfo.CodePoint = $003D ) then begin  // '='
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
          if not ( ( CurrentCharInfo.CodePoint = $0022 ) or
                   ( CurrentCharInfo.CodePoint = $0027 ) ) then begin  // '"' or '''
            Result := False;
            Exit;
          end;
          QM := CurrentCharInfo.CodePoint;
          Next;
          if IsXmlEncNameLeadingCharCodePoint(CurrentCharInfo.CodePoint) then begin
            EncName := string(Char(CurrentCharInfo.CodePoint));
          end else begin
            Result := False;
            Exit;
          end;
          Next;
          while IsXmlEncNameFollowingCharCodePoint(CurrentCharInfo.CodePoint) do begin
            EncName := Concat(EncName, string(Char(CurrentCharInfo.CodePoint)));
            Next;
          end;
          if CurrentCharInfo.CodePoint <> QM then begin  // Is the first quotation mark of the same type as the second?
            Result := False;
            Exit;
          end;
          WhitespaceSkipped := SkipNext(GetXmlWhitespaceWideString) > 0;
        end else begin
          Result := False;
          Exit;
        end; {if ... else ...}
      end else begin
        if DeclType = DT_TEXT_DECLARATION then begin
          Result := False;
          Exit;
        end else DeclType := DT_XML_DECLARATION;
      end; {if ... else ...}

      // SDDecl:
      if CurrentCharInfo.CodePoint = $0073 then begin // 's'
        if not WhitespaceSkipped then begin
          Result := False;
          Exit;
        end;
        if Match('tandalone') then begin
          SkipNext(GetXmlWhitespaceWideString);
          if not ( CurrentCharInfo.CodePoint = $003D ) then begin  // '='
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
          if not ( ( CurrentCharInfo.CodePoint = $0022 ) or
                   ( CurrentCharInfo.CodePoint = $0027 ) ) then begin  // '"' or '''
            Result := False;
            Exit;
          end;
          QM := CurrentCharInfo.CodePoint;
          Next;

          case CurrentCharInfo.CodePoint of
            $0079: begin // 'y'
              Next;
              if CurrentCharInfo.CodePoint = $0065 then begin  // 'e'
                Next;
                if CurrentCharInfo.CodePoint = $0073 then begin // 's'
                  Standalone := STANDALONE_YES;
                end else begin
                  Result := False;
                  Exit;
                end;
              end else begin
                Result := False;
                Exit;
              end;
            end;
            $006e: begin // 'n'
              Next;
              if CurrentCharInfo.CodePoint = $006f then begin // 'o'
                Standalone := STANDALONE_NO;
              end else begin
                Result := False;
                Exit;
              end;
            end;
          else
            Result := False;
            Exit;
          end; {case ...}
          Next;
          if CurrentCharInfo.CodePoint <> QM then begin  // Is the first quotation mark of the same type as the second?
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
        end else begin
          Result := False;
          Exit;
        end; {if ... else ...}
        if DeclType = DT_TEXT_DECLARATION then begin
          Result := False;
          Exit;
        end else DeclType := DT_XML_DECLARATION;
      end; {if ...}

      // '?>':
      if (CurrentCharInfo.CodePoint = $003F) // '?'
        and Match('>') then begin   // '>'

        ResetPosition := Position - NextCharInfo.Size;

      end else
        Result := False;

    end else
      Reset;

  except
    Result := False;
  end; {try ...}
end;

class procedure TXmlInputSource.GetXMLProlog(const Stream: TStream;
                                             out Encoding: string;
                                             out Standalone: TDomStandalone;
                                             out Version: string);
begin
  with Create(Stream, 0, '', False, 0, 0, 0, 0, 0) do
  try
    Encoding := FXmlEncoding;
    Standalone := FXmlStandalone;
    Version := FXmlVersion;
  finally
    Free;
  end;
end;

end.
