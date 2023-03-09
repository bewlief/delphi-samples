{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Text;

{$MINENUMSIZE 4}

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.UITypes, System.Character, System.Math, FMX.Types, FMX.Graphics;

type

  { TCaretPosition }

  /// <summary>Record describes platform independant position in multiline text</summary>
  /// <remarks>Better use this type of position instead of absolute integer position. Absolute integer position includes
  /// length of the LineBreak. It's value depends on platform (e.g. on Windows LineBreak.Length=2,
  /// on OSX LineBreak.Length=1).
  /// TCaretPosition is better in cases if you want to store current caret position in your application accross
  /// multiple platforms.</remarks>
  TCaretPosition = record
    ///<summary>Text line number value</summary>
    Line: Integer;
    ///<summary>Caret position in line defined by Line value</summary>
    Pos: Integer;
  public
    ///<summary>Create new TCaretPosition with devined values of Line and Pos</summary>
    class function Create(const ALine, APos: Integer): TCaretPosition; static; inline;

    ///<summary>Overriding equality check operator</summary>
    class operator Equal(const A, B: TCaretPosition): Boolean;
    ///<summary>Overriding inequality check operator</summary>
    class operator NotEqual(const A, B: TCaretPosition): Boolean;
    ///<summary>Overriding compare operator: Less Or Equal</summary>
    class operator LessThanOrEqual(const A, B: TCaretPosition): Boolean;
    ///<summary>Overriding compare operator: Less Than</summary>
    class operator LessThan(const A, B: TCaretPosition): Boolean;
    ///<summary>Overriding compare operator: Greater Or Equal</summary>
    class operator GreaterThanOrEqual(const A, B: TCaretPosition): Boolean;
    ///<summary>Overriding compare operator: Greater Than</summary>
    class operator GreaterThan(const A, B: TCaretPosition): Boolean;
    ///<summary>Overriding implicit conversion to TPoint</summary>
    class operator Implicit(const APosition: TCaretPosition): TPoint;
    ///<summary>Overriding implicit conversion to TCaretPosition</summary>
    class operator Implicit(const APoint: TPoint): TCaretPosition;

    ///<summary>Returns zero caret position value (0; 0)</summary>
    class function Zero: TCaretPosition; inline; static;
    ///<summary>Resturn invalid caret position value (-1; -1)</summary>
    class function Invalid: TCaretPosition; inline; static;

    ///<summary>Increment line number value</summary>
    procedure IncrementLine;
    ///<summary>Decrement line number value</summary>
    procedure DecrementLine;
    ///<summary>Check wherever current caret position has zero value (0; 0)</summary>
    function IsZero: Boolean;
    ///<summary>Checks wherever current caret position has invalid value (either <c>Line</c> or <c>Pos</c> has -1
    ///value)</summary>
    function IsInvalid: Boolean;
  end;

{ TTextService }

  TMarkedTextAttribute = (Input, TargetConverted, Converted, TargetNotConverted, InputError);

  TTextService = class
  private
    FOwner: IControl;
    FMultiLine: Boolean;
    FMaxLength: Integer;
    FCharCase: TEditCharCase;
    FFilterChar: string;
    FImeMode: TImeMode;
    FMarkedTextPosition: TCaretPosition;
    procedure SetMarkedTextPosition(const Value: TCaretPosition);
  protected
                                                                                                                                   
    FCaretPosition: TPoint;
                                                                                                                                   
    FText: string;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    function GetCaretPosition: TPoint; virtual;
    procedure SetCaretPosition(const Value: TPoint); virtual;
    procedure SetMaxLength(const Value: Integer); virtual;
    procedure SetCharCase(const Value: TEditCharCase); virtual;
    procedure SetFilterChar(const Value: string); virtual;
    procedure ImeModeChanged; virtual;
    procedure TextChanged; virtual;
    procedure MarkedTextPositionChanged; virtual;
    procedure CaretPositionChanged; virtual;
    function GetMarketTextAttributes: TArray<TMarkedTextAttribute>; virtual;
  public
    constructor Create(const AOwner: IControl; const ASupportMultiLine: Boolean); virtual;
    destructor Destroy; override;
    { Text support }
    procedure InternalSetMarkedText(const AMarkedText: string); virtual; abstract;
    function InternalGetMarkedText: string; virtual; abstract;
    /// <summary>Returns <c>Text</c> with inserted IME <c>MarkedText</c>.</summary>
    function CombinedText: string; virtual;
    function TargetClausePosition: TPoint; virtual; abstract;
    function HasMarkedText: Boolean; virtual; abstract;
    { Enter/Exit }
    procedure EnterControl(const FormHandle: TWindowHandle); virtual; abstract;
    procedure ExitControl(const FormHandle: TWindowHandle); virtual; abstract;
    { Drawing Lines }
    procedure DrawSingleLine(const Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; virtual; abstract;
    procedure DrawSingleLine(const Canvas: TCanvas; const S: string;
      const ARect: TRectF; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; virtual; abstract;

    /// <summary>Refreshes position of IME related UI controls.</summary>
    procedure RefreshImePosition; virtual;
    { IME Mode }
    function GetImeMode: TImeMode;
    procedure SetImeMode(const Value: TImeMode);
    { Selection }
    procedure BeginSelection; virtual;
    procedure EndSelection; virtual;
    property CaretPosition: TPoint read GetCaretPosition write SetCaretPosition;
    /// <summary>Original text without IME marked text.</summary>
    property Text: string read GetText write SetText;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default TImeMode.imDontCare;
    ///<summary>Defines the maximum text of the text that could inputed via text service</summary>
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    ///<summary>Defines wherever input control allows to input several lines on text or just a single one.</summary>
    property Multiline: Boolean read FMultiLine;
    /// <summary>Defines input character case</summary>
    property CharCase: TEditCharCase read FCharCase write SetCharCase;
    /// <summary>Defines input filter</summary>
    property FilterChar: string read FFilterChar write SetFilterChar;
    ///<summary>Holds a reference to the text input control in UI</summary>
    property Owner: IControl read FOwner;
    /// <summary>Returns the IME text that user is entering.</summary>
    property MarkedText: string read InternalGetMarkedText;
    /// <summary>Specifies position of IME marked text.</summary>
    property MarkedTextPosition: TCaretPosition read FMarkedTextPosition write SetMarkedTextPosition;
    /// <summary>Returns text attributes for each character in <c>MarkedText</c>.</summary>
    property MarketTextAttributes: TArray<TMarkedTextAttribute> read GetMarketTextAttributes;
  end;

  TTextServiceClass = class of TTextService;

{ TTextWordWrapping }

  ///<summary>Manage text line counting and line retrieving for a given width in a canvas.</summary>
  TTextWordWrapping = class
  public
    ///<summary>Fills ALinesFound with the text that fits on a certain Width (AMaxWidth). Also fills the real maximum
    ///with found</summary>
    class procedure GetLines(const AText: String; const ACanvas: TCanvas; const AMaxWidth: Integer;
      var ALinesFound: TStringList; var AResWidth: Integer);
    ///<summary>Computes the number of lines needed to draw the text supplied for a certain Width (AMaxWidth). Also
    ///fills the real maximum with found.</summary>
    class function ComputeLineCount(const AText: String; const ACanvas: TCanvas; const AMaxWidth: Integer;
      var AResWidth: Integer): Integer;
  end;

{ ITextInput }

  ITextInput = interface
    ['{56D79E74-58D6-4c1e-B832-F133D669B952}']
    function GetTextService: TTextService;
    { IME }
    /// <summary>Returns position</summary>
    function GetTargetClausePointF: TPointF;
    procedure StartIMEInput;
    procedure EndIMEInput;
    /// <summary>Platform using this method to notify control that either text or caret position was changed</summary>
    procedure IMEStateUpdated;
    { Selection }
    function GetSelection: string;
    /// <summary>Returns selection rect in local cooridnate system of text-input control.</summary>
    function GetSelectionRect: TRectF;
    function GetSelectionBounds: TRect;
    function GetSelectionPointSize: TSizeF;
    { Text }
    function HasText: Boolean;
  end;

{ ITextLinesSource }

  /// <summary>Interface for accessing text lines.</summary>
  ITextLinesSource = interface
  ['{21E863AD-6411-4B68-A985-4D36D899DA97}']
    { Lines accessing }

    /// <summary>Returns line by index.</summary>
    function GetLine(const ALineIndex: Integer): string;
    /// <summary>Returns lines break separator.</summary>
    function GetLineBreak: string;
    /// <summary>Returns lines count.</summary>
    function GetCount: Integer;
    /// <summary>Returns line by index.</summary>
    property Lines[const AIndex: Integer]: string read GetLine;
    /// <summary>Line break separator.</summary>
    property LineBreak: string read GetLineBreak;
    /// <summary>Lines count.</summary>
    property Count: Integer read GetCount;

    { Position conversion }

    ///<summary>Convert absolute platform-dependent position in text to platform independent value in format
    ///(line_number, position_in_line)</summary>
    function TextPosToPos(const APos: Integer): TCaretPosition;
    ///<summary>Convert platform-independent position to absolute platform-dependent position</summary>
    function PosToTextPos(const APostion: TCaretPosition): Integer;
  end;

{ ITextSpellCheck }

  ITextSpellCheck = interface
    ['{30AA8C32-5ADA-456C-AAC5-B9F0309AE3A8}']
    { common }
    function IsSpellCheckEnabled: Boolean;
    function IsCurrentWordWrong: Boolean;
    function GetListOfPrepositions: TArray<string>;
    { Spell check }
    procedure HighlightSpell;
    procedure HideHighlightSpell;
  end;

{ ITextActions }

  { Standard actions for operation with the text. Objects which want to get
    support of actions in a shortcut menu shall implement this interface }
  ITextActions = interface
    ['{9DB49126-36DB-4193-AE96-C0BD27090DCD}']
    procedure DeleteSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure SelectWord;
    procedure ResetSelection;
    procedure GoToTextEnd;
    procedure GoToTextBegin;
    procedure Replace(const AStartPos: Integer; const ALength: Integer; const AStr: string);
  end;

{ ITextSpellCheckActions }

  ITextSpellCheckActions = interface
    ['{82A33171-C825-4B7F-B0C4-A56DDD4FF85C}']
    procedure Spell(const AWord: string);
  end;

  { Search of boundaries of the word in the text since |Index|
    Returns an index of the left |BeginPos| and right |EndPos| boundary of the found word
      |Text| - Source text
      |Index| - Index of carriage position
      |BeginPos| - Index of carriage position
      |EndPos| - Index of carriage position
    Position of the carriage begins with 0. 0 specifies position of the carriage to the first character }
  function FindWordBound(const AText: string; const AIndex: Integer; out ABeginPos, AEndPos: Integer): Boolean;
  function GetLexemeBegin(const AText: string; const AIndex: Integer): Integer;
  function GetLexemeEnd(const AText: string; const AIndex: Integer): Integer;
  function GetNextLexemeBegin(const AText: string; const AIndex: Integer): Integer;
  function GetPrevLexemeBegin(const AText: string; const AIndex: Integer): Integer;
  function TruncateText(const Text: string; const MaxLength: Integer): string;
  /// <summary>Removes from the source string Input all characters that are not
  /// in the Filter string.</summary>
  function FilterText(const Input: string; const Filter: string): string;

type
  TNumValueType = (Integer, Float);

  TValidateTextEvent = procedure(Sender: TObject; var Text: string) of object;

  function FilterCharByValueType(const AValueType: TNumValueType): string;
  function TryTextToValue(AText: string; var AValue: Single; DefaultValue: Single): Boolean; overload;
  function TryTextToValue(AText: string; var AValue: Double; DefaultValue: Double): Boolean; overload;

implementation

uses
  System.SysUtils, System.RegularExpressions, FMX.Utils;

type
  TTextWordWrappingImpl = class
  strict private
    class var FRegExpr: TRegEx;
    class var FRegExprSpace: TRegEx;
  public
    class constructor Create;
    class procedure GetWords(const AText: String; var AWordsFound: TStringList);
    class function GetLines(const AText: String; const ACanvas: TCanvas; const AMaxWidth: Integer;
      var ALinesFound: TStringList; var AResWidth: Single): Integer;
end;

{ TCaretPosition }

class function TCaretPosition.Create(const ALine, APos: Integer): TCaretPosition;
begin
  Result.Line := ALine;
  Result.Pos := APos;
end;

class operator TCaretPosition.Equal(const A, B: TCaretPosition): Boolean;
begin
  Result := (A.Line = B.Line) and (A.Pos = B.Pos);
end;

class operator TCaretPosition.NotEqual(const A, B: TCaretPosition): Boolean;
begin
  Result := (A.Line <> B.Line) or (A.Pos <> B.Pos);
end;

class operator TCaretPosition.LessThanOrEqual(const A, B: TCaretPosition): Boolean;
begin
  Result := (A.Line < B.Line) or (A.Line = B.Line) and (A.Pos <= B.Pos);
end;

class operator TCaretPosition.LessThan(const A, B: TCaretPosition): Boolean;
begin
  Result := (A.Line < B.Line) or (A.Line = B.Line) and (A.Pos < B.Pos);
end;

class operator TCaretPosition.GreaterThanOrEqual(const A, B: TCaretPosition): Boolean;
begin
  Result := (A.Line > B.Line) or (A.Line = B.Line) and (A.Pos >= B.Pos);
end;

class operator TCaretPosition.GreaterThan(const A, B: TCaretPosition): Boolean;
begin
  Result := (A.Line > B.Line) or (A.Line = B.Line) and (A.Pos > B.Pos);
end;

class operator TCaretPosition.Implicit(const APosition: TCaretPosition): TPoint;
begin
  Result := TPoint.Create(APosition.Pos, APosition.Line);
end;

procedure TCaretPosition.DecrementLine;
begin
  Dec(Line);
end;

class operator TCaretPosition.Implicit(const APoint: TPoint): TCaretPosition;
begin
  Result := TCaretPosition.Create(APoint.Y, APoint.X);
end;

procedure TCaretPosition.IncrementLine;
begin
  Inc(Line);
end;

class function TCaretPosition.Invalid: TCaretPosition;
begin
  Result := TCaretPosition.Create(-1, -1);
end;

function TCaretPosition.IsInvalid: Boolean;
begin
  Result := (Line < 0) or (Pos < 0);
end;

function TCaretPosition.IsZero: Boolean;
begin
  Result := (Line = 0) and (Pos = 0);
end;

class function TCaretPosition.Zero: TCaretPosition;
begin
  Result := TCaretPosition.Create(0, 0);
end;

{ TTextService }

procedure TTextService.SetMarkedTextPosition(const Value: TCaretPosition);
begin
  if FMarkedTextPosition <> Value then
  begin
    FMarkedTextPosition := Value;
    MarkedTextPositionChanged;
  end;
end;

procedure TTextService.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
end;

procedure TTextService.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TTextService.TextChanged;
begin
  // Nothing
end;

procedure TTextService.SetCaretPosition(const Value: TPoint);
begin
  FCaretPosition := Value;
  CaretPositionChanged;
end;

procedure TTextService.SetCharCase(const Value: TEditCharCase);
begin
  FCharCase := Value;
end;

procedure TTextService.SetFilterChar(const Value: string);
begin
  FFilterChar := Value;
end;

procedure TTextService.SetImeMode(const Value: TImeMode);
begin
  if FImeMode <> Value then
  begin
    FImeMode := Value;
    ImeModeChanged;
  end;
end;

procedure TTextService.CaretPositionChanged;
begin
  // Nothing
end;

function TTextService.CombinedText: string;
begin
  if MarkedText.IsEmpty then
    Result := Text
  else
    Result := Text.Substring(0, CaretPosition.X) + MarkedText + Text.Substring(CaretPosition.X);
end;

constructor TTextService.Create(const AOwner: IControl; const ASupportMultiLine: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FMultiLine := ASupportMultiLine;
  CaretPosition := TPoint.Zero;
  FMarkedTextPosition := TCaretPosition.Invalid;
end;

destructor TTextService.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

procedure TTextService.EndSelection;
begin
  // Nothing
end;

function TTextService.GetCaretPosition: TPoint;
begin
  Result := FCaretPosition;
end;

function TTextService.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

function TTextService.GetMarketTextAttributes: TArray<TMarkedTextAttribute>;
begin
  Result := [];
end;

function TTextService.GetText: string;
begin
  Result := FText;
end;

procedure TTextService.ImeModeChanged;
begin
  // Nothing
end;

procedure TTextService.MarkedTextPositionChanged;
begin
  // Nothing
end;

procedure TTextService.RefreshImePosition;
begin
  // Nothing
end;

procedure TTextService.BeginSelection;
begin
  // Nothing
end;

type
  TLexemeAnalyzer = class
  private type
    TLexemeType = (Word, OneLetterWord, HalfFullWidth, CJKSymbolsAndPunctuation, None);
    TDirection = (Backward, Forward);
    class function IsKatana(const AChar: Char): Boolean; inline;
    class function IsPunctuation(const AChar: Char): Boolean; inline;
    class function IsSeparator(const AChar: Char): Boolean; inline;
    class function IsHalfFullWidthForm(const AChar: Char): Boolean; inline;
    class function IsCJKSymbolsAndPunctuation(const AChar: Char): Boolean; inline;
    class function IsHiragana(const AChar: Char): Boolean; inline;
    class function IsCJK(const AChar: Char): Boolean; inline;
    { Lexeme }
    class function GetLexemeType(const AText: string; const AIndex: Integer): TLexemeType;
    class procedure SkipNonLexeme(const AText: string; var AIndex: Integer; const ADirection: TDirection);
  public
    class function GetLexemeBegin(const AText: string; const AIndex: Integer): Integer;
    class function GetLexemeEnd(const AText: string; const AIndex: Integer): Integer;
    class function GetNextLexemeBegin(const AText: string; const AIndex: Integer): Integer;
    class function GetPrevLexemeBegin(const AText: string; const AIndex: Integer): Integer;
    class function FindWordBound(const AText: string; const AIndex: Integer; out ABeginPos, AEndPos: Integer): Boolean;
  end;

class function TLexemeAnalyzer.IsKatana(const AChar: Char): Boolean;
begin
  Result := InRange(AChar.ToUCS4Char, $30A0, $30FF);
end;

class function TLexemeAnalyzer.IsPunctuation(const AChar: Char): Boolean;
begin
  Result := AChar.GetUnicodeCategory in [TUnicodeCategory.ucMathSymbol,
                                         TUnicodeCategory.ucConnectPunctuation,
                                         TUnicodeCategory.ucDashPunctuation,
                                         TUnicodeCategory.ucClosePunctuation,
                                         TUnicodeCategory.ucFinalPunctuation,
                                         TUnicodeCategory.ucInitialPunctuation,
                                         TUnicodeCategory.ucOtherPunctuation,
                                         TUnicodeCategory.ucOpenPunctuation,
                                         TUnicodeCategory.ucClosePunctuation];
end;

class function TLexemeAnalyzer.IsSeparator(const AChar: Char): Boolean;
begin
   Result := AChar.GetUnicodeCategory in [TUnicodeCategory.ucSpaceSeparator];
end;

class function TLexemeAnalyzer.IsHalfFullWidthForm(const AChar: Char): Boolean;
begin
  Result := InRange(AChar.ToUCS4Char, $FF00, $FFEF);
end;

class function TLexemeAnalyzer.IsCJKSymbolsAndPunctuation(const AChar: Char): Boolean;
begin
  Result := InRange(AChar.ToUCS4Char, $3000, $303F);
end;

class function TLexemeAnalyzer.IsHiragana(const AChar: Char): Boolean;
begin
  Result := InRange(AChar.ToUCS4Char, $3040, $309F);
end;

class function TLexemeAnalyzer.IsCJK(const AChar: Char): Boolean;
begin
  Result := InRange(AChar.ToUCS4Char, $4E00, $9FFF);
end;

class function TLexemeAnalyzer.FindWordBound(const AText: string; const AIndex: Integer; out ABeginPos,
  AEndPos: Integer): Boolean;
var
  Index: Integer;
begin
  if AText.IsEmpty then
    Exit(False);

  Index := EnsureRange(AIndex, 0, AText.Length - 1);
  if GetLexemeType(AText, Index) = TLexemeType.None then
    Exit(False);

  ABeginPos := GetLexemeBegin(AText, Index);
  AEndPos := GetLexemeEnd(AText, Index);

  Result := AEndPos >= ABeginPos;
end;

class function TLexemeAnalyzer.GetLexemeBegin(const AText: string; const AIndex: Integer): Integer;
var
  OriginalLexemeType: TLexemeType;
begin
  if AText.IsEmpty then
    Exit(0);

  Result := EnsureRange(AIndex, 0, AText.Length - 1);
  if not AText.IsEmpty and (Result >= 0) then
  begin
    SkipNonLexeme(AText, Result, TDirection.Backward);

    if Result > 0 then
    begin
      OriginalLexemeType := GetLexemeType(AText, Result);
      case OriginalLexemeType of
        TLexemeType.Word:
          while (Result > 0) and (GetLexemeType(AText, Result - 1) = OriginalLexemeType) do
            Dec(Result);

        TLexemeType.HalfFullWidth,
        TLexemeType.CJKSymbolsAndPunctuation:
          if GetLexemeType(AText, Result - 1) = TLexemeType.OneLetterWord then
            Dec(Result);
      end;
    end;
  end;
  Result := Max(Result, 0);
end;

class function TLexemeAnalyzer.GetLexemeEnd(const AText: string; const AIndex: Integer): Integer;
var
  OriginalLexemType: TLexemeType;
  TextLength: Integer;
begin
  if AText.IsEmpty then
    Exit(0);

  TextLength := AText.Length;
  Result := EnsureRange(AIndex, 0, TextLength - 1);
  if not AText.IsEmpty and (Result >= 0) then
  begin
    SkipNonLexeme(AText, Result, TDirection.Forward);

    if Result < TextLength then
    begin
      OriginalLexemType := GetLexemeType(AText, Result);
      case OriginalLexemType of
        TLexemeType.Word:
          while (Result < TextLength - 1) and (GetLexemeType(AText, Result + 1) = OriginalLexemType) do
            Inc(Result);

        TLexemeType.OneLetterWord:
          if GetLexemeType(AText, Result + 1) in [TLexemeType.HalfFullWidth, TLexemeType.CJKSymbolsAndPunctuation] then
            Inc(Result);
      end;
    end;
  end;
  Result := Min(Result, TextLength - 1);
end;

class function TLexemeAnalyzer.GetLexemeType(const AText: string; const AIndex: Integer): TLexemeType;
var
  Ch: Char;
begin
  Ch := AText.Chars[AIndex];
  if IsHalfFullWidthForm(Ch) then
    Result := TLexemeType.HalfFullWidth
  else if IsCJKSymbolsAndPunctuation(Ch) then
    Result := TLexemeType.CJKSymbolsAndPunctuation
  else if IsPunctuation(Ch) or IsSeparator(Ch) then
    Result := TLexemeType.None
  else if IsKatana(Ch) or IsHiragana(Ch) or IsCJK(Ch) then
    Result := TLexemeType.OneLetterWord
  else
    Result := TLexemeType.Word;
end;

class function TLexemeAnalyzer.GetNextLexemeBegin(const AText: string; const AIndex: Integer): Integer;
begin
  if AText.IsEmpty then
    Exit(0);

  if GetLexemeType(AText, AIndex) = TLexemeType.None then
  begin
    Result := AIndex;
    SkipNonLexeme(AText, Result, TDirection.Forward);
  end
  else
  begin
    Result := GetLexemeEnd(AText, AIndex);

    if Result < AText.Length then
    begin
      Inc(Result);
      SkipNonLexeme(AText, Result, TDirection.Forward);
    end;
  end;
end;

class function TLexemeAnalyzer.GetPrevLexemeBegin(const AText: string; const AIndex: Integer): Integer;
var
  Index: Integer;
begin
  if AText.IsEmpty then
    Exit(0);

  Index := EnsureRange(AIndex, 0, AText.Length - 1);

  if GetLexemeType(AText, Index) = TLexemeType.None then
  begin
    Result := Index;
    SkipNonLexeme(AText, Result, TDirection.Backward);
    Result := GetLexemeBegin(AText, Result);
  end
  else
  begin
    Result := GetLexemeBegin(AText, Index);

    if Result > 0 then
    begin
      Dec(Result);
      SkipNonLexeme(AText, Result, TDirection.Backward);
      Result := GetLexemeBegin(AText, Result);
    end;
  end;
end;

class procedure TLexemeAnalyzer.SkipNonLexeme(const AText: string; var AIndex: Integer; const ADirection: TDirection);
var
  LexemType: TLexemeType;
begin
  LexemType := GetLexemeType(AText, AIndex);
  if LexemType = TLexemeType.None then
    repeat
      case ADirection of
        TLexemeAnalyzer.TDirection.Backward:
          Dec(AIndex);
        TLexemeAnalyzer.TDirection.Forward:
          Inc(AIndex);
      end;
    until (AIndex = 0) and (ADirection = TLexemeAnalyzer.TDirection.Backward) or
          (AIndex = AText.Length) and (ADirection = TLexemeAnalyzer.TDirection.Forward) or
          (GetLexemeType(AText, AIndex) <> TLexemeType.None);
end;

function GetLexemeBegin(const AText: string; const AIndex: Integer): Integer;
begin
  Result := TLexemeAnalyzer.GetLexemeBegin(AText, AIndex);
end;

function GetLexemeEnd(const AText: string; const AIndex: Integer): Integer;
begin
  Result := TLexemeAnalyzer.GetLexemeEnd(AText, AIndex);
end;

function GetNextLexemeBegin(const AText: string; const AIndex: Integer): Integer;
begin
  Result := TLexemeAnalyzer.GetNextLexemeBegin(AText, AIndex);
end;

function GetPrevLexemeBegin(const AText: string; const AIndex: Integer): Integer;
begin
  Result := TLexemeAnalyzer.GetPrevLexemeBegin(AText, AIndex);
end;

function FindWordBound(const AText: string; const AIndex: Integer; out ABeginPos, AEndPos: Integer): Boolean;
begin
  Result := TLexemeAnalyzer.FindWordBound(AText, AIndex, ABeginPos, AEndPos);
end;

function TryTextToValue(AText: string; var AValue: Single; DefaultValue: Single): boolean;
var
  TempValue: Single;
begin
  Result := TryStrToFloat(FixNumberText(AText), TempValue, FormatSettings);
  if Result then
    AValue := TempValue
  else
    AValue := DefaultValue;
end;

function TryTextToValue(AText: string; var AValue: Double; DefaultValue: Double): boolean;
var
  TempValue: Double;
begin
  Result := TryStrToFloat(FixNumberText(AText), TempValue, FormatSettings);
  if Result then
    AValue := TempValue
  else
    AValue := DefaultValue;
end;

function FilterCharByValueType(const AValueType: TNumValueType): string;
begin
  if AValueType = TNumValueType.Integer then
    Result := '0123456789-+'
  else
  begin
    Result := '0123456789.,-+';
    if Pos(FormatSettings.DecimalSeparator, Result) = 0 then
      Result := Result + FormatSettings.DecimalSeparator;
  end;
end;

function TruncateText(const Text: string; const MaxLength: Integer): string;
begin
  if MaxLength > 0 then
    Result := Text.Substring(0, MaxLength)
  else
    Result := Text;
end;

function FilterText(const Input: string; const Filter: string): string;
begin
  Result := Input;
  if not Filter.IsEmpty then
    Result := TRegEx.Replace(Result, Format('[^%s]', [TRegEx.Escape(Filter)]), string.Empty);
end;

{$REGION 'TTextWordWrapping' }

class constructor TTextWordWrappingImpl.Create;
begin
  FRegExpr := TRegEx.Create('\r\n|\r|\n|[^\S\n\r]+|\S+', [roMultiLine, roCompiled]);
  FRegExprSpace := TRegEx.Create('[^\S\n\r]+', [roCompiled]);
end;

class procedure TTextWordWrappingImpl.GetWords(const AText: String; var AWordsFound: TStringList);
var
  Matches: TMatchCollection;
  I: Integer;
begin
  Matches := FRegExpr.Matches(AText);
  for I := 0 to Matches.Count - 1 do
    if Matches[I].Success then
      AWordsFound.Add(Matches[I].Value);
end;

class function TTextWordWrappingImpl.GetLines(const AText: String; const ACanvas: TCanvas; const AMaxWidth: Integer;
  var ALinesFound: TStringList; var AResWidth: Single): Integer;
var
  WordList: TStringList;
  WordStr: String;
  CurrStr: String;
  Width: Single;
  AccWidth: Single;

  procedure IncLine;
  begin
    if ALinesFound <> nil then
      ALinesFound.Add(CurrStr);
    AResWidth := Max(AResWidth, AccWidth);
    CurrStr := String.Empty;
    AccWidth := 0;
    Inc(Result);
  end;

begin
  Result := 0;
  AResWidth := 0;
  WordList := TStringList.Create;
  try
    AccWidth := 0;
    GetWords(AText, WordList);
    for WordStr in WordList do
    begin
      if (WordStr = #13) or (WordStr = #10) or (WordStr = #13#10) then
      begin
        IncLine;
        Continue;
      end
      else
      begin
        Width := ACanvas.TextWidth(WordStr);
        if Width + AccWidth > AMaxWidth then
        begin
          if not CurrStr.IsEmpty then
            IncLine;
          if FRegExprSpace.Match(WordStr).Success then
            Continue;
        end;
      end;
      AccWidth := AccWidth + Width;
      CurrStr := CurrStr + WordStr;
    end;
    AResWidth := Max(AResWidth, AccWidth);
    if ALinesFound <> nil then
      ALinesFound.Add(CurrStr);
    Inc(Result);
  finally
    WordList.Free;
  end;
end;

class procedure TTextWordWrapping.GetLines(const AText: String; const ACanvas: TCanvas; const AMaxWidth: Integer;
  var ALinesFound: TStringList; var AResWidth: Integer);
var
  ResWidth: Single;
begin
  TTextWordWrappingImpl.GetLines(AText, ACanvas, AMaxWidth, ALinesFound, ResWidth);
  AResWidth := Ceil(ResWidth);
end;

class function TTextWordWrapping.ComputeLineCount(const AText: String; const ACanvas: TCanvas; const AMaxWidth: Integer;
  var AResWidth: Integer): Integer;
var
  ResWidth: Single;
  NilList: TStringList;
begin
  NilList := nil;
  Result := TTextWordWrappingImpl.GetLines(AText, ACanvas, AMaxWidth, NilList, ResWidth);
  AResWidth := Ceil(ResWidth);
end;

{$ENDREGION 'TTextWordWrapping' }

end.

