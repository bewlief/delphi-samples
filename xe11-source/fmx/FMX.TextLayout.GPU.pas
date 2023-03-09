{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.TextLayout.GPU;

{$MINENUMSIZE 4}

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Generics.Collections, System.Math.Vectors, FMX.Types, FMX.TextLayout,
  FMX.FontGlyphs, FMX.Graphics, FMX.Surfaces;

type
  PCharRec = ^TCharRec;
  TCharRec = record
    Glyph: TFontGlyph;
    SrcRect: TRectF;
    Bitmap: TBitmap;
    BitmapRef: Boolean;
    constructor Create(const AGlyph: TFontGlyph);
    function HasNoEmptyBitmap: Boolean;
  end;

  TCharDic = class(TDictionary<UCS4String, PCharRec>)
  private
    FBaseline: Single;
    procedure CharRecNotifyHandler(Sender: TObject; const Value: PCharRec; Action: TCollectionNotification);
  public
    constructor Create(ACapacity: Integer = 0);
    property Baseline: Single read FBaseline write FBaseline;
  end;

  TFamilyDic = class(TObjectDictionary<Int64, TCharDic>);

{ Objects pool }

  /// <summary>Special version of TObject, which can be reused with <c>TGPUObjectsPool</c>.</summary>
  TReusableObject = class(TObject)
  protected
    procedure Init; virtual;
  public
    /// <summary>Creates object with using objects pool. If Pool is enabled, that object will be requested from Pool.
    /// If pool has one available object, it will be returned. Otherwise it will allocate and create new one.</summary>
    class function CreateObject<T: TReusableObject, constructor>: T;

    /// <summary>Destroys current object with using objects pool. If Pool is enabled, current object will be put into Pool.
    /// Otherwise it will destroy and release object memory.</summary>
    procedure DestroyObject;
  end;
  TReusableObjectClass = class of TReusableObject;

  /// <summary>Special version of TList, which can be reused with <c>TGPUObjectsPool</c>.</summary>
  TReusableList<T: TReusableObject, constructor> = class(TReusableObject)
  private
    FItems: TList<T>;
    function GetCount: Integer; inline;
    function GetItems(const AIndex: Integer): T; inline;
    { Handler }
    procedure NotifyEventHandler(Sender: TObject; const Item: T; Action: TCollectionNotification);
  protected
    procedure Init; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: T; inline;
    procedure Delete(const AIndex: Integer); inline;
    procedure DeleteRange(const AIndex: Integer; const ACount: Integer); inline;
    procedure Remove(const AItem: T); inline;
    procedure Insert(const AIndex: Integer; const AItem: T); inline;
    function First: T; inline;
    function Last: T; inline;

    property Items[const AIndex: Integer]: T read GetItems; default;
    property Count: Integer read GetCount;
  end;

  /// <summary>Objects pool, which allows to save time for memory allocation/release for gpu objects
  /// (like a frames, lines, runs).</summary>
  TGPUObjectsPool = class
  private
    class var FInstance: TGPUObjectsPool;
    class var FDisabled: Boolean;
    class function GetInstance: TGPUObjectsPool; static;
    class procedure SetDisabled(const Value: Boolean); static;
    class constructor Create;
  private
    FStorage: TDictionary<TClass, TObjectList<TReusableObject>>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Returns/allocates object of specified class T. If pool has free object of specified class, it will be
    /// returned, otherwise pool will create new object.</summary>
    function GetObject<T: TReusableObject, constructor>: T;
    /// <summary>Puts used object to pool for future reuse.</summary>
    procedure ReturnObject(const AObject: TReusableObject);

    /// <summary>Is pool available for using.</summary>
    class function IsAvailable: Boolean;
    /// <summary>Makes final cleaning up of pool.</summary>
    class procedure Uninitialize;
    /// <summary>Current object pool.</summary>
    class property Instance: TGPUObjectsPool read GetInstance;
    /// <summary>This option allows to turn on/off using pool for TGPUTextLayout.</summary>
    class property Disabled: Boolean read FDisabled write SetDisabled;
  end;

(*
  --------GPUFrame-------------
  |(GPURun)(GPURun)...(GPURun)| <- GPULine (several GPURun's with different font and/or color)
  |(GPURun)                   | <- GPULine (no additional styling, so only a single GPURun)
  |(GPURun)                   | <- GPULine
  |                           | ...
  |                           |
  |                           |
  -----------------------------
*)

  TGPURun = class(TReusableObject)
  private
    FChars: TList<UCS4String>;
    FGlyphs: TList<TSizeF>;
    FStartIndex: Integer;
    FLength: Integer;
    FImageRect: TRectF;
    FColor: TAlphaColor;
    [Weak] FFont: TFont;
    FIsDefaultColor: Boolean;
    FClipped: Boolean;
    FBaseline: Single;
    function GetEndIndex: Integer;
    function GetWidth: Single; inline;
    function GetHeight: Single; inline;
  protected
    procedure Init; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetColor(const AColor: TAlphaColor; const AIsDefault: Boolean);
    procedure Clip;
    procedure SetText(const AText: string; const AStartIndex, ALength: Integer);
    procedure AddChar(const AChar: UCS4String; const AGlyphSize: TSizeF; const ALength: Integer); overload;
    procedure DeleteLastChar;
    function ToString: string; override;
    procedure NormalizeHeight;

    property Chars: TList<UCS4String> read FChars;
    property Glyphs: TList<TSizeF> read FGlyphs;
    property StartIndex: Integer read FStartIndex;
    property EndIndex: Integer read GetEndIndex;
    property Length: Integer read FLength;
    property ImageRect: TRectF read FImageRect write FImageRect;
    property Color: TAlphaColor read FColor;
    property IsDefaultColor: Boolean read FIsDefaultColor;
    property Font: TFont read FFont write FFont;
    property IsClipped: Boolean read FClipped;
    property Baseline: Single read FBaseline write FBaseline;
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
  end;

  TGPULine = class(TReusableList<TGPURun>)
  private
    FHeight: Single;
    FWidth: Single;
    FTopLeft: TPointF;
  protected
    procedure Init; override;
  public
    function IsEmpty: Boolean;
    function CalculateBaseline: Single;
    procedure AdjustHeight;
    procedure MarkAsClipped;
    function ToString: string; override;

    property Height: Single read FHeight write FHeight;
    property Width: Single read FWidth write FWidth;
    property TopLeft: TPointF read FTopLeft write FTopLeft;
  end;

  TGPUFrame = class(TReusableList<TGPULine>)
  private
    FHeight: Single;
    FWidth: Single;
    FTopLeft: TPointF;
  protected
    procedure Init; override;
  public
    procedure ApplyDefaultColor(const AColor: TAlphaColor);
    function ToString: string; override;

    property TopLeft: TPointF read FTopLeft write FTopLeft;
    property Height: Single read FHeight write FHeight;
    property Width: Single read FWidth write FWidth;
  end;

  TFrameRender = class;

  /// <summary>Centralized management of graphical representations of characters.</summary>
  TGlyphsManager = class
  public const
    AntialiasMargin = 1;
    MaxUsefulCharMapOccupancy = 0.95; // At 5% of free space or lower the charmap is considered full.
  public type
    TCharMap = class
      Texture: TBitmap;
      BinPack: TGuillotineBinPack;
      constructor Create(const AScale: Single);
      destructor Destroy; override;
    end;
    TCharMaps = TObjectList<TCharMap>;
  private
    class var FDefault: TGlyphsManager;
    class function GetDefault: TGlyphsManager; static; inline;
  private
    FFamilyDic: TFamilyDic;
    FCharMaps: TCharMaps;
    FRendering: Integer;
    FNewGlyphList: TList<PCharRec>;
    FDisableGlyphPopulation: Boolean;
    function AddCharacterGlyph(const ACharacter: UCS4String; const AFont: TFont; const AScale: Single; const APathPresentation: Boolean): PCharRec;
    procedure UpdateCharacterGlyph(var AGlyphRec: PCharRec; const ACharacter: UCS4String; const AFont: TFont; const AScale: Single; const APathPresentation: Boolean);
    procedure PutGlyphToCharMaps(const ACharRec: PCharRec);
    class function FontToId(const AFont: TFont; const AScale: Single): Int64;
    class procedure Copy(const ASource: TBitmapSurface; const ADest: TBitmap; const AOffset: TPoint;
                         const ANeedClearDest: Boolean = True);
  public
    constructor Create;
    destructor Destroy; override;

    function GetCharGlyph(const ACharacter: UCS4String; const AFont: TFont; const AScale: Single; const APathPresentation: Boolean = False): PCharRec; overload;
    function GetCharGlyph(const CharDic: TCharDic; const ACharacter: UCS4String; const AFont: TFont; const AScale: Single; const APathPresentation: Boolean = False): PCharRec; overload;
    function GetCharDictionary(const AFont: TFont; const AScale: Single): TCharDic;

    { Optimization for disable putting glyph presentation into CharMaps, when user fast scrolls content }
    procedure BeginRender;
    procedure EndRender;
    property DisableGlyphPopulation: Boolean read FDisableGlyphPopulation write FDisableGlyphPopulation;

    /// <summary>List of packaged glyphs graphic presentations. Several glyphs are packed into one Texture.
    /// It increases speed of loading texture into graphic context.</summary>
    property CharMaps: TCharMaps read FCharMaps;

    class procedure Uninitialize;
    class property Default: TGlyphsManager read GetDefault;
  end;

  TGPUTextLayout = class(TTextLayout)
  public
    {$REGION 'Settings'}
    /// Do We need to automatically wrap spaces on a new line or leave them on the previous one.
    class var MoveSpacesOnTheNewLine: Boolean;
    {$ENDREGION}
    {$REGION 'Debuggging'}
    /// <summary>Allows to draw glyph bounds for debugging.</summary>
    /// <remarks>Using this setting significantly slows down the speed of text rendering.</remarks>
    class var DebugDrawGlyphBounds: Boolean;
    /// <summary>Allows to draw text bounds for debugging.</summary>
    class var DebugDrawTextBounds: Boolean;
    {$ENDREGION}
  private
    FFrame: TGPUFrame;
    FRender: TFrameRender;
    FNeedUpdateColor: Boolean;
    FScale: Single;
    FScaleFactor: Single;
    function GetGlyphAdvance(const ARec: PCharRec): Single; inline;
    function MeasureRange(APos, ALength: Integer): TRegion;
    function GetCharDictionary(const AFont: TFont = nil): TCharDic;
    function GetCharGlyph(const Ch: UCS4String; const CharDic: TCharDic; const AFont: TFont;
                          const NeedPath: Boolean = False): PCharRec;
    function GetRender: TFrameRender;
    procedure UpdateDefaultColor;
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    procedure DoColorChanaged; override;
    function GetTextHeight: Single; override;
    function GetTextWidth: Single; override;
    function GetTextRect: TRectF; override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
    procedure ReleaseFrames;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    class procedure Uninitialize;

    procedure ConvertToPath(const APath: TPathData); override;

    property Render: TFrameRender read GetRender;
  end;

  TGPURunAction = (BeginNewRun, Continue);

  /// <summary>The current context of the formed part of the word.</summary>
  TPartContext = class
  public
    /// <summary>Beginning character index of part.</summary>
    BeginIndex: Integer;
    /// <summary>Sizes of all glyphs in part.</summary>
    Sizes: TList<TSizeF>;
    /// <summary>Attribute indexes of all characters in a part. -1 is default font settings from TTextLayout</summary>
    AttributesIndex: TList<Integer>;
    /// <summary>Helpers markers for <c>TGPURun</c> management. It allows to understand, when new attribute is beginning.</summary>
    RunsActions: TList<TGPURunAction>;
    /// <summary>Part width.</summary>
    Width: Single;
    /// <summary>Part width without spaces in the beginning.</summary>
    TrimmedWidth: Single;
    /// <summary>Spaces count in the beginning of part.</summary>
    LeftSpacesCount: Integer;
    IsLastSpace: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AddSize(const ASize: TSizeF; const AAttributeIndex: Integer; const AAction: TGPURunAction);
  end;

  /// <summary>Splits the text into a formatted string, given the font settings, word wrap and trimming.</summary>
  TLineReader = class
  public const
    EllipsisChar: UCS4String = [$2026, 0];
  private
    FCharacters: TList<UCS4String>;
    FCharactersOffsets: TList<Integer>;
    FLineBreaks: TList<Integer>;
    FLineBreaksLength: TList<Integer>;
    { Settings }
    FWordWrap: Boolean;
    FTrimming: TTextTrimming;
    FDefaultColor: TAlphaColor;
    FScale: Single;
    FScaleFactor: Single;
    [Weak] FDefaultFont: TFont;
    { Default measured sizes }
    FEllipsisGlyphSize: TSizeF;
    FDefaultLineSize: TSizeF;
    FMaxSize: TSizeF;
    FLineBreakChars: UCS4String;
    { Context }
    FCharacterIndex: Integer;
    FPreviousLineBreakIndex: Integer;
    FGlyphSize: TSizeF;
    FPartContext: TPartContext;
    { Attributes }
    FAttributes: TList<Pointer>; // Weak TTextAttributedRange list
    FCurrentAttributeIndex: Integer;
    FSelectedAttributeIndex: Integer;
    FAttributeAction: TGPURunAction;
    { Current line measurement }
    FCurrentLineWidth: Single;
    FRemainLineWidth: Single;
    [Weak] FFrame: TGPUFrame;
    [Weak] FCurrentLine: TGPULine;
    [Weak] FCurrentRun: TGPURun;
    procedure SetText(const Value: string);
    function GetAttribute(const Index: Integer): TTextAttributedRange;
    procedure SetScale(const Value: Single);
  protected
    procedure Init;
    { Measurement of Glyph }
    function GetGlyphSize: TSizeF; overload;
    function GetGlyphSize(const ACharacter: UCS4String): TSizeF; overload;
    function GetBaseline(const AFont: TFont): Single; inline;
    function HasAvailableSpaceInCurrentLine: Boolean;
    function HasAvailableSpaceInNextLine: Boolean;
    /// <summary>Returns true, if current character is word splitter and we need to split words before this character.</summary>
    function IsSplitterBefore: Boolean;
    /// <summary>Returns true, if current character is word splitter and we need to split words after this character.</summary>
    function IsSplitterAfter: Boolean;
    function IsSpace: Boolean; overload;
    function IsSpace(const AGlyphIndex: Integer): Boolean; overload;
    function IsSpace(const ACharacter: UCS4String): Boolean; overload;
    function IsLineBreak: Boolean; overload;
    function IsLineBreak(const ASource: string; const ABeginIndex: Integer; var ALength: Integer): Boolean; overload;
    { Part building }
    procedure BeginPart;
    procedure AddCharToPart;
    procedure EndPart;
    { Line management }
    procedure CloseCurrentRun;
    procedure CloseCurrentLine;
    procedure BeginNewLine; overload;
    procedure BeginNewLine(const AStartCharacterIndex: Integer); overload;
    procedure BeginNewRun(const AStartOffset: Integer; const AAttributeIndex: Integer = -1);
    procedure RollbackGlyph;
    procedure RollbackWord;
    function IsRunEmpty: Boolean;
    function IsLineEmpty: Boolean;
    function IsFirstLine: Boolean;
    function IsFrameEmpty: Boolean;
    { Character }
    function GetCharacterLength(const ACharacterIndex: Integer): Integer; inline;
    { Cursor navigation }
    procedure NextChar;
    procedure SkipChars(const ACount: Integer);
    procedure SkipLineBreak;
    function IsEOL: Boolean;
  public
    constructor Create(const ADefaultFont: TFont);
    destructor Destroy; override;

    procedure FillLines(const AFrame: TGPUFrame);

    procedure AddAttribute(const AAttribute: TTextAttributedRange);
    procedure ClearAttributes;

    property Attributes[const Index: Integer]: TTextAttributedRange read GetAttribute;
    property Scale: Single read FScale write SetScale;
    property MaxSize: TSizeF read FMaxSize write FMaxSize;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultFont: TFont read FDefaultFont;
    property Text: string write SetText;
    property Trimming: TTextTrimming read FTrimming write FTrimming;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  end;

  /// <summary>Renders TGPUFrame on Canvas.</summary>
  TFrameRender = class
  protected type
    TPixelAlignment = (Horizontal, Vertical);
    TPixelAlignments = set of TPixelAlignment;
  private
    [Weak] FDefaultFont: TFont;
    FStrokeBrush: TStrokeBrush;
    FScale: Single;
    FScaleFactor: Single;
    FOpacity: Single;
    FOutputBounds: TRectF;
    { Rendering context }
    FCanvasClipRect: TRectF;
    FPixelAlignments: TPixelAlignments;
    [Weak] FFrame: TGPUFrame;
    [Weak] FCanvas: TCanvas;
    procedure SetScale(const Value: Single);
    function GetMaxSize: TSizeF;
    function GetTopLeft: TPointF;
    procedure SetMaxSize(const Value: TSizeF);
    procedure SetTopLeft(const Value: TPointF);
  protected
    procedure RecalculateCanvasClipRect;
    procedure DifinePixelAlignment;
    function CalculateGlyphDestRect(const APosition: TPointF; const ABaselineOffset: Single; const AGlyphRec: PCharRec): TRectF;
    function IsOutOfCanvasArea(const ARect: TRectF): Boolean; inline;
    function IsOutOfOutputArea(const ARect: TRectF): Boolean; inline;
    class function ConvexHull(const APolygon: TPolygon): TRectF;
    { Drawing }
    procedure DrawTextDecorationIfNeeded(const ARun: TGPURun; const AFirstGlyphPos: TPointF; const ABaselineOffset: Single);
    procedure DrawGlyphBounds(const AGlyph: TFontGlyph; const ABounds: TRectF);
    procedure DrawTextBounds(const ABounds: TRectF);
  public
    constructor Create(const ADefaultFont: TFont);
    destructor Destroy; override;

    procedure Render(const AFrame: TGPUFrame; const ACanvas: TCanvas; const AOpacity: Single);

    property Scale: Single read FScale write SetScale;
    property MaxSize: TSizeF read GetMaxSize write SetMaxSize;
    property TopLeft: TPointF read GetTopLeft write SetTopLeft;
  end;

/// <summary>Reads from <c>AText</c> string unicode character from <c>AStartIndex</c> position. Function returns length
/// of read character in <c>ARunLength</c>.
/// </summary>
function ReadCharacter(const AText: string; const AStartIndex: Integer; var ARunLength: Integer): UCS4String;

implementation

uses
  System.Classes, System.Math, System.SysUtils, System.Character, System.RTLConsts, System.Hash,
  System.Diagnostics, System.Generics.Defaults, FMX.Consts, FMX.Platform, FMX.Canvas.GPU, FMX.Text;

function IsRegionalIndicator(const AChar: UCS4Char): Boolean;
const
  FlagA = $1F1E6;
  FlagZ = $1F1FF;
begin
  Result := InRange(AChar, FlagA, FlagZ);
end;

function IsEmojiModifier(const AChar: UCS4Char): Boolean;
const
  FirstSkinModifier = $1F3FB;
  lastSkinModifier = $1F3FF;
begin
  Result := InRange(AChar, FirstSkinModifier, lastSkinModifier);
end;

function ReadCharacter(const AText: string; const AStartIndex: Integer; var ARunLength: Integer): UCS4String;
var
  CharIndex: Integer;
  CharLength: Integer;
  I: Integer;
  RemainLength: Integer;
  Category: TUnicodeCategory;
  NeedExit: Boolean;
  IsFlagCode: Boolean;
  NextCode: UCS4Char;

  function IsEOL: Boolean;
  begin
    Result := CharIndex >= RemainLength;
  end;

  function ReadCode: UCS4Char;
  begin
    Result := System.Char.ConvertToUtf32(AText, CharIndex, CharLength);
    Inc(ARunLength, CharLength);
    Inc(CharIndex, CharLength);
  end;

begin
  RemainLength := AText.Length;
  SetLength(Result, RemainLength * 2);
  CharIndex := AStartIndex;
  I := 0;
  ARunLength := 0;
  NeedExit := True;
  repeat
    Category := Char.GetUnicodeCategory(AText, CharIndex);
    Result[I] := ReadCode;

    IsFlagCode := IsRegionalIndicator(Result[I]);
    Inc(I);

    if Category = TUnicodeCategory.ucControl then
      Break;

    // https://unicode.org/reports/tr51/#def_emoji_flag_sequence
    //
    // emoji_flag_sequence := regional_indicator regional_indicator
    // regional_indicator := \p{Regional_Indicator}
    if IsFlagCode then
    begin
      // Skip regional indicator
      if not IsEOL then
      begin
        NextCode := System.Char.ConvertToUtf32(AText, CharIndex, CharLength);
        if IsRegionalIndicator(NextCode) then
        begin
          Result[I] := ReadCode;
          Inc(I);
        end;

        NeedExit := True;
      end;
    end
    else
    begin
      // Skip Modifier
      //
      // https://unicode.org/reports/tr51/#Emoji_Modifiers
      //
      // emoji_modifier := \p{Emoji_Modifier}
      while not IsEOL do
      begin
        NextCode := System.Char.ConvertToUtf32(AText, CharIndex, CharLength);

        if not IsEmojiModifier(NextCode) then
          Break;

        Result[I] := ReadCode;
        Inc(I);
      end;

      NeedExit := True;
      // Skip format, NonSpacingMark and EnclosingMark
      while not IsEOL do
      begin
        Category := Char.GetUnicodeCategory(AText, CharIndex);
        if not (Category in [TUnicodeCategory.ucFormat, TUnicodeCategory.ucNonSpacingMark, TUnicodeCategory.ucEnclosingMark]) then
          Break;

        Result[I] := ReadCode;
        Inc(I);
        if Category = TUnicodeCategory.ucFormat then
          NeedExit := False;
      end;

      // Stop on Control character
      if not IsEOL then
      begin
        Category := Char.GetUnicodeCategory(AText, CharIndex);
        if Category = TUnicodeCategory.ucControl then
          Break;
      end;
    end;
  until IsEOL or NeedExit;

  SetLength(Result, I + 1);
  Result[I] := 0;
end;


function IsCombiningCharacter(const Ch: Char): Boolean;
begin
  Result := Ch.GetUnicodeCategory in [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucEnclosingMark, TUnicodeCategory.ucNonSpacingMark]
end;

{ TCharDic }

procedure TCharDic.CharRecNotifyHandler(Sender: TObject; const Value: PCharRec; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
  begin
    FreeAndNil(Value.Glyph);
    if not Value.BitmapRef then
      FreeAndNil(Value.Bitmap);
    Dispose(Value);
  end;
end;

constructor TCharDic.Create(ACapacity: Integer);
begin
  inherited Create(ACapacity);
  OnValueNotify := CharRecNotifyHandler;
end;

{ TGPURun }

procedure TGPURun.AddChar(const AChar: UCS4String; const AGlyphSize: TSizeF; const ALength: Integer);
begin
  FChars.Add(AChar);
  FGlyphs.Add(AGlyphSize);
  Inc(FLength, ALength);

  FImageRect.Width := FImageRect.Width + AGlyphSize.Width;
  FImageRect.Height := Max(FImageRect.Height, AGlyphSize.Height);
end;

procedure TGPURun.Clip;
begin
  if not FClipped then
    FClipped := True;
end;

constructor TGPURun.Create;
begin
  inherited;
  FChars := TList<UCS4String>.Create;
  FGlyphs := TList<TSizeF>.Create;
  Init;
end;

procedure TGPURun.DeleteLastChar;
var
  LastGlyphSize: TSizeF;
begin
  if FChars.Count = 0 then
    Exit;

  LastGlyphSize := FGlyphs.Last;
  Dec(FLength, UCS4StringToUnicodeString(FChars.Last).Length);
  FChars.Delete(FChars.Count - 1);
  FGlyphs.Delete(FGlyphs.Count - 1);
  FImageRect.Width := FImageRect.Width - LastGlyphSize.Width;
end;

destructor TGPURun.Destroy;
begin
  FreeAndNil(FGlyphs);
  FreeAndNil(FChars);
  inherited;
end;

function TGPURun.GetEndIndex: Integer;
begin
  Result := StartIndex + Length;
end;

function TGPURun.GetHeight: Single;
begin
  Result := FImageRect.Height;
end;

function TGPURun.GetWidth: Single;
begin
  Result := FImageRect.Width;
end;

procedure TGPURun.Init;
begin
  inherited;
  FChars.Clear;
  FGlyphs.Clear;
  FIsDefaultColor := True;
  FLength := 0;
  FStartIndex := 0;
  FImageRect := TRectF.Empty;
  FColor := TAlphaColorRec.Null;
  FFont := nil;
  FClipped := False;
  FBaseline := 0;
end;

procedure TGPURun.NormalizeHeight;
var
  I: Integer;
  LHeight: Single;
begin
  LHeight := 0;
  for I := 0 to FGlyphs.Count - 1 do
    LHeight := Max(LHeight, FGlyphs[I].Height);
  FImageRect.Height := LHeight;
end;

procedure TGPURun.SetColor(const AColor: TAlphaColor; const AIsDefault: Boolean);
begin
  if AIsDefault then
    if IsDefaultColor then
    //Just changing value of default color
      FColor := AColor
    else
  else
  begin
    //Overriding default color with attribute color
    FColor := AColor;
    FIsDefaultColor := False;
  end;
end;

procedure TGPURun.SetText(const AText: string; const AStartIndex, ALength: Integer);
var
  I, CharLength: Integer;
begin
  FStartIndex := AStartIndex;
  FLength := ALength;
  FChars.Clear;
  I := 0;
  while I < FLength do
  begin
    FChars.Add(ReadCharacter(AText, I + FStartIndex, CharLength));
    Inc(I, CharLength);
  end;
end;

function TGPURun.ToString: string;
var
  Text: string;
  I: Integer;
begin
  Text := string.Empty;
  for I := 0 to FChars.Count - 1 do
    Text := Text + UCS4StringToUnicodeString(FChars[I]);
  Result := Format('TGPURun{text="%s", startIndex=%d, length=%d}', [Text, StartIndex, Length]);
end;

{ TGPULine }

procedure TGPULine.AdjustHeight;
var
  LHeight: Single;
  I: Integer;
begin
  LHeight := 0;
  for I := 0 to Count - 1 do
    LHeight := Max(LHeight, Items[I].Height);

  Height := LHeight;
end;

function TGPULine.CalculateBaseline: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].Baseline);
end;

procedure TGPULine.Init;
begin
  inherited;
  FHeight := 0;
  FWidth := 0;
  FTopLeft := TPointF.Zero;
end;

function TGPULine.IsEmpty: Boolean;
begin
  Result := (Count = 0) or (Count = 1) and (Items[0].FChars.Count = 0);
end;

procedure TGPULine.MarkAsClipped;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Clip;
end;

function TGPULine.ToString: string;
var
  Text: string;
  I: Integer;
begin
  Text := string.Empty;
  for I := 0 to Count - 1 do
  begin
    Text := Text + Items[I].ToString;
    if I <> Count - 1 then
      Text := Text + ', ';
  end;
  Result := Format('TGPULine{runs=[%s], width=%f, height=%f}', [Text, Width, Height]);
end;

{ TGPUFrame }

procedure TGPUFrame.ApplyDefaultColor(const AColor: TAlphaColor);
var
  I: Integer;
  Line: TGPULine;
  J: Integer;
  Run: TGPURun;
begin
  for I := 0 to Count - 1 do
  begin
    Line := Items[I];
    for J := 0 to Line.Count - 1 do
    begin
      Run := Line[J];
      Run.SetColor(AColor, True);
    end;
  end;
end;

procedure TGPUFrame.Init;
begin
  inherited;
  FHeight := 0;
  FWidth := 0;
  FTopLeft := TPointF.Zero;
end;

function TGPUFrame.ToString: string;
var
  Text: string;
  I: Integer;
begin
  Text := string.Empty;
  for I := 0 to Count - 1 do
  begin
    Text := Text + Items[I].ToString;
    if I <> Count - 1 then
      Text := Text + ', ';
  end;
  Result := Format('TGPUFrame{lines=[%s], width=%f, height=%f}', [Text, Width, Height]);
end;

{ TGPUTextLayout }

constructor TGPUTextLayout.Create(const ACanvas: TCanvas);
var
  ScreenSrv: IFMXScreenService;
begin
  inherited Create(ACanvas);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSrv) then
    FScale := ScreenSrv.GetScreenScale
  else
    FScale := 1;
  FScaleFactor := 1 / FScale;
  FFrame := TGPUFrame.CreateObject<TGPUFrame>;
  FNeedUpdateColor := True;
end;

destructor TGPUTextLayout.Destroy;
begin
  FreeAndNil(FRender);
  ReleaseFrames;
  inherited;
end;

class procedure TGPUTextLayout.Uninitialize;
begin
  TFontGlyphManager.UnInitialize;
  TGlyphsManager.Uninitialize;
  TGPUObjectsPool.Uninitialize;
end;

procedure TGPUTextLayout.UpdateDefaultColor;
begin
  try
    FFrame.ApplyDefaultColor(Color);
  finally
    FNeedUpdateColor := False;
  end;
end;

procedure TGPUTextLayout.ConvertToPath(const APath: TPathData);

  procedure ApplyHorizontalAlignment(const ALinePath: TPathData; const AVerticalAdvance: Single);
  var
    TextR: TRectF;
  begin
    TextR := ALinePath.GetBounds;
    case HorizontalAlign of
      TTextAlign.Center:
        begin
          OffsetRect(TextR, -TextR.Left, 0);
          OffsetRect(TextR, (MaxSize.X - Padding.Left - Padding.Right - TextR.Width) / 2, 0);
          OffsetRect(TextR, TopLeft.X, 0);
        end;
      TTextAlign.Leading:
        begin
          OffsetRect(TextR, -TextR.Left, 0);
          OffsetRect(TextR, TopLeft.X, 0);
        end;
      TTextAlign.Trailing:
        begin
          OffsetRect(TextR, -TextR.Left, 0);
          OffsetRect(TextR, (MaxSize.X - Padding.Left - Padding.Right - TextR.Width), 0);
          OffsetRect(TextR, TopLeft.X, 0);
        end;
    end;
    ALinePath.Translate(TextR.Left, AVerticalAdvance);
  end;

  procedure ApplyVerticalAlignment;
  var
    TextR: TRectF;
  begin
    TextR := APath.GetBounds;
    APath.Translate(0, -TextR.Top);
    case VerticalAlign of
      TTextAlign.Center:
        APath.Translate(0, (MaxSize.Y - Padding.Top - Padding.Bottom - TextR.Height) / 2);
      TTextAlign.Leading:;
      TTextAlign.Trailing:
        APath.Translate(0, (MaxSize.Y - Padding.Top - Padding.Bottom - TextR.Height));
    end;
    APath.Translate(0, TopLeft.Y);
  end;

var
  CharDic: TCharDic;
  Rec: PCharRec;
  LLine: TGPULine;
  LRun: TGPURun;
  I, J, K: Integer;
  LinePath: TPathData;
  LineAdvance, VerticalAdvance, LineVerticalAdvance: Single;
  BaselineMaxValue, BaselineOffset: Single;
  CharPath: TPathData;
begin
  if Text.IsEmpty then
    Exit;

  BaselineOffset := 0;
  BaselineMaxValue := GetCharDictionary(Font).Baseline;
  VerticalAdvance := 0;
  for I := 0 to FFrame.Count - 1 do
  try
    LinePath := TPathData.Create;
    LLine := FFrame[I];
    LineAdvance := 0;
    LineVerticalAdvance := 0;
    if AttributesCount > 0 then
    begin
      for J := 0 to LLine.Count - 1 do
      begin
        CharDic := GetCharDictionary(LLine[J].Font);
        BaselineMaxValue := Max(BaselineMaxValue, CharDic.Baseline);
      end;
    end;
    CharPath := TPathData.Create;
    try
      for J := 0 to LLine.Count - 1 do
      begin
        LRun := LLine[J];
        CharDic := GetCharDictionary(LRun.Font);
        if AttributesCount > 0 then
          BaselineOffset := (BaselineMaxValue - CharDic.Baseline) * FScaleFactor;
        for K := 0 to LRun.Chars.Count - 1 do
        begin
          Rec := GetCharGlyph(LRun.Chars[K], CharDic, LRun.Font, True);
          if (Rec.Glyph <> nil) and (Rec.Glyph.Path <> nil) then
          begin
            if not SameValue(BaselineOffset, 0, TEpsilon.FontSize) then
            begin
              CharPath.Assign(Rec.Glyph.Path);
              CharPath.Translate(0, BaselineOffset);
              LinePath.AddPath(CharPath);
            end
            else
              LinePath.AddPath(Rec.Glyph.Path);
          end;
          LinePath.Translate(-Rec.Glyph.Advance, 0);
          LineAdvance := LineAdvance + Rec.Glyph.Advance;
          LineVerticalAdvance := Max(LineVerticalAdvance, Rec.Glyph.VerticalAdvance);
        end;
      end;
    finally
      CharPath.Free;
    end;
    LinePath.Translate(LineAdvance, 0);
    //Aligning line
    ApplyHorizontalAlignment(LinePath, VerticalAdvance);
    VerticalAdvance := VerticalAdvance + LineVerticalAdvance;
    //
    APath.AddPath(LinePath);
  finally
    FreeAndNil(LinePath);
  end;
  // Vertical alignment
  ApplyVerticalAlignment;

  APath.Scale(FScaleFactor, FScaleFactor);
end;

procedure TGPUTextLayout.DoColorChanaged;
begin
  inherited;
  FNeedUpdateColor := True;
end;

procedure TGPUTextLayout.DoDrawLayout(const ACanvas: TCanvas);

  function IsScaleChanged: Boolean;
  begin
    Result := not SameValue(FScale, ACanvas.Scale, Single.Epsilon);
  end;

begin
  if Text.IsEmpty then
    Exit;

  if FNeedUpdateColor then
    UpdateDefaultColor;

  if IsScaleChanged then
  begin
    FScale := ACanvas.Scale;
    FScaleFactor := 1 / FScale;
    DoRenderLayout;
  end;

  Render.Scale := FScale;
  Render.TopLeft := TopLeft;
  Render.MaxSize := MaxSize;
  Render.Render(FFrame, ACanvas, Opacity);
end;

procedure TGPUTextLayout.DoRenderLayout;

  procedure AlignFrame;
  var
    LTop, LLeft: Single;
    I: Integer;
  begin
    LLeft := Padding.Left;
    case HorizontalAlign of
      TTextAlign.Center:
        begin
          LLeft := (MaxSize.X - Padding.Right - Padding.Left - FFrame.Width) / 2;
          for I := 0 to FFrame.Count - 1 do
            FFrame[I].TopLeft := TPointF.Create((MaxSize.X - Padding.Right - Padding.Left - FFrame[I].Width) / 2, 0);
        end;
      TTextAlign.Trailing:
        begin
          LLeft := MaxSize.X - Padding.Right - FFrame.Width;
          for I := 0 to FFrame.Count - 1 do
            FFrame[I].TopLeft := TPointF.Create(MaxSize.X - Padding.Right - FFrame[I].Width, 0);
        end;
    end;
    LTop := Padding.Top;
    case VerticalAlign of
      TTextAlign.Center:
        LTop := (MaxSize.Y - Padding.Top - Padding.Bottom - FFrame.Height) / 2;
      TTextAlign.Trailing:
        LTop := MaxSize.Y - Padding.Bottom - FFrame.Height;
    end;
    FFrame.TopLeft := TPointF.Create(LLeft, LTop);
    for I := 0 to FFrame.Count - 1 do
    begin
      FFrame[I].TopLeft := TPointF.Create(FFrame[I].TopLeft.X, LTop);
      LTop := LTop + FFrame[I].Height;
    end;
  end;

  procedure ApplyGlyphsClipping;
  var
    I, J, K: Integer;
    X: Single;
    Run: TGPURun;
    Rec: PCharRec;
    ChDic: TCharDic;
    Line: TGPULine;
  begin
    if (FFrame.Width < MaxSize.X) and (FFrame.Height < MaxSize.Y) then
      Exit;

    //Checking for lines upper than top border
    if VerticalAlign <> TTextAlign.Leading then
      for I := 0 to FFrame.Count - 1 do
      begin
        Line := FFrame[I];
        if Line.TopLeft.Y < 0 then
          Line.MarkAsClipped
        else
          Break;
      end;

    //Checking for lines lower than bottom border
    for I := FFrame.Count - 1 downto 0 do
    begin
      Line := FFrame[I];
      if Line.TopLeft.Y > MaxSize.Y then
        Continue
      else if Line.TopLeft.Y + Line.Height > MaxSize.Y then
        Line.MarkAsClipped
      else
        Break;
    end;

    //
    for I := 0 to FFrame.Count - 1 do
    begin
      Line := FFrame[I];
      if Line.Width > MaxSize.X then
      begin
        //Checking for characters that are lefter than left border
        if HorizontalAlign <> TTextAlign.Leading then
        begin
          X := Line.TopLeft.X;
          if X < 0 then
            while Line.Count > 0 do
              if X < 0 then
              begin
                Run := Line[0];
                if Run.Length > 0 then
                begin
                  ChDic := GetCharDictionary(Run.Font);
                  while (X < 0) and (Run.Chars.Count > 0) do
                  begin
                    Run.Clip;
                    Rec := GetCharGlyph(Run.Chars[0], ChDic, Run.Font);
                    X := X + GetGlyphAdvance(Rec);
                  end;
                end
                else
                  Break;
              end
              else
                Break;
        end;
        //Checking for characters that are righter than right border
        if HorizontalAlign <> TTextAlign.Trailing then
        begin
          X := Line.TopLeft.X;
          J := 0;
          while (X < MaxSize.X) and (J < FFrame[I].Count) do
          begin
            Run := Line[J];
            ChDic := GetCharDictionary(Run.Font);
            for K := 0 to Run.Chars.Count - 1 do
            begin
              Rec := GetCharGlyph(Run.Chars[K], ChDic, Run.Font);
              X := X + GetGlyphAdvance(Rec);
              if X > MaxSize.X then
                Run.Clip;
            end;
            Inc(J);
          end;
        end;
      end;
    end;
  end;

  procedure ReduceFrameSize;
  var
    MaxWidth: Single;
    I: Integer;
  begin
    MaxWidth := 0;
    for I := 0 to FFrame.Count - 1 do
      MaxWidth := Max(MaxWidth, FFrame[I].Width);
    FFrame.Width := MaxWidth;
  end;

var
  LineReader: TLineReader;
  I: Integer;
begin
  if (LayoutCanvas <> nil) and not SameValue(FScale, LayoutCanvas.Scale, Epsilon) then
  begin
    FScale := LayoutCanvas.Scale;
    FScaleFactor := 1 / FScale;
  end;
  ReleaseFrames;

  FFrame := TGPUFrame.CreateObject<TGPUFrame>;
  LineReader := TLineReader.Create(Font);
  try
    LineReader.MaxSize := TSizeF.Create(MaxSize.X - Padding.Left - Padding.Right, MaxSize.Y - Padding.Top - Padding.Bottom);
    LineReader.Text := Text;
    LineReader.WordWrap := WordWrap;
    LineReader.Trimming := Trimming;
    LineReader.DefaultColor := Color;
    LineReader.Scale := FScale;
    LineReader.ClearAttributes;
    for I := 0 to AttributesCount - 1 do
      LineReader.AddAttribute(Attributes[I]);

    LineReader.FillLines(FFrame);
  finally
    LineReader.Free;
  end;

  AlignFrame;
  ApplyGlyphsClipping;

  ReduceFrameSize;
  FNeedUpdateColor := False;
end;

function TGPUTextLayout.GetCharDictionary(const AFont: TFont = nil): TCharDic;
var
  LFont: TFont;
begin
  if AFont = nil then
    LFont := Font
  else
    LFont := AFont;
  Result := TGlyphsManager.Default.GetCharDictionary(LFont, FScale);
end;

function TGPUTextLayout.GetGlyphAdvance(const ARec: PCharRec): Single;
begin
  Result := ARec.Glyph.Advance * FScaleFactor;
end;

function TGPUTextLayout.GetRender: TFrameRender;
begin
  if FRender = nil then
    FRender := TFrameRender.Create(Font);

  Result := FRender;
end;

function TGPUTextLayout.GetTextHeight: Single;
begin
  if not SameValue(MaxSize.Y, TTextLayout.MaxLayoutSize.Y, Epsilon) then
    Result := Min(FFrame.Height, MaxSize.Y)
  else
    Result := FFrame.Height;
end;

function TGPUTextLayout.GetTextRect: TRectF;
begin
  Result := TRectF.Create(FFrame.TopLeft, TextWidth, TextHeight);
  Result.Offset(TopLeft);
  if FFrame.TopLeft.Y < 0 then
    Result.Offset(0, Abs(FFrame.TopLeft.Y));
end;

function TGPUTextLayout.GetTextWidth: Single;
begin
  if not SameValue(MaxSize.X, TTextLayout.MaxLayoutSize.X, Epsilon) then
    Result := Min(FFrame.Width, MaxSize.X)
  else
    Result := FFrame.Width;
end;

function TGPUTextLayout.GetCharGlyph(const Ch: UCS4String; const CharDic: TCharDic; const AFont: TFont;
  const NeedPath: Boolean): PCharRec;
var
  LFont: TFont;
begin
  if AFont = nil then
    LFont := Font
  else
    LFont := AFont;

  Result := TGlyphsManager.Default.GetCharGlyph(CharDic, Ch, LFont, FScale, NeedPath);
end;

function TGPUTextLayout.MeasureRange(APos, ALength: Integer): TRegion;
var
  I, LengthOffset, LLength, CharLength: Integer;
  CharDic: TCharDic;
  Rec: PCharRec;
  R, R1: TRectF;
  Offset: Single;
  LRun: TGPURun;
  LineIndex, RunIndex: Integer;
  Line: TGPULine;
  GlyphAdvance: Single;
begin
  SetLength(Result, 0);

  LLength := Text.Length;
  //Skipping combining characters
  while ((APos + ALength) < LLength) and IsCombiningCharacter(Text.Chars[APos + ALength]) do
    Inc(ALength);
  if (APos < LLength) and Text.Chars[APos].IsLowSurrogate then
  begin
    Dec(APos);
    Inc(ALength);
  end;

  for LineIndex := 0 to FFrame.Count - 1 do
  begin
    Line := FFrame[LineIndex];

    Offset := 0;
    if (LineIndex > 0) and (Length(Result) > 0) and (FFrame[LineIndex - 1].Count > 0) and (Line.Count > 0) then
      Dec(ALength, Line.First.StartIndex - FFrame[LineIndex - 1].Last.StartIndex - FFrame[LineIndex - 1].Last.Length);

    for RunIndex := 0 to Line.Count - 1 do
    begin
      LRun := Line[RunIndex];
      LengthOffset := LRun.StartIndex;
      R := TRectF.Empty;
      if APos < LRun.EndIndex then
      begin
        CharDic := GetCharDictionary(LRun.Font);
        for I := 0 to LRun.Chars.Count - 1 do
        begin
          CharLength := UCS4StringToUnicodeString(LRun.Chars[I]).Length;
          Rec := GetCharGlyph(LRun.Chars[I], CharDic, LRun.Font);
          GlyphAdvance := GetGlyphAdvance(Rec);
          if LengthOffset < APos then
          begin
            Offset := Offset + GlyphAdvance;
            Inc(LengthOffset, CharLength);
          end
          else if ALength > 0 then
          begin
            R1 := TRectF.Create(TPointF.Create(Offset, 0), GlyphAdvance, FFrame[LineIndex].Height);
            if R.IsEmpty then
              R := R1
            else
              R.Union(R1);
            Offset := Offset + GlyphAdvance;
            Dec(ALength, CharLength);
            Inc(LengthOffset, CharLength);
          end
          else
            Break;
        end;
      end
      else
        if APos = LLength then
        begin
          R := LRun.ImageRect;
          R.Left := R.Right;
          Dec(ALength);
        end
        else if ALength > 0 then
          Offset := Offset + LRun.ImageRect.Width
        else
          Break;
      if R.Right > 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        R.Offset(Line.TopLeft);
        Result[High(Result)] := R;
        R := R.Empty;
      end;
    end;
    if R.Right > 0 then
    begin
      SetLength(Result, Length(Result) + 1);
      R.Offset(Line.TopLeft);
      Result[High(Result)] := R;
      R := R.Empty;
    end;
    if ALength = 0 then
      Exit;
  end;
end;

procedure TGPUTextLayout.ReleaseFrames;
begin
  if FFrame <> nil then
  begin
    FFrame.DestroyObject;
    FFrame := nil;
  end;
end;

function TGPUTextLayout.DoPositionAtPoint(const APoint: TPointF): Integer;

  function RegionContains(const ARect: TRectF; const LPoint: TPointF): Boolean;
  begin
    Result := ((LPoint.X > ARect.Left) or SameValue(LPoint.X, ARect.Left, Epsilon)) and
              ((LPoint.X < ARect.Right)or SameValue(LPoint.X, ARect.Right, Epsilon)) and
              ((LPoint.Y > ARect.Top) or SameValue(LPoint.Y, ARect.Top, Epsilon)) and
              ((LPoint.Y < ARect.Bottom) or SameValue(LPoint.Y, ARect.Bottom, Epsilon));
  end;

  function CharacterLength(const ACharacter: UCS4String): Integer;
  var
    I: Integer;
  begin
    Result := 1;
    for I := Low(ACharacter) to High(ACharacter) - 1 do // #0 terminated UCS4 string
      if ACharacter[I] > $FFFF then
        Inc(Result);
  end;

  function GetPositionInLine(const ALine: TGPULine; const AHitTest: TPointF) : Integer;
  var
    Offset: Single;
    RunIndex: Integer;
    LRun: TGPURun;
    CharsLength: Integer;
    I: Integer;
    Character: UCS4String;
    CharacterSize: TSizeF;
    LCharLength: Integer;
    CharacterRect: TRectF;
    Index: Integer;
  begin
    Result := -1;
    Offset := ALine.TopLeft.X;
    for RunIndex := 0 to ALine.Count - 1 do
    begin
      LRun := ALine[RunIndex];
      if LRun.Chars.Count = 0 then
        Exit(LRun.StartIndex);
      CharsLength := 0;
      for I := 0 to LRun.Chars.Count - 1 do
      begin
        Character := LRun.Chars[I];
        CharacterSize := LRun.Glyphs[I];
        LCharLength := CharacterLength(Character);

        CharacterRect := TRectF.Create(TPointF.Create(Offset, 0), CharacterSize.Width, ALine.Height);
        if RegionContains(CharacterRect, AHitTest) then
        begin
          Index := CharsLength + LRun.StartIndex;
          if AHitTest.X > (CharacterRect.Left + CharacterRect.Width / 2) then
            Inc(Index, LCharLength);
          Exit(Min(Index, Text.Length));
        end;
        Offset := Offset + CharacterRect.Width;
        Inc(CharsLength, LCharLength);
      end;
    end;
  end;

var
  HitTestPoint: TPointF;
  LineIndex: Integer;
  Line: TGPULine;
begin
  Result := -1;

  HitTestPoint := APoint - TopLeft - Padding.Rect.TopLeft;
  HitTestPoint.Offset(0, -FFrame.TopLeft.Y);

  for LineIndex := 0 to FFrame.Count - 1 do
  begin
    Line := FFrame[LineIndex];
    if HitTestPoint.Y > Line.Height then
    begin
      HitTestPoint.Offset(0, -Line.Height);
      Continue;
    end;

    if HitTestPoint.X < Line.TopLeft.X then
      Exit(Line.First.StartIndex);
    if HitTestPoint.X > (Line.TopLeft.X + Line.Width) then
      Exit(Line.Last.EndIndex);

    Result := GetPositionInLine(Line, HitTestPoint);
    Exit;
  end;
end;

function TGPUTextLayout.DoRegionForRange(const ARange: TTextRange): TRegion;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if (ARange.Pos < 0) or (ARange.Length < 0) then
    Exit;

  if (ARange.Pos = Text.Length) and (ARange.Length = 0) then
    if Text.IsEmpty then
    begin
      SetLength(Result, 1);
      Result[0] := Self.TextRect;
      Exit;
    end
    else
    begin
      Result := MeasureRange(Text.Length - 1, 1);
      for I := Low(Result) to High(Result) do
        Result[I].Left := Result[I].Right;
    end
  else
  begin
    Result := MeasureRange(ARange.Pos, ARange.Length);
    if Length(Result) = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := Self.TextRect;
      Result[0].Right := Result[0].Left;
      Exit;
    end;
  end;
  for I := Low(Result) to High(Result) do
    Result[I].Offset(TopLeft);
end;

{ TLineReader }

procedure TLineReader.AddAttribute(const AAttribute: TTextAttributedRange);
begin
  FAttributes.Add(AAttribute);
end;

procedure TLineReader.AddCharToPart;
begin
  if IsSpace then
  begin
    if FPartContext.Sizes.Count = 0 then
      FPartContext.IsLastSpace := True;
    if FPartContext.IsLastSpace then
      Inc(FPartContext.LeftSpacesCount);
  end
  else
    FPartContext.IsLastSpace := False;

  FPartContext.AddSize(FGlyphSize, FSelectedAttributeIndex, FAttributeAction);
end;

procedure TLineReader.BeginNewLine;
begin
  BeginNewLine(FCharacterIndex);
end;

procedure TLineReader.BeginNewLine(const AStartCharacterIndex: Integer);
begin
  if FCurrentLine <> nil then
    CloseCurrentLine;

  // Begin new line
  FCurrentLine := FFrame.Add;
  if AStartCharacterIndex = -1 then
    BeginNewRun(0, FSelectedAttributeIndex)
  else
    BeginNewRun(FCharactersOffsets[AStartCharacterIndex], FSelectedAttributeIndex);
  FRemainLineWidth := FMaxSize.Width;
  FCurrentLineWidth := 0;
end;

procedure TLineReader.BeginNewRun(const AStartOffset: Integer; const AAttributeIndex: Integer);
var
  Attribute: TTextAttribute;
begin
  FCurrentRun := FCurrentLine.Add;
  FCurrentRun.FStartIndex := AStartOffset;

  if AAttributeIndex = -1 then
  begin
    FCurrentRun.SetColor(FDefaultColor, True);
    FCurrentRun.Font := FDefaultFont;
  end
  else
  begin
    Attribute := TTextAttributedRange(FAttributes[AAttributeIndex]).Attribute;
    FCurrentRun.SetColor(Attribute.Color, False);
    FCurrentRun.Font := Attribute.Font;
  end;
  FCurrentRun.Baseline := GetBaseline(FCurrentRun.Font);
end;

procedure TLineReader.BeginPart;
begin
  FPartContext.BeginIndex := FCharacterIndex;
  FPartContext.Sizes.Clear;
  FPartContext.AttributesIndex.Clear;
  FPartContext.RunsActions.Clear;
  FPartContext.Width := 0;
  FPartContext.TrimmedWidth := 0;
  FPartContext.IsLastSpace := False;
  FPartContext.LeftSpacesCount := 0;
end;

procedure TLineReader.ClearAttributes;
begin
  FAttributes.Clear;
end;

procedure TLineReader.CloseCurrentLine;
begin
  FCurrentLine.Width := FCurrentLine.Width + FCurrentRun.Width;
  FCurrentLine.Height := Max(FCurrentLine.Height, FCurrentRun.Height);

  if FCurrentLine.IsEmpty then
  begin
    FCurrentLine.Width := FDefaultLineSize.Width;
    FCurrentLine.Height := FDefaultLineSize.Height;
  end;

  FFrame.Width := Max(FFrame.Width, FCurrentLine.Width);
  FFrame.Height := FFrame.Height + FCurrentLine.Height;
end;

procedure TLineReader.CloseCurrentRun;
begin
  // GPURun can be empty, if we are rollшing back characters by trimming.
  if FCurrentRun.FChars.Count = 0 then
    FCurrentLine.Remove(FCurrentRun)
  else
  begin
    FCurrentLine.Width := FCurrentLine.Width + FCurrentRun.Width;
    FCurrentLine.Height := Max(FCurrentLine.Height, FCurrentRun.Height);
  end;
end;

constructor TLineReader.Create(const ADefaultFont: TFont);
begin
  FDefaultFont := ADefaultFont;
  FCharacters := TList<UCS4String>.Create;
  FLineBreaks := TList<Integer>.Create;
  FLineBreaksLength := TList<Integer>.Create;
  FCharactersOffsets := TList<Integer>.Create;
  FPartContext := TPartContext.Create;
  FAttributes := TList<Pointer>.Create(TDelegatedComparer<Pointer>.Create(
    function(const Left, Right: Pointer): Integer
    var
      LeftRange: TTextRange;
      RightRange: TTextRange;
    begin
      LeftRange := TTextAttributedRange(Left).Range;
      RightRange := TTextAttributedRange(Right).Range;

      Result := CompareValue(LeftRange.Pos, RightRange.Pos);
      if Result = 0 then
        Result := CompareValue(LeftRange.Length, RightRange.Length);
    end));
  FLineBreakChars := UnicodeStringToUCS4String(sLineBreak);
  FScale := 1;
  FScaleFactor := 1;
  FDefaultColor := TAlphaColorRec.Black;
  FWordWrap := False;
  FTrimming := TTextTrimming.None;
end;

destructor TLineReader.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FPartContext);
  FreeAndNil(FCharactersOffsets);
  FreeAndNil(FLineBreaksLength);
  FreeAndNil(FLineBreaks);
  FreeAndNil(FCharacters);
  inherited;
end;

procedure TLineReader.EndPart;

  function CalculateTrimmedLength: Integer;
  var
    I: Integer;
    GlyphSize: TSizeF;
    RemainedLineWidth: Single;
  begin
    Result := FPartContext.Sizes.Count - 1;
    RemainedLineWidth := FRemainLineWidth;
    for I := 0 to FPartContext.Sizes.Count - 1 do
    begin
      GlyphSize := FPartContext.Sizes[I];

      if GlyphSize.Width > RemainedLineWidth - FEllipsisGlyphSize.Width then
        Exit(I);
      RemainedLineWidth := RemainedLineWidth - GlyphSize.Width;
    end;
  end;

  procedure FixPart(const ALength: Integer);
  var
    I: Integer;
    GlyphSize: TSizeF;
    CharacterIndex: Integer;
  begin
    for I := 0 to ALength - 1 do
    begin
      GlyphSize := FPartContext.Sizes[I];
      CharacterIndex := FPartContext.BeginIndex + I;

      if FPartContext.RunsActions[I] = TGPURunAction.BeginNewRun then
      begin
        CloseCurrentRun;
        BeginNewRun(FCharactersOffsets[CharacterIndex], FPartContext.AttributesIndex[I]);
      end;

      if not TGPUTextLayout.MoveSpacesOnTheNewLine then
        // We emit spaces in the beginning of the current line. The first line is a exception.
        if IsLineEmpty and not IsFirstLine and IsSpace(I + FPartContext.BeginIndex) and FWordWrap then
          if (FTrimming = TTextTrimming.None) or (FTrimming <> TTextTrimming.None) and not IsFrameEmpty then
          begin
            // If new we skip spaces in the beginning of new line, we have to adjust StartIndex of current GPURun.
            if FCurrentRun.Length = 0 then
              FCurrentRun.FStartIndex := FCharacterIndex;
            Continue;
          end;

      FCurrentRun.AddChar(FCharacters[CharacterIndex], GlyphSize, GetCharacterLength(CharacterIndex));
      FRemainLineWidth := FRemainLineWidth - GlyphSize.Width;
      FCurrentLineWidth := FCurrentLineWidth + GlyphSize.Width;
    end;
  end;

  function CanRollbackGlyph: Boolean;
  begin
    Result := (FCurrentLine.Count > 1) or (FCurrentRun.Chars.Count > 1);
  end;

  function DefineTrimmedPartLength: Integer;
  begin
    case FTrimming of
      TTextTrimming.Character:
      begin
        // Part can have characters more, than can be placed in current line
        Result := CalculateTrimmedLength;

        // Handle the situation that the part is truncated so that only spaces remains
        if Result = FPartContext.LeftSpacesCount then
          Result := 0;

        if IsLineEmpty then
          // If the string is empty and the first character does not fit, then leave the first character and add "..."
          Result := Max(Result, 1);
      end;

      TTextTrimming.Word:
        Result := IfThen(IsLineEmpty, 1, 0);
    else
      Result := 0;
    end;
  end;

  procedure RollbackGlyphs;
  begin
    case FTrimming of
      TTextTrimming.Character:
        // Line doesn't have a place for ellipsis glyph, so we need to rollback few glyphs from the end
        while (FRemainLineWidth < FEllipsisGlyphSize.Width) and CanRollbackGlyph do
        begin
          FRemainLineWidth := FRemainLineWidth + FCurrentRun.FGlyphs.Last.Width;
          FCurrentLineWidth := FCurrentLineWidth + FCurrentRun.FGlyphs.Last.Width;
          RollbackGlyph;
        end;

      TTextTrimming.Word:
        RollbackWord;
    end;
  end;

var
  NeedTrim: Boolean;
  TrimmedPartLength: Integer;
begin
  NeedTrim := not FWordWrap and (FTrimming <> TTextTrimming.None);

  // If there is a trimming, then we fix only a part of the typed word
  if NeedTrim and (FPartContext.Width > FRemainLineWidth) then
  begin
    TrimmedPartLength := DefineTrimmedPartLength;
    FixPart(TrimmedPartLength);

    // If line doesn't have space for ellipsis glyph, we have to remove some glyphs from the end of line for
    // free space for it
    if FRemainLineWidth < FEllipsisGlyphSize.Width then
      RollbackGlyphs;

    // Insert Ellipsis glyph with using default font.
    if FCurrentAttributeIndex <> -1 then
    begin
      CloseCurrentRun;
      BeginNewRun(FCharactersOffsets[FCharacterIndex]);
    end;
    FCurrentRun.AddChar(EllipsisChar, FEllipsisGlyphSize, 1);

    // If there are available future user's line breaks, we should move cursor to next line break position.
    if InRange(FPreviousLineBreakIndex, -1, FLineBreaks.Count - 2) then
      SkipChars(FLineBreaks[FPreviousLineBreakIndex + 1] - FCharacterIndex)
    else
      FCharacterIndex := FCharacters.Count;
  end
  else
    FixPart(FPartContext.Sizes.Count);

  BeginPart;
end;

function TLineReader.GetAttribute(const Index: Integer): TTextAttributedRange;
begin
  Result := TTextAttributedRange(FAttributes[Index]);
end;

function TLineReader.GetBaseline(const AFont: TFont): Single;
begin
  Result := TGlyphsManager.Default.GetCharDictionary(AFont, FScale).Baseline;
end;

function TLineReader.GetGlyphSize(const ACharacter: UCS4String): TSizeF;
var
  GlyphInfo: PCharRec;
begin
  GlyphInfo := TGlyphsManager.Default.GetCharGlyph(ACharacter, FDefaultFont, FScale);

  Result := TSizeF.Create(GlyphInfo.Glyph.Advance * FScaleFactor,
                          GlyphInfo.Glyph.VerticalAdvance * FScaleFactor);
end;

function TLineReader.GetCharacterLength(const ACharacterIndex: Integer): Integer;
begin
  if ACharacterIndex + 1 < FCharacters.Count then
    Result := FCharactersOffsets[ACharacterIndex + 1] - FCharactersOffsets[ACharacterIndex]
  else
    Result := UCS4StringToUnicodeString(FCharacters[ACharacterIndex]).Length;
end;

function TLineReader.HasAvailableSpaceInCurrentLine: Boolean;
var
  LLineWidth: Single;
begin
  if IsLineEmpty then
    LLineWidth := FCurrentLineWidth + FPartContext.TrimmedWidth + FGlyphSize.Width
  else
    LLineWidth := FCurrentLineWidth + FPartContext.Width + FGlyphSize.Width;

  Result := LLineWidth <= MaxSize.Width;
end;

function TLineReader.HasAvailableSpaceInNextLine: Boolean;
begin
  Result := FPartContext.TrimmedWidth + FGlyphSize.Width <= FMaxSize.Width;
end;

function TLineReader.GetGlyphSize: TSizeF;
var
  GlyphInfo: PCharRec;
  Font: TFont;
begin
  if FSelectedAttributeIndex = -1 then
    Font := FDefaultFont
  else
    Font := Attributes[FSelectedAttributeIndex].Attribute.Font;

  GlyphInfo := TGlyphsManager.Default.GetCharGlyph(FCharacters[FCharacterIndex], Font, FScale);
  Result := TSizeF.Create(GlyphInfo.Glyph.Advance * FScaleFactor,
                          GlyphInfo.Glyph.VerticalAdvance * FScaleFactor);
end;

procedure TLineReader.Init;
begin
  FEllipsisGlyphSize := GetGlyphSize(EllipsisChar);
  FDefaultLineSize := TSizeF.Create(1, GetGlyphSize(UnicodeStringToUCS4String('|')).Height);
  FRemainLineWidth := FMaxSize.Width;
  FCurrentLineWidth := 0;
  FAttributes.Sort;
  FSelectedAttributeIndex := -1;
  FCurrentAttributeIndex := -1;
  FAttributeAction := TGPURunAction.Continue;
  FCharacterIndex := -1;
  FPreviousLineBreakIndex := -1;
  BeginNewLine;
end;

function TLineReader.IsEOL: Boolean;
begin
  Result := FCharacterIndex >= FCharacters.Count;
end;

function TLineReader.IsFirstLine: Boolean;
begin
  Result := FFrame.Count = 1;
end;

function TLineReader.IsFrameEmpty: Boolean;
begin
  Result := (FFrame.Count = 1) and SameValue(FFrame[0].Width, 0, Single.Epsilon);
end;

function TLineReader.IsSplitterAfter: Boolean;
const
  CJKSymbolsAndPunctuationBegin = $3000;
  CJKSymbolsAndPunctuationEnd = $303F;
  HalfwidthAndFullwidthFormsBegin = $FF00;
  HalfwidthAndFullwidthFormsEnd = $FFEF;
var
  Character: UCS4String;
begin
  Assert(InRange(FCharacterIndex, 0, FCharacters.Count - 1));

  Character := FCharacters[FCharacterIndex];
  Result := (Length(Character) = 2) and (
             InRange(Character[0], HalfwidthAndFullwidthFormsBegin, HalfwidthAndFullwidthFormsEnd) or
             InRange(Character[0], CJKSymbolsAndPunctuationBegin, CJKSymbolsAndPunctuationEnd) or
             (Char.GetUnicodeCategory(Character[0]) in [TUnicodeCategory.ucOtherSymbol, TUnicodeCategory.ucOtherLetter]));
end;

function TLineReader.IsLineBreak: Boolean;
begin
  Result := InRange(FPreviousLineBreakIndex, -1, FLineBreaks.Count - 2) and (FLineBreaks[FPreviousLineBreakIndex + 1] = FCharacterIndex);
end;

function TLineReader.IsLineBreak(const ASource: string; const ABeginIndex: Integer; var ALength: Integer): Boolean;

  function IsEqualedSystemLineBreak: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to Length(SLineBreak) - 1 do
      if ASource.Chars[ABeginIndex + I] <> string(SLineBreak).Chars[I] then
      begin
       Result := False;
       Break;
      end;
  end;

begin
  Assert(InRange(ABeginIndex, 0, ASource.Length - 1));

  ALength := 0;
  if ABeginIndex <= ASource.Length - Length(SLineBreak) + 1 then
    Result := IsEqualedSystemLineBreak
  else
    Result := False;

  if Result then
  begin
    ALength := string(SLineBreak).Length;
    Exit;
  end;

  // User can use #10/#13 as a single line break despite the system separator.
  if ASource.Chars[ABeginIndex].IsInArray([#10, #13]) then
  begin
    ALength := 1;
    Exit(True);
  end;
end;

function TLineReader.IsLineEmpty: Boolean;
begin
  Result := (FCurrentLine.Count = 1) and SameValue(FCurrentLine[0].Width, 0, Single.Epsilon);
end;

function TLineReader.IsRunEmpty: Boolean;
begin
  Result := FCurrentRun.Chars.Count = 0;
end;

function TLineReader.IsSpace: Boolean;
begin
  Result := IsSpace(FCharacterIndex);
end;

function TLineReader.IsSpace(const AGlyphIndex: Integer): Boolean;
begin
  Assert(InRange(AGlyphIndex, 0, FCharacters.Count - 1));

  Result := IsSpace(FCharacters[AGlyphIndex]);
end;

function TLineReader.IsSpace(const ACharacter: UCS4String): Boolean;
begin
  Result := (Length(ACharacter) = 2) and (Char.GetUnicodeCategory(ACharacter[0]) in [TUnicodeCategory.ucSpaceSeparator]);
end;

function TLineReader.IsSplitterBefore: Boolean;
const
  SplittersCategories = [TUnicodeCategory.ucSpaceSeparator];
var
  Character: UCS4String;
begin
  Assert(InRange(FCharacterIndex, 0, FCharacters.Count - 1));

  Character := FCharacters[FCharacterIndex];

  Result := (Length(Character) = 2) and (Char.GetUnicodeCategory(Character[0]) in SplittersCategories) or IsLineBreak;
end;

procedure TLineReader.NextChar;

  function IsNextAttributeAvailable(const AIndex: Integer): Boolean;
  begin
    Result := AIndex < FAttributes.Count - 1;
  end;

var
  CurrentAttribute: TTextAttributedRange;
  NeedFindAttribute: Boolean;
  PreviousSelectedAttributeIndex: Integer;
begin
  PreviousSelectedAttributeIndex := FSelectedAttributeIndex;
  Inc(FCharacterIndex);

  // We use "Sliding window" algorithm for definition attribute of current character.
  // First, check if the symbol is already in the current attribute range
  NeedFindAttribute := (FAttributes.Count > 0) and IsNextAttributeAvailable(FCurrentAttributeIndex);
  if FSelectedAttributeIndex <> -1 then
  begin
    CurrentAttribute := FAttributes[FSelectedAttributeIndex];
    if CurrentAttribute.Range.InRange(FCharacterIndex) then
    begin
      NeedFindAttribute := False;
      FAttributeAction := TGPURunAction.Continue;
    end
    else
    begin
      FSelectedAttributeIndex := -1;
      FAttributeAction := TGPURunAction.BeginNewRun;
    end;
  end;

  if NeedFindAttribute then
  begin
    // Current character is not in current attribute. Looking for the next one.
    CurrentAttribute := FAttributes[FCurrentAttributeIndex + 1];
    if CurrentAttribute.Range.InRange(FCharacterIndex) then
    begin
      // Character in the following attribute range, so fix it.
      FCurrentAttributeIndex := FCurrentAttributeIndex + 1;
      FSelectedAttributeIndex := FCurrentAttributeIndex;
      FAttributeAction := TGPURunAction.BeginNewRun;
    end
    else
    begin
      // The following attribute range does not contain character. therefore, we reset the active attribute index.
      FSelectedAttributeIndex := -1;
      if PreviousSelectedAttributeIndex = -1 then
        FAttributeAction := TGPURunAction.Continue
      else
        FAttributeAction := TGPURunAction.BeginNewRun;
    end;
  end;

  if not IsEOL then
    FGlyphSize := GetGlyphSize;
end;

procedure TLineReader.FillLines(const AFrame: TGPUFrame);
type
  TParserState = (BeginningWord, ScanningChar, EndingWord, Finish);

  function HasSpaceForCurrentGlyph: Boolean;
  begin
    Result := FCurrentLine.IsEmpty or (FGlyphSize.Width <= FRemainLineWidth);
  end;

  function NeedApplyTrimming: Boolean;
  begin
    Result := not FWordWrap and (FTrimming <> TTextTrimming.None);
  end;

  procedure CreateEmptyLine;
  begin
    FCurrentRun.ImageRect := TRectF.Create(0, 0, FDefaultLineSize.Width, FDefaultLineSize.Height);
    FCurrentLine.Width := FCurrentRun.Width;
    FCurrentLine.Height := FCurrentRun.Height;
    FFrame.Height := FCurrentLine.Height;
    FFrame.Width := FCurrentLine.Width;
  end;

var
  State: TParserState;
  LIsSplitterAfter: Boolean;
begin
  FFrame := AFrame;
  Init;

  if FCharacters.Count = 0  then
  begin
    CreateEmptyLine;
    Exit;
  end;

  // The general algorithm is based on state machine. It scans character one by one and put it into context of part.
  // When new character is being added, it performs analyze, should it add this part from context into current line,
  // or move on the next line.
  //
  // Part - sequence of characters, which should be placed on a single line.
  // Part Context - additional information about current part, which contains cached data (sizes, indicies and etc).

  State := TParserState.BeginningWord;
  NextChar;
  repeat
    case State of
      TParserState.BeginningWord:
      begin
        if IsLineBreak then
        begin
          SkipLineBreak;
          if not IsEOL then
            BeginNewLine;
        end
        else
        begin
          BeginPart;
          State := TParserState.ScanningChar;
        end;

        if IsEOL then
          State := TParserState.EndingWord;
      end;

      TParserState.ScanningChar:
      begin
        LIsSplitterAfter := IsSplitterAfter;
        if (FPartContext.Sizes.Count > 0) and (IsSplitterBefore or (FPartContext.LeftSpacesCount = FPartContext.Sizes.Count) and not NeedApplyTrimming) then
          State := TParserState.EndingWord
        else if not FWordWrap and (FTrimming = TTextTrimming.None) then
        begin
          // No word wrap and trimming so we continue
          AddCharToPart;
          NextChar;
        end
        else if HasAvailableSpaceInCurrentLine then
        begin
          // Word with new characters got into the current line
          AddCharToPart;
          NextChar;
        end
        else if HasAvailableSpaceInNextLine then
        begin
          // Word with new characters does not fit in the current line, but fits in the next
          if NeedApplyTrimming then
          begin
            AddCharToPart;
            NextChar;
            FCurrentLineWidth := FPartContext.Width;
            State := TParserState.EndingWord;
          end
          else
            BeginNewLine(FPartContext.BeginIndex);
        end
        else if FPartContext.BeginIndex = FCharacterIndex then
        begin
          // Word from one new character does not fit into either the current line or the next, so we need to fix it.
          if not NeedApplyTrimming and (FCharacterIndex <> 0) then
            BeginNewLine(FPartContext.BeginIndex);

          AddCharToPart;
          NextChar;
          FCurrentLineWidth := FPartContext.Width;
        end
        else
        begin
          if not IsLineEmpty and (FCurrentLineWidth > 0) and FWordWrap then
            BeginNewLine(FPartContext.BeginIndex);

          if NeedApplyTrimming then
            AddCharToPart;

          if IsEOL then
          begin
            EndPart;
            State := TParserState.Finish;
          end
          else
            State := TParserState.EndingWord;
        end;

        if IsEOL or LIsSplitterAfter then
          State := TParserState.EndingWord;
      end;

      TParserState.EndingWord:
      begin
        EndPart;

        if IsEOL then
        begin
          CloseCurrentLine;
          State := TParserState.Finish;
        end
        else
          State := TParserState.BeginningWord;
      end;
    end;
  until State = TParserState.Finish;

  FCurrentLine := nil;
  FCurrentRun := nil;
  FFrame := nil;
end;

procedure TLineReader.RollbackGlyph;
begin
  if FCurrentRun.Chars.Count > 0 then
  begin
    FCurrentRun.DeleteLastChar;
    if IsRunEmpty then
    begin
      FCurrentLine.Remove(FCurrentRun);
      FCurrentLine.AdjustHeight;
      FCurrentRun := FCurrentLine.Last;
    end
    else
      FCurrentRun.NormalizeHeight;
  end;
end;

procedure TLineReader.RollbackWord;

  function CanRollbackGlyph: Boolean;
  begin
    Result := (FCurrentLine.Count > 1) or (FCurrentRun.Chars.Count > 1);
  end;

var
  Stop: Boolean;
begin
  Stop := False;
  while CanRollbackGlyph and not Stop do
  begin
    FRemainLineWidth := FRemainLineWidth + FCurrentRun.FGlyphs.Last.Width;
    FCurrentLineWidth := FCurrentLineWidth + FCurrentRun.FGlyphs.Last.Width;
    RollbackGlyph;
    if IsSpace(FCurrentRun.FChars.Last) and CanRollbackGlyph then
    begin
      FRemainLineWidth := FRemainLineWidth + FCurrentRun.FGlyphs.Last.Width;
      FCurrentLineWidth := FCurrentLineWidth + FCurrentRun.FGlyphs.Last.Width;
      RollbackGlyph;

      Stop := True;
    end;
  end;
end;

procedure TLineReader.SetScale(const Value: Single);
begin
  if not SameValue(FScale, Value, Single.Epsilon) then
  begin
    FScale := Value;
    FScaleFactor := 1 / FScale;
  end;
end;

procedure TLineReader.SetText(const Value: string);
var
  CharOffset, CharLength: Integer;
  CharIndex: Integer;
  BreakLength: Integer;
begin
  FCharacters.Clear;
  FCharactersOffsets.Clear;
  FLineBreaks.Clear;
  FLineBreaksLength.Clear;
  CharOffset := 0;
  CharIndex := 0;
  BreakLength := 0;
  while CharOffset < Value.Length do
  begin
    FCharacters.Add(ReadCharacter(Value, CharOffset, CharLength));
    FCharactersOffsets.Add(CharOffset);
    if (BreakLength = 0) and IsLineBreak(Value, CharOffset, BreakLength) then
    begin
      FLineBreaks.Add(CharIndex);
      FLineBreaksLength.Add(BreakLength);
    end;

    if BreakLength > 0 then
      Dec(BreakLength);

    Inc(CharOffset, CharLength);
    Inc(CharIndex);
  end;
end;

procedure TLineReader.SkipChars(const ACount: Integer);
var
  I: Integer;
begin
  for I := FCharacterIndex to Min(FCharacterIndex + ACount - 1, FCharacters.Count) do
    NextChar;
end;

procedure TLineReader.SkipLineBreak;
begin
  Inc(FPreviousLineBreakIndex);
  // User can use #10/#13 as a single line break despite the system separator. So the line of linebreak depends on it.
  SkipChars(FLineBreaksLength[FPreviousLineBreakIndex]);
end;

{ TPartContext }

procedure TPartContext.AddSize(const ASize: TSizeF; const AAttributeIndex: Integer; const AAction: TGPURunAction);
begin
  Sizes.Add(ASize);
  AttributesIndex.Add(AAttributeIndex);
  RunsActions.Add(AAction);
  Width := Width + ASize.Width;
  if not IsLastSpace then
    TrimmedWidth := TrimmedWidth + ASize.Width;
end;

constructor TPartContext.Create;
begin
  inherited;
  BeginIndex := -1;
  Sizes := TList<TSizeF>.Create;
  AttributesIndex := TList<Integer>.Create;
  RunsActions := TList<TGPURunAction>.Create;
end;

destructor TPartContext.Destroy;
begin
  FreeAndNil(RunsActions);
  FreeAndNil(AttributesIndex);
  FreeAndNil(Sizes);
  inherited;
end;

{ TFrameRender }

procedure TFrameRender.RecalculateCanvasClipRect;

  procedure RectToPoligon(const ARect: TRectF; var APolygon: TPolygon); inline;
  begin
    Assert(Length(APolygon) = 4);

    APolygon[0] := ARect.TopLeft;
    APolygon[1] := TPointF.Create(ARect.Right, ARect.Top);
    APolygon[2] := ARect.BottomRight;
    APolygon[3] := TPointF.Create(ARect.Left, ARect.Bottom);
  end;

  procedure MulPoligon(var APolygon: TPolygon; const AMatrix: TMatrix); inline;
  var
    I: Integer;
  begin
    for I := Low(APolygon) to High(APolygon) do
      APolygon[I] := APolygon[I] * AMatrix;
  end;

var
  CanvasRect: TRectF;
  Polygon: TPolygon;
  TextRect: TRectF;
begin
  Assert(FCanvas <> nil);

  TextRect := TRectF.Create(FFrame.TopLeft, FFrame.Width, FFrame.Height);
  TextRect.Offset(FOutputBounds.TopLeft);

  SetLength(Polygon, 4);

  { Convert TextRect to Absolute coordinate system }
  RectToPoligon(TextRect, Polygon);
  MulPoligon(Polygon, FCanvas.Matrix);
  FCanvasClipRect := ConvexHull(Polygon);

  { Intersects with canvas bounds }
  CanvasRect := TRectF.Create(0, 0, FCanvas.Width, FCanvas.Height);
  FCanvasClipRect := TRectF.Intersect(CanvasRect, FCanvasClipRect);

  { Convert clipped result rect to Local coordinate system }
  RectToPoligon(FCanvasClipRect, Polygon);
  MulPoligon(Polygon, FCanvas.Matrix.Inverse);
  FCanvasClipRect := ConvexHull(Polygon);
end;

function TFrameRender.CalculateGlyphDestRect(const APosition: TPointF; const ABaselineOffset: Single;
  const AGlyphRec: PCharRec): TRectF;
begin
  Assert(FCanvas <> nil);

  if TPixelAlignment.Horizontal in FPixelAlignments then
    Result.Left := FCanvas.AlignToPixelHorizontally(APosition.X)
  else
    Result.Left := APosition.X;
  Result.Left := Result.Left + AGlyphRec.Glyph.Origin.X * FScaleFactor;

  if TPixelAlignment.Vertical in FPixelAlignments then
    Result.Top := FCanvas.AlignToPixelVertically(APosition.Y + ABaselineOffset)
  else
    Result.Top := APosition.Y + ABaselineOffset;
  Result.Top := Result.Top + AGlyphRec.Glyph.Origin.Y * FScaleFactor;

  Result.Right := Result.Left + AGlyphRec.SrcRect.Width * FScaleFactor;
  Result.Bottom := Result.Top + AGlyphRec.SrcRect.Height * FScaleFactor;
end;

class function TFrameRender.ConvexHull(const APolygon: TPolygon): TRectF;
var
  MinX: Single;
  MinY: Single;
  MaxX: Single;
  MaxY: Single;
begin
  Assert(Length(APolygon) = 4);

  MinX := MinValue([APolygon[0].X, APolygon[1].X, APolygon[2].X, APolygon[3].X]);
  MinY := MinValue([APolygon[0].Y, APolygon[1].Y, APolygon[2].Y, APolygon[3].Y]);
  MaxX := MaxValue([APolygon[0].X, APolygon[1].X, APolygon[2].X, APolygon[3].X]);
  MaxY := MaxValue([APolygon[0].Y, APolygon[1].Y, APolygon[2].Y, APolygon[3].Y]);
  Result := TRectF.Create(MinX, MinY, MaxX, MaxY);
end;

constructor TFrameRender.Create(const ADefaultFont: TFont);
var
  ScreenService: IFMXScreenService;
begin
  FDefaultFont := ADefaultFont;
  FStrokeBrush := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    Scale := ScreenService.GetScreenScale
  else
    Scale := 1;
  FPixelAlignments := [];
  FOutputBounds := TRectF.Empty;
end;

destructor TFrameRender.Destroy;
begin
  FreeAndNil(FStrokeBrush);
  inherited;
end;

procedure TFrameRender.DifinePixelAlignment;
begin
  Assert(FCanvas <> nil);

  FPixelAlignments := [];
  if SameValue(Frac(FOutputBounds.Left), 0.0, TEpsilon.Position) and SameValue(Frac(FCanvas.Matrix.m31), 0.0, TEpsilon.Position) then
    Include(FPixelAlignments, TPixelAlignment.Horizontal);

  if SameValue(Frac(FOutputBounds.Top), 0.0, TEpsilon.Position) and SameValue(Frac(FCanvas.Matrix.m32), 0.0, TEpsilon.Position) then
    Include(FPixelAlignments, TPixelAlignment.Vertical);
end;

procedure TFrameRender.DrawGlyphBounds(const AGlyph: TFontGlyph; const ABounds: TRectF);
begin
  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Color := TAlphaColorRec.Blue;
  FCanvas.DrawRect(ABounds, 0, 0, AllCorners, 1);
end;

procedure TFrameRender.DrawTextBounds(const ABounds: TRectF);
begin
  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Color := TAlphaColorRec.Red;
  FCanvas.DrawRect(ABounds, 0, 0, AllCorners, 1);
end;

procedure TFrameRender.DrawTextDecorationIfNeeded(const ARun: TGPURun; const AFirstGlyphPos: TPointF;
  const ABaselineOffset: Single);
var
  Thickness: Single;
  Styles: TFontStyles;
  CharDic: TCharDic;
begin
  Styles := ARun.Font.Style;
  if not (TFontStyle.fsStrikeOut in Styles) and not (TFontStyle.fsUnderline in Styles) then
    Exit;

  FStrokeBrush.Color := ARun.Color;
  if ARun.Font <> nil then
    Thickness := ARun.Font.Size / 15
  else
    Thickness := FDefaultFont.Size / 15;
  FStrokeBrush.Thickness := Thickness;
  if TFontStyle.fsStrikeOut in Styles then
    FCanvas.DrawLine(TPointF.Create(AFirstGlyphPos.X, AFirstGlyphPos.Y + ABaselineOffset + ARun.ImageRect.Height / 2),
                     TPointF.Create(AFirstGlyphPos.X + ARun.ImageRect.Width, AFirstGlyphPos.Y + ABaselineOffset + ARun.ImageRect.Height / 2),
                     FOpacity, FStrokeBrush);
  if TFontStyle.fsUnderline in Styles then
  begin
    CharDic := TGlyphsManager.Default.GetCharDictionary(ARun.Font, FScale);
    FCanvas.DrawLine(TPointF.Create(AFirstGlyphPos.X, AFirstGlyphPos.Y + ABaselineOffset + CharDic.Baseline * FScaleFactor + 1.5 * Thickness),
                     TPointF.Create(AFirstGlyphPos.X + ARun.ImageRect.Width, AFirstGlyphPos.Y + ABaselineOffset + CharDic.Baseline * FScaleFactor + 1.5 * Thickness),
                     FOpacity, FStrokeBrush);
  end;
end;

function TFrameRender.GetMaxSize: TSizeF;
begin
  Result := FOutputBounds.Size;
end;

function TFrameRender.GetTopLeft: TPointF;
begin
  Result := FOutputBounds.TopLeft;
end;

function TFrameRender.IsOutOfCanvasArea(const ARect: TRectF): Boolean;
begin
  Result := not FCanvasClipRect.IntersectsWith(ARect);
end;

function TFrameRender.IsOutOfOutputArea(const ARect: TRectF): Boolean;
begin
  Result := not FOutputBounds.IntersectsWith(ARect);
end;

procedure TFrameRender.Render(const AFrame: TGPUFrame; const ACanvas: TCanvas; const AOpacity: Single);
var
  GlyphSize: TSizeF;
  DefaultBaseline: Single;
  LineRect: TRectF;
  CharDic: TCharDic;
  Rec: PCharRec;
  GlyphPos: TPointF;
  DestR, SrcR, ClipBounds: TRectF;
  Line: TGPULine;
  LRun: TGPURun;
  I, J, K: Integer;
  ColoredGlyph: Boolean;
  BaselineMaxValue, BaselineOffset: Single;
  ClipPadding: TRectF;
  FirstGlyphRunPos: TPointF;
  GlyphRect: TRectF;
begin
  if AFrame.Count = 0 then
    Exit;

  FFrame := AFrame;
  FCanvas := ACanvas;
  FOpacity := AOpacity;
  try
    // It's a part of optimization. We calculate convex hull of the text taking into account the rotations, scales and
    // canvas boundaries. Later we skip rendering of all lines and glyphs, which lay out of this area.
    RecalculateCanvasClipRect;
    DifinePixelAlignment;
    DefaultBaseline := TGlyphsManager.Default.GetCharDictionary(FDefaultFont, FScale).Baseline;

    for I := 0 to FFrame.Count - 1 do
    begin
      Line := FFrame[I];
      GlyphPos := Line.TopLeft + FOutputBounds.TopLeft;

      LineRect := TRectF.Create(GlyphPos, Line.Width, Line.Height);
      if IsOutOfCanvasArea(LineRect) or IsOutOfOutputArea(LineRect) then
        Continue;

      BaselineMaxValue := Max(Line.CalculateBaseline, DefaultBaseline);
      for J := 0 to Line.Count - 1 do
      begin
        LRun := Line[J];
        CharDic := TGlyphsManager.Default.GetCharDictionary(LRun.Font, FScale);
        BaselineOffset := (BaselineMaxValue - CharDic.Baseline) * FScaleFactor;
        TCustomCanvasGpu(ACanvas).ModulateColor := LRun.Color;
        FirstGlyphRunPos := GlyphPos;
        for K := 0 to LRun.Chars.Count - 1 do
        begin
          GlyphSize := LRun.Glyphs[K];
          GlyphRect := TRectF.Create(GlyphPos, GlyphSize.Width, GlyphSize.Height);

          if IsOutOfCanvasArea(GlyphRect) or IsOutOfOutputArea(GlyphRect) then
          begin
            GlyphPos.X := GlyphPos.X + GlyphSize.Width;
            Continue;
          end;

          Rec := TGlyphsManager.Default.GetCharGlyph(CharDic, LRun.Chars[K], LRun.Font, FScale);
          if Rec.Bitmap <> nil then
          begin
            DestR := CalculateGlyphDestRect(GlyphPos, BaselineOffset, Rec);

            SrcR := Rec.SrcRect;
            if LRun.IsClipped then
            begin
              ClipBounds := FOutputBounds;
              ClipPadding.Top := Max(DestR.Top, ClipBounds.Top) - DestR.Top;
              ClipPadding.Left := Max(DestR.Left, ClipBounds.Left) - DestR.Left;
              ClipPadding.Right := DestR.Right - Min(DestR.Right, ClipBounds.Right);
              ClipPadding.Bottom := DestR.Bottom - Min(DestR.Bottom, ClipBounds.Bottom);

              DestR.Top := DestR.Top + ClipPadding.Top;
              DestR.Bottom := DestR.Bottom - ClipPadding.Bottom;
              DestR.Left := DestR.Left + ClipPadding.Left;
              DestR.Right := DestR.Right - ClipPadding.Right;

              SrcR.Top := SrcR.Top + ClipPadding.Top * FScale;
              SrcR.Bottom := SrcR.Bottom - ClipPadding.Bottom * FScale;
              SrcR.Left := SrcR.Left + ClipPadding.Left * FScale;
              SrcR.Right := SrcR.Right - ClipPadding.Right * FScale;
            end;

            if not SrcR.IsEmpty and not DestR.IsEmpty then
            begin
              // Draw
              ColoredGlyph := TFontGlyphStyle.ColorGlyph in Rec.Glyph.Style;
              if ColoredGlyph then
                TCustomCanvasGpu(ACanvas).ModulateColor := TAlphaColorRec.White;
              ACanvas.DrawBitmap(Rec.Bitmap, SrcR, DestR, FOpacity);

              if TGPUTextLayout.DebugDrawGlyphBounds then
                DrawGlyphBounds(Rec.Glyph, DestR);

              if ColoredGlyph then
                TCustomCanvasGpu(ACanvas).ModulateColor := LRun.Color;
            end;
          end;
          GlyphPos.X := GlyphPos.X + GlyphSize.Width;
        end;

        DrawTextDecorationIfNeeded(LRun, FirstGlyphRunPos, BaselineOffset);
      end;
    end;
    TCustomCanvasGpu(ACanvas).ModulateColor := TAlphaColorRec.White;

    if TGPUTextLayout.DebugDrawTextBounds then
      DrawTextBounds(TRectF.Create(FOutputBounds.TopLeft + FFrame.TopLeft, FFrame.Width, FFrame.Height));
  finally
    FFrame := nil;
    FCanvas := nil;
  end;
end;

procedure TFrameRender.SetMaxSize(const Value: TSizeF);
begin
  FOutputBounds.Size := Value;
end;

procedure TFrameRender.SetScale(const Value: Single);
begin
  FScale := Value;
  FScaleFactor := 1 / FScale;
end;

procedure TFrameRender.SetTopLeft(const Value: TPointF);
begin
  FOutputBounds.Location := Value;
end;

{ TGlyphsManager }

function TGlyphsManager.AddCharacterGlyph(const ACharacter: UCS4String; const AFont: TFont; const AScale: Single;
  const APathPresentation: Boolean): PCharRec;
var
  GlyphSettings: TFontGlyphSettings;
begin
  GlyphSettings := [TFontGlyphSetting.Bitmap, TFontGlyphSetting.PremultipliedAlpha];
  if APathPresentation then
    Include(GlyphSettings, TFontGlyphSetting.Path);

  New(Result);
  Result.Create(TFontGlyphManager.Current.GetGlyph(ACharacter, AFont, AScale, GlyphSettings));

  if Result.HasNoEmptyBitmap then
  begin
    var SourceBitmapSurface := Result.Glyph.Bitmap;
    var Bitmap := TBitmap.Create(SourceBitmapSurface.Width + AntialiasMargin * 2,
                                 SourceBitmapSurface.Height + AntialiasMargin * 2);
    Bitmap.BitmapScale := AScale;

    Result.Bitmap := Bitmap;
    Result.BitmapRef := False;
    Result.SrcRect := TRectF.Create(0, 0, SourceBitmapSurface.Width, SourceBitmapSurface.Height);
    Result.SrcRect.Offset(AntialiasMargin, AntialiasMargin);

    Copy(SourceBitmapSurface, Bitmap, TPoint.Create(AntialiasMargin, AntialiasMargin));

    if FRendering = 0 then
      PutGlyphToCharMaps(Result)
    else
      FNewGlyphList.Add(Result);
  end;
end;

procedure TGlyphsManager.BeginRender;
begin
  Inc(FRendering);
end;

class procedure TGlyphsManager.Copy(const ASource: TBitmapSurface; const ADest: TBitmap; const AOffset: TPoint;
  const ANeedClearDest: Boolean);
var
  Map: TBitmapData;
begin
  if ADest.Map(TMapAccess.Write, Map) then
  try
    if ANeedClearDest then
      FillChar(Map.Data^, Map.Pitch * Map.Height, 0);
    if ASource.PixelFormat = ADest.PixelFormat then
      for var I := 0 to ASource.Height - 1 do
        Move(ASource.Scanline[I]^, Map.GetPixelAddr(AOffset.X, I + AOffset.Y)^, ASource.Pitch)
    else
      for var I := 0 to ASource.Height - 1 do
        ChangePixelFormat(ASource.Scanline[I], Map.GetPixelAddr(AOffset.X, I + AOffset.Y), ASource.Width,
                          ASource.PixelFormat, ADest.PixelFormat);
  finally
    ADest.Unmap(Map);
  end;
end;

constructor TGlyphsManager.Create;
begin
  FFamilyDic := TFamilyDic.Create([doOwnsValues]);
  FCharMaps := TCharMaps.Create;
  FNewGlyphList := TList<PCharRec>.Create;
end;

destructor TGlyphsManager.Destroy;
begin
  FreeAndNil(FNewGlyphList);
  FreeAndNil(FCharMaps);
  FreeAndNil(FFamilyDic);
  inherited;
end;

procedure TGlyphsManager.EndRender;
var
  I: Integer;
  Rec: PCharRec;
begin
  if FRendering = 0 then
    raise EInvalidOperation.Create('Error Message');

  Dec(FRendering);
  if (FRendering = 0) and (FNewGlyphList <> nil) and (FNewGlyphList.Count > 0) and not FDisableGlyphPopulation then
  begin
    for I := 0 to FNewGlyphList.Count - 1 do
    begin
      Rec := FNewGlyphList[I];
      if (Rec.Glyph.Bitmap.Width > 0) and (Rec.Glyph.Bitmap.Height > 0) then
        PutGlyphToCharMaps(Rec);
    end;
    FNewGlyphList.Clear;
  end;
end;

class function TGlyphsManager.FontToId(const AFont: TFont; const AScale: Single): Int64;
var
  StyleCode: Integer;
begin
  StyleCode := 0;
  if TFontStyle.fsBold in AFont.Style then
    StyleCode := StyleCode + 1;
  if TFontStyle.fsItalic in AFont.Style then
    StyleCode := StyleCode + 2;

  Result := THashBobJenkins.GetHashValue(AFont.Family + StyleCode.ToString + Trunc(AFont.Size).ToString + Trunc(AScale * 100).ToString);
end;

function TGlyphsManager.GetCharGlyph(const ACharacter: UCS4String; const AFont: TFont; const AScale: Single;
  const APathPresentation: Boolean): PCharRec;
var
  FontDictionary: TCharDic;
begin
  FontDictionary := GetCharDictionary(AFont, AScale);
  Result := GetCharGlyph(FontDictionary, ACharacter, AFont, AScale, APathPresentation);
end;

function TGlyphsManager.GetCharGlyph(const CharDic: TCharDic; const ACharacter: UCS4String; const AFont: TFont;
  const AScale: Single; const APathPresentation: Boolean): PCharRec;
var
  GlyphRec: PCharRec;
  NeedUpdate: Boolean;
begin
  if CharDic.TryGetValue(ACharacter, GlyphRec) then
  begin
    // Проверяем актуально ли представление
    NeedUpdate := APathPresentation and not (TFontGlyphStyle.HasPath in GlyphRec.Glyph.Style);

    if NeedUpdate then
      UpdateCharacterGlyph(GlyphRec, ACharacter, AFont, AScale, APathPresentation);
    Result := GlyphRec;
  end
  else
  begin
    // Добавляем новый глиф
    Result := AddCharacterGlyph(ACharacter, AFont, AScale, APathPresentation);
    CharDic.Add(ACharacter, Result);
  end;
end;

function TGlyphsManager.GetCharDictionary(const AFont: TFont; const AScale: Single): TCharDic;
var
  FontId: Int64;
begin
  FontId := FontToId(AFont, AScale);
  if not FFamilyDic.TryGetValue(FontId, Result) then
  begin
    Result := TCharDic.Create(1024);
    Result.Baseline := TFontGlyphManager.Current.GetBaseline(AFont, AScale);
    FFamilyDic.Add(FontId, Result);
  end;
end;

class function TGlyphsManager.GetDefault: TGlyphsManager;
begin
  if FDefault = nil then
   FDefault := TGlyphsManager.Create;
  Result := FDefault;
end;

procedure TGlyphsManager.PutGlyphToCharMaps(const ACharRec: PCharRec);
var
  GlyphSize: TPoint;
  I, LIndex: Integer;
  LRect: TRect;
  CharMap: TCharMap;
  Scale: Single;
begin
  // CharRec.Bitmap already has AntialiasMargin applied to it.
  if ACharRec.Bitmap <> nil then
    GlyphSize := TPoint.Create(ACharRec.Bitmap.Width, ACharRec.Bitmap.Height)
  else
    GlyphSize := TPoint.Create(ACharRec.Glyph.Bitmap.Width + AntialiasMargin * 2, ACharRec.Glyph.Bitmap.Height + AntialiasMargin * 2);
  LRect := TRect.Empty;
  LIndex := -1;
  for I := 0 to FCharMaps.Count - 1 do
    if FCharMaps[I].BinPack.Occupancy < MaxUsefulCharMapOccupancy then
    begin
      LRect := FCharMaps[I].BinPack.Insert(GlyphSize, False);
      if not LRect.IsEmpty then
      begin
        LIndex := I;
        Break;
      end;
    end;

  if LIndex = -1 then
  begin
    if ACharRec.Bitmap <> nil then
      Scale := ACharRec.Bitmap.BitmapScale
    else
      Scale := 1;

    CharMap := TCharMap.Create(Scale);
    FCharMaps.Add(CharMap);
    LRect := CharMap.BinPack.Insert(GlyphSize, False);
    if LRect.IsEmpty then
      Exit;
  end
  else
    CharMap := FCharMaps[LIndex];

  if not ACharRec.BitmapRef then
    FreeAndNil(ACharRec.Bitmap);

  ACharRec.Bitmap := CharMap.Texture;
  ACharRec.BitmapRef := True;
  ACharRec.SrcRect := TRectF.Create(LRect.Left + AntialiasMargin, LRect.Top + AntialiasMargin,
                                    LRect.Right - AntialiasMargin, LRect.Bottom - AntialiasMargin);

  Copy(ACharRec.Glyph.Bitmap, CharMap.Texture, TPoint.Create(LRect.Left + AntialiasMargin, LRect.Top + AntialiasMargin),
       False);
end;

class procedure TGlyphsManager.Uninitialize;
begin
  FreeAndNil(FDefault);
end;

procedure TGlyphsManager.UpdateCharacterGlyph(var AGlyphRec: PCharRec; const ACharacter: UCS4String; const AFont: TFont;
  const AScale: Single; const APathPresentation: Boolean);
var
  Bitmap: TBitmap;
  GlyphSettings: TFontGlyphSettings;
begin
  FreeAndNil(AGlyphRec.Glyph);

  if not AGlyphRec.BitmapRef then
    FreeAndNil(AGlyphRec.Bitmap);

  GlyphSettings := [TFontGlyphSetting.Bitmap, TFontGlyphSetting.PremultipliedAlpha];
  if APathPresentation then
    Include(GlyphSettings, TFontGlyphSetting.Path);

  // это должно быть в добавлении
  AGlyphRec.Glyph := TFontGlyphManager.Current.GetGlyph(ACharacter, AFont, AScale, GlyphSettings);

  if not (TFontGlyphStyle.NoGlyph in AGlyphRec.Glyph.Style) and (AGlyphRec.Glyph.Bitmap <> nil) and
    (AGlyphRec.Glyph.Bitmap.Width > 0) and (AGlyphRec.Glyph.Bitmap.Height > 0) then
  begin
    if FRendering = 0 then
      PutGlyphToCharMaps(AGlyphRec)
    else
    begin
      Bitmap := TBitmap.Create(AGlyphRec.Glyph.Bitmap.Width + AntialiasMargin * 2, AGlyphRec.Glyph.Bitmap.Height +
        AntialiasMargin * 2);
      Bitmap.BitmapScale := AScale;
      AGlyphRec.Bitmap := Bitmap;
      AGlyphRec.BitmapRef := False;
      AGlyphRec.SrcRect := TRectF.Create(0, 0, AGlyphRec.Glyph.Bitmap.Width, AGlyphRec.Glyph.Bitmap.Height);
      AGlyphRec.SrcRect.Offset(AntialiasMargin, AntialiasMargin);

      Copy(AGlyphRec.Glyph.Bitmap, Bitmap, TPoint.Create(AntialiasMargin, AntialiasMargin));
      FNewGlyphList.Add(AGlyphRec);
    end;
  end
  else
  begin
    AGlyphRec.Bitmap := nil;
    AGlyphRec.SrcRect := TRectF.Empty;
  end;
end;

{ TCharRec }

constructor TCharRec.Create(const AGlyph: TFontGlyph);
begin
  Glyph := AGlyph;
  SrcRect := TRectF.Empty;
  BitmapRef := True;
end;

function TCharRec.HasNoEmptyBitmap: Boolean;
begin
  Result := not (TFontGlyphStyle.NoGlyph in Glyph.Style) and (Glyph.Bitmap <> nil) and
            (Glyph.Bitmap.Width > 0) and (Glyph.Bitmap.Height > 0);
end;

{ TGlyphsManager.TCharMap }

constructor TGlyphsManager.TCharMap.Create(const AScale: Single);
const
  BitmapSize = 1024;
begin
  BinPack := TGuillotineBinPack.Create(TPoint.Create(BitmapSize, BitmapSize));
  Texture := TBitmap.Create(BitmapSize, BitmapSize);
  Texture.BitmapScale := AScale;
end;

destructor TGlyphsManager.TCharMap.Destroy;
begin
  FreeAndNil(BinPack);
  FreeAndNil(Texture);
  inherited;
end;

{ TReusableObject }

class function TReusableObject.CreateObject<T>: T;
begin
  if TGPUObjectsPool.IsAvailable then
    Result := TGPUObjectsPool.Instance.GetObject<T>
  else
    Result := T.Create;
end;

procedure TReusableObject.DestroyObject;
begin
  if TGPUObjectsPool.IsAvailable then
    TGPUObjectsPool.Instance.ReturnObject(Self)
  else
    Self.Free;
end;

procedure TReusableObject.Init;
begin
end;

{ TGPUObjectsPool }

constructor TGPUObjectsPool.Create;
begin
  FStorage := TObjectDictionary<TClass, TObjectList<TReusableObject>>.Create([doOwnsValues]);
end;

class constructor TGPUObjectsPool.Create;
begin
  FInstance := TGPUObjectsPool.Create;
end;

destructor TGPUObjectsPool.Destroy;
begin
  FreeAndNil(FStorage);
  inherited;
end;

class function TGPUObjectsPool.GetInstance: TGPUObjectsPool;
begin
  if FDisabled then
    Exit(nil);

  if FInstance = nil then
    FInstance := TGPUObjectsPool.Create;
  Result := FInstance;
end;

function TGPUObjectsPool.GetObject<T>: T;
var
  AvailableObjects: TObjectList<TReusableObject>;
begin
  if FStorage.TryGetValue(T, AvailableObjects) and (AvailableObjects.Count > 0) then
  begin
    Result := AvailableObjects.ExtractAt(AvailableObjects.Count - 1) as T;
    Result.Init;
  end
  else
    Result := T.Create;
end;

class function TGPUObjectsPool.IsAvailable: Boolean;
begin
  Result := (FInstance <> nil) and not FDisabled;
end;

procedure TGPUObjectsPool.ReturnObject(const AObject: TReusableObject);
var
  AvailableObjects: TObjectList<TReusableObject>;
  List: TObjectList<TReusableObject>;
begin
  if FStorage.TryGetValue(AObject.ClassType, AvailableObjects) then
    List := AvailableObjects
  else
  begin
    List := TObjectList<TReusableObject>.Create;
    FStorage.Add(AObject.ClassType, List);
  end;
  List.Add(AObject);
end;

class procedure TGPUObjectsPool.SetDisabled(const Value: Boolean);
begin
  if FDisabled <> Value then
  begin
    FDisabled := Value;
    if FDisabled then
      FreeAndNil(FInstance);
  end;
end;

class procedure TGPUObjectsPool.Uninitialize;
begin
  FreeAndNil(FInstance);
end;

{ TReusableList<T> }

function TReusableList<T>.Add: T;
begin
  Result := T.CreateObject<T>;
  FItems.Add(Result);
end;

constructor TReusableList<T>.Create;
begin
  inherited;
  FItems := TList<T>.Create;
  FItems.OnNotify := NotifyEventHandler;
end;

procedure TReusableList<T>.DeleteRange(const AIndex, ACount: Integer);
begin
  FItems.DeleteRange(AIndex, ACount);
end;

procedure TReusableList<T>.Delete(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor TReusableList<T>.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TReusableList<T>.First: T;
begin
  Result := FItems.First;
end;

function TReusableList<T>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TReusableList<T>.GetItems(const AIndex: Integer): T;
begin
  Result := FItems[AIndex];
end;

procedure TReusableList<T>.Init;
begin
  inherited;
  FItems.Clear;
end;

procedure TReusableList<T>.Insert(const AIndex: Integer; const AItem: T);
begin
  FItems.Insert(AIndex, AItem)
end;

function TReusableList<T>.Last: T;
begin
  Result := FItems.Last;
end;

procedure TReusableList<T>.NotifyEventHandler(Sender: TObject; const Item: T; Action: TCollectionNotification);
begin
  if Action in [cnDeleting, cnRemoved] then
  begin
    if TGPUObjectsPool.IsAvailable then
      Item.DestroyObject
    else
      Item.DisposeOf;
  end;
end;

procedure TReusableList<T>.Remove(const AItem: T);
begin
  FItems.Remove(AItem);
end;

initialization
finalization
  TGPUTextLayout.Uninitialize;
end.
