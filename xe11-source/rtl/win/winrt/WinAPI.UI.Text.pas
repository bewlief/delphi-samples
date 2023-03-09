{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.Text;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonTypes, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  CaretType = Winapi.CommonTypes.CaretType;
  PCaretType = Winapi.CommonTypes.PCaretType;
  FindOptions = Winapi.CommonTypes.FindOptions;
  PFindOptions = Winapi.CommonTypes.PFindOptions;
  FontStretch = Winapi.CommonTypes.FontStretch;
  PFontStretch = Winapi.CommonTypes.PFontStretch;
  FontStyle = Winapi.CommonTypes.FontStyle;
  PFontStyle = Winapi.CommonTypes.PFontStyle;
  FormatEffect = Winapi.CommonTypes.FormatEffect;
  PFormatEffect = Winapi.CommonTypes.PFormatEffect;
  HorizontalCharacterAlignment = Winapi.CommonTypes.HorizontalCharacterAlignment;
  PHorizontalCharacterAlignment = Winapi.CommonTypes.PHorizontalCharacterAlignment;
  IContentLinkInfo = Winapi.CommonTypes.IContentLinkInfo;
  PIContentLinkInfo = Winapi.CommonTypes.PIContentLinkInfo;
  ITextCharacterFormat = Winapi.CommonTypes.ITextCharacterFormat;
  PITextCharacterFormat = Winapi.CommonTypes.PITextCharacterFormat;
  ITextDocument = Winapi.CommonTypes.ITextDocument;
  PITextDocument = Winapi.CommonTypes.PITextDocument;
  ITextParagraphFormat = Winapi.CommonTypes.ITextParagraphFormat;
  PITextParagraphFormat = Winapi.CommonTypes.PITextParagraphFormat;
  ITextRange = Winapi.CommonTypes.ITextRange;
  PITextRange = Winapi.CommonTypes.PITextRange;
  ITextSelection = Winapi.CommonTypes.ITextSelection;
  PITextSelection = Winapi.CommonTypes.PITextSelection;
  LetterCase = Winapi.CommonTypes.LetterCase;
  PLetterCase = Winapi.CommonTypes.PLetterCase;
  LineSpacingRule = Winapi.CommonTypes.LineSpacingRule;
  PLineSpacingRule = Winapi.CommonTypes.PLineSpacingRule;
  LinkType = Winapi.CommonTypes.LinkType;
  PLinkType = Winapi.CommonTypes.PLinkType;
  MarkerAlignment = Winapi.CommonTypes.MarkerAlignment;
  PMarkerAlignment = Winapi.CommonTypes.PMarkerAlignment;
  MarkerStyle = Winapi.CommonTypes.MarkerStyle;
  PMarkerStyle = Winapi.CommonTypes.PMarkerStyle;
  MarkerType = Winapi.CommonTypes.MarkerType;
  PMarkerType = Winapi.CommonTypes.PMarkerType;
  ParagraphAlignment = Winapi.CommonTypes.ParagraphAlignment;
  PParagraphAlignment = Winapi.CommonTypes.PParagraphAlignment;
  ParagraphStyle = Winapi.CommonTypes.ParagraphStyle;
  PParagraphStyle = Winapi.CommonTypes.PParagraphStyle;
  PointOptions = Winapi.CommonTypes.PointOptions;
  PPointOptions = Winapi.CommonTypes.PPointOptions;
  RangeGravity = Winapi.CommonTypes.RangeGravity;
  PRangeGravity = Winapi.CommonTypes.PRangeGravity;
  SelectionOptions = Winapi.CommonTypes.SelectionOptions;
  PSelectionOptions = Winapi.CommonTypes.PSelectionOptions;
  SelectionType = Winapi.CommonTypes.SelectionType;
  PSelectionType = Winapi.CommonTypes.PSelectionType;
  TabAlignment = Winapi.CommonTypes.TabAlignment;
  PTabAlignment = Winapi.CommonTypes.PTabAlignment;
  TabLeader = Winapi.CommonTypes.TabLeader;
  PTabLeader = Winapi.CommonTypes.PTabLeader;
  TextGetOptions = Winapi.CommonTypes.TextGetOptions;
  PTextGetOptions = Winapi.CommonTypes.PTextGetOptions;
  TextRangeUnit = Winapi.CommonTypes.TextRangeUnit;
  PTextRangeUnit = Winapi.CommonTypes.PTextRangeUnit;
  TextScript = Winapi.CommonTypes.TextScript;
  PTextScript = Winapi.CommonTypes.PTextScript;
  TextSetOptions = Winapi.CommonTypes.TextSetOptions;
  PTextSetOptions = Winapi.CommonTypes.PTextSetOptions;
  UnderlineType = Winapi.CommonTypes.UnderlineType;
  PUnderlineType = Winapi.CommonTypes.PUnderlineType;
  VerticalCharacterAlignment = Winapi.CommonTypes.VerticalCharacterAlignment;
  PVerticalCharacterAlignment = Winapi.CommonTypes.PVerticalCharacterAlignment;

  // Forward declarations for interfaces

  // Windows.UI.Text.ITextDocument2
  ITextDocument2 = interface;
  PITextDocument2 = ^ITextDocument2;

  // Windows.Foundation.IReference`1<Windows.UI.Text.UnderlineType>
  IReference_1__UnderlineType = interface;
  PIReference_1__UnderlineType = ^IReference_1__UnderlineType;

  // Windows.UI.Text Enums

  // Windows.UI.Text.Core.CoreTextFormatUpdatingReason
  Core_CoreTextFormatUpdatingReason = (
    None = 0,
    CompositionUnconverted = 1,
    CompositionConverted = 2,
    CompositionTargetUnconverted = 3,
    CompositionTargetConverted = 4
  );
  PCore_CoreTextFormatUpdatingReason = ^Core_CoreTextFormatUpdatingReason;

  // Windows.UI.Text.Core.CoreTextFormatUpdatingResult
  Core_CoreTextFormatUpdatingResult = (
    Succeeded = 0,
    Failed = 1
  );
  PCore_CoreTextFormatUpdatingResult = ^Core_CoreTextFormatUpdatingResult;

  // Windows.UI.Text.Core.CoreTextInputPaneDisplayPolicy
  Core_CoreTextInputPaneDisplayPolicy = (
    Automatic = 0,
    Manual = 1
  );
  PCore_CoreTextInputPaneDisplayPolicy = ^Core_CoreTextInputPaneDisplayPolicy;

  // Windows.UI.Text.Core.CoreTextInputScope
  Core_CoreTextInputScope = (
    Default = 0,
    Url = 1,
    FilePath = 2,
    FileName = 3,
    EmailUserName = 4,
    EmailAddress = 5,
    UserName = 6,
    PersonalFullName = 7,
    PersonalNamePrefix = 8,
    PersonalGivenName = 9,
    PersonalMiddleName = 10,
    PersonalSurname = 11,
    PersonalNameSuffix = 12,
    Address = 13,
    AddressPostalCode = 14,
    AddressStreet = 15,
    AddressStateOrProvince = 16,
    AddressCity = 17,
    AddressCountryName = 18,
    AddressCountryShortName = 19,
    CurrencyAmountAndSymbol = 20,
    CurrencyAmount = 21,
    Date = 22,
    DateMonth = 23,
    DateDay = 24,
    DateYear = 25,
    DateMonthName = 26,
    DateDayName = 27,
    Number = 29,
    SingleCharacter = 30,
    Password = 31,
    TelephoneNumber = 32,
    TelephoneCountryCode = 33,
    TelephoneAreaCode = 34,
    TelephoneLocalNumber = 35,
    Time = 36,
    TimeHour = 37,
    TimeMinuteOrSecond = 38,
    NumberFullWidth = 39,
    AlphanumericHalfWidth = 40,
    AlphanumericFullWidth = 41,
    CurrencyChinese = 42,
    Bopomofo = 43,
    Hiragana = 44,
    KatakanaHalfWidth = 45,
    KatakanaFullWidth = 46,
    Hanja = 47,
    HangulHalfWidth = 48,
    HangulFullWidth = 49,
    Search = 50,
    Formula = 51,
    SearchIncremental = 52,
    ChineseHalfWidth = 53,
    ChineseFullWidth = 54,
    NativeScript = 55,
    Text = 57,
    Chat = 58,
    NameOrPhoneNumber = 59,
    EmailUserNameOrAddress = 60,
    &Private = 61,
    Maps = 62,
    PasswordNumeric = 63,
    FormulaNumber = 67,
    ChatWithoutEmoji = 68,
    Digits = 28,
    PinNumeric = 64,
    PinAlphanumeric = 65
  );
  PCore_CoreTextInputScope = ^Core_CoreTextInputScope;

  // Windows.UI.Text.Core.CoreTextSelectionUpdatingResult
  Core_CoreTextSelectionUpdatingResult = (
    Succeeded = 0,
    Failed = 1
  );
  PCore_CoreTextSelectionUpdatingResult = ^Core_CoreTextSelectionUpdatingResult;

  // Windows.UI.Text.Core.CoreTextTextUpdatingResult
  Core_CoreTextTextUpdatingResult = (
    Succeeded = 0,
    Failed = 1
  );
  PCore_CoreTextTextUpdatingResult = ^Core_CoreTextTextUpdatingResult;

  // Windows.UI.Text.RichEditMathMode
  RichEditMathMode = (
    NoMath = 0,
    MathOnly = 1
  );
  PRichEditMathMode = ^RichEditMathMode;

  // Windows.UI.Text.TextDecorations
  TextDecorations = (
    None = 0,
    Underline = 1,
    Strikethrough = 2
  );
  PTextDecorations = ^TextDecorations;

  // Windows.UI.Text Records
  // Windows.UI.Text.Core.CoreTextRange
  Core_CoreTextRange = record
    StartCaretPosition: Integer;
    EndCaretPosition: Integer;
  end;
  PCore_CoreTextRange = ^Core_CoreTextRange;

  // Windows.UI.Text.FontWeight
  FontWeight = record
    Weight: Word;
  end;
  PFontWeight = ^FontWeight;

  // Windows.UI.Text Interfaces

  // UsedAPI Interface
  // Windows.UI.Text.ITextDocument2
  ITextDocument2 = interface(IInspectable)
  ['{F2311112-8C89-49C9-9118-F057CBB814EE}']
    function get_AlignmentIncludesTrailingWhitespace: Boolean; safecall;
    procedure put_AlignmentIncludesTrailingWhitespace(value: Boolean); safecall;
    function get_IgnoreTrailingCharacterSpacing: Boolean; safecall;
    procedure put_IgnoreTrailingCharacterSpacing(value: Boolean); safecall;
    property AlignmentIncludesTrailingWhitespace: Boolean read get_AlignmentIncludesTrailingWhitespace write put_AlignmentIncludesTrailingWhitespace;
    property IgnoreTrailingCharacterSpacing: Boolean read get_IgnoreTrailingCharacterSpacing write put_IgnoreTrailingCharacterSpacing;
  end;

  // Windows.Foundation.IReference`1<Windows.UI.Text.UnderlineType>
  IReference_1__UnderlineType = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: UnderlineType; safecall;
    property Value: UnderlineType read get_Value;
  end;

implementation

end.
