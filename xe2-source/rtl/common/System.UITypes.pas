{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit System.UITypes;

interface

//uses System.Types;


const
  { Logical Font }
  {$EXTERNALSYM LF_FACESIZE}
  LF_FACESIZE = 32;

{$SCOPEDENUMS ON}
type

{ TOpenDialog }
  TOpenOption = (ofReadOnly, ofOverwritePrompt, ofHideReadOnly,
    ofNoChangeDir, ofShowHelp, ofNoValidate, ofAllowMultiSelect,
    ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofCreatePrompt,
    ofShareAware, ofNoReadOnlyReturn, ofNoTestFileCreate, ofNoNetworkButton,
    ofNoLongNames, ofOldStyleDialog, ofNoDereferenceLinks, ofEnableIncludeNotify,
    ofEnableSizing, ofDontAddToRecent, ofForceShowHidden);
  TOpenOptions = set of TOpenOption;

  TOpenOptionEx = (ofExNoPlacesBar);
  TOpenOptionsEx = set of TOpenOptionEx;

{ TPrintDialog }
  TPrintRange = (prAllPages, prSelection, prPageNums);
  TPrintDialogOption = (poPrintToFile, poPageNums, poSelection, poWarning,
    poHelp, poDisablePrintToFile);
  TPrintDialogOptions = set of TPrintDialogOption;

  TPageSetupDialogOption = (psoDefaultMinMargins, psoDisableMargins,
    psoDisableOrientation, psoDisablePagePainting, psoDisablePaper, psoDisablePrinter,
    psoMargins, psoMinMargins, psoShowHelp, psoWarning, psoNoNetworkButton);
  TPageSetupDialogOptions = set of TPageSetupDialogOption;
  TPrinterKind = (pkDotMatrix, pkHPPCL);
  TPageType = (ptEnvelope, ptPaper);
  TPageMeasureUnits = (pmDefault, pmMillimeters, pmInches);

{ Message dialog }
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;

{ Calendar common control support }
  TCalDayOfWeek = (dowMonday, dowTuesday, dowWednesday, dowThursday,
    dowFriday, dowSaturday, dowSunday, dowLocaleDefault);

{ TCustomForm }
  TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);
  TBorderIcons = set of TBorderIcon;

{ TScrollingWinControl }
  TWindowState = (wsNormal, wsMinimized, wsMaximized);

{ Editors common support }
  TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);

{ TFont }
  TFontCharset = 0..255;
  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontQuality = (fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased,
    fqClearType, fqClearTypeNatural);

  { Changes to the following types should be reflected in the $HPPEMIT directives. }
  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
{$IF NOT DEFINED(CLR)}
  {$NODEFINE TFontStyle}
{$IFEND}
  TFontStyles = set of TFontStyle;
  TFontName = type string;
{$IF DEFINED(CLR)}
  TFontDataName = string;
{$ELSE}
  TFontDataName = string[(LF_FACESIZE - 1) * 4];
  {$NODEFINE TFontDataName}
  TFontStylesBase = set of TFontStyle;
  {$NODEFINE TFontStylesBase}

  (*$HPPEMIT OPENNAMESPACE *)
  (*$HPPEMIT '  enum TFontStyle { fsBold, fsItalic, fsUnderline, fsStrikeOut };'*)
  (*$HPPEMIT '  typedef ::System::SmallStringBase<124> TFontDataName;'*)
  (*$HPPEMIT '  typedef ::System::SetBase<TFontStyle, fsBold, fsStrikeOut> TFontStylesBase;'*)
  (*$HPPEMIT CLOSENAMESPACE *)
{$IFEND}


  TCloseAction = (caNone, caHide, caFree, caMinimize);
  TMouseButton = (mbLeft, mbRight, mbMiddle);
  TMouseActivate = (maDefault, maActivate, maActivateAndEat, maNoActivate, maNoActivateAndEat);
  TTabOrder = -1 .. 32767;
  TModalResult = Low(Integer) .. High(Integer);
  TDragMode = (dmManual, dmAutomatic);
  TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  TDragKind = (dkDrag, dkDock);

  TAnchorKind = (akLeft, akTop, akRight, akBottom);
  TAnchors = set of TAnchorKind;

  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
    scTrack, scTop, scBottom, scEndScroll);

  TPrinterState = (psNoHandle, psHandleIC, psHandleDC);
  TPrinterOrientation = (poPortrait, poLandscape);
  TPrinterCapability = (pcCopies, pcOrientation, pcCollation);
  TPrinterCapabilities = set of TPrinterCapability;

  TCursor = -32768..32767;
{$IF NOT DEFINED(CLR)}
  {$NODEFINE TCursor}
  {$OBJTYPENAME TCursor 'NTCursor'}

  (*$HPPEMIT OPENNAMESPACE*)
  (*$HPPEMIT '#pragma option -b-'*)
  (*$HPPEMIT '  enum TCursor {crMin=-32768, crMax=32767};'}*)
  (*$HPPEMIT '#pragma option -b.'*)
  (*$HPPEMIT CLOSENAMESPACE*)
{$IFEND}

const
  crDefault     = TCursor(0);
  crNone        = TCursor(-1);
  crArrow       = TCursor(-2);
  crCross       = TCursor(-3);
  crIBeam       = TCursor(-4);
  crSize        = TCursor(-22);
  crSizeNESW    = TCursor(-6);
  crSizeNS      = TCursor(-7);
  crSizeNWSE    = TCursor(-8);
  crSizeWE      = TCursor(-9);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
  crSQLWait     = TCursor(-17);
  crNo          = TCursor(-18);
  crAppStart    = TCursor(-19);
  crHelp        = TCursor(-20);
  crHandPoint   = TCursor(-21);
  crSizeAll     = TCursor(-22);

const
  idOK       = 1;
  idCancel   = 2;
  idAbort    = 3;
  idRetry    = 4;
  idIgnore   = 5;
  idYes      = 6;
  idNo       = 7;
  idClose    = 8;
  idHelp     = 9;
  idTryAgain = 10;
  idContinue = 11;
  mrNone     = 0;
  mrOk       = idOk;
  mrCancel   = idCancel;
  mrAbort    = idAbort;
  mrRetry    = idRetry;
  mrIgnore   = idIgnore;
  mrYes      = idYes;
  mrNo       = idNo;
  mrClose    = idClose;
  mrHelp     = idHelp;
  mrTryAgain = idTryAgain;
  mrContinue = idContinue;
  mrAll      = mrContinue + 1;
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;

function IsPositiveResult(const AModalResult: TModalResult): Boolean;
function IsNegativeResult(const AModalResult: TModalResult): Boolean;
function IsAbortResult(const AModalResult: TModalResult): Boolean;
function IsAnAllResult(const AModalResult: TModalResult): Boolean;
function StripAllFromResult(const AModalResult: TModalResult): TModalResult;

const
  { Virtual Keys, Standard Set }
  vkLButton = 1;
  vkRButton = 2;
  vkCancel = 3;
  vkMButton = 4;
  vkXButton1 = 5;
  vkXButton2 = 6;
  vkBack = 8;
  vkTab = 9;
  vkClear = 12;
  vkReturn = 13;
  vkShift = $10;
  vkControl = 17;
  vkMenu = 18;
  vkPause = 19;
  vkCapital = 20;
  vkKana = 21;
  vkHangul = 21;
  vkJunja = 23;
  vkFinal = 24;
  vkHanja = 25;
  vkKanji = 25;
  vkConvert = 28;
  vkNonConvert = 29;
  vkAccept = 30;
  vkModeChange = 31;
  vkEscape = 27;
  vkSpace = $20;
  vkPrior = 33;
  vkNext = 34;
  vkEnd = 35;
  vkHome = 36;
  vkLeft = 37;
  vkUp = 38;
  vkRight = 39;
  vkDown = 40;
  vkSelect = 41;
  vkPrint = 42;
  vkExecute = 43;
  vkSnapshot = 44;
  vkInsert = 45;
  vkDelete = 46;
  vkHelp = 47;
  { vk0 thru vk9 are the same as ASCII '0' thru '9' ($30 - $39) }
  { vkA thru vkZ are the same as ASCII 'A' thru 'Z' ($41 - $5A) }
  vkLWin = 91;
  vkRWin = 92;
  vkApps = 93;
  vkSleep = 95;
  vkNumpad0 = 96;
  vkNumpad1 = 97;
  vkNumpad2 = 98;
  vkNumpad3 = 99;
  vkNumpad4 = 100;
  vkNumpad5 = 101;
  vkNumpad6 = 102;
  vkNumpad7 = 103;
  vkNumpad8 = 104;
  vkNumpad9 = 105;
  vkMultiply = 106;
  vkAdd = 107;
  vkSeparator = 108;
  vkSubtract = 109;
  vkDecimal = 110;
  vkDivide = 111;
  vkF1 = 112;
  vkF2 = 113;
  vkF3 = 114;
  vkF4 = 115;
  vkF5 = 116;
  vkF6 = 117;
  vkF7 = 118;
  vkF8 = 119;
  vkF9 = 120;
  vkF10 = 121;
  vkF11 = 122;
  vkF12 = 123;
  vkF13 = 124;
  vkF14 = 125;
  vkF15 = 126;
  vkF16 = 127;
  vkF17 = 128;
  vkF18 = 129;
  vkF19 = 130;
  vkF20 = 131;
  vkF21 = 132;
  vkF22 = 133;
  vkF23 = 134;
  vkF24 = 135;
  vkNumLock = 144;
  vkScroll = 145;
  vkLShift = 160;
  vkRShift = 161;
  vkLControl = 162;
  vkRControl = 163;
  vkLMenu = 164;
  vkRMenu = 165;
  vkProcessKey = 229;
  vkAttn = 246;
  vkCrsel = 247;
  vkExsel = 248;
  vkErEof = 249;
  vkPlay = 250;
  vkZoom = 251;
  vkNoname = 252;
  vkPA1 = 253;
  vkOemClear = 254;

{ Graphics Objects }

type
  TColorRef = UInt32;

  PColor = ^TColor;
  TColor = -$7FFFFFFF-1..$7FFFFFFF;

{$IF NOT DEFINED(CLR)}
  {$NODEFINE TColor}
  {$OBJTYPENAME TColor 'NTColor'}

  (*$HPPEMIT OPENNAMESPACE*)
  (*$HPPEMIT '  enum TColor {clMin=-0x7fffffff-1, clMax=0x7fffffff};'*)
  (*$HPPEMIT CLOSENAMESPACE*)
{$IFEND}

  PColorRec = ^TColorRec;
  TColorRec = record
  const
    SystemColor = $FF000000;
    // System Colors (Windows only)
    cSCROLLBAR = 0;
    cBACKGROUND = 1;
    cACTIVECAPTION = 2;
    cINACTIVECAPTION = 3;
    cMENU = 4;
    cWINDOW = 5;
    cWINDOWFRAME = 6;
    cMENUTEXT = 7;
    cWINDOWTEXT = 8;
    cCAPTIONTEXT = 9;
    cACTIVEBORDER = 10;
    cINACTIVEBORDER = 11;
    cAPPWORKSPACE = 12;
    cHIGHLIGHT = 13;
    cHIGHLIGHTTEXT = 14;
    cBTNFACE = 15;
    cBTNSHADOW = $10;
    cGRAYTEXT = 17;
    cBTNTEXT = 18;
    cINACTIVECAPTIONTEXT = 19;
    cBTNHIGHLIGHT = 20;
    c3DDKSHADOW = 21;
    c3DLIGHT = 22;
    cINFOTEXT = 23;
    cINFOBK = 24;
    cHOTLIGHT = 26;
    cGRADIENTACTIVECAPTION = 27;
    cGRADIENTINACTIVECAPTION = 28;
    cMENUHILIGHT = 29;
    cMENUBAR = 30;
    cENDCOLORS = cMENUBAR;
    cDESKTOP = cBACKGROUND;
    c3DFACE = cBTNFACE;
    c3DSHADOW = cBTNSHADOW;
    c3DHIGHLIGHT = cBTNHIGHLIGHT;
    c3DHILIGHT = cBTNHIGHLIGHT;
    cBTNHILIGHT = cBTNHIGHLIGHT;
    SysScrollBar = TColor(SystemColor or cSCROLLBAR);
    SysBackground = TColor(SystemColor or cBACKGROUND);
    SysActiveCaption = TColor(SystemColor or cACTIVECAPTION);
    SysInactiveCaption = TColor(SystemColor or cINACTIVECAPTION);
    SysMenu = TColor(SystemColor or cMENU);
    SysWindow = TColor(SystemColor or cWINDOW);
    SysWindowFrame = TColor(SystemColor or cWINDOWFRAME);
    SysMenuText = TColor(SystemColor or cMENUTEXT);
    SysWindowText = TColor(SystemColor or cWINDOWTEXT);
    SysCaptionText = TColor(SystemColor or cCAPTIONTEXT);
    SysActiveBorder = TColor(SystemColor or cACTIVEBORDER);
    SysInactiveBorder = TColor(SystemColor or cINACTIVEBORDER);
    SysAppWorkSpace = TColor(SystemColor or cAPPWORKSPACE);
    SysHighlight = TColor(SystemColor or cHIGHLIGHT);
    SysHighlightText = TColor(SystemColor or cHIGHLIGHTTEXT);
    SysBtnFace = TColor(SystemColor or cBTNFACE);
    SysBtnShadow = TColor(SystemColor or cBTNSHADOW);
    SysGrayText = TColor(SystemColor or cGRAYTEXT);
    SysBtnText = TColor(SystemColor or cBTNTEXT);
    SysInactiveCaptionText = TColor(SystemColor or cINACTIVECAPTIONTEXT);
    SysBtnHighlight = TColor(SystemColor or cBTNHIGHLIGHT);
    Sys3DDkShadow = TColor(SystemColor or c3DDKSHADOW);
    Sys3DLight = TColor(SystemColor or c3DLIGHT);
    SysInfoText = TColor(SystemColor or cINFOTEXT);
    SysInfoBk = TColor(SystemColor or cINFOBK);
    SysHotLight = TColor(SystemColor or cHOTLIGHT);
    SysGradientActiveCaption = TColor(SystemColor or cGRADIENTACTIVECAPTION);
    SysGradientInactiveCaption = TColor(SystemColor or cGRADIENTINACTIVECAPTION);
    SysMenuHighlight = TColor(SystemColor or cMENUHILIGHT);
    SysMenuBar = TColor(SystemColor or cMENUBAR);
    SysNone = TColor($1FFFFFFF);
    SysDefault = TColor($20000000);
    // Actual colors
    Aliceblue = TColor($FFF8F0);
    Antiquewhite = TColor($D7EBFA);
    Aqua = TColor($FFFF00);
    Aquamarine = TColor($D4FF7F);
    Azure = TColor($FFFFF0);
    Beige = TColor($DCF5F5);
    Bisque = TColor($C4E4FF);
    Black = TColor($000000);
    Blanchedalmond = TColor($CDEBFF);
    Blue = TColor($FF0000);
    Blueviolet = TColor($E22B8A);
    Brown = TColor($2A2AA5);
    Burlywood = TColor($87B8DE);
    Cadetblue = TColor($A09E5F);
    Chartreuse = TColor($00FF7F);
    Chocolate = TColor($1E69D2);
    Coral = TColor($507FFF);
    Cornflowerblue = TColor($ED9564);
    Cornsilk = TColor($DCF8FF);
    Crimson = TColor($3C14DC);
    Cyan = TColor($FFFF00);
    Darkblue = TColor($8B0000);
    Darkcyan = TColor($8B8B00);
    Darkgoldenrod = TColor($0B86B8);
    Darkgray = TColor($A9A9A9);
    Darkgreen = TColor($006400);
    Darkgrey = TColor($A9A9A9);
    Darkkhaki = TColor($6BB7BD);
    Darkmagenta = TColor($8B008B);
    Darkolivegreen = TColor($2F6B55);
    Darkorange = TColor($008CFF);
    Darkorchid = TColor($CC3299);
    Darkred = TColor($00008B);
    Darksalmon = TColor($7A96E9);
    Darkseagreen = TColor($8FBC8F);
    Darkslateblue = TColor($8B3D48);
    Darkslategray = TColor($4F4F2F);
    Darkslategrey = TColor($4F4F2F);
    Darkturquoise = TColor($D1CE00);
    Darkviolet = TColor($D30094);
    Deeppink = TColor($9314FF);
    Deepskyblue = TColor($FFBF00);
    Dimgray = TColor($696969);
    Dimgrey = TColor($696969);
    Dodgerblue = TColor($FF901E);
    Firebrick = TColor($2222B2);
    Floralwhite = TColor($F0FAFF);
    Forestgreen = TColor($228B22);
    Fuchsia = TColor($FF00FF);
    Gainsboro = TColor($DCDCDC);
    Ghostwhite = TColor($FFF8F8);
    Gold = TColor($00D7FF);
    Goldenrod = TColor($20A5DA);
    Gray = TColor($808080);
    Green = TColor($008000);
    Greenyellow = TColor($2FFFAD);
    Grey = TColor($808080);
    Honeydew = TColor($F0FFF0);
    Hotpink = TColor($B469FF);
    Indianred = TColor($5C5CCD);
    Indigo = TColor($82004B);
    Ivory = TColor($F0FFFF);
    Khaki = TColor($8CE6F0);
    Lavender = TColor($FAE6E6);
    Lavenderblush = TColor($F5F0FF);
    Lawngreen = TColor($00FC7C);
    Lemonchiffon = TColor($CDFAFF);
    Lightblue = TColor($E6D8AD);
    Lightcoral = TColor($8080F0);
    Lightcyan = TColor($FFFFE0);
    Lightgoldenrodyellow = TColor($D2FAFA);
    Lightgray = TColor($D3D3D3);
    Lightgreen = TColor($90EE90);
    Lightgrey = TColor($D3D3D3);
    Lightpink = TColor($C1B6FF);
    Lightsalmon = TColor($7AA0FF);
    Lightseagreen = TColor($AAB220);
    Lightskyblue = TColor($FACE87);
    Lightslategray = TColor($998877);
    Lightslategrey = TColor($998877);
    Lightsteelblue = TColor($DEC4B0);
    Lightyellow = TColor($E0FFFF);
    LtGray = TColor($C0C0C0);
    MedGray = TColor($A4A0A0);
    DkGray = TColor($808080);
    MoneyGreen = TColor($C0DCC0);
    LegacySkyBlue = TColor($F0CAA6);
    Cream = TColor($F0FBFF);
    Lime = TColor($00FF00);
    Limegreen = TColor($32CD32);
    Linen = TColor($E6F0FA);
    Magenta = TColor($FF00FF);
    Maroon = TColor($000080);
    Mediumaquamarine = TColor($AACD66);
    Mediumblue = TColor($CD0000);
    Mediumorchid = TColor($D355BA);
    Mediumpurple = TColor($DB7093);
    Mediumseagreen = TColor($71B33C);
    Mediumslateblue = TColor($EE687B);
    Mediumspringgreen = TColor($9AFA00);
    Mediumturquoise = TColor($CCD148);
    Mediumvioletred = TColor($8515C7);
    Midnightblue = TColor($701919);
    Mintcream = TColor($FAFFF5);
    Mistyrose = TColor($E1E4FF);
    Moccasin = TColor($B5E4FF);
    Navajowhite = TColor($ADDEFF);
    Navy = TColor($800000);
    Oldlace = TColor($E6F5FD);
    Olive = TColor($008080);
    Olivedrab = TColor($238E6B);
    Orange = TColor($00A5FF);
    Orangered = TColor($0045FF);
    Orchid = TColor($D670DA);
    Palegoldenrod = TColor($AAE8EE);
    Palegreen = TColor($98FB98);
    Paleturquoise = TColor($EEEEAF);
    Palevioletred = TColor($9370DB);
    Papayawhip = TColor($D5EFFF);
    Peachpuff = TColor($B9DAFF);
    Peru = TColor($3F85CD);
    Pink = TColor($CBC0FF);
    Plum = TColor($DDA0DD);
    Powderblue = TColor($E6E0B0);
    Purple = TColor($800080);
    Red = TColor($0000FF);
    Rosybrown = TColor($8F8FBC);
    Royalblue = TColor($E16941);
    Saddlebrown = TColor($13458B);
    Salmon = TColor($7280FA);
    Sandybrown = TColor($60A4F4);
    Seagreen = TColor($578B2E);
    Seashell = TColor($EEF5FF);
    Sienna = TColor($2D52A0);
    Silver = TColor($C0C0C0);
    Skyblue = TColor($EBCE87);
    Slateblue = TColor($CD5A6A);
    Slategray = TColor($908070);
    Slategrey = TColor($908070);
    Snow = TColor($FAFAFF);
    Springgreen = TColor($7FFF00);
    Steelblue = TColor($B48246);
    Tan = TColor($8CB4D2);
    Teal = TColor($808000);
    Thistle = TColor($D8BFD8);
    Tomato = TColor($4763FF);
    Turquoise = TColor($D0E040);
    Violet = TColor($EE82EE);
    Wheat = TColor($B3DEF5);
    White = TColor($FFFFFF);
    Whitesmoke = TColor($F5F5F5);
    Yellow = TColor($00FFFF);
    Yellowgreen = TColor($32CD9A);
    Null = TColor($00000000);
    class var ColorToRGB: function (Color: TColor): Longint;
{    class operator Implicit(const C: TColor): TColorRec; inline;
    class operator Implicit(const C: TColorRec): TColor; inline;
    class operator Implicit(const C: TColorRec): Longint; inline;
    class operator Explicit(const C: TColorRec): Longint; inline;}
    case LongWord of
      0:
        (Color: TColor);
      2:
        (HiWord, LoWord: Word);
      3:
{$IFDEF BIGENDIAN}
        (A, B, G, R: System.Byte);
{$ELSE}
        (R, G, B, A: System.Byte);
{$ENDIF}
  end;

  TColors = TColorRec;

  PAlphaColor = ^TAlphaColor;
  TAlphaColor = type Cardinal;

  PAlphaColorRec = ^TAlphaColorRec;
  TAlphaColorRec = record
  const
    Alpha = TAlphaColor($FF000000);
    Aliceblue = Alpha or TAlphaColor($F0F8FF);
    Antiquewhite = Alpha or TAlphaColor($FAEBD7);
    Aqua = Alpha or TAlphaColor($00FFFF);
    Aquamarine = Alpha or TAlphaColor($7FFFD4);
    Azure = Alpha or TAlphaColor($F0FFFF);
    Beige = Alpha or TAlphaColor($F5F5DC);
    Bisque = Alpha or TAlphaColor($FFE4C4);
    Black = Alpha or TAlphaColor($000000);
    Blanchedalmond = Alpha or TAlphaColor($FFEBCD);
    Blue = Alpha or TAlphaColor($0000FF);
    Blueviolet = Alpha or TAlphaColor($8A2BE2);
    Brown = Alpha or TAlphaColor($A52A2A);
    Burlywood = Alpha or TAlphaColor($DEB887);
    Cadetblue = Alpha or TAlphaColor($5F9EA0);
    Chartreuse = Alpha or TAlphaColor($7FFF00);
    Chocolate = Alpha or TAlphaColor($D2691E);
    Coral = Alpha or TAlphaColor($FF7F50);
    Cornflowerblue = Alpha or TAlphaColor($6495ED);
    Cornsilk = Alpha or TAlphaColor($FFF8DC);
    Crimson = Alpha or TAlphaColor($DC143C);
    Cyan = Alpha or TAlphaColor($00FFFF);
    Darkblue = Alpha or TAlphaColor($00008B);
    Darkcyan = Alpha or TAlphaColor($008B8B);
    Darkgoldenrod = Alpha or TAlphaColor($B8860B);
    Darkgray = Alpha or TAlphaColor($A9A9A9);
    Darkgreen = Alpha or TAlphaColor($006400);
    Darkgrey = Alpha or TAlphaColor($A9A9A9);
    Darkkhaki = Alpha or TAlphaColor($BDB76B);
    Darkmagenta = Alpha or TAlphaColor($8B008B);
    Darkolivegreen = Alpha or TAlphaColor($556B2F);
    Darkorange = Alpha or TAlphaColor($FF8C00);
    Darkorchid = Alpha or TAlphaColor($9932CC);
    Darkred = Alpha or TAlphaColor($8B0000);
    Darksalmon = Alpha or TAlphaColor($E9967A);
    Darkseagreen = Alpha or TAlphaColor($8FBC8F);
    Darkslateblue = Alpha or TAlphaColor($483D8B);
    Darkslategray = Alpha or TAlphaColor($2F4F4F);
    Darkslategrey = Alpha or TAlphaColor($2F4F4F);
    Darkturquoise = Alpha or TAlphaColor($00CED1);
    Darkviolet = Alpha or TAlphaColor($9400D3);
    Deeppink = Alpha or TAlphaColor($FF1493);
    Deepskyblue = Alpha or TAlphaColor($00BFFF);
    Dimgray = Alpha or TAlphaColor($696969);
    Dimgrey = Alpha or TAlphaColor($696969);
    Dodgerblue = Alpha or TAlphaColor($1E90FF);
    Firebrick = Alpha or TAlphaColor($B22222);
    Floralwhite = Alpha or TAlphaColor($FFFAF0);
    Forestgreen = Alpha or TAlphaColor($228B22);
    Fuchsia = Alpha or TAlphaColor($FF00FF);
    Gainsboro = Alpha or TAlphaColor($DCDCDC);
    Ghostwhite = Alpha or TAlphaColor($F8F8FF);
    Gold = Alpha or TAlphaColor($FFD700);
    Goldenrod = Alpha or TAlphaColor($DAA520);
    Gray = Alpha or TAlphaColor($808080);
    Green = Alpha or TAlphaColor($008000);
    Greenyellow = Alpha or TAlphaColor($ADFF2F);
    Grey = Alpha or TAlphaColor($808080);
    Honeydew = Alpha or TAlphaColor($F0FFF0);
    Hotpink = Alpha or TAlphaColor($FF69B4);
    Indianred = Alpha or TAlphaColor($CD5C5C);
    Indigo = Alpha or TAlphaColor($4B0082);
    Ivory = Alpha or TAlphaColor($FFFFF0);
    Khaki = Alpha or TAlphaColor($F0E68C);
    Lavender = Alpha or TAlphaColor($E6E6FA);
    Lavenderblush = Alpha or TAlphaColor($FFF0F5);
    Lawngreen = Alpha or TAlphaColor($7CFC00);
    Lemonchiffon = Alpha or TAlphaColor($FFFACD);
    Lightblue = Alpha or TAlphaColor($ADD8E6);
    Lightcoral = Alpha or TAlphaColor($F08080);
    Lightcyan = Alpha or TAlphaColor($E0FFFF);
    Lightgoldenrodyellow = Alpha or TAlphaColor($FAFAD2);
    Lightgray = Alpha or TAlphaColor($D3D3D3);
    Lightgreen = Alpha or TAlphaColor($90EE90);
    Lightgrey = Alpha or TAlphaColor($D3D3D3);
    Lightpink = Alpha or TAlphaColor($FFB6C1);
    Lightsalmon = Alpha or TAlphaColor($FFA07A);
    Lightseagreen = Alpha or TAlphaColor($20B2AA);
    Lightskyblue = Alpha or TAlphaColor($87CEFA);
    Lightslategray = Alpha or TAlphaColor($778899);
    Lightslategrey = Alpha or TAlphaColor($778899);
    Lightsteelblue = Alpha or TAlphaColor($B0C4DE);
    Lightyellow = Alpha or TAlphaColor($FFFFE0);
    LtGray = Alpha or TAlphaColor($C0C0C0);
    MedGray = Alpha or TAlphaColor($A0A0A0);
    DkGray = Alpha or TAlphaColor($808080);
    MoneyGreen = Alpha or TAlphaColor($C0DCC0);
    LegacySkyBlue = Alpha or TAlphaColor($F0CAA6);
    Cream = Alpha or TAlphaColor($F0FBFF);
    Lime = Alpha or TAlphaColor($00FF00);
    Limegreen = Alpha or TAlphaColor($32CD32);
    Linen = Alpha or TAlphaColor($FAF0E6);
    Magenta = Alpha or TAlphaColor($FF00FF);
    Maroon = Alpha or TAlphaColor($800000);
    Mediumaquamarine = Alpha or TAlphaColor($66CDAA);
    Mediumblue = Alpha or TAlphaColor($0000CD);
    Mediumorchid = Alpha or TAlphaColor($BA55D3);
    Mediumpurple = Alpha or TAlphaColor($9370DB);
    Mediumseagreen = Alpha or TAlphaColor($3CB371);
    Mediumslateblue = Alpha or TAlphaColor($7B68EE);
    Mediumspringgreen = Alpha or TAlphaColor($00FA9A);
    Mediumturquoise = Alpha or TAlphaColor($48D1CC);
    Mediumvioletred = Alpha or TAlphaColor($C71585);
    Midnightblue = Alpha or TAlphaColor($191970);
    Mintcream = Alpha or TAlphaColor($F5FFFA);
    Mistyrose = Alpha or TAlphaColor($FFE4E1);
    Moccasin = Alpha or TAlphaColor($FFE4B5);
    Navajowhite = Alpha or TAlphaColor($FFDEAD);
    Navy = Alpha or TAlphaColor($000080);
    Oldlace = Alpha or TAlphaColor($FDF5E6);
    Olive = Alpha or TAlphaColor($808000);
    Olivedrab = Alpha or TAlphaColor($6B8E23);
    Orange = Alpha or TAlphaColor($FFA500);
    Orangered = Alpha or TAlphaColor($FF4500);
    Orchid = Alpha or TAlphaColor($DA70D6);
    Palegoldenrod = Alpha or TAlphaColor($EEE8AA);
    Palegreen = Alpha or TAlphaColor($98FB98);
    Paleturquoise = Alpha or TAlphaColor($AFEEEE);
    Palevioletred = Alpha or TAlphaColor($DB7093);
    Papayawhip = Alpha or TAlphaColor($FFEFD5);
    Peachpuff = Alpha or TAlphaColor($FFDAB9);
    Peru = Alpha or TAlphaColor($CD853F);
    Pink = Alpha or TAlphaColor($FFC0CB);
    Plum = Alpha or TAlphaColor($DDA0DD);
    Powderblue = Alpha or TAlphaColor($B0E0E6);
    Purple = Alpha or TAlphaColor($800080);
    Red = Alpha or TAlphaColor($FF0000);
    Rosybrown = Alpha or TAlphaColor($BC8F8F);
    Royalblue = Alpha or TAlphaColor($4169E1);
    Saddlebrown = Alpha or TAlphaColor($8B4513);
    Salmon = Alpha or TAlphaColor($FA8072);
    Sandybrown = Alpha or TAlphaColor($F4A460);
    Seagreen = Alpha or TAlphaColor($2E8B57);
    Seashell = Alpha or TAlphaColor($FFF5EE);
    Sienna = Alpha or TAlphaColor($A0522D);
    Silver = Alpha or TAlphaColor($C0C0C0);
    Skyblue = Alpha or TAlphaColor($87CEEB);
    Slateblue = Alpha or TAlphaColor($6A5ACD);
    Slategray = Alpha or TAlphaColor($708090);
    Slategrey = Alpha or TAlphaColor($708090);
    Snow = Alpha or TAlphaColor($FFFAFA);
    Springgreen = Alpha or TAlphaColor($00FF7F);
    Steelblue = Alpha or TAlphaColor($4682B4);
    Tan = Alpha or TAlphaColor($D2B48C);
    Teal = Alpha or TAlphaColor($008080);
    Thistle = Alpha or TAlphaColor($D8BFD8);
    Tomato = Alpha or TAlphaColor($FF6347);
    Turquoise = Alpha or TAlphaColor($40E0D0);
    Violet = Alpha or TAlphaColor($EE82EE);
    Wheat = Alpha or TAlphaColor($F5DEB3);
    White = Alpha or TAlphaColor($FFFFFF);
    Whitesmoke = Alpha or TAlphaColor($F5F5F5);
    Yellow = Alpha or TAlphaColor($FFFF00);
    Yellowgreen = Alpha or TAlphaColor($9ACD32);
    Null = TAlphaColor($00000000);
    class var ColorToRGB: function (Color: TAlphaColor): Longint;
{    class operator Implicit(const C: TAlphaColorRec): TAlphaColor; inline;
    class operator Implicit(const C: TAlphaColorRec): TColor; inline;
    class operator Implicit(const C: TColorRec): TAlphaColorRec; inline;
    class operator Implicit(const C: TColor): TAlphaColorRec; inline;}
    case LongWord of
      0:
        (Color: TAlphaColor);
      2:
        (HiWord, LoWord: Word);
      3:
{$IFDEF BIGENDIAN}
        (A, R, G, B: System.Byte);
{$ELSE}
        (B, G, R, A: System.Byte);
{$ENDIF}
  end;

  TAlphaColors = TAlphaColorRec;

implementation

{ Modal result testers }

function IsPositiveResult(const AModalResult: TModalResult): Boolean;
begin
  case AModalResult of
    mrOk, mrYes, mrAll, mrYesToAll, mrContinue: Result := True;
  else
    Result := False;
  end;
end;

function IsNegativeResult(const AModalResult: TModalResult): Boolean;
begin
  case AModalResult of
    mrNo, mrNoToAll, mrTryAgain: Result := True;
  else
    Result := False;
  end;
end;

function IsAbortResult(const AModalResult: TModalResult): Boolean;
begin
  case AModalResult of
    mrCancel, mrAbort: Result := True;
  else
    Result := False;
  end;
end;

function IsAnAllResult(const AModalResult: TModalResult): Boolean;
begin
  case AModalResult of
    mrAll, mrNoToAll, mrYesToAll: Result := True;
  else
    Result := False;
  end;
end;

function StripAllFromResult(const AModalResult: TModalResult): TModalResult;
begin
  case AModalResult of
    mrAll:      Result := mrOk;
    mrNoToAll:  Result := mrNo;
    mrYesToAll: Result := mrYes;
  else
    Result := AModalResult;
  end;
end;

{ TColorRec }
(*
class operator TColorRec.Implicit(const C: TColor): TColorRec;
begin
  Result.Color := C;
end;

class operator TColorRec.Implicit(const C: TColorRec): TColor;
begin
  Result := C.Color;
end;

class operator TColorRec.Explicit(const C: TColorRec): Longint;
begin
  Result := C;
end;

class operator TColorRec.Implicit(const C: TColorRec): Longint;
begin
  if Assigned(ColorToRGB) then
    Result := ColorToRGB(C.Color)
  else
    Result := Longint(C.Color and not SystemColor);
end;
*)
{ TAlphaColorRec }
(*
class operator TAlphaColorRec.Implicit(const C: TAlphaColorRec): TAlphaColor;
begin
  Result := C.Color;
end;

class operator TAlphaColorRec.Implicit(const C: TColorRec): TAlphaColorRec;
begin
  Result.Color := Alpha or TAlphaColor(C.Color);
end;

class operator TAlphaColorRec.Implicit(const C: TAlphaColorRec): TColor;
begin
  Result := TColor(C.Color and not Alpha);
end;

class operator TAlphaColorRec.Implicit(const C: TColor): TAlphaColorRec;
begin
  Result.Color := Alpha or TAlphaColor(Longint(TColorRec(C)));
end;
*)

end.
