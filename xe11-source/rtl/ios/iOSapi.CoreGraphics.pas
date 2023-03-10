{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework CoreGraphics              }
{                                                          }
{ Copyright (c) 2008-2008, Apple Inc. All rights reserved. }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2022 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit iOSapi.CoreGraphics;

interface

uses
  System.Types, Posix.StdDef, Posix.SysTypes, Macapi.CoreFoundation, iOSapi.Foundation, iOSapi.CocoaTypes;

const
  CGFLOAT_DEFINED = 1;
  CGFLOAT_IS_DOUBLE = 0;
  CGFLOAT_MAX = 3.4028235E+38;
  CGFLOAT_MIN = 1.1754944E-38;
  CGGlyphMax = 65534;
  CGGlyphMin = 0;
  CGPDFDataFormatJPEG2000 = 2;
  CGPDFDataFormatJPEGEncoded = 1;
  CGPDFDataFormatRaw = 0;
  CGRectMaxXEdge = 2;
  CGRectMaxYEdge = 3;
  CGRectMinXEdge = 0;
  CGRectMinYEdge = 1;
  kCGBitmapAlphaInfoMask = 31;
  kCGBitmapByteOrder16Big = 12288;
  kCGBitmapByteOrder16Host = 4096;
  kCGBitmapByteOrder16Little = 4096;
  kCGBitmapByteOrder32Big = 16384;
  kCGBitmapByteOrder32Host = 8192;
  kCGBitmapByteOrder32Little = 8192;
  kCGBitmapByteOrderDefault = 0;
  kCGBitmapByteOrderMask = 28672;
  kCGBitmapFloatComponents = 256;
  kCGBlendModeClear = 16;
  kCGBlendModeColor = 14;
  kCGBlendModeColorBurn = 7;
  kCGBlendModeColorDodge = 6;
  kCGBlendModeCopy = 17;
  kCGBlendModeDarken = 4;
  kCGBlendModeDestinationAtop = 24;
  kCGBlendModeDestinationIn = 22;
  kCGBlendModeDestinationOut = 23;
  kCGBlendModeDestinationOver = 21;
  kCGBlendModeDifference = 10;
  kCGBlendModeExclusion = 11;
  kCGBlendModeHardLight = 9;
  kCGBlendModeHue = 12;
  kCGBlendModeLighten = 5;
  kCGBlendModeLuminosity = 15;
  kCGBlendModeMultiply = 1;
  kCGBlendModeNormal = 0;
  kCGBlendModeOverlay = 3;
  kCGBlendModePlusDarker = 26;
  kCGBlendModePlusLighter = 27;
  kCGBlendModeSaturation = 13;
  kCGBlendModeScreen = 2;
  kCGBlendModeSoftLight = 8;
  kCGBlendModeSourceAtop = 20;
  kCGBlendModeSourceIn = 18;
  kCGBlendModeSourceOut = 19;
  kCGBlendModeXOR = 25;
  kCGColorSpaceModelCMYK = 2;
  kCGColorSpaceModelDeviceN = 4;
  kCGColorSpaceModelIndexed = 5;
  kCGColorSpaceModelLab = 3;
  kCGColorSpaceModelMonochrome = 0;
  kCGColorSpaceModelPattern = 6;
  kCGColorSpaceModelRGB = 1;
  kCGColorSpaceModelUnknown = -1;
  kCGEncodingFontSpecific = 0;
  kCGEncodingMacRoman = 1;
  kCGErrorApplicationAlreadyRunning = 1025;
  kCGErrorApplicationCanOnlyBeRunInOneSessionAtATime = 1026;
  kCGErrorApplicationIncorrectExecutableFormatFound = 1023;
  kCGErrorApplicationIsLaunching = 1024;
  kCGErrorApplicationNotPermittedToExecute = 1016;
  kCGErrorApplicationRequiresNewerSystem = 1015;
  kCGErrorCannotComplete = 1004;
  kCGErrorClassicApplicationsMustBeLaunchedByClassic = 1027;
  kCGErrorFailure = 1000;
  kCGErrorFirst = 1000;
  kCGErrorForkFailed = 1028;
  kCGErrorIllegalArgument = 1001;
  kCGErrorInvalidConnection = 1002;
  kCGErrorInvalidContext = 1003;
  kCGErrorInvalidOperation = 1010;
  kCGErrorLast = 1029;
  kCGErrorNameTooLong = 1005;
  kCGErrorNoCurrentPoint = 1009;
  kCGErrorNoneAvailable = 1011;
  kCGErrorNotImplemented = 1006;
  kCGErrorRangeCheck = 1007;
  kCGErrorRetryRegistration = 1029;
  kCGErrorSuccess = 0;
  kCGErrorTypeCheck = 1008;
  kCGFontIndexInvalid = 65535;
  kCGFontIndexMax = 65534;
  kCGFontPostScriptFormatType1 = 1;
  kCGFontPostScriptFormatType3 = 3;
  kCGFontPostScriptFormatType42 = 42;
  kCGGlyphMax = 65534;
  kCGGradientDrawsAfterEndLocation = 2;
  kCGGradientDrawsBeforeStartLocation = 1;
  kCGImageAlphaFirst = 4;
  kCGImageAlphaLast = 3;
  kCGImageAlphaNone = 0;
  kCGImageAlphaNoneSkipFirst = 6;
  kCGImageAlphaNoneSkipLast = 5;
  kCGImageAlphaOnly = 7;
  kCGImageAlphaPremultipliedFirst = 2;
  kCGImageAlphaPremultipliedLast = 1;
  kCGInterpolationDefault = 0;
  kCGInterpolationHigh = 3;
  kCGInterpolationLow = 2;
  kCGInterpolationMedium = 4;
  kCGInterpolationNone = 1;
  kCGLineCapButt = 0;
  kCGLineCapRound = 1;
  kCGLineCapSquare = 2;
  kCGLineJoinBevel = 2;
  kCGLineJoinMiter = 0;
  kCGLineJoinRound = 1;
  kCGPDFArtBox = 4;
  kCGPDFBleedBox = 2;
  kCGPDFCropBox = 1;
  kCGPDFMediaBox = 0;
  kCGPDFObjectTypeArray = 7;
  kCGPDFObjectTypeBoolean = 2;
  kCGPDFObjectTypeDictionary = 8;
  kCGPDFObjectTypeInteger = 3;
  kCGPDFObjectTypeName = 5;
  kCGPDFObjectTypeNull = 1;
  kCGPDFObjectTypeReal = 4;
  kCGPDFObjectTypeStream = 9;
  kCGPDFObjectTypeString = 6;
  kCGPDFTrimBox = 3;
  kCGPathEOFill = 1;
  kCGPathEOFillStroke = 4;
  kCGPathElementAddCurveToPoint = 3;
  kCGPathElementAddLineToPoint = 1;
  kCGPathElementAddQuadCurveToPoint = 2;
  kCGPathElementCloseSubpath = 4;
  kCGPathElementMoveToPoint = 0;
  kCGPathFill = 0;
  kCGPathFillStroke = 3;
  kCGPathStroke = 2;
  kCGPatternTilingConstantSpacing = 2;
  kCGPatternTilingConstantSpacingMinimalDistortion = 1;
  kCGPatternTilingNoDistortion = 0;
  kCGRenderingIntentAbsoluteColorimetric = 1;
  kCGRenderingIntentDefault = 0;
  kCGRenderingIntentPerceptual = 3;
  kCGRenderingIntentRelativeColorimetric = 2;
  kCGRenderingIntentSaturation = 4;
  kCGTextClip = 7;
  kCGTextFill = 0;
  kCGTextFillClip = 4;
  kCGTextFillStroke = 2;
  kCGTextFillStrokeClip = 6;
  kCGTextInvisible = 3;
  kCGTextStroke = 1;
  kCGTextStrokeClip = 5;

// ===== Typedefs and structs =====
{$M+}
type
  CGBitmapInfo = UInt32;
  CGBlendMode = UInt32;
  CGButtonCount = UInt32;
  CGCaptureOptions = UInt32;
  CGCharCode = Word;
  CGColorRef = Pointer;
  CGColorRenderingIntent = UInt32;
  CGColorSpaceModel = Integer;
  CGColorSpaceRef = Pointer;
  CGConfigureOption = UInt32;
  CGContextRef = Pointer;
  PCGContextRef = ^CGContextRef;
  CGDataConsumerRef = Pointer;
  CGDataProviderRef = Pointer;
  CGDirectDisplayID = UInt32;
  PCGDirectDisplayID = ^CGDirectDisplayID;
  CGDirectPaletteRef = Pointer;
  CGDisplayBlendFraction = Single;
  CGDisplayChangeSummaryFlags = UInt32;
  CGDisplayConfigRef = Pointer;
  CGDisplayFadeInterval = Single;
  CGDisplayFadeReservationToken = UInt32;
  PCGDisplayFadeReservationToken = ^CGDisplayFadeReservationToken;
  CGDisplayModeRef = Pointer;
  CGDisplayReservationInterval = Single;
  CGError = Integer;
  CGEventField = UInt32;
  CGEventFilterMask = UInt32;
  CGEventFlags = UInt64;
  CGEventMask = UInt64;
  CGEventRef = Pointer;
  CGEventSourceKeyboardType = UInt32;
  CGEventSourceRef = Pointer;
  CGEventSourceStateID = UInt32;
  CGEventSourceSuppressionState = UInt32;
  CGEventSuppressionState = UInt32;
  CGEventTapLocation = UInt32;
  CGEventTapOptions = UInt32;
  CGEventTapPlacement = UInt32;
  CGEventTapProxy = Pointer;
  CGEventTimestamp = UInt64;
  CGEventType = UInt32;
  CGFontIndex = Word;
  CGFontPostScriptFormat = UInt32;
  CGFontRef = Pointer;
  CGFunctionRef = Pointer;
  CGGammaValue = Single;
  PCGGammaValue = ^CGGammaValue;
  CGGlyph = Word;
  PCGGlyph = ^CGGlyph;
  CGGradientDrawingOptions = UInt32;
  CGGradientRef = Pointer;
  CGImageAlphaInfo = UInt32;
  CGImageDestinationRef = Pointer;
  CGImageRef = Pointer;
  CGImageSourceRef = Pointer;
  CGImageSourceStatus = NSInteger;
  CGInterpolationQuality = UInt32;
  CGKeyCode = Word;
  CGLPBufferObj = Pointer;
  PCGLPBufferObj = Pointer;
  CGLContextEnable = UInt32;
  CGLContextObj = Pointer;
  PCGLContextObj = Pointer;
  CGLContextParameter = UInt32;
  CGLError = UInt32;
  CGLGlobalOption = UInt32;
  CGLineCap = UInt32;
  CGLineJoin = UInt32;
  CGLayerRef = Pointer;
  CGLRendererInfoObj = Pointer;
  PCGLRendererInfoObj = Pointer;
  CGLRendererProperty = UInt32;
  CGLShareGroupObj = Pointer;
  CGMouseButton = UInt32;
  CGMutablePathRef = Pointer;
  CGOpenGLDisplayMask = UInt32;
  CGPaletteBlendFraction = Single;
  CGPathDrawingMode = UInt32;
  CGPathElementType = UInt32;
  CGPathRef = Pointer;
  CGPatternRef = Pointer;
  CGPatternTiling = UInt32;
  CGPDFArrayRef = Pointer;
  PCGPDFArrayRef = ^CGPDFArrayRef;
  CGPDFBoolean = Byte;
  PCGPDFBoolean = ^CGPDFBoolean;
  CGPDFBox = UInt32;
  CGPDFContentStreamRef = Pointer;
  CGPDFDataFormat = UInt32;
  PCGPDFDataFormat = ^CGPDFDataFormat;
  CGPDFDictionaryRef = Pointer;
  PCGPDFDictionaryRef = ^CGPDFDictionaryRef;
  CGPDFDocumentRef = Pointer;
  CGPDFInteger = LongInt;
  PCGPDFInteger = ^CGPDFInteger;
  CGPDFObjectRef = Pointer;
  PCGPDFObjectRef = ^CGPDFObjectRef;
  CGPDFObjectType = UInt32;
  CGPDFOperatorTableRef = Pointer;
  CGPDFPageRef = Pointer;
  CGPDFReal = CGFloat;
  PCGPDFReal = ^CGPDFReal;
  CGPDFScannerRef = Pointer;
  CGPDFStreamRef = Pointer;
  PCGPDFStreamRef = ^CGPDFStreamRef;
  CGPDFStringRef = Pointer;
  PCGPDFStringRef = ^CGPDFStringRef;
  CGPSConverterRef = Pointer;
  CGRectCount = UInt32;
  CGRectEdge = UInt32;
  CGRefreshRate = Double;
  CGScreenUpdateOperation = UInt32;
  PCGScreenUpdateOperation = ^CGScreenUpdateOperation;
  CGScrollEventUnit = UInt32;
  CGShadingRef = Pointer;
  CGTextDrawingMode = UInt32;
  CGTextEncoding = UInt32;
  CGWheelCount = UInt32;
  CGWindowID = UInt32;
  CGWindowImageOption = UInt32;
  CGWindowLevel = Integer;
  CGWindowLevelKey = Integer;
  CGWindowListOption = UInt32;

  CGPoint = NSPoint;
  PCGPoint = ^CGPoint;
  CGRect = NSRect;
  PCGRect = ^CGRect;
  CGSize = NSSize;
  PCGSize = ^CGSize;

  CGVector = record
    dx: CGFloat;
    dy: CGFloat;
  end;

  PCGVector = ^CGVector;

  CGAffineTransform = record
    a: CGFloat;
    b: CGFloat;
    c: CGFloat;
    d: CGFloat;
    tx: CGFloat;
    ty: CGFloat;
  public
    constructor Create(const aa, ab, ac, ad, atx, aty: CGFloat);
  end;
  PCGAffineTransform = ^CGAffineTransform;

  // CoreGraphics callbacks

  CGBitmapContextReleaseDataCallback = procedure(releaseInfo: Pointer; data: Pointer); cdecl;

  CGDataConsumerPutBytesCallback = function(info: Pointer; buffer: Pointer; count: size_t): size_t; cdecl;
  CGDataConsumerReleaseInfoCallback = procedure(info: Pointer); cdecl;

  CGDataConsumerCallbacks = record
    putBytes: CGDataConsumerPutBytesCallback;
    releaseConsumer: CGDataConsumerReleaseInfoCallback;
  end;
  PCGDataConsumerCallbacks = ^CGDataConsumerCallbacks;

  CGDataProviderGetBytesCallback = function(info: Pointer; buffer: Pointer; count: size_t): size_t; cdecl;
  CGDataProviderSkipBytesCallback = procedure(info: Pointer; count: size_t); cdecl;
  CGDataProviderRewindCallback = procedure(info: Pointer); cdecl;
  CGDataProviderReleaseInfoCallback = procedure(info: Pointer); cdecl;
  CGDataProviderGetBytePointerCallback  = function(info: Pointer): Pointer; cdecl;
  CGDataProviderReleaseBytePointerCallback = procedure(info: Pointer; ptr: Pointer); cdecl;
  CGDataProviderGetBytesAtPositionCallback = function(info: Pointer; buffer: Pointer; position: off_t; count: size_t): size_t; cdecl;
  CGDataProviderGetBytesAtOffsetCallback = function(info: Pointer; buffer: Pointer; offset: size_t; count: size_t): size_t; cdecl;
  CGDataProviderSkipForwardCallback = function(info: Pointer; count: size_t): off_t; cdecl;
  CGDataProviderReleaseDataCallback = procedure(info: Pointer; data: Pointer; size: size_t); cdecl;

  CGDataProviderCallbacks = record
    getBytes: CGDataProviderGetBytesCallback;
    skipBytes: CGDataProviderSkipBytesCallback;
    rewind: CGDataProviderRewindCallback;
    releaseProvider: CGDataProviderReleaseInfoCallback;
  end;
  PCGDataProviderCallbacks = ^CGDataProviderCallbacks;

  CGDataProviderDirectCallbacks = record
    version: Cardinal;
    getBytePointer: CGDataProviderGetBytePointerCallback;
    releaseBytePointer: CGDataProviderReleaseBytePointerCallback;
    getBytesAtPosition: CGDataProviderGetBytesAtPositionCallback;
    releaseProvider: CGDataProviderReleaseInfoCallback;
  end;
  PCGDataProviderDirectCallbacks = ^CGDataProviderDirectCallbacks;

  CGDataProviderDirectAccessCallbacks = record
    getBytePointer: CGDataProviderGetBytePointerCallback;
    releaseBytePointer: CGDataProviderReleaseBytePointerCallback;
    getBytes: CGDataProviderGetBytesAtOffsetCallback;
    releaseProvider: CGDataProviderReleaseInfoCallback;
  end;
  PCGDataProviderDirectAccessCallbacks = ^CGDataProviderDirectAccessCallbacks;

  CGDataProviderSequentialCallbacks = record
    version: Cardinal;
    getBytes: CGDataProviderGetBytesCallback;
    skipForward: CGDataProviderSkipForwardCallback;
    rewind: CGDataProviderRewindCallback;
    releaseInfo: CGDataProviderReleaseInfoCallback;
  end;
  PCGDataProviderSequentialCallbacks = ^CGDataProviderSequentialCallbacks;

  CGDeviceColor = record
    red: Single;
    green: Single;
    blue: Single;
  end;
  PCGDeviceColor = ^CGDeviceColor;

  CGDeviceByteColor = record
    red: Byte;
    green: Byte;
    blue: Byte;
  end;
  PCGDeviceByteColor = ^CGDeviceByteColor;

  CGFunctionEvaluateCallback = procedure(info: Pointer; inData, outData: PCGFloat); cdecl;
  CGFunctionReleaseInfoCallback = procedure(info: Pointer); cdecl;

  CGFunctionCallbacks = record
    version: Cardinal;
    evaluate: CGFunctionEvaluateCallback;
    releaseInfo: CGFunctionReleaseInfoCallback;
  end;
  PCGFunctionCallbacks = ^CGFunctionCallbacks;

  CGPDFDictionaryApplierFunction = procedure(key: MarshaledAString; value: CGPDFObjectRef; info: Pointer); cdecl;
  CGPDFOperatorCallback = procedure(scanner: CGPDFScannerRef; info: Pointer); cdecl;

  CGPathElement = record
    type_: CGPathElementType;
    points: PCGPoint;
  end;
  PCGPathElement = ^CGPathElement;

  CGPathApplierFunction = procedure(info: Pointer; const element: PCGPathElement); cdecl;

  CGPatternDrawPatternCallback = procedure(info: Pointer; context: CGContextRef); cdecl;
  CGPatternReleaseInfoCallback = procedure(info: Pointer); cdecl;
  CGPatternCallbacks = record
    version: Cardinal;
    drawPattern: CGPatternDrawPatternCallback;
    releaseInfo: CGPatternReleaseInfoCallback;
  end;
  PCGPatternCallbacks = ^CGPatternCallbacks;

// ===== External functions =====

const
  libCoreGraphics = '/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics';

function CGAffineTransformConcat(t1: CGAffineTransform; t2: CGAffineTransform): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformConcat';
function CGAffineTransformEqualToTransform(t1: CGAffineTransform; t2: CGAffineTransform): Boolean; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformEqualToTransform';
function CGAffineTransformInvert(t: CGAffineTransform): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformInvert';
function CGAffineTransformIsIdentity(t: CGAffineTransform): Boolean; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformIsIdentity';
function CGAffineTransformMake(a: CGFloat; b: CGFloat; c: CGFloat; d: CGFloat; tx: CGFloat; ty: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformMake';
function CGAffineTransformMakeRotation(angle: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformMakeRotation';
function CGAffineTransformMakeScale(sx: CGFloat; sy: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformMakeScale';
function CGAffineTransformMakeTranslation(tx: CGFloat; ty: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformMakeTranslation';
function CGAffineTransformRotate(t: CGAffineTransform; angle: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformRotate';
function CGAffineTransformScale(t: CGAffineTransform; sx: CGFloat; sy: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformScale';
function CGAffineTransformTranslate(t: CGAffineTransform; tx: CGFloat; ty: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGAffineTransformTranslate';
function CGBitmapContextCreate(data: Pointer; width: size_t; height: size_t; bitsPerComponent: size_t; bytesPerRow: size_t; space: CGColorSpaceRef; bitmapInfo: CGBitmapInfo): CGContextRef; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextCreate';
function CGBitmapContextCreateImage(context: CGContextRef): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextCreateImage';
function CGBitmapContextCreateWithData(data: Pointer; width: size_t; height: size_t; bitsPerComponent: size_t; bytesPerRow: size_t; space: CGColorSpaceRef; bitmapInfo: CGBitmapInfo; releaseCallback: CGBitmapContextReleaseDataCallback; releaseInfo: Pointer): CGContextRef; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextCreateWithData';
function CGBitmapContextGetAlphaInfo(context: CGContextRef): CGImageAlphaInfo; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetAlphaInfo';
function CGBitmapContextGetBitmapInfo(context: CGContextRef): CGBitmapInfo; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetBitmapInfo';
function CGBitmapContextGetBitsPerComponent(context: CGContextRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetBitsPerComponent';
function CGBitmapContextGetBitsPerPixel(context: CGContextRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetBitsPerPixel';
function CGBitmapContextGetBytesPerRow(context: CGContextRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetBytesPerRow';
function CGBitmapContextGetColorSpace(context: CGContextRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetColorSpace';
function CGBitmapContextGetData(context: CGContextRef): Pointer; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetData';
function CGBitmapContextGetHeight(context: CGContextRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetHeight';
function CGBitmapContextGetWidth(context: CGContextRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGBitmapContextGetWidth';
function CGColorCreate(space: CGColorSpaceRef; components: PCGFloat): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreate';
function CGColorCreateCopy(color: CGColorRef): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreateCopy';
function CGColorCreateCopyWithAlpha(color: CGColorRef; alpha: CGFloat): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreateCopyWithAlpha';
function CGColorCreateGenericCMYK(cyan: CGFloat; magenta: CGFloat; yellow: CGFloat; black: CGFloat; alpha: CGFloat): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreateGenericCMYK';
function CGColorCreateGenericGray(gray: CGFloat; alpha: CGFloat): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreateGenericGray';
function CGColorCreateGenericRGB(red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreateGenericRGB';
function CGColorCreateWithPattern(space: CGColorSpaceRef; pattern: CGPatternRef; components: PCGFloat): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorCreateWithPattern';
function CGColorEqualToColor(color1: CGColorRef; color2: CGColorRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGColorEqualToColor';
function CGColorGetAlpha(color: CGColorRef): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGColorGetAlpha';
function CGColorGetColorSpace(color: CGColorRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorGetColorSpace';
function CGColorGetComponents(color: CGColorRef): PCGFloat; cdecl; external libCoreGraphics name _PU + 'CGColorGetComponents';
function CGColorGetConstantColor(colorName: CFStringRef): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorGetConstantColor';
function CGColorGetNumberOfComponents(color: CGColorRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGColorGetNumberOfComponents';
function CGColorGetPattern(color: CGColorRef): CGPatternRef; cdecl; external libCoreGraphics name _PU + 'CGColorGetPattern';
function CGColorGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGColorGetTypeID';
procedure CGColorRelease(color: CGColorRef); cdecl; external libCoreGraphics name _PU + 'CGColorRelease';
function CGColorRetain(color: CGColorRef): CGColorRef; cdecl; external libCoreGraphics name _PU + 'CGColorRetain';
function CGColorSpaceCopyICCProfile(space: CGColorSpaceRef): CFDataRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCopyICCProfile';
function CGColorSpaceCopyName(space: CGColorSpaceRef): CFStringRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCopyName';
function CGColorSpaceCreateCalibratedGray(whitePoint: PCGFloat; blackPoint: PCGFloat; gamma: CGFloat): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateCalibratedGray';
function CGColorSpaceCreateCalibratedRGB(whitePoint: PCGFloat; blackPoint: PCGFloat; gamma: PCGFloat; matrix: PCGFloat): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateCalibratedRGB';
function CGColorSpaceCreateDeviceCMYK: CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateDeviceCMYK';
function CGColorSpaceCreateDeviceGray: CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateDeviceGray';
function CGColorSpaceCreateDeviceRGB: CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateDeviceRGB';
function CGColorSpaceCreateICCBased(nComponents: size_t; range: PCGFloat; profile: CGDataProviderRef; alternate: CGColorSpaceRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateICCBased';
function CGColorSpaceCreateIndexed(baseSpace: CGColorSpaceRef; lastIndex: size_t; colorTable: PByte): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateIndexed';
function CGColorSpaceCreateLab(whitePoint: PCGFloat; blackPoint: PCGFloat; range: PCGFloat): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateLab';
function CGColorSpaceCreatePattern(baseSpace: CGColorSpaceRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreatePattern';
function CGColorSpaceCreateWithICCProfile(data: CFDataRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateWithICCProfile';
function CGColorSpaceCreateWithName(name: CFStringRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateWithName';
function CGColorSpaceCreateWithPlatformColorSpace(ref: Pointer): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceCreateWithPlatformColorSpace';
function CGColorSpaceGetBaseColorSpace(space: CGColorSpaceRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceGetBaseColorSpace';
procedure CGColorSpaceGetColorTable(space: CGColorSpaceRef; table: PByte); cdecl; external libCoreGraphics name _PU + 'CGColorSpaceGetColorTable';
function CGColorSpaceGetColorTableCount(space: CGColorSpaceRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceGetColorTableCount';
function CGColorSpaceGetModel(space: CGColorSpaceRef): CGColorSpaceModel; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceGetModel';
function CGColorSpaceGetNumberOfComponents(space: CGColorSpaceRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceGetNumberOfComponents';
function CGColorSpaceGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceGetTypeID';
procedure CGColorSpaceRelease(space: CGColorSpaceRef); cdecl; external libCoreGraphics name _PU + 'CGColorSpaceRelease';
function CGColorSpaceRetain(space: CGColorSpaceRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGColorSpaceRetain';
procedure CGContextAddArc(c: CGContextRef; x: CGFloat; y: CGFloat; radius: CGFloat; startAngle: CGFloat; endAngle: CGFloat; clockwise: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextAddArc';
procedure CGContextAddArcToPoint(c: CGContextRef; x1: CGFloat; y1: CGFloat; x2: CGFloat; y2: CGFloat; radius: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextAddArcToPoint';
procedure CGContextAddCurveToPoint(c: CGContextRef; cp1x: CGFloat; cp1y: CGFloat; cp2x: CGFloat; cp2y: CGFloat; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextAddCurveToPoint';
procedure CGContextAddEllipseInRect(context: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextAddEllipseInRect';
procedure CGContextAddLineToPoint(c: CGContextRef; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextAddLineToPoint';
procedure CGContextAddLines(c: CGContextRef; points: PCGPoint; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextAddLines';
procedure CGContextAddPath(context: CGContextRef; path: CGPathRef); cdecl; external libCoreGraphics name _PU + 'CGContextAddPath';
procedure CGContextAddQuadCurveToPoint(c: CGContextRef; cpx: CGFloat; cpy: CGFloat; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextAddQuadCurveToPoint';
procedure CGContextAddRect(c: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextAddRect';
procedure CGContextAddRects(c: CGContextRef; rects: PCGRect; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextAddRects';
procedure CGContextBeginPage(c: CGContextRef; mediaBox: PCGRect); cdecl; external libCoreGraphics name _PU + 'CGContextBeginPage';
procedure CGContextBeginPath(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextBeginPath';
procedure CGContextBeginTransparencyLayer(context: CGContextRef; auxiliaryInfo: CFDictionaryRef); cdecl; external libCoreGraphics name _PU + 'CGContextBeginTransparencyLayer';
procedure CGContextBeginTransparencyLayerWithRect(context: CGContextRef; rect: CGRect; auxiliaryInfo: CFDictionaryRef); cdecl; external libCoreGraphics name _PU + 'CGContextBeginTransparencyLayerWithRect';
procedure CGContextClearRect(c: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextClearRect';
procedure CGContextClip(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextClip';
procedure CGContextClipToMask(c: CGContextRef; rect: CGRect; mask: CGImageRef); cdecl; external libCoreGraphics name _PU + 'CGContextClipToMask';
procedure CGContextClipToRect(c: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextClipToRect';
procedure CGContextClipToRects(c: CGContextRef; rects: PCGRect; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextClipToRects';
procedure CGContextClosePath(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextClosePath';
procedure CGContextConcatCTM(c: CGContextRef; transform: CGAffineTransform); cdecl; external libCoreGraphics name _PU + 'CGContextConcatCTM';
function CGContextConvertPointToDeviceSpace(context: CGContextRef; point: CGPoint): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGContextConvertPointToDeviceSpace';
function CGContextConvertPointToUserSpace(context: CGContextRef; point: CGPoint): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGContextConvertPointToUserSpace';
function CGContextConvertRectToDeviceSpace(context: CGContextRef; rect: CGRect): CGRect; cdecl; external libCoreGraphics name _PU + 'CGContextConvertRectToDeviceSpace';
function CGContextConvertRectToUserSpace(context: CGContextRef; rect: CGRect): CGRect; cdecl; external libCoreGraphics name _PU + 'CGContextConvertRectToUserSpace';
function CGContextConvertSizeToDeviceSpace(context: CGContextRef; size: CGSize): CGSize; cdecl; external libCoreGraphics name _PU + 'CGContextConvertSizeToDeviceSpace';
function CGContextConvertSizeToUserSpace(context: CGContextRef; size: CGSize): CGSize; cdecl; external libCoreGraphics name _PU + 'CGContextConvertSizeToUserSpace';
function CGContextCopyPath(context: CGContextRef): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGContextCopyPath';
procedure CGContextDrawImage(c: CGContextRef; rect: CGRect; image: CGImageRef); cdecl; external libCoreGraphics name _PU + 'CGContextDrawImage';
procedure CGContextDrawLayerAtPoint(context: CGContextRef; point: CGPoint; layer: CGLayerRef); cdecl; external libCoreGraphics name _PU + 'CGContextDrawLayerAtPoint';
procedure CGContextDrawLayerInRect(context: CGContextRef; rect: CGRect; layer: CGLayerRef); cdecl; external libCoreGraphics name _PU + 'CGContextDrawLayerInRect';
procedure CGContextDrawLinearGradient(context: CGContextRef; gradient: CGGradientRef; startPoint: CGPoint; endPoint: CGPoint; options: CGGradientDrawingOptions); cdecl; external libCoreGraphics name _PU + 'CGContextDrawLinearGradient';
procedure CGContextDrawPDFDocument(c: CGContextRef; rect: CGRect; document: CGPDFDocumentRef; page: Integer); cdecl; external libCoreGraphics name _PU + 'CGContextDrawPDFDocument';
procedure CGContextDrawPDFPage(c: CGContextRef; page: CGPDFPageRef); cdecl; external libCoreGraphics name _PU + 'CGContextDrawPDFPage';
procedure CGContextDrawPath(c: CGContextRef; mode: CGPathDrawingMode); cdecl; external libCoreGraphics name _PU + 'CGContextDrawPath';
procedure CGContextDrawRadialGradient(context: CGContextRef; gradient: CGGradientRef; startCenter: CGPoint; startRadius: CGFloat; endCenter: CGPoint; endRadius: CGFloat; options: CGGradientDrawingOptions); cdecl; external libCoreGraphics name _PU + 'CGContextDrawRadialGradient';
procedure CGContextDrawShading(context: CGContextRef; shading: CGShadingRef); cdecl; external libCoreGraphics name _PU + 'CGContextDrawShading';
procedure CGContextDrawTiledImage(c: CGContextRef; rect: CGRect; image: CGImageRef); cdecl; external libCoreGraphics name _PU + 'CGContextDrawTiledImage';
procedure CGContextEOClip(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextEOClip';
procedure CGContextEOFillPath(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextEOFillPath';
procedure CGContextEndPage(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextEndPage';
procedure CGContextEndTransparencyLayer(context: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextEndTransparencyLayer';
procedure CGContextFillEllipseInRect(context: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextFillEllipseInRect';
procedure CGContextFillPath(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextFillPath';
procedure CGContextFillRect(c: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextFillRect';
procedure CGContextFillRects(c: CGContextRef; rects: PCGRect; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextFillRects';
procedure CGContextFlush(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextFlush';
function CGContextGetCTM(c: CGContextRef): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGContextGetCTM';
function CGContextGetClipBoundingBox(c: CGContextRef): CGRect; cdecl; external libCoreGraphics name _PU + 'CGContextGetClipBoundingBox';
function CGContextGetInterpolationQuality(context: CGContextRef): CGInterpolationQuality; cdecl; external libCoreGraphics name _PU + 'CGContextGetInterpolationQuality';
function CGContextGetPathBoundingBox(context: CGContextRef): CGRect; cdecl; external libCoreGraphics name _PU + 'CGContextGetPathBoundingBox';
function CGContextGetPathCurrentPoint(context: CGContextRef): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGContextGetPathCurrentPoint';
function CGContextGetTextMatrix(c: CGContextRef): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGContextGetTextMatrix';
function CGContextGetTextPosition(context: CGContextRef): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGContextGetTextPosition';
function CGContextGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGContextGetTypeID';
function CGContextGetUserSpaceToDeviceSpaceTransform(context: CGContextRef): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGContextGetUserSpaceToDeviceSpaceTransform';
function CGContextIsPathEmpty(context: CGContextRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGContextIsPathEmpty';
procedure CGContextMoveToPoint(c: CGContextRef; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextMoveToPoint';
function CGContextPathContainsPoint(context: CGContextRef; point: CGPoint; mode: CGPathDrawingMode): Boolean; cdecl; external libCoreGraphics name _PU + 'CGContextPathContainsPoint';
procedure CGContextRelease(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextRelease';
procedure CGContextReplacePathWithStrokedPath(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextReplacePathWithStrokedPath';
procedure CGContextRestoreGState(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextRestoreGState';
function CGContextRetain(c: CGContextRef): CGContextRef; cdecl; external libCoreGraphics name _PU + 'CGContextRetain';
procedure CGContextRotateCTM(c: CGContextRef; angle: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextRotateCTM';
procedure CGContextSaveGState(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextSaveGState';
procedure CGContextScaleCTM(c: CGContextRef; sx: CGFloat; sy: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextScaleCTM';
procedure CGContextSelectFont(c: CGContextRef; name: MarshaledAString; size: CGFloat; textEncoding: CGTextEncoding); cdecl; external libCoreGraphics name _PU + 'CGContextSelectFont';
procedure CGContextSetAllowsAntialiasing(context: CGContextRef; allowsAntialiasing: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetAllowsAntialiasing';
procedure CGContextSetAllowsFontSmoothing(context: CGContextRef; allowsFontSmoothing: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetAllowsFontSmoothing';
procedure CGContextSetAllowsFontSubpixelPositioning(context: CGContextRef; allowsFontSubpixelPositioning: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetAllowsFontSubpixelPositioning';
procedure CGContextSetAllowsFontSubpixelQuantization(context: CGContextRef; allowsFontSubpixelQuantization: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetAllowsFontSubpixelQuantization';
procedure CGContextSetAlpha(c: CGContextRef; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetAlpha';
procedure CGContextSetBlendMode(context: CGContextRef; mode: CGBlendMode); cdecl; external libCoreGraphics name _PU + 'CGContextSetBlendMode';
procedure CGContextSetCMYKFillColor(context: CGContextRef; cyan: CGFloat; magenta: CGFloat; yellow: CGFloat; black: CGFloat; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetCMYKFillColor';
procedure CGContextSetCMYKStrokeColor(context: CGContextRef; cyan: CGFloat; magenta: CGFloat; yellow: CGFloat; black: CGFloat; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetCMYKStrokeColor';
procedure CGContextSetCharacterSpacing(context: CGContextRef; spacing: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetCharacterSpacing';
procedure CGContextSetFillColor(context: CGContextRef; components: PCGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetFillColor';
procedure CGContextSetFillColorSpace(context: CGContextRef; space: CGColorSpaceRef); cdecl; external libCoreGraphics name _PU + 'CGContextSetFillColorSpace';
procedure CGContextSetFillColorWithColor(c: CGContextRef; color: CGColorRef); cdecl; external libCoreGraphics name _PU + 'CGContextSetFillColorWithColor';
procedure CGContextSetFillPattern(context: CGContextRef; pattern: CGPatternRef; components: PCGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetFillPattern';
procedure CGContextSetFlatness(c: CGContextRef; flatness: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetFlatness';
procedure CGContextSetFont(c: CGContextRef; font: CGFontRef); cdecl; external libCoreGraphics name _PU + 'CGContextSetFont';
procedure CGContextSetFontSize(c: CGContextRef; size: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetFontSize';
procedure CGContextSetGrayFillColor(context: CGContextRef; gray: CGFloat; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetGrayFillColor';
procedure CGContextSetGrayStrokeColor(context: CGContextRef; gray: CGFloat; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetGrayStrokeColor';
procedure CGContextSetInterpolationQuality(context: CGContextRef; quality: CGInterpolationQuality); cdecl; external libCoreGraphics name _PU + 'CGContextSetInterpolationQuality';
procedure CGContextSetLineCap(c: CGContextRef; cap: CGLineCap); cdecl; external libCoreGraphics name _PU + 'CGContextSetLineCap';
procedure CGContextSetLineDash(c: CGContextRef; phase: CGFloat; lengths: PCGFloat; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextSetLineDash';
procedure CGContextSetLineJoin(c: CGContextRef; join: CGLineJoin); cdecl; external libCoreGraphics name _PU + 'CGContextSetLineJoin';
procedure CGContextSetLineWidth(c: CGContextRef; width: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetLineWidth';
procedure CGContextSetMiterLimit(c: CGContextRef; limit: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetMiterLimit';
procedure CGContextSetPatternPhase(context: CGContextRef; phase: CGSize); cdecl; external libCoreGraphics name _PU + 'CGContextSetPatternPhase';
procedure CGContextSetRGBFillColor(context: CGContextRef; red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetRGBFillColor';
procedure CGContextSetRGBStrokeColor(context: CGContextRef; red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetRGBStrokeColor';
procedure CGContextSetRenderingIntent(context: CGContextRef; intent: CGColorRenderingIntent); cdecl; external libCoreGraphics name _PU + 'CGContextSetRenderingIntent';
procedure CGContextSetShadow(context: CGContextRef; offset: CGSize; blur: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetShadow';
procedure CGContextSetShadowWithColor(context: CGContextRef; offset: CGSize; blur: CGFloat; color: CGColorRef); cdecl; external libCoreGraphics name _PU + 'CGContextSetShadowWithColor';
procedure CGContextSetShouldAntialias(context: CGContextRef; shouldAntialias: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetShouldAntialias';
procedure CGContextSetShouldSmoothFonts(context: CGContextRef; shouldSmoothFonts: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetShouldSmoothFonts';
procedure CGContextSetShouldSubpixelPositionFonts(context: CGContextRef; shouldSubpixelPositionFonts: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetShouldSubpixelPositionFonts';
procedure CGContextSetShouldSubpixelQuantizeFonts(context: CGContextRef; shouldSubpixelQuantizeFonts: Boolean); cdecl; external libCoreGraphics name _PU + 'CGContextSetShouldSubpixelQuantizeFonts';
procedure CGContextSetStrokeColor(context: CGContextRef; components: PCGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetStrokeColor';
procedure CGContextSetStrokeColorSpace(context: CGContextRef; space: CGColorSpaceRef); cdecl; external libCoreGraphics name _PU + 'CGContextSetStrokeColorSpace';
procedure CGContextSetStrokeColorWithColor(c: CGContextRef; color: CGColorRef); cdecl; external libCoreGraphics name _PU + 'CGContextSetStrokeColorWithColor';
procedure CGContextSetStrokePattern(context: CGContextRef; pattern: CGPatternRef; components: PCGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetStrokePattern';
procedure CGContextSetTextDrawingMode(c: CGContextRef; mode: CGTextDrawingMode); cdecl; external libCoreGraphics name _PU + 'CGContextSetTextDrawingMode';
procedure CGContextSetTextMatrix(c: CGContextRef; t: CGAffineTransform); cdecl; external libCoreGraphics name _PU + 'CGContextSetTextMatrix';
procedure CGContextSetTextPosition(c: CGContextRef; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextSetTextPosition';
procedure CGContextShowGlyphs(c: CGContextRef; g: PCGGlyph; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextShowGlyphs';
procedure CGContextShowGlyphsAtPoint(context: CGContextRef; x: CGFloat; y: CGFloat; glyphs: PCGGlyph; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextShowGlyphsAtPoint';
procedure CGContextShowGlyphsAtPositions(context: CGContextRef; glyphs: PCGGlyph; positions: PCGPoint; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextShowGlyphsAtPositions';
procedure CGContextShowGlyphsWithAdvances(c: CGContextRef; glyphs: PCGGlyph; advances: PCGSize; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextShowGlyphsWithAdvances';
procedure CGContextShowText(c: CGContextRef; string_: MarshaledAString; length: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextShowText';
procedure CGContextShowTextAtPoint(c: CGContextRef; x: CGFloat; y: CGFloat; string_: MarshaledAString; length: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextShowTextAtPoint';
procedure CGContextStrokeEllipseInRect(context: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextStrokeEllipseInRect';
procedure CGContextStrokeLineSegments(c: CGContextRef; points: PCGPoint; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGContextStrokeLineSegments';
procedure CGContextStrokePath(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextStrokePath';
procedure CGContextStrokeRect(c: CGContextRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGContextStrokeRect';
procedure CGContextStrokeRectWithWidth(c: CGContextRef; rect: CGRect; width: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextStrokeRectWithWidth';
procedure CGContextSynchronize(c: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGContextSynchronize';
procedure CGContextTranslateCTM(c: CGContextRef; tx: CGFloat; ty: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGContextTranslateCTM';
function CGDataConsumerCreate(info: Pointer; callbacks: PCGDataConsumerCallbacks): CGDataConsumerRef; cdecl; external libCoreGraphics name _PU + 'CGDataConsumerCreate';
function CGDataConsumerCreateWithCFData(data: CFMutableDataRef): CGDataConsumerRef; cdecl; external libCoreGraphics name _PU + 'CGDataConsumerCreateWithCFData';
function CGDataConsumerCreateWithURL(url: CFURLRef): CGDataConsumerRef; cdecl; external libCoreGraphics name _PU + 'CGDataConsumerCreateWithURL';
function CGDataConsumerGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGDataConsumerGetTypeID';
procedure CGDataConsumerRelease(consumer: CGDataConsumerRef); cdecl; external libCoreGraphics name _PU + 'CGDataConsumerRelease';
function CGDataConsumerRetain(consumer: CGDataConsumerRef): CGDataConsumerRef; cdecl; external libCoreGraphics name _PU + 'CGDataConsumerRetain';
function CGDataProviderCopyData(provider: CGDataProviderRef): CFDataRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCopyData';
function CGDataProviderCreate(info: Pointer; callbacks: PCGDataProviderCallbacks): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreate';
function CGDataProviderCreateDirect(info: Pointer; size: off_t; callbacks: PCGDataProviderDirectCallbacks): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateDirect';
function CGDataProviderCreateDirectAccess(info: Pointer; size: size_t; callbacks: PCGDataProviderDirectAccessCallbacks): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateDirectAccess';
function CGDataProviderCreateSequential(info: Pointer; callbacks: PCGDataProviderSequentialCallbacks): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateSequential';
function CGDataProviderCreateWithCFData(data: CFDataRef): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateWithCFData';
function CGDataProviderCreateWithData(info: Pointer; data: Pointer; size: size_t; releaseData: CGDataProviderReleaseDataCallback): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateWithData';
function CGDataProviderCreateWithFilename(filename: MarshaledAString): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateWithFilename';
function CGDataProviderCreateWithURL(url: CFURLRef): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderCreateWithURL';
function CGDataProviderGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGDataProviderGetTypeID';
procedure CGDataProviderRelease(provider: CGDataProviderRef); cdecl; external libCoreGraphics name _PU + 'CGDataProviderRelease';
function CGDataProviderRetain(provider: CGDataProviderRef): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGDataProviderRetain';
function CGFontCanCreatePostScriptSubset(font: CGFontRef; format: CGFontPostScriptFormat): Boolean; cdecl; external libCoreGraphics name _PU + 'CGFontCanCreatePostScriptSubset';
function CGFontCopyFullName(font: CGFontRef): CFStringRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyFullName';
function CGFontCopyGlyphNameForGlyph(font: CGFontRef; glyph: CGGlyph): CFStringRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyGlyphNameForGlyph';
function CGFontCopyPostScriptName(font: CGFontRef): CFStringRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyPostScriptName';
function CGFontCopyTableForTag(font: CGFontRef; tag: UInt32): CFDataRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyTableForTag';
function CGFontCopyTableTags(font: CGFontRef): CFArrayRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyTableTags';
function CGFontCopyVariationAxes(font: CGFontRef): CFArrayRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyVariationAxes';
function CGFontCopyVariations(font: CGFontRef): CFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGFontCopyVariations';
function CGFontCreateCopyWithVariations(font: CGFontRef; variations: CFDictionaryRef): CGFontRef; cdecl; external libCoreGraphics name _PU + 'CGFontCreateCopyWithVariations';
function CGFontCreatePostScriptEncoding(font: CGFontRef; encoding: PCGGlyph): CFDataRef; cdecl; external libCoreGraphics name _PU + 'CGFontCreatePostScriptEncoding';
function CGFontCreatePostScriptSubset(font: CGFontRef; subsetName: CFStringRef; format: CGFontPostScriptFormat; glyphs: PCGGlyph; count: size_t; encoding: PCGGlyph): CFDataRef; cdecl; external libCoreGraphics name _PU + 'CGFontCreatePostScriptSubset';
function CGFontCreateWithDataProvider(provider: CGDataProviderRef): CGFontRef; cdecl; external libCoreGraphics name _PU + 'CGFontCreateWithDataProvider';
function CGFontCreateWithFontName(name: CFStringRef): CGFontRef; cdecl; external libCoreGraphics name _PU + 'CGFontCreateWithFontName';
function CGFontCreateWithPlatformFont(platformFontReference: Pointer): CGFontRef; cdecl; external libCoreGraphics name _PU + 'CGFontCreateWithPlatformFont';
function CGFontGetAscent(font: CGFontRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGFontGetAscent';
function CGFontGetCapHeight(font: CGFontRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGFontGetCapHeight';
function CGFontGetDescent(font: CGFontRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGFontGetDescent';
function CGFontGetFontBBox(font: CGFontRef): CGRect; cdecl; external libCoreGraphics name _PU + 'CGFontGetFontBBox';
function CGFontGetGlyphAdvances(font: CGFontRef; glyphs: PCGGlyph; count: size_t; advances: PInteger): Boolean; cdecl; external libCoreGraphics name _PU + 'CGFontGetGlyphAdvances';
function CGFontGetGlyphBBoxes(font: CGFontRef; glyphs: PCGGlyph; count: size_t; bboxes: PCGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGFontGetGlyphBBoxes';
function CGFontGetGlyphWithGlyphName(font: CGFontRef; name: CFStringRef): CGGlyph; cdecl; external libCoreGraphics name _PU + 'CGFontGetGlyphWithGlyphName';
function CGFontGetItalicAngle(font: CGFontRef): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGFontGetItalicAngle';
function CGFontGetLeading(font: CGFontRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGFontGetLeading';
function CGFontGetNumberOfGlyphs(font: CGFontRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGFontGetNumberOfGlyphs';
function CGFontGetStemV(font: CGFontRef): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGFontGetStemV';
function CGFontGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGFontGetTypeID';
function CGFontGetUnitsPerEm(font: CGFontRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGFontGetUnitsPerEm';
function CGFontGetXHeight(font: CGFontRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGFontGetXHeight';
procedure CGFontRelease(font: CGFontRef); cdecl; external libCoreGraphics name _PU + 'CGFontRelease';
function CGFontRetain(font: CGFontRef): CGFontRef; cdecl; external libCoreGraphics name _PU + 'CGFontRetain';
function CGFunctionCreate(info: Pointer; domainDimension: size_t; domain: PCGFloat; rangeDimension: size_t; range: PCGFloat; callbacks: PCGFunctionCallbacks): CGFunctionRef; cdecl; external libCoreGraphics name _PU + 'CGFunctionCreate';
function CGFunctionGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGFunctionGetTypeID';
procedure CGFunctionRelease(function_: CGFunctionRef); cdecl; external libCoreGraphics name _PU + 'CGFunctionRelease';
function CGFunctionRetain(function_: CGFunctionRef): CGFunctionRef; cdecl; external libCoreGraphics name _PU + 'CGFunctionRetain';
function CGGradientCreateWithColorComponents(space: CGColorSpaceRef; components: PCGFloat; locations: PCGFloat; count: size_t): CGGradientRef; cdecl; external libCoreGraphics name _PU + 'CGGradientCreateWithColorComponents';
function CGGradientCreateWithColors(space: CGColorSpaceRef; colors: CFArrayRef; locations: PCGFloat): CGGradientRef; cdecl; external libCoreGraphics name _PU + 'CGGradientCreateWithColors';
function CGGradientGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGGradientGetTypeID';
procedure CGGradientRelease(gradient: CGGradientRef); cdecl; external libCoreGraphics name _PU + 'CGGradientRelease';
function CGGradientRetain(gradient: CGGradientRef): CGGradientRef; cdecl; external libCoreGraphics name _PU + 'CGGradientRetain';
function CGImageCreate(width: size_t; height: size_t; bitsPerComponent: size_t; bitsPerPixel: size_t; bytesPerRow: size_t; space: CGColorSpaceRef; bitmapInfo: CGBitmapInfo; provider: CGDataProviderRef; decode: PCGFloat; shouldInterpolate: Boolean; intent: CGColorRenderingIntent): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreate';
function CGImageCreateCopy(image: CGImageRef): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateCopy';
function CGImageCreateCopyWithColorSpace(image: CGImageRef; space: CGColorSpaceRef): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateCopyWithColorSpace';
function CGImageCreateWithImageInRect(image: CGImageRef; rect: CGRect): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateWithImageInRect';
function CGImageCreateWithJPEGDataProvider(source: CGDataProviderRef; decode: PCGFloat; shouldInterpolate: Boolean; intent: CGColorRenderingIntent): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateWithJPEGDataProvider';
function CGImageCreateWithMask(image: CGImageRef; mask: CGImageRef): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateWithMask';
function CGImageCreateWithMaskingColors(image: CGImageRef; components: PCGFloat): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateWithMaskingColors';
function CGImageCreateWithPNGDataProvider(source: CGDataProviderRef; decode: PCGFloat; shouldInterpolate: Boolean; intent: CGColorRenderingIntent): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageCreateWithPNGDataProvider';
function CGImageGetAlphaInfo(image: CGImageRef): CGImageAlphaInfo; cdecl; external libCoreGraphics name _PU + 'CGImageGetAlphaInfo';
function CGImageGetBitmapInfo(image: CGImageRef): CGBitmapInfo; cdecl; external libCoreGraphics name _PU + 'CGImageGetBitmapInfo';
function CGImageGetBitsPerComponent(image: CGImageRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGImageGetBitsPerComponent';
function CGImageGetBitsPerPixel(image: CGImageRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGImageGetBitsPerPixel';
function CGImageGetBytesPerRow(image: CGImageRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGImageGetBytesPerRow';
function CGImageGetColorSpace(image: CGImageRef): CGColorSpaceRef; cdecl; external libCoreGraphics name _PU + 'CGImageGetColorSpace';
function CGImageGetDataProvider(image: CGImageRef): CGDataProviderRef; cdecl; external libCoreGraphics name _PU + 'CGImageGetDataProvider';
function CGImageGetDecode(image: CGImageRef): PCGFloat; cdecl; external libCoreGraphics name _PU + 'CGImageGetDecode';
function CGImageGetHeight(image: CGImageRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGImageGetHeight';
function CGImageGetRenderingIntent(image: CGImageRef): CGColorRenderingIntent; cdecl; external libCoreGraphics name _PU + 'CGImageGetRenderingIntent';
function CGImageGetShouldInterpolate(image: CGImageRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGImageGetShouldInterpolate';
function CGImageGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGImageGetTypeID';
function CGImageGetWidth(image: CGImageRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGImageGetWidth';
function CGImageIsMask(image: CGImageRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGImageIsMask';
function CGImageMaskCreate(width: size_t; height: size_t; bitsPerComponent: size_t; bitsPerPixel: size_t; bytesPerRow: size_t; provider: CGDataProviderRef; decode: PCGFloat; shouldInterpolate: Boolean): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageMaskCreate';
procedure CGImageRelease(image: CGImageRef); cdecl; external libCoreGraphics name _PU + 'CGImageRelease';
function CGImageRetain(image: CGImageRef): CGImageRef; cdecl; external libCoreGraphics name _PU + 'CGImageRetain';
function CGLayerCreateWithContext(context: CGContextRef; size: CGSize; auxiliaryInfo: CFDictionaryRef): CGLayerRef; cdecl; external libCoreGraphics name _PU + 'CGLayerCreateWithContext';
function CGLayerGetContext(layer: CGLayerRef): CGContextRef; cdecl; external libCoreGraphics name _PU + 'CGLayerGetContext';
function CGLayerGetSize(layer: CGLayerRef): CGSize; cdecl; external libCoreGraphics name _PU + 'CGLayerGetSize';
function CGLayerGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGLayerGetTypeID';
procedure CGLayerRelease(layer: CGLayerRef); cdecl; external libCoreGraphics name _PU + 'CGLayerRelease';
function CGLayerRetain(layer: CGLayerRef): CGLayerRef; cdecl; external libCoreGraphics name _PU + 'CGLayerRetain';
function CGPDFArrayGetArray(array_: CGPDFArrayRef; index: size_t; value: PCGPDFArrayRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetArray';
function CGPDFArrayGetBoolean(array_: CGPDFArrayRef; index: size_t; value: PCGPDFBoolean): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetBoolean';
function CGPDFArrayGetCount(array_: CGPDFArrayRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetCount';
function CGPDFArrayGetDictionary(array_: CGPDFArrayRef; index: size_t; value: PCGPDFDictionaryRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetDictionary';
function CGPDFArrayGetInteger(array_: CGPDFArrayRef; index: size_t; value: PCGPDFInteger): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetInteger';
function CGPDFArrayGetName(array_: CGPDFArrayRef; index: size_t; value: Pchar): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetName';
function CGPDFArrayGetNull(array_: CGPDFArrayRef; index: size_t): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetNull';
function CGPDFArrayGetNumber(array_: CGPDFArrayRef; index: size_t; value: PCGPDFReal): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetNumber';
function CGPDFArrayGetObject(array_: CGPDFArrayRef; index: size_t; value: PCGPDFObjectRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetObject';
function CGPDFArrayGetStream(array_: CGPDFArrayRef; index: size_t; value: PCGPDFStreamRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetStream';
function CGPDFArrayGetString(array_: CGPDFArrayRef; index: size_t; value: PCGPDFStringRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFArrayGetString';
function CGPDFContentStreamCreateWithPage(page: CGPDFPageRef): CGPDFContentStreamRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContentStreamCreateWithPage';
function CGPDFContentStreamCreateWithStream(stream: CGPDFStreamRef; streamResources: CGPDFDictionaryRef; parent: CGPDFContentStreamRef): CGPDFContentStreamRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContentStreamCreateWithStream';
function CGPDFContentStreamGetResource(cs: CGPDFContentStreamRef; category: MarshaledAString; name: MarshaledAString): CGPDFObjectRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContentStreamGetResource';
function CGPDFContentStreamGetStreams(cs: CGPDFContentStreamRef): CFArrayRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContentStreamGetStreams';
procedure CGPDFContentStreamRelease(cs: CGPDFContentStreamRef); cdecl; external libCoreGraphics name _PU + 'CGPDFContentStreamRelease';
function CGPDFContentStreamRetain(cs: CGPDFContentStreamRef): CGPDFContentStreamRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContentStreamRetain';
procedure CGPDFContextAddDestinationAtPoint(context: CGContextRef; name: CFStringRef; point: CGPoint); cdecl; external libCoreGraphics name _PU + 'CGPDFContextAddDestinationAtPoint';
procedure CGPDFContextAddDocumentMetadata(context: CGContextRef; metadata: CFDataRef); cdecl; external libCoreGraphics name _PU + 'CGPDFContextAddDocumentMetadata';
procedure CGPDFContextBeginPage(context: CGContextRef; pageInfo: CFDictionaryRef); cdecl; external libCoreGraphics name _PU + 'CGPDFContextBeginPage';
procedure CGPDFContextClose(context: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGPDFContextClose';
function CGPDFContextCreate(consumer: CGDataConsumerRef; mediaBox: PCGRect; auxiliaryInfo: CFDictionaryRef): CGContextRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContextCreate';
function CGPDFContextCreateWithURL(url: CFURLRef; mediaBox: PCGRect; auxiliaryInfo: CFDictionaryRef): CGContextRef; cdecl; external libCoreGraphics name _PU + 'CGPDFContextCreateWithURL';
procedure CGPDFContextEndPage(context: CGContextRef); cdecl; external libCoreGraphics name _PU + 'CGPDFContextEndPage';
procedure CGPDFContextSetDestinationForRect(context: CGContextRef; name: CFStringRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGPDFContextSetDestinationForRect';
procedure CGPDFContextSetURLForRect(context: CGContextRef; url: CFURLRef; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGPDFContextSetURLForRect';
procedure CGPDFDictionaryApplyFunction(dict: CGPDFDictionaryRef; function_: CGPDFDictionaryApplierFunction; info: Pointer); cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryApplyFunction';
function CGPDFDictionaryGetArray(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFArrayRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetArray';
function CGPDFDictionaryGetBoolean(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFBoolean): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetBoolean';
function CGPDFDictionaryGetCount(dict: CGPDFDictionaryRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetCount';
function CGPDFDictionaryGetDictionary(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFDictionaryRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetDictionary';
function CGPDFDictionaryGetInteger(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFInteger): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetInteger';
function CGPDFDictionaryGetName(dict: CGPDFDictionaryRef; key: MarshaledAString; value: Pchar): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetName';
function CGPDFDictionaryGetNumber(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFReal): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetNumber';
function CGPDFDictionaryGetObject(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFObjectRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetObject';
function CGPDFDictionaryGetStream(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFStreamRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetStream';
function CGPDFDictionaryGetString(dict: CGPDFDictionaryRef; key: MarshaledAString; value: PCGPDFStringRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDictionaryGetString';
function CGPDFDocumentAllowsCopying(document: CGPDFDocumentRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentAllowsCopying';
function CGPDFDocumentAllowsPrinting(document: CGPDFDocumentRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentAllowsPrinting';
function CGPDFDocumentCreateWithProvider(provider: CGDataProviderRef): CGPDFDocumentRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentCreateWithProvider';
function CGPDFDocumentCreateWithURL(url: CFURLRef): CGPDFDocumentRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentCreateWithURL';
function CGPDFDocumentGetArtBox(document: CGPDFDocumentRef; page: Integer): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetArtBox';
function CGPDFDocumentGetBleedBox(document: CGPDFDocumentRef; page: Integer): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetBleedBox';
function CGPDFDocumentGetCatalog(document: CGPDFDocumentRef): CGPDFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetCatalog';
function CGPDFDocumentGetCropBox(document: CGPDFDocumentRef; page: Integer): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetCropBox';
function CGPDFDocumentGetID(document: CGPDFDocumentRef): CGPDFArrayRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetID';
function CGPDFDocumentGetInfo(document: CGPDFDocumentRef): CGPDFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetInfo';
function CGPDFDocumentGetMediaBox(document: CGPDFDocumentRef; page: Integer): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetMediaBox';
function CGPDFDocumentGetNumberOfPages(document: CGPDFDocumentRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetNumberOfPages';
function CGPDFDocumentGetPage(document: CGPDFDocumentRef; pageNumber: size_t): CGPDFPageRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetPage';
function CGPDFDocumentGetRotationAngle(document: CGPDFDocumentRef; page: Integer): Integer; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetRotationAngle';
function CGPDFDocumentGetTrimBox(document: CGPDFDocumentRef; page: Integer): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetTrimBox';
function CGPDFDocumentGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetTypeID';
procedure CGPDFDocumentGetVersion(document: CGPDFDocumentRef; majorVersion: PInteger; minorVersion: PInteger); cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentGetVersion';
function CGPDFDocumentIsEncrypted(document: CGPDFDocumentRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentIsEncrypted';
function CGPDFDocumentIsUnlocked(document: CGPDFDocumentRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentIsUnlocked';
procedure CGPDFDocumentRelease(document: CGPDFDocumentRef); cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentRelease';
function CGPDFDocumentRetain(document: CGPDFDocumentRef): CGPDFDocumentRef; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentRetain';
function CGPDFDocumentUnlockWithPassword(document: CGPDFDocumentRef; password: MarshaledAString): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFDocumentUnlockWithPassword';
function CGPDFObjectGetType(object_: CGPDFObjectRef): CGPDFObjectType; cdecl; external libCoreGraphics name _PU + 'CGPDFObjectGetType';
function CGPDFObjectGetValue(object_: CGPDFObjectRef; type_: CGPDFObjectType; value: Pointer): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFObjectGetValue';
function CGPDFOperatorTableCreate: CGPDFOperatorTableRef; cdecl; external libCoreGraphics name _PU + 'CGPDFOperatorTableCreate';
procedure CGPDFOperatorTableRelease(table: CGPDFOperatorTableRef); cdecl; external libCoreGraphics name _PU + 'CGPDFOperatorTableRelease';
function CGPDFOperatorTableRetain(table: CGPDFOperatorTableRef): CGPDFOperatorTableRef; cdecl; external libCoreGraphics name _PU + 'CGPDFOperatorTableRetain';
procedure CGPDFOperatorTableSetCallback(table: CGPDFOperatorTableRef; name: MarshaledAString; callback: CGPDFOperatorCallback); cdecl; external libCoreGraphics name _PU + 'CGPDFOperatorTableSetCallback';
function CGPDFPageGetBoxRect(page: CGPDFPageRef; box: CGPDFBox): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetBoxRect';
function CGPDFPageGetDictionary(page: CGPDFPageRef): CGPDFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetDictionary';
function CGPDFPageGetDocument(page: CGPDFPageRef): CGPDFDocumentRef; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetDocument';
function CGPDFPageGetDrawingTransform(page: CGPDFPageRef; box: CGPDFBox; rect: CGRect; rotate: Integer; preserveAspectRatio: Boolean): CGAffineTransform; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetDrawingTransform';
function CGPDFPageGetPageNumber(page: CGPDFPageRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetPageNumber';
function CGPDFPageGetRotationAngle(page: CGPDFPageRef): Integer; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetRotationAngle';
function CGPDFPageGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGPDFPageGetTypeID';
procedure CGPDFPageRelease(page: CGPDFPageRef); cdecl; external libCoreGraphics name _PU + 'CGPDFPageRelease';
function CGPDFPageRetain(page: CGPDFPageRef): CGPDFPageRef; cdecl; external libCoreGraphics name _PU + 'CGPDFPageRetain';
function CGPDFScannerCreate(cs: CGPDFContentStreamRef; table: CGPDFOperatorTableRef; info: Pointer): CGPDFScannerRef; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerCreate';
function CGPDFScannerGetContentStream(scanner: CGPDFScannerRef): CGPDFContentStreamRef; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerGetContentStream';
function CGPDFScannerPopArray(scanner: CGPDFScannerRef; value: PCGPDFArrayRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopArray';
function CGPDFScannerPopBoolean(scanner: CGPDFScannerRef; value: PCGPDFBoolean): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopBoolean';
function CGPDFScannerPopDictionary(scanner: CGPDFScannerRef; value: PCGPDFDictionaryRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopDictionary';
function CGPDFScannerPopInteger(scanner: CGPDFScannerRef; value: PCGPDFInteger): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopInteger';
function CGPDFScannerPopName(scanner: CGPDFScannerRef; value: Pchar): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopName';
function CGPDFScannerPopNumber(scanner: CGPDFScannerRef; value: PCGPDFReal): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopNumber';
function CGPDFScannerPopObject(scanner: CGPDFScannerRef; value: PCGPDFObjectRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopObject';
function CGPDFScannerPopStream(scanner: CGPDFScannerRef; value: PCGPDFStreamRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopStream';
function CGPDFScannerPopString(scanner: CGPDFScannerRef; value: PCGPDFStringRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerPopString';
procedure CGPDFScannerRelease(scanner: CGPDFScannerRef); cdecl; external libCoreGraphics name _PU + 'CGPDFScannerRelease';
function CGPDFScannerRetain(scanner: CGPDFScannerRef): CGPDFScannerRef; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerRetain';
function CGPDFScannerScan(scanner: CGPDFScannerRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPDFScannerScan';
function CGPDFStreamCopyData(stream: CGPDFStreamRef; format: PCGPDFDataFormat): CFDataRef; cdecl; external libCoreGraphics name _PU + 'CGPDFStreamCopyData';
function CGPDFStreamGetDictionary(stream: CGPDFStreamRef): CGPDFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGPDFStreamGetDictionary';
function CGPDFStringCopyDate(string_: CGPDFStringRef): CFDateRef; cdecl; external libCoreGraphics name _PU + 'CGPDFStringCopyDate';
function CGPDFStringCopyTextString(string_: CGPDFStringRef): CFStringRef; cdecl; external libCoreGraphics name _PU + 'CGPDFStringCopyTextString';
function CGPDFStringGetBytePtr(string_: CGPDFStringRef): PByte; cdecl; external libCoreGraphics name _PU + 'CGPDFStringGetBytePtr';
function CGPDFStringGetLength(string_: CGPDFStringRef): size_t; cdecl; external libCoreGraphics name _PU + 'CGPDFStringGetLength';
procedure CGPathAddArc(path: CGMutablePathRef; m: PCGAffineTransform; x: CGFloat; y: CGFloat; radius: CGFloat; startAngle: CGFloat; endAngle: CGFloat; clockwise: Integer); cdecl; external libCoreGraphics name _PU + 'CGPathAddArc';
procedure CGPathAddArcToPoint(path: CGMutablePathRef; m: PCGAffineTransform; x1: CGFloat; y1: CGFloat; x2: CGFloat; y2: CGFloat; radius: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGPathAddArcToPoint';
procedure CGPathAddCurveToPoint(path: CGMutablePathRef; m: PCGAffineTransform; cp1x: CGFloat; cp1y: CGFloat; cp2x: CGFloat; cp2y: CGFloat; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGPathAddCurveToPoint';
procedure CGPathAddEllipseInRect(path: CGMutablePathRef; m: PCGAffineTransform; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGPathAddEllipseInRect';
procedure CGPathAddLineToPoint(path: CGMutablePathRef; m: PCGAffineTransform; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGPathAddLineToPoint';
procedure CGPathAddLines(path: CGMutablePathRef; m: PCGAffineTransform; points: PCGPoint; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGPathAddLines';
procedure CGPathAddPath(path1: CGMutablePathRef; m: PCGAffineTransform; path2: CGPathRef); cdecl; external libCoreGraphics name _PU + 'CGPathAddPath';
procedure CGPathAddQuadCurveToPoint(path: CGMutablePathRef; m: PCGAffineTransform; cpx: CGFloat; cpy: CGFloat; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGPathAddQuadCurveToPoint';
procedure CGPathAddRect(path: CGMutablePathRef; m: PCGAffineTransform; rect: CGRect); cdecl; external libCoreGraphics name _PU + 'CGPathAddRect';
procedure CGPathAddRects(path: CGMutablePathRef; m: PCGAffineTransform; rects: PCGRect; count: size_t); cdecl; external libCoreGraphics name _PU + 'CGPathAddRects';
procedure CGPathAddRelativeArc(path: CGMutablePathRef; matrix: PCGAffineTransform; x: CGFloat; y: CGFloat; radius: CGFloat; startAngle: CGFloat; delta: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGPathAddRelativeArc';
procedure CGPathApply(path: CGPathRef; info: Pointer; function_: CGPathApplierFunction); cdecl; external libCoreGraphics name _PU + 'CGPathApply';
procedure CGPathCloseSubpath(path: CGMutablePathRef); cdecl; external libCoreGraphics name _PU + 'CGPathCloseSubpath';
function CGPathContainsPoint(path: CGPathRef; m: PCGAffineTransform; point: CGPoint; eoFill: Integer): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPathContainsPoint';
function CGPathCreateCopy(path: CGPathRef): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateCopy';
function CGPathCreateCopyByDashingPath(path: CGPathRef; transform: PCGAffineTransform; phase: CGFloat; lengths: PCGFloat; count: size_t): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateCopyByDashingPath';
function CGPathCreateCopyByStrokingPath(path: CGPathRef; transform: PCGAffineTransform; lineWidth: CGFloat; lineCap: CGLineCap; lineJoin: CGLineJoin; miterLimit: CGFloat): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateCopyByStrokingPath';
function CGPathCreateCopyByTransformingPath(path: CGPathRef; transform: PCGAffineTransform): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateCopyByTransformingPath';
function CGPathCreateMutable: CGMutablePathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateMutable';
function CGPathCreateMutableCopy(path: CGPathRef): CGMutablePathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateMutableCopy';
function CGPathCreateMutableCopyByTransformingPath(path: CGPathRef; transform: PCGAffineTransform): CGMutablePathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateMutableCopyByTransformingPath';
function CGPathCreateWithEllipseInRect(rect: CGRect; transform: PCGAffineTransform): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateWithEllipseInRect';
function CGPathCreateWithRect(rect: CGRect; transform: PCGAffineTransform): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathCreateWithRect';
function CGPathEqualToPath(path1: CGPathRef; path2: CGPathRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPathEqualToPath';
function CGPathGetBoundingBox(path: CGPathRef): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPathGetBoundingBox';
function CGPathGetCurrentPoint(path: CGPathRef): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGPathGetCurrentPoint';
function CGPathGetPathBoundingBox(path: CGPathRef): CGRect; cdecl; external libCoreGraphics name _PU + 'CGPathGetPathBoundingBox';
function CGPathGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGPathGetTypeID';
function CGPathIsEmpty(path: CGPathRef): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPathIsEmpty';
function CGPathIsRect(path: CGPathRef; rect: PCGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPathIsRect';
procedure CGPathMoveToPoint(path: CGMutablePathRef; m: PCGAffineTransform; x: CGFloat; y: CGFloat); cdecl; external libCoreGraphics name _PU + 'CGPathMoveToPoint';
procedure CGPathRelease(path: CGPathRef); cdecl; external libCoreGraphics name _PU + 'CGPathRelease';
function CGPathRetain(path: CGPathRef): CGPathRef; cdecl; external libCoreGraphics name _PU + 'CGPathRetain';
function CGPatternCreate(info: Pointer; bounds: CGRect; matrix: CGAffineTransform; xStep: CGFloat; yStep: CGFloat; tiling: CGPatternTiling; isColored: Boolean; callbacks: PCGPatternCallbacks): CGPatternRef; cdecl; external libCoreGraphics name _PU + 'CGPatternCreate';
function CGPatternGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGPatternGetTypeID';
procedure CGPatternRelease(pattern: CGPatternRef); cdecl; external libCoreGraphics name _PU + 'CGPatternRelease';
function CGPatternRetain(pattern: CGPatternRef): CGPatternRef; cdecl; external libCoreGraphics name _PU + 'CGPatternRetain';
function CGPointApplyAffineTransform(point: CGPoint; t: CGAffineTransform): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGPointApplyAffineTransform';
function CGPointCreateDictionaryRepresentation(point: CGPoint): CFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGPointCreateDictionaryRepresentation';
function CGPointEqualToPoint(point1: CGPoint; point2: CGPoint): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPointEqualToPoint';
//function CGPointMake(x: CGFloat; y: CGFloat): CGPoint; cdecl; external libCoreGraphics name _PU + 'CGPointMake';
function CGPointMakeWithDictionaryRepresentation(dict: CFDictionaryRef; point: PCGPoint): Boolean; cdecl; external libCoreGraphics name _PU + 'CGPointMakeWithDictionaryRepresentation';
function CGRectApplyAffineTransform(rect: CGRect; t: CGAffineTransform): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectApplyAffineTransform';
function CGRectContainsPoint(rect: CGRect; point: CGPoint): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectContainsPoint';
function CGRectContainsRect(rect1: CGRect; rect2: CGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectContainsRect';
function CGRectCreateDictionaryRepresentation(dummy: CGRect): CFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGRectCreateDictionaryRepresentation';
procedure CGRectDivide(rect: CGRect; slice: PCGRect; remainder: PCGRect; amount: CGFloat; edge: CGRectEdge); cdecl; external libCoreGraphics name _PU + 'CGRectDivide';
function CGRectEqualToRect(rect1: CGRect; rect2: CGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectEqualToRect';
function CGRectGetHeight(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetHeight';
function CGRectGetMaxX(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetMaxX';
function CGRectGetMaxY(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetMaxY';
function CGRectGetMidX(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetMidX';
function CGRectGetMidY(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetMidY';
function CGRectGetMinX(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetMinX';
function CGRectGetMinY(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetMinY';
function CGRectGetWidth(rect: CGRect): CGFloat; cdecl; external libCoreGraphics name _PU + 'CGRectGetWidth';
function CGRectInset(rect: CGRect; dx: CGFloat; dy: CGFloat): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectInset';
function CGRectIntegral(rect: CGRect): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectIntegral';
function CGRectIntersection(r1: CGRect; r2: CGRect): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectIntersection';
function CGRectIntersectsRect(rect1: CGRect; rect2: CGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectIntersectsRect';
function CGRectIsEmpty(rect: CGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectIsEmpty';
function CGRectIsInfinite(rect: CGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectIsInfinite';
function CGRectIsNull(rect: CGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectIsNull';
//function CGRectMake(x: CGFloat; y: CGFloat; width: CGFloat; height: CGFloat): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectMake';
function CGRectMakeWithDictionaryRepresentation(dict: CFDictionaryRef; rect: PCGRect): Boolean; cdecl; external libCoreGraphics name _PU + 'CGRectMakeWithDictionaryRepresentation';
function CGRectOffset(rect: CGRect; dx: CGFloat; dy: CGFloat): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectOffset';
function CGRectStandardize(rect: CGRect): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectStandardize';
function CGRectUnion(r1: CGRect; r2: CGRect): CGRect; cdecl; external libCoreGraphics name _PU + 'CGRectUnion';
function CGShadingCreateAxial(space: CGColorSpaceRef; start: CGPoint; end_: CGPoint; function_: CGFunctionRef; extendStart: Boolean; extendEnd: Boolean): CGShadingRef; cdecl; external libCoreGraphics name _PU + 'CGShadingCreateAxial';
function CGShadingCreateRadial(space: CGColorSpaceRef; start: CGPoint; startRadius: CGFloat; end_: CGPoint; endRadius: CGFloat; function_: CGFunctionRef; extendStart: Boolean; extendEnd: Boolean): CGShadingRef; cdecl; external libCoreGraphics name _PU + 'CGShadingCreateRadial';
function CGShadingGetTypeID: CFTypeID; cdecl; external libCoreGraphics name _PU + 'CGShadingGetTypeID';
procedure CGShadingRelease(shading: CGShadingRef); cdecl; external libCoreGraphics name _PU + 'CGShadingRelease';
function CGShadingRetain(shading: CGShadingRef): CGShadingRef; cdecl; external libCoreGraphics name _PU + 'CGShadingRetain';
function CGSizeApplyAffineTransform(size: CGSize; t: CGAffineTransform): CGSize; cdecl; external libCoreGraphics name _PU + 'CGSizeApplyAffineTransform';
function CGSizeCreateDictionaryRepresentation(size: CGSize): CFDictionaryRef; cdecl; external libCoreGraphics name _PU + 'CGSizeCreateDictionaryRepresentation';
function CGSizeEqualToSize(size1: CGSize; size2: CGSize): Boolean; cdecl; external libCoreGraphics name _PU + 'CGSizeEqualToSize';
//function CGSizeMake(width: CGFloat; height: CGFloat): CGSize; cdecl; external libCoreGraphics name _PU + 'CGSizeMake';
function CGSizeMakeWithDictionaryRepresentation(dict: CFDictionaryRef; size: PCGSize): Boolean; cdecl; external libCoreGraphics name _PU + 'CGSizeMakeWithDictionaryRepresentation';
function __CGAffineTransformMake(a: CGFloat; b: CGFloat; c: CGFloat; d: CGFloat; tx: CGFloat; ty: CGFloat): CGAffineTransform; cdecl; external libCoreGraphics name _PU + '__CGAffineTransformMake';
function __CGPointApplyAffineTransform(point: CGPoint; t: CGAffineTransform): CGPoint; cdecl; external libCoreGraphics name _PU + '__CGPointApplyAffineTransform';
function __CGPointEqualToPoint(point1: CGPoint; point2: CGPoint): Boolean; cdecl; external libCoreGraphics name _PU + '__CGPointEqualToPoint';
function __CGSizeApplyAffineTransform(size: CGSize; t: CGAffineTransform): CGSize; cdecl; external libCoreGraphics name _PU + '__CGSizeApplyAffineTransform';
function __CGSizeEqualToSize(size1: CGSize; size2: CGSize): Boolean; cdecl; external libCoreGraphics name _PU + '__CGSizeEqualToSize';

// ===== Utility functions =====

function CGPointMake(const AX, AY: CGFloat): CGPoint; overload;
function CGPointMake(const APoint: TPointF): CGPoint; overload;
function CGPointToPointF(const APoint: CGPoint): TPointF;

function CGSizeMake(const AWidth, AHeight: CGFloat): CGSize; overload;
function CGSizeMake(const ASize: TSizeF): CGSize; overload;
function CGSizeToSizeF(const ASize: CGSize): TSizeF;

function CGRectMake(const AX, AY: CGFloat; const AWidth, AHeight: CGFloat): CGRect; overload;
function CGRectMake(const ARect: TRectF): CGRect; overload;
function CGRectToRectF(const ARect: CGRect): TRectF;

const
  CGAffineTransformIdentity: CGAffineTransform = (
    a: 1;
    b: 0;
    c: 0;
    d: 1;
    tx: 0;
    ty: 0;
  );

implementation

function CGPointMake(const AX, AY: CGFloat): CGPoint;
begin
  Result.x := AX;
  Result.y := AY;
end;

function CGPointMake(const APoint: TPointF): CGPoint;
begin
  Result := CGPointMake(APoint.X, APoint.Y);
end;

function CGPointToPointF(const APoint: CGPoint): TPointF;
begin
  Result.X := APoint.x;
  Result.Y := APoint.y;
end;

function CGSizeMake(const AWidth, AHeight: CGFloat): CGSize;
begin
  Result.width := AWidth;
  Result.height := AHeight;
end;

function CGSizeMake(const ASize: TSizeF): CGSize;
begin
  Result := CGSizeMake(ASize.cx, ASize.cy);
end;

function CGSizeToSizeF(const ASize: CGSize): TSizeF;
begin
  Result.cx := ASize.width;
  Result.cy := ASize.height;
end;

function CGRectMake(const AX, AY: CGFloat; const AWidth, AHeight: CGFloat): CGRect;
begin
  Result.origin.x := AX;
  Result.origin.y := AY;
  Result.size.width := AWidth;
  Result.size.height := AHeight;
end;

function CGRectMake(const ARect: TRectF): CGRect;
begin
  Result := CGRectMake(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;

function CGRectToRectF(const ARect: CGRect): TRectF;
begin
  Result.TopLeft := CGPointToPointF(ARect.origin);
  Result.Size := CGSizeToSizeF(ARect.size);
end;

{ CGAffineTransform }

constructor CGAffineTransform.Create(const aa, ab, ac, ad, atx, aty: CGFloat);
begin
  a := aa;
  b := ab;
  c := ac;
  d := ad;
  tx := atx;
  ty := aty;
end;

end.
