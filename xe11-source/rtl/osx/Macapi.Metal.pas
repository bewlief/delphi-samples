{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework Metal                     }
{                                                          }
{ Copyright (c) 2008-2011 Apple Inc. All rights reserved.  }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2022 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit Macapi.Metal;

interface

uses
  {$IF defined(IOS)}
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  {$ELSEIF defined(MACOS)}
  Macapi.CocoaTypes,
  Macapi.Foundation,
  {$ENDIF}
  Macapi.CoreFoundation,
  Macapi.CoreServices,
  Macapi.Dispatch,
  Macapi.Mach,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC;

const
  MTLResourceUsageRead = 1 shl 0;
  MTLResourceUsageWrite = 1 shl 1;
  MTLResourceUsageSample = 1 shl 2;
  MTLBarrierScopeBuffers = 1 shl 0;
  MTLBarrierScopeTextures = 1 shl 1;
  MTLBarrierScopeRenderTargets = 1 shl 2;
  MTLPixelFormatInvalid = 0;
  MTLPixelFormatA8Unorm = 1;
  MTLPixelFormatR8Unorm = 10;
  MTLPixelFormatR8Unorm_sRGB = 11;
  MTLPixelFormatR8Snorm = 12;
  MTLPixelFormatR8Uint = 13;
  MTLPixelFormatR8Sint = 14;
  MTLPixelFormatR16Unorm = 20;
  MTLPixelFormatR16Snorm = 22;
  MTLPixelFormatR16Uint = 23;
  MTLPixelFormatR16Sint = 24;
  MTLPixelFormatR16Float = 25;
  MTLPixelFormatRG8Unorm = 30;
  MTLPixelFormatRG8Unorm_sRGB = 31;
  MTLPixelFormatRG8Snorm = 32;
  MTLPixelFormatRG8Uint = 33;
  MTLPixelFormatRG8Sint = 34;
  MTLPixelFormatB5G6R5Unorm = 40;
  MTLPixelFormatA1BGR5Unorm = 41;
  MTLPixelFormatABGR4Unorm = 42;
  MTLPixelFormatBGR5A1Unorm = 43;
  MTLPixelFormatR32Uint = 53;
  MTLPixelFormatR32Sint = 54;
  MTLPixelFormatR32Float = 55;
  MTLPixelFormatRG16Unorm = 60;
  MTLPixelFormatRG16Snorm = 62;
  MTLPixelFormatRG16Uint = 63;
  MTLPixelFormatRG16Sint = 64;
  MTLPixelFormatRG16Float = 65;
  MTLPixelFormatRGBA8Unorm = 70;
  MTLPixelFormatRGBA8Unorm_sRGB = 71;
  MTLPixelFormatRGBA8Snorm = 72;
  MTLPixelFormatRGBA8Uint = 73;
  MTLPixelFormatRGBA8Sint = 74;
  MTLPixelFormatBGRA8Unorm = 80;
  MTLPixelFormatBGRA8Unorm_sRGB = 81;
  MTLPixelFormatRGB10A2Unorm = 90;
  MTLPixelFormatRGB10A2Uint = 91;
  MTLPixelFormatRG11B10Float = 92;
  MTLPixelFormatRGB9E5Float = 93;
  MTLPixelFormatBGR10A2Unorm = 94;
  MTLPixelFormatBGR10_XR = 554;
  MTLPixelFormatBGR10_XR_sRGB = 555;
  MTLPixelFormatRG32Uint = 103;
  MTLPixelFormatRG32Sint = 104;
  MTLPixelFormatRG32Float = 105;
  MTLPixelFormatRGBA16Unorm = 110;
  MTLPixelFormatRGBA16Snorm = 112;
  MTLPixelFormatRGBA16Uint = 113;
  MTLPixelFormatRGBA16Sint = 114;
  MTLPixelFormatRGBA16Float = 115;
  MTLPixelFormatBGRA10_XR = 552;
  MTLPixelFormatBGRA10_XR_sRGB = 553;
  MTLPixelFormatRGBA32Uint = 123;
  MTLPixelFormatRGBA32Sint = 124;
  MTLPixelFormatRGBA32Float = 125;
  MTLPixelFormatBC1_RGBA = 130;
  MTLPixelFormatBC1_RGBA_sRGB = 131;
  MTLPixelFormatBC2_RGBA = 132;
  MTLPixelFormatBC2_RGBA_sRGB = 133;
  MTLPixelFormatBC3_RGBA = 134;
  MTLPixelFormatBC3_RGBA_sRGB = 135;
  MTLPixelFormatBC4_RUnorm = 140;
  MTLPixelFormatBC4_RSnorm = 141;
  MTLPixelFormatBC5_RGUnorm = 142;
  MTLPixelFormatBC5_RGSnorm = 143;
  MTLPixelFormatBC6H_RGBFloat = 150;
  MTLPixelFormatBC6H_RGBUfloat = 151;
  MTLPixelFormatBC7_RGBAUnorm = 152;
  MTLPixelFormatBC7_RGBAUnorm_sRGB = 153;
  MTLPixelFormatPVRTC_RGB_2BPP = 160;
  MTLPixelFormatPVRTC_RGB_2BPP_sRGB = 161;
  MTLPixelFormatPVRTC_RGB_4BPP = 162;
  MTLPixelFormatPVRTC_RGB_4BPP_sRGB = 163;
  MTLPixelFormatPVRTC_RGBA_2BPP = 164;
  MTLPixelFormatPVRTC_RGBA_2BPP_sRGB = 165;
  MTLPixelFormatPVRTC_RGBA_4BPP = 166;
  MTLPixelFormatPVRTC_RGBA_4BPP_sRGB = 167;
  MTLPixelFormatEAC_R11Unorm = 170;
  MTLPixelFormatEAC_R11Snorm = 172;
  MTLPixelFormatEAC_RG11Unorm = 174;
  MTLPixelFormatEAC_RG11Snorm = 176;
  MTLPixelFormatEAC_RGBA8 = 178;
  MTLPixelFormatEAC_RGBA8_sRGB = 179;
  MTLPixelFormatETC2_RGB8 = 180;
  MTLPixelFormatETC2_RGB8_sRGB = 181;
  MTLPixelFormatETC2_RGB8A1 = 182;
  MTLPixelFormatETC2_RGB8A1_sRGB = 183;
  MTLPixelFormatASTC_4x4_sRGB = 186;
  MTLPixelFormatASTC_5x4_sRGB = 187;
  MTLPixelFormatASTC_5x5_sRGB = 188;
  MTLPixelFormatASTC_6x5_sRGB = 189;
  MTLPixelFormatASTC_6x6_sRGB = 190;
  MTLPixelFormatASTC_8x5_sRGB = 192;
  MTLPixelFormatASTC_8x6_sRGB = 193;
  MTLPixelFormatASTC_8x8_sRGB = 194;
  MTLPixelFormatASTC_10x5_sRGB = 195;
  MTLPixelFormatASTC_10x6_sRGB = 196;
  MTLPixelFormatASTC_10x8_sRGB = 197;
  MTLPixelFormatASTC_10x10_sRGB = 198;
  MTLPixelFormatASTC_12x10_sRGB = 199;
  MTLPixelFormatASTC_12x12_sRGB = 200;
  MTLPixelFormatASTC_4x4_LDR = 204;
  MTLPixelFormatASTC_5x4_LDR = 205;
  MTLPixelFormatASTC_5x5_LDR = 206;
  MTLPixelFormatASTC_6x5_LDR = 207;
  MTLPixelFormatASTC_6x6_LDR = 208;
  MTLPixelFormatASTC_8x5_LDR = 210;
  MTLPixelFormatASTC_8x6_LDR = 211;
  MTLPixelFormatASTC_8x8_LDR = 212;
  MTLPixelFormatASTC_10x5_LDR = 213;
  MTLPixelFormatASTC_10x6_LDR = 214;
  MTLPixelFormatASTC_10x8_LDR = 215;
  MTLPixelFormatASTC_10x10_LDR = 216;
  MTLPixelFormatASTC_12x10_LDR = 217;
  MTLPixelFormatASTC_12x12_LDR = 218;
  MTLPixelFormatGBGR422 = 240;
  MTLPixelFormatBGRG422 = 241;
  MTLPixelFormatDepth16Unorm = 250;
  MTLPixelFormatDepth32Float = 252;
  MTLPixelFormatStencil8 = 253;
  MTLPixelFormatDepth24Unorm_Stencil8 = 255;
  MTLPixelFormatDepth32Float_Stencil8 = 260;
  MTLPixelFormatX32_Stencil8 = 261;
  MTLPixelFormatX24_Stencil8 = 262;
  MTLPurgeableStateKeepCurrent = 1;
  MTLPurgeableStateNonVolatile = 2;
  MTLPurgeableStateVolatile = 3;
  MTLPurgeableStateEmpty = 4;
  MTLCPUCacheModeDefaultCache = 0;
  MTLCPUCacheModeWriteCombined = 1;
  MTLStorageModeShared = 0;
  MTLStorageModeManaged = 1;
  MTLStorageModePrivate = 2;
  MTLStorageModeMemoryless = 3;
  MTLResourceCPUCacheModeDefaultCache = MTLCPUCacheModeDefaultCache shl 0;
  MTLResourceCPUCacheModeWriteCombined = MTLCPUCacheModeWriteCombined shl 0;
  MTLResourceStorageModeShared = MTLStorageModeShared shl 4;
  MTLResourceStorageModeManaged = MTLStorageModeManaged shl 4;
  MTLResourceStorageModePrivate = MTLStorageModePrivate shl 4;
  MTLResourceStorageModeMemoryless = MTLStorageModeMemoryless shl 4;
  MTLResourceHazardTrackingModeUntracked = 1 shl 8;
  MTLResourceOptionCPUCacheModeDefault = MTLResourceCPUCacheModeDefaultCache;
  MTLResourceOptionCPUCacheModeWriteCombined = MTLResourceCPUCacheModeWriteCombined;
  MTLTextureType1D = 0;
  MTLTextureType1DArray = 1;
  MTLTextureType2D = 2;
  MTLTextureType2DArray = 3;
  MTLTextureType2DMultisample = 4;
  MTLTextureTypeCube = 5;
  MTLTextureTypeCubeArray = 6;
  MTLTextureType3D = 7;
  MTLTextureType2DMultisampleArray = 8;
  MTLTextureTypeTextureBuffer = 9;
  MTLTextureUsageUnknown = 0;
  MTLTextureUsageShaderRead = 1;
  MTLTextureUsageShaderWrite = 2;
  MTLTextureUsageRenderTarget = 4;
  MTLTextureUsagePixelFormatView = 16;
  MTLDataTypeNone = 0;
  MTLDataTypeStruct = 1;
  MTLDataTypeArray = 2;
  MTLDataTypeFloat = 3;
  MTLDataTypeFloat2 = 4;
  MTLDataTypeFloat3 = 5;
  MTLDataTypeFloat4 = 6;
  MTLDataTypeFloat2x2 = 7;
  MTLDataTypeFloat2x3 = 8;
  MTLDataTypeFloat2x4 = 9;
  MTLDataTypeFloat3x2 = 10;
  MTLDataTypeFloat3x3 = 11;
  MTLDataTypeFloat3x4 = 12;
  MTLDataTypeFloat4x2 = 13;
  MTLDataTypeFloat4x3 = 14;
  MTLDataTypeFloat4x4 = 15;
  MTLDataTypeHalf = 16;
  MTLDataTypeHalf2 = 17;
  MTLDataTypeHalf3 = 18;
  MTLDataTypeHalf4 = 19;
  MTLDataTypeHalf2x2 = 20;
  MTLDataTypeHalf2x3 = 21;
  MTLDataTypeHalf2x4 = 22;
  MTLDataTypeHalf3x2 = 23;
  MTLDataTypeHalf3x3 = 24;
  MTLDataTypeHalf3x4 = 25;
  MTLDataTypeHalf4x2 = 26;
  MTLDataTypeHalf4x3 = 27;
  MTLDataTypeHalf4x4 = 28;
  MTLDataTypeInt = 29;
  MTLDataTypeInt2 = 30;
  MTLDataTypeInt3 = 31;
  MTLDataTypeInt4 = 32;
  MTLDataTypeUInt = 33;
  MTLDataTypeUInt2 = 34;
  MTLDataTypeUInt3 = 35;
  MTLDataTypeUInt4 = 36;
  MTLDataTypeShort = 37;
  MTLDataTypeShort2 = 38;
  MTLDataTypeShort3 = 39;
  MTLDataTypeShort4 = 40;
  MTLDataTypeUShort = 41;
  MTLDataTypeUShort2 = 42;
  MTLDataTypeUShort3 = 43;
  MTLDataTypeUShort4 = 44;
  MTLDataTypeChar = 45;
  MTLDataTypeChar2 = 46;
  MTLDataTypeChar3 = 47;
  MTLDataTypeChar4 = 48;
  MTLDataTypeUChar = 49;
  MTLDataTypeUChar2 = 50;
  MTLDataTypeUChar3 = 51;
  MTLDataTypeUChar4 = 52;
  MTLDataTypeBool = 53;
  MTLDataTypeBool2 = 54;
  MTLDataTypeBool3 = 55;
  MTLDataTypeBool4 = 56;
  MTLDataTypeTexture = 58;
  MTLDataTypeSampler = 59;
  MTLDataTypePointer = 60;
  MTLDataTypeR8Unorm = 62;
  MTLDataTypeR8Snorm = 63;
  MTLDataTypeR16Unorm = 64;
  MTLDataTypeR16Snorm = 65;
  MTLDataTypeRG8Unorm = 66;
  MTLDataTypeRG8Snorm = 67;
  MTLDataTypeRG16Unorm = 68;
  MTLDataTypeRG16Snorm = 69;
  MTLDataTypeRGBA8Unorm = 70;
  MTLDataTypeRGBA8Unorm_sRGB = 71;
  MTLDataTypeRGBA8Snorm = 72;
  MTLDataTypeRGBA16Unorm = 73;
  MTLDataTypeRGBA16Snorm = 74;
  MTLDataTypeRGB10A2Unorm = 75;
  MTLDataTypeRG11B10Float = 76;
  MTLDataTypeRGB9E5Float = 77;
  MTLDataTypeRenderPipeline = 78;
  MTLDataTypeIndirectCommandBuffer = 80;
  MTLArgumentTypeBuffer = 0;
  MTLArgumentTypeThreadgroupMemory = 1;
  MTLArgumentTypeTexture = 2;
  MTLArgumentTypeSampler = 3;
  MTLArgumentTypeImageblockData = 16;
  MTLArgumentTypeImageblock = 17;
  MTLArgumentAccessReadOnly = 0;
  MTLArgumentAccessReadWrite = 1;
  MTLArgumentAccessWriteOnly = 2;
  MTLPatchTypeNone = 0;
  MTLPatchTypeTriangle = 1;
  MTLPatchTypeQuad = 2;
  MTLFunctionTypeVertex = 1;
  MTLFunctionTypeFragment = 2;
  MTLFunctionTypeKernel = 3;
  MTLLanguageVersion1_0 = (1 shl 16);
  MTLLanguageVersion1_1 = (1 shl 16) + 1;
  MTLLanguageVersion1_2 = (1 shl 16) + 2;
  MTLLanguageVersion2_0 = (2 shl 16);
  MTLLanguageVersion2_1 = (2 shl 16) + 1;
  MTLLibraryErrorUnsupported = 1;
  MTLLibraryErrorInternal = 2;
  MTLLibraryErrorCompileFailure = 3;
  MTLLibraryErrorCompileWarning = 4;
  MTLLibraryErrorFunctionNotFound = 5;
  MTLLibraryErrorFileNotFound = 6;
  MTLFeatureSet_iOS_GPUFamily1_v1 = 0;
  MTLFeatureSet_iOS_GPUFamily2_v1 = 1;
  MTLFeatureSet_iOS_GPUFamily1_v2 = 2;
  MTLFeatureSet_iOS_GPUFamily2_v2 = 3;
  MTLFeatureSet_iOS_GPUFamily3_v1 = 4;
  MTLFeatureSet_iOS_GPUFamily1_v3 = 5;
  MTLFeatureSet_iOS_GPUFamily2_v3 = 6;
  MTLFeatureSet_iOS_GPUFamily3_v2 = 7;
  MTLFeatureSet_iOS_GPUFamily1_v4 = 8;
  MTLFeatureSet_iOS_GPUFamily2_v4 = 9;
  MTLFeatureSet_iOS_GPUFamily3_v3 = 10;
  MTLFeatureSet_iOS_GPUFamily4_v1 = 11;
  MTLFeatureSet_iOS_GPUFamily1_v5 = 12;
  MTLFeatureSet_iOS_GPUFamily2_v5 = 13;
  MTLFeatureSet_iOS_GPUFamily3_v4 = 14;
  MTLFeatureSet_iOS_GPUFamily4_v2 = 15;
  MTLFeatureSet_iOS_GPUFamily5_v1 = 16;
  MTLFeatureSet_macOS_GPUFamily1_v1 = 10000;
  MTLFeatureSet_OSX_GPUFamily1_v1 = MTLFeatureSet_macOS_GPUFamily1_v1;
  MTLFeatureSet_macOS_GPUFamily1_v2 = 10001;
  MTLFeatureSet_OSX_GPUFamily1_v2 = MTLFeatureSet_macOS_GPUFamily1_v2;
  MTLFeatureSet_macOS_ReadWriteTextureTier2 = 10002;
  MTLFeatureSet_OSX_ReadWriteTextureTier2 = MTLFeatureSet_macOS_ReadWriteTextureTier2;
  MTLFeatureSet_macOS_GPUFamily1_v3 = 10003;
  MTLFeatureSet_macOS_GPUFamily1_v4 = 10004;
  MTLFeatureSet_macOS_GPUFamily2_v1 = 10005;
  MTLFeatureSet_tvOS_GPUFamily1_v1 = 30000;
  //MTLFeatureSet_TVOS_GPUFamily1_v1 = MTLFeatureSet_tvOS_GPUFamily1_v1;
  MTLFeatureSet_tvOS_GPUFamily1_v2 = 30001;
  MTLFeatureSet_tvOS_GPUFamily1_v3 = 30002;
  MTLFeatureSet_tvOS_GPUFamily1_v4 = 30004;
  MTLPipelineOptionNone = 0;
  MTLPipelineOptionArgumentInfo = 1 shl 0;
  MTLPipelineOptionBufferTypeInfo = 1 shl 1;
  MTLReadWriteTextureTierNone = 0;
  MTLReadWriteTextureTier1 = 1;
  MTLReadWriteTextureTier2 = 2;
  MTLArgumentBuffersTier1 = 0;
  MTLArgumentBuffersTier2 = 1;
  MTLBlitOptionNone = 0;
  MTLBlitOptionDepthFromDepthStencil = 1 shl 0;
  MTLBlitOptionStencilFromDepthStencil = 1 shl 1;
  MTLBlitOptionRowLinearPVRTC = 1 shl 2;
  MTLCommandBufferStatusNotEnqueued = 0;
  MTLCommandBufferStatusEnqueued = 1;
  MTLCommandBufferStatusCommitted = 2;
  MTLCommandBufferStatusScheduled = 3;
  MTLCommandBufferStatusCompleted = 4;
  MTLCommandBufferStatusError = 5;
  MTLCommandBufferErrorNone = 0;
  MTLCommandBufferErrorInternal = 1;
  MTLCommandBufferErrorTimeout = 2;
  MTLCommandBufferErrorPageFault = 3;
  MTLCommandBufferErrorBlacklisted = 4;
  MTLCommandBufferErrorNotPermitted = 7;
  MTLCommandBufferErrorOutOfMemory = 8;
  MTLCommandBufferErrorInvalidResource = 9;
  MTLCommandBufferErrorMemoryless = 10;
  MTLCommandBufferErrorDeviceRemoved = 11;
  MTLDispatchTypeSerial = 0;
  MTLDispatchTypeConcurrent = 1;
  MTLCompareFunctionNever = 0;
  MTLCompareFunctionLess = 1;
  MTLCompareFunctionEqual = 2;
  MTLCompareFunctionLessEqual = 3;
  MTLCompareFunctionGreater = 4;
  MTLCompareFunctionNotEqual = 5;
  MTLCompareFunctionGreaterEqual = 6;
  MTLCompareFunctionAlways = 7;
  MTLStencilOperationKeep = 0;
  MTLStencilOperationZero = 1;
  MTLStencilOperationReplace = 2;
  MTLStencilOperationIncrementClamp = 3;
  MTLStencilOperationDecrementClamp = 4;
  MTLStencilOperationInvert = 5;
  MTLStencilOperationIncrementWrap = 6;
  MTLStencilOperationDecrementWrap = 7;
  MTLLoadActionDontCare = 0;
  MTLLoadActionLoad = 1;
  MTLLoadActionClear = 2;
  MTLStoreActionDontCare = 0;
  MTLStoreActionStore = 1;
  MTLStoreActionMultisampleResolve = 2;
  MTLStoreActionStoreAndMultisampleResolve = 3;
  MTLStoreActionUnknown = 4;
  MTLStoreActionCustomSampleDepthStore = 5;
  MTLStoreActionOptionNone = 0;
  MTLStoreActionOptionCustomSamplePositions = 1 shl 0;
  MTLMultisampleDepthResolveFilterSample0 = 0;
  MTLMultisampleDepthResolveFilterMin = 1;
  MTLMultisampleDepthResolveFilterMax = 2;
  MTLMultisampleStencilResolveFilterSample0 = 0;
  MTLMultisampleStencilResolveFilterDepthResolvedSample = 1;
  MTLAttributeFormatInvalid = 0;
  MTLAttributeFormatUChar2 = 1;
  MTLAttributeFormatUChar3 = 2;
  MTLAttributeFormatUChar4 = 3;
  MTLAttributeFormatChar2 = 4;
  MTLAttributeFormatChar3 = 5;
  MTLAttributeFormatChar4 = 6;
  MTLAttributeFormatUChar2Normalized = 7;
  MTLAttributeFormatUChar3Normalized = 8;
  MTLAttributeFormatUChar4Normalized = 9;
  MTLAttributeFormatChar2Normalized = 10;
  MTLAttributeFormatChar3Normalized = 11;
  MTLAttributeFormatChar4Normalized = 12;
  MTLAttributeFormatUShort2 = 13;
  MTLAttributeFormatUShort3 = 14;
  MTLAttributeFormatUShort4 = 15;
  MTLAttributeFormatShort2 = 16;
  MTLAttributeFormatShort3 = 17;
  MTLAttributeFormatShort4 = 18;
  MTLAttributeFormatUShort2Normalized = 19;
  MTLAttributeFormatUShort3Normalized = 20;
  MTLAttributeFormatUShort4Normalized = 21;
  MTLAttributeFormatShort2Normalized = 22;
  MTLAttributeFormatShort3Normalized = 23;
  MTLAttributeFormatShort4Normalized = 24;
  MTLAttributeFormatHalf2 = 25;
  MTLAttributeFormatHalf3 = 26;
  MTLAttributeFormatHalf4 = 27;
  MTLAttributeFormatFloat = 28;
  MTLAttributeFormatFloat2 = 29;
  MTLAttributeFormatFloat3 = 30;
  MTLAttributeFormatFloat4 = 31;
  MTLAttributeFormatInt = 32;
  MTLAttributeFormatInt2 = 33;
  MTLAttributeFormatInt3 = 34;
  MTLAttributeFormatInt4 = 35;
  MTLAttributeFormatUInt = 36;
  MTLAttributeFormatUInt2 = 37;
  MTLAttributeFormatUInt3 = 38;
  MTLAttributeFormatUInt4 = 39;
  MTLAttributeFormatInt1010102Normalized = 40;
  MTLAttributeFormatUInt1010102Normalized = 41;
  MTLAttributeFormatUChar4Normalized_BGRA = 42;
  MTLAttributeFormatUChar = 45;
  MTLAttributeFormatChar = 46;
  MTLAttributeFormatUCharNormalized = 47;
  MTLAttributeFormatCharNormalized = 48;
  MTLAttributeFormatUShort = 49;
  MTLAttributeFormatShort = 50;
  MTLAttributeFormatUShortNormalized = 51;
  MTLAttributeFormatShortNormalized = 52;
  MTLAttributeFormatHalf = 53;
  MTLIndexTypeUInt16 = 0;
  MTLIndexTypeUInt32 = 1;
  MTLStepFunctionConstant = 0;
  MTLStepFunctionPerVertex = 1;
  MTLStepFunctionPerInstance = 2;
  MTLStepFunctionPerPatch = 3;
  MTLStepFunctionPerPatchControlPoint = 4;
  MTLStepFunctionThreadPositionInGridX = 5;
  MTLStepFunctionThreadPositionInGridY = 6;
  MTLStepFunctionThreadPositionInGridXIndexed = 7;
  MTLStepFunctionThreadPositionInGridYIndexed = 8;
  MTLMutabilityDefault = 0;
  MTLMutabilityMutable = 1;
  MTLMutabilityImmutable = 2;
  MTLPrimitiveTypePoint = 0;
  MTLPrimitiveTypeLine = 1;
  MTLPrimitiveTypeLineStrip = 2;
  MTLPrimitiveTypeTriangle = 3;
  MTLPrimitiveTypeTriangleStrip = 4;
  MTLVisibilityResultModeDisabled = 0;
  MTLVisibilityResultModeBoolean = 1;
  MTLVisibilityResultModeCounting = 2;
  MTLCullModeNone = 0;
  MTLCullModeFront = 1;
  MTLCullModeBack = 2;
  MTLWindingClockwise = 0;
  MTLWindingCounterClockwise = 1;
  MTLDepthClipModeClip = 0;
  MTLDepthClipModeClamp = 1;
  MTLTriangleFillModeFill = 0;
  MTLTriangleFillModeLines = 1;
  MTLRenderStageVertex = (1 shl 0);
  MTLRenderStageFragment = (1 shl 1);
  MTLBlendFactorZero = 0;
  MTLBlendFactorOne = 1;
  MTLBlendFactorSourceColor = 2;
  MTLBlendFactorOneMinusSourceColor = 3;
  MTLBlendFactorSourceAlpha = 4;
  MTLBlendFactorOneMinusSourceAlpha = 5;
  MTLBlendFactorDestinationColor = 6;
  MTLBlendFactorOneMinusDestinationColor = 7;
  MTLBlendFactorDestinationAlpha = 8;
  MTLBlendFactorOneMinusDestinationAlpha = 9;
  MTLBlendFactorSourceAlphaSaturated = 10;
  MTLBlendFactorBlendColor = 11;
  MTLBlendFactorOneMinusBlendColor = 12;
  MTLBlendFactorBlendAlpha = 13;
  MTLBlendFactorOneMinusBlendAlpha = 14;
  MTLBlendFactorSource1Color = 15;
  MTLBlendFactorOneMinusSource1Color = 16;
  MTLBlendFactorSource1Alpha = 17;
  MTLBlendFactorOneMinusSource1Alpha = 18;
  MTLBlendOperationAdd = 0;
  MTLBlendOperationSubtract = 1;
  MTLBlendOperationReverseSubtract = 2;
  MTLBlendOperationMin = 3;
  MTLBlendOperationMax = 4;
  MTLColorWriteMaskNone = 0;
  MTLColorWriteMaskRed = 1 shl 3;
  MTLColorWriteMaskGreen = 1 shl 2;
  MTLColorWriteMaskBlue = 1 shl 1;
  MTLColorWriteMaskAlpha = 1 shl 0;
  MTLColorWriteMaskAll = 15;
  MTLPrimitiveTopologyClassUnspecified = 0;
  MTLPrimitiveTopologyClassPoint = 1;
  MTLPrimitiveTopologyClassLine = 2;
  MTLPrimitiveTopologyClassTriangle = 3;
  MTLTessellationPartitionModePow2 = 0;
  MTLTessellationPartitionModeInteger = 1;
  MTLTessellationPartitionModeFractionalOdd = 2;
  MTLTessellationPartitionModeFractionalEven = 3;
  MTLTessellationFactorStepFunctionConstant = 0;
  MTLTessellationFactorStepFunctionPerPatch = 1;
  MTLTessellationFactorStepFunctionPerInstance = 2;
  MTLTessellationFactorStepFunctionPerPatchAndPerInstance = 3;
  MTLTessellationFactorFormatHalf = 0;
  MTLTessellationControlPointIndexTypeNone = 0;
  MTLTessellationControlPointIndexTypeUInt16 = 1;
  MTLTessellationControlPointIndexTypeUInt32 = 2;
  MTLVertexFormatInvalid = 0;
  MTLVertexFormatUChar2 = 1;
  MTLVertexFormatUChar3 = 2;
  MTLVertexFormatUChar4 = 3;
  MTLVertexFormatChar2 = 4;
  MTLVertexFormatChar3 = 5;
  MTLVertexFormatChar4 = 6;
  MTLVertexFormatUChar2Normalized = 7;
  MTLVertexFormatUChar3Normalized = 8;
  MTLVertexFormatUChar4Normalized = 9;
  MTLVertexFormatChar2Normalized = 10;
  MTLVertexFormatChar3Normalized = 11;
  MTLVertexFormatChar4Normalized = 12;
  MTLVertexFormatUShort2 = 13;
  MTLVertexFormatUShort3 = 14;
  MTLVertexFormatUShort4 = 15;
  MTLVertexFormatShort2 = 16;
  MTLVertexFormatShort3 = 17;
  MTLVertexFormatShort4 = 18;
  MTLVertexFormatUShort2Normalized = 19;
  MTLVertexFormatUShort3Normalized = 20;
  MTLVertexFormatUShort4Normalized = 21;
  MTLVertexFormatShort2Normalized = 22;
  MTLVertexFormatShort3Normalized = 23;
  MTLVertexFormatShort4Normalized = 24;
  MTLVertexFormatHalf2 = 25;
  MTLVertexFormatHalf3 = 26;
  MTLVertexFormatHalf4 = 27;
  MTLVertexFormatFloat = 28;
  MTLVertexFormatFloat2 = 29;
  MTLVertexFormatFloat3 = 30;
  MTLVertexFormatFloat4 = 31;
  MTLVertexFormatInt = 32;
  MTLVertexFormatInt2 = 33;
  MTLVertexFormatInt3 = 34;
  MTLVertexFormatInt4 = 35;
  MTLVertexFormatUInt = 36;
  MTLVertexFormatUInt2 = 37;
  MTLVertexFormatUInt3 = 38;
  MTLVertexFormatUInt4 = 39;
  MTLVertexFormatInt1010102Normalized = 40;
  MTLVertexFormatUInt1010102Normalized = 41;
  MTLVertexFormatUChar4Normalized_BGRA = 42;
  MTLVertexFormatUChar = 45;
  MTLVertexFormatChar = 46;
  MTLVertexFormatUCharNormalized = 47;
  MTLVertexFormatCharNormalized = 48;
  MTLVertexFormatUShort = 49;
  MTLVertexFormatShort = 50;
  MTLVertexFormatUShortNormalized = 51;
  MTLVertexFormatShortNormalized = 52;
  MTLVertexFormatHalf = 53;
  MTLVertexStepFunctionConstant = 0;
  MTLVertexStepFunctionPerVertex = 1;
  MTLVertexStepFunctionPerInstance = 2;
  MTLVertexStepFunctionPerPatch = 3;
  MTLVertexStepFunctionPerPatchControlPoint = 4;
  MTLSamplerMinMagFilterNearest = 0;
  MTLSamplerMinMagFilterLinear = 1;
  MTLSamplerMipFilterNotMipmapped = 0;
  MTLSamplerMipFilterNearest = 1;
  MTLSamplerMipFilterLinear = 2;
  MTLSamplerAddressModeClampToEdge = 0;
  MTLSamplerAddressModeMirrorClampToEdge = 1;
  MTLSamplerAddressModeRepeat = 2;
  MTLSamplerAddressModeMirrorRepeat = 3;
  MTLSamplerAddressModeClampToZero = 4;
  MTLSamplerAddressModeClampToBorderColor = 5;
  MTLSamplerBorderColorTransparentBlack = 0;
  MTLSamplerBorderColorOpaqueBlack = 1;
  MTLSamplerBorderColorOpaqueWhite = 2;
  MTLIndirectCommandTypeDraw = (1 shl 0);
  MTLIndirectCommandTypeDrawIndexed = (1 shl 1);
  MTLIndirectCommandTypeDrawPatches = (1 shl 2);
  MTLIndirectCommandTypeDrawIndexedPatches = (1 shl 3);

type


  // ===== Forward declarations =====

{$M+}

  MTLDevice = interface;
  MTLCommandEncoder = interface;
  MTLHeap = interface;
  MTLResource = interface;
  MTLTextureDescriptor = interface;
  MTLTexture = interface;
  MTLBuffer = interface;
  MTLArgument = interface;
  MTLStructType = interface;
  MTLArrayType = interface;
  MTLTextureReferenceType = interface;
  MTLPointerType = interface;
  MTLType = interface;
  MTLStructMember = interface;
  MTLFunction = interface;
  MTLLibrary = interface;
  MTLCompileOptions = interface;
  MTLFunctionConstantValues = interface;
  MTLArgumentEncoder = interface;
  MTLVertexAttribute = interface;
  MTLAttribute = interface;
  MTLFunctionConstant = interface;
  MTLCommandQueue = interface;
  MTLDepthStencilState = interface;
  MTLSamplerState = interface;
  MTLRenderPipelineState = interface;
  MTLComputePipelineState = interface;
  MTLFence = interface;
  MTLTileRenderPipelineDescriptor = interface;
  MTLSamplerDescriptor = interface;
  MTLRenderPipelineColorAttachmentDescriptor = interface;
  MTLDepthStencilDescriptor = interface;
  MTLRenderPipelineDescriptor = interface;
  MTLRenderPassDescriptor = interface;
  MTLRenderPipelineReflection = interface;
  MTLComputePipelineDescriptor = interface;
  MTLComputePipelineReflection = interface;
  MTLHeapDescriptor = interface;
  MTLIndirectCommandBufferDescriptor = interface;
  MTLIndirectRenderCommandEncoder = interface;
  MTLIndirectComputeCommandEncoder = interface;
  MTLIndirectCommandBuffer = interface;
  MTLEvent = interface;
  MTLSharedEvent = interface;
  MTLSharedEventHandle = interface;
  MTLArgumentDescriptor = interface;
  MTLBlitCommandEncoder = interface;
  MTLRenderCommandEncoder = interface;
  MTLParallelRenderCommandEncoder = interface;
  MTLComputeCommandEncoder = interface;
  MTLDrawable = interface;
  MTLCommandBuffer = interface;
  MTLStencilDescriptor = interface;
  MTLRenderPassAttachmentDescriptor = interface;
  MTLRenderPassColorAttachmentDescriptor = interface;
  MTLRenderPassDepthAttachmentDescriptor = interface;
  MTLRenderPassStencilAttachmentDescriptor = interface;
  MTLRenderPassColorAttachmentDescriptorArray = interface;
  MTLBufferLayoutDescriptor = interface;
  MTLBufferLayoutDescriptorArray = interface;
  MTLAttributeDescriptor = interface;
  MTLAttributeDescriptorArray = interface;
  MTLStageInputOutputDescriptor = interface;
  MTLPipelineBufferDescriptor = interface;
  MTLPipelineBufferDescriptorArray = interface;
  MTLVertexDescriptor = interface;
  MTLRenderPipelineColorAttachmentDescriptorArray = interface;
  MTLTileRenderPipelineColorAttachmentDescriptor = interface;
  MTLTileRenderPipelineColorAttachmentDescriptorArray = interface;
  MTLVertexBufferLayoutDescriptor = interface;
  MTLVertexBufferLayoutDescriptorArray = interface;
  MTLVertexAttributeDescriptor = interface;
  MTLVertexAttributeDescriptorArray = interface;
  MTLCaptureScope = interface;
  MTLCaptureManager = interface;
  MTLIndirectRenderCommand = interface;
  MTLSharedEventListener = interface;


  // ===== Framework typedefs =====

{$M+}

  MTLOrigin = record
    x: NSUInteger;
    y: NSUInteger;
    z: NSUInteger;
  end;
  PMTLOrigin = ^MTLOrigin;

  MTLSize = record
    width: NSUInteger;
    height: NSUInteger;
    depth: NSUInteger;
  end;
  PMTLSize = ^MTLSize;

  MTLRegion = record
    origin: MTLOrigin;
    size: MTLSize;
  end;
  PMTLRegion = ^MTLRegion;

  MTLSamplePosition = record
    x: Single;
    y: Single;
  end;
  PMTLSamplePosition = ^MTLSamplePosition;

  MTLResourceUsage = NSUInteger;
  MTLBarrierScope = NSUInteger;
  MTLPixelFormat = NSUInteger;
  MTLPurgeableState = NSUInteger;
  MTLCPUCacheMode = NSUInteger;
  MTLStorageMode = NSUInteger;
  MTLResourceOptions = NSUInteger;

  MTLTextureType = NSUInteger;
  MTLTextureUsage = NSUInteger;
  IOSurfaceRef = Pointer;
  PIOSurfaceRef = ^IOSurfaceRef;
  MTLDataType = NSUInteger;
  MTLArgumentType = NSUInteger;
  MTLArgumentAccess = NSUInteger;
  MTLAutoreleasedArgument = MTLArgument;
  PMTLAutoreleasedArgument = ^MTLAutoreleasedArgument;
  MTLPatchType = NSUInteger;
  MTLFunctionType = NSUInteger;

  MTLLanguageVersion = NSUInteger;
  MTLLibraryError = NSUInteger;
  TMTLLibraryNewFunctionWithNameConstantValuesCompletionHandler = procedure(&function: MTLFunction; error: NSError) of object;
  MTLFeatureSet = NSUInteger;
  MTLPipelineOption = NSUInteger;
  MTLReadWriteTextureTier = NSUInteger;
  MTLArgumentBuffersTier = NSUInteger;

  MTLSizeAndAlign = record
    size: NSUInteger;
    align: NSUInteger;
  end;
  PMTLSizeAndAlign = ^MTLSizeAndAlign;

  MTLAutoreleasedRenderPipelineReflection = MTLRenderPipelineReflection;
  PMTLAutoreleasedRenderPipelineReflection = ^MTLAutoreleasedRenderPipelineReflection;
  MTLAutoreleasedComputePipelineReflection = MTLComputePipelineReflection;
  PMTLAutoreleasedComputePipelineReflection = ^MTLAutoreleasedComputePipelineReflection;
  MTLNewLibraryCompletionHandler = procedure(&library: MTLLibrary; error: NSError) of object;
  MTLNewRenderPipelineStateCompletionHandler = procedure(renderPipelineState: MTLRenderPipelineState; error: NSError) of object;
  MTLNewRenderPipelineStateWithReflectionCompletionHandler = procedure(renderPipelineState: MTLRenderPipelineState; reflection: MTLRenderPipelineReflection; error: NSError) of object;
  MTLNewComputePipelineStateCompletionHandler = procedure(computePipelineState: MTLComputePipelineState; error: NSError) of object;
  MTLNewComputePipelineStateWithReflectionCompletionHandler = procedure(computePipelineState: MTLComputePipelineState; reflection: MTLComputePipelineReflection; error: NSError) of object;
  TMTLDeviceNewBufferWithBytesNoCopyDeallocator = procedure(pointer: Pointer; length: NSUInteger) of object;
  dispatch_data_t = Pointer;
  Pdispatch_data_t = ^dispatch_data_t;
  MTLBlitOption = NSUInteger;
  MTLCommandBufferStatus = NSUInteger;
  MTLCommandBufferError = NSUInteger;
  MTLCommandBufferHandler = procedure(commandBuffer: MTLCommandBuffer) of object;
  MTLDispatchType = NSUInteger;

  MTLDispatchThreadgroupsIndirectArguments = record
    threadgroupsPerGrid: array[0..2] of LongWord;
  end;
  PMTLDispatchThreadgroupsIndirectArguments = ^MTLDispatchThreadgroupsIndirectArguments;

  MTLStageInRegionIndirectArguments = record
    stageInOrigin: array[0..2] of LongWord;
    stageInSize: array[0..2] of LongWord;
  end;
  PMTLStageInRegionIndirectArguments = ^MTLStageInRegionIndirectArguments;

  MTLCompareFunction = NSUInteger;
  MTLStencilOperation = NSUInteger;
  MTLDrawablePresentedHandler = procedure(drawable: MTLDrawable) of object;
  MTLLoadAction = NSUInteger;
  MTLStoreAction = NSUInteger;
  MTLStoreActionOptions = NSUInteger;

  MTLClearColor = record
    red: Double;
    green: Double;
    blue: Double;
    alpha: Double;
  end;
  PMTLClearColor = ^MTLClearColor;

  MTLMultisampleDepthResolveFilter = NSUInteger;
  MTLMultisampleStencilResolveFilter = NSUInteger;
  MTLAttributeFormat = NSUInteger;
  MTLIndexType = NSUInteger;
  MTLStepFunction = NSUInteger;
  MTLMutability = NSUInteger;
  MTLPrimitiveType = NSUInteger;
  MTLVisibilityResultMode = NSUInteger;

  MTLScissorRect = record
    x: NSUInteger;
    y: NSUInteger;
    width: NSUInteger;
    height: NSUInteger;
  end;
  PMTLScissorRect = ^MTLScissorRect;

  MTLViewport = record
    originX: Double;
    originY: Double;
    width: Double;
    height: Double;
    znear: Double;
    zfar: Double;
  end;
  PMTLViewport = ^MTLViewport;

  MTLCullMode = NSUInteger;
  MTLWinding = NSUInteger;
  MTLDepthClipMode = NSUInteger;
  MTLTriangleFillMode = NSUInteger;

  MTLDrawPrimitivesIndirectArguments = record
    vertexCount: LongWord;
    instanceCount: LongWord;
    vertexStart: LongWord;
    baseInstance: LongWord;
  end;
  PMTLDrawPrimitivesIndirectArguments = ^MTLDrawPrimitivesIndirectArguments;

  MTLDrawIndexedPrimitivesIndirectArguments = record
    indexCount: LongWord;
    instanceCount: LongWord;
    indexStart: LongWord;
    baseVertex: Int32;
    baseInstance: LongWord;
  end;
  PMTLDrawIndexedPrimitivesIndirectArguments = ^MTLDrawIndexedPrimitivesIndirectArguments;

  MTLDrawPatchIndirectArguments = record
    patchCount: LongWord;
    instanceCount: LongWord;
    patchStart: LongWord;
    baseInstance: LongWord;
  end;
  PMTLDrawPatchIndirectArguments = ^MTLDrawPatchIndirectArguments;

  MTLQuadTessellationFactorsHalf = record
    edgeTessellationFactor: array[0..3] of Word;
    insideTessellationFactor: array[0..1] of Word;
  end;
  PMTLQuadTessellationFactorsHalf = ^MTLQuadTessellationFactorsHalf;

  MTLTriangleTessellationFactorsHalf = record
    edgeTessellationFactor: array[0..2] of Word;
    insideTessellationFactor: Word;
  end;
  PMTLTriangleTessellationFactorsHalf = ^MTLTriangleTessellationFactorsHalf;

  MTLRenderStages = NSUInteger;
  MTLBlendFactor = NSUInteger;
  MTLBlendOperation = NSUInteger;
  MTLColorWriteMask = NSUInteger;
  MTLPrimitiveTopologyClass = NSUInteger;
  MTLTessellationPartitionMode = NSUInteger;
  MTLTessellationFactorStepFunction = NSUInteger;
  MTLTessellationFactorFormat = NSUInteger;
  MTLTessellationControlPointIndexType = NSUInteger;
  MTLVertexFormat = NSUInteger;
  MTLVertexStepFunction = NSUInteger;
  MTLSamplerMinMagFilter = NSUInteger;
  MTLSamplerMipFilter = NSUInteger;
  MTLSamplerAddressMode = NSUInteger;
  MTLSamplerBorderColor = NSUInteger;
  MTLIndirectCommandType = NSUInteger;
  MTLSharedEventNotificationBlock = procedure(sharedEvent: MTLSharedEvent; value: UInt64) of object;


  // ===== Interface declarations =====

  MTLTextureDescriptorClass = interface(NSObjectClass)
    ['{AB8548A2-66BF-4CDD-B72F-C4E574C7632A}']
    {class} function texture2DDescriptorWithPixelFormat(pixelFormat: MTLPixelFormat; width: NSUInteger; height: NSUInteger; mipmapped: Boolean) : MTLTextureDescriptor; cdecl;
    {class} function textureCubeDescriptorWithPixelFormat(pixelFormat: MTLPixelFormat; size: NSUInteger; mipmapped: Boolean) : MTLTextureDescriptor; cdecl;
    {class} function textureBufferDescriptorWithPixelFormat(pixelFormat: MTLPixelFormat; width: NSUInteger; resourceOptions: MTLResourceOptions; usage: MTLTextureUsage) : MTLTextureDescriptor; cdecl;
  end;
  MTLTextureDescriptor = interface(NSObject)
    ['{9E0D3B5F-C43D-4A37-9341-CD93A6E47D81}']
    procedure setTextureType(textureType: MTLTextureType); cdecl;
    function textureType : MTLTextureType; cdecl;
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    function pixelFormat : MTLPixelFormat; cdecl;
    procedure setWidth(width: NSUInteger); cdecl;
    function width : NSUInteger; cdecl;
    procedure setHeight(height: NSUInteger); cdecl;
    function height : NSUInteger; cdecl;
    procedure setDepth(depth: NSUInteger); cdecl;
    function depth : NSUInteger; cdecl;
    procedure setMipmapLevelCount(mipmapLevelCount: NSUInteger); cdecl;
    function mipmapLevelCount : NSUInteger; cdecl;
    procedure setSampleCount(sampleCount: NSUInteger); cdecl;
    function sampleCount : NSUInteger; cdecl;
    procedure setArrayLength(arrayLength: NSUInteger); cdecl;
    function arrayLength : NSUInteger; cdecl;
    procedure setResourceOptions(resourceOptions: MTLResourceOptions); cdecl;
    function resourceOptions : MTLResourceOptions; cdecl;
    procedure setCpuCacheMode(cpuCacheMode: MTLCPUCacheMode); cdecl;
    function cpuCacheMode : MTLCPUCacheMode; cdecl;
    procedure setStorageMode(storageMode: MTLStorageMode); cdecl;
    function storageMode : MTLStorageMode; cdecl;
    procedure setUsage(usage: MTLTextureUsage); cdecl;
    function usage : MTLTextureUsage; cdecl;
    procedure setAllowGPUOptimizedContents(allowGPUOptimizedContents: Boolean); cdecl;
    function allowGPUOptimizedContents : Boolean; cdecl;
  end;
  TMTLTextureDescriptor = class(TOCGenericImport<MTLTextureDescriptorClass, MTLTextureDescriptor>)  end;
  PMTLTextureDescriptor = Pointer;

  MTLArgumentClass = interface(NSObjectClass)
    ['{FED43758-4F5D-4E52-B96A-EAA5237C415D}']
  end;
  MTLArgument = interface(NSObject)
    ['{444C2B50-9E66-4C28-91BC-4028F67999BF}']
    function name : NSString; cdecl;
    function &type : MTLArgumentType; cdecl;
    function access : MTLArgumentAccess; cdecl;
    function index : NSUInteger; cdecl;
    function isActive : Boolean; cdecl;
    function bufferAlignment : NSUInteger; cdecl;
    function bufferDataSize : NSUInteger; cdecl;
    function bufferDataType : MTLDataType; cdecl;
    function bufferStructType : MTLStructType; cdecl;
    function bufferPointerType : MTLPointerType; cdecl;
    function threadgroupMemoryAlignment : NSUInteger; cdecl;
    function threadgroupMemoryDataSize : NSUInteger; cdecl;
    function textureType : MTLTextureType; cdecl;
    function textureDataType : MTLDataType; cdecl;
    function isDepthTexture : Boolean; cdecl;
    function arrayLength : NSUInteger; cdecl;
  end;
  TMTLArgument = class(TOCGenericImport<MTLArgumentClass, MTLArgument>)  end;
  PMTLArgument = Pointer;

  MTLTypeClass = interface(NSObjectClass)
    ['{3FD45DDD-637A-417D-849E-B4B7820FDA76}']
  end;
  MTLType = interface(NSObject)
    ['{E5A1BD9D-F2DB-4500-A874-85EFDDE5F076}']
    function dataType : MTLDataType; cdecl;
  end;
  TMTLType = class(TOCGenericImport<MTLTypeClass, MTLType>)  end;
  PMTLType = Pointer;

  MTLStructTypeClass = interface(MTLTypeClass)
    ['{7499FAE1-96EC-41D3-91A8-F8A5023D7FAE}']
  end;
  MTLStructType = interface(MTLType)
    ['{82D158CC-947E-4355-B79A-A2E3DE92C67A}']
    function members : NSArray; cdecl;
    function memberByName(name: NSString) : MTLStructMember; cdecl;
  end;
  TMTLStructType = class(TOCGenericImport<MTLStructTypeClass, MTLStructType>)  end;
  PMTLStructType = Pointer;

  MTLArrayTypeClass = interface(MTLTypeClass)
    ['{A40CB0C5-2698-4206-8CF4-3250E5273776}']
  end;
  MTLArrayType = interface(MTLType)
    ['{19124D0B-C332-4B91-A327-516C97E7F883}']
    function elementType : MTLDataType; cdecl;
    function arrayLength : NSUInteger; cdecl;
    function stride : NSUInteger; cdecl;
    function argumentIndexStride : NSUInteger; cdecl;
    function elementStructType : MTLStructType; cdecl;
    function elementArrayType : MTLArrayType; cdecl;
    function elementTextureReferenceType : MTLTextureReferenceType; cdecl;
    function elementPointerType : MTLPointerType; cdecl;
  end;
  TMTLArrayType = class(TOCGenericImport<MTLArrayTypeClass, MTLArrayType>)  end;
  PMTLArrayType = Pointer;

  MTLTextureReferenceTypeClass = interface(MTLTypeClass)
    ['{0D001BF2-F086-473F-9736-D88C19EE6069}']
  end;
  MTLTextureReferenceType = interface(MTLType)
    ['{12ACF7BD-A5B8-4BD1-9796-8AD98E7E71C7}']
    function textureDataType : MTLDataType; cdecl;
    function textureType : MTLTextureType; cdecl;
    function access : MTLArgumentAccess; cdecl;
    function isDepthTexture : Boolean; cdecl;
  end;
  TMTLTextureReferenceType = class(TOCGenericImport<MTLTextureReferenceTypeClass, MTLTextureReferenceType>)  end;
  PMTLTextureReferenceType = Pointer;

  MTLPointerTypeClass = interface(MTLTypeClass)
    ['{8CCBCACE-C3D1-4CDF-BA52-0D7E57AC9FF1}']
  end;
  MTLPointerType = interface(MTLType)
    ['{197CDF9A-FC7B-4572-8113-432944E41477}']
    function elementType : MTLDataType; cdecl;
    function access : MTLArgumentAccess; cdecl;
    function alignment : NSUInteger; cdecl;
    function dataSize : NSUInteger; cdecl;
    function elementIsArgumentBuffer : Boolean; cdecl;
    function elementStructType : MTLStructType; cdecl;
    function elementArrayType : MTLArrayType; cdecl;
  end;
  TMTLPointerType = class(TOCGenericImport<MTLPointerTypeClass, MTLPointerType>)  end;
  PMTLPointerType = Pointer;

  MTLStructMemberClass = interface(NSObjectClass)
    ['{260FE9F5-A55E-4A81-9B57-91162D522B9F}']
  end;
  MTLStructMember = interface(NSObject)
    ['{C1C73EA9-36D6-4D3B-892D-342A8F2C77C5}']
    function name : NSString; cdecl;
    function offset : NSUInteger; cdecl;
    function dataType : MTLDataType; cdecl;
    function structType : MTLStructType; cdecl;
    function arrayType : MTLArrayType; cdecl;
    function textureReferenceType : MTLTextureReferenceType; cdecl;
    function pointerType : MTLPointerType; cdecl;
    function argumentIndex : NSUInteger; cdecl;
  end;
  TMTLStructMember = class(TOCGenericImport<MTLStructMemberClass, MTLStructMember>)  end;
  PMTLStructMember = Pointer;

  MTLCompileOptionsClass = interface(NSObjectClass)
    ['{794A8BDE-4634-41C4-9A1D-B4FBDBA563C9}']
  end;
  MTLCompileOptions = interface(NSObject)
    ['{3FCEA625-354F-4485-82B7-C2672BE8CB0C}']
    procedure setPreprocessorMacros(preprocessorMacros: NSDictionary); cdecl;
    function preprocessorMacros : NSDictionary; cdecl;
    procedure setFastMathEnabled(fastMathEnabled: Boolean); cdecl;
    function fastMathEnabled : Boolean; cdecl;
    procedure setLanguageVersion(languageVersion: MTLLanguageVersion); cdecl;
    function languageVersion : MTLLanguageVersion; cdecl;
  end;
  TMTLCompileOptions = class(TOCGenericImport<MTLCompileOptionsClass, MTLCompileOptions>)  end;
  PMTLCompileOptions = Pointer;

  MTLFunctionConstantValuesClass = interface(NSObjectClass)
    ['{3467F290-C092-4040-A63D-A8599892BE45}']
  end;
  MTLFunctionConstantValues = interface(NSObject)
    ['{85BA7F72-44D6-44BD-BAC0-FFE735A6E2F5}']
    [MethodName('setConstantValue:type:atIndex:')]
    procedure setConstantValueTypeAtIndex(value: Pointer; &type: MTLDataType; atIndex: NSUInteger); cdecl;
    procedure setConstantValues(values: Pointer; &type: MTLDataType; withRange: NSRange); cdecl;
    [MethodName('setConstantValue:type:withName:')]
    procedure setConstantValueTypeWithName(value: Pointer; &type: MTLDataType; withName: NSString); cdecl;
    procedure reset; cdecl;
  end;
  TMTLFunctionConstantValues = class(TOCGenericImport<MTLFunctionConstantValuesClass, MTLFunctionConstantValues>)  end;
  PMTLFunctionConstantValues = Pointer;

  MTLVertexAttributeClass = interface(NSObjectClass)
    ['{028B0A42-68CA-459A-9331-EA9ADCA2C6B3}']
  end;
  MTLVertexAttribute = interface(NSObject)
    ['{A389D726-24C8-41EF-9C85-CDF74C0C07D2}']
    function name : NSString; cdecl;
    function attributeIndex : NSUInteger; cdecl;
    function attributeType : MTLDataType; cdecl;
    function isActive : Boolean; cdecl;
    function isPatchData : Boolean; cdecl;
    function isPatchControlPointData : Boolean; cdecl;
  end;
  TMTLVertexAttribute = class(TOCGenericImport<MTLVertexAttributeClass, MTLVertexAttribute>)  end;
  PMTLVertexAttribute = Pointer;

  MTLAttributeClass = interface(NSObjectClass)
    ['{B78CF324-DCCC-46C5-AF06-4AB024912F7B}']
  end;
  MTLAttribute = interface(NSObject)
    ['{0C742290-5A77-4DB6-B557-32BE3DF4A2F7}']
    function name : NSString; cdecl;
    function attributeIndex : NSUInteger; cdecl;
    function attributeType : MTLDataType; cdecl;
    function isActive : Boolean; cdecl;
    function isPatchData : Boolean; cdecl;
    function isPatchControlPointData : Boolean; cdecl;
  end;
  TMTLAttribute = class(TOCGenericImport<MTLAttributeClass, MTLAttribute>)  end;
  PMTLAttribute = Pointer;

  MTLFunctionConstantClass = interface(NSObjectClass)
    ['{46F8DD00-5A8C-4D48-9C4A-B7946F3643A5}']
  end;
  MTLFunctionConstant = interface(NSObject)
    ['{E7E4C9B1-69D3-4877-A0E1-4CAA7E6EC70C}']
    function name : NSString; cdecl;
    function &type : MTLDataType; cdecl;
    function index : NSUInteger; cdecl;
    function required : Boolean; cdecl;
  end;
  TMTLFunctionConstant = class(TOCGenericImport<MTLFunctionConstantClass, MTLFunctionConstant>)  end;
  PMTLFunctionConstant = Pointer;

  MTLTileRenderPipelineDescriptorClass = interface(NSObjectClass)
    ['{00BF2FE1-F704-4538-99D7-AD852276F01A}']
  end;
  MTLTileRenderPipelineDescriptor = interface(NSObject)
    ['{20FE634C-458D-4126-BD90-E4119BA2E9FE}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    procedure setTileFunction(tileFunction: MTLFunction); cdecl;
    function tileFunction : MTLFunction; cdecl;
    procedure setRasterSampleCount(rasterSampleCount: NSUInteger); cdecl;
    function rasterSampleCount : NSUInteger; cdecl;
    function colorAttachments : MTLTileRenderPipelineColorAttachmentDescriptorArray; cdecl;
    procedure setThreadgroupSizeMatchesTileSize(threadgroupSizeMatchesTileSize: Boolean); cdecl;
    function threadgroupSizeMatchesTileSize : Boolean; cdecl;
    function tileBuffers : MTLPipelineBufferDescriptorArray; cdecl;
    procedure setMaxTotalThreadsPerThreadgroup(maxTotalThreadsPerThreadgroup: NSUInteger); cdecl;
    function maxTotalThreadsPerThreadgroup : NSUInteger; cdecl;
    procedure reset; cdecl;
  end;
  TMTLTileRenderPipelineDescriptor = class(TOCGenericImport<MTLTileRenderPipelineDescriptorClass, MTLTileRenderPipelineDescriptor>)  end;
  PMTLTileRenderPipelineDescriptor = Pointer;

  MTLSamplerDescriptorClass = interface(NSObjectClass)
    ['{5310941F-E1F3-445E-A85C-E50979B96996}']
  end;
  MTLSamplerDescriptor = interface(NSObject)
    ['{7DED2ECB-3BAC-4177-99BA-793C67EBE96F}']
    procedure setMinFilter(minFilter: MTLSamplerMinMagFilter); cdecl;
    function minFilter : MTLSamplerMinMagFilter; cdecl;
    procedure setMagFilter(magFilter: MTLSamplerMinMagFilter); cdecl;
    function magFilter : MTLSamplerMinMagFilter; cdecl;
    procedure setMipFilter(mipFilter: MTLSamplerMipFilter); cdecl;
    function mipFilter : MTLSamplerMipFilter; cdecl;
    procedure setMaxAnisotropy(maxAnisotropy: NSUInteger); cdecl;
    function maxAnisotropy : NSUInteger; cdecl;
    procedure setSAddressMode(sAddressMode: MTLSamplerAddressMode); cdecl;
    function sAddressMode : MTLSamplerAddressMode; cdecl;
    procedure setTAddressMode(tAddressMode: MTLSamplerAddressMode); cdecl;
    function tAddressMode : MTLSamplerAddressMode; cdecl;
    procedure setRAddressMode(rAddressMode: MTLSamplerAddressMode); cdecl;
    function rAddressMode : MTLSamplerAddressMode; cdecl;
    procedure setBorderColor(borderColor: MTLSamplerBorderColor); cdecl;
    function borderColor : MTLSamplerBorderColor; cdecl;
    procedure setNormalizedCoordinates(normalizedCoordinates: Boolean); cdecl;
    function normalizedCoordinates : Boolean; cdecl;
    procedure setLodMinClamp(lodMinClamp: Single); cdecl;
    function lodMinClamp : Single; cdecl;
    procedure setLodMaxClamp(lodMaxClamp: Single); cdecl;
    function lodMaxClamp : Single; cdecl;
    procedure setLodAverage(lodAverage: Boolean); cdecl;
    function lodAverage : Boolean; cdecl;
    procedure setCompareFunction(compareFunction: MTLCompareFunction); cdecl;
    function compareFunction : MTLCompareFunction; cdecl;
    procedure setSupportArgumentBuffers(supportArgumentBuffers: Boolean); cdecl;
    function supportArgumentBuffers : Boolean; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
  end;
  TMTLSamplerDescriptor = class(TOCGenericImport<MTLSamplerDescriptorClass, MTLSamplerDescriptor>)  end;
  PMTLSamplerDescriptor = Pointer;

  MTLRenderPipelineColorAttachmentDescriptorClass = interface(NSObjectClass)
    ['{9AA354FA-0E2E-411D-B6B8-2F404A65605E}']
  end;
  MTLRenderPipelineColorAttachmentDescriptor = interface(NSObject)
    ['{AA55F953-CD08-45EA-8776-9173A195E8FA}']
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    function pixelFormat : MTLPixelFormat; cdecl;
    procedure setBlendingEnabled(blendingEnabled: Boolean); cdecl;
    function isBlendingEnabled : Boolean; cdecl;
    procedure setSourceRGBBlendFactor(sourceRGBBlendFactor: MTLBlendFactor); cdecl;
    function sourceRGBBlendFactor : MTLBlendFactor; cdecl;
    procedure setDestinationRGBBlendFactor(destinationRGBBlendFactor: MTLBlendFactor); cdecl;
    function destinationRGBBlendFactor : MTLBlendFactor; cdecl;
    procedure setRgbBlendOperation(rgbBlendOperation: MTLBlendOperation); cdecl;
    function rgbBlendOperation : MTLBlendOperation; cdecl;
    procedure setSourceAlphaBlendFactor(sourceAlphaBlendFactor: MTLBlendFactor); cdecl;
    function sourceAlphaBlendFactor : MTLBlendFactor; cdecl;
    procedure setDestinationAlphaBlendFactor(destinationAlphaBlendFactor: MTLBlendFactor); cdecl;
    function destinationAlphaBlendFactor : MTLBlendFactor; cdecl;
    procedure setAlphaBlendOperation(alphaBlendOperation: MTLBlendOperation); cdecl;
    function alphaBlendOperation : MTLBlendOperation; cdecl;
    procedure setWriteMask(writeMask: MTLColorWriteMask); cdecl;
    function writeMask : MTLColorWriteMask; cdecl;
  end;
  TMTLRenderPipelineColorAttachmentDescriptor = class(TOCGenericImport<MTLRenderPipelineColorAttachmentDescriptorClass, MTLRenderPipelineColorAttachmentDescriptor>)  end;
  PMTLRenderPipelineColorAttachmentDescriptor = Pointer;

  MTLDepthStencilDescriptorClass = interface(NSObjectClass)
    ['{B158E71C-C6B6-4782-8FB1-6A8FA665F081}']
  end;
  MTLDepthStencilDescriptor = interface(NSObject)
    ['{E0D887A3-E13B-44F3-B1A9-F67543C8F596}']
    procedure setDepthCompareFunction(depthCompareFunction: MTLCompareFunction); cdecl;
    function depthCompareFunction : MTLCompareFunction; cdecl;
    procedure setDepthWriteEnabled(depthWriteEnabled: Boolean); cdecl;
    function isDepthWriteEnabled : Boolean; cdecl;
    procedure setFrontFaceStencil(frontFaceStencil: MTLStencilDescriptor); cdecl;
    function frontFaceStencil : MTLStencilDescriptor; cdecl;
    procedure setBackFaceStencil(backFaceStencil: MTLStencilDescriptor); cdecl;
    function backFaceStencil : MTLStencilDescriptor; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
  end;
  TMTLDepthStencilDescriptor = class(TOCGenericImport<MTLDepthStencilDescriptorClass, MTLDepthStencilDescriptor>)  end;
  PMTLDepthStencilDescriptor = Pointer;

  MTLRenderPipelineDescriptorClass = interface(NSObjectClass)
    ['{2F9903CE-78B8-43B2-ABFB-74014E51E7FA}']
  end;
  MTLRenderPipelineDescriptor = interface(NSObject)
    ['{07059857-3A96-4B72-B8BC-081F7CCA88A3}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    procedure setVertexFunction(vertexFunction: MTLFunction); cdecl;
    function vertexFunction : MTLFunction; cdecl;
    procedure setFragmentFunction(fragmentFunction: MTLFunction); cdecl;
    function fragmentFunction : MTLFunction; cdecl;
    procedure setVertexDescriptor(vertexDescriptor: MTLVertexDescriptor); cdecl;
    function vertexDescriptor : MTLVertexDescriptor; cdecl;
    procedure setSampleCount(sampleCount: NSUInteger); cdecl;
    function sampleCount : NSUInteger; cdecl;
    procedure setRasterSampleCount(rasterSampleCount: NSUInteger); cdecl;
    function rasterSampleCount : NSUInteger; cdecl;
    procedure setAlphaToCoverageEnabled(alphaToCoverageEnabled: Boolean); cdecl;
    function isAlphaToCoverageEnabled : Boolean; cdecl;
    procedure setAlphaToOneEnabled(alphaToOneEnabled: Boolean); cdecl;
    function isAlphaToOneEnabled : Boolean; cdecl;
    procedure setRasterizationEnabled(rasterizationEnabled: Boolean); cdecl;
    function isRasterizationEnabled : Boolean; cdecl;
    function colorAttachments : MTLRenderPipelineColorAttachmentDescriptorArray; cdecl;
    procedure setDepthAttachmentPixelFormat(depthAttachmentPixelFormat: MTLPixelFormat); cdecl;
    function depthAttachmentPixelFormat : MTLPixelFormat; cdecl;
    procedure setStencilAttachmentPixelFormat(stencilAttachmentPixelFormat: MTLPixelFormat); cdecl;
    function stencilAttachmentPixelFormat : MTLPixelFormat; cdecl;
    procedure setInputPrimitiveTopology(inputPrimitiveTopology: MTLPrimitiveTopologyClass); cdecl;
    function inputPrimitiveTopology : MTLPrimitiveTopologyClass; cdecl;
    procedure setTessellationPartitionMode(tessellationPartitionMode: MTLTessellationPartitionMode); cdecl;
    function tessellationPartitionMode : MTLTessellationPartitionMode; cdecl;
    procedure setMaxTessellationFactor(maxTessellationFactor: NSUInteger); cdecl;
    function maxTessellationFactor : NSUInteger; cdecl;
    procedure setTessellationFactorScaleEnabled(tessellationFactorScaleEnabled: Boolean); cdecl;
    function isTessellationFactorScaleEnabled : Boolean; cdecl;
    procedure setTessellationFactorFormat(tessellationFactorFormat: MTLTessellationFactorFormat); cdecl;
    function tessellationFactorFormat : MTLTessellationFactorFormat; cdecl;
    procedure setTessellationControlPointIndexType(tessellationControlPointIndexType: MTLTessellationControlPointIndexType); cdecl;
    function tessellationControlPointIndexType : MTLTessellationControlPointIndexType; cdecl;
    procedure setTessellationFactorStepFunction(tessellationFactorStepFunction: MTLTessellationFactorStepFunction); cdecl;
    function tessellationFactorStepFunction : MTLTessellationFactorStepFunction; cdecl;
    procedure setTessellationOutputWindingOrder(tessellationOutputWindingOrder: MTLWinding); cdecl;
    function tessellationOutputWindingOrder : MTLWinding; cdecl;
    function vertexBuffers : MTLPipelineBufferDescriptorArray; cdecl;
    function fragmentBuffers : MTLPipelineBufferDescriptorArray; cdecl;
    procedure setSupportIndirectCommandBuffers(supportIndirectCommandBuffers: Boolean); cdecl;
    function supportIndirectCommandBuffers : Boolean; cdecl;
    procedure reset; cdecl;
  end;
  TMTLRenderPipelineDescriptor = class(TOCGenericImport<MTLRenderPipelineDescriptorClass, MTLRenderPipelineDescriptor>)  end;
  PMTLRenderPipelineDescriptor = Pointer;

  MTLRenderPassDescriptorClass = interface(NSObjectClass)
    ['{9FD99F4F-E419-4F3B-B211-254FCA348F8A}']
    {class} function renderPassDescriptor : MTLRenderPassDescriptor; cdecl;
  end;
  MTLRenderPassDescriptor = interface(NSObject)
    ['{252DD6A8-244D-413A-8F3C-31FB32BB67F2}']
    function colorAttachments : MTLRenderPassColorAttachmentDescriptorArray; cdecl;
    procedure setDepthAttachment(depthAttachment: MTLRenderPassDepthAttachmentDescriptor); cdecl;
    function depthAttachment : MTLRenderPassDepthAttachmentDescriptor; cdecl;
    procedure setStencilAttachment(stencilAttachment: MTLRenderPassStencilAttachmentDescriptor); cdecl;
    function stencilAttachment : MTLRenderPassStencilAttachmentDescriptor; cdecl;
    procedure setVisibilityResultBuffer(visibilityResultBuffer: MTLBuffer); cdecl;
    function visibilityResultBuffer : MTLBuffer; cdecl;
    procedure setRenderTargetArrayLength(renderTargetArrayLength: NSUInteger); cdecl;
    function renderTargetArrayLength : NSUInteger; cdecl;
    procedure setImageblockSampleLength(imageblockSampleLength: NSUInteger); cdecl;
    function imageblockSampleLength : NSUInteger; cdecl;
    procedure setThreadgroupMemoryLength(threadgroupMemoryLength: NSUInteger); cdecl;
    function threadgroupMemoryLength : NSUInteger; cdecl;
    procedure setTileWidth(tileWidth: NSUInteger); cdecl;
    function tileWidth : NSUInteger; cdecl;
    procedure setTileHeight(tileHeight: NSUInteger); cdecl;
    function tileHeight : NSUInteger; cdecl;
    procedure setDefaultRasterSampleCount(defaultRasterSampleCount: NSUInteger); cdecl;
    function defaultRasterSampleCount : NSUInteger; cdecl;
    procedure setRenderTargetWidth(renderTargetWidth: NSUInteger); cdecl;
    function renderTargetWidth : NSUInteger; cdecl;
    procedure setRenderTargetHeight(renderTargetHeight: NSUInteger); cdecl;
    function renderTargetHeight : NSUInteger; cdecl;
    procedure setSamplePositions(positions: PMTLSamplePosition; count: NSUInteger); cdecl;
    function getSamplePositions(positions: PMTLSamplePosition; count: NSUInteger) : NSUInteger; cdecl;
  end;
  TMTLRenderPassDescriptor = class(TOCGenericImport<MTLRenderPassDescriptorClass, MTLRenderPassDescriptor>)  end;
  PMTLRenderPassDescriptor = Pointer;

  MTLRenderPipelineReflectionClass = interface(NSObjectClass)
    ['{3D0B4500-CCC1-44E6-9674-F7955D3113F5}']
  end;
  MTLRenderPipelineReflection = interface(NSObject)
    ['{ADBA1021-90B3-4410-AAE5-30261781F995}']
    function vertexArguments : NSArray; cdecl;
    function fragmentArguments : NSArray; cdecl;
    function tileArguments : NSArray; cdecl;
  end;
  TMTLRenderPipelineReflection = class(TOCGenericImport<MTLRenderPipelineReflectionClass, MTLRenderPipelineReflection>)  end;
  PMTLRenderPipelineReflection = Pointer;

  MTLComputePipelineDescriptorClass = interface(NSObjectClass)
    ['{3664FB32-790A-47C2-8CC9-540731012A84}']
  end;
  MTLComputePipelineDescriptor = interface(NSObject)
    ['{B9EBC808-71E3-45B4-8FF9-1AA4E4287E69}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    procedure setComputeFunction(computeFunction: MTLFunction); cdecl;
    function computeFunction : MTLFunction; cdecl;
    procedure setThreadGroupSizeIsMultipleOfThreadExecutionWidth(threadGroupSizeIsMultipleOfThreadExecutionWidth: Boolean); cdecl;
    function threadGroupSizeIsMultipleOfThreadExecutionWidth : Boolean; cdecl;
    procedure setMaxTotalThreadsPerThreadgroup(maxTotalThreadsPerThreadgroup: NSUInteger); cdecl;
    function maxTotalThreadsPerThreadgroup : NSUInteger; cdecl;
    procedure setStageInputDescriptor(stageInputDescriptor: MTLStageInputOutputDescriptor); cdecl;
    function stageInputDescriptor : MTLStageInputOutputDescriptor; cdecl;
    function buffers : MTLPipelineBufferDescriptorArray; cdecl;
    procedure reset; cdecl;
  end;
  TMTLComputePipelineDescriptor = class(TOCGenericImport<MTLComputePipelineDescriptorClass, MTLComputePipelineDescriptor>)  end;
  PMTLComputePipelineDescriptor = Pointer;

  MTLComputePipelineReflectionClass = interface(NSObjectClass)
    ['{B8868236-A376-47B1-BDF1-9969F75EBBFA}']
  end;
  MTLComputePipelineReflection = interface(NSObject)
    ['{F9F607DE-8F16-47DD-9CFE-2DC272043F7D}']
    function arguments : NSArray; cdecl;
  end;
  TMTLComputePipelineReflection = class(TOCGenericImport<MTLComputePipelineReflectionClass, MTLComputePipelineReflection>)  end;
  PMTLComputePipelineReflection = Pointer;

  MTLHeapDescriptorClass = interface(NSObjectClass)
    ['{3D79443D-A1A9-4F3D-B66C-B7AD60FA842F}']
  end;
  MTLHeapDescriptor = interface(NSObject)
    ['{8C29FD29-E9F5-4499-B0D3-21A0331FA407}']
    procedure setSize(size: NSUInteger); cdecl;
    function size : NSUInteger; cdecl;
    procedure setStorageMode(storageMode: MTLStorageMode); cdecl;
    function storageMode : MTLStorageMode; cdecl;
    procedure setCpuCacheMode(cpuCacheMode: MTLCPUCacheMode); cdecl;
    function cpuCacheMode : MTLCPUCacheMode; cdecl;
  end;
  TMTLHeapDescriptor = class(TOCGenericImport<MTLHeapDescriptorClass, MTLHeapDescriptor>)  end;
  PMTLHeapDescriptor = Pointer;

  MTLIndirectCommandBufferDescriptorClass = interface(NSObjectClass)
    ['{3CDEF6D5-23D8-4DD7-A28E-9F96721EC4E5}']
  end;
  MTLIndirectCommandBufferDescriptor = interface(NSObject)
    ['{74A6FD5F-1654-40F0-A35C-000731973CF3}']
    procedure setCommandTypes(commandTypes: MTLIndirectCommandType); cdecl;
    function commandTypes : MTLIndirectCommandType; cdecl;
    procedure setInheritPipelineState(inheritPipelineState: Boolean); cdecl;
    function inheritPipelineState : Boolean; cdecl;
    procedure setInheritBuffers(inheritBuffers: Boolean); cdecl;
    function inheritBuffers : Boolean; cdecl;
    procedure setMaxVertexBufferBindCount(maxVertexBufferBindCount: NSUInteger); cdecl;
    function maxVertexBufferBindCount : NSUInteger; cdecl;
    procedure setMaxFragmentBufferBindCount(maxFragmentBufferBindCount: NSUInteger); cdecl;
    function maxFragmentBufferBindCount : NSUInteger; cdecl;
  end;
  TMTLIndirectCommandBufferDescriptor = class(TOCGenericImport<MTLIndirectCommandBufferDescriptorClass, MTLIndirectCommandBufferDescriptor>)  end;
  PMTLIndirectCommandBufferDescriptor = Pointer;

  MTLSharedEventHandleClass = interface(NSObjectClass)
    ['{E6DAEF1E-8B5D-48DB-901E-5F0B3C1442DF}']
  end;
  MTLSharedEventHandle = interface(NSObject)
    ['{197A608C-95C0-4266-865B-F8277A86A036}']
    function &label : NSString; cdecl;
  end;
  TMTLSharedEventHandle = class(TOCGenericImport<MTLSharedEventHandleClass, MTLSharedEventHandle>)  end;
  PMTLSharedEventHandle = Pointer;

  MTLArgumentDescriptorClass = interface(NSObjectClass)
    ['{B74B2404-15BD-483B-894D-4925AC00FB8A}']
    {class} function argumentDescriptor : MTLArgumentDescriptor; cdecl;
  end;
  MTLArgumentDescriptor = interface(NSObject)
    ['{5BA99F6B-5057-45A5-B717-0540133E97B7}']
    procedure setDataType(dataType: MTLDataType); cdecl;
    function dataType : MTLDataType; cdecl;
    procedure setIndex(index: NSUInteger); cdecl;
    function index : NSUInteger; cdecl;
    procedure setArrayLength(arrayLength: NSUInteger); cdecl;
    function arrayLength : NSUInteger; cdecl;
    procedure setAccess(access: MTLArgumentAccess); cdecl;
    function access : MTLArgumentAccess; cdecl;
    procedure setTextureType(textureType: MTLTextureType); cdecl;
    function textureType : MTLTextureType; cdecl;
    procedure setConstantBlockAlignment(constantBlockAlignment: NSUInteger); cdecl;
    function constantBlockAlignment : NSUInteger; cdecl;
  end;
  TMTLArgumentDescriptor = class(TOCGenericImport<MTLArgumentDescriptorClass, MTLArgumentDescriptor>)  end;
  PMTLArgumentDescriptor = Pointer;

  MTLStencilDescriptorClass = interface(NSObjectClass)
    ['{18256F3A-E36F-4C7E-8171-F300B0C4D45B}']
  end;
  MTLStencilDescriptor = interface(NSObject)
    ['{F4732280-D5F0-44D8-8D43-8BB62287A04C}']
    procedure setStencilCompareFunction(stencilCompareFunction: MTLCompareFunction); cdecl;
    function stencilCompareFunction : MTLCompareFunction; cdecl;
    procedure setStencilFailureOperation(stencilFailureOperation: MTLStencilOperation); cdecl;
    function stencilFailureOperation : MTLStencilOperation; cdecl;
    procedure setDepthFailureOperation(depthFailureOperation: MTLStencilOperation); cdecl;
    function depthFailureOperation : MTLStencilOperation; cdecl;
    procedure setDepthStencilPassOperation(depthStencilPassOperation: MTLStencilOperation); cdecl;
    function depthStencilPassOperation : MTLStencilOperation; cdecl;
    procedure setReadMask(readMask: LongWord); cdecl;
    function readMask : LongWord; cdecl;
    procedure setWriteMask(writeMask: LongWord); cdecl;
    function writeMask : LongWord; cdecl;
  end;
  TMTLStencilDescriptor = class(TOCGenericImport<MTLStencilDescriptorClass, MTLStencilDescriptor>)  end;
  PMTLStencilDescriptor = Pointer;

  MTLRenderPassAttachmentDescriptorClass = interface(NSObjectClass)
    ['{33E4B62A-C49C-41FA-AD49-FBA534DF7159}']
  end;
  MTLRenderPassAttachmentDescriptor = interface(NSObject)
    ['{F0B9B352-4066-448C-AB84-DD788FC9BAF4}']
    procedure setTexture(texture: MTLTexture); cdecl;
    function texture : MTLTexture; cdecl;
    procedure setLevel(level: NSUInteger); cdecl;
    function level : NSUInteger; cdecl;
    procedure setSlice(slice: NSUInteger); cdecl;
    function slice : NSUInteger; cdecl;
    procedure setDepthPlane(depthPlane: NSUInteger); cdecl;
    function depthPlane : NSUInteger; cdecl;
    procedure setResolveTexture(resolveTexture: MTLTexture); cdecl;
    function resolveTexture : MTLTexture; cdecl;
    procedure setResolveLevel(resolveLevel: NSUInteger); cdecl;
    function resolveLevel : NSUInteger; cdecl;
    procedure setResolveSlice(resolveSlice: NSUInteger); cdecl;
    function resolveSlice : NSUInteger; cdecl;
    procedure setResolveDepthPlane(resolveDepthPlane: NSUInteger); cdecl;
    function resolveDepthPlane : NSUInteger; cdecl;
    procedure setLoadAction(loadAction: MTLLoadAction); cdecl;
    function loadAction : MTLLoadAction; cdecl;
    procedure setStoreAction(storeAction: MTLStoreAction); cdecl;
    function storeAction : MTLStoreAction; cdecl;
    procedure setStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    function storeActionOptions : MTLStoreActionOptions; cdecl;
  end;
  TMTLRenderPassAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassAttachmentDescriptorClass, MTLRenderPassAttachmentDescriptor>)  end;
  PMTLRenderPassAttachmentDescriptor = Pointer;

  MTLRenderPassColorAttachmentDescriptorClass = interface(MTLRenderPassAttachmentDescriptorClass)
    ['{86D11781-62AA-4EDD-B360-1DC227FDA98F}']
  end;
  MTLRenderPassColorAttachmentDescriptor = interface(MTLRenderPassAttachmentDescriptor)
    ['{52C1BB69-00F2-4D27-8DA6-00C7DCEB3680}']
    procedure setClearColor(clearColor: MTLClearColor); cdecl;
    function clearColor : MTLClearColor; cdecl;
  end;
  TMTLRenderPassColorAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassColorAttachmentDescriptorClass, MTLRenderPassColorAttachmentDescriptor>)  end;
  PMTLRenderPassColorAttachmentDescriptor = Pointer;

  MTLRenderPassDepthAttachmentDescriptorClass = interface(MTLRenderPassAttachmentDescriptorClass)
    ['{F1B447AB-EF74-4393-A55B-E8B078AD9F28}']
  end;
  MTLRenderPassDepthAttachmentDescriptor = interface(MTLRenderPassAttachmentDescriptor)
    ['{9D6029A1-F934-4B52-A28C-04DAEB78F428}']
    procedure setClearDepth(clearDepth: Double); cdecl;
    function clearDepth : Double; cdecl;
    procedure setDepthResolveFilter(depthResolveFilter: MTLMultisampleDepthResolveFilter); cdecl;
    function depthResolveFilter : MTLMultisampleDepthResolveFilter; cdecl;
  end;
  TMTLRenderPassDepthAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassDepthAttachmentDescriptorClass, MTLRenderPassDepthAttachmentDescriptor>)  end;
  PMTLRenderPassDepthAttachmentDescriptor = Pointer;

  MTLRenderPassStencilAttachmentDescriptorClass = interface(MTLRenderPassAttachmentDescriptorClass)
    ['{AB7DF324-CEB8-4ACF-AAFE-544723894389}']
  end;
  MTLRenderPassStencilAttachmentDescriptor = interface(MTLRenderPassAttachmentDescriptor)
    ['{2EDC01F7-62B7-4C85-AA90-2DC20E9D8506}']
    procedure setClearStencil(clearStencil: LongWord); cdecl;
    function clearStencil : LongWord; cdecl;
    procedure setStencilResolveFilter(stencilResolveFilter: MTLMultisampleStencilResolveFilter); cdecl;
    function stencilResolveFilter : MTLMultisampleStencilResolveFilter; cdecl;
  end;
  TMTLRenderPassStencilAttachmentDescriptor = class(TOCGenericImport<MTLRenderPassStencilAttachmentDescriptorClass, MTLRenderPassStencilAttachmentDescriptor>)  end;
  PMTLRenderPassStencilAttachmentDescriptor = Pointer;

  MTLRenderPassColorAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{E7A039D2-4D74-4311-9508-C8DAA63E1DE3}']
  end;
  MTLRenderPassColorAttachmentDescriptorArray = interface(NSObject)
    ['{0611EEC2-9D06-436F-BA99-44B224646FEF}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger) : MTLRenderPassColorAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLRenderPassColorAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRenderPassColorAttachmentDescriptorArray = class(TOCGenericImport<MTLRenderPassColorAttachmentDescriptorArrayClass, MTLRenderPassColorAttachmentDescriptorArray>)  end;
  PMTLRenderPassColorAttachmentDescriptorArray = Pointer;

  MTLBufferLayoutDescriptorClass = interface(NSObjectClass)
    ['{4A5B8CC7-CA94-4A54-9717-5439F45A6045}']
  end;
  MTLBufferLayoutDescriptor = interface(NSObject)
    ['{95DDE8E2-8099-4FAF-8903-EB5E7A4E619C}']
    procedure setStride(stride: NSUInteger); cdecl;
    function stride : NSUInteger; cdecl;
    procedure setStepFunction(stepFunction: MTLStepFunction); cdecl;
    function stepFunction : MTLStepFunction; cdecl;
    procedure setStepRate(stepRate: NSUInteger); cdecl;
    function stepRate : NSUInteger; cdecl;
  end;
  TMTLBufferLayoutDescriptor = class(TOCGenericImport<MTLBufferLayoutDescriptorClass, MTLBufferLayoutDescriptor>)  end;
  PMTLBufferLayoutDescriptor = Pointer;

  MTLBufferLayoutDescriptorArrayClass = interface(NSObjectClass)
    ['{27F56753-8CC6-4971-AEB7-83959537248D}']
  end;
  MTLBufferLayoutDescriptorArray = interface(NSObject)
    ['{4AD6AA7E-CD94-44BC-9A09-E15C11476980}']
    function objectAtIndexedSubscript(index: NSUInteger) : MTLBufferLayoutDescriptor; cdecl;
    procedure setObject(bufferDesc: MTLBufferLayoutDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLBufferLayoutDescriptorArray = class(TOCGenericImport<MTLBufferLayoutDescriptorArrayClass, MTLBufferLayoutDescriptorArray>)  end;
  PMTLBufferLayoutDescriptorArray = Pointer;

  MTLAttributeDescriptorClass = interface(NSObjectClass)
    ['{7E6137DE-10CB-42A9-83D5-C7A97E311D05}']
  end;
  MTLAttributeDescriptor = interface(NSObject)
    ['{90EB98E3-F320-44C0-BCAB-CE73F832373C}']
    procedure setFormat(format: MTLAttributeFormat); cdecl;
    function format : MTLAttributeFormat; cdecl;
    procedure setOffset(offset: NSUInteger); cdecl;
    function offset : NSUInteger; cdecl;
    procedure setBufferIndex(bufferIndex: NSUInteger); cdecl;
    function bufferIndex : NSUInteger; cdecl;
  end;
  TMTLAttributeDescriptor = class(TOCGenericImport<MTLAttributeDescriptorClass, MTLAttributeDescriptor>)  end;
  PMTLAttributeDescriptor = Pointer;

  MTLAttributeDescriptorArrayClass = interface(NSObjectClass)
    ['{B738EA13-064B-4016-923C-022985BBFB84}']
  end;
  MTLAttributeDescriptorArray = interface(NSObject)
    ['{D80EBC85-BA1D-4917-B0AB-809F59202FAD}']
    function objectAtIndexedSubscript(index: NSUInteger) : MTLAttributeDescriptor; cdecl;
    procedure setObject(attributeDesc: MTLAttributeDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLAttributeDescriptorArray = class(TOCGenericImport<MTLAttributeDescriptorArrayClass, MTLAttributeDescriptorArray>)  end;
  PMTLAttributeDescriptorArray = Pointer;

  MTLStageInputOutputDescriptorClass = interface(NSObjectClass)
    ['{3C5B8E60-D888-4091-B451-EBC09A1CDA03}']
    {class} function stageInputOutputDescriptor : MTLStageInputOutputDescriptor; cdecl;
  end;
  MTLStageInputOutputDescriptor = interface(NSObject)
    ['{A78D73EC-0D62-4BDC-9137-B9A668F551D4}']
    function layouts : MTLBufferLayoutDescriptorArray; cdecl;
    function attributes : MTLAttributeDescriptorArray; cdecl;
    procedure setIndexType(indexType: MTLIndexType); cdecl;
    function indexType : MTLIndexType; cdecl;
    procedure setIndexBufferIndex(indexBufferIndex: NSUInteger); cdecl;
    function indexBufferIndex : NSUInteger; cdecl;
    procedure reset; cdecl;
  end;
  TMTLStageInputOutputDescriptor = class(TOCGenericImport<MTLStageInputOutputDescriptorClass, MTLStageInputOutputDescriptor>)  end;
  PMTLStageInputOutputDescriptor = Pointer;

  MTLPipelineBufferDescriptorClass = interface(NSObjectClass)
    ['{DB6D1469-34DD-4F1A-A21C-3E4600C1934A}']
  end;
  MTLPipelineBufferDescriptor = interface(NSObject)
    ['{3C8CEAC8-422A-42DB-977B-725B8E515A2C}']
    procedure setMutability(mutability: MTLMutability); cdecl;
    function mutability : MTLMutability; cdecl;
  end;
  TMTLPipelineBufferDescriptor = class(TOCGenericImport<MTLPipelineBufferDescriptorClass, MTLPipelineBufferDescriptor>)  end;
  PMTLPipelineBufferDescriptor = Pointer;

  MTLPipelineBufferDescriptorArrayClass = interface(NSObjectClass)
    ['{7F447EB3-D369-48A2-9439-8DD671DF20D1}']
  end;
  MTLPipelineBufferDescriptorArray = interface(NSObject)
    ['{1B975261-ED33-47BE-A60A-775A4A3747A1}']
    function objectAtIndexedSubscript(bufferIndex: NSUInteger) : MTLPipelineBufferDescriptor; cdecl;
    procedure setObject(buffer: MTLPipelineBufferDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLPipelineBufferDescriptorArray = class(TOCGenericImport<MTLPipelineBufferDescriptorArrayClass, MTLPipelineBufferDescriptorArray>)  end;
  PMTLPipelineBufferDescriptorArray = Pointer;

  MTLVertexDescriptorClass = interface(NSObjectClass)
    ['{B92E5B5F-607F-496C-BDCB-53BD8DC32014}']
    {class} function vertexDescriptor : MTLVertexDescriptor; cdecl;
  end;
  MTLVertexDescriptor = interface(NSObject)
    ['{451A2F29-F42E-42CC-9947-FEB887888ABF}']
    function layouts : MTLVertexBufferLayoutDescriptorArray; cdecl;
    function attributes : MTLVertexAttributeDescriptorArray; cdecl;
    procedure reset; cdecl;
  end;
  TMTLVertexDescriptor = class(TOCGenericImport<MTLVertexDescriptorClass, MTLVertexDescriptor>)  end;
  PMTLVertexDescriptor = Pointer;

  MTLRenderPipelineColorAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{D350C8EF-C946-4E41-9CC9-1589E0B66AD9}']
  end;
  MTLRenderPipelineColorAttachmentDescriptorArray = interface(NSObject)
    ['{E7CFF0F0-DE3D-456A-B54E-B3A5D9B44D60}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger) : MTLRenderPipelineColorAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLRenderPipelineColorAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLRenderPipelineColorAttachmentDescriptorArray = class(TOCGenericImport<MTLRenderPipelineColorAttachmentDescriptorArrayClass, MTLRenderPipelineColorAttachmentDescriptorArray>)  end;
  PMTLRenderPipelineColorAttachmentDescriptorArray = Pointer;

  MTLTileRenderPipelineColorAttachmentDescriptorClass = interface(NSObjectClass)
    ['{C4D9BC48-E9B2-4510-B0E1-F530EF4015CF}']
  end;
  MTLTileRenderPipelineColorAttachmentDescriptor = interface(NSObject)
    ['{45D3F6E6-E697-4CCD-955E-2B925D421FFB}']
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    function pixelFormat : MTLPixelFormat; cdecl;
  end;
  TMTLTileRenderPipelineColorAttachmentDescriptor = class(TOCGenericImport<MTLTileRenderPipelineColorAttachmentDescriptorClass, MTLTileRenderPipelineColorAttachmentDescriptor>)  end;
  PMTLTileRenderPipelineColorAttachmentDescriptor = Pointer;

  MTLTileRenderPipelineColorAttachmentDescriptorArrayClass = interface(NSObjectClass)
    ['{B10769A1-7923-4972-AA47-2B38AEA85C81}']
  end;
  MTLTileRenderPipelineColorAttachmentDescriptorArray = interface(NSObject)
    ['{2A7741A2-CF64-4CF0-A138-6DB3514CB559}']
    function objectAtIndexedSubscript(attachmentIndex: NSUInteger) : MTLTileRenderPipelineColorAttachmentDescriptor; cdecl;
    procedure setObject(attachment: MTLTileRenderPipelineColorAttachmentDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLTileRenderPipelineColorAttachmentDescriptorArray = class(TOCGenericImport<MTLTileRenderPipelineColorAttachmentDescriptorArrayClass, MTLTileRenderPipelineColorAttachmentDescriptorArray>)  end;
  PMTLTileRenderPipelineColorAttachmentDescriptorArray = Pointer;

  MTLVertexBufferLayoutDescriptorClass = interface(NSObjectClass)
    ['{F24A040C-99F4-4676-A45E-E03E3AFB00E7}']
  end;
  MTLVertexBufferLayoutDescriptor = interface(NSObject)
    ['{93CEA6E8-9D0A-4674-A0F8-184650144AC8}']
    procedure setStride(stride: NSUInteger); cdecl;
    function stride : NSUInteger; cdecl;
    procedure setStepFunction(stepFunction: MTLVertexStepFunction); cdecl;
    function stepFunction : MTLVertexStepFunction; cdecl;
    procedure setStepRate(stepRate: NSUInteger); cdecl;
    function stepRate : NSUInteger; cdecl;
  end;
  TMTLVertexBufferLayoutDescriptor = class(TOCGenericImport<MTLVertexBufferLayoutDescriptorClass, MTLVertexBufferLayoutDescriptor>)  end;
  PMTLVertexBufferLayoutDescriptor = Pointer;

  MTLVertexBufferLayoutDescriptorArrayClass = interface(NSObjectClass)
    ['{5A9C23AE-E651-4BA0-AC7A-934093C5B2BA}']
  end;
  MTLVertexBufferLayoutDescriptorArray = interface(NSObject)
    ['{43B7FCDC-7CEB-48E3-B981-189D678B1A6D}']
    function objectAtIndexedSubscript(index: NSUInteger) : MTLVertexBufferLayoutDescriptor; cdecl;
    procedure setObject(bufferDesc: MTLVertexBufferLayoutDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLVertexBufferLayoutDescriptorArray = class(TOCGenericImport<MTLVertexBufferLayoutDescriptorArrayClass, MTLVertexBufferLayoutDescriptorArray>)  end;
  PMTLVertexBufferLayoutDescriptorArray = Pointer;

  MTLVertexAttributeDescriptorClass = interface(NSObjectClass)
    ['{434116A4-1676-4FA2-ADD1-7C9D2FF3EFF8}']
  end;
  MTLVertexAttributeDescriptor = interface(NSObject)
    ['{DF91A57D-499D-47CE-A796-142243F5C9DA}']
    procedure setFormat(format: MTLVertexFormat); cdecl;
    function format : MTLVertexFormat; cdecl;
    procedure setOffset(offset: NSUInteger); cdecl;
    function offset : NSUInteger; cdecl;
    procedure setBufferIndex(bufferIndex: NSUInteger); cdecl;
    function bufferIndex : NSUInteger; cdecl;
  end;
  TMTLVertexAttributeDescriptor = class(TOCGenericImport<MTLVertexAttributeDescriptorClass, MTLVertexAttributeDescriptor>)  end;
  PMTLVertexAttributeDescriptor = Pointer;

  MTLVertexAttributeDescriptorArrayClass = interface(NSObjectClass)
    ['{6C6659D3-59B9-4F7D-AB1E-6EA8411A6BDB}']
  end;
  MTLVertexAttributeDescriptorArray = interface(NSObject)
    ['{EACEC432-C463-4EC1-B115-C31E09C7CD82}']
    function objectAtIndexedSubscript(index: NSUInteger) : MTLVertexAttributeDescriptor; cdecl;
    procedure setObject(attributeDesc: MTLVertexAttributeDescriptor; atIndexedSubscript: NSUInteger); cdecl;
  end;
  TMTLVertexAttributeDescriptorArray = class(TOCGenericImport<MTLVertexAttributeDescriptorArrayClass, MTLVertexAttributeDescriptorArray>)  end;
  PMTLVertexAttributeDescriptorArray = Pointer;

  MTLCaptureManagerClass = interface(NSObjectClass)
    ['{31DAC14E-AA71-47A3-994A-6301ADEE584D}']
    {class} function sharedCaptureManager : MTLCaptureManager; cdecl;
  end;
  MTLCaptureManager = interface(NSObject)
    ['{5FD0485A-242F-420E-B478-798A81EBBFBF}']
    function init : MTLCaptureManager {instancetype}; cdecl;
    function newCaptureScopeWithDevice(device: MTLDevice) : MTLCaptureScope; cdecl;
    function newCaptureScopeWithCommandQueue(commandQueue: MTLCommandQueue) : MTLCaptureScope; cdecl;
    procedure startCaptureWithDevice(device: MTLDevice); cdecl;
    procedure startCaptureWithCommandQueue(commandQueue: MTLCommandQueue); cdecl;
    procedure startCaptureWithScope(captureScope: MTLCaptureScope); cdecl;
    procedure stopCapture; cdecl;
    procedure setDefaultCaptureScope(defaultCaptureScope: MTLCaptureScope); cdecl;
    function defaultCaptureScope : MTLCaptureScope; cdecl;
    function isCapturing : Boolean; cdecl;
  end;
  TMTLCaptureManager = class(TOCGenericImport<MTLCaptureManagerClass, MTLCaptureManager>)  end;
  PMTLCaptureManager = Pointer;

  MTLSharedEventListenerClass = interface(NSObjectClass)
    ['{F01F4932-B60C-420A-9E52-35F2C004DCA8}']
  end;
  MTLSharedEventListener = interface(NSObject)
    ['{B419F971-D043-4A32-BF5D-034DD54B1171}']
    function init : MTLSharedEventListener {instancetype}; cdecl;
    function initWithDispatchQueue(dispatchQueue: dispatch_queue_t) : MTLSharedEventListener {instancetype}; cdecl;
    function dispatchQueue : dispatch_queue_t; cdecl;
  end;
  TMTLSharedEventListener = class(TOCGenericImport<MTLSharedEventListenerClass, MTLSharedEventListener>)  end;
  PMTLSharedEventListener = Pointer;


  // ===== Protocol declarations =====

  MTLDeviceClass = interface(NSObjectClass)
    ['{13D44692-0C22-4731-BA10-34B77942DA9E}']
  end;
  MTLDevice = interface(NSObject)
    ['{D5740836-B1B6-426C-935B-D0E614083878}']
    function name : NSString; cdecl;
    function registryID : UInt64; cdecl;
    function maxThreadsPerThreadgroup : MTLSize; cdecl;
    function isLowPower : Boolean; cdecl;
    function isHeadless : Boolean; cdecl;
    function isRemovable : Boolean; cdecl;
    function recommendedMaxWorkingSetSize : UInt64; cdecl;
    function isDepth24Stencil8PixelFormatSupported : Boolean; cdecl;
    function readWriteTextureSupport : MTLReadWriteTextureTier; cdecl;
    function argumentBuffersSupport : MTLArgumentBuffersTier; cdecl;
    function areRasterOrderGroupsSupported : Boolean; cdecl;
    function currentAllocatedSize : NSUInteger; cdecl;
    function newCommandQueue : MTLCommandQueue; cdecl;
    function newCommandQueueWithMaxCommandBufferCount(maxCommandBufferCount: NSUInteger) : MTLCommandQueue; cdecl;
    function heapTextureSizeAndAlignWithDescriptor(desc: MTLTextureDescriptor) : MTLSizeAndAlign; cdecl;
    function heapBufferSizeAndAlignWithLength(length: NSUInteger; options: MTLResourceOptions) : MTLSizeAndAlign; cdecl;
    function newHeapWithDescriptor(descriptor: MTLHeapDescriptor) : MTLHeap; cdecl;
    function newBufferWithLength(length: NSUInteger; options: MTLResourceOptions) : MTLBuffer; cdecl;
    function newBufferWithBytes(pointer: Pointer; length: NSUInteger; options: MTLResourceOptions) : MTLBuffer; cdecl;
    function newBufferWithBytesNoCopy(pointer: Pointer; length: NSUInteger; options: MTLResourceOptions; deallocator: TMTLDeviceNewBufferWithBytesNoCopyDeallocator) : MTLBuffer; cdecl;
    function newDepthStencilStateWithDescriptor(descriptor: MTLDepthStencilDescriptor) : MTLDepthStencilState; cdecl;
    [MethodName('newTextureWithDescriptor:')]
    function newTextureWithDescriptor(descriptor: MTLTextureDescriptor) : MTLTexture; cdecl;
    [MethodName('newTextureWithDescriptor:iosurface:plane:')]
    function newTextureWithDescriptorIosurfacePlane(descriptor: MTLTextureDescriptor; iosurface: IOSurfaceRef; plane: NSUInteger) : MTLTexture; cdecl;
    function newSamplerStateWithDescriptor(descriptor: MTLSamplerDescriptor) : MTLSamplerState; cdecl;
    function newDefaultLibrary : MTLLibrary; cdecl;
    function newDefaultLibraryWithBundle(bundle: NSBundle; error: PPointer) : MTLLibrary; cdecl;
    function newLibraryWithFile(filepath: NSString; error: PPointer) : MTLLibrary; cdecl;
    function newLibraryWithURL(url: NSURL; error: PPointer) : MTLLibrary; cdecl;
    function newLibraryWithData(data: dispatch_data_t; error: PPointer) : MTLLibrary; cdecl;
    [MethodName('newLibraryWithSource:options:error:')]
    function newLibraryWithSourceOptionsError(source: NSString; options: MTLCompileOptions; error: PPointer) : MTLLibrary; cdecl;
    [MethodName('newLibraryWithSource:options:completionHandler:')]
    procedure newLibraryWithSourceOptionsCompletionHandler(source: NSString; options: MTLCompileOptions; completionHandler: MTLNewLibraryCompletionHandler); cdecl;
    [MethodName('newRenderPipelineStateWithDescriptor:error:')]
    function newRenderPipelineStateWithDescriptorError(descriptor: MTLRenderPipelineDescriptor; error: PPointer) : MTLRenderPipelineState; cdecl;
    [MethodName('newRenderPipelineStateWithDescriptor:options:reflection:error:')]
    function newRenderPipelineStateWithDescriptorOptionsReflectionError(descriptor: MTLRenderPipelineDescriptor; options: MTLPipelineOption; reflection: PMTLAutoreleasedRenderPipelineReflection; error: PPointer) : MTLRenderPipelineState; cdecl;
    [MethodName('newRenderPipelineStateWithDescriptor:completionHandler:')]
    procedure newRenderPipelineStateWithDescriptorCompletionHandler(descriptor: MTLRenderPipelineDescriptor; completionHandler: MTLNewRenderPipelineStateCompletionHandler); cdecl;
    [MethodName('newRenderPipelineStateWithDescriptor:options:completionHandler:')]
    procedure newRenderPipelineStateWithDescriptorOptionsCompletionHandler(descriptor: MTLRenderPipelineDescriptor; options: MTLPipelineOption; completionHandler: MTLNewRenderPipelineStateWithReflectionCompletionHandler); cdecl;
    [MethodName('newComputePipelineStateWithFunction:error:')]
    function newComputePipelineStateWithFunctionError(computeFunction: MTLFunction; error: PPointer) : MTLComputePipelineState; cdecl;
    [MethodName('newComputePipelineStateWithFunction:options:reflection:error:')]
    function newComputePipelineStateWithFunctionOptionsReflectionError(computeFunction: MTLFunction; options: MTLPipelineOption; reflection: PMTLAutoreleasedComputePipelineReflection; error: PPointer) : MTLComputePipelineState; cdecl;
    [MethodName('newComputePipelineStateWithFunction:completionHandler:')]
    procedure newComputePipelineStateWithFunctionCompletionHandler(computeFunction: MTLFunction; completionHandler: MTLNewComputePipelineStateCompletionHandler); cdecl;
    [MethodName('newComputePipelineStateWithFunction:options:completionHandler:')]
    procedure newComputePipelineStateWithFunctionOptionsCompletionHandler(computeFunction: MTLFunction; options: MTLPipelineOption; completionHandler: MTLNewComputePipelineStateWithReflectionCompletionHandler); cdecl;
    [MethodName('newComputePipelineStateWithDescriptor:options:reflection:error:')]
    function newComputePipelineStateWithDescriptorOptionsReflectionError(descriptor: MTLComputePipelineDescriptor; options: MTLPipelineOption; reflection: PMTLAutoreleasedComputePipelineReflection; error: PPointer) : MTLComputePipelineState; cdecl;
    [MethodName('newComputePipelineStateWithDescriptor:options:completionHandler:')]
    procedure newComputePipelineStateWithDescriptorOptionsCompletionHandler(descriptor: MTLComputePipelineDescriptor; options: MTLPipelineOption; completionHandler: MTLNewComputePipelineStateWithReflectionCompletionHandler); cdecl;
    function newFence : MTLFence; cdecl;
    function supportsFeatureSet(featureSet: MTLFeatureSet) : Boolean; cdecl;
    function supportsTextureSampleCount(sampleCount: NSUInteger) : Boolean; cdecl;
    function minimumLinearTextureAlignmentForPixelFormat(format: MTLPixelFormat) : NSUInteger; cdecl;
    function minimumTextureBufferAlignmentForPixelFormat(format: MTLPixelFormat) : NSUInteger; cdecl;
    [MethodName('newRenderPipelineStateWithTileDescriptor:options:reflection:error:')]
    function newRenderPipelineStateWithTileDescriptorOptionsReflectionError(descriptor: MTLTileRenderPipelineDescriptor; options: MTLPipelineOption; reflection: PMTLAutoreleasedRenderPipelineReflection; error: PPointer) : MTLRenderPipelineState; cdecl;
    [MethodName('newRenderPipelineStateWithTileDescriptor:options:completionHandler:')]
    procedure newRenderPipelineStateWithTileDescriptorOptionsCompletionHandler(descriptor: MTLTileRenderPipelineDescriptor; options: MTLPipelineOption; completionHandler: MTLNewRenderPipelineStateWithReflectionCompletionHandler); cdecl;
    function maxThreadgroupMemoryLength : NSUInteger; cdecl;
    function maxArgumentBufferSamplerCount : NSUInteger; cdecl;
    function areProgrammableSamplePositionsSupported : Boolean; cdecl;
    procedure getDefaultSamplePositions(positions: PMTLSamplePosition; count: NSUInteger); cdecl;
    function newArgumentEncoderWithArguments(arguments: NSArray) : MTLArgumentEncoder; cdecl;
    function newIndirectCommandBufferWithDescriptor(descriptor: MTLIndirectCommandBufferDescriptor; maxCommandCount: NSUInteger; options: MTLResourceOptions) : MTLIndirectCommandBuffer; cdecl;
    function newEvent : MTLEvent; cdecl;
    function newSharedEvent : MTLSharedEvent; cdecl;
    function newSharedEventWithHandle(sharedEventHandle: MTLSharedEventHandle) : MTLSharedEvent; cdecl;
    function maxBufferLength : NSUInteger; cdecl;
  end;
  TMTLDevice = class(TOCGenericImport<MTLDeviceClass, MTLDevice>) end;
  PMTLDevice = Pointer;

  MTLCommandEncoder = interface(NSObject)
    ['{6FC08DBA-1CC1-44D2-BCD0-429314107B98}']
    function device : MTLDevice; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    procedure endEncoding; cdecl;
    procedure insertDebugSignpost(&string: NSString); cdecl;
    procedure pushDebugGroup(&string: NSString); cdecl;
    procedure popDebugGroup; cdecl;
  end;

  MTLHeap = interface(NSObject)
    ['{6B62994A-7182-47DC-8F99-AE1C6D82769C}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function storageMode : MTLStorageMode; cdecl;
    function cpuCacheMode : MTLCPUCacheMode; cdecl;
    function size : NSUInteger; cdecl;
    function usedSize : NSUInteger; cdecl;
    function currentAllocatedSize : NSUInteger; cdecl;
    function maxAvailableSizeWithAlignment(alignment: NSUInteger) : NSUInteger; cdecl;
    function newBufferWithLength(length: NSUInteger; options: MTLResourceOptions) : MTLBuffer; cdecl;
    function newTextureWithDescriptor(desc: MTLTextureDescriptor) : MTLTexture; cdecl;
    function setPurgeableState(state: MTLPurgeableState) : MTLPurgeableState; cdecl;
  end;

  MTLResource = interface(NSObject)
    ['{52FD0811-9354-4285-855F-C764403D9704}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function cpuCacheMode : MTLCPUCacheMode; cdecl;
    function storageMode : MTLStorageMode; cdecl;
    function setPurgeableState(state: MTLPurgeableState) : MTLPurgeableState; cdecl;
    function heap : MTLHeap; cdecl;
    function allocatedSize : NSUInteger; cdecl;
    procedure makeAliasable; cdecl;
    function isAliasable : Boolean; cdecl;
  end;

  MTLTextureClass = interface(NSObjectClass)
    ['{53787C08-9CA3-4595-BE99-6BC14C4FB440}']
  end;
  MTLTexture = interface(MTLResource)
    ['{EE934D5D-96DD-4B80-9CC7-4B0D2CF0B969}']
    function rootResource : MTLResource; cdecl;
    function parentTexture : MTLTexture; cdecl;
    function parentRelativeLevel : NSUInteger; cdecl;
    function parentRelativeSlice : NSUInteger; cdecl;
    function buffer : MTLBuffer; cdecl;
    function bufferOffset : NSUInteger; cdecl;
    function bufferBytesPerRow : NSUInteger; cdecl;
    function iosurface : IOSurfaceRef; cdecl;
    function iosurfacePlane : NSUInteger; cdecl;
    function textureType : MTLTextureType; cdecl;
    function pixelFormat : MTLPixelFormat; cdecl;
    function width : NSUInteger; cdecl;
    function height : NSUInteger; cdecl;
    function depth : NSUInteger; cdecl;
    function mipmapLevelCount : NSUInteger; cdecl;
    function sampleCount : NSUInteger; cdecl;
    function arrayLength : NSUInteger; cdecl;
    function usage : MTLTextureUsage; cdecl;
    function isFramebufferOnly : Boolean; cdecl;
    function allowGPUOptimizedContents : Boolean; cdecl;
    [MethodName('getBytes:bytesPerRow:bytesPerImage:fromRegion:mipmapLevel:slice:')]
    procedure getBytesBytesPerRowBytesPerImageFromRegionMipmapLevelSlice(pixelBytes: Pointer; bytesPerRow: NSUInteger; bytesPerImage: NSUInteger; fromRegion: MTLRegion; mipmapLevel: NSUInteger; slice: NSUInteger); cdecl;
    [MethodName('replaceRegion:mipmapLevel:slice:withBytes:bytesPerRow:bytesPerImage:')]
    procedure replaceRegionMipmapLevelSliceWithBytesBytesPerRowBytesPerImage(region: MTLRegion; mipmapLevel: NSUInteger; slice: NSUInteger; withBytes: Pointer; bytesPerRow: NSUInteger; bytesPerImage: NSUInteger); cdecl;
    [MethodName('getBytes:bytesPerRow:fromRegion:mipmapLevel:')]
    procedure getBytesBytesPerRowFromRegionMipmapLevel(pixelBytes: Pointer; bytesPerRow: NSUInteger; fromRegion: MTLRegion; mipmapLevel: NSUInteger); cdecl;
    [MethodName('replaceRegion:mipmapLevel:withBytes:bytesPerRow:')]
    procedure replaceRegionMipmapLevelWithBytesBytesPerRow(region: MTLRegion; mipmapLevel: NSUInteger; withBytes: Pointer; bytesPerRow: NSUInteger); cdecl;
    [MethodName('newTextureViewWithPixelFormat:')]
    function newTextureViewWithPixelFormat(pixelFormat: MTLPixelFormat) : MTLTexture; cdecl;
    [MethodName('newTextureViewWithPixelFormat:textureType:levels:slices:')]
    function newTextureViewWithPixelFormatTextureTypeLevelsSlices(pixelFormat: MTLPixelFormat; textureType: MTLTextureType; levels: NSRange; slices: NSRange) : MTLTexture; cdecl;
  end;
  TMTLTexture = class(TOCGenericImport<MTLTextureClass, MTLTexture>) end;
  PMTLTexture = Pointer;

  MTLBuffer = interface(MTLResource)
    ['{B7DF13D0-4BFC-4383-9675-86792216DACD}']
    function length : NSUInteger; cdecl;
    function contents : Pointer; cdecl;
    procedure didModifyRange(range: NSRange); cdecl;
    function newTextureWithDescriptor(descriptor: MTLTextureDescriptor; offset: NSUInteger; bytesPerRow: NSUInteger) : MTLTexture; cdecl;
    procedure addDebugMarker(marker: NSString; range: NSRange); cdecl;
    procedure removeAllDebugMarkers; cdecl;
  end;

  MTLFunction = interface(NSObject)
    ['{515A016D-22F1-4CAF-B8C4-0410C0113741}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function functionType : MTLFunctionType; cdecl;
    function patchType : MTLPatchType; cdecl;
    function patchControlPointCount : NSInteger; cdecl;
    function vertexAttributes : NSArray; cdecl;
    function stageInputAttributes : NSArray; cdecl;
    function name : NSString; cdecl;
    function functionConstantsDictionary : NSDictionary; cdecl;
    [MethodName('newArgumentEncoderWithBufferIndex:')]
    function newArgumentEncoderWithBufferIndex(bufferIndex: NSUInteger) : MTLArgumentEncoder; cdecl;
    [MethodName('newArgumentEncoderWithBufferIndex:reflection:')]
    function newArgumentEncoderWithBufferIndexReflection(bufferIndex: NSUInteger; reflection: PMTLAutoreleasedArgument) : MTLArgumentEncoder; cdecl;
  end;

  MTLLibrary = interface(NSObject)
    ['{4298B0BA-D300-43B0-9BCF-A45675B536EA}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    [MethodName('newFunctionWithName:')]
    function newFunctionWithName(functionName: NSString) : MTLFunction; cdecl;
    [MethodName('newFunctionWithName:constantValues:error:')]
    function newFunctionWithNameConstantValuesError(name: NSString; constantValues: MTLFunctionConstantValues; error: PPointer) : MTLFunction; cdecl;
    [MethodName('newFunctionWithName:constantValues:completionHandler:')]
    procedure newFunctionWithNameConstantValuesCompletionHandler(name: NSString; constantValues: MTLFunctionConstantValues; completionHandler: TMTLLibraryNewFunctionWithNameConstantValuesCompletionHandler); cdecl;
    function functionNames : NSArray; cdecl;
  end;

  MTLArgumentEncoder = interface(NSObject)
    ['{9BA6ED55-53E0-4D44-8CF8-4EF551A26722}']
    function device : MTLDevice; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function encodedLength : NSUInteger; cdecl;
    function alignment : NSUInteger; cdecl;
    [MethodName('setArgumentBuffer:offset:')]
    procedure setArgumentBufferOffset(argumentBuffer: MTLBuffer; offset: NSUInteger); cdecl;
    [MethodName('setArgumentBuffer:startOffset:arrayElement:')]
    procedure setArgumentBufferStartOffsetArrayElement(argumentBuffer: MTLBuffer; startOffset: NSUInteger; arrayElement: NSUInteger); cdecl;
    procedure setBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBuffers(buffers: Pointer; offsets: NSUInteger; withRange: NSRange); cdecl;
    procedure setTexture(texture: MTLTexture; atIndex: NSUInteger); cdecl;
    procedure setTextures(textures: Pointer; withRange: NSRange); cdecl;
    procedure setSamplerState(sampler: MTLSamplerState; atIndex: NSUInteger); cdecl;
    procedure setSamplerStates(samplers: Pointer; withRange: NSRange); cdecl;
    function constantDataAtIndex(index: NSUInteger) : Pointer; cdecl;
    procedure setRenderPipelineState(pipeline: MTLRenderPipelineState; atIndex: NSUInteger); cdecl;
    procedure setRenderPipelineStates(pipelines: Pointer; withRange: NSRange); cdecl;
    procedure setIndirectCommandBuffer(indirectCommandBuffer: MTLIndirectCommandBuffer; atIndex: NSUInteger); cdecl;
    procedure setIndirectCommandBuffers(buffers: Pointer; withRange: NSRange); cdecl;
    function newArgumentEncoderForBufferAtIndex(index: NSUInteger) : MTLArgumentEncoder; cdecl;
  end;

  MTLCommandQueue = interface(NSObject)
    ['{A0E91871-C819-44B7-957A-B5E6050C9C51}']
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function commandBuffer : MTLCommandBuffer; cdecl;
    function commandBufferWithUnretainedReferences : MTLCommandBuffer; cdecl;
    procedure insertDebugCaptureBoundary; cdecl;
  end;

  MTLDepthStencilState = interface(NSObject)
    ['{1C90F0F6-153A-4FC8-B3AA-6051232B3500}']
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
  end;

  MTLSamplerState = interface(NSObject)
    ['{FBA5E937-E7CC-4041-9695-51C25E8FA426}']
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
  end;

  MTLRenderPipelineState = interface(NSObject)
    ['{6CF8C67F-5E88-423F-87F3-788FE7EFAE1D}']
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function maxTotalThreadsPerThreadgroup : NSUInteger; cdecl;
    function threadgroupSizeMatchesTileSize : Boolean; cdecl;
    function imageblockSampleLength : NSUInteger; cdecl;
    function imageblockMemoryLengthForDimensions(imageblockDimensions: MTLSize) : NSUInteger; cdecl;
    function supportIndirectCommandBuffers : Boolean; cdecl;
  end;

  MTLComputePipelineState = interface(NSObject)
    ['{5F67B318-EBA2-4BE7-8D08-79554B59CF86}']
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function maxTotalThreadsPerThreadgroup : NSUInteger; cdecl;
    function threadExecutionWidth : NSUInteger; cdecl;
    function staticThreadgroupMemoryLength : NSUInteger; cdecl;
    function imageblockMemoryLengthForDimensions(imageblockDimensions: MTLSize) : NSUInteger; cdecl;
  end;

  MTLFence = interface(NSObject)
    ['{DCF0FC28-16D9-414D-A52B-E224AE23A6EF}']
    function device : MTLDevice; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
  end;

  MTLIndirectRenderCommandEncoder = interface(IObjectiveC)
    ['{E213AEC0-C361-4601-AC9F-988B78FF1ECF}']
  end;

  MTLIndirectComputeCommandEncoder = interface(IObjectiveC)
   ['{251ED9FA-B28A-4065-B8B5-4C973AD1E57F}']
  end;

  MTLIndirectCommandBuffer = interface(MTLResource)
    ['{FEE36D3A-5346-4E7A-B636-6331E039809F}']
    function size : NSUInteger; cdecl;
    procedure resetWithRange(range: NSRange); cdecl;
    function indirectRenderCommandAtIndex(commandIndex: NSUInteger) : MTLIndirectRenderCommand; cdecl;
  end;

  MTLEvent = interface(NSObject)
    ['{D617B463-8008-4B4E-9182-59AB88E165E9}']
    function device : MTLDevice; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
  end;

  MTLSharedEvent = interface(MTLEvent)
    ['{1DA7324B-A022-4AFA-9883-3F60762EF435}']
    procedure notifyListener(listener: MTLSharedEventListener; atValue: UInt64; block: MTLSharedEventNotificationBlock); cdecl;
    function newSharedEventHandle : MTLSharedEventHandle; cdecl;
    procedure setSignaledValue(signaledValue: UInt64); cdecl;
    function signaledValue : UInt64; cdecl;
  end;

  MTLBlitCommandEncoder = interface(MTLCommandEncoder)
    ['{506FC0AB-7046-4A94-BC71-FEF166E10296}']
    procedure synchronizeResource(resource: MTLResource); cdecl;
    procedure synchronizeTexture(texture: MTLTexture; slice: NSUInteger; level: NSUInteger); cdecl;
    [MethodName('copyFromTexture:sourceSlice:sourceLevel:sourceOrigin:sourceSize:toTexture:destinationSlice:destinationLevel:destinationOrigin:')]
    procedure copyFromTextureSourceSliceSourceLevelSourceOriginSourceSizeToTextureDestinationSliceDestinationLevelDestinationOrigin(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; sourceOrigin: MTLOrigin; sourceSize: MTLSize; toTexture: Pointer; destinationSlice: NSUInteger; destinationLevel: NSUInteger; destinationOrigin: MTLOrigin); cdecl;
    [MethodName('copyFromBuffer:sourceOffset:sourceBytesPerRow:sourceBytesPerImage:sourceSize:toTexture:destinationSlice:destinationLevel:destinationOrigin:')]
    procedure copyFromBufferSourceOffsetSourceBytesPerRowSourceBytesPerImageSourceSizeToTextureDestinationSliceDestinationLevelDestinationOrigin(sourceBuffer: Pointer; sourceOffset: NSUInteger; sourceBytesPerRow: NSUInteger; sourceBytesPerImage: NSUInteger; sourceSize: MTLSize; toTexture: Pointer; destinationSlice: NSUInteger; destinationLevel: NSUInteger; destinationOrigin: MTLOrigin); cdecl;
    [MethodName('copyFromBuffer:sourceOffset:sourceBytesPerRow:sourceBytesPerImage:sourceSize:toTexture:destinationSlice:destinationLevel:destinationOrigin:options:')]
    procedure copyFromBufferSourceOffsetSourceBytesPerRowSourceBytesPerImageSourceSizeToTextureDestinationSliceDestinationLevelDestinationOriginOptions(sourceBuffer: Pointer; sourceOffset: NSUInteger; sourceBytesPerRow: NSUInteger; sourceBytesPerImage: NSUInteger; sourceSize: MTLSize; toTexture: Pointer; destinationSlice: NSUInteger; destinationLevel: NSUInteger; destinationOrigin: MTLOrigin; options: MTLBlitOption); cdecl;
    [MethodName('copyFromTexture:sourceSlice:sourceLevel:sourceOrigin:sourceSize:toBuffer:destinationOffset:destinationBytesPerRow:destinationBytesPerImage:')]
    procedure copyFromTextureSourceSliceSourceLevelSourceOriginSourceSizeToBufferDestinationOffsetDestinationBytesPerRowDestinationBytesPerImage(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; sourceOrigin: MTLOrigin; sourceSize: MTLSize; toBuffer: Pointer; destinationOffset: NSUInteger; destinationBytesPerRow: NSUInteger; destinationBytesPerImage: NSUInteger); cdecl;
    [MethodName('copyFromTexture:sourceSlice:sourceLevel:sourceOrigin:sourceSize:toBuffer:destinationOffset:destinationBytesPerRow:destinationBytesPerImage:options:')]
    procedure copyFromTextureSourceSliceSourceLevelSourceOriginSourceSizeToBufferDestinationOffsetDestinationBytesPerRowDestinationBytesPerImageOptions(sourceTexture: Pointer; sourceSlice: NSUInteger; sourceLevel: NSUInteger; sourceOrigin: MTLOrigin; sourceSize: MTLSize; toBuffer: Pointer; destinationOffset: NSUInteger; destinationBytesPerRow: NSUInteger; destinationBytesPerImage: NSUInteger; options: MTLBlitOption); cdecl;
    procedure generateMipmapsForTexture(texture: MTLTexture); cdecl;
    procedure fillBuffer(buffer: MTLBuffer; range: NSRange; value: Byte); cdecl;
    [MethodName('copyFromBuffer:sourceOffset:toBuffer:destinationOffset:size:')]
    procedure copyFromBufferSourceOffsetToBufferDestinationOffsetSize(sourceBuffer: Pointer; sourceOffset: NSUInteger; toBuffer: Pointer; destinationOffset: NSUInteger; size: NSUInteger); cdecl;
    procedure updateFence(fence: MTLFence); cdecl;
    procedure waitForFence(fence: MTLFence); cdecl;
    [MethodName('optimizeContentsForGPUAccess:')]
    procedure optimizeContentsForGPUAccess(texture: MTLTexture); cdecl;
    [MethodName('optimizeContentsForGPUAccess:slice:level:')]
    procedure optimizeContentsForGPUAccessSliceLevel(texture: MTLTexture; slice: NSUInteger; level: NSUInteger); cdecl;
    [MethodName('optimizeContentsForCPUAccess:')]
    procedure optimizeContentsForCPUAccess(texture: MTLTexture); cdecl;
    [MethodName('optimizeContentsForCPUAccess:slice:level:')]
    procedure optimizeContentsForCPUAccessSliceLevel(texture: MTLTexture; slice: NSUInteger; level: NSUInteger); cdecl;
    procedure resetCommandsInBuffer(buffer: MTLIndirectCommandBuffer; withRange: NSRange); cdecl;
    procedure copyIndirectCommandBuffer(source: MTLIndirectCommandBuffer; sourceRange: NSRange; destination: MTLIndirectCommandBuffer; destinationIndex: NSUInteger); cdecl;
    procedure optimizeIndirectCommandBuffer(indirectCommandBuffer: MTLIndirectCommandBuffer; withRange: NSRange); cdecl;
  end;

  MTLRenderCommandEncoder = interface(MTLCommandEncoder)
    ['{AC616C94-803C-4BC3-8FBA-55C24184078C}']
    procedure setRenderPipelineState(pipelineState: MTLRenderPipelineState); cdecl;
    procedure setVertexBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVertexBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVertexBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setVertexBuffers(buffers: Pointer; offsets: NSUInteger; withRange: NSRange); cdecl;
    procedure setVertexTexture(texture: MTLTexture; atIndex: NSUInteger); cdecl;
    procedure setVertexTextures(textures: Pointer; withRange: NSRange); cdecl;
    [MethodName('setVertexSamplerState:atIndex:')]
    procedure setVertexSamplerStateAtIndex(sampler: MTLSamplerState; atIndex: NSUInteger); cdecl;
    [MethodName('setVertexSamplerStates:withRange:')]
    procedure setVertexSamplerStatesWithRange(samplers: Pointer; withRange: NSRange); cdecl;
    [MethodName('setVertexSamplerState:lodMinClamp:lodMaxClamp:atIndex:')]
    procedure setVertexSamplerStateLodMinClampLodMaxClampAtIndex(sampler: MTLSamplerState; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); cdecl;
    [MethodName('setVertexSamplerStates:lodMinClamps:lodMaxClamps:withRange:')]
    procedure setVertexSamplerStatesLodMinClampsLodMaxClampsWithRange(samplers: Pointer; lodMinClamps: Single; lodMaxClamps: Single; withRange: NSRange); cdecl;
    procedure setViewport(viewport: MTLViewport); cdecl;
    procedure setViewports(viewports: Pointer; count: NSUInteger); cdecl;
    procedure setFrontFacingWinding(frontFacingWinding: MTLWinding); cdecl;
    procedure setCullMode(cullMode: MTLCullMode); cdecl;
    procedure setDepthClipMode(depthClipMode: MTLDepthClipMode); cdecl;
    procedure setDepthBias(depthBias: Single; slopeScale: Single; clamp: Single); cdecl;
    procedure setScissorRect(rect: MTLScissorRect); cdecl;
    procedure setScissorRects(scissorRects: MTLScissorRect; count: NSUInteger); cdecl;
    procedure setTriangleFillMode(fillMode: MTLTriangleFillMode); cdecl;
    procedure setFragmentBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentBuffers(buffers: Pointer; offsets: NSUInteger; withRange: NSRange); cdecl;
    procedure setFragmentTexture(texture: MTLTexture; atIndex: NSUInteger); cdecl;
    procedure setFragmentTextures(textures: Pointer; withRange: NSRange); cdecl;
    [MethodName('setFragmentSamplerState:atIndex:')]
    procedure setFragmentSamplerStateAtIndex(sampler: MTLSamplerState; atIndex: NSUInteger); cdecl;
    [MethodName('setFragmentSamplerStates:withRange:')]
    procedure setFragmentSamplerStatesWithRange(samplers: Pointer; withRange: NSRange); cdecl;
    [MethodName('setFragmentSamplerState:lodMinClamp:lodMaxClamp:atIndex:')]
    procedure setFragmentSamplerStateLodMinClampLodMaxClampAtIndex(sampler: MTLSamplerState; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); cdecl;
    [MethodName('setFragmentSamplerStates:lodMinClamps:lodMaxClamps:withRange:')]
    procedure setFragmentSamplerStatesLodMinClampsLodMaxClampsWithRange(samplers: Pointer; lodMinClamps: Single; lodMaxClamps: Single; withRange: NSRange); cdecl;
    procedure setBlendColorRed(red: Single; green: Single; blue: Single; alpha: Single); cdecl;
    procedure setDepthStencilState(depthStencilState: MTLDepthStencilState); cdecl;
    procedure setStencilReferenceValue(referenceValue: LongWord); cdecl;
    procedure setStencilFrontReferenceValue(frontReferenceValue: LongWord; backReferenceValue: LongWord); cdecl;
    procedure setVisibilityResultMode(mode: MTLVisibilityResultMode; offset: NSUInteger); cdecl;
    procedure setColorStoreAction(storeAction: MTLStoreAction; atIndex: NSUInteger); cdecl;
    procedure setDepthStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setStencilStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setColorStoreActionOptions(storeActionOptions: MTLStoreActionOptions; atIndex: NSUInteger); cdecl;
    procedure setDepthStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    procedure setStencilStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    [MethodName('drawPrimitives:vertexStart:vertexCount:instanceCount:')]
    procedure drawPrimitivesVertexStartVertexCountInstanceCount(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger; instanceCount: NSUInteger); cdecl;
    [MethodName('drawPrimitives:vertexStart:vertexCount:')]
    procedure drawPrimitivesVertexStartVertexCount(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger); cdecl;
    [MethodName('drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:instanceCount:')]
    procedure drawIndexedPrimitivesIndexCountIndexTypeIndexBufferIndexBufferOffsetInstanceCount(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: MTLBuffer; indexBufferOffset: NSUInteger; instanceCount: NSUInteger); cdecl;
    [MethodName('drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:')]
    procedure drawIndexedPrimitivesIndexCountIndexTypeIndexBufferIndexBufferOffset(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: MTLBuffer; indexBufferOffset: NSUInteger); cdecl;
    [MethodName('drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:')]
    procedure drawPrimitivesVertexStartVertexCountInstanceCountBaseInstance(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger); cdecl;
    [MethodName('drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:instanceCount:baseVertex:baseInstance:')]
    procedure drawIndexedPrimitivesIndexCountIndexTypeIndexBufferIndexBufferOffsetInstanceCountBaseVertexBaseInstance(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: MTLBuffer; indexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseVertex: NSInteger; baseInstance: NSUInteger); cdecl;
    [MethodName('drawPrimitives:indirectBuffer:indirectBufferOffset:')]
    procedure drawPrimitivesIndirectBufferIndirectBufferOffset(primitiveType: MTLPrimitiveType; indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger); cdecl;
    [MethodName('drawIndexedPrimitives:indexType:indexBuffer:indexBufferOffset:indirectBuffer:indirectBufferOffset:')]
    procedure drawIndexedPrimitivesIndexTypeIndexBufferIndexBufferOffsetIndirectBufferIndirectBufferOffset(primitiveType: MTLPrimitiveType; indexType: MTLIndexType; indexBuffer: MTLBuffer; indexBufferOffset: NSUInteger; indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger); cdecl;
    procedure textureBarrier; cdecl;
    procedure updateFence(fence: MTLFence; afterStages: MTLRenderStages); cdecl;
    procedure waitForFence(fence: MTLFence; beforeStages: MTLRenderStages); cdecl;
    procedure setTessellationFactorBuffer(buffer: MTLBuffer; offset: NSUInteger; instanceStride: NSUInteger); cdecl;
    procedure setTessellationFactorScale(scale: Single); cdecl;
    [MethodName('drawPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:instanceCount:baseInstance:')]
    procedure drawPatchesPatchStartPatchCountPatchIndexBufferPatchIndexBufferOffsetInstanceCountBaseInstance(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: MTLBuffer; patchIndexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger); cdecl;
    [MethodName('drawPatches:patchIndexBuffer:patchIndexBufferOffset:indirectBuffer:indirectBufferOffset:')]
    procedure drawPatchesPatchIndexBufferPatchIndexBufferOffsetIndirectBufferIndirectBufferOffset(numberOfPatchControlPoints: NSUInteger; patchIndexBuffer: MTLBuffer; patchIndexBufferOffset: NSUInteger; indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger); cdecl;
    [MethodName('drawIndexedPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:controlPointIndexBuffer:controlPointIndexBufferOffset:instanceCount:baseInstance:')]
    procedure drawIndexedPatchesPatchStartPatchCountPatchIndexBufferPatchIndexBufferOffsetControlPointIndexBufferControlPointIndexBufferOffsetInstanceCountBaseInstance(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: MTLBuffer; patchIndexBufferOffset: NSUInteger; controlPointIndexBuffer: MTLBuffer; controlPointIndexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger); cdecl;
    [MethodName('drawIndexedPatches:patchIndexBuffer:patchIndexBufferOffset:controlPointIndexBuffer:controlPointIndexBufferOffset:indirectBuffer:indirectBufferOffset:')]
    procedure drawIndexedPatchesPatchIndexBufferPatchIndexBufferOffsetControlPointIndexBufferControlPointIndexBufferOffsetIndirectBufferIndirectBufferOffset(numberOfPatchControlPoints: NSUInteger; patchIndexBuffer: MTLBuffer; patchIndexBufferOffset: NSUInteger; controlPointIndexBuffer: MTLBuffer; controlPointIndexBufferOffset: NSUInteger; indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger); cdecl;
    function tileWidth : NSUInteger; cdecl;
    function tileHeight : NSUInteger; cdecl;
    procedure setTileBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setTileBuffers(buffers: Pointer; offsets: NSUInteger; withRange: NSRange); cdecl;
    procedure setTileTexture(texture: MTLTexture; atIndex: NSUInteger); cdecl;
    procedure setTileTextures(textures: Pointer; withRange: NSRange); cdecl;
    [MethodName('setTileSamplerState:atIndex:')]
    procedure setTileSamplerStateAtIndex(sampler: MTLSamplerState; atIndex: NSUInteger); cdecl;
    [MethodName('setTileSamplerStates:withRange:')]
    procedure setTileSamplerStatesWithRange(samplers: Pointer; withRange: NSRange); cdecl;
    [MethodName('setTileSamplerState:lodMinClamp:lodMaxClamp:atIndex:')]
    procedure setTileSamplerStateLodMinClampLodMaxClampAtIndex(sampler: MTLSamplerState; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); cdecl;
    [MethodName('setTileSamplerStates:lodMinClamps:lodMaxClamps:withRange:')]
    procedure setTileSamplerStatesLodMinClampsLodMaxClampsWithRange(samplers: Pointer; lodMinClamps: Single; lodMaxClamps: Single; withRange: NSRange); cdecl;
    procedure dispatchThreadsPerTile(threadsPerTile: MTLSize); cdecl;
    procedure setThreadgroupMemoryLength(length: NSUInteger; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure useResource(resource: MTLResource; usage: MTLResourceUsage); cdecl;
    procedure useResources(resources: Pointer; count: NSUInteger; usage: MTLResourceUsage); cdecl;
    procedure useHeap(heap: MTLHeap); cdecl;
    procedure useHeaps(heaps: Pointer; count: NSUInteger); cdecl;
    [MethodName('executeCommandsInBuffer:withRange:')]
    procedure executeCommandsInBufferWithRange(indirectCommandBuffer: MTLIndirectCommandBuffer; withRange: NSRange); cdecl;
    [MethodName('executeCommandsInBuffer:indirectBuffer:indirectBufferOffset:')]
    procedure executeCommandsInBufferIndirectBufferIndirectBufferOffset(indirectCommandbuffer: MTLIndirectCommandBuffer; indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger); cdecl;
    procedure memoryBarrierWithScope(scope: MTLBarrierScope; afterStages: MTLRenderStages; beforeStages: MTLRenderStages); cdecl;
    procedure memoryBarrierWithResources(resources: Pointer; count: NSUInteger; afterStages: MTLRenderStages; beforeStages: MTLRenderStages); cdecl;
  end;

  MTLParallelRenderCommandEncoder = interface(MTLCommandEncoder)
    ['{932F58F4-6B5D-4CA8-B304-4B58DF7BB91E}']
    function renderCommandEncoder : MTLRenderCommandEncoder; cdecl;
    procedure setColorStoreAction(storeAction: MTLStoreAction; atIndex: NSUInteger); cdecl;
    procedure setDepthStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setStencilStoreAction(storeAction: MTLStoreAction); cdecl;
    procedure setColorStoreActionOptions(storeActionOptions: MTLStoreActionOptions; atIndex: NSUInteger); cdecl;
    procedure setDepthStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
    procedure setStencilStoreActionOptions(storeActionOptions: MTLStoreActionOptions); cdecl;
  end;

  MTLComputeCommandEncoder = interface(MTLCommandEncoder)
    ['{604FD071-CD8A-4EE0-9685-EED4E4D38596}']
    function dispatchType : MTLDispatchType; cdecl;
    procedure setComputePipelineState(state: MTLComputePipelineState); cdecl;
    procedure setBytes(bytes: Pointer; length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBufferOffset(offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setBuffers(buffers: Pointer; offsets: NSUInteger; withRange: NSRange); cdecl;
    procedure setTexture(texture: MTLTexture; atIndex: NSUInteger); cdecl;
    procedure setTextures(textures: Pointer; withRange: NSRange); cdecl;
    [MethodName('setSamplerState:atIndex:')]
    procedure setSamplerStateAtIndex(sampler: MTLSamplerState; atIndex: NSUInteger); cdecl;
    [MethodName('setSamplerStates:withRange:')]
    procedure setSamplerStatesWithRange(samplers: Pointer; withRange: NSRange); cdecl;
    [MethodName('setSamplerState:lodMinClamp:lodMaxClamp:atIndex:')]
    procedure setSamplerStateLodMinClampLodMaxClampAtIndex(sampler: MTLSamplerState; lodMinClamp: Single; lodMaxClamp: Single; atIndex: NSUInteger); cdecl;
    [MethodName('setSamplerStates:lodMinClamps:lodMaxClamps:withRange:')]
    procedure setSamplerStatesLodMinClampsLodMaxClampsWithRange(samplers: Pointer; lodMinClamps: Single; lodMaxClamps: Single; withRange: NSRange); cdecl;
    procedure setThreadgroupMemoryLength(length: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setImageblockWidth(width: NSUInteger; height: NSUInteger); cdecl;
    procedure setStageInRegion(region: MTLRegion); cdecl;
    procedure setStageInRegionWithIndirectBuffer(indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger); cdecl;
    procedure dispatchThreadgroups(threadgroupsPerGrid: MTLSize; threadsPerThreadgroup: MTLSize); cdecl;
    procedure dispatchThreadgroupsWithIndirectBuffer(indirectBuffer: MTLBuffer; indirectBufferOffset: NSUInteger; threadsPerThreadgroup: MTLSize); cdecl;
    procedure dispatchThreads(threadsPerGrid: MTLSize; threadsPerThreadgroup: MTLSize); cdecl;
    procedure updateFence(fence: MTLFence); cdecl;
    procedure waitForFence(fence: MTLFence); cdecl;
    procedure useResource(resource: MTLResource; usage: MTLResourceUsage); cdecl;
    procedure useResources(resources: Pointer; count: NSUInteger; usage: MTLResourceUsage); cdecl;
    procedure useHeap(heap: MTLHeap); cdecl;
    procedure useHeaps(heaps: Pointer; count: NSUInteger); cdecl;
    procedure memoryBarrierWithScope(scope: MTLBarrierScope); cdecl;
    procedure memoryBarrierWithResources(resources: Pointer; count: NSUInteger); cdecl;
  end;

  MTLDrawable = interface(NSObject)
    ['{7B979164-8DE0-4B2A-B9D1-EFD006E35D3B}']
    procedure present; cdecl;
    procedure presentAtTime(presentationTime: CFTimeInterval); cdecl;
    procedure presentAfterMinimumDuration(duration: CFTimeInterval); cdecl;
    procedure addPresentedHandler(block: MTLDrawablePresentedHandler); cdecl;
    function presentedTime : CFTimeInterval; cdecl;
    function drawableID : NSUInteger; cdecl;
  end;

  MTLCommandBuffer = interface(NSObject)
    ['{FDA9D69B-A21A-49B5-8EF2-93443D6A6EC2}']
    function device : MTLDevice; cdecl;
    function commandQueue : MTLCommandQueue; cdecl;
    function retainedReferences : Boolean; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function kernelStartTime : CFTimeInterval; cdecl;
    function kernelEndTime : CFTimeInterval; cdecl;
    function GPUStartTime : CFTimeInterval; cdecl;
    function GPUEndTime : CFTimeInterval; cdecl;
    procedure enqueue; cdecl;
    procedure commit; cdecl;
    procedure addScheduledHandler(block: MTLCommandBufferHandler); cdecl;
    [MethodName('presentDrawable:')]
    procedure presentDrawable(drawable: MTLDrawable); cdecl;
    [MethodName('presentDrawable:atTime:')]
    procedure presentDrawableAtTime(drawable: MTLDrawable; atTime: CFTimeInterval); cdecl;
    [MethodName('presentDrawable:afterMinimumDuration:')]
    procedure presentDrawableAfterMinimumDuration(drawable: MTLDrawable; afterMinimumDuration: CFTimeInterval); cdecl;
    procedure waitUntilScheduled; cdecl;
    procedure addCompletedHandler(block: MTLCommandBufferHandler); cdecl;
    procedure waitUntilCompleted; cdecl;
    function status : MTLCommandBufferStatus; cdecl;
    function error : NSError; cdecl;
    function blitCommandEncoder : MTLBlitCommandEncoder; cdecl;
    function renderCommandEncoderWithDescriptor(renderPassDescriptor: MTLRenderPassDescriptor) : MTLRenderCommandEncoder; cdecl;
    function computeCommandEncoder : MTLComputeCommandEncoder; cdecl;
    function computeCommandEncoderWithDispatchType(dispatchType: MTLDispatchType) : MTLComputeCommandEncoder; cdecl;
    procedure encodeWaitForEvent(event: MTLEvent; value: UInt64); cdecl;
    procedure encodeSignalEvent(event: MTLEvent; value: UInt64); cdecl;
    function parallelRenderCommandEncoderWithDescriptor(renderPassDescriptor: MTLRenderPassDescriptor) : MTLParallelRenderCommandEncoder; cdecl;
    procedure pushDebugGroup(&string: NSString); cdecl;
    procedure popDebugGroup; cdecl;
  end;

  MTLCaptureScope = interface(NSObject)
    ['{3C068601-3E58-4321-BF92-06B7FC5F0258}']
    procedure beginScope; cdecl;
    procedure endScope; cdecl;
    procedure setLabel(&label: NSString); cdecl;
    function &label : NSString; cdecl;
    function device : MTLDevice; cdecl;
    function commandQueue : MTLCommandQueue; cdecl;
  end;

  MTLIndirectRenderCommand = interface(NSObject)
    ['{0A78AADD-AA14-4BB5-B1F4-11E483CBB082}']
    procedure setRenderPipelineState(pipelineState: MTLRenderPipelineState); cdecl;
    procedure setVertexBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure setFragmentBuffer(buffer: MTLBuffer; offset: NSUInteger; atIndex: NSUInteger); cdecl;
    procedure drawPatches(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: MTLBuffer; patchIndexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger; tessellationFactorBuffer: MTLBuffer; tessellationFactorBufferOffset: NSUInteger; tessellationFactorBufferInstanceStride: NSUInteger); cdecl;
    procedure drawIndexedPatches(numberOfPatchControlPoints: NSUInteger; patchStart: NSUInteger; patchCount: NSUInteger; patchIndexBuffer: MTLBuffer; patchIndexBufferOffset: NSUInteger; controlPointIndexBuffer: MTLBuffer; controlPointIndexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger; tessellationFactorBuffer: MTLBuffer; tessellationFactorBufferOffset: NSUInteger; tessellationFactorBufferInstanceStride: NSUInteger); cdecl;
    procedure drawPrimitives(primitiveType: MTLPrimitiveType; vertexStart: NSUInteger; vertexCount: NSUInteger; instanceCount: NSUInteger; baseInstance: NSUInteger); cdecl;
    procedure drawIndexedPrimitives(primitiveType: MTLPrimitiveType; indexCount: NSUInteger; indexType: MTLIndexType; indexBuffer: MTLBuffer; indexBufferOffset: NSUInteger; instanceCount: NSUInteger; baseVertex: NSInteger; baseInstance: NSUInteger); cdecl;
    procedure reset; cdecl;
  end;


  // ===== Exported string consts =====

  function MTLLibraryErrorDomain: NSString;
  function MTLCommandBufferErrorDomain: NSString;


  // ===== External functions =====

const
  libMetal = '/System/Library/Frameworks/Metal.framework/Metal';

function MTLOriginMake(x: NSUInteger; y: NSUInteger; z: NSUInteger): MTLOrigin; cdecl; external libMetal name _PU + 'MTLOriginMake';
function MTLSizeMake(width: NSUInteger; height: NSUInteger; depth: NSUInteger): MTLSize; cdecl; external libMetal name _PU + 'MTLSizeMake';
function MTLRegionMake1D(x: NSUInteger; width: NSUInteger): MTLRegion; cdecl; external libMetal name _PU + 'MTLRegionMake1D';
function MTLRegionMake2D(x: NSUInteger; y: NSUInteger; width: NSUInteger; height: NSUInteger): MTLRegion; cdecl; external libMetal name _PU + 'MTLRegionMake2D';
function MTLRegionMake3D(x: NSUInteger; y: NSUInteger; z: NSUInteger; width: NSUInteger; height: NSUInteger; depth: NSUInteger): MTLRegion; cdecl; external libMetal name _PU + 'MTLRegionMake3D';
function MTLSamplePositionMake(x: Single; y: Single): MTLSamplePosition; cdecl; external libMetal name _PU + 'MTLSamplePositionMake';
function MTLCreateSystemDefaultDevice: Pointer; cdecl; external libMetal name _PU + 'MTLCreateSystemDefaultDevice';
function MTLCopyAllDevices: Pointer {NSArray}; cdecl; external libMetal name _PU + 'MTLCopyAllDevices';
function MTLClearColorMake(red: Double; green: Double; blue: Double; alpha: Double): MTLClearColor; cdecl; external libMetal name _PU + 'MTLClearColorMake';


implementation

{$IF defined(MACOS64)}

uses
  Posix.Dlfcn;

var
  MetalModule: THandle;

{$ENDIF}


function MTLLibraryErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLLibraryErrorDomain');
end;

function MTLCommandBufferErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libMetal, 'MTLCommandBufferErrorDomain');
end;

{$IF defined(MACOS64)}

initialization
  MetalModule := dlopen(MarshaledAString(libMetal), RTLD_LAZY);

finalization
  dlclose(MetalModule);

{$ENDIF}

end.
