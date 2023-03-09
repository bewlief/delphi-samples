{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework CoreAudio                 }
{                                                          }
{ Copyright (c) 1985-2010, Apple Inc. All rights reserved. }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2022 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit iOSapi.CoreAudio;

interface

uses
  Macapi.CoreServices;

const
  CA_PREFER_FIXED_POINT = 0;
  COREAUDIOTYPES_VERSION = 1051;
  kAppleLosslessFormatFlag_16BitSourceData = 1;
  kAppleLosslessFormatFlag_20BitSourceData = 2;
  kAppleLosslessFormatFlag_24BitSourceData = 3;
  kAppleLosslessFormatFlag_32BitSourceData = 4;
  kAudioChannelBit_Center = 4;
  kAudioChannelBit_CenterSurround = 256;
  kAudioChannelBit_LFEScreen = 8;
  kAudioChannelBit_Left = 1;
  kAudioChannelBit_LeftCenter = 64;
  kAudioChannelBit_LeftSurround = 16;
  kAudioChannelBit_LeftSurroundDirect = 512;
  kAudioChannelBit_Right = 2;
  kAudioChannelBit_RightCenter = 128;
  kAudioChannelBit_RightSurround = 32;
  kAudioChannelBit_RightSurroundDirect = 1024;
  kAudioChannelBit_TopBackCenter = 65536;
  kAudioChannelBit_TopBackLeft = 32768;
  kAudioChannelBit_TopBackRight = 131072;
  kAudioChannelBit_TopCenterSurround = 2048;
  kAudioChannelBit_VerticalHeightCenter = 8192;
  kAudioChannelBit_VerticalHeightLeft = 4096;
  kAudioChannelBit_VerticalHeightRight = 16384;
  kAudioChannelCoordinates_Azimuth = 0;
  kAudioChannelCoordinates_BackFront = 1;
  kAudioChannelCoordinates_Distance = 2;
  kAudioChannelCoordinates_DownUp = 2;
  kAudioChannelCoordinates_Elevation = 1;
  kAudioChannelCoordinates_LeftRight = 0;
  kAudioChannelFlags_AllOff = 0;
  kAudioChannelFlags_Meters = 4;
  kAudioChannelFlags_RectangularCoordinates = 1;
  kAudioChannelFlags_SphericalCoordinates = 2;
  kAudioChannelLabel_Ambisonic_W = 200;
  kAudioChannelLabel_Ambisonic_X = 201;
  kAudioChannelLabel_Ambisonic_Y = 202;
  kAudioChannelLabel_Ambisonic_Z = 203;
  kAudioChannelLabel_Center = 3;
  kAudioChannelLabel_CenterSurround = 9;
  kAudioChannelLabel_CenterSurroundDirect = 44;
  kAudioChannelLabel_ClickTrack = 304;
  kAudioChannelLabel_DialogCentricMix = 43;
  kAudioChannelLabel_Discrete = 400;
  kAudioChannelLabel_Discrete_0 = 65536;
  kAudioChannelLabel_Discrete_1 = 65537;
  kAudioChannelLabel_Discrete_10 = 65546;
  kAudioChannelLabel_Discrete_11 = 65547;
  kAudioChannelLabel_Discrete_12 = 65548;
  kAudioChannelLabel_Discrete_13 = 65549;
  kAudioChannelLabel_Discrete_14 = 65550;
  kAudioChannelLabel_Discrete_15 = 65551;
  kAudioChannelLabel_Discrete_2 = 65538;
  kAudioChannelLabel_Discrete_3 = 65539;
  kAudioChannelLabel_Discrete_4 = 65540;
  kAudioChannelLabel_Discrete_5 = 65541;
  kAudioChannelLabel_Discrete_6 = 65542;
  kAudioChannelLabel_Discrete_65535 = 131071;
  kAudioChannelLabel_Discrete_7 = 65543;
  kAudioChannelLabel_Discrete_8 = 65544;
  kAudioChannelLabel_Discrete_9 = 65545;
  kAudioChannelLabel_ForeignLanguage = 305;
  kAudioChannelLabel_Haptic = 45;
  kAudioChannelLabel_HeadphonesLeft = 301;
  kAudioChannelLabel_HeadphonesRight = 302;
  kAudioChannelLabel_HearingImpaired = 40;
  kAudioChannelLabel_LFE2 = 37;
  kAudioChannelLabel_LFEScreen = 4;
  kAudioChannelLabel_Left = 1;
  kAudioChannelLabel_LeftCenter = 7;
  kAudioChannelLabel_LeftSurround = 5;
  kAudioChannelLabel_LeftSurroundDirect = 10;
  kAudioChannelLabel_LeftTotal = 38;
  kAudioChannelLabel_LeftWide = 35;
  kAudioChannelLabel_MS_Mid = 204;
  kAudioChannelLabel_MS_Side = 205;
  kAudioChannelLabel_Mono = 42;
  kAudioChannelLabel_Narration = 41;
  kAudioChannelLabel_RearSurroundLeft = 33;
  kAudioChannelLabel_RearSurroundRight = 34;
  kAudioChannelLabel_Right = 2;
  kAudioChannelLabel_RightCenter = 8;
  kAudioChannelLabel_RightSurround = 6;
  kAudioChannelLabel_RightSurroundDirect = 11;
  kAudioChannelLabel_RightTotal = 39;
  kAudioChannelLabel_RightWide = 36;
  kAudioChannelLabel_TopBackCenter = 17;
  kAudioChannelLabel_TopBackLeft = 16;
  kAudioChannelLabel_TopBackRight = 18;
  kAudioChannelLabel_TopCenterSurround = 12;
  kAudioChannelLabel_Unknown = 4294967295;
  kAudioChannelLabel_Unused = 0;
  kAudioChannelLabel_UseCoordinates = 100;
  kAudioChannelLabel_VerticalHeightCenter = 14;
  kAudioChannelLabel_VerticalHeightLeft = 13;
  kAudioChannelLabel_VerticalHeightRight = 15;
  kAudioChannelLabel_XY_X = 206;
  kAudioChannelLabel_XY_Y = 207;
  kAudioChannelLayoutTag_AAC_3_0 = 7471107;
  kAudioChannelLayoutTag_AAC_4_0 = 7602180;
  kAudioChannelLayoutTag_AAC_5_0 = 7864325;
  kAudioChannelLayoutTag_AAC_5_1 = 8126470;
  kAudioChannelLayoutTag_AAC_6_0 = 9240582;
  kAudioChannelLayoutTag_AAC_6_1 = 9306119;
  kAudioChannelLayoutTag_AAC_7_0 = 9371655;
  kAudioChannelLayoutTag_AAC_7_1 = 8323080;
  kAudioChannelLayoutTag_AAC_Octagonal = 9437192;
  kAudioChannelLayoutTag_AAC_Quadraphonic = 7077892;
  kAudioChannelLayoutTag_AC3_1_0_1 = 9764866;
  kAudioChannelLayoutTag_AC3_2_1_1 = 10027012;
  kAudioChannelLayoutTag_AC3_3_0 = 9830403;
  kAudioChannelLayoutTag_AC3_3_0_1 = 9961476;
  kAudioChannelLayoutTag_AC3_3_1 = 9895940;
  kAudioChannelLayoutTag_AC3_3_1_1 = 10092549;
  kAudioChannelLayoutTag_Ambisonic_B_Format = 7012356;
  kAudioChannelLayoutTag_AudioUnit_4 = 7077892;
  kAudioChannelLayoutTag_AudioUnit_5 = 7143429;
  kAudioChannelLayoutTag_AudioUnit_5_0 = 7733253;
  kAudioChannelLayoutTag_AudioUnit_5_1 = 7929862;
  kAudioChannelLayoutTag_AudioUnit_6 = 7208966;
  kAudioChannelLayoutTag_AudioUnit_6_0 = 9109510;
  kAudioChannelLayoutTag_AudioUnit_6_1 = 8192007;
  kAudioChannelLayoutTag_AudioUnit_7_0 = 9175047;
  kAudioChannelLayoutTag_AudioUnit_7_0_Front = 9699335;
  kAudioChannelLayoutTag_AudioUnit_7_1 = 8388616;
  kAudioChannelLayoutTag_AudioUnit_7_1_Front = 8257544;
  kAudioChannelLayoutTag_AudioUnit_8 = 7274504;
  kAudioChannelLayoutTag_Binaural = 6946818;
  kAudioChannelLayoutTag_Cube = 7340040;
  kAudioChannelLayoutTag_DTS_3_1 = 11010052;
  kAudioChannelLayoutTag_DTS_4_1 = 11075589;
  kAudioChannelLayoutTag_DTS_6_0_A = 11141126;
  kAudioChannelLayoutTag_DTS_6_0_B = 11206662;
  kAudioChannelLayoutTag_DTS_6_0_C = 11272198;
  kAudioChannelLayoutTag_DTS_6_1_A = 11337735;
  kAudioChannelLayoutTag_DTS_6_1_B = 11403271;
  kAudioChannelLayoutTag_DTS_6_1_C = 11468807;
  kAudioChannelLayoutTag_DTS_6_1_D = 11927559;
  kAudioChannelLayoutTag_DTS_7_0 = 11534343;
  kAudioChannelLayoutTag_DTS_7_1 = 11599880;
  kAudioChannelLayoutTag_DTS_8_0_A = 11665416;
  kAudioChannelLayoutTag_DTS_8_0_B = 11730952;
  kAudioChannelLayoutTag_DTS_8_1_A = 11796489;
  kAudioChannelLayoutTag_DTS_8_1_B = 11862025;
  kAudioChannelLayoutTag_DVD_0 = 6553601;
  kAudioChannelLayoutTag_DVD_1 = 6619138;
  kAudioChannelLayoutTag_DVD_10 = 8912900;
  kAudioChannelLayoutTag_DVD_11 = 8978437;
  kAudioChannelLayoutTag_DVD_12 = 7929862;
  kAudioChannelLayoutTag_DVD_13 = 7536644;
  kAudioChannelLayoutTag_DVD_14 = 7667717;
  kAudioChannelLayoutTag_DVD_15 = 8912900;
  kAudioChannelLayoutTag_DVD_16 = 8978437;
  kAudioChannelLayoutTag_DVD_17 = 7929862;
  kAudioChannelLayoutTag_DVD_18 = 9043973;
  kAudioChannelLayoutTag_DVD_19 = 7733253;
  kAudioChannelLayoutTag_DVD_2 = 8585219;
  kAudioChannelLayoutTag_DVD_20 = 7995398;
  kAudioChannelLayoutTag_DVD_3 = 8650756;
  kAudioChannelLayoutTag_DVD_4 = 8716291;
  kAudioChannelLayoutTag_DVD_5 = 8781828;
  kAudioChannelLayoutTag_DVD_6 = 8847365;
  kAudioChannelLayoutTag_DVD_7 = 7405571;
  kAudioChannelLayoutTag_DVD_8 = 7536644;
  kAudioChannelLayoutTag_DVD_9 = 7667717;
  kAudioChannelLayoutTag_DiscreteInOrder = 9633792;
  kAudioChannelLayoutTag_EAC3_6_1_A = 10289159;
  kAudioChannelLayoutTag_EAC3_6_1_B = 10354695;
  kAudioChannelLayoutTag_EAC3_6_1_C = 10420231;
  kAudioChannelLayoutTag_EAC3_7_1_A = 10485768;
  kAudioChannelLayoutTag_EAC3_7_1_B = 10551304;
  kAudioChannelLayoutTag_EAC3_7_1_C = 10616840;
  kAudioChannelLayoutTag_EAC3_7_1_D = 10682376;
  kAudioChannelLayoutTag_EAC3_7_1_E = 10747912;
  kAudioChannelLayoutTag_EAC3_7_1_F = 10813448;
  kAudioChannelLayoutTag_EAC3_7_1_G = 10878984;
  kAudioChannelLayoutTag_EAC3_7_1_H = 10944520;
  kAudioChannelLayoutTag_EAC_6_0_A = 10158086;
  kAudioChannelLayoutTag_EAC_7_0_A = 10223623;
  kAudioChannelLayoutTag_Emagic_Default_7_1 = 8454152;
  kAudioChannelLayoutTag_Hexagonal = 7208966;
  kAudioChannelLayoutTag_ITU_1_0 = 6553601;
  kAudioChannelLayoutTag_ITU_2_0 = 6619138;
  kAudioChannelLayoutTag_ITU_2_1 = 8585219;
  kAudioChannelLayoutTag_ITU_2_2 = 8650756;
  kAudioChannelLayoutTag_ITU_3_0 = 7405571;
  kAudioChannelLayoutTag_ITU_3_1 = 7536644;
  kAudioChannelLayoutTag_ITU_3_2 = 7667717;
  kAudioChannelLayoutTag_ITU_3_2_1 = 7929862;
  kAudioChannelLayoutTag_ITU_3_4_1 = 8388616;
  kAudioChannelLayoutTag_MPEG_1_0 = 6553601;
  kAudioChannelLayoutTag_MPEG_2_0 = 6619138;
  kAudioChannelLayoutTag_MPEG_3_0_A = 7405571;
  kAudioChannelLayoutTag_MPEG_3_0_B = 7471107;
  kAudioChannelLayoutTag_MPEG_4_0_A = 7536644;
  kAudioChannelLayoutTag_MPEG_4_0_B = 7602180;
  kAudioChannelLayoutTag_MPEG_5_0_A = 7667717;
  kAudioChannelLayoutTag_MPEG_5_0_B = 7733253;
  kAudioChannelLayoutTag_MPEG_5_0_C = 7798789;
  kAudioChannelLayoutTag_MPEG_5_0_D = 7864325;
  kAudioChannelLayoutTag_MPEG_5_1_A = 7929862;
  kAudioChannelLayoutTag_MPEG_5_1_B = 7995398;
  kAudioChannelLayoutTag_MPEG_5_1_C = 8060934;
  kAudioChannelLayoutTag_MPEG_5_1_D = 8126470;
  kAudioChannelLayoutTag_MPEG_6_1_A = 8192007;
  kAudioChannelLayoutTag_MPEG_7_1_A = 8257544;
  kAudioChannelLayoutTag_MPEG_7_1_B = 8323080;
  kAudioChannelLayoutTag_MPEG_7_1_C = 8388616;
  kAudioChannelLayoutTag_MatrixStereo = 6750210;
  kAudioChannelLayoutTag_MidSide = 6815746;
  kAudioChannelLayoutTag_Mono = 6553601;
  kAudioChannelLayoutTag_Octagonal = 7274504;
  kAudioChannelLayoutTag_Pentagonal = 7143429;
  kAudioChannelLayoutTag_Quadraphonic = 7077892;
  kAudioChannelLayoutTag_SMPTE_DTV = 8519688;
  kAudioChannelLayoutTag_Stereo = 6619138;
  kAudioChannelLayoutTag_StereoHeadphones = 6684674;
  kAudioChannelLayoutTag_TMH_10_2_full = 9568277;
  kAudioChannelLayoutTag_TMH_10_2_std = 9502736;
  kAudioChannelLayoutTag_Unknown = 4294901760;
  kAudioChannelLayoutTag_UseChannelBitmap = 65536;
  kAudioChannelLayoutTag_UseChannelDescriptions = 0;
  kAudioChannelLayoutTag_XY = 6881282;
  kAudioFormat60958AC3 = 1667326771;
  kAudioFormatAC3 = 1633889587;
  kAudioFormatAES3 = 1634038579;
  kAudioFormatALaw = 1634492791;
  kAudioFormatAMR = 1935764850;
  kAudioFormatAppleIMA4 = 1768775988;
  kAudioFormatAppleLossless = 1634492771;
  kAudioFormatAudible = 1096107074;
  kAudioFormatDVIIntelIMA = 1836253201;
  kAudioFormatFlagIsAlignedHigh = 16;
  kAudioFormatFlagIsBigEndian = 2;
  kAudioFormatFlagIsFloat = 1;
  kAudioFormatFlagIsNonInterleaved = 32;
  kAudioFormatFlagIsNonMixable = 64;
  kAudioFormatFlagIsPacked = 8;
  kAudioFormatFlagIsSignedInteger = 4;
  kAudioFormatFlagsAreAllClear = -2147483648;
  kAudioFormatFlagsAudioUnitCanonical = 41;
  kAudioFormatFlagsCanonical = 9;
  kAudioFormatFlagsNativeEndian = 0;
  kAudioFormatFlagsNativeFloatPacked = 9;
  kAudioFormatLinearPCM = 1819304813;
  kAudioFormatMACE3 = 1296122675;
  kAudioFormatMACE6 = 1296122678;
  kAudioFormatMIDIStream = 1835623529;
  kAudioFormatMPEG4AAC = 1633772320;
  kAudioFormatMPEG4AAC_ELD = 1633772389;
  kAudioFormatMPEG4AAC_ELD_SBR = 1633772390;
  kAudioFormatMPEG4AAC_HE = 1633772392;
  kAudioFormatMPEG4AAC_HE_V2 = 1633772400;
  kAudioFormatMPEG4AAC_LD = 1633772396;
  kAudioFormatMPEG4AAC_Spatial = 1633772403;
  kAudioFormatMPEG4CELP = 1667591280;
  kAudioFormatMPEG4HVXC = 1752594531;
  kAudioFormatMPEG4TwinVQ = 1953986161;
  kAudioFormatMPEGLayer1 = 778924081;
  kAudioFormatMPEGLayer2 = 778924082;
  kAudioFormatMPEGLayer3 = 778924083;
  kAudioFormatMicrosoftGSM = 1836253233;
  kAudioFormatParameterValueStream = 1634760307;
  kAudioFormatQDesign = 1363430723;
  kAudioFormatQDesign2 = 1363430706;
  kAudioFormatQUALCOMM = 1365470320;
  kAudioFormatTimeCode = 1953066341;
  kAudioFormatULaw = 1970037111;
  kAudioFormatiLBC = 1768710755;
  kAudioStreamAnyRate = 0;
  kAudioTimeStampHostTimeValid = 2;
  kAudioTimeStampRateScalarValid = 4;
  kAudioTimeStampSMPTETimeValid = 16;
  kAudioTimeStampSampleHostTimeValid = 3;
  kAudioTimeStampSampleTimeValid = 1;
  kAudioTimeStampWordClockTimeValid = 8;
  kAudio_FileNotFoundError = -43;
  kAudio_MemFullError = -108;
  kAudio_ParamError = -50;
  kAudio_UnimplementedError = -4;
  kLinearPCMFormatFlagIsAlignedHigh = 16;
  kLinearPCMFormatFlagIsBigEndian = 2;
  kLinearPCMFormatFlagIsFloat = 1;
  kLinearPCMFormatFlagIsNonInterleaved = 32;
  kLinearPCMFormatFlagIsNonMixable = 64;
  kLinearPCMFormatFlagIsPacked = 8;
  kLinearPCMFormatFlagIsSignedInteger = 4;
  kLinearPCMFormatFlagsAreAllClear = -2147483648;
  kLinearPCMFormatFlagsSampleFractionMask = 8064;
  kLinearPCMFormatFlagsSampleFractionShift = 7;
  kMPEG4Object_AAC_LC = 2;
  kMPEG4Object_AAC_LTP = 4;
  kMPEG4Object_AAC_Main = 1;
  kMPEG4Object_AAC_SBR = 5;
  kMPEG4Object_AAC_SSR = 3;
  kMPEG4Object_AAC_Scalable = 6;
  kMPEG4Object_CELP = 8;
  kMPEG4Object_HVXC = 9;
  kMPEG4Object_TwinVQ = 7;
  kSMPTETimeRunning = 2;
  kSMPTETimeType2398 = 11;
  kSMPTETimeType24 = 0;
  kSMPTETimeType25 = 1;
  kSMPTETimeType2997 = 4;
  kSMPTETimeType2997Drop = 5;
  kSMPTETimeType30 = 3;
  kSMPTETimeType30Drop = 2;
  kSMPTETimeType50 = 10;
  kSMPTETimeType5994 = 7;
  kSMPTETimeType5994Drop = 9;
  kSMPTETimeType60 = 6;
  kSMPTETimeType60Drop = 8;
  kSMPTETimeValid = 1;

// ===== Typedefs and structs =====
{$M+}
type
  AudioChannelLabel = UInt32;
  AudioChannelLayoutTag = UInt32;

  AudioBuffer = record
    mNumberChannels: UInt32;
    mDataByteSize: UInt32;
    mData: Pointer;
  end;
  PAudioBuffer = ^AudioBuffer;

  AudioBufferList = record
    mNumberBuffers: UInt32;
    mBuffers: array[0..0] of AudioBuffer;
  end;
  PAudioBufferList = ^AudioBufferList;

  AudioStreamBasicDescription = record
    mSampleRate: Double;
    mFormatID: UInt32;
    mFormatFlags: UInt32;
    mBytesPerPacket: UInt32;
    mFramesPerPacket: UInt32;
    mBytesPerFrame: UInt32;
    mChannelsPerFrame: UInt32;
    mBitsPerChannel: UInt32;
    mReserved: UInt32;
  end;
  PAudioStreamBasicDescription = ^AudioStreamBasicDescription;

  AudioStreamPacketDescription = record
    mStartOffset: SInt64;
    mVariableFramesInPackets: UInt32;
    mDataByteSize: UInt32;
  end;
  PAudioStreamPacketDescription = ^AudioStreamPacketDescription;

  TAudioChannelCoordinatesArray = array[0..2] of Single;
  AudioChannelDescription = record
    mChannelLabel: AudioChannelLabel;
    mChannelFlags: UInt32;
    mCoordinates: TAudioChannelCoordinatesArray;
  end;
  PAudioChannelDescription = ^AudioChannelDescription;

  AudioChannelLayout = record
    mChannelLayoutTag: AudioChannelLayoutTag;
    mChannelBitmap: UInt32;
    mNumberChannelDescriptions: UInt32;
    mChannelDescriptions: AudioChannelDescription;
  end;
  PAudioChannelLayout = ^AudioChannelLayout;

  AudioClassDescription = record
    mType: OSType;
    mSubType: OSType;
    mManufacturer: OSType;
  end;
  PAudioClassDescription = ^AudioClassDescription;

  SMPTETime = record
    mSubframes: SInt16;
    mSubframeDivisor: SInt16;
    mCounter: UInt32;
    mType: UInt32;
    mFlags: UInt32;
    mHours: SInt16;
    mMinutes: SInt16;
    mSeconds: SInt16;
    mFrames: SInt16;
  end;

  AudioTimeStamp = record
    mSampleTime: Double;
    mHostTime: UInt64;
    mRateScalar: Double;
    mWordClockTime: UInt64;
    mSMPTETime: SMPTETime;
    mFlags: UInt32;
    mReserved: UInt32;
  end;
  PAudioTimeStamp = ^AudioTimeStamp;

const
  libCoreAudio = '/System/Library/Frameworks/CoreAudio.framework/CoreAudio';

implementation

end.
