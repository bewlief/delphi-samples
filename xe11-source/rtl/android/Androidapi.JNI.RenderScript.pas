{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.RenderScript;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes;

type
// ===== Forward declarations =====

  JBaseObj = interface;//android.renderscript.BaseObj
  JAllocation = interface;//android.renderscript.Allocation
  JAllocation_MipmapControl = interface;//android.renderscript.Allocation$MipmapControl
  JAllocation_OnBufferAvailableListener = interface;//android.renderscript.Allocation$OnBufferAvailableListener
  JAllocationAdapter = interface;//android.renderscript.AllocationAdapter
  JByte2 = interface;//android.renderscript.Byte2
  JByte3 = interface;//android.renderscript.Byte3
  JByte4 = interface;//android.renderscript.Byte4
  JDouble2 = interface;//android.renderscript.Double2
  JDouble3 = interface;//android.renderscript.Double3
  JDouble4 = interface;//android.renderscript.Double4
  Jrenderscript_Element = interface;//android.renderscript.Element
  JElement_Builder = interface;//android.renderscript.Element$Builder
  JElement_DataKind = interface;//android.renderscript.Element$DataKind
  JElement_DataType = interface;//android.renderscript.Element$DataType
  JFieldPacker = interface;//android.renderscript.FieldPacker
  JFloat2 = interface;//android.renderscript.Float2
  JFloat3 = interface;//android.renderscript.Float3
  JFloat4 = interface;//android.renderscript.Float4
  JInt2 = interface;//android.renderscript.Int2
  JInt3 = interface;//android.renderscript.Int3
  JInt4 = interface;//android.renderscript.Int4
  JLong2 = interface;//android.renderscript.Long2
  JLong3 = interface;//android.renderscript.Long3
  JLong4 = interface;//android.renderscript.Long4
  JMatrix2f = interface;//android.renderscript.Matrix2f
  JMatrix3f = interface;//android.renderscript.Matrix3f
  JMatrix4f = interface;//android.renderscript.Matrix4f
  JRSRuntimeException = interface;//android.renderscript.RSRuntimeException
  JRSDriverException = interface;//android.renderscript.RSDriverException
  JRSIllegalArgumentException = interface;//android.renderscript.RSIllegalArgumentException
  JRSInvalidStateException = interface;//android.renderscript.RSInvalidStateException
  JRenderScript = interface;//android.renderscript.RenderScript
  JRenderScript_ContextType = interface;//android.renderscript.RenderScript$ContextType
  JRenderScript_Priority = interface;//android.renderscript.RenderScript$Priority
  JRenderScript_RSErrorHandler = interface;//android.renderscript.RenderScript$RSErrorHandler
  JRenderScript_RSMessageHandler = interface;//android.renderscript.RenderScript$RSMessageHandler
  JSampler = interface;//android.renderscript.Sampler
  JSampler_Builder = interface;//android.renderscript.Sampler$Builder
  JSampler_Value = interface;//android.renderscript.Sampler$Value
  JScript = interface;//android.renderscript.Script
  JScript_Builder = interface;//android.renderscript.Script$Builder
  JScript_FieldBase = interface;//android.renderscript.Script$FieldBase
  JScript_FieldID = interface;//android.renderscript.Script$FieldID
  JScript_InvokeID = interface;//android.renderscript.Script$InvokeID
  JScript_KernelID = interface;//android.renderscript.Script$KernelID
  JScript_LaunchOptions = interface;//android.renderscript.Script$LaunchOptions
  JScriptC = interface;//android.renderscript.ScriptC
  JScriptGroup = interface;//android.renderscript.ScriptGroup
  JScriptGroup_Binding = interface;//android.renderscript.ScriptGroup$Binding
  JScriptGroup_Builder = interface;//android.renderscript.ScriptGroup$Builder
  JScriptGroup_Builder2 = interface;//android.renderscript.ScriptGroup$Builder2
  JScriptGroup_Closure = interface;//android.renderscript.ScriptGroup$Closure
  JScriptGroup_Future = interface;//android.renderscript.ScriptGroup$Future
  JScriptGroup_Input = interface;//android.renderscript.ScriptGroup$Input
  JScriptIntrinsic = interface;//android.renderscript.ScriptIntrinsic
  JScriptIntrinsic3DLUT = interface;//android.renderscript.ScriptIntrinsic3DLUT
  JScriptIntrinsicBLAS = interface;//android.renderscript.ScriptIntrinsicBLAS
  JScriptIntrinsicBlend = interface;//android.renderscript.ScriptIntrinsicBlend
  JScriptIntrinsicBlur = interface;//android.renderscript.ScriptIntrinsicBlur
  JScriptIntrinsicColorMatrix = interface;//android.renderscript.ScriptIntrinsicColorMatrix
  JScriptIntrinsicConvolve3x3 = interface;//android.renderscript.ScriptIntrinsicConvolve3x3
  JScriptIntrinsicConvolve5x5 = interface;//android.renderscript.ScriptIntrinsicConvolve5x5
  JScriptIntrinsicHistogram = interface;//android.renderscript.ScriptIntrinsicHistogram
  JScriptIntrinsicLUT = interface;//android.renderscript.ScriptIntrinsicLUT
  JScriptIntrinsicResize = interface;//android.renderscript.ScriptIntrinsicResize
  JScriptIntrinsicYuvToRGB = interface;//android.renderscript.ScriptIntrinsicYuvToRGB
  JShort2 = interface;//android.renderscript.Short2
  JShort3 = interface;//android.renderscript.Short3
  JShort4 = interface;//android.renderscript.Short4
  JType = interface;//android.renderscript.Type
  JType_Builder = interface;//android.renderscript.Type$Builder
  JType_CubemapFace = interface;//android.renderscript.Type$CubemapFace

// ===== Interface declarations =====

  JBaseObjClass = interface(JObjectClass)
    ['{F1A3D5D0-1151-47D2-A690-B3B562F350CC}']
  end;

  [JavaSignature('android/renderscript/BaseObj')]
  JBaseObj = interface(JObject)
    ['{20615312-245C-4906-B481-80FDCA85B4B7}']
    procedure destroy; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getName: JString; cdecl;
    function hashCode: Integer; cdecl;
    procedure setName(name: JString); cdecl;
  end;
  TJBaseObj = class(TJavaGenericImport<JBaseObjClass, JBaseObj>) end;

  JAllocationClass = interface(JBaseObjClass)
    ['{B01FABA3-B1B6-4F4F-AAB7-FCDDCEECB161}']
    {class} function _GetUSAGE_GRAPHICS_CONSTANTS: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_RENDER_TARGET: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_TEXTURE: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_VERTEX: Integer; cdecl;
    {class} function _GetUSAGE_IO_INPUT: Integer; cdecl;
    {class} function _GetUSAGE_IO_OUTPUT: Integer; cdecl;
    {class} function _GetUSAGE_SCRIPT: Integer; cdecl;
    {class} function _GetUSAGE_SHARED: Integer; cdecl;
    {class} function createAllocations(rs: JRenderScript; t: JType; usage: Integer; numAlloc: Integer): TJavaObjectArray<JAllocation>; cdecl;
    {class} function createCubemapFromBitmap(rs: JRenderScript; b: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createCubemapFromBitmap(rs: JRenderScript; b: JBitmap): JAllocation; cdecl; overload;
    {class} function createCubemapFromCubeFaces(rs: JRenderScript; xpos: JBitmap; xneg: JBitmap; ypos: JBitmap; yneg: JBitmap; zpos: JBitmap; zneg: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createCubemapFromCubeFaces(rs: JRenderScript; xpos: JBitmap; xneg: JBitmap; ypos: JBitmap; yneg: JBitmap; zpos: JBitmap; zneg: JBitmap): JAllocation; cdecl; overload;
    {class} function createFromBitmap(rs: JRenderScript; b: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createFromBitmap(rs: JRenderScript; b: JBitmap): JAllocation; cdecl; overload;
    {class} function createFromBitmapResource(rs: JRenderScript; res: JResources; id: Integer; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createFromBitmapResource(rs: JRenderScript; res: JResources; id: Integer): JAllocation; cdecl; overload;
    {class} function createFromString(rs: JRenderScript; str: JString; usage: Integer): JAllocation; cdecl;
    {class} function createSized(rs: JRenderScript; e: Jrenderscript_Element; count: Integer; usage: Integer): JAllocation; cdecl; overload;
    {class} function createSized(rs: JRenderScript; e: Jrenderscript_Element; count: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType; usage: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType): JAllocation; cdecl; overload;
    {class} property USAGE_GRAPHICS_CONSTANTS: Integer read _GetUSAGE_GRAPHICS_CONSTANTS;
    {class} property USAGE_GRAPHICS_RENDER_TARGET: Integer read _GetUSAGE_GRAPHICS_RENDER_TARGET;
    {class} property USAGE_GRAPHICS_TEXTURE: Integer read _GetUSAGE_GRAPHICS_TEXTURE;
    {class} property USAGE_GRAPHICS_VERTEX: Integer read _GetUSAGE_GRAPHICS_VERTEX;
    {class} property USAGE_IO_INPUT: Integer read _GetUSAGE_IO_INPUT;
    {class} property USAGE_IO_OUTPUT: Integer read _GetUSAGE_IO_OUTPUT;
    {class} property USAGE_SCRIPT: Integer read _GetUSAGE_SCRIPT;
    {class} property USAGE_SHARED: Integer read _GetUSAGE_SHARED;
  end;

  [JavaSignature('android/renderscript/Allocation')]
  JAllocation = interface(JBaseObj)
    ['{1A259581-F9C8-4808-99C1-63859107FE15}']
    procedure copy1DRangeFrom(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; data: JAllocation; dataOff: Integer); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; array_: JObject); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Integer>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Single>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: JAllocation; dataXoff: Integer; dataYoff: Integer); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; data: JBitmap); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; array_: JObject); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Integer>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Single>); cdecl; overload;
    procedure copy3DRangeFrom(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; array_: JObject); cdecl; overload;
    procedure copy3DRangeFrom(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; data: JAllocation; dataXoff: Integer; dataYoff: Integer; dataZoff: Integer); cdecl; overload;
    procedure copy3DRangeTo(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; array_: JObject); cdecl;
    procedure copyFrom(d: TJavaObjectArray<JBaseObj>); cdecl; overload;
    procedure copyFrom(array_: JObject); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Single>); cdecl; overload;
    procedure copyFrom(b: JBitmap); cdecl; overload;
    procedure copyFrom(a: JAllocation); cdecl; overload;
    procedure copyFromUnchecked(array_: JObject); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Single>); cdecl; overload;
    procedure copyTo(b: JBitmap); cdecl; overload;
    procedure copyTo(array_: JObject); cdecl; overload;
    procedure copyTo(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyTo(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyTo(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyTo(d: TJavaArray<Single>); cdecl; overload;
    procedure destroy; cdecl;
    procedure generateMipmaps; cdecl;
    function getByteBuffer: JByteBuffer; cdecl;
    function getBytesSize: Integer; cdecl;
    function getElement: Jrenderscript_Element; cdecl;
    function getStride: Int64; cdecl;
    function getSurface: JSurface; cdecl;
    function getTimeStamp: Int64; cdecl;
    function getType: JType; cdecl;
    function getUsage: Integer; cdecl;
    procedure ioReceive; cdecl;
    procedure ioSend; cdecl;
    procedure resize(dimX: Integer); cdecl;//Deprecated
    procedure setAutoPadding(useAutoPadding: Boolean); cdecl;
    procedure setFromFieldPacker(xoff: Integer; fp: JFieldPacker); cdecl; overload;
    procedure setFromFieldPacker(xoff: Integer; component_number: Integer; fp: JFieldPacker); cdecl; overload;
    procedure setFromFieldPacker(xoff: Integer; yoff: Integer; zoff: Integer; component_number: Integer; fp: JFieldPacker); cdecl; overload;
    procedure setOnBufferAvailableListener(callback: JAllocation_OnBufferAvailableListener); cdecl;
    procedure setSurface(sur: JSurface); cdecl;
    procedure syncAll(srcLocation: Integer); cdecl;
  end;
  TJAllocation = class(TJavaGenericImport<JAllocationClass, JAllocation>) end;

  JAllocation_MipmapControlClass = interface(JEnumClass)
    ['{87E40B07-4700-4177-9E49-3FED1F0886F7}']
    {class} function _GetMIPMAP_FULL: JAllocation_MipmapControl; cdecl;
    {class} function _GetMIPMAP_NONE: JAllocation_MipmapControl; cdecl;
    {class} function _GetMIPMAP_ON_SYNC_TO_TEXTURE: JAllocation_MipmapControl; cdecl;
    {class} function valueOf(name: JString): JAllocation_MipmapControl; cdecl;
    {class} function values: TJavaObjectArray<JAllocation_MipmapControl>; cdecl;
    {class} property MIPMAP_FULL: JAllocation_MipmapControl read _GetMIPMAP_FULL;
    {class} property MIPMAP_NONE: JAllocation_MipmapControl read _GetMIPMAP_NONE;
    {class} property MIPMAP_ON_SYNC_TO_TEXTURE: JAllocation_MipmapControl read _GetMIPMAP_ON_SYNC_TO_TEXTURE;
  end;

  [JavaSignature('android/renderscript/Allocation$MipmapControl')]
  JAllocation_MipmapControl = interface(JEnum)
    ['{2381D33F-176C-4B5F-A896-F00D69E2B8A5}']
  end;
  TJAllocation_MipmapControl = class(TJavaGenericImport<JAllocation_MipmapControlClass, JAllocation_MipmapControl>) end;

  JAllocation_OnBufferAvailableListenerClass = interface(IJavaClass)
    ['{11AC4ED8-AF4B-410C-B541-A295CC4C949B}']
  end;

  [JavaSignature('android/renderscript/Allocation$OnBufferAvailableListener')]
  JAllocation_OnBufferAvailableListener = interface(IJavaInstance)
    ['{A8F407F2-6004-4A74-82C2-434973C349F6}']
    procedure onBufferAvailable(a: JAllocation); cdecl;
  end;
  TJAllocation_OnBufferAvailableListener = class(TJavaGenericImport<JAllocation_OnBufferAvailableListenerClass, JAllocation_OnBufferAvailableListener>) end;

  JAllocationAdapterClass = interface(JAllocationClass)
    ['{3F11D804-4C00-4F5A-96DD-D37AE05B5262}']
    {class} function create1D(rs: JRenderScript; a: JAllocation): JAllocationAdapter; cdecl;
    {class} function create2D(rs: JRenderScript; a: JAllocation): JAllocationAdapter; cdecl;
    {class} function createTyped(rs: JRenderScript; a: JAllocation; t: JType): JAllocationAdapter; cdecl;
  end;

  [JavaSignature('android/renderscript/AllocationAdapter')]
  JAllocationAdapter = interface(JAllocation)
    ['{42D2A32F-1F42-4C98-AFDB-F99BE0453A30}']
    procedure resize(dimX: Integer); cdecl;
    procedure setFace(cf: JType_CubemapFace); cdecl;
    procedure setLOD(lod: Integer); cdecl;
    procedure setX(x: Integer); cdecl;
    procedure setY(y: Integer); cdecl;
    procedure setZ(z: Integer); cdecl;
  end;
  TJAllocationAdapter = class(TJavaGenericImport<JAllocationAdapterClass, JAllocationAdapter>) end;

  JByte2Class = interface(JObjectClass)
    ['{FD826C34-18C0-4A2D-A25D-C2DE2157CBA4}']
    {class} function init: JByte2; cdecl; overload;
    {class} function init(initX: Byte; initY: Byte): JByte2; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Byte2')]
  JByte2 = interface(JObject)
    ['{D204CF71-A2E9-4378-BACC-F17A59CC5A2A}']
    function _Getx: Byte; cdecl;
    procedure _Setx(Value: Byte); cdecl;
    function _Gety: Byte; cdecl;
    procedure _Sety(Value: Byte); cdecl;
    property x: Byte read _Getx write _Setx;
    property y: Byte read _Gety write _Sety;
  end;
  TJByte2 = class(TJavaGenericImport<JByte2Class, JByte2>) end;

  JByte3Class = interface(JObjectClass)
    ['{B16A7374-C542-4692-B43C-EC47C9B25103}']
    {class} function init: JByte3; cdecl; overload;
    {class} function init(initX: Byte; initY: Byte; initZ: Byte): JByte3; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Byte3')]
  JByte3 = interface(JObject)
    ['{B4299E7D-4F20-4B2A-A51A-ECA6FE05CEF1}']
    function _Getx: Byte; cdecl;
    procedure _Setx(Value: Byte); cdecl;
    function _Gety: Byte; cdecl;
    procedure _Sety(Value: Byte); cdecl;
    function _Getz: Byte; cdecl;
    procedure _Setz(Value: Byte); cdecl;
    property x: Byte read _Getx write _Setx;
    property y: Byte read _Gety write _Sety;
    property z: Byte read _Getz write _Setz;
  end;
  TJByte3 = class(TJavaGenericImport<JByte3Class, JByte3>) end;

  JByte4Class = interface(JObjectClass)
    ['{7CECAA7D-78B8-4A82-ABEA-CA1A960B5F7D}']
    {class} function init: JByte4; cdecl; overload;
    {class} function init(initX: Byte; initY: Byte; initZ: Byte; initW: Byte): JByte4; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Byte4')]
  JByte4 = interface(JObject)
    ['{25793C6B-3BD8-4F36-A7CB-6B249A84BA00}']
    function _Getw: Byte; cdecl;
    procedure _Setw(Value: Byte); cdecl;
    function _Getx: Byte; cdecl;
    procedure _Setx(Value: Byte); cdecl;
    function _Gety: Byte; cdecl;
    procedure _Sety(Value: Byte); cdecl;
    function _Getz: Byte; cdecl;
    procedure _Setz(Value: Byte); cdecl;
    property w: Byte read _Getw write _Setw;
    property x: Byte read _Getx write _Setx;
    property y: Byte read _Gety write _Sety;
    property z: Byte read _Getz write _Setz;
  end;
  TJByte4 = class(TJavaGenericImport<JByte4Class, JByte4>) end;

  JDouble2Class = interface(JObjectClass)
    ['{3296D13A-9497-4E70-B04F-6A6FB0B94CA2}']
    {class} function init: JDouble2; cdecl; overload;
    {class} function init(x: Double; y: Double): JDouble2; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Double2')]
  JDouble2 = interface(JObject)
    ['{960329E8-5599-48F5-8A3C-EDB4F77860C7}']
    function _Getx: Double; cdecl;
    procedure _Setx(Value: Double); cdecl;
    function _Gety: Double; cdecl;
    procedure _Sety(Value: Double); cdecl;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
  end;
  TJDouble2 = class(TJavaGenericImport<JDouble2Class, JDouble2>) end;

  JDouble3Class = interface(JObjectClass)
    ['{D09E8456-E324-4365-8C24-F7BE9076CC92}']
    {class} function init: JDouble3; cdecl; overload;
    {class} function init(x: Double; y: Double; z: Double): JDouble3; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Double3')]
  JDouble3 = interface(JObject)
    ['{CDDE7EEA-0EAA-4A3C-9E38-B5CE628AB935}']
    function _Getx: Double; cdecl;
    procedure _Setx(Value: Double); cdecl;
    function _Gety: Double; cdecl;
    procedure _Sety(Value: Double); cdecl;
    function _Getz: Double; cdecl;
    procedure _Setz(Value: Double); cdecl;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property z: Double read _Getz write _Setz;
  end;
  TJDouble3 = class(TJavaGenericImport<JDouble3Class, JDouble3>) end;

  JDouble4Class = interface(JObjectClass)
    ['{9FB171A5-D63F-4E2D-B6B4-7AD0A82DCA54}']
    {class} function init: JDouble4; cdecl; overload;
    {class} function init(x: Double; y: Double; z: Double; w: Double): JDouble4; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Double4')]
  JDouble4 = interface(JObject)
    ['{CC62BE0A-7716-4C85-998B-67FCF7A9C846}']
    function _Getw: Double; cdecl;
    procedure _Setw(Value: Double); cdecl;
    function _Getx: Double; cdecl;
    procedure _Setx(Value: Double); cdecl;
    function _Gety: Double; cdecl;
    procedure _Sety(Value: Double); cdecl;
    function _Getz: Double; cdecl;
    procedure _Setz(Value: Double); cdecl;
    property w: Double read _Getw write _Setw;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property z: Double read _Getz write _Setz;
  end;
  TJDouble4 = class(TJavaGenericImport<JDouble4Class, JDouble4>) end;

  Jrenderscript_ElementClass = interface(JBaseObjClass)
    ['{19D75D2F-CF5B-4EFE-9A01-A69F51A1685D}']
    {class} function ALLOCATION(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function A_8(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function BOOLEAN(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function ELEMENT(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F16(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F16_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F16_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F16_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F32(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F32_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F32_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F32_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F64(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F64_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F64_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function F64_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function FONT(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I16(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I16_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I16_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I16_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I32(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I32_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I32_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I32_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I64(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I64_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I64_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I64_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I8(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I8_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I8_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function I8_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function MATRIX4X4(rs: JRenderScript): Jrenderscript_Element; cdecl;//Deprecated
    {class} function MATRIX_2X2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function MATRIX_3X3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function MATRIX_4X4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function MESH(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function PROGRAM_FRAGMENT(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function PROGRAM_RASTER(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function PROGRAM_STORE(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function PROGRAM_VERTEX(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function RGBA_4444(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function RGBA_5551(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function RGBA_8888(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function RGB_565(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function RGB_888(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function SAMPLER(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function SCRIPT(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function &TYPE(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U16(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U16_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U16_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U16_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U32(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U32_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U32_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U32_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U64(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U64_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U64_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U64_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U8(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U8_2(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U8_3(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function U8_4(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function YUV(rs: JRenderScript): Jrenderscript_Element; cdecl;
    {class} function createPixel(rs: JRenderScript; dt: JElement_DataType; dk: JElement_DataKind): Jrenderscript_Element; cdecl;
    {class} function createVector(rs: JRenderScript; dt: JElement_DataType; size: Integer): Jrenderscript_Element; cdecl;
  end;

  [JavaSignature('android/renderscript/Element')]
  Jrenderscript_Element = interface(JBaseObj)
    ['{47E2A1DE-1101-43E3-BF42-9F8128EDD03E}']
    function getBytesSize: Integer; cdecl;
    function getDataKind: JElement_DataKind; cdecl;
    function getDataType: JElement_DataType; cdecl;
    function getSubElement(index: Integer): Jrenderscript_Element; cdecl;
    function getSubElementArraySize(index: Integer): Integer; cdecl;
    function getSubElementCount: Integer; cdecl;
    function getSubElementName(index: Integer): JString; cdecl;
    function getSubElementOffsetBytes(index: Integer): Integer; cdecl;
    function getVectorSize: Integer; cdecl;
    function isCompatible(e: Jrenderscript_Element): Boolean; cdecl;
    function isComplex: Boolean; cdecl;
  end;
  TJrenderscript_Element = class(TJavaGenericImport<Jrenderscript_ElementClass, Jrenderscript_Element>) end;

  JElement_BuilderClass = interface(JObjectClass)
    ['{9A906C07-DE8B-436A-A045-08079CCBF48F}']
    {class} function init(rs: JRenderScript): JElement_Builder; cdecl;
  end;

  [JavaSignature('android/renderscript/Element$Builder')]
  JElement_Builder = interface(JObject)
    ['{A4906DDF-7DD6-4433-B4F9-A3CDE394B6CA}']
    function add(element: Jrenderscript_Element; name: JString; arraySize: Integer): JElement_Builder; cdecl; overload;
    function add(element: Jrenderscript_Element; name: JString): JElement_Builder; cdecl; overload;
    function create: Jrenderscript_Element; cdecl;
  end;
  TJElement_Builder = class(TJavaGenericImport<JElement_BuilderClass, JElement_Builder>) end;

  JElement_DataKindClass = interface(JEnumClass)
    ['{994F8F53-C8C0-45BE-8AB4-0194F2E93C42}']
    {class} function _GetPIXEL_A: JElement_DataKind; cdecl;
    {class} function _GetPIXEL_DEPTH: JElement_DataKind; cdecl;
    {class} function _GetPIXEL_L: JElement_DataKind; cdecl;
    {class} function _GetPIXEL_LA: JElement_DataKind; cdecl;
    {class} function _GetPIXEL_RGB: JElement_DataKind; cdecl;
    {class} function _GetPIXEL_RGBA: JElement_DataKind; cdecl;
    {class} function _GetPIXEL_YUV: JElement_DataKind; cdecl;
    {class} function _GetUSER: JElement_DataKind; cdecl;
    {class} function valueOf(name: JString): JElement_DataKind; cdecl;
    {class} function values: TJavaObjectArray<JElement_DataKind>; cdecl;
    {class} property PIXEL_A: JElement_DataKind read _GetPIXEL_A;
    {class} property PIXEL_DEPTH: JElement_DataKind read _GetPIXEL_DEPTH;
    {class} property PIXEL_L: JElement_DataKind read _GetPIXEL_L;
    {class} property PIXEL_LA: JElement_DataKind read _GetPIXEL_LA;
    {class} property PIXEL_RGB: JElement_DataKind read _GetPIXEL_RGB;
    {class} property PIXEL_RGBA: JElement_DataKind read _GetPIXEL_RGBA;
    {class} property PIXEL_YUV: JElement_DataKind read _GetPIXEL_YUV;
    {class} property USER: JElement_DataKind read _GetUSER;
  end;

  [JavaSignature('android/renderscript/Element$DataKind')]
  JElement_DataKind = interface(JEnum)
    ['{4623979D-33C7-4948-A859-E6275BCC84CD}']
  end;
  TJElement_DataKind = class(TJavaGenericImport<JElement_DataKindClass, JElement_DataKind>) end;

  JElement_DataTypeClass = interface(JEnumClass)
    ['{196C4F4D-D545-43A4-8E4C-302BFCEB06E6}']
    {class} function _GetBOOLEAN: JElement_DataType; cdecl;
    {class} function _GetFLOAT_16: JElement_DataType; cdecl;
    {class} function _GetFLOAT_32: JElement_DataType; cdecl;
    {class} function _GetFLOAT_64: JElement_DataType; cdecl;
    {class} function _GetMATRIX_2X2: JElement_DataType; cdecl;
    {class} function _GetMATRIX_3X3: JElement_DataType; cdecl;
    {class} function _GetMATRIX_4X4: JElement_DataType; cdecl;
    {class} function _GetNONE: JElement_DataType; cdecl;
    {class} function _GetRS_ALLOCATION: JElement_DataType; cdecl;
    {class} function _GetRS_ELEMENT: JElement_DataType; cdecl;
    {class} function _GetRS_FONT: JElement_DataType; cdecl;
    {class} function _GetRS_MESH: JElement_DataType; cdecl;
    {class} function _GetRS_PROGRAM_FRAGMENT: JElement_DataType; cdecl;
    {class} function _GetRS_PROGRAM_RASTER: JElement_DataType; cdecl;
    {class} function _GetRS_PROGRAM_STORE: JElement_DataType; cdecl;
    {class} function _GetRS_PROGRAM_VERTEX: JElement_DataType; cdecl;
    {class} function _GetRS_SAMPLER: JElement_DataType; cdecl;
    {class} function _GetRS_SCRIPT: JElement_DataType; cdecl;
    {class} function _GetRS_TYPE: JElement_DataType; cdecl;
    {class} function _GetSIGNED_16: JElement_DataType; cdecl;
    {class} function _GetSIGNED_32: JElement_DataType; cdecl;
    {class} function _GetSIGNED_64: JElement_DataType; cdecl;
    {class} function _GetSIGNED_8: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_16: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_32: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_4_4_4_4: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_5_5_5_1: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_5_6_5: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_64: JElement_DataType; cdecl;
    {class} function _GetUNSIGNED_8: JElement_DataType; cdecl;
    {class} function valueOf(name: JString): JElement_DataType; cdecl;
    {class} function values: TJavaObjectArray<JElement_DataType>; cdecl;
    {class} property BOOLEAN: JElement_DataType read _GetBOOLEAN;
    {class} property FLOAT_16: JElement_DataType read _GetFLOAT_16;
    {class} property FLOAT_32: JElement_DataType read _GetFLOAT_32;
    {class} property FLOAT_64: JElement_DataType read _GetFLOAT_64;
    {class} property MATRIX_2X2: JElement_DataType read _GetMATRIX_2X2;
    {class} property MATRIX_3X3: JElement_DataType read _GetMATRIX_3X3;
    {class} property MATRIX_4X4: JElement_DataType read _GetMATRIX_4X4;
    {class} property NONE: JElement_DataType read _GetNONE;
    {class} property RS_ALLOCATION: JElement_DataType read _GetRS_ALLOCATION;
    {class} property RS_ELEMENT: JElement_DataType read _GetRS_ELEMENT;
    {class} property RS_FONT: JElement_DataType read _GetRS_FONT;
    {class} property RS_MESH: JElement_DataType read _GetRS_MESH;
    {class} property RS_PROGRAM_FRAGMENT: JElement_DataType read _GetRS_PROGRAM_FRAGMENT;
    {class} property RS_PROGRAM_RASTER: JElement_DataType read _GetRS_PROGRAM_RASTER;
    {class} property RS_PROGRAM_STORE: JElement_DataType read _GetRS_PROGRAM_STORE;
    {class} property RS_PROGRAM_VERTEX: JElement_DataType read _GetRS_PROGRAM_VERTEX;
    {class} property RS_SAMPLER: JElement_DataType read _GetRS_SAMPLER;
    {class} property RS_SCRIPT: JElement_DataType read _GetRS_SCRIPT;
    {class} property RS_TYPE: JElement_DataType read _GetRS_TYPE;
    {class} property SIGNED_16: JElement_DataType read _GetSIGNED_16;
    {class} property SIGNED_32: JElement_DataType read _GetSIGNED_32;
    {class} property SIGNED_64: JElement_DataType read _GetSIGNED_64;
    {class} property SIGNED_8: JElement_DataType read _GetSIGNED_8;
    {class} property UNSIGNED_16: JElement_DataType read _GetUNSIGNED_16;
    {class} property UNSIGNED_32: JElement_DataType read _GetUNSIGNED_32;
    {class} property UNSIGNED_4_4_4_4: JElement_DataType read _GetUNSIGNED_4_4_4_4;
    {class} property UNSIGNED_5_5_5_1: JElement_DataType read _GetUNSIGNED_5_5_5_1;
    {class} property UNSIGNED_5_6_5: JElement_DataType read _GetUNSIGNED_5_6_5;
    {class} property UNSIGNED_64: JElement_DataType read _GetUNSIGNED_64;
    {class} property UNSIGNED_8: JElement_DataType read _GetUNSIGNED_8;
  end;

  [JavaSignature('android/renderscript/Element$DataType')]
  JElement_DataType = interface(JEnum)
    ['{CD73A622-83C1-4E6A-AAA3-FDDD26FE6D70}']
  end;
  TJElement_DataType = class(TJavaGenericImport<JElement_DataTypeClass, JElement_DataType>) end;

  JFieldPackerClass = interface(JObjectClass)
    ['{A81ECEEF-416F-4EF9-B267-A620F21CA310}']
    {class} function init(len: Integer): JFieldPacker; cdecl; overload;
    {class} function init(data: TJavaArray<Byte>): JFieldPacker; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/FieldPacker')]
  JFieldPacker = interface(JObject)
    ['{B633F233-E449-47BC-8DB1-E8E82116D429}']
    procedure addBoolean(v: Boolean); cdecl;
    procedure addF32(v: Single); cdecl; overload;
    procedure addF32(v: JFloat2); cdecl; overload;
    procedure addF32(v: JFloat3); cdecl; overload;
    procedure addF32(v: JFloat4); cdecl; overload;
    procedure addF64(v: Double); cdecl; overload;
    procedure addF64(v: JDouble2); cdecl; overload;
    procedure addF64(v: JDouble3); cdecl; overload;
    procedure addF64(v: JDouble4); cdecl; overload;
    procedure addI16(v: SmallInt); cdecl; overload;
    procedure addI16(v: JShort2); cdecl; overload;
    procedure addI16(v: JShort3); cdecl; overload;
    procedure addI16(v: JShort4); cdecl; overload;
    procedure addI32(v: Integer); cdecl; overload;
    procedure addI32(v: JInt2); cdecl; overload;
    procedure addI32(v: JInt3); cdecl; overload;
    procedure addI32(v: JInt4); cdecl; overload;
    procedure addI64(v: Int64); cdecl; overload;
    procedure addI64(v: JLong2); cdecl; overload;
    procedure addI64(v: JLong3); cdecl; overload;
    procedure addI64(v: JLong4); cdecl; overload;
    procedure addI8(v: Byte); cdecl; overload;
    procedure addI8(v: JByte2); cdecl; overload;
    procedure addI8(v: JByte3); cdecl; overload;
    procedure addI8(v: JByte4); cdecl; overload;
    procedure addMatrix(v: JMatrix4f); cdecl; overload;
    procedure addMatrix(v: JMatrix3f); cdecl; overload;
    procedure addMatrix(v: JMatrix2f); cdecl; overload;
    procedure addObj(obj: JBaseObj); cdecl;
    procedure addU16(v: Integer); cdecl; overload;
    procedure addU16(v: JInt2); cdecl; overload;
    procedure addU16(v: JInt3); cdecl; overload;
    procedure addU16(v: JInt4); cdecl; overload;
    procedure addU32(v: Int64); cdecl; overload;
    procedure addU32(v: JLong2); cdecl; overload;
    procedure addU32(v: JLong3); cdecl; overload;
    procedure addU32(v: JLong4); cdecl; overload;
    procedure addU64(v: Int64); cdecl; overload;
    procedure addU64(v: JLong2); cdecl; overload;
    procedure addU64(v: JLong3); cdecl; overload;
    procedure addU64(v: JLong4); cdecl; overload;
    procedure addU8(v: SmallInt); cdecl; overload;
    procedure addU8(v: JShort2); cdecl; overload;
    procedure addU8(v: JShort3); cdecl; overload;
    procedure addU8(v: JShort4); cdecl; overload;
    procedure align(v: Integer); cdecl;
    function getData: TJavaArray<Byte>; cdecl;
    procedure reset; cdecl; overload;
    procedure reset(i: Integer); cdecl; overload;
    procedure skip(i: Integer); cdecl;
    function subBoolean: Boolean; cdecl;
    function subByte2: JByte2; cdecl;
    function subByte3: JByte3; cdecl;
    function subByte4: JByte4; cdecl;
    function subDouble2: JDouble2; cdecl;
    function subDouble3: JDouble3; cdecl;
    function subDouble4: JDouble4; cdecl;
    function subF32: Single; cdecl;
    function subF64: Double; cdecl;
    function subFloat2: JFloat2; cdecl;
    function subFloat3: JFloat3; cdecl;
    function subFloat4: JFloat4; cdecl;
    function subI16: SmallInt; cdecl;
    function subI32: Integer; cdecl;
    function subI64: Int64; cdecl;
    function subI8: Byte; cdecl;
    function subInt2: JInt2; cdecl;
    function subInt3: JInt3; cdecl;
    function subInt4: JInt4; cdecl;
    function subLong2: JLong2; cdecl;
    function subLong3: JLong3; cdecl;
    function subLong4: JLong4; cdecl;
    function subMatrix2f: JMatrix2f; cdecl;
    function subMatrix3f: JMatrix3f; cdecl;
    function subMatrix4f: JMatrix4f; cdecl;
    function subShort2: JShort2; cdecl;
    function subShort3: JShort3; cdecl;
    function subShort4: JShort4; cdecl;
    procedure subalign(v: Integer); cdecl;
  end;
  TJFieldPacker = class(TJavaGenericImport<JFieldPackerClass, JFieldPacker>) end;

  JFloat2Class = interface(JObjectClass)
    ['{6050BD90-1EA8-449A-8786-E4E8602ECF91}']
    {class} function init: JFloat2; cdecl; overload;
    {class} function init(x: Single; y: Single): JFloat2; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Float2')]
  JFloat2 = interface(JObject)
    ['{6EBAAAB6-A69D-4FC8-8F42-79F855A90837}']
    function _Getx: Single; cdecl;
    procedure _Setx(Value: Single); cdecl;
    function _Gety: Single; cdecl;
    procedure _Sety(Value: Single); cdecl;
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
  end;
  TJFloat2 = class(TJavaGenericImport<JFloat2Class, JFloat2>) end;

  JFloat3Class = interface(JObjectClass)
    ['{2E7D5B03-0B13-45A1-8A8B-44404B54AD9D}']
    {class} function init: JFloat3; cdecl; overload;
    {class} function init(x: Single; y: Single; z: Single): JFloat3; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Float3')]
  JFloat3 = interface(JObject)
    ['{92201ACB-EBD1-4596-A190-7CC34D9383F4}']
    function _Getx: Single; cdecl;
    procedure _Setx(Value: Single); cdecl;
    function _Gety: Single; cdecl;
    procedure _Sety(Value: Single); cdecl;
    function _Getz: Single; cdecl;
    procedure _Setz(Value: Single); cdecl;
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
    property z: Single read _Getz write _Setz;
  end;
  TJFloat3 = class(TJavaGenericImport<JFloat3Class, JFloat3>) end;

  JFloat4Class = interface(JObjectClass)
    ['{D2E339DE-2B56-4A91-B229-CB583171E2B6}']
    {class} function init: JFloat4; cdecl; overload;
    {class} function init(x: Single; y: Single; z: Single; w: Single): JFloat4; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Float4')]
  JFloat4 = interface(JObject)
    ['{5B33BB30-A5CA-4165-93A4-5F1EF8A07219}']
    function _Getw: Single; cdecl;
    procedure _Setw(Value: Single); cdecl;
    function _Getx: Single; cdecl;
    procedure _Setx(Value: Single); cdecl;
    function _Gety: Single; cdecl;
    procedure _Sety(Value: Single); cdecl;
    function _Getz: Single; cdecl;
    procedure _Setz(Value: Single); cdecl;
    property w: Single read _Getw write _Setw;
    property x: Single read _Getx write _Setx;
    property y: Single read _Gety write _Sety;
    property z: Single read _Getz write _Setz;
  end;
  TJFloat4 = class(TJavaGenericImport<JFloat4Class, JFloat4>) end;

  JInt2Class = interface(JObjectClass)
    ['{F2EC55B3-B3AE-4D65-931B-9CA64CA68996}']
    {class} function init: JInt2; cdecl; overload;
    {class} function init(x: Integer; y: Integer): JInt2; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Int2')]
  JInt2 = interface(JObject)
    ['{66BB1064-9C8A-4ABB-A0BC-9E68D358DC7B}']
    function _Getx: Integer; cdecl;
    procedure _Setx(Value: Integer); cdecl;
    function _Gety: Integer; cdecl;
    procedure _Sety(Value: Integer); cdecl;
    property x: Integer read _Getx write _Setx;
    property y: Integer read _Gety write _Sety;
  end;
  TJInt2 = class(TJavaGenericImport<JInt2Class, JInt2>) end;

  JInt3Class = interface(JObjectClass)
    ['{341DC972-08DD-4C54-BE97-240BBD8A7437}']
    {class} function init: JInt3; cdecl; overload;
    {class} function init(x: Integer; y: Integer; z: Integer): JInt3; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Int3')]
  JInt3 = interface(JObject)
    ['{7A5CA488-046A-4195-8FF7-54B3A93C7854}']
    function _Getx: Integer; cdecl;
    procedure _Setx(Value: Integer); cdecl;
    function _Gety: Integer; cdecl;
    procedure _Sety(Value: Integer); cdecl;
    function _Getz: Integer; cdecl;
    procedure _Setz(Value: Integer); cdecl;
    property x: Integer read _Getx write _Setx;
    property y: Integer read _Gety write _Sety;
    property z: Integer read _Getz write _Setz;
  end;
  TJInt3 = class(TJavaGenericImport<JInt3Class, JInt3>) end;

  JInt4Class = interface(JObjectClass)
    ['{2AF2D02D-02D7-4584-B031-D68072843E83}']
    {class} function init: JInt4; cdecl; overload;
    {class} function init(x: Integer; y: Integer; z: Integer; w: Integer): JInt4; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Int4')]
  JInt4 = interface(JObject)
    ['{42D7C7D1-30B7-45C3-94D6-8059A07F964D}']
    function _Getw: Integer; cdecl;
    procedure _Setw(Value: Integer); cdecl;
    function _Getx: Integer; cdecl;
    procedure _Setx(Value: Integer); cdecl;
    function _Gety: Integer; cdecl;
    procedure _Sety(Value: Integer); cdecl;
    function _Getz: Integer; cdecl;
    procedure _Setz(Value: Integer); cdecl;
    property w: Integer read _Getw write _Setw;
    property x: Integer read _Getx write _Setx;
    property y: Integer read _Gety write _Sety;
    property z: Integer read _Getz write _Setz;
  end;
  TJInt4 = class(TJavaGenericImport<JInt4Class, JInt4>) end;

  JLong2Class = interface(JObjectClass)
    ['{5CBD4D1F-1429-40E6-A678-7E387C2F7351}']
    {class} function init: JLong2; cdecl; overload;
    {class} function init(x: Int64; y: Int64): JLong2; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Long2')]
  JLong2 = interface(JObject)
    ['{90A3A963-CA89-4D9C-84E0-0EB14F2BA4CB}']
    function _Getx: Int64; cdecl;
    procedure _Setx(Value: Int64); cdecl;
    function _Gety: Int64; cdecl;
    procedure _Sety(Value: Int64); cdecl;
    property x: Int64 read _Getx write _Setx;
    property y: Int64 read _Gety write _Sety;
  end;
  TJLong2 = class(TJavaGenericImport<JLong2Class, JLong2>) end;

  JLong3Class = interface(JObjectClass)
    ['{08A2088D-461C-4FE0-97E4-385DABAD1FA1}']
    {class} function init: JLong3; cdecl; overload;
    {class} function init(x: Int64; y: Int64; z: Int64): JLong3; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Long3')]
  JLong3 = interface(JObject)
    ['{95FD593E-F7C6-4A93-B523-16F2191CE900}']
    function _Getx: Int64; cdecl;
    procedure _Setx(Value: Int64); cdecl;
    function _Gety: Int64; cdecl;
    procedure _Sety(Value: Int64); cdecl;
    function _Getz: Int64; cdecl;
    procedure _Setz(Value: Int64); cdecl;
    property x: Int64 read _Getx write _Setx;
    property y: Int64 read _Gety write _Sety;
    property z: Int64 read _Getz write _Setz;
  end;
  TJLong3 = class(TJavaGenericImport<JLong3Class, JLong3>) end;

  JLong4Class = interface(JObjectClass)
    ['{7A4DCA11-1342-4361-9C80-A33F6EFD795B}']
    {class} function init: JLong4; cdecl; overload;
    {class} function init(x: Int64; y: Int64; z: Int64; w: Int64): JLong4; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Long4')]
  JLong4 = interface(JObject)
    ['{CE624C29-BF0C-409E-BD05-4E122BF5DB0B}']
    function _Getw: Int64; cdecl;
    procedure _Setw(Value: Int64); cdecl;
    function _Getx: Int64; cdecl;
    procedure _Setx(Value: Int64); cdecl;
    function _Gety: Int64; cdecl;
    procedure _Sety(Value: Int64); cdecl;
    function _Getz: Int64; cdecl;
    procedure _Setz(Value: Int64); cdecl;
    property w: Int64 read _Getw write _Setw;
    property x: Int64 read _Getx write _Setx;
    property y: Int64 read _Gety write _Sety;
    property z: Int64 read _Getz write _Setz;
  end;
  TJLong4 = class(TJavaGenericImport<JLong4Class, JLong4>) end;

  JMatrix2fClass = interface(JObjectClass)
    ['{62C79CAE-98F8-4A49-AEA5-16898C7545D5}']
    {class} function init: JMatrix2f; cdecl; overload;
    {class} function init(dataArray: TJavaArray<Single>): JMatrix2f; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Matrix2f')]
  JMatrix2f = interface(JObject)
    ['{1E766A89-5F56-4818-9864-06837EAF0E0C}']
    function &get(x: Integer; y: Integer): Single; cdecl;
    function getArray: TJavaArray<Single>; cdecl;
    procedure load(src: JMatrix2f); cdecl;
    procedure loadIdentity; cdecl;
    procedure loadMultiply(lhs: JMatrix2f; rhs: JMatrix2f); cdecl;
    procedure loadRotate(rot: Single); cdecl;
    procedure loadScale(x: Single; y: Single); cdecl;
    procedure multiply(rhs: JMatrix2f); cdecl;
    procedure rotate(rot: Single); cdecl;
    procedure scale(x: Single; y: Single); cdecl;
    procedure &set(x: Integer; y: Integer; v: Single); cdecl;
    procedure transpose; cdecl;
  end;
  TJMatrix2f = class(TJavaGenericImport<JMatrix2fClass, JMatrix2f>) end;

  JMatrix3fClass = interface(JObjectClass)
    ['{2F3F915F-90FE-477A-AC7E-CCCF53E5F631}']
    {class} function init: JMatrix3f; cdecl; overload;
    {class} function init(dataArray: TJavaArray<Single>): JMatrix3f; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Matrix3f')]
  JMatrix3f = interface(JObject)
    ['{4AF7E3DE-7AC0-4677-91AB-8DF4ECF227A1}']
    function &get(x: Integer; y: Integer): Single; cdecl;
    function getArray: TJavaArray<Single>; cdecl;
    procedure load(src: JMatrix3f); cdecl;
    procedure loadIdentity; cdecl;
    procedure loadMultiply(lhs: JMatrix3f; rhs: JMatrix3f); cdecl;
    procedure loadRotate(rot: Single; x: Single; y: Single; z: Single); cdecl; overload;
    procedure loadRotate(rot: Single); cdecl; overload;
    procedure loadScale(x: Single; y: Single); cdecl; overload;
    procedure loadScale(x: Single; y: Single; z: Single); cdecl; overload;
    procedure loadTranslate(x: Single; y: Single); cdecl;
    procedure multiply(rhs: JMatrix3f); cdecl;
    procedure rotate(rot: Single; x: Single; y: Single; z: Single); cdecl; overload;
    procedure rotate(rot: Single); cdecl; overload;
    procedure scale(x: Single; y: Single); cdecl; overload;
    procedure scale(x: Single; y: Single; z: Single); cdecl; overload;
    procedure &set(x: Integer; y: Integer; v: Single); cdecl;
    procedure translate(x: Single; y: Single); cdecl;
    procedure transpose; cdecl;
  end;
  TJMatrix3f = class(TJavaGenericImport<JMatrix3fClass, JMatrix3f>) end;

  JMatrix4fClass = interface(JObjectClass)
    ['{6483A6CB-C4F7-4E4E-B40E-F887A68403BD}']
    {class} function init: JMatrix4f; cdecl; overload;
    {class} function init(dataArray: TJavaArray<Single>): JMatrix4f; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Matrix4f')]
  JMatrix4f = interface(JObject)
    ['{3E2D34F5-4AB7-4CBC-B0B9-255312A3ACFD}']
    function &get(x: Integer; y: Integer): Single; cdecl;
    function getArray: TJavaArray<Single>; cdecl;
    function inverse: Boolean; cdecl;
    function inverseTranspose: Boolean; cdecl;
    procedure load(src: JMatrix4f); cdecl;
    procedure loadFrustum(l: Single; r: Single; b: Single; t: Single; n: Single; f: Single); cdecl;
    procedure loadIdentity; cdecl;
    procedure loadMultiply(lhs: JMatrix4f; rhs: JMatrix4f); cdecl;
    procedure loadOrtho(l: Single; r: Single; b: Single; t: Single; n: Single; f: Single); cdecl;
    procedure loadOrthoWindow(w: Integer; h: Integer); cdecl;
    procedure loadPerspective(fovy: Single; aspect: Single; near: Single; far: Single); cdecl;
    procedure loadProjectionNormalized(w: Integer; h: Integer); cdecl;
    procedure loadRotate(rot: Single; x: Single; y: Single; z: Single); cdecl;
    procedure loadScale(x: Single; y: Single; z: Single); cdecl;
    procedure loadTranslate(x: Single; y: Single; z: Single); cdecl;
    procedure multiply(rhs: JMatrix4f); cdecl;
    procedure rotate(rot: Single; x: Single; y: Single; z: Single); cdecl;
    procedure scale(x: Single; y: Single; z: Single); cdecl;
    procedure &set(x: Integer; y: Integer; v: Single); cdecl;
    procedure translate(x: Single; y: Single; z: Single); cdecl;
    procedure transpose; cdecl;
  end;
  TJMatrix4f = class(TJavaGenericImport<JMatrix4fClass, JMatrix4f>) end;

  JRSRuntimeExceptionClass = interface(JRuntimeExceptionClass)
    ['{03D61660-F32B-4021-8229-25BF94795756}']
    {class} function init(string_: JString): JRSRuntimeException; cdecl;
  end;

  [JavaSignature('android/renderscript/RSRuntimeException')]
  JRSRuntimeException = interface(JRuntimeException)
    ['{DD1FAE5C-1626-48AA-8C1F-1309C133CD47}']
  end;
  TJRSRuntimeException = class(TJavaGenericImport<JRSRuntimeExceptionClass, JRSRuntimeException>) end;

  JRSDriverExceptionClass = interface(JRSRuntimeExceptionClass)
    ['{8C8445E5-1F76-441B-9747-D4DB57DCCC3C}']
    {class} function init(string_: JString): JRSDriverException; cdecl;
  end;

  [JavaSignature('android/renderscript/RSDriverException')]
  JRSDriverException = interface(JRSRuntimeException)
    ['{7F2014E4-DD11-498A-94C2-B2E13A43ADA5}']
  end;
  TJRSDriverException = class(TJavaGenericImport<JRSDriverExceptionClass, JRSDriverException>) end;

  JRSIllegalArgumentExceptionClass = interface(JRSRuntimeExceptionClass)
    ['{EE678B06-E8C2-47B5-89DA-A2BEFC96DC51}']
    {class} function init(string_: JString): JRSIllegalArgumentException; cdecl;
  end;

  [JavaSignature('android/renderscript/RSIllegalArgumentException')]
  JRSIllegalArgumentException = interface(JRSRuntimeException)
    ['{166BD74D-2CD8-492F-82CF-4DD4A49A9A53}']
  end;
  TJRSIllegalArgumentException = class(TJavaGenericImport<JRSIllegalArgumentExceptionClass, JRSIllegalArgumentException>) end;

  JRSInvalidStateExceptionClass = interface(JRSRuntimeExceptionClass)
    ['{7AA83170-1921-4DC8-85C3-59434692FF5C}']
    {class} function init(string_: JString): JRSInvalidStateException; cdecl;
  end;

  [JavaSignature('android/renderscript/RSInvalidStateException')]
  JRSInvalidStateException = interface(JRSRuntimeException)
    ['{6DF1A237-27E3-4C24-B258-FB956C279B1E}']
  end;
  TJRSInvalidStateException = class(TJavaGenericImport<JRSInvalidStateExceptionClass, JRSInvalidStateException>) end;

  JRenderScriptClass = interface(JObjectClass)
    ['{ACD2A56F-451D-4D2C-9EF0-339EE08C663F}']
    {class} function _GetCREATE_FLAG_LOW_LATENCY: Integer; cdecl;
    {class} function _GetCREATE_FLAG_LOW_POWER: Integer; cdecl;
    {class} function _GetCREATE_FLAG_NONE: Integer; cdecl;
    {class} function create(ctx: JContext): JRenderScript; cdecl; overload;
    {class} function create(ctx: JContext; ct: JRenderScript_ContextType): JRenderScript; cdecl; overload;
    {class} function create(ctx: JContext; ct: JRenderScript_ContextType; flags: Integer): JRenderScript; cdecl; overload;
    {class} function createMultiContext(ctx: JContext; ct: JRenderScript_ContextType; flags: Integer; API_number: Integer): JRenderScript; cdecl;
    {class} function getMinorVersion: Int64; cdecl;
    {class} procedure releaseAllContexts; cdecl;
    {class} property CREATE_FLAG_LOW_LATENCY: Integer read _GetCREATE_FLAG_LOW_LATENCY;
    {class} property CREATE_FLAG_LOW_POWER: Integer read _GetCREATE_FLAG_LOW_POWER;
    {class} property CREATE_FLAG_NONE: Integer read _GetCREATE_FLAG_NONE;
  end;

  [JavaSignature('android/renderscript/RenderScript')]
  JRenderScript = interface(JObject)
    ['{1375A3CE-12EA-4DB9-B20F-128B944A172B}']
    procedure contextDump; cdecl;
    procedure destroy; cdecl;
    procedure finish; cdecl;
    function getApplicationContext: JContext; cdecl;
    function getErrorHandler: JRenderScript_RSErrorHandler; cdecl;
    function getMessageHandler: JRenderScript_RSMessageHandler; cdecl;
    procedure sendMessage(id: Integer; data: TJavaArray<Integer>); cdecl;
    procedure setErrorHandler(msg: JRenderScript_RSErrorHandler); cdecl;
    procedure setMessageHandler(msg: JRenderScript_RSMessageHandler); cdecl;
    procedure setPriority(p: JRenderScript_Priority); cdecl;
  end;
  TJRenderScript = class(TJavaGenericImport<JRenderScriptClass, JRenderScript>) end;

  JRenderScript_ContextTypeClass = interface(JEnumClass)
    ['{9AC82907-B608-45DE-9C01-32619316E00B}']
    {class} function _GetDEBUG: JRenderScript_ContextType; cdecl;
    {class} function _GetNORMAL: JRenderScript_ContextType; cdecl;
    {class} function _GetPROFILE: JRenderScript_ContextType; cdecl;
    {class} function valueOf(name: JString): JRenderScript_ContextType; cdecl;
    {class} function values: TJavaObjectArray<JRenderScript_ContextType>; cdecl;
    {class} property DEBUG: JRenderScript_ContextType read _GetDEBUG;
    {class} property NORMAL: JRenderScript_ContextType read _GetNORMAL;
    {class} property PROFILE: JRenderScript_ContextType read _GetPROFILE;
  end;

  [JavaSignature('android/renderscript/RenderScript$ContextType')]
  JRenderScript_ContextType = interface(JEnum)
    ['{A7DD5E86-28E9-46D5-86F3-42B3AC09F992}']
  end;
  TJRenderScript_ContextType = class(TJavaGenericImport<JRenderScript_ContextTypeClass, JRenderScript_ContextType>) end;

  JRenderScript_PriorityClass = interface(JEnumClass)
    ['{F4D97121-C8FA-4318-AFF2-EA810216B789}']
    {class} function _GetLOW: JRenderScript_Priority; cdecl;
    {class} function _GetNORMAL: JRenderScript_Priority; cdecl;
    {class} function valueOf(name: JString): JRenderScript_Priority; cdecl;
    {class} function values: TJavaObjectArray<JRenderScript_Priority>; cdecl;
    {class} property LOW: JRenderScript_Priority read _GetLOW;
    {class} property NORMAL: JRenderScript_Priority read _GetNORMAL;
  end;

  [JavaSignature('android/renderscript/RenderScript$Priority')]
  JRenderScript_Priority = interface(JEnum)
    ['{A53C6A12-652B-4272-AEBC-DC5E71376BA6}']
  end;
  TJRenderScript_Priority = class(TJavaGenericImport<JRenderScript_PriorityClass, JRenderScript_Priority>) end;

  JRenderScript_RSErrorHandlerClass = interface(JObjectClass)
    ['{C53AAF16-0541-4A5E-85D5-62A3D50EFA2B}']
    {class} function init: JRenderScript_RSErrorHandler; cdecl;
  end;

  [JavaSignature('android/renderscript/RenderScript$RSErrorHandler')]
  JRenderScript_RSErrorHandler = interface(JObject)
    ['{50BCE00A-0A52-4159-A030-D5EDEBABD142}']
    procedure run; cdecl;
  end;
  TJRenderScript_RSErrorHandler = class(TJavaGenericImport<JRenderScript_RSErrorHandlerClass, JRenderScript_RSErrorHandler>) end;

  JRenderScript_RSMessageHandlerClass = interface(JObjectClass)
    ['{46CB4337-A752-4A50-BE4D-1F8BE837700C}']
    {class} function init: JRenderScript_RSMessageHandler; cdecl;
  end;

  [JavaSignature('android/renderscript/RenderScript$RSMessageHandler')]
  JRenderScript_RSMessageHandler = interface(JObject)
    ['{7B2E311E-5144-4366-A315-1689480B541D}']
    procedure run; cdecl;
  end;
  TJRenderScript_RSMessageHandler = class(TJavaGenericImport<JRenderScript_RSMessageHandlerClass, JRenderScript_RSMessageHandler>) end;

  JSamplerClass = interface(JBaseObjClass)
    ['{070535D1-2A17-4382-9BB1-77F0D4164FA2}']
    {class} function CLAMP_LINEAR(rs: JRenderScript): JSampler; cdecl;
    {class} function CLAMP_LINEAR_MIP_LINEAR(rs: JRenderScript): JSampler; cdecl;
    {class} function CLAMP_NEAREST(rs: JRenderScript): JSampler; cdecl;
    {class} function MIRRORED_REPEAT_LINEAR(rs: JRenderScript): JSampler; cdecl;
    {class} function MIRRORED_REPEAT_LINEAR_MIP_LINEAR(rs: JRenderScript): JSampler; cdecl;
    {class} function MIRRORED_REPEAT_NEAREST(rs: JRenderScript): JSampler; cdecl;
    {class} function WRAP_LINEAR(rs: JRenderScript): JSampler; cdecl;
    {class} function WRAP_LINEAR_MIP_LINEAR(rs: JRenderScript): JSampler; cdecl;
    {class} function WRAP_NEAREST(rs: JRenderScript): JSampler; cdecl;
  end;

  [JavaSignature('android/renderscript/Sampler')]
  JSampler = interface(JBaseObj)
    ['{0FA0CED8-A2D6-44F9-9473-1F9CBCB613BA}']
    function getAnisotropy: Single; cdecl;
    function getMagnification: JSampler_Value; cdecl;
    function getMinification: JSampler_Value; cdecl;
    function getWrapS: JSampler_Value; cdecl;
    function getWrapT: JSampler_Value; cdecl;
  end;
  TJSampler = class(TJavaGenericImport<JSamplerClass, JSampler>) end;

  JSampler_BuilderClass = interface(JObjectClass)
    ['{37390FE7-A7CC-4990-9B18-0F1FF3F310E2}']
    {class} function init(rs: JRenderScript): JSampler_Builder; cdecl;
  end;

  [JavaSignature('android/renderscript/Sampler$Builder')]
  JSampler_Builder = interface(JObject)
    ['{D56000B6-2005-4FEE-B0C4-01581A09D489}']
    function create: JSampler; cdecl;
    procedure setAnisotropy(v: Single); cdecl;
    procedure setMagnification(v: JSampler_Value); cdecl;
    procedure setMinification(v: JSampler_Value); cdecl;
    procedure setWrapS(v: JSampler_Value); cdecl;
    procedure setWrapT(v: JSampler_Value); cdecl;
  end;
  TJSampler_Builder = class(TJavaGenericImport<JSampler_BuilderClass, JSampler_Builder>) end;

  JSampler_ValueClass = interface(JEnumClass)
    ['{8077E896-05A5-444B-B3E7-344DDDB57BC0}']
    {class} function _GetCLAMP: JSampler_Value; cdecl;
    {class} function _GetLINEAR: JSampler_Value; cdecl;
    {class} function _GetLINEAR_MIP_LINEAR: JSampler_Value; cdecl;
    {class} function _GetLINEAR_MIP_NEAREST: JSampler_Value; cdecl;
    {class} function _GetMIRRORED_REPEAT: JSampler_Value; cdecl;
    {class} function _GetNEAREST: JSampler_Value; cdecl;
    {class} function _GetWRAP: JSampler_Value; cdecl;
    {class} function valueOf(name: JString): JSampler_Value; cdecl;
    {class} function values: TJavaObjectArray<JSampler_Value>; cdecl;
    {class} property CLAMP: JSampler_Value read _GetCLAMP;
    {class} property LINEAR: JSampler_Value read _GetLINEAR;
    {class} property LINEAR_MIP_LINEAR: JSampler_Value read _GetLINEAR_MIP_LINEAR;
    {class} property LINEAR_MIP_NEAREST: JSampler_Value read _GetLINEAR_MIP_NEAREST;
    {class} property MIRRORED_REPEAT: JSampler_Value read _GetMIRRORED_REPEAT;
    {class} property NEAREST: JSampler_Value read _GetNEAREST;
    {class} property WRAP: JSampler_Value read _GetWRAP;
  end;

  [JavaSignature('android/renderscript/Sampler$Value')]
  JSampler_Value = interface(JEnum)
    ['{AE9FA332-D600-46B2-B4C7-6AA8BD8C88B1}']
  end;
  TJSampler_Value = class(TJavaGenericImport<JSampler_ValueClass, JSampler_Value>) end;

  JScriptClass = interface(JBaseObjClass)
    ['{3C4B94DB-AA9D-4EAE-BE4C-22FB4E0E6EAE}']
  end;

  [JavaSignature('android/renderscript/Script')]
  JScript = interface(JBaseObj)
    ['{25CE110C-9510-469A-A5C5-957A802D881A}']
    procedure bindAllocation(va: JAllocation; slot: Integer); cdecl;
    function getVarB(index: Integer): Boolean; cdecl;
    function getVarD(index: Integer): Double; cdecl;
    function getVarF(index: Integer): Single; cdecl;
    function getVarI(index: Integer): Integer; cdecl;
    function getVarJ(index: Integer): Int64; cdecl;
    procedure getVarV(index: Integer; v: JFieldPacker); cdecl;
    procedure setTimeZone(timeZone: JString); cdecl;
    procedure setVar(index: Integer; v: Single); cdecl; overload;
    procedure setVar(index: Integer; v: Double); cdecl; overload;
    procedure setVar(index: Integer; v: Integer); cdecl; overload;
    procedure setVar(index: Integer; v: Int64); cdecl; overload;
    procedure setVar(index: Integer; v: Boolean); cdecl; overload;
    procedure setVar(index: Integer; o: JBaseObj); cdecl; overload;
    procedure setVar(index: Integer; v: JFieldPacker); cdecl; overload;
    procedure setVar(index: Integer; v: JFieldPacker; e: Jrenderscript_Element; dims: TJavaArray<Integer>); cdecl; overload;
  end;
  TJScript = class(TJavaGenericImport<JScriptClass, JScript>) end;

  JScript_BuilderClass = interface(JObjectClass)
    ['{C053D63B-E1EA-4DDF-93C0-B7FFACD38922}']
  end;

  [JavaSignature('android/renderscript/Script$Builder')]
  JScript_Builder = interface(JObject)
    ['{FF9991B7-C7CD-4652-A3D6-9A02EC654457}']
  end;
  TJScript_Builder = class(TJavaGenericImport<JScript_BuilderClass, JScript_Builder>) end;

  JScript_FieldBaseClass = interface(JObjectClass)
    ['{55F38A69-0626-4BDD-BB0C-786E6044EC93}']
  end;

  [JavaSignature('android/renderscript/Script$FieldBase')]
  JScript_FieldBase = interface(JObject)
    ['{8921B206-F389-439E-8DCB-C51630F72EB4}']
    function getAllocation: JAllocation; cdecl;
    function getElement: Jrenderscript_Element; cdecl;
    function getType: JType; cdecl;
    procedure updateAllocation; cdecl;
  end;
  TJScript_FieldBase = class(TJavaGenericImport<JScript_FieldBaseClass, JScript_FieldBase>) end;

  JScript_FieldIDClass = interface(JBaseObjClass)
    ['{A0544DC8-4ECA-445F-B266-6B2F138E6D92}']
  end;

  [JavaSignature('android/renderscript/Script$FieldID')]
  JScript_FieldID = interface(JBaseObj)
    ['{B05644E2-FEB8-4DD7-819F-1D5F85648761}']
  end;
  TJScript_FieldID = class(TJavaGenericImport<JScript_FieldIDClass, JScript_FieldID>) end;

  JScript_InvokeIDClass = interface(JBaseObjClass)
    ['{AC750D8A-FFDF-4A29-90EC-5EDCC99A2D62}']
  end;

  [JavaSignature('android/renderscript/Script$InvokeID')]
  JScript_InvokeID = interface(JBaseObj)
    ['{19995BD8-07C5-4CA4-A4BE-3F29F4BC5C01}']
  end;
  TJScript_InvokeID = class(TJavaGenericImport<JScript_InvokeIDClass, JScript_InvokeID>) end;

  JScript_KernelIDClass = interface(JBaseObjClass)
    ['{1D8D5235-9976-469E-AC1D-7FA0B10C5AAB}']
  end;

  [JavaSignature('android/renderscript/Script$KernelID')]
  JScript_KernelID = interface(JBaseObj)
    ['{0F171B16-CC18-41DE-A121-79023D34C764}']
  end;
  TJScript_KernelID = class(TJavaGenericImport<JScript_KernelIDClass, JScript_KernelID>) end;

  JScript_LaunchOptionsClass = interface(JObjectClass)
    ['{1BDBA116-91A8-4614-B453-E851D81CC968}']
    {class} function init: JScript_LaunchOptions; cdecl;
  end;

  [JavaSignature('android/renderscript/Script$LaunchOptions')]
  JScript_LaunchOptions = interface(JObject)
    ['{6AF9C535-E4CE-4FE0-AD57-42EC87BA046A}']
    function getXEnd: Integer; cdecl;
    function getXStart: Integer; cdecl;
    function getYEnd: Integer; cdecl;
    function getYStart: Integer; cdecl;
    function getZEnd: Integer; cdecl;
    function getZStart: Integer; cdecl;
    function setX(xstartArg: Integer; xendArg: Integer): JScript_LaunchOptions; cdecl;
    function setY(ystartArg: Integer; yendArg: Integer): JScript_LaunchOptions; cdecl;
    function setZ(zstartArg: Integer; zendArg: Integer): JScript_LaunchOptions; cdecl;
  end;
  TJScript_LaunchOptions = class(TJavaGenericImport<JScript_LaunchOptionsClass, JScript_LaunchOptions>) end;

  JScriptCClass = interface(JScriptClass)
    ['{E8C599CD-8577-488C-BB2E-6E258B28F9ED}']
  end;

  [JavaSignature('android/renderscript/ScriptC')]
  JScriptC = interface(JScript)
    ['{EE227749-F51D-48C8-9BBC-2400ACD7C317}']
  end;
  TJScriptC = class(TJavaGenericImport<JScriptCClass, JScriptC>) end;

  JScriptGroupClass = interface(JBaseObjClass)
    ['{1045A21B-0421-41C3-9F70-00ADCA568DCC}']
  end;

  [JavaSignature('android/renderscript/ScriptGroup')]
  JScriptGroup = interface(JBaseObj)
    ['{47963EEE-BB3D-48A5-AE1C-6EFFB53886CC}']
    procedure destroy; cdecl;
    procedure execute; cdecl; overload;//Deprecated
    procedure setInput(s: JScript_KernelID; a: JAllocation); cdecl;//Deprecated
    procedure setOutput(s: JScript_KernelID; a: JAllocation); cdecl;//Deprecated
  end;
  TJScriptGroup = class(TJavaGenericImport<JScriptGroupClass, JScriptGroup>) end;

  JScriptGroup_BindingClass = interface(JObjectClass)
    ['{92F1F6B6-E6C8-4C3F-8D44-746D24F98D9F}']
    {class} function init(field: JScript_FieldID; value: JObject): JScriptGroup_Binding; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptGroup$Binding')]
  JScriptGroup_Binding = interface(JObject)
    ['{2B4A8CA8-01CB-4649-86EC-4EA257DFC73C}']
  end;
  TJScriptGroup_Binding = class(TJavaGenericImport<JScriptGroup_BindingClass, JScriptGroup_Binding>) end;

  JScriptGroup_BuilderClass = interface(JObjectClass)
    ['{601170DB-3547-48AD-8C65-51592BFF2ED6}']
    {class} function init(rs: JRenderScript): JScriptGroup_Builder; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptGroup$Builder')]
  JScriptGroup_Builder = interface(JObject)
    ['{1BFC76E1-C11E-46D7-925E-E6F8BB5A04E2}']
    function addConnection(t: JType; from: JScript_KernelID; to_: JScript_FieldID): JScriptGroup_Builder; cdecl; overload;
    function addConnection(t: JType; from: JScript_KernelID; to_: JScript_KernelID): JScriptGroup_Builder; cdecl; overload;
    function addKernel(k: JScript_KernelID): JScriptGroup_Builder; cdecl;
    function create: JScriptGroup; cdecl;
  end;
  TJScriptGroup_Builder = class(TJavaGenericImport<JScriptGroup_BuilderClass, JScriptGroup_Builder>) end;

  JScriptGroup_Builder2Class = interface(JObjectClass)
    ['{E02ABA26-A570-4325-9FE9-7948CB98189E}']
    {class} function init(rs: JRenderScript): JScriptGroup_Builder2; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptGroup$Builder2')]
  JScriptGroup_Builder2 = interface(JObject)
    ['{A6D8A0F0-57AE-489E-ACC6-680F07F8EA3C}']
    function addInput: JScriptGroup_Input; cdecl;
  end;
  TJScriptGroup_Builder2 = class(TJavaGenericImport<JScriptGroup_Builder2Class, JScriptGroup_Builder2>) end;

  JScriptGroup_ClosureClass = interface(JBaseObjClass)
    ['{0F69F36B-8782-4C0D-AA23-A67B4BBB8E08}']
  end;

  [JavaSignature('android/renderscript/ScriptGroup$Closure')]
  JScriptGroup_Closure = interface(JBaseObj)
    ['{6773E7A7-F3AF-482C-96EE-3837809B8045}']
    procedure destroy; cdecl;
    function getGlobal(field: JScript_FieldID): JScriptGroup_Future; cdecl;
    function getReturn: JScriptGroup_Future; cdecl;
  end;
  TJScriptGroup_Closure = class(TJavaGenericImport<JScriptGroup_ClosureClass, JScriptGroup_Closure>) end;

  JScriptGroup_FutureClass = interface(JObjectClass)
    ['{5FA43287-34F4-4CBC-91C0-E92718AE31F0}']
  end;

  [JavaSignature('android/renderscript/ScriptGroup$Future')]
  JScriptGroup_Future = interface(JObject)
    ['{0129A47F-30BE-4B1D-A632-C9B36486DC4C}']
  end;
  TJScriptGroup_Future = class(TJavaGenericImport<JScriptGroup_FutureClass, JScriptGroup_Future>) end;

  JScriptGroup_InputClass = interface(JObjectClass)
    ['{26F8BA5F-62A3-4316-A282-EA0CBFAA5C4C}']
  end;

  [JavaSignature('android/renderscript/ScriptGroup$Input')]
  JScriptGroup_Input = interface(JObject)
    ['{EC9FDE93-BD29-4BBB-9009-AE8E4DC3B918}']
  end;
  TJScriptGroup_Input = class(TJavaGenericImport<JScriptGroup_InputClass, JScriptGroup_Input>) end;

  JScriptIntrinsicClass = interface(JScriptClass)
    ['{EFFAD6DF-A3D4-461C-B594-851D221474ED}']
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsic')]
  JScriptIntrinsic = interface(JScript)
    ['{584A2682-B1D5-4DD4-A5DD-CBCF599E52E8}']
  end;
  TJScriptIntrinsic = class(TJavaGenericImport<JScriptIntrinsicClass, JScriptIntrinsic>) end;

  JScriptIntrinsic3DLUTClass = interface(JScriptIntrinsicClass)
    ['{C3C054BB-29E9-42EC-825E-B399F5D382FE}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsic3DLUT; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsic3DLUT')]
  JScriptIntrinsic3DLUT = interface(JScriptIntrinsic)
    ['{BAB26680-9347-44A2-AC20-3020D3596C69}']
    procedure forEach(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEach(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setLUT(lut: JAllocation); cdecl;
  end;
  TJScriptIntrinsic3DLUT = class(TJavaGenericImport<JScriptIntrinsic3DLUTClass, JScriptIntrinsic3DLUT>) end;

  JScriptIntrinsicBLASClass = interface(JScriptIntrinsicClass)
    ['{40581C44-819D-4AF0-B944-F34A6948AEBE}']
    {class} function _GetCONJ_TRANSPOSE: Integer; cdecl;
    {class} function _GetLEFT: Integer; cdecl;
    {class} function _GetLOWER: Integer; cdecl;
    {class} function _GetNON_UNIT: Integer; cdecl;
    {class} function _GetNO_TRANSPOSE: Integer; cdecl;
    {class} function _GetRIGHT: Integer; cdecl;
    {class} function _GetTRANSPOSE: Integer; cdecl;
    {class} function _GetUNIT: Integer; cdecl;
    {class} function _GetUPPER: Integer; cdecl;
    {class} function create(rs: JRenderScript): JScriptIntrinsicBLAS; cdecl;
    {class} property CONJ_TRANSPOSE: Integer read _GetCONJ_TRANSPOSE;
    {class} property LEFT: Integer read _GetLEFT;
    {class} property LOWER: Integer read _GetLOWER;
    {class} property NON_UNIT: Integer read _GetNON_UNIT;
    {class} property NO_TRANSPOSE: Integer read _GetNO_TRANSPOSE;
    {class} property RIGHT: Integer read _GetRIGHT;
    {class} property TRANSPOSE: Integer read _GetTRANSPOSE;
    {class} property &UNIT: Integer read _GetUNIT;
    {class} property UPPER: Integer read _GetUPPER;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicBLAS')]
  JScriptIntrinsicBLAS = interface(JScriptIntrinsic)
    ['{5612542C-48B4-47DE-90BC-A98935B00215}']
    procedure BNNM(A: JAllocation; a_offset: Integer; B: JAllocation; b_offset: Integer; C: JAllocation; c_offset: Integer; c_mult: Integer); cdecl;
    procedure CGBMV(TransA: Integer; KL: Integer; KU: Integer; alpha: JFloat2; A: JAllocation; X: JAllocation; incX: Integer; beta: JFloat2; Y: JAllocation; incY: Integer); cdecl;
    procedure CGEMM(TransA: Integer; TransB: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation; beta: JFloat2; C: JAllocation); cdecl;
    procedure CGEMV(TransA: Integer; alpha: JFloat2; A: JAllocation; X: JAllocation; incX: Integer; beta: JFloat2; Y: JAllocation; incY: Integer); cdecl;
    procedure CGERC(alpha: JFloat2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure CGERU(alpha: JFloat2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure CHBMV(Uplo: Integer; K: Integer; alpha: JFloat2; A: JAllocation; X: JAllocation; incX: Integer; beta: JFloat2; Y: JAllocation; incY: Integer); cdecl;
    procedure CHEMM(Side: Integer; Uplo: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation; beta: JFloat2; C: JAllocation); cdecl;
    procedure CHEMV(Uplo: Integer; alpha: JFloat2; A: JAllocation; X: JAllocation; incX: Integer; beta: JFloat2; Y: JAllocation; incY: Integer); cdecl;
    procedure CHER(Uplo: Integer; alpha: Single; X: JAllocation; incX: Integer; A: JAllocation); cdecl;
    procedure CHER2(Uplo: Integer; alpha: JFloat2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure CHER2K(Uplo: Integer; Trans: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation; beta: Single; C: JAllocation); cdecl;
    procedure CHERK(Uplo: Integer; Trans: Integer; alpha: Single; A: JAllocation; beta: Single; C: JAllocation); cdecl;
    procedure CHPMV(Uplo: Integer; alpha: JFloat2; Ap: JAllocation; X: JAllocation; incX: Integer; beta: JFloat2; Y: JAllocation; incY: Integer); cdecl;
    procedure CHPR(Uplo: Integer; alpha: Single; X: JAllocation; incX: Integer; Ap: JAllocation); cdecl;
    procedure CHPR2(Uplo: Integer; alpha: JFloat2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; Ap: JAllocation); cdecl;
    procedure CSYMM(Side: Integer; Uplo: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation; beta: JFloat2; C: JAllocation); cdecl;
    procedure CSYR2K(Uplo: Integer; Trans: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation; beta: JFloat2; C: JAllocation); cdecl;
    procedure CSYRK(Uplo: Integer; Trans: Integer; alpha: JFloat2; A: JAllocation; beta: JFloat2; C: JAllocation); cdecl;
    procedure CTBMV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure CTBSV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure CTPMV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure CTPSV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure CTRMM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation); cdecl;
    procedure CTRMV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure CTRSM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: JFloat2; A: JAllocation; B: JAllocation); cdecl;
    procedure CTRSV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure DGBMV(TransA: Integer; KL: Integer; KU: Integer; alpha: Double; A: JAllocation; X: JAllocation; incX: Integer; beta: Double; Y: JAllocation; incY: Integer); cdecl;
    procedure DGEMM(TransA: Integer; TransB: Integer; alpha: Double; A: JAllocation; B: JAllocation; beta: Double; C: JAllocation); cdecl;
    procedure DGEMV(TransA: Integer; alpha: Double; A: JAllocation; X: JAllocation; incX: Integer; beta: Double; Y: JAllocation; incY: Integer); cdecl;
    procedure DGER(alpha: Double; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure DSBMV(Uplo: Integer; K: Integer; alpha: Double; A: JAllocation; X: JAllocation; incX: Integer; beta: Double; Y: JAllocation; incY: Integer); cdecl;
    procedure DSPMV(Uplo: Integer; alpha: Double; Ap: JAllocation; X: JAllocation; incX: Integer; beta: Double; Y: JAllocation; incY: Integer); cdecl;
    procedure DSPR(Uplo: Integer; alpha: Double; X: JAllocation; incX: Integer; Ap: JAllocation); cdecl;
    procedure DSPR2(Uplo: Integer; alpha: Double; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; Ap: JAllocation); cdecl;
    procedure DSYMM(Side: Integer; Uplo: Integer; alpha: Double; A: JAllocation; B: JAllocation; beta: Double; C: JAllocation); cdecl;
    procedure DSYMV(Uplo: Integer; alpha: Double; A: JAllocation; X: JAllocation; incX: Integer; beta: Double; Y: JAllocation; incY: Integer); cdecl;
    procedure DSYR(Uplo: Integer; alpha: Double; X: JAllocation; incX: Integer; A: JAllocation); cdecl;
    procedure DSYR2(Uplo: Integer; alpha: Double; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure DSYR2K(Uplo: Integer; Trans: Integer; alpha: Double; A: JAllocation; B: JAllocation; beta: Double; C: JAllocation); cdecl;
    procedure DSYRK(Uplo: Integer; Trans: Integer; alpha: Double; A: JAllocation; beta: Double; C: JAllocation); cdecl;
    procedure DTBMV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure DTBSV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure DTPMV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure DTPSV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure DTRMM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: Double; A: JAllocation; B: JAllocation); cdecl;
    procedure DTRMV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure DTRSM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: Double; A: JAllocation; B: JAllocation); cdecl;
    procedure DTRSV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure SGBMV(TransA: Integer; KL: Integer; KU: Integer; alpha: Single; A: JAllocation; X: JAllocation; incX: Integer; beta: Single; Y: JAllocation; incY: Integer); cdecl;
    procedure SGEMM(TransA: Integer; TransB: Integer; alpha: Single; A: JAllocation; B: JAllocation; beta: Single; C: JAllocation); cdecl;
    procedure SGEMV(TransA: Integer; alpha: Single; A: JAllocation; X: JAllocation; incX: Integer; beta: Single; Y: JAllocation; incY: Integer); cdecl;
    procedure SGER(alpha: Single; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure SSBMV(Uplo: Integer; K: Integer; alpha: Single; A: JAllocation; X: JAllocation; incX: Integer; beta: Single; Y: JAllocation; incY: Integer); cdecl;
    procedure SSPMV(Uplo: Integer; alpha: Single; Ap: JAllocation; X: JAllocation; incX: Integer; beta: Single; Y: JAllocation; incY: Integer); cdecl;
    procedure SSPR(Uplo: Integer; alpha: Single; X: JAllocation; incX: Integer; Ap: JAllocation); cdecl;
    procedure SSPR2(Uplo: Integer; alpha: Single; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; Ap: JAllocation); cdecl;
    procedure SSYMM(Side: Integer; Uplo: Integer; alpha: Single; A: JAllocation; B: JAllocation; beta: Single; C: JAllocation); cdecl;
    procedure SSYMV(Uplo: Integer; alpha: Single; A: JAllocation; X: JAllocation; incX: Integer; beta: Single; Y: JAllocation; incY: Integer); cdecl;
    procedure SSYR(Uplo: Integer; alpha: Single; X: JAllocation; incX: Integer; A: JAllocation); cdecl;
    procedure SSYR2(Uplo: Integer; alpha: Single; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure SSYR2K(Uplo: Integer; Trans: Integer; alpha: Single; A: JAllocation; B: JAllocation; beta: Single; C: JAllocation); cdecl;
    procedure SSYRK(Uplo: Integer; Trans: Integer; alpha: Single; A: JAllocation; beta: Single; C: JAllocation); cdecl;
    procedure STBMV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure STBSV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure STPMV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure STPSV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure STRMM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: Single; A: JAllocation; B: JAllocation); cdecl;
    procedure STRMV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure STRSM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: Single; A: JAllocation; B: JAllocation); cdecl;
    procedure STRSV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure ZGBMV(TransA: Integer; KL: Integer; KU: Integer; alpha: JDouble2; A: JAllocation; X: JAllocation; incX: Integer; beta: JDouble2; Y: JAllocation; incY: Integer); cdecl;
    procedure ZGEMM(TransA: Integer; TransB: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation; beta: JDouble2; C: JAllocation); cdecl;
    procedure ZGEMV(TransA: Integer; alpha: JDouble2; A: JAllocation; X: JAllocation; incX: Integer; beta: JDouble2; Y: JAllocation; incY: Integer); cdecl;
    procedure ZGERC(alpha: JDouble2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure ZGERU(alpha: JDouble2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure ZHBMV(Uplo: Integer; K: Integer; alpha: JDouble2; A: JAllocation; X: JAllocation; incX: Integer; beta: JDouble2; Y: JAllocation; incY: Integer); cdecl;
    procedure ZHEMM(Side: Integer; Uplo: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation; beta: JDouble2; C: JAllocation); cdecl;
    procedure ZHEMV(Uplo: Integer; alpha: JDouble2; A: JAllocation; X: JAllocation; incX: Integer; beta: JDouble2; Y: JAllocation; incY: Integer); cdecl;
    procedure ZHER(Uplo: Integer; alpha: Double; X: JAllocation; incX: Integer; A: JAllocation); cdecl;
    procedure ZHER2(Uplo: Integer; alpha: JDouble2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; A: JAllocation); cdecl;
    procedure ZHER2K(Uplo: Integer; Trans: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation; beta: Double; C: JAllocation); cdecl;
    procedure ZHERK(Uplo: Integer; Trans: Integer; alpha: Double; A: JAllocation; beta: Double; C: JAllocation); cdecl;
    procedure ZHPMV(Uplo: Integer; alpha: JDouble2; Ap: JAllocation; X: JAllocation; incX: Integer; beta: JDouble2; Y: JAllocation; incY: Integer); cdecl;
    procedure ZHPR(Uplo: Integer; alpha: Double; X: JAllocation; incX: Integer; Ap: JAllocation); cdecl;
    procedure ZHPR2(Uplo: Integer; alpha: JDouble2; X: JAllocation; incX: Integer; Y: JAllocation; incY: Integer; Ap: JAllocation); cdecl;
    procedure ZSYMM(Side: Integer; Uplo: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation; beta: JDouble2; C: JAllocation); cdecl;
    procedure ZSYR2K(Uplo: Integer; Trans: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation; beta: JDouble2; C: JAllocation); cdecl;
    procedure ZSYRK(Uplo: Integer; Trans: Integer; alpha: JDouble2; A: JAllocation; beta: JDouble2; C: JAllocation); cdecl;
    procedure ZTBMV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure ZTBSV(Uplo: Integer; TransA: Integer; Diag: Integer; K: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure ZTPMV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure ZTPSV(Uplo: Integer; TransA: Integer; Diag: Integer; Ap: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure ZTRMM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation); cdecl;
    procedure ZTRMV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
    procedure ZTRSM(Side: Integer; Uplo: Integer; TransA: Integer; Diag: Integer; alpha: JDouble2; A: JAllocation; B: JAllocation); cdecl;
    procedure ZTRSV(Uplo: Integer; TransA: Integer; Diag: Integer; A: JAllocation; X: JAllocation; incX: Integer); cdecl;
  end;
  TJScriptIntrinsicBLAS = class(TJavaGenericImport<JScriptIntrinsicBLASClass, JScriptIntrinsicBLAS>) end;

  JScriptIntrinsicBlendClass = interface(JScriptIntrinsicClass)
    ['{7CA741B4-F235-4661-8EBD-39A36ABF073D}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicBlend; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicBlend')]
  JScriptIntrinsicBlend = interface(JScriptIntrinsic)
    ['{8F609357-C61C-4D34-8B24-08FD277E70F4}']
    procedure forEachAdd(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachAdd(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachClear(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachClear(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachDst(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachDst(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachDstAtop(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachDstAtop(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachDstIn(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachDstIn(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachDstOut(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachDstOut(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachDstOver(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachDstOver(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachMultiply(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachMultiply(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachSrc(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachSrc(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachSrcAtop(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachSrcAtop(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachSrcIn(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachSrcIn(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachSrcOut(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachSrcOut(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachSrcOver(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachSrcOver(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachSubtract(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachSubtract(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEachXor(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEachXor(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getKernelIDAdd: JScript_KernelID; cdecl;
    function getKernelIDClear: JScript_KernelID; cdecl;
    function getKernelIDDst: JScript_KernelID; cdecl;
    function getKernelIDDstAtop: JScript_KernelID; cdecl;
    function getKernelIDDstIn: JScript_KernelID; cdecl;
    function getKernelIDDstOut: JScript_KernelID; cdecl;
    function getKernelIDDstOver: JScript_KernelID; cdecl;
    function getKernelIDMultiply: JScript_KernelID; cdecl;
    function getKernelIDSrc: JScript_KernelID; cdecl;
    function getKernelIDSrcAtop: JScript_KernelID; cdecl;
    function getKernelIDSrcIn: JScript_KernelID; cdecl;
    function getKernelIDSrcOut: JScript_KernelID; cdecl;
    function getKernelIDSrcOver: JScript_KernelID; cdecl;
    function getKernelIDSubtract: JScript_KernelID; cdecl;
    function getKernelIDXor: JScript_KernelID; cdecl;
  end;
  TJScriptIntrinsicBlend = class(TJavaGenericImport<JScriptIntrinsicBlendClass, JScriptIntrinsicBlend>) end;

  JScriptIntrinsicBlurClass = interface(JScriptIntrinsicClass)
    ['{38491E8D-407C-4BA3-86D4-FE5A6419C4CB}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicBlur; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicBlur')]
  JScriptIntrinsicBlur = interface(JScriptIntrinsic)
    ['{63EE931E-7DE6-468A-B5A8-6ACC301BCAF7}']
    procedure forEach(aout: JAllocation); cdecl; overload;
    procedure forEach(aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getFieldID_Input: JScript_FieldID; cdecl;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setInput(ain: JAllocation); cdecl;
    procedure setRadius(radius: Single); cdecl;
  end;
  TJScriptIntrinsicBlur = class(TJavaGenericImport<JScriptIntrinsicBlurClass, JScriptIntrinsicBlur>) end;

  JScriptIntrinsicColorMatrixClass = interface(JScriptIntrinsicClass)
    ['{F662D945-89C0-4E5E-9653-3C0A934F298C}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicColorMatrix; cdecl; overload;//Deprecated
    {class} function create(rs: JRenderScript): JScriptIntrinsicColorMatrix; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicColorMatrix')]
  JScriptIntrinsicColorMatrix = interface(JScriptIntrinsic)
    ['{4A785F7C-1096-4C91-BE13-9CC5CE6F560F}']
    procedure forEach(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEach(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setAdd(f: JFloat4); cdecl; overload;
    procedure setAdd(r: Single; g: Single; b: Single; a: Single); cdecl; overload;
    procedure setColorMatrix(m: JMatrix4f); cdecl; overload;
    procedure setColorMatrix(m: JMatrix3f); cdecl; overload;
    procedure setGreyscale; cdecl;
    procedure setRGBtoYUV; cdecl;
    procedure setYUVtoRGB; cdecl;
  end;
  TJScriptIntrinsicColorMatrix = class(TJavaGenericImport<JScriptIntrinsicColorMatrixClass, JScriptIntrinsicColorMatrix>) end;

  JScriptIntrinsicConvolve3x3Class = interface(JScriptIntrinsicClass)
    ['{97FF9326-FF80-4D24-B546-3A053102D03F}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicConvolve3x3; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicConvolve3x3')]
  JScriptIntrinsicConvolve3x3 = interface(JScriptIntrinsic)
    ['{223BE7FC-0B99-42BD-A984-B96A3746E9B6}']
    procedure forEach(aout: JAllocation); cdecl; overload;
    procedure forEach(aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getFieldID_Input: JScript_FieldID; cdecl;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setCoefficients(v: TJavaArray<Single>); cdecl;
    procedure setInput(ain: JAllocation); cdecl;
  end;
  TJScriptIntrinsicConvolve3x3 = class(TJavaGenericImport<JScriptIntrinsicConvolve3x3Class, JScriptIntrinsicConvolve3x3>) end;

  JScriptIntrinsicConvolve5x5Class = interface(JScriptIntrinsicClass)
    ['{5A6A9A23-2FFC-430E-BA36-80AD7C05D067}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicConvolve5x5; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicConvolve5x5')]
  JScriptIntrinsicConvolve5x5 = interface(JScriptIntrinsic)
    ['{31407942-C83C-4341-BA48-73D0AD667C3C}']
    procedure forEach(aout: JAllocation); cdecl; overload;
    procedure forEach(aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getFieldID_Input: JScript_FieldID; cdecl;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setCoefficients(v: TJavaArray<Single>); cdecl;
    procedure setInput(ain: JAllocation); cdecl;
  end;
  TJScriptIntrinsicConvolve5x5 = class(TJavaGenericImport<JScriptIntrinsicConvolve5x5Class, JScriptIntrinsicConvolve5x5>) end;

  JScriptIntrinsicHistogramClass = interface(JScriptIntrinsicClass)
    ['{870E7DC1-4E6C-426D-BF19-45B400408680}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicHistogram; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicHistogram')]
  JScriptIntrinsicHistogram = interface(JScriptIntrinsic)
    ['{4D59E7CE-58C5-4CD6-ADA6-1F183D9C0D9B}']
    procedure forEach(ain: JAllocation); cdecl; overload;
    procedure forEach(ain: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    procedure forEach_Dot(ain: JAllocation); cdecl; overload;
    procedure forEach_Dot(ain: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getFieldID_Input: JScript_FieldID; cdecl;
    function getKernelID_Separate: JScript_KernelID; cdecl;
    procedure setDotCoefficients(r: Single; g: Single; b: Single; a: Single); cdecl;
    procedure setOutput(aout: JAllocation); cdecl;
  end;
  TJScriptIntrinsicHistogram = class(TJavaGenericImport<JScriptIntrinsicHistogramClass, JScriptIntrinsicHistogram>) end;

  JScriptIntrinsicLUTClass = interface(JScriptIntrinsicClass)
    ['{AF117583-280C-467D-A777-FAD4C723E932}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicLUT; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicLUT')]
  JScriptIntrinsicLUT = interface(JScriptIntrinsic)
    ['{AB6CA036-20C9-4AC9-9D66-0E4735CBDE6E}']
    procedure destroy; cdecl;
    procedure forEach(ain: JAllocation; aout: JAllocation); cdecl; overload;
    procedure forEach(ain: JAllocation; aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setAlpha(index: Integer; value: Integer); cdecl;
    procedure setBlue(index: Integer; value: Integer); cdecl;
    procedure setGreen(index: Integer; value: Integer); cdecl;
    procedure setRed(index: Integer; value: Integer); cdecl;
  end;
  TJScriptIntrinsicLUT = class(TJavaGenericImport<JScriptIntrinsicLUTClass, JScriptIntrinsicLUT>) end;

  JScriptIntrinsicResizeClass = interface(JScriptIntrinsicClass)
    ['{09C122BE-D6AA-45B4-BE38-EE4DA5529B12}']
    {class} function create(rs: JRenderScript): JScriptIntrinsicResize; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicResize')]
  JScriptIntrinsicResize = interface(JScriptIntrinsic)
    ['{23934D18-3078-4D91-B96C-9DDFE28B9756}']
    procedure forEach_bicubic(aout: JAllocation); cdecl; overload;
    procedure forEach_bicubic(aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    function getFieldID_Input: JScript_FieldID; cdecl;
    function getKernelID_bicubic: JScript_KernelID; cdecl;
    procedure setInput(ain: JAllocation); cdecl;
  end;
  TJScriptIntrinsicResize = class(TJavaGenericImport<JScriptIntrinsicResizeClass, JScriptIntrinsicResize>) end;

  JScriptIntrinsicYuvToRGBClass = interface(JScriptIntrinsicClass)
    ['{A4F423A2-7CE4-4DB5-8E2D-CFE990CAB744}']
    {class} function create(rs: JRenderScript; e: Jrenderscript_Element): JScriptIntrinsicYuvToRGB; cdecl;
  end;

  [JavaSignature('android/renderscript/ScriptIntrinsicYuvToRGB')]
  JScriptIntrinsicYuvToRGB = interface(JScriptIntrinsic)
    ['{9883EE76-C5FF-4D2C-9A5D-AF574136F5E3}']
    procedure forEach(aout: JAllocation); cdecl;
    function getFieldID_Input: JScript_FieldID; cdecl;
    function getKernelID: JScript_KernelID; cdecl;
    procedure setInput(ain: JAllocation); cdecl;
  end;
  TJScriptIntrinsicYuvToRGB = class(TJavaGenericImport<JScriptIntrinsicYuvToRGBClass, JScriptIntrinsicYuvToRGB>) end;

  JShort2Class = interface(JObjectClass)
    ['{671EDDB5-33BC-4E9A-9E8C-03E19430D456}']
    {class} function init: JShort2; cdecl; overload;
    {class} function init(x: SmallInt; y: SmallInt): JShort2; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Short2')]
  JShort2 = interface(JObject)
    ['{3B138DD0-8A11-4342-A376-EB24993F932D}']
    function _Getx: SmallInt; cdecl;
    procedure _Setx(Value: SmallInt); cdecl;
    function _Gety: SmallInt; cdecl;
    procedure _Sety(Value: SmallInt); cdecl;
    property x: SmallInt read _Getx write _Setx;
    property y: SmallInt read _Gety write _Sety;
  end;
  TJShort2 = class(TJavaGenericImport<JShort2Class, JShort2>) end;

  JShort3Class = interface(JObjectClass)
    ['{0D00C53F-10D9-4531-ACEC-975204178548}']
    {class} function init: JShort3; cdecl; overload;
    {class} function init(x: SmallInt; y: SmallInt; z: SmallInt): JShort3; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Short3')]
  JShort3 = interface(JObject)
    ['{2F5E58BB-0DA1-42D1-8854-5D5540789D54}']
    function _Getx: SmallInt; cdecl;
    procedure _Setx(Value: SmallInt); cdecl;
    function _Gety: SmallInt; cdecl;
    procedure _Sety(Value: SmallInt); cdecl;
    function _Getz: SmallInt; cdecl;
    procedure _Setz(Value: SmallInt); cdecl;
    property x: SmallInt read _Getx write _Setx;
    property y: SmallInt read _Gety write _Sety;
    property z: SmallInt read _Getz write _Setz;
  end;
  TJShort3 = class(TJavaGenericImport<JShort3Class, JShort3>) end;

  JShort4Class = interface(JObjectClass)
    ['{80A95F44-C8C5-4D74-8619-DD1F8E877891}']
    {class} function init: JShort4; cdecl; overload;
    {class} function init(x: SmallInt; y: SmallInt; z: SmallInt; w: SmallInt): JShort4; cdecl; overload;
  end;

  [JavaSignature('android/renderscript/Short4')]
  JShort4 = interface(JObject)
    ['{C33683EC-B50F-412D-9540-BE5D1188B931}']
    function _Getw: SmallInt; cdecl;
    procedure _Setw(Value: SmallInt); cdecl;
    function _Getx: SmallInt; cdecl;
    procedure _Setx(Value: SmallInt); cdecl;
    function _Gety: SmallInt; cdecl;
    procedure _Sety(Value: SmallInt); cdecl;
    function _Getz: SmallInt; cdecl;
    procedure _Setz(Value: SmallInt); cdecl;
    property w: SmallInt read _Getw write _Setw;
    property x: SmallInt read _Getx write _Setx;
    property y: SmallInt read _Gety write _Sety;
    property z: SmallInt read _Getz write _Setz;
  end;
  TJShort4 = class(TJavaGenericImport<JShort4Class, JShort4>) end;

  JTypeClass = interface(JBaseObjClass)
    ['{EA772550-90A2-4831-A62F-063EC75F6402}']
    {class} function createX(rs: JRenderScript; e: Jrenderscript_Element; dimX: Integer): JType; cdecl;
    {class} function createXY(rs: JRenderScript; e: Jrenderscript_Element; dimX: Integer; dimY: Integer): JType; cdecl;
    {class} function createXYZ(rs: JRenderScript; e: Jrenderscript_Element; dimX: Integer; dimY: Integer; dimZ: Integer): JType; cdecl;
  end;

  [JavaSignature('android/renderscript/Type')]
  JType = interface(JBaseObj)
    ['{1EC7E148-3497-4D42-AF8C-67FF257EECFD}']
    function getCount: Integer; cdecl;
    function getElement: Jrenderscript_Element; cdecl;
    function getX: Integer; cdecl;
    function getY: Integer; cdecl;
    function getYuv: Integer; cdecl;
    function getZ: Integer; cdecl;
    function hasFaces: Boolean; cdecl;
    function hasMipmaps: Boolean; cdecl;
  end;
  TJType = class(TJavaGenericImport<JTypeClass, JType>) end;

  JType_BuilderClass = interface(JObjectClass)
    ['{76A458CF-E09E-427D-8FFA-F0F601C7FEA7}']
    {class} function init(rs: JRenderScript; e: Jrenderscript_Element): JType_Builder; cdecl;
  end;

  [JavaSignature('android/renderscript/Type$Builder')]
  JType_Builder = interface(JObject)
    ['{18D670D4-F7E5-4808-948F-A446D3ABE4E3}']
    function create: JType; cdecl;
    function setFaces(value: Boolean): JType_Builder; cdecl;
    function setMipmaps(value: Boolean): JType_Builder; cdecl;
    function setX(value: Integer): JType_Builder; cdecl;
    function setY(value: Integer): JType_Builder; cdecl;
    function setYuvFormat(yuvFormat: Integer): JType_Builder; cdecl;
    function setZ(value: Integer): JType_Builder; cdecl;
  end;
  TJType_Builder = class(TJavaGenericImport<JType_BuilderClass, JType_Builder>) end;

  JType_CubemapFaceClass = interface(JEnumClass)
    ['{AB16FAD5-6539-42A1-B2CF-2E59843FFA3F}']
    {class} function _GetNEGATIVE_X: JType_CubemapFace; cdecl;
    {class} function _GetNEGATIVE_Y: JType_CubemapFace; cdecl;
    {class} function _GetNEGATIVE_Z: JType_CubemapFace; cdecl;
    {class} function _GetPOSITIVE_X: JType_CubemapFace; cdecl;
    {class} function _GetPOSITIVE_Y: JType_CubemapFace; cdecl;
    {class} function _GetPOSITIVE_Z: JType_CubemapFace; cdecl;
    {class} function _GetPOSITVE_X: JType_CubemapFace; cdecl;
    {class} function _GetPOSITVE_Y: JType_CubemapFace; cdecl;
    {class} function _GetPOSITVE_Z: JType_CubemapFace; cdecl;
    {class} function valueOf(name: JString): JType_CubemapFace; cdecl;
    {class} function values: TJavaObjectArray<JType_CubemapFace>; cdecl;
    {class} property NEGATIVE_X: JType_CubemapFace read _GetNEGATIVE_X;
    {class} property NEGATIVE_Y: JType_CubemapFace read _GetNEGATIVE_Y;
    {class} property NEGATIVE_Z: JType_CubemapFace read _GetNEGATIVE_Z;
    {class} property POSITIVE_X: JType_CubemapFace read _GetPOSITIVE_X;
    {class} property POSITIVE_Y: JType_CubemapFace read _GetPOSITIVE_Y;
    {class} property POSITIVE_Z: JType_CubemapFace read _GetPOSITIVE_Z;
    {class} property POSITVE_X: JType_CubemapFace read _GetPOSITVE_X;
    {class} property POSITVE_Y: JType_CubemapFace read _GetPOSITVE_Y;
    {class} property POSITVE_Z: JType_CubemapFace read _GetPOSITVE_Z;
  end;

  [JavaSignature('android/renderscript/Type$CubemapFace')]
  JType_CubemapFace = interface(JEnum)
    ['{7E789A7C-6CDD-4989-8631-B587F9DB3A20}']
  end;
  TJType_CubemapFace = class(TJavaGenericImport<JType_CubemapFaceClass, JType_CubemapFace>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JBaseObj', TypeInfo(Androidapi.JNI.RenderScript.JBaseObj));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JAllocation', TypeInfo(Androidapi.JNI.RenderScript.JAllocation));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JAllocation_MipmapControl', TypeInfo(Androidapi.JNI.RenderScript.JAllocation_MipmapControl));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JAllocation_OnBufferAvailableListener', TypeInfo(Androidapi.JNI.RenderScript.JAllocation_OnBufferAvailableListener));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JAllocationAdapter', TypeInfo(Androidapi.JNI.RenderScript.JAllocationAdapter));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JByte2', TypeInfo(Androidapi.JNI.RenderScript.JByte2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JByte3', TypeInfo(Androidapi.JNI.RenderScript.JByte3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JByte4', TypeInfo(Androidapi.JNI.RenderScript.JByte4));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JDouble2', TypeInfo(Androidapi.JNI.RenderScript.JDouble2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JDouble3', TypeInfo(Androidapi.JNI.RenderScript.JDouble3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JDouble4', TypeInfo(Androidapi.JNI.RenderScript.JDouble4));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.Jrenderscript_Element', TypeInfo(Androidapi.JNI.RenderScript.Jrenderscript_Element));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JElement_Builder', TypeInfo(Androidapi.JNI.RenderScript.JElement_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JElement_DataKind', TypeInfo(Androidapi.JNI.RenderScript.JElement_DataKind));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JElement_DataType', TypeInfo(Androidapi.JNI.RenderScript.JElement_DataType));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JFieldPacker', TypeInfo(Androidapi.JNI.RenderScript.JFieldPacker));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JFloat2', TypeInfo(Androidapi.JNI.RenderScript.JFloat2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JFloat3', TypeInfo(Androidapi.JNI.RenderScript.JFloat3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JFloat4', TypeInfo(Androidapi.JNI.RenderScript.JFloat4));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JInt2', TypeInfo(Androidapi.JNI.RenderScript.JInt2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JInt3', TypeInfo(Androidapi.JNI.RenderScript.JInt3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JInt4', TypeInfo(Androidapi.JNI.RenderScript.JInt4));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JLong2', TypeInfo(Androidapi.JNI.RenderScript.JLong2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JLong3', TypeInfo(Androidapi.JNI.RenderScript.JLong3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JLong4', TypeInfo(Androidapi.JNI.RenderScript.JLong4));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JMatrix2f', TypeInfo(Androidapi.JNI.RenderScript.JMatrix2f));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JMatrix3f', TypeInfo(Androidapi.JNI.RenderScript.JMatrix3f));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JMatrix4f', TypeInfo(Androidapi.JNI.RenderScript.JMatrix4f));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRSRuntimeException', TypeInfo(Androidapi.JNI.RenderScript.JRSRuntimeException));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRSDriverException', TypeInfo(Androidapi.JNI.RenderScript.JRSDriverException));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRSIllegalArgumentException', TypeInfo(Androidapi.JNI.RenderScript.JRSIllegalArgumentException));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRSInvalidStateException', TypeInfo(Androidapi.JNI.RenderScript.JRSInvalidStateException));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRenderScript', TypeInfo(Androidapi.JNI.RenderScript.JRenderScript));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRenderScript_ContextType', TypeInfo(Androidapi.JNI.RenderScript.JRenderScript_ContextType));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRenderScript_Priority', TypeInfo(Androidapi.JNI.RenderScript.JRenderScript_Priority));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRenderScript_RSErrorHandler', TypeInfo(Androidapi.JNI.RenderScript.JRenderScript_RSErrorHandler));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JRenderScript_RSMessageHandler', TypeInfo(Androidapi.JNI.RenderScript.JRenderScript_RSMessageHandler));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JSampler', TypeInfo(Androidapi.JNI.RenderScript.JSampler));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JSampler_Builder', TypeInfo(Androidapi.JNI.RenderScript.JSampler_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JSampler_Value', TypeInfo(Androidapi.JNI.RenderScript.JSampler_Value));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript', TypeInfo(Androidapi.JNI.RenderScript.JScript));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript_Builder', TypeInfo(Androidapi.JNI.RenderScript.JScript_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript_FieldBase', TypeInfo(Androidapi.JNI.RenderScript.JScript_FieldBase));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript_FieldID', TypeInfo(Androidapi.JNI.RenderScript.JScript_FieldID));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript_InvokeID', TypeInfo(Androidapi.JNI.RenderScript.JScript_InvokeID));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript_KernelID', TypeInfo(Androidapi.JNI.RenderScript.JScript_KernelID));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScript_LaunchOptions', TypeInfo(Androidapi.JNI.RenderScript.JScript_LaunchOptions));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptC', TypeInfo(Androidapi.JNI.RenderScript.JScriptC));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup_Binding', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup_Binding));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup_Builder', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup_Builder2', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup_Builder2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup_Closure', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup_Closure));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup_Future', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup_Future));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptGroup_Input', TypeInfo(Androidapi.JNI.RenderScript.JScriptGroup_Input));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsic', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsic));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsic3DLUT', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsic3DLUT));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicBLAS', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicBLAS));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicBlend', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicBlend));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicBlur', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicBlur));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicColorMatrix', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicColorMatrix));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicConvolve3x3', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicConvolve3x3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicConvolve5x5', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicConvolve5x5));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicHistogram', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicHistogram));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicLUT', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicLUT));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicResize', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicResize));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JScriptIntrinsicYuvToRGB', TypeInfo(Androidapi.JNI.RenderScript.JScriptIntrinsicYuvToRGB));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JShort2', TypeInfo(Androidapi.JNI.RenderScript.JShort2));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JShort3', TypeInfo(Androidapi.JNI.RenderScript.JShort3));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JShort4', TypeInfo(Androidapi.JNI.RenderScript.JShort4));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JType', TypeInfo(Androidapi.JNI.RenderScript.JType));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JType_Builder', TypeInfo(Androidapi.JNI.RenderScript.JType_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.RenderScript.JType_CubemapFace', TypeInfo(Androidapi.JNI.RenderScript.JType_CubemapFace));
end;

initialization
  RegisterTypes;
end.


