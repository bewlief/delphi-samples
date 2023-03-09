{*******************************************************}
{                                                       }
{               Delphi Runtime Library                  }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Rtti;

{$R-,T-,X+,H+,B-}

interface

{$WARN DUPLICATE_CTOR_DTOR OFF}
{$WARN IMMUTABLE_STRINGS ON}

uses
  System.Variants, System.TypInfo, System.SysUtils, System.Generics.Collections;

// Dcciosarm doesn't use VFP registers
{$IF defined(IOS) and defined(CPUARM32)}
  // Dcciosarm doesn't use VFP registers if XE6 or before
  {$IF (CompilerVersion < 28.0)}
    {$DEFINE ARM_NO_VFP_USE}
  {$ENDIF}
{$ENDIF}

{$IF Defined(WIN32) or Defined(WIN64)}
  {$UNDEF USE_MONITOR_FOR_GLOBALCONTEXT}
{$ELSE}
  {$DEFINE USE_MONITOR_FOR_GLOBALCONTEXT}
{$ENDIF}

{$HPPEMIT LEGACYHPP}
(*$HPPEMIT END '#include <SystemRtti.h>'*)

type
  EInsufficientRtti = class(Exception);
  EInvocationError = class(Exception);
  // Types not declared in interface section of unit have no qualified name
  // and can't be looked up by name.
  ENonPublicType = class(Exception);

  IValueData = interface
    function GetDataSize: Integer;
    // Will extract a *copy* of the internal data.
    procedure ExtractRawData(ABuffer: Pointer);
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    function GetReferenceToRawData: Pointer;
  end;

  TValueData = record
    FTypeInfo: PTypeInfo;
    // FValueData vs old FHeapData:
    // FHeapData doubled as storage for interfaces. However, that was ambiguous
    // in the case of nil interface values: FTypeInfo couldn't be trusted
    // because it looked like the structure was uninitialized. Then, DataSize
    // would be 0.
    // FValueData is different: interfaces are always stored like strings etc.,
    // as a reference stored in a blob on the heap.
    FValueData: IValueData;
    case Integer of
      0: (FAsUByte: Byte);
      1: (FAsUWord: Word);
      2: (FAsULong: Cardinal);
      3: (FAsObject: Pointer);
      4: (FAsClass: TClass);
      5: (FAsSByte: ShortInt);
      6: (FAsSWord: SmallInt);
      7: (FAsSLong: Integer);
      8: (FAsSingle: Single);
      9: (FAsDouble: Double);
      10: (FAsExtended: Extended);
      11: (FAsComp: Comp);
      12: (FAsCurr: Currency);
      13: (FAsUInt64: UInt64);
      14: (FAsSInt64: Int64);
      15: (FAsMethod: TMethod);
      16: (FAsPointer: Pointer);
  end;

  [HPPGEN(HPPGenAttribute.mkNonPackage)]
  TValue = record
  private class var
    FEmpty: TValue;
  private
    class constructor Create;
    function GetIsEmpty: Boolean;
    function GetTypeInfo: PTypeInfo; inline;
    function GetTypeKind: TTypeKind;
    function GetTypeDataProp: PTypeData;
    function GetDataSize: Integer;
    class procedure InitData(var AValue: TValue); static; inline;
    class function Create(ATypeInfo: PTypeInfo): TValue; static;
    class function GetEmpty: TValue; static;
    function TryAsTypeInternal(var AResult; ATypeInfo: PTypeInfo;
      const EmptyAsAnyType: Boolean = True): Boolean;
    // ATypeInfo is Pointer to enable inlining of callers without including System.TypInfo
    procedure AsTypeInternal(var AResult; ATypeInfo: Pointer {PTypeInfo});
  public
    // Easy in
    class operator Implicit(const Value: string): TValue;
    class operator Implicit(Value: Integer): TValue;
    class operator Implicit(Value: Cardinal): TValue;
    class operator Implicit(Value: Single): TValue;
    class operator Implicit(Value: Double): TValue;
    class operator Implicit(Value: Extended): TValue;
    class operator Implicit(Value: Currency): TValue;
    class operator Implicit(Value: Int64): TValue;
    class operator Implicit(Value: UInt64): TValue;
    class operator Implicit(Value: TObject): TValue;
    class operator Implicit(Value: TClass): TValue;
    class operator Implicit(Value: Boolean): TValue;
    class operator Implicit(Value: TDateTime): TValue;
    [HPPGen('// NOP')]
    class operator Implicit(Value: TDate): TValue;
    [HPPGen('// NOP')]
    class operator Implicit(Value: TTime): TValue;
    class operator Implicit(const VarRec: TVarRec): TValue; inline;
    class function FromVariant(const Value: Variant): TValue; static;
    class function From<T>(const Value: T): TValue; inline; static;
    class function FromOrdinal(ATypeInfo: PTypeInfo; AValue: Int64): TValue; static;
    class function FromArray(ArrayTypeInfo: PTypeInfo; const Values: array of TValue): TValue; static;
    class function FromVarRec(const VarRec: TVarRec): TValue; static;

    // Easy out
    property Kind: TTypeKind read GetTypeKind;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeData: PTypeData read GetTypeDataProp;
    // Empty converts to all-zero for the corresponding type, but IsEmpty
    // will only return True for reference types (where 'nil' is logical)
    // and method pointers (where TMethod.Code equivalent is nil).
    // A typeless empty (like 'nil' literal) will also have 0 for DataSize.
    property IsEmpty: Boolean read GetIsEmpty;

    function IsObject: Boolean;
    function IsObjectInstance: Boolean;
    function AsObject: TObject;
    function IsInstanceOf(AClass: TClass): Boolean;

    function IsClass: Boolean; inline;
    function AsClass: TClass; inline;

    function IsOrdinal: Boolean;
    function AsOrdinal: Int64;
    function TryAsOrdinal(out AResult: Int64): Boolean;

    // TValue -> concrete type
    // IsType returns true if AsType or Cast would succeed
    // AsType / Cast are only for what would normally be implicit conversions in Delphi.
    function IsType<T>(const EmptyAsAnyType: Boolean = True): Boolean; overload; inline;
    function IsType(ATypeInfo: PTypeInfo; const EmptyAsAnyType: Boolean = True): Boolean; overload;
    function AsType<T>(const EmptyAsAnyType: Boolean = True): T;
    function TryAsType<T>(out AResult: T; const EmptyAsAnyType: Boolean = True): Boolean; inline;

    // TValue -> TValue conversions
    function Cast<T>(const EmptyAsAnyType: Boolean = True): TValue; overload;
    function Cast(ATypeInfo: PTypeInfo; const EmptyAsAnyType: Boolean = True): TValue; overload;
    function TryCast(ATypeInfo: PTypeInfo; out AResult: TValue; const EmptyAsAnyType: Boolean = True): Boolean;

    function AsInteger: Integer;
    function AsBoolean: Boolean;
    function AsExtended: Extended;
    function AsInt64: Int64;
    function AsUInt64: UInt64;
    function AsInterface: IInterface;
    function AsString: string;
    function AsVariant: Variant; inline;
    function AsCurrency: Currency; inline;
    function IsArray: Boolean;

    function CastToVarRec: TValue;
    /// <summary>
    ///    Returns a TVarRec instance suitable for passing to an "array of const" parameter such as with Format().
    /// </summary>
    function AsVarRec: TVarRec;

    function GetArrayLength: Integer;
    function GetArrayElement(Index: Integer): TValue;
    procedure SetArrayElement(Index: Integer; const AValue: TValue);

    // Low-level in
    class procedure Make(ABuffer: Pointer; ATypeInfo: PTypeInfo; var Result: TValue); overload; static;
    class procedure Make(AValue: NativeInt; ATypeInfo: PTypeInfo; var Result: TValue); overload; static;
    class procedure Make<T>(const Value: T; var Result: TValue); overload; static; inline;
    class procedure MakeWithoutCopy(ABuffer: Pointer; ATypeInfo: PTypeInfo; var Result: TValue; IsMoved: Boolean = False); overload; static;

    // Low-level out
    property DataSize: Integer read GetDataSize;
    procedure ExtractRawData(ABuffer: Pointer);
    // If internal data is something with lifetime management, this copies a
    // reference out *without* updating the reference count.
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    function GetReferenceToRawData: Pointer;
    function GetReferenceToRawArrayElement(Index: Integer): Pointer;

    class property Empty: TValue read GetEmpty;

    function ToString: string;
  private
    FTypeInfo: PTypeInfo;
    // FValueData vs old FHeapData:
    // FHeapData doubled as storage for interfaces. However, that was ambiguous
    // in the case of nil interface values: FTypeInfo couldn't be trusted
    // because it looked like the structure was uninitialized. Then, DataSize
    // would be 0.
    // FValueData is different: interfaces are always stored like strings etc.,
    // as a reference stored in a blob on the heap.
    FValueData: IValueData;
    case Integer of
      0: (FAsUByte: Byte);
      1: (FAsUWord: Word);
      2: (FAsULong: Cardinal);
      3: (FAsObject: Pointer);
      4: (FAsClass: TClass);
      5: (FAsSByte: ShortInt);
      6: (FAsSWord: SmallInt);
      7: (FAsSLong: Integer);
      8: (FAsSingle: Single);
      9: (FAsDouble: Double);
      10: (FAsExtended: Extended);
      11: (FAsComp: Comp);
      12: (FAsCurr: Currency);
      13: (FAsUInt64: UInt64);
      14: (FAsSInt64: Int64);
      15: (FAsMethod: TMethod);
      16: (FAsPointer: Pointer);
  end;
  PValue = ^TValue;

/// <summary>
///    Converts an "array of const" (aka. "array of TVarRec") into a dynamic array of TValue instances.
/// </summary>
function ArrayOfConstToTValueArray(const Params: array of const): TArray<TValue>;
/// <summary>
///    Converts a dynamic array of TValue instances to a dynamic array of TVarRec. This dynamic array is suitable for
///    passing to an "array of const" parameter. NOTE: The source array of TValue instances must remain valid while
///    the dynamic array of TVarRec instances is being used.
/// </summary>
function TValueArrayToArrayOfConst(const Params: array of TValue): TArray<TVarRec>;

type
  TRttiPackage = class;

  TRttiObject = class abstract
  private
    FHandle: Pointer;
    FRttiDataSize: Integer;
    [Weak] FPackage: TRttiPackage;
    [Weak] FParent: TRttiObject;
    FAttributeGetter: TFunc<TArray<TCustomAttribute>>;
  private
    // TRttiObject descendants should only be retrieved via an RTTI context.
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); virtual;
  public
    destructor Destroy; override;
    function GetAttribute(AAttrClass: TCustomAttributeClass): TCustomAttribute; overload;
    function GetAttribute<T: TCustomAttribute>: T; overload; inline;
    function HasAttribute(AAttrClass: TCustomAttributeClass): Boolean; overload; inline;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload; inline;
    // The starting address of the raw data in the executable image for this RTTI.
    property Handle: Pointer read FHandle;
    // Size of data pointed to by Handle.
    property RttiDataSize: Integer read FRttiDataSize;
    property Parent: TRttiObject read FParent;
    property Package: TRttiPackage read FPackage;
    function GetAttributes: TArray<TCustomAttribute>; virtual;
  end;

  TRttiNamedObject = class(TRttiObject)
  private
    function GetName: string; virtual; abstract;
  public
    function HasName(const AName: string): Boolean; virtual;
    property Name: string read GetName;
  end;

  TRttiClass = class of TRttiObject;
  TRttiMethod = class;
  TRttiInstanceType = class;
  TRttiInterfaceType = class;
  TRttiOrdinalType = class;
  TRttiRecordType = class;
  TRttiField = class;
  TRttiProperty = class;
  TRttiIndexedProperty = class;
  TRttiSetType = class;
  TRttiPointerType = class;
  TRttiFloatType = class;

  TRttiType = class(TRttiNamedObject)
  protected type
    TGetListFunc<T: TRttiNamedObject> = function (AType: TRttiType): TArray<T>;
  private
    function GetName: string; override;
    function GetTypeKind: TTypeKind;
    function GetTypeData: PTypeData;
    function GetIsManaged: Boolean;
    function GetAsInstance: TRttiInstanceType;
    function GetAsOrdinal: TRttiOrdinalType;
    function GetAsRecord: TRttiRecordType;
    function GetIsInstance: Boolean;
    function GetIsOrdinal: Boolean;
    function GetIsRecord: Boolean;
    function GetHandle: PTypeInfo; inline;
    function GetAsSet: TRttiSetType;
    function GetIsSet: Boolean;
    function GetIsHFA: Boolean; virtual;
    function GetHFAElementType: TRttiFloatType; virtual;
    function GetHFAElementCount: Integer; virtual;
    function GetTypeSize: Integer; virtual;
    function GetQualifiedName: string;
    function GetBaseType: TRttiType; virtual;
    function GetIsPublicType: Boolean;
    property TypeData: PTypeData read GetTypeData;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    function GetNamedObject<T: TRttiNamedObject>(const AName: string; const AGetListFunc: TGetListFunc<T>): T;
    function GetNamedObjects<T: TRttiNamedObject>(const AName: string; const AGetListFunc: TGetListFunc<T>): TArray<T>;
    function GetObjectList<T: TRttiNamedObject>(const AGetListFunc: TGetListFunc<T>): TArray<T>;
  public
    function ToString: string; override;
    property Handle: PTypeInfo read GetHandle;
    // QualifiedName is only available on types declared in interface section of units;
    // i.e. IsPublicType is true.
    property QualifiedName: string read GetQualifiedName;
    property IsPublicType: Boolean read GetIsPublicType;
    property TypeKind: TTypeKind read GetTypeKind;
    // The size of a location (variable) of this type.
    property TypeSize: Integer read GetTypeSize;
    property IsManaged: Boolean read GetIsManaged;

    function HasName(const AName: string): Boolean; override;

    // To make writing query code easier, hoist some methods from descendants
    // into this type. These return elements flattened across the type hierarchy
    // in order from most derived to least derived.
    function GetMethods: TArray<TRttiMethod>; overload; virtual;
    function GetFields: TArray<TRttiField>; virtual;
    function GetProperties: TArray<TRttiProperty>; virtual;
    function GetIndexedProperties: TArray<TRttiIndexedProperty>; virtual;

    function GetMethod(const AName: string): TRttiMethod; virtual;
    function GetMethods(const AName: string): TArray<TRttiMethod>; overload; virtual;
    function GetField(const AName: string): TRttiField; virtual;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    function GetIndexedProperty(const AName: string): TRttiIndexedProperty; virtual;

    function GetDeclaredMethods: TArray<TRttiMethod>; virtual;
    function GetDeclaredProperties: TArray<TRttiProperty>; virtual;
    function GetDeclaredFields: TArray<TRttiField>; virtual;
    function GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>; virtual;

    // The ancestor for types with ancestors.
    property BaseType: TRttiType read GetBaseType;

    property AsInstance: TRttiInstanceType read GetAsInstance;
    property IsInstance: Boolean read GetIsInstance;
    property AsOrdinal: TRttiOrdinalType read GetAsOrdinal;
    property IsOrdinal: Boolean read GetIsOrdinal;
    property AsRecord: TRttiRecordType read GetAsRecord;
    property IsRecord: Boolean read GetIsRecord;
    property IsSet: Boolean read GetIsSet;
    property AsSet: TRttiSetType read GetAsSet;

    /// <summary>Returns true if the reflected type is HFA.</summary>
    property IsHFA: Boolean read GetIsHFA;
    /// <summary>Return the element type of HFA, if IsHFA is true. Otherwise, Return NIL. </summary>
    property HFAElementType: TRttiFloatType read GetHFAElementType;
    /// <summary>Return the number of element type of HFA, if IsHFA is true. Otherwise, Return 0. </summary>
    property HFAElementCount: Integer read GetHFAElementCount;
  end;

  TRttiMember = class(TRttiNamedObject)
  private
    function GetParent: TRttiType;
    function GetVisibility: TMemberVisibility; virtual;
  public
    property Parent: TRttiType read GetParent;
    property Visibility: TMemberVisibility read GetVisibility;
  end;

  TRttiStructuredType = class abstract(TRttiType)
  end;

  TRttiField = class(TRttiMember)
  private
    FFieldType: TRttiType;
    FOffset: Integer;
    function GetFieldType: TRttiType; virtual;
    function GetOffset: Integer; virtual;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property FieldType: TRttiType read FFieldType;
    property Offset: Integer read FOffset;

    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    function ToString: string; override;
  end;

  TRttiManagedField = class(TRttiObject)
  private
    function GetFieldOffset: Integer;
    function GetFieldType: TRttiType;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property FieldType: TRttiType read GetFieldType;
    property FieldOffset: Integer read GetFieldOffset;
  end;

  TRttiRecordType = class(TRttiStructuredType)
  private
    FMethOfs: PByte;
{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    FHFAElementType: TRttiFloatType;
    FHFAElementCount: Integer;
    FHFAHasInsufficientTypeInformation: Boolean;
{$ENDIF CPUARM64 or LINUX64 or OSX64}
    function GetManagedFields: TArray<TRttiManagedField>;
{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    procedure GetHFAElement;
    function GetIsHFA: Boolean; override;
    function GetHFAElementType: TRttiFloatType; override;
    function GetHFAElementCount: Integer; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
{$ENDIF CPUARM64 or LINUX64 or OSX64}
  protected
    function GetTypeSize: Integer; override;
{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    property HFAHasInsufficientTypeInformation: Boolean read FHFAHasInsufficientTypeInformation;
{$ENDIF CPUARM64 or LINUX64 or OSX64}
  public
    function GetDeclaredFields: TArray<TRttiField>; override;
    function GetDeclaredMethods: TArray<TRttiMethod>; override;
    function GetAttributes: TArray<TCustomAttribute>; override;
    property ManagedFields: TArray<TRttiManagedField> read GetManagedFields;
  end;

  TRttiProperty = class(TRttiMember)
  private
    function GetPropertyType: TRttiType; virtual; abstract;
    function GetIsReadable: Boolean; virtual; abstract;
    function GetIsWritable: Boolean; virtual; abstract;
    function DoGetValue(Instance: Pointer): TValue; virtual; abstract;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); virtual; abstract;
  public
    property PropertyType: TRttiType read GetPropertyType;
    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
  end;

  PRecordTypeMethod = ^TRecordTypeMethod;

  TRecordTypeMethod = packed record
    Flags: Byte;
    Code: Pointer;
    NameLen: Byte;
   {Name: ShortString;}
   {Sig: TProcedureSignature;
    AttrData: TAttrData;}
  end;

  TRttiInstanceProperty = class(TRttiProperty)
  private
    function GetDefault: Integer; virtual;
    function GetIndex: Integer; virtual;
    function GetNameIndex: SmallInt; virtual;
    function GetPropertyType: TRttiType; override;
    function GetPropInfo: PPropInfo; virtual; abstract;
    function GetName: string; override;
    function GetIsReadable: Boolean; override;
    function GetIsWritable: Boolean; override;
    function DoGetValue(Instance: Pointer): TValue; override;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); override;
  public
    function ToString: string; override;
    function HasName(const AName: string): Boolean; override;

    property PropertyType: TRttiType read GetPropertyType;
    property Index: Integer read GetIndex;
    property Default: Integer read GetDefault;
    property NameIndex: SmallInt read GetNameIndex;
    property PropInfo: PPropInfo read GetPropInfo;
  end;

  TRttiParameter = class(TRttiNamedObject)
  private
    function GetFlags: TParamFlags; virtual; abstract;
    function GetParamType: TRttiType; virtual; abstract;
  public
    function ToString: string; override;
    property Flags: TParamFlags read GetFlags;
    // ParamType may be nil if it's an untyped var or const parameter.
    property ParamType: TRttiType read GetParamType;
  end;

  TDispatchKind = (dkStatic, dkVtable, dkDynamic, dkMessage, dkInterface);

  TMethodImplementationCallback = reference to procedure(
    UserData: Pointer;
    const Args: TArray<TValue>;
    out Result: TValue);

  [HPPGEN(HPPGenAttribute.mkFriend, 'DELPHICLASS TRttiMethod; DELPHICLASS TRawVirtualClass')]
  TMethodImplementation = class
{$IF Defined(CPUX86)}
  private const
    // give regs negative values to distinguish from stack offs
    regEAX = not 0;                   // -1
    regEDX = not 1;                   // -2
    regECX = not 2;                   // -3
    regFloat = not 3;                 // -4

    rsNone = 0;
    rsEAX = 1 shl not regEAX;         //  1
    rsEDX = 1 shl not regEDX;         //  2
    rsECX = 1 shl not regECX;         //  4
    rsAll = rsEAX or rsEDX or rsECX;  //  7

  private type
    TFloatReg = record
      case Integer of
        0: {ftSingle} (RegSingle: Single);
        1: {ftDouble} (RegDouble: Double);
        2: {ftExtended} (RegExtended: Extended);
        3: {ftComp} (RegComp: Comp);
        4: {ftCurr} (RegCurr: Currency);
        5: (
          Unused1, Unused2: Pointer;
          Unused3: Word; // limit of data used by Extended
          Kind: TFloatType);
    end;

    TInterceptFrame = record
      FP: TFloatReg;
      RegEAX: Pointer;
      RegEDX: Pointer;
      RegECX: Pointer;
 {$IFDEF PC_MAPPED_EXCEPTIONS}
      /// Fake return address for the unwinder to find to enable
      /// us to unwind the stack past an intercept thunk.
      FakeRA: Pointer;
      /// Number of bytes the callee must clear from the stack on
      /// an unwind
      BytesToPop: Integer;
 {$ENDIF PC_MAPPED_EXCEPTIONS}
      Impl: TMethodImplementation;
      PreviousFrame: Pointer;
      RetAddr: Pointer;
      Args: record end;
    end;

    TFirstStageIntercept = packed record
      PushEBP_55: Byte;
      MovEBP_ESP_1_89: Byte;
      MovEBP_ESP_2_E5: Byte;
      Push_68: Byte;
      PushVal: Pointer;
 {$IFDEF PC_MAPPED_EXCEPTIONS}
      /// Intruction to push the number of bytes to pop (BTP)
      BTP_Push_68: Byte;
      BTP_PushVal: Integer;
      /// Instruction to push the fake return addr for the unwinder to find.
      FakeRA_Push_68: Byte;
      FakeRA_PushVal: Pointer;
 {$ENDIF PC_MAPPED_EXCEPTIONS}
      JmpRel_E9: Byte;
      RelTarget: Integer; // relative to @JmpRel_E9
    end;
{$ELSEIF Defined(CPUX64)}
   private type

    {
      Thunk code:

      MOV RAX, SelfVal
      MOV R10, Target
      JMP R10           // absolute addresss
    }
    TFirstStageIntercept = packed record
      MovRAX_48: Byte;
      MovRAX_B8: Byte;
      SelfVal: Pointer;
      MovR10_49: Byte;
      MovR10_BA: Byte;
      Target: Pointer;
      JmpR10_49: Byte;
      JmpR10_FF: Byte;
      JmpR10_E2: Byte;
    end;
 {$IF Defined(WIN64)}
  private const
    // give regs negative values to distinguish from stack offs
    regRCX = not 0;
    regRDX = not 1;
    regR8  = not 2;
    regR9  = not 3;
    regXMM0 = not 4;
    regXMM1 = not 5;
    regXMM2 = not 6;
    regXMM3 = not 7;
    rsNone = 0;
    rsRCX  = 1 shl not regRCX;
    rsXMM0 = rsRCX;
    rsRDX  = 1 shl not regRDX;
    rsXMM1 = rsRDX;
    rsR8   = 1 shl not regR8;
    rsXMM2 = rsR8;
    rsR9   = 1 shl not regR9;
    rsXMM3 = rsR9;
    rsAll  = rsRCX or rsRDX or rsR8 or rsR9;
  private type
    TInterceptFrame = record
      RegXMM0: Pointer;
      RegXMM1: Pointer;
      RegXMM2: Pointer;
      RegXMM3: Pointer;
      PrevFrame: Pointer;
      RetAddr: Pointer;
      // Take advantage of the fact we're given space for parameters
      RegRCX: Pointer;
      RegRDX: Pointer;
      RegR8: Pointer;
      RegR9: Pointer;
      Args: record end;
    end;
 {$ELSEIF Defined(LINUX64) or Defined(OSX64) or (Defined(IOSSIMULATOR) and Defined(CPUX64))}
   private type
    TInterceptFrame = record
      RegXMM: array[0..7] of Double; // XMM0-XMM7, XMM0-XMM1 first/second out float   // offset 0-7     $0-$7
      RegR: array [0..5] of Int64;   // RDI, RSI, RDX, RCX, R8, R9                    // offset 8-13    $8-$D
      OutRAX: UInt64;                // RAX, return first integer                     // offset 14      $E
      OutRDX: UInt64;                // RDX, return second integer                    // offset 15      $F
      OutFPU: Extended;              // FPU, return extended                          // offset 16-17   $10-$11
      UseFPU: Int64;                 // Flag to check if FPU is used                  // offset 18      $12
      Filler: array [0..1] of Pointer;
      Args: record end;
    end;
 {$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Missing types for this CPU'}
 {$ENDIF}
{$ELSEIF Defined(CPUARM32)}
  private type
    TVFPRegisters = record
      case Integer of
        0: ( D: array[0..7] of Double ); // 8 * 8 =64
        1: ( S: array[0..15] of Single );
    end;

    { The Filler field is used to store intermediate jumps addresses
      between dispatch_first_stage_intercept->RawIntercept->rtti_raw_intercept,
      so we need 16 bytes:

        RawIntercept Stub  -> push R7, LR
        rtti_raw_intercept -> push R7, LR
    }
    TInterceptFrame = record
      RegFP: TVFPRegisters;
      Filler : array[0..3] of UInt32;
      RegLR,
      RegCR0,
      RegCR1,
      RegCR2,
      RegCR3: UInt32;
      Args: record end;
    end;

    TFirstStageIntercept = record
    end;
{$ELSEIF Defined(CPUARM64)}
  private type
    m128 = record
      case Integer of
        1: (Lo, Hi : UInt64);
        2: (LL, LH, HL, HH: UInt32);
    end;
    TInterceptFrame = record
//  [sp + 0 = Q0, +16 = Q1 ... +128 = X8, +136 = X0, +208 = old FP, +216 = LR ]
      RegFP : array[0..7] of m128;
      RegCR8 : UInt64;
      RegCR : array[0..7] of UInt64;
      Filler : array[0..2] of UInt64;
      Args: record end;
    end;
    TFirstStageIntercept = record
    end;
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Missing types for this CPU'}
{$ENDIF}

  private type
    PInterceptFrame = ^TInterceptFrame;
    PFirstStageIntercept = ^TFirstStageIntercept;

    TRuntimeTypeInfos = class
    strict private
    const
      CNameLen = 12;
      CNameTempl: ShortString = '~array~of~';
    type
      TOpenArrayTypeInfo = packed record
        FKind: TTypeKind;
        FNameLen: Byte;
        FName: array[0 .. CNameLen - 1] of Byte;
        Size: Integer;
        ElCount: Integer;
        ElType: PPTypeInfo;
        DimCount: Byte;
        Dims: array[0..0] of PPTypeInfo;
        FElType: PTypeInfo;
        FIndType: PTypeInfo;
      end;
    private
      FList: TList<Pointer>;
    public
      constructor Create;
      destructor Destroy; override;
      function DefineOpenArray(AElType: PTypeInfo; AElCount: Integer): PTypeInfo; overload;
      class function IsOpenArray(ATypeInfo: PTypeInfo): Boolean; static;
    end;

    PParamLoc = ^TParamLoc;
    TParamLoc = record
      FTypeInfo: PTypeInfo;
      FByRefParam: Boolean;
      FConstant: Boolean;
      FSetDefault: Boolean;
      FOpenArray: Boolean;
      FOffset: Integer;
      constructor Create(AType: PTypeInfo; AByRef, AConstant, ASetDefault, AOpenArray: Boolean);
      constructor CreateReturn(AType: PTypeInfo; AByRef: Boolean);
      function GetArgLoc(AFrame: PInterceptFrame): Pointer;
      procedure GetArg(AFrame: PInterceptFrame; var Value: TValue);
      procedure GetOpenArrayArg(AFrame: PInterceptFrame; const AHighLoc: TParamLoc;
        var Value: TValue; var ATypeInfos: TRuntimeTypeInfos);
      procedure SetArg(AFrame: PInterceptFrame; const Value: TValue);
    end;

    // Represents all static information required to perform an invocation,
    // in easier to consume format than RTTI objects (stack layout etc.)
    TInvokeInfo = class
    private
{$IF Defined(CPUX86)}
      FCallerPopsStack: Boolean;
{$ENDIF}
{$IF Defined(CPUX86) or (Defined(CPUX64) and not Defined(MSWINDOWS))}
      FResultFP: TFloatType;
{$ENDIF}
      FHasSelf: Boolean; // needed for Pascal calling convention
      FStackSize: Integer;

      FParams: TArray<TParamLoc>;
      FResultLoc: TParamLoc;
      FParamList: TList<TParamLoc>;
      FReturnType: PTypeInfo;
      FCallConv: TCallConv;
      FTypeInfos: TRuntimeTypeInfos;
      procedure SetReturnType(Value: PTypeInfo);
      function GetTypeInfos: TRuntimeTypeInfos;
      procedure CheckNotSealed;
      procedure Seal;
      function LoadArguments(AFrame: PInterceptFrame;
        var ATypeInfos: TRuntimeTypeInfos): TArray<TValue>;
      procedure SaveArguments(AFrame: PInterceptFrame;
        const Args: TArray<TValue>; const Result: TValue);
    public
      constructor Create(ACallConv: TCallConv; AHasSelf: Boolean);
      destructor Destroy; override;
      function GetParamLocs: TArray<TParamLoc>;
      procedure AddParameter(AType: PTypeInfo; AByRef, AConstant, ASetDefault, AOpenArray: Boolean);
      property TypeInfos: TRuntimeTypeInfos read GetTypeInfos;
      property ReturnType: PTypeInfo read FReturnType write SetReturnType;
    end;

  strict private
    FStub: PFirstStageIntercept;
    FUserData: Pointer;
    FCallback: TMethodImplementationCallback;
  protected
    FInvokeInfo: TInvokeInfo;
    [weak] FSafeExcHandler: TObject;
    constructor Create(AUserData: Pointer; AInvokeInfo: TInvokeInfo;
      const ACallback: TMethodImplementationCallback); overload;
    procedure Intercept(AFrame: PInterceptFrame);
    function GetCodeAddress: Pointer; inline;
  public
    // Constructor throws - don't create this way
    constructor Create; overload;
    destructor Destroy; override;
    property CodeAddress: Pointer read GetCodeAddress;
  end;

  TRttiMethod = class(TRttiMember)
  private
    FInvokeInfo: TMethodImplementation.TInvokeInfo;

    function GetIsConstructor: Boolean;
    function GetIsDestructor: Boolean;

    function GetMethodKind: TMethodKind; virtual; abstract;
    function GetCallingConvention: TCallConv; virtual; abstract;
    function GetReturnType: TRttiType; virtual; abstract;
    function GetDispatchKind: TDispatchKind; virtual;
    function GetHasExtendedInfo: Boolean; virtual;
    function GetVirtualIndex: SmallInt; virtual;
    function GetCodeAddress: Pointer; virtual;
    function GetIsClassMethod: Boolean; virtual;
    function GetIsStatic: Boolean; virtual;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; virtual; abstract;
    function GetInvokeInfo: TMethodImplementation.TInvokeInfo;
    procedure GetCommonInvokeParams(var isCons, isDest, isStat, isClas: Boolean;
      var callConv: TCallConv);
  public
    destructor Destroy; override;
    function Invoke(Instance: TObject; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TClass; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TValue; const Args: array of TValue): TValue; overload;
    // Create an implementation of a method with this signature, which delegates
    // implementation to the passed-in callback.
    function CreateImplementation(AUserData: Pointer;
      const ACallback: TMethodImplementationCallback): TMethodImplementation;
    function GetParameters: TArray<TRttiParameter>; virtual; abstract;
    function ToString: string; override;
    property ReturnType: TRttiType read GetReturnType;
    property HasExtendedInfo: Boolean read GetHasExtendedInfo;
    property MethodKind: TMethodKind read GetMethodKind;
    property DispatchKind: TDispatchKind read GetDispatchKind;

    property IsConstructor: Boolean read GetIsConstructor;
    property IsDestructor: Boolean read GetIsDestructor;
    property IsClassMethod: Boolean read GetIsClassMethod;
    // Static: No 'Self' parameter
    property IsStatic: Boolean read GetIsStatic;

    // Vtable slot for virtual methods.
    // Message index for message methods (non-negative).
    // Dynamic index for dynamic methods (negative).
    property VirtualIndex: SmallInt read GetVirtualIndex;
    property CallingConvention: TCallConv read GetCallingConvention;
    property CodeAddress: Pointer read GetCodeAddress;
  end;

  TRttiIndexedProperty = class(TRttiMember)
  private
    FReadMethod: TRttiMethod;
    FWriteMethod: TRttiMethod;
    procedure GetAccessors;
    function GetPropertyType: TRttiType;
    function GetIsReadable: Boolean; inline;
    function GetIsWritable: Boolean; inline;
    function GetIsDefault: Boolean;
    function GetReadMethod: TRttiMethod;
    function GetWriteMethod: TRttiMethod;
    function GetHandle: PArrayPropInfo; inline;
    function GetName: string; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetVisibility: TMemberVisibility; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PArrayPropInfo read GetHandle;
    property PropertyType: TRttiType read GetPropertyType;
    property ReadMethod: TRttiMethod read GetReadMethod;
    property WriteMethod: TRttiMethod read GetWriteMethod;
    function GetValue(Instance: Pointer; const Args: array of TValue): TValue;
    procedure SetValue(Instance: Pointer; const Args: array of TValue; const Value: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property IsDefault: Boolean read GetIsDefault;
    function ToString: string; override;
  end;

  TRttiInstanceType = class(TRttiStructuredType)
  private
    FProps: TArray<TRttiProperty>;
    FMeths: TArray<TRttiMethod>;
    FVirtCount: Word;
    FIndexedProps: TArray<TRttiIndexedProperty>;
    FClassTab: PVmtFieldClassTab;
    FReadPropData: Boolean;
    FReadMethData: Boolean;
    procedure ReadPropData;
    procedure ReadMethData;
    function GetBaseType: TRttiType; override;
    function GetBaseTyped: TRttiInstanceType;
    function GetMetaclassType: TClass;
    function GetDeclaringUnitName: string;
    function GetVmtSize: Integer;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property BaseType: TRttiInstanceType read GetBaseTyped;
    property DeclaringUnitName: string read GetDeclaringUnitName;
    property MetaclassType: TClass read GetMetaclassType;

    function GetDeclaredProperties: TArray<TRttiProperty>; override;
    function GetDeclaredMethods: TArray<TRttiMethod>; override;
    function GetDeclaredFields: TArray<TRttiField>; override;
    function GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>; override;

    function GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
    function GetImplementedInterfaces: TArray<TRttiInterfaceType>;

    // Members declared in this type only.
    function GetAttributes: TArray<TCustomAttribute>; override;
    property VmtSize: Integer read GetVmtSize;
  end;

  TRttiInterfaceType = class(TRttiStructuredType)
  private
    FMethods: TArray<TRttiMethod>;
    FTotalMethodCount: Integer;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetIntfFlags: TIntfFlags;
    function GetDeclaringUnitName: string;
    function GetBaseType: TRttiType; override;
    function GetBaseTyped: TRttiInterfaceType;
    function GetGUID: TGUID;
  public
    property BaseType: TRttiInterfaceType read GetBaseTyped;
    property GUID: TGUID read GetGUID;
    property IntfFlags: TIntfFlags read GetIntfFlags;
    property DeclaringUnitName: string read GetDeclaringUnitName;

    function GetDeclaredMethods: TArray<TRttiMethod>; override;
  end;

                                                                                 
  TRttiOrdinalType = class(TRttiType)
  private
    function GetMaxValue: Integer; virtual;
    function GetMinValue: Integer; virtual;
    function GetOrdType: TOrdType;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    function GetTypeSize: Integer; override;
  public
    property OrdType: TOrdType read GetOrdType;
    property MinValue: Integer read GetMinValue;
    property MaxValue: Integer read GetMaxValue;
  end;

  TRttiInt64Type = class(TRttiType)
  private
    function GetMaxValue: Int64;
    function GetMinValue: Int64;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    function GetTypeSize: Integer; override;
  public
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  TRttiInvokableType = class(TRttiType)
  private
    FProcSig: Pointer;
    function GetReturnType: TRttiType;
    function GetCallingConvention: TCallConv;
  public
    function Invoke(const ProcOrMeth: TValue; const Args: array of TValue): TValue; virtual; abstract;
    function GetParameters: TArray<TRttiParameter>;
    property ReturnType: TRttiType read GetReturnType;
    property CallingConvention: TCallConv read GetCallingConvention;
    function ToString: string; override;
  end;

  TRttiMethodType = class(TRttiInvokableType)
  private
    function GetMethodKind: TMethodKind;
    function GetTypeSize: Integer; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    function Invoke(const Callable: TValue; const Args: array of TValue): TValue; override;
    property MethodKind: TMethodKind read GetMethodKind;
    function ToString: string; override;
  end;

  TRttiProcedureType = class(TRttiInvokableType)
  private
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    function Invoke(const Callable: TValue; const Args: array of TValue): TValue; override;
    function GetAttributes: TArray<TCustomAttribute>; override;
  end;

  TRttiClassRefType = class(TRttiType)
  private
    function GetInstanceType: TRttiInstanceType;
    function GetMetaclassType: TClass;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property InstanceType: TRttiInstanceType read GetInstanceType;
    property MetaclassType: TClass read GetMetaclassType;
  end;

                                                                                     
  TRttiEnumerationType = class(TRttiOrdinalType)
  private
    function GetMaxValue: Integer; override;
    function GetMinValue: Integer; override;
    function GetUnderlyingType: TRttiType;
    function HasEnumNameList: Boolean;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    function GetNames: TArray<string>;
    class function GetName<T{: enum}>(AValue: T): string; reintroduce; static;
    class function GetValue<T{: enum}>(const AName: string): T; static;
    property UnderlyingType: TRttiType read GetUnderlyingType;
  end;

  TRttiSetType = class(TRttiType)
  private
    function GetElementType: TRttiType;
    function GetTypeSize: Integer; override;
    function GetByteOffset: Integer;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property ElementType: TRttiType read GetElementType;
    property ByteOffset: Integer read GetByteOffset;
  end;

  TRttiStringKind = (skShortString, skAnsiString, skWideString, skUnicodeString);

  TRttiStringType = class(TRttiType)
  private
    function GetStringKind: TRttiStringKind;
    function GetTypeSize: Integer; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property StringKind: TRttiStringKind read GetStringKind;
  end;

  TRttiAnsiStringType = class(TRttiStringType)
  private
    function GetCodePage: Word;
  public
    property CodePage: Word read GetCodePage;
  end;

  TRttiFloatType = class(TRttiType)
  private
    function GetFloatType: TFloatType;
    function GetTypeSize: Integer; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property FloatType: TFloatType read GetFloatType;
  end;

  TRttiArrayType = class(TRttiType)
  private
{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    FHFAElementType: TRttiFloatType;
    FHFAElementCount: Integer;
{$ENDIF CPUARM64 or LINUX64 or OSX64}
    function GetTypeSize: Integer; override;
    function GetTotalElementCount: Integer;
    function GetElementType: TRttiType;
    function GetDimensionCount: Integer;
    function GetDimension(Index: Integer): TRttiType;
{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    procedure GetHFAElement;
    function GetIsHFA: Boolean; override;
    function GetHFAElementType: TRttiFloatType; override;
    function GetHFAElementCount: Integer; override;
{$ENDIF CPUARM64 or LINUX64 or OSX64}
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property TotalElementCount: Integer read GetTotalElementCount;
    property ElementType: TRttiType read GetElementType;
    property DimensionCount: Integer read GetDimensionCount;
    property Dimensions[Index: Integer]: TRttiType read GetDimension;
  end;

                                                                                      
  TRttiDynamicArrayType = class(TRttiType)
  private
    function GetDeclaringUnitName: string;
    function GetElementSize: Integer;
    function GetElementType: TRttiType;
    function GetOleAutoVarType: TVarType;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    property ElementSize: Integer read GetElementSize;
    property ElementType: TRttiType read GetElementType;
    property OleAutoVarType: TVarType read GetOleAutoVarType;
    property DeclaringUnitName: string read GetDeclaringUnitName;
  end;

  TRttiPointerType = class(TRttiType)
  private
    function GetReferredType: TRttiType;
  public
    property ReferredType: TRttiType read GetReferredType;
    function GetAttributes: TArray<TCustomAttribute>; override;
  end;

  TRttiPackage = class(TRttiNamedObject)
  private
    FLock: TObject;
    FHandleToObject: TDictionary<Pointer,TRttiObject>;
    FBaseAddress: Pointer;

    function ReadObject(ARttiClass: TRttiClass; AParent: TRttiObject;
      var P: PByte): TRttiObject;
    function ReadObjectPointer(ARttiClass: TRttiClass; AParent: TRttiObject;
      P: Pointer): TRttiObject;
    function GetNameFromType(AType: TRttiType): string; virtual;

    function GetHandle: HINST;
  public
    destructor Destroy; override;
    property Handle: HINST read GetHandle;
    property BaseAddress: Pointer read FBaseAddress;
    function GetTypes: TArray<TRttiType>; virtual; abstract;
    function FindType(const AQualifiedName: string): TRttiType; virtual; abstract;
  end;

  TRttiContext = record
  private
    FContextToken: IInterface;
{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
  private class var
    FGlobalContextCounter: Integer;
    FGlobalContextToken: IInterface;
{$ENDIF}
  public
    class function Create: TRttiContext; static;
    procedure Free;

    class procedure KeepContext; static;
    class procedure DropContext; static;

    function GetType(ATypeInfo: Pointer): TRttiType; overload;
    function GetType(AClass: TClass): TRttiType; overload;
    function GetTypes: TArray<TRttiType>;
    function FindType(const AQualifiedName: string): TRttiType;
    function GetPackages: TArray<TRttiPackage>;
  end;

  TInterceptBeforeNotify = reference to procedure(Instance: TObject;
    Method: TRttiMethod; const Args: TArray<TValue>; out DoInvoke: Boolean;
    out Result: TValue);
  TInterceptAfterNotify = reference to procedure(Instance: TObject;
    Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
  TInterceptExceptionNotify = reference to procedure(Instance: TObject;
    Method: TRttiMethod; const Args: TArray<TValue>; out RaiseException: Boolean;
    TheException: Exception; out Result: TValue);

  TVirtualMethodInterceptor = class
  private type
    TExtraMethodInfo = (eiNormal, eiObjAddRef, eiObjRelease, eiFreeInstance);
    TInterceptInfo = class
    private
      FExtraMethodInfo: TExtraMethodInfo;
      FImpl: TMethodImplementation;
      FOriginalCode: Pointer;
      FProxyCode: Pointer;
      FMethod: TRttiMethod;
    public
      constructor Create(AOriginalCode: Pointer; AMethod: TRttiMethod;
        const ACallback: TMethodImplementationCallback;
        const ExtraMethodInfo: TExtraMethodInfo);
      destructor Destroy; override;
      property ExtraMethodInfo: TExtraMethodInfo read FExtraMethodInfo;
      property OriginalCode: Pointer read FOriginalCode;
      property ProxyCode: Pointer read FProxyCode;
      property Method: TRttiMethod read FMethod;
    end;

  private
    FContext: TRttiContext;
    FOriginalClass: TClass;
    FProxyClass: TClass;
    FProxyClassData: Pointer;
    FIntercepts: TObjectList<TInterceptInfo>;
    FImplementationCallback: TMethodImplementationCallback;
    FOnBefore: TInterceptBeforeNotify;
    FOnAfter: TInterceptAfterNotify;
    FOnException: TInterceptExceptionNotify;
    procedure CreateProxyClass;
    procedure RawCallback(UserData: Pointer; const Args: TArray<TValue>;
      out Result: TValue);
  protected
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoAfter(Instance: TObject; Method: TRttiMethod; const Args: TArray<TValue>;
      var Result: TValue);
    procedure DoException(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out RaiseException: Boolean;
      TheException: Exception; out Result: TValue);
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;
    procedure Proxify(AInstance: TObject);
    procedure Unproxify(AInstance: TObject);
    property OriginalClass: TClass read FOriginalClass;
    property ProxyClass: TClass read FProxyClass;
    property OnBefore: TInterceptBeforeNotify read FOnBefore write FOnBefore;
    property OnAfter: TInterceptAfterNotify read FOnAfter write FOnAfter;
    property OnException: TInterceptExceptionNotify read FOnException write FOnException;
  end;

  ///
  ///  Provides a basis for dynamically creating instances that implement
  ///  interfaces.  The support here is extremely raw.  You create an instance
  ///  of a virtual class by handing it a vtable.  The vtable is a specialized
  ///  type defined here.  The vtable will be populated with slots that point
  ///  to small thunks that dispatch to user code.<p></p>
  ///
  ///  The basic usage model is to construct a virtual class with a vtable,
  ///  and a list of GUIDs of interfaces that the vtable matches.  Thus if
  ///  you have an interface declared to inherit a line of several parents,
  ///  the single vtable will represent each of the parent interfaces.<p></p>
  ///
  ///  The <code>TRawVirtualClass</code> implements <code>QueryInterface</code>,
  ///  <code>AddRef</code> and <code>Release</code> for the interface, so
  ///  that you can query an instance and get back a Delphi interface that can
  ///  be used as if it were any Delphi interface.<p></p>
  ///
  ///  Future improvements: support adding vtables to an existing instance.
  TRawVirtualClass = class(TInterfacedObject, IInterface)
  public
    type

    ///
    ///  The vtable type for the virtual class.  The client will construct
    ///  the vtable to contain whatever slots the client wants.  Slots can
    ///  be populated with specialized thunks that load some context information
    ///  and then dispatch to user code, or by addresses provided directly
    ///  by the user.
    TVTable = class
    const
      ///  Slots for QueryInterface, AddRef and Release are all reserved.
      RESERVED_VTABLE_SLOTS = 3;
    private
      ///  The actual vtable.
      FVTable: TArray<Pointer>;
      ///  Array of thunks that the vtable will point to.  The thunks
      ///  will dispatch to the client's code.
      FInterceptors: TArray<Pointer>;

      function GetVTable: TArray<Pointer>;

    protected
      ///
      ///  Create a vtable with the given number of method slots.  We will allocated
      ///  an addition 3 entries to fill in QueryInterface, AddRef and Release.
      ///  These will dispatch to the virtual class implementation, and cannot be
      ///  set by the client.
      constructor Create(MethodCount: Integer);

    public
      destructor Destroy; override;

      ///
      ///  Set a vtable slot to dispatch to the given procedure pointer.  The thunk
      ///  will execute a very raw dispatch to the procedure.  It will set up a stack
      ///  frame, and push the <code>Context</code> pointer onto the stack.<p></p>
      ///
      ///  The client is responsible for managing the lifetime of the <code>Context</code>.<p></p>
      ///
      ///  See <code>AllocateRawThunk</code> for more detail on the stack frame setup.<p></p>
      ///
      ///  <param name="Idx">Zero based index of the vtable slot to set.  This index
      ///  must be between 0 and the <code>MethodCount - 1</code> value used in
      ///  construction.</param>
      ///  <param name="Proc">Address that the thunk will dispatch to.</param>
      ///  <param name="Context">Pointer to user data that will be pushed
      ///  onto the stack by the thunk.</param>
      procedure SetVTableSlot(Idx: Integer; Proc: Pointer; Context: Pointer); overload;

      ///
      ///  Sets a vtable slot to the actual value of the <code>Proc</code> pointer,
      ///  with no thunk interceding.  Calls to the given vtable slot will land
      ///  directly at that address.
      procedure SetVTableSlot(Idx: Integer; Proc: Pointer); overload;

      ///  Use this property to get the actual vtable.  Typically, this will be used
      ///  by the virtual class instance.
      property VTable: TArray<pointer> read GetVTable;

      ///  Allocates a thunk.  The thunk will be executable, in process code that will
      ///  set up a stack frame, push the <code>Context</code> pointer, and jump to
      ///  <code>Proc</code>.  On x86, the frame will be a standard EBP frame.  When you
      ///  arrive at your code (<code>Proc</code>), the stack will look like this:
      ///  <code>
      ///    |             |  <--- EBP
      ///    | Return Addr |
      ///    | EBP         |
      ///    | Context     |  <--- ESP
      ///  </code><p></p>
      ///
      ///  In a PC Mapped regime, the stack will include two additional magic values:
      ///  <code>
      ///    |             |  <--- EBP
      ///    | Return Addr |
      ///    | EBP         |
      ///    | Context     |
      ///    | BytesToPop  |
      ///    | Thunk addr  |  <--- ESP
      ///  </code><p></p>
      ///
      ///  These values are used to clean up the stack in the event of an exception being
      ///  thrown past the thunk.  If your target procedure has callee cleanup semantics,
      ///  then you must create this thunk with the number of bytes that procedure would
      ///  clear off the stack if you want exception handling semantics to be maintained
      ///  properly.
      ///
      ///  OS X implementors note:  the stack will not be aligned properly.<p></p>
      ///
      class function AllocateRawThunk(Proc: Pointer; Context: Pointer; BytesToPop: Integer): Pointer; static;

      ///
      ///  Frees a thunk allocated with <code>AllocateRawThunk</code>.
      class procedure FreeRawThunk(Thunk: Pointer); static;
    end;

  private
    type
      ///  This record just allows us to have a deterministic way of getting back
      ///  to the instance of the <code>TRawVirtualClass</code> that implements
      ///  a given interface.
      BoundInterface = packed record
        ///  Obj will be set to Self.  This way, we can get to it from the interface ptr.
        Obj: Pointer;
        ///  When we do <code>QueryInterface</code>, we'll return the address of this
        ///  field.
        VTable: TArray<Pointer>;
      end;
      PBoundInterface = ^BoundInterface;
  var
    // N.B.:  If we expand this to support multiple interfaces, adding interfaces
    // to existing instances, then you have to be careful here.  You can't just use
    // an array of BoundInterface, because resizing the array may reallocate it,
    // and this would invalidate any interfaces that people have already made references
    // to.  It will have to be a linked list, and not a TList, either.  You have to make
    // sure that the BoundInterface records do not move in memory.
    FVTable: BoundInterface;
    FIIDs: TArray<TGUID>;    // GUIDs of the interfaces that we have vtables for
    function _AddRefFromIntf: Integer; stdcall;
    function _ReleaseFromIntf: Integer; stdcall;
    function _QIFromIntf(const IID: TGUID; out Obj): HResult; stdcall;

  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    ///
    ///  Finds the interface pointer for the given GUID.  This returns only pointers
    ///  to interfaces that are actual virtual interfaces added to this class.  It
    ///  does not follow the full semantics of QueryInterface, in which we delegate
    ///  to inherited versions if we don't find the interface locally.  This interface
    ///  is very special in that it returns the raw pointer of the interface, without
    ///  calling AddRef on the interface.  It's a very special case operation that
    ///  is useful only in extreme edge cases.  Because the pointer is not AddRefed,
    ///  you must be extremely careful with how you use the result, lest you
    ///  inadvertantly cause an object to be freed.  Returns <code>nil</code> if the
    ///  interface cannot be found locally.
    function FindInterface(IID: TGUID): Pointer;

    ///
    ///  Create instances of virtual classes by handing in a vtable, along with
    ///  a list of GUIDS that you know that vtable represents.
    constructor Create(Guids: TArray<TGUID>; VTable: TRawVirtualClass.TVTable); overload;

    destructor Destroy; override;

    ///
    ///  Given an interface that you <emphasis>know</emphasis> comes from a virtual
    ///  class, this method will return the instance of <code>TRawVirtualClass</code>
    ///  that implements that specific interface.  If you hand in an interface that
    ///  is not from a virtual class, you'll get back a reference to something, but
    ///  the contents will be undefined.
    class function GetInstanceFromInterface(Intf: Pointer): TRawVirtualClass; static;
  end;

  { TVirtualInterfaceInvokeEvent: Invoke Event for TVirtualMethodInterface.
    When a method of the Interface that is being virtualized is invoked, the
    OnInvoke event is fired. Implement this event handler to perform some
    action for invoked methods. }
  TVirtualInterfaceInvokeEvent = reference to procedure(Method: TRttiMethod;
    const Args: TArray<TValue>; out Result: TValue);

  { TVirtualInterface: Creates an implementation of an interface at runtime.
    All methods in the Interface are marshaled through a generic stub function
    that raises the OnInvoke event.}
  TVirtualInterface = class(TInterfacedObject, IInterface)

  private type
    { TImplInfo: Helper class that keeps a reference to the TRttiMethod metadata
      as well as the implemention of the stub function }
    TImplInfo = class
    private
      FImpl: TMethodImplementation;
      FMethod: TRttiMethod;
      function GetCodeAddress: Pointer;
        function GetVirtualIndex: SmallInt;
    public
      constructor Create(AMethod: TRttiMethod;
        const ACallback: TMethodImplementationCallback);
      destructor Destroy; override;
      property CodeAddress: Pointer read GetCodeAddress;
      property VirtualIndex: SmallInt read GetVirtualIndex;
    end;

  private
    VTable: PPointer;       // Constructed VTable for Interface
    FIID: TGUID;            // GUID of the Interface being implemented
    FContext: TRttiContext; // Local reference to Context so metadata doesn't expire
    FIntercepts: TObjectList<TImplInfo>;     // List of Implelmentation functions
    FOnInvoke: TVirtualInterfaceInvokeEvent; // Invoke Event
    { Functions for the generated VTable. When these are invoked, the Self
      pointer is pointing to the generated VTable. In order to access members
      of the TVirtualInterface class (such as the reference count), this pointer
      has to be adjusted before actually performing these actions}
    function _AddRefFromIntf: Integer; stdcall;
    function _ReleaseFromIntf: Integer; stdcall;
    function _QIFromIntf(const IID: TGUID; out Obj): HResult; stdcall;

    // Stub function called by all methods in the interface.
    procedure RawCallback(UserData: Pointer {TImplInfo}; const Args: TArray<TValue>;
      out Result: TValue); virtual;
    procedure ErrorProc;
  protected
    // IInterface methods. Make them virtual so derived classes can do their own
    // lifetime management if needed.
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    { Create an instance of TVirtualInterface that implements the methods of
      an interface.  PIID is the PTypeInfo for the Interface that is to be
      implemented. The Interface must have TypeInfo ($M+). Either inherit from
      IInvokable, or enable TypeInfo for the interface. Because this is an
      TInterfacedObject, it is reference counted and it should not be Freed directly.
      }
    constructor Create(PIID: PTypeInfo); overload;
    constructor Create(PIID: PTypeInfo; InvokeEvent: TVirtualInterfaceInvokeEvent); overload;
    destructor Destroy; override;
    { OnInvoke: Event raised when a method of the implemented interface is called.
      Assign a OnInvoke handler to perform some action on invoked methods.}
    property OnInvoke: TVirtualInterfaceInvokeEvent read FOnInvoke write FOnInvoke;
  end;

function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False;
  IsConstructor: Boolean = False ): TValue;
function IsManaged(TypeInfo: PTypeInfo): Boolean;
function IsBoolType(ATypeInfo: PTypeInfo): Boolean;

type
  TGetArrayValueFunc = reference to function(AArrType: TRttiType): TValue;

{ These 2 functions convert TValue keeping TListHelper instance to/from TValue
  keeping TArray<T>. This is useful for JSON marshaling. These functions must
  be in sync with System.Generics.Collections.TListHelper layout. }
function GetArrayValueFromTListHelperValue(const ACtx: TRttiContext;
  const AListHelperValue: TValue; out ACount: Integer): TValue;
procedure SetTListHelperValueFromArrayValue(const ACtx: TRttiContext;
  var AListHelperValue: TValue; const AGetArrValFunc: TGetArrayValueFunc);

implementation

{ A note on locking protocol:

  Mutations to objects generally occur while holding the package lock
  (FPackage.FLock) for the owning package. Thus, it is not safe to
  mutate an object from another package while holding the current package lock;
  it could lead to deadlock. Similarly, package-owned objects constructors'
  run while the package lock is held.

  Pool initialization is protected by the pool lock, PoolLock.

  Attribute construction (possibly lazy) is controlled by the PoolLock,
  since attribute construction may involve instantiation of a TRttiType as
  an argument where that type lives in another package, which could lead
  to deadlock. So, attribute constructors should not start a new thread,
  attempt RTTI operations, and block on this new thread without anticipating
  deadlock; all attribute construction is serialized. }

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysMman,
  Posix.Unistd,
  Posix.Dlfcn,
{$ENDIF POSIX}
{$IFDEF PC_MAPPED_EXCEPTIONS}
  System.Internal.Unwind,
{$ENDIF PC_MAPPED_EXCEPTIONS}
  System.SysConst,
  System.Hash,
  System.Types,
  System.Math,
  System.Generics.Defaults,
  System.RTLConsts;

{$IFDEF EXTERNALLINKER}
const
  RTLHelperLibName =
{$IF Defined(PIC) and Defined(LINUX)}
   'librtlhelper_PIC.a';
{$ELSE}
   'librtlhelper.a';
{$ENDIF}
{$ENDIF EXTERNALLINKER}

type
  PIntPtr = ^IntPtr;

type
  TPrivateHeap = class(TObject)
  private type
    THeapItem = record
      Addr: PByte;
      Size: Cardinal;
    end;
  private
    FMMaped: TList<Pointer>;
    FFree: TList<THeapItem>;
    FAllocated: TList<THeapItem>;
    FPageSize: Cardinal;
{$IFDEF MSWINDOWS}
  private
    FHandle: THandle;
    function GetHandle: THandle;
  public
    property Handle: THandle read GetHandle;
{$ENDIF MSWINDOWS}
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetMem(var P{: pointer}; Size: DWORD); virtual;
    procedure FreeMem(P: pointer);
    function SizeOfMem(P: pointer): DWORD;
  end;

var
  FCodeHeap: TPrivateHeap;

function CodeHeap: TPrivateHeap;
var
  Temp: TPrivateHeap;
begin
  if FCodeHeap = nil then
  begin
    Temp := TPrivateHeap.Create;
    if AtomicCmpExchange(Pointer(FCodeHeap), Pointer(Temp), nil) = nil then
      Pointer(Temp) := nil // For ARC-based platforms this will ensure the local var doesn't affect the refcount
    else
      Temp.Free;
  end;
  Result := FCodeHeap;
end;

{ TPrivateHeap }

constructor TPrivateHeap.Create;
{$IFDEF MSWINDOWS}
var
  SysInfo: SYSTEM_INFO;
{$ENDIF MSWINDOWS}
begin
  FMMaped := TList<Pointer>.Create;
  FFree := TList<THeapItem>.Create;
  FAllocated := TList<THeapItem>.Create;
{$IFDEF MSWINDOWS}
  GetSystemInfo(SysInfo);
  FPageSize := SysInfo.dwPageSize;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  FPageSize := sysconf(_SC_PAGESIZE);
{$ENDIF POSIX}
end;

destructor TPrivateHeap.Destroy;
var
  I: Integer;
begin
  FFree.Free;
  FAllocated.Free;
  for I := 0 to FMMaped.Count - 1 do
  begin
{$IFDEF MSWINDOWS}
    if not Winapi.Windows.HeapFree(Handle, 0, FMMaped[I]) then
      RaiseLastOSError;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    if munmap(FMMaped[I], FPageSize) <> 0 then
      RaiseLastOSError;
{$ENDIF POSIX}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    // Make sure the unwinder doesn't track this block of memory anymore
    // for exceptions.
    SysUnregisterIPLookup(NativeUInt(FMMaped[I]));
{$ENDIF PC_MAPPED_EXCEPTIONS}
  end;
  FMMaped.Free;
{$IFDEF MSWINDOWS}
  if FHandle <> 0 then
  begin
    if not Winapi.Windows.HeapDestroy(FHandle) then
      RaiseLastOSError;
    FHandle := 0;
  end;
{$ENDIF MSWINDOWS}
  inherited Destroy;
end;

procedure TPrivateHeap.FreeMem(P: pointer);
var
  I: Integer;
  Item: THeapItem;
  AllocList: TList<THeapItem>.ParrayofT;
  FreeList: TList<THeapItem>.ParrayofT;

  function TryMergeFree(const I: Integer): Boolean;
  begin
    Result := False;
    FreeList := FFree.PList;
    if (FreeList^[I].Addr + FreeList^[I].Size) = FreeList^[I + 1].Addr then
    begin
      FreeList^[I].Size := FreeList^[I].Size + FreeList^[I + 1].Size;
      FFree.Delete(I + 1);
      Result := True;
    end;
  end;

begin
  Item := Default(THeapItem);
  TMonitor.Enter(FAllocated);
  try
    AllocList := FAllocated.PList;
    for I := 0 to FAllocated.Count - 1 do
      if AllocList^[I].Addr = P then
      begin
        Item := AllocList^[I];
        FAllocated.Delete(I);
        Break;
      end;
  finally
    TMonitor.Exit(FAllocated);
  end;
  if Item.Addr = nil then
    raise Exception.CreateRes(@SArgumentInvalid);

  TMonitor.Enter(FFree);
  try
    FreeList := FFree.PList;
    if (FFree.Count = 0) or (UIntPtr(P) < UIntPtr(FreeList^[0].Addr)) then
    begin
      FFree.Insert(0, Item);
      if FFree.Count > 1 then
        TryMergeFree(0);
    end
    else if (FFree.Count > 0) and (UIntPtr(P) > UIntPtr(FreeList^[FFree.Count - 1].Addr)) then
    begin
      FFree.Add(Item);
      if FFree.Count > 1 then
        TryMergeFree(FFree.Count - 2);
    end
    else
      for I := 1 to FFree.Count - 1 do
        if (UIntPtr(P) > UIntPtr(FreeList^[I - 1].Addr)) and (UIntPtr(P) < UIntPtr(FreeList^[I].Addr)) then
        begin
          FFree.Insert(I, Item);
          while (I <= FFree.Count - 1) and TryMergeFree(I - 1) do
            ;
          Break;
        end;
  finally
    TMonitor.Exit(FFree);
  end;
end;

{$IFDEF MSWINDOWS}
function TPrivateHeap.GetHandle: THandle;
var
  Temp: THandle;
begin
  if FHandle = 0 then
  begin
    Temp := Winapi.Windows.HeapCreate(HEAP_CREATE_ENABLE_EXECUTE, 0, 0);
    if Temp = 0 then
      RaiseLastOSError;
    if AtomicCmpExchange(FHandle, Temp, 0) <> 0 then
      if not Winapi.Windows.HeapDestroy(Temp) then
        RaiseLastOSError;
  end;
  Result := FHandle;
end;
{$ENDIF MSWINDOWS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
///
///  Called by the unwinder when unwinding the stack for an exception in the
///  PC Mapped environment.
function VirtualFrameUnwinder(Version: Integer; Actions: _Unwind_Action;
                              ExceptionObject: _Unwind_Exception;
                              Context: _Unwind_Context;
                              ImplementationData: Pointer) : _Unwind_Reason_Code; cdecl;
var
  P: _PUnwind_RegVal;
  PRetAddr: _PUnwind_RegVal;
  BytesToPop: Integer;
begin
  // Our thunks have a stack frame in them that we'll use to toss the frame.
  // In addition, they contain information about how many bytes the callee was
  // expected to clear off the stack.
  P := _PUnwind_RegVal(BorUnwind_GetGR(Context, _UW_REG_EBP));
  PRetAddr := _PUnwind_RegVal(_Unwind_RegVal(P) + SizeOf(Pointer));
  BytesToPop := PInteger(_Unwind_RegVal(P) - SizeOf(Pointer) * 2)^;
  BorUnwind_SetGR(Context, _UW_REG_EBP, P^);
  BorUnwind_SetGR(Context, _UW_REG_ESP, _Unwind_RegVal(P) + (SizeOf(Pointer) * 2) + NativeUInt(BytesToPop));
  BorUnwind_SetGR(Context, _UW_REG_EIP, PRetAddr^);
  Result := _URC_CONTINUE_UNWIND;
end;

///
///  Called by the unwinder to lookup the unwinder for a given PC Map address range.
///  In our case, we never have to search around the range at all for any information,
///  we just return our single generic unwinder, since all thunks look the same.
function VirtualFrameLookup(Addr: NativeUInt; Context: Pointer): TFrameUnwinder; cdecl;
begin
  Result := @VirtualFrameUnwinder;
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$WARN SYMBOL_PLATFORM OFF}
procedure TPrivateHeap.GetMem(var P{: pointer}; Size: DWORD);
var
  I, UseItem: Integer;
  FreeList: TList<THeapItem>.ParrayofT;
  FreeItem, NewItem: THeapItem;
begin
                                                              
  {$IF Defined(CPU64BITS)}
  Size := (Size + 15) and not 15;  // Always align to 16 bytes.
  {$ELSE}
  Size := (Size + 7) and not 7;    // Always align to 8 bytes.
  {$ENDIF}
  UseItem := -1;
  TMonitor.Enter(FFree);
  try
    FreeList := FFree.PList;
    for I := 0 to FFree.Count -1 do
    begin
      if FreeList^[I].Size >= Size then
      begin
        FreeItem := FreeList^[I];
        UseItem := I;
        Break;
      end;
    end;
    if UseItem = -1 then
    begin
  {$IFDEF MSWINDOWS}
      FreeItem.Addr := Winapi.Windows.HeapAlloc(Handle, 0, FPageSize);
      if FreeItem.Addr = nil then
        RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
      FreeItem.Addr := mmap(nil, FPageSize, PROT_WRITE or PROT_EXEC, MAP_PRIVATE or MAP_ANON, -1, 0);
      if FreeItem.Addr = MAP_FAILED then
        RaiseLastOsError;
  {$ENDIF POSIX}
  {$IFDEF PC_MAPPED_EXCEPTIONS}
      // Register the address range of the block with the unwinder, so that
      // we can unwind the stack if an exception is thrown past our thunk.
      SysRegisterIPLookupFunc(NativeUInt(FreeItem.Addr),
                              NativeUInt(FreeItem.Addr) + NativeUInt(FPageSize - 1), nil, 0,
                              @VirtualFrameLookup);
  {$ENDIF PC_MAPPED_EXCEPTIONS}
      FreeItem.Size := FPageSize;
      FMMaped.Add(FreeItem.Addr);
      for I := FFree.Count - 1 downto 0 do
        if FreeItem.Addr < FreeList^[I].Addr then
        begin
          UseItem := I;
          FFree.Insert(UseItem, FreeItem);
          Break;
        end;
      if UseItem = -1 then
        UseItem := FFree.Add(FreeItem);
    end;
    NewItem.Addr := FreeItem.Addr;
    NewItem.Size := Size;
    FreeItem.Addr := FreeItem.Addr + Size;
    FreeItem.Size := FreeItem.Size - Size;
    if FreeItem.Size = 0 then
      FFree.Delete(UseItem)
    else
      FFree[UseItem] := FreeItem;
    Pointer(P) := NewItem.Addr;
  finally
    TMonitor.Exit(FFree);
  end;
  TMonitor.Enter(FAllocated);
  try
    FAllocated.Add(NewItem);
  finally
    TMonitor.Exit(FAllocated);
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

function TPrivateHeap.SizeOfMem(P: pointer): DWORD;
var
  I: Integer;
  AllocList: TList<THeapItem>.ParrayofT;
begin
  Result := 0;
  TMonitor.Enter(FAllocated);
  try
    AllocList := FAllocated.PList;
    for I := 0 to FAllocated.Count - 1 do
      if AllocList^[I].Addr = P then
        Exit(AllocList^[I].Size);
  finally
    TMonitor.Exit(FAllocated);
  end;
end;

{ Loaded Module List versioning }

var
  ModListRemovals: Integer;

function GetModuleListVersion: Integer; inline;
begin
  // Items are added to start, but may be removed from middle.
  Result := ModListRemovals xor Integer(LibModuleList);
end;

procedure DeletePackageFromPool(HInstance: HINST); forward;

procedure OnUnloadModule(HInstance: HINST);
begin
  DeletePackageFromPool(HInstance);
end;

{ Finalizer, for destroying objects upon anonymous method scope destruction }

type
  IFinalizer = interface
    procedure Add(Obj: TObject);
  end;

  TFinalizer = class(TInterfacedObject, IFinalizer)
  private
    FItems: TArray<TObject>;
    FCount: Integer;
  public
    destructor Destroy; override;
    procedure Add(Obj: TObject);
  end;

{ TFinalizer }

destructor TFinalizer.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].Free;
  inherited;
end;

procedure TFinalizer.Add(Obj: TObject);
var
  len: Integer;
begin
  len := Length(FItems);
  if len = 0 then
    SetLength(FItems, 4)
  else if len = FCount then
    SetLength(FItems, len * 2);
  FItems[FCount] := Obj;
  Inc(FCount);
end;

function MakeFinalizer: IFinalizer;
begin
  Result := TFinalizer.Create;
end;

{ Helpers }

function InsufficientRtti: Exception;
begin
  Result := EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

procedure CheckCodeAddress(code: Pointer);
begin
  if (code = nil) or (PPointer(code)^ = nil) then
    raise InsufficientRtti;
end;

function DerefPointer(p: Pointer): Pointer; inline;
begin
  Result := p;
  if Result <> nil then
    Result := PPointer(Result)^;
end;

function GetEnumBaseType(ATypeInfo: PTypeInfo): PTypeInfo;
var
  pResult: PPTypeInfo;
begin
  if (ATypeInfo = nil) or (ATypeInfo^.Kind <> tkEnumeration) then
    Exit(nil);
  Result := ATypeInfo;
  while True do
  begin
    pResult := GetTypeData(Result)^.BaseType;
    if (pResult <> nil) and (pResult^ <> nil) and (pResult^ <> Result) then
      Result := pResult^
    else
      Break;
  end;
end;

function IsBoolType(ATypeInfo: PTypeInfo): Boolean;
begin
  ATypeInfo := GetEnumBaseType(ATypeInfo);
  Result := (ATypeInfo = System.TypeInfo(Boolean)) or
    (ATypeInfo = System.TypeInfo(ByteBool)) or
    (ATypeInfo = System.TypeInfo(WordBool)) or
    (ATypeInfo = System.TypeInfo(LongBool));
  if not Result then
    if (ATypeInfo <> nil) and (ATypeInfo^.Kind = tkEnumeration) then
      with GetTypeData(ATypeInfo)^ do
        if (MinValue = 0) and (MaxValue = 1) then // match C++ bool
          Result := ATypeInfo.NameFld.ToString = 'bool'; // do not localize
end;

{ Do-nothing interface implementation }

function NopRefCount(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function NopRet0(inst: Pointer): Integer;
begin
  Result := 0;
end;

const
  Nop_Vtable: array[0..6] of Pointer =
  (
    @NopQueryInterface,
    @NopRefCount,
    @NopRefCount,
    @NopRet0, // function GetDataSize: Integer;
    @NopRet0, // procedure ExtractRawData(ABuffer: Pointer);
    @NopRet0, // procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    @NopRet0 // function GetReferenceToRawData: Pointer;
  );
  Nop_Instance_Data: Pointer = @Nop_Vtable;
  Nop_Instance: Pointer = @Nop_Instance_Data;

function IsManaged(TypeInfo: PTypeInfo): Boolean;
var
  elTypePtr: PPTypeInfo;
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
{$IFDEF AUTOREFCOUNT}
    tkClass,
{$ENDIF AUTOREFCOUNT}
    tkDynArray, tkUString, tkWString, tkLString, tkInterface, tkVariant,
    tkMethod, tkMRecord:
      Result := True;
    tkRecord:
      Result := GetTypeData(TypeInfo)^.ManagedFldCount > 0;
    tkArray:
      begin
        elTypePtr := GetTypeData(TypeInfo)^.ArrayData.ElType;
        Result := (elTypePtr <> nil) and IsManaged(elTypePtr^);
      end;
  else
    Result := False;
  end;
end;

procedure PushSelfFirst(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
{$IFDEF CPUX86}
  if CC = ccPascal then Exit;
{$ENDIF CPUX86}
  argList[Index] := Value;
  Inc(Index);
end;

procedure PushSelfLast(CC: TCallConv; var argList: TArray<TValue>;
  var Index: Integer; const Value: TValue); inline;
begin
{$IFDEF CPUX86}
  if CC <> ccPascal then Exit;
  argList[Index] := Value;
{$ELSE !CPUX86}
                                                                                   
{$ENDIF CPUX86}
end;

// 0 => nil
// >0 => inline ok
// <0 => heap data
// Main purpose is to determine where & how much data to blit when copying in / out.
function GetInlineSize(TypeInfo: PTypeInfo): Integer;
begin
  if TypeInfo = nil then
    Exit(0);

  case TypeInfo^.Kind of
    tkInteger, tkEnumeration, tkChar, tkWChar:
      case GetTypeData(TypeInfo)^.OrdType of
        otSByte, otUByte: Exit(1);
        otSWord, otUWord: Exit(2);
        otSLong, otULong: Exit(4);
      else
        Exit(0);
      end;
    tkSet:
      begin
        Result := SizeOfSet(TypeInfo);
{$IF   SizeOf(Extended) > SizeOf(TMethod)}
        if Result > SizeOf(Extended) then
          Result := -Result;
{$ELSE SizeOf(Extended) <= SizeOf(TMethod)}
        if Result > SizeOf(TMethod) then
          Result := -Result;
{$ENDIF}
        Exit;
      end;
    tkFloat:
      case GetTypeData(TypeInfo)^.FloatType of
        ftSingle: Exit(4);
        ftDouble: Exit(8);
        ftExtended: Exit(SizeOf(Extended));
        ftComp: Exit(8);
        ftCurr: Exit(8);
      else
        Exit(0);
      end;
    tkClass:
{$IFDEF AUTOREFCOUNT}
      Exit(-SizeOf(Pointer));
{$ELSE  AUTOREFCOUNT}
      Exit(SizeOf(Pointer));
{$ENDIF AUTOREFCOUNT}
    tkClassRef: Exit(SizeOf(Pointer));
    tkMethod: Exit(SizeOf(TMethod));
    tkInt64: Exit(8);
    tkDynArray, tkUString, tkLString, tkWString, tkInterface: Exit(-SizeOf(Pointer));
{$IFNDEF NEXTGEN}
    tkString: Exit(-GetTypeData(TypeInfo)^.MaxLength - 1);
{$ENDIF !NEXTGEN}
    tkPointer: Exit(SizeOf(Pointer));
    tkProcedure: Exit(SizeOf(Pointer));
    tkRecord, tkMRecord: Exit(-GetTypeData(TypeInfo)^.RecSize);
    tkArray: Exit(-GetTypeData(TypeInfo)^.ArrayData.Size);
    tkVariant: Exit(-SizeOf(Variant));
  else
    Exit(0);
  end;
end;

function UseResultPointer(TypeInfo: PTypeInfo; IsConstructor: Boolean;CC: TCallConv): Boolean;
{$IF Defined(CPUARM64)}
var
  ctx: TRttiContext;
{$ENDIF}
begin
  if TypeInfo = nil then
    Exit(False);

  case TypeInfo^.Kind of
{$IFDEF AUTOREFCOUNT}
    tkClass:
      Result := not IsConstructor;
{$ENDIF AUTOREFCOUNT}
    tkMethod:
{$IF Defined(OSX64)}
      Exit( CC in [ccSafeCall] ); // RDX:RAX will used except SafeCall
{$ELSE}
      Exit(True);
{$ENDIF}
    tkInterface, tkDynArray, tkUString, tkLString, tkWString,
    tkString, tkVariant:
      Exit(True);
    tkRecord, tkMRecord:
{$IF Defined(CPUX64)}
{$IF Defined(MSWINDOWS)}
      case GetTypeData(TypeInfo)^.RecSize of
        1, 2, 4: Result := False;
        8: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ELSEIF Defined(LINUX64) or Defined(OSX64)}
      case GetTypeData(TypeInfo)^.RecSize of
        1..7: Result := False; // Managed-type spend a size of pointer.
        8..16: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ELSE}
      Result := not (GetTypeData(TypeInfo)^.RecSize in [1..16]);
{$ENDIF}
{$ELSEIF Defined(CPUX86)}
      case GetTypeData(TypeInfo)^.RecSize of
        1, 2: Result := False;
        4: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ELSEIF Defined(CPUARM32)}
      case GetTypeData(TypeInfo)^.RecSize of
{$IF Defined(ANDROID32) or Defined(LINUX)}
        1..4: Result := False;
{$ELSEIF Defined(IOS32)}
        1: Result := False;
{$ENDIF}
      else
        Result := True;
      end;
{$ELSEIF Defined(CPUARM64)}
      if ctx.GetType(TypeInfo).IsHFA then
        Result := False
      else
        case GetTypeData(TypeInfo)^.RecSize of
          1..7: Result := False; // Managed-type spend a size of pointer.
          8..16: Result := IsManaged(TypeInfo);
        else
          Result := True;
        end;
{$ENDIF CPU}
    tkArray:
{$IF     Defined(CPUX64) and (Defined(Linux64) or Defined(OSX64))}
      case GetTypeData(TypeInfo)^.ArrayData.Size of
        1..7: Result := False; // Managed-type spend a size of pointer.
        8..16: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ELSEIF Defined(CPUX64)}
      case GetTypeData(TypeInfo)^.ArrayData.Size of
        1, 2, 4: Result := False;
        8: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ELSEIF Defined(CPUX86) or Defined(CPUARM32)}
      case GetTypeData(TypeInfo)^.ArrayData.Size of
        1, 2: Result := False;
        4: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ELSEIF Defined(CPUARM64)}
      case GetTypeData(TypeInfo)^.ArrayData.Size of
        1..7: Result := False; // Managed-type spend a size of pointer.
        8..16: Result := IsManaged(TypeInfo);
      else
        Result := True;
      end;
{$ENDIF CPU}
    tkSet:
      Result := SizeOfSet(TypeInfo) > SizeOf(NativeInt);
  else
    Result := False;
  end;
end;

{$IF Defined(CPUARM64) or Defined(LINUX64)}
procedure CheckHFA(const ARttiType: TRttiType; IsResult: Boolean);
begin
                                  
  if not ARttiType.IsHFA and (ARttiType.TypeKind = tkRecord) and
    ARttiType.AsRecord.HFAHasInsufficientTypeInformation
  then
    if IsResult then
      raise ENotImplemented.CreateFmt(SInsufficientTypeInformationResult,
        [ARttiType.Name])
    else
      raise ENotImplemented.CreateFmt(SInsufficientTypeInformation,
        [ARttiType.Name]);
end;
{$ENDIF CPUARM64 or LINUX64}

function PassByRef(TypeInfo: PTypeInfo; CC: TCallConv; IsConst: Boolean = False): Boolean;
var
  setSize: Integer;
{$IF Defined(CPUX64) and not Defined(MSWINDOWS) or Defined(CPUARM64)}
  ctx: TRttiContext;
{$ENDIF CPUX64 and !MSWINDOWS or CPUARM64}
{$IF Defined(CPUX64) and not Defined(MSWINDOWS)}
  rttiType: TRttiType;
{$ENDIF CPUX64 and !MSWINDOWS}
begin
  if TypeInfo = nil then
    Exit(False);
  case TypeInfo^.Kind of
{$IF Defined(CPUX86)}
    tkArray:
      Result := GetTypeData(TypeInfo)^.ArrayData.Size > SizeOf(Pointer);
    tkRecord:
      if (CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst then
        Result := False
      else
        Result := GetTypeData(TypeInfo)^.RecSize > SizeOf(Pointer);
    tkMRecord:
      Result := True;
    tkVariant: // like tkRecord, but hard-coded size
      Result := not ((CC in [ccCdecl, ccStdCall, ccSafeCall]) and not IsConst);
{$ELSEIF Defined(CPUX64)}
{$IF Defined(MSWINDOWS)}
    tkArray:
      if CC in [ccCdecl, ccStdCall, ccSafeCall] then
        Result := not (GetTypeData(TypeInfo)^.ArrayData.Size in [1,2,4,8])
      else
        Result := not (GetTypeData(TypeInfo)^.ArrayData.Size in [1,2,4]);
    tkRecord:
      if CC in [ccCdecl, ccStdCall, ccSafeCall] then
        Result := not (GetTypeData(TypeInfo)^.RecSize in [1,2,4,8])
      else
        Result := not (GetTypeData(TypeInfo)^.RecSize in [1,2,4]);
    tkMRecord,
    tkMethod,
    tkVariant:
      Result := True;
{$ELSE !MSWINDOWS} // Linux / OSX
    tkArray:
    begin
      rttiType := ctx.GetType(TypeInfo);
      {$IF Defined(LINUX64)}
      CheckHFA(rttiType, False);
      {$ENDIF}
      if rttiType.IsHFA then
        Result := CC in [ccReg, ccPascal]
      else
        Result := (GetTypeData(TypeInfo)^.ArrayData.Size > 4) and (CC in [ccReg, ccPascal]) or
          IsManaged(TypeInfo);
    end;
    tkRecord:
      Result := (GetTypeData(TypeInfo)^.RecSize > 4) and (CC in [ccReg, ccPascal]) or
        IsManaged(TypeInfo);
    tkMRecord,
    tkMethod,
    tkVariant:
      Result := True;
{$ENDIF}
{$ELSEIF Defined(CPUARM32)}
    tkArray,
    tkRecord:
      Result := CC in [ccReg, ccPascal];                                    
    tkMRecord,
    tkMethod,
    tkVariant:
      Result := True;
{$ELSEIF Defined(CPUARM64)}
    tkArray:
      if ctx.GetType(TypeInfo).IsHFA then
        Result := CC in [ccReg, ccPascal]
      else
        Result := (GetTypeData(TypeInfo)^.ArrayData.Size > 16) // ARM64 use two-GPR to store 16bytes record
{$IFDEF ANDROID64}
               or ((CC in [ccReg, ccPascal]) and (GetTypeData(TypeInfo)^.ArrayData.Size > 4));
{$ELSE !ANDROID64}
               or (CC in [ccReg, ccPascal]);
{$ENDIF ANDROID64}
    tkRecord:
      if ctx.GetType(TypeInfo).IsHFA then
        Result := CC in [ccReg, ccPascal]
      else
{$IFDEF ANDROID64}
      if TypeInfo^.Kind = tkMRecord then
        Result := True
      else
        Result := (GetTypeData(TypeInfo)^.ArrayData.Size > 16) // ARM64 use two-GPR to store 16bytes record
               or ((CC in [ccReg, ccPascal]) and (GetTypeData(TypeInfo)^.ArrayData.Size > 4));
{$ELSE !ANDROID64}
        Result := (GetTypeData(TypeInfo)^.RecSize > 16) or (CC in [ccReg, ccPascal]);
{$ENDIF ANDROID64}
    tkMRecord,
    tkMethod,
    tkVariant:
      Result := True;
{$ENDIF CPUTYPE}
{$IFNDEF NEXTGEN}
    tkString:
      Result := True;
{$ENDIF !NEXTGEN}
    tkSet:
    begin
      setSize := SizeOfSet(TypeInfo);
{$IF Defined(WIN64)}
      Result := (setSize > 8) or (setSize > 4) and (CC in [ccReg, ccPascal]);
{$ELSEIF Defined(EXTERNALLINKER)}
      Result := setSize > 4;
{$ELSE}
      Result := setSize > SizeOf(NativeInt);
{$ENDIF}
    end;
  else
    Result := False;
  end;
end;

procedure PassArg(Par: TRttiParameter; const ArgSrc: TValue;
  var ArgDest: TValue; CC: TCallConv);
var
  parType: TRttiType;
  parFlags: TParamFlags;
begin
  parType := Par.ParamType;
  parFlags := Par.Flags;
  if parType = nil then
    TValue.Make<Pointer>(ArgSrc.GetReferenceToRawData, ArgDest) // untyped var or const
  else if parFlags * [pfVar, pfOut] <> [] then
  begin
    if parType.Handle <> ArgSrc.TypeInfo then
      raise EInvalidCast.CreateRes(@SByRefArgMismatch);
    TValue.Make<Pointer>(ArgSrc.GetReferenceToRawData, ArgDest);
  end
  else if (pfConst in parFlags) and
    ((pfReference in parFlags) or PassByRef(parType.Handle, CC, True)) then
  begin
    if TypeInfo(TValue) = parType.Handle then
      TValue.Make(ArgSrc, ArgDest)
    else
    begin
      if parType.Handle <> ArgSrc.TypeInfo then
        raise EInvalidCast.CreateRes(@SByRefArgMismatch);
      TValue.Make(ArgSrc.GetReferenceToRawData, ArgDest);
    end
  end
  else if ArgSrc.TypeInfo = parType.Handle then
    ArgDest := ArgSrc
  else if not ArgSrc.TryCast(parType.Handle, ArgDest) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function AllocReg(var Regs: UInt32): UInt32;
var
  newRegs: UInt32;
begin
  if Regs = 0 then
    Exit(0);
  newRegs := Regs and (Regs - 1); // clear lowest bit
  Result := Regs and not newRegs; // reveal bit cleared
  Regs := newRegs;
end;

function FreeRegCount(Regs: UInt32): UInt32;
begin
  Result := 0;
  while Regs <> 0 do
  begin
    if Regs and 1 <> 0 then
      Inc(Result);
    Regs := Regs shr 1;
  end;
end;

function Align4(Value: Integer): Integer; inline;
begin
  Result := (Value + 3) and not 3;
end;

function Align8(Value: Integer): Integer; inline;
begin
  Result := (Value + 7) and not 7;
end;

function Align16(Value: Integer): Integer; inline;
begin
  Result := (Value + 15) and not 15;
end;

function AlignPointer(const p: Pointer; const Alignment: NativeUInt): Pointer; inline;
begin
  Result := Pointer((NativeUInt(p) + (Alignment - 1)) and not (Alignment - 1));
end;

{ TValueDataImpl }

type
  TValueDataImpl = class(TInterfacedObject, IValueData)
  private
    FTypeInfo: PTypeInfo;
    FData: TArray<Byte>;
    FIsMoved: Boolean;
  public
    constructor Create(ABuffer: Pointer; ACount: Integer; ATypeInfo: PTypeInfo);
    constructor CreateEmpty(ACount: Integer; ATypeInfo: PTypeInfo);
    constructor CreateWithoutCopy(ABuffer: Pointer; ACount: Integer; ATypeInfo: PTypeInfo; IsMoved: Boolean);
    destructor Destroy; override;
    function GetDataSize: Integer;
    procedure ExtractRawData(ABuffer: Pointer);
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    function GetReferenceToRawData: Pointer;
  end;

constructor TValueDataImpl.Create(ABuffer: Pointer; ACount: Integer; ATypeInfo: PTypeInfo);
begin
  CreateEmpty(ACount, ATypeInfo);
  if (ATypeInfo <> nil) and (ATypeInfo^.Kind = tkMRecord) then
    InitializeArray(@FData[0], ATypeInfo, 1);
  if ABuffer <> nil then
    if IsManaged(ATypeInfo) then
      CopyArray(@FData[0], ABuffer, ATypeInfo, 1)
    else
      Move(ABuffer^, FData[0], ACount);
end;

constructor TValueDataImpl.CreateEmpty(ACount: Integer; ATypeInfo: PTypeInfo);
begin
  FTypeInfo := ATypeInfo;
  FIsMoved := False;
  SetLength(FData, ACount);
end;

constructor TValueDataImpl.CreateWithoutCopy(ABuffer: Pointer; ACount: Integer; ATypeInfo: PTypeInfo; IsMoved: Boolean);
begin
  CreateEmpty(ACount, ATypeInfo);
  FIsMoved := IsMoved;
  if ABuffer <> nil then
    Move(ABuffer^, FData[0], ACount);
end;

destructor TValueDataImpl.Destroy;
begin
  if IsManaged(FTypeInfo) and not FIsMoved then
    FinalizeArray(@FData[0], FTypeInfo, 1);
end;

function TValueDataImpl.GetDataSize: Integer;
begin
  Result := Length(FData);
end;

procedure TValueDataImpl.ExtractRawData(ABuffer: Pointer);
begin
  if IsManaged(FTypeInfo) then
  begin
    if FTypeInfo^.Kind = tkMRecord then
      InitializeArray(ABuffer, FTypeInfo, 1);
    CopyArray(ABuffer, @FData[0], FTypeInfo, 1);
  end
  else
    Move(FData[0], ABuffer^, Length(FData));
end;

procedure TValueDataImpl.ExtractRawDataNoCopy(ABuffer: Pointer);
begin
  Move(FData[0], ABuffer^, Length(FData));
end;

function TValueDataImpl.GetReferenceToRawData: Pointer;
begin
  Result := @FData[0];
end;

{ TValueMDataImpl<T> }

type
  TValueMDataImpl<T> = record
  private type
    PT = ^T;
  private
    FVTable: Pointer;
    FRefCount: Integer;
    FData: T;
    FIsMoved: Boolean;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IValueData }
    function GetDataSize: Integer;
    procedure ExtractRawData(ABuffer: Pointer);
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    function GetReferenceToRawData: Pointer;
  public
    class function Create(ABuffer: PT; AVMT: Pointer): Pointer; static;
    class function CreateWithoutCopy(ABuffer: PT; AIsMoved: Boolean; AVMT: Pointer): Pointer; static;
  end;
  TValueDataStringImpl = TValueMDataImpl<UnicodeString>;
  TValueDataInterfaceImpl = TValueMDataImpl<IInterface>;

const
  Str_VTable: array[0..6] of Pointer =
  (
    @TValueDataStringImpl.QueryInterface,
    @TValueDataStringImpl._AddRef,
    @TValueDataStringImpl._Release,
    @TValueDataStringImpl.GetDataSize,
    @TValueDataStringImpl.ExtractRawData,
    @TValueDataStringImpl.ExtractRawDataNoCopy,
    @TValueDataStringImpl.GetReferenceToRawData
  );
  Intf_VTable: array[0..6] of Pointer =
  (
    @TValueDataInterfaceImpl.QueryInterface,
    @TValueDataInterfaceImpl._AddRef,
    @TValueDataInterfaceImpl._Release,
    @TValueDataInterfaceImpl.GetDataSize,
    @TValueDataInterfaceImpl.ExtractRawData,
    @TValueDataInterfaceImpl.ExtractRawDataNoCopy,
    @TValueDataInterfaceImpl.GetReferenceToRawData
  );

class function TValueMDataImpl<T>.Create(ABuffer: PT; AVMT: Pointer): Pointer;
var
  p: Pointer;
begin
  p := AllocMem(SizeOf(TValueMDataImpl<T>));
  with TValueMDataImpl<T>(p^) do
  begin
    FVTable := AVMT;
    if ABuffer <> nil then
      FData := ABuffer^;
    Result := @FVTable;
  end;
end;

class function TValueMDataImpl<T>.CreateWithoutCopy(ABuffer: PT; AIsMoved: Boolean; AVMT: Pointer): Pointer;
var
  p: Pointer;
begin
  p := AllocMem(SizeOf(TValueMDataImpl<T>));
  with TValueMDataImpl<T>(p^) do
  begin
    FVTable := AVMT;
    FIsMoved := AIsMoved;
    if ABuffer <> nil then
      PPointer(@FData)^ := PPointer(ABuffer)^;
    Result := @FVTable;
  end;
end;

function TValueMDataImpl<T>._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TValueMDataImpl<T>._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    if FIsMoved then
      PPointer(@FData)^ := nil
    else
      FData := Default(T);
    FreeMem(@Self);
  end;
end;

function TValueMDataImpl<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

function TValueMDataImpl<T>.GetDataSize: Integer;
begin
  Result := SizeOf(T);
end;

procedure TValueMDataImpl<T>.ExtractRawData(ABuffer: Pointer);
begin
  PT(ABuffer)^ := FData;
end;

procedure TValueMDataImpl<T>.ExtractRawDataNoCopy(ABuffer: Pointer);
begin
  PPointer(ABuffer)^ := PPointer(@FData)^;
end;

function TValueMDataImpl<T>.GetReferenceToRawData: Pointer;
begin
  Result := @FData;
end;

{ TValue }

class procedure TValue.InitData(var AValue: TValue);
var
  p: PUInt64;
begin
  p := PUInt64(@AValue.FAsUByte);
{$IF defined(CPU64BITS) and (SizeOf(TValue) = 32) or defined(CPU32BITS) and (SizeOf(TValue) = 24)}
  p^ := 0;
  PUInt64(NativeUInt(p) + 8)^ := 0;
{$ELSEIF defined(CPU32BITS) and (SizeOf(TValue) = 16)}
  p^ := 0;
{$ELSE}
  {$MESSAGE Fatal 'Unsupported TValue size'}
{$ENDIF}
end;

class function TValue.Create(ATypeInfo: PTypeInfo): TValue;
begin
  Result.FTypeInfo := ATypeInfo;
  Result.FValueData := nil; // we may be constructed in place of already-existing record
  Pointer(Result.FValueData) := Nop_Instance;
  InitData(Result);
end;

class constructor TValue.Create;
begin
  FEmpty := TValue.Create(nil);
end;

class function TValue.From<T>(const Value: T): TValue;
begin
  Make(@Value, System.TypeInfo(T), Result);
end;

function TValue.IsType<T>(const EmptyAsAnyType: Boolean): Boolean;
begin
  Result := IsType(System.TypeInfo(T), EmptyAsAnyType);
end;

function TValue.TryAsTypeInternal(var AResult; ATypeInfo: PTypeInfo;
  const EmptyAsAnyType: Boolean): Boolean;
var
  val: TValue;
begin
  Result := TryCast(ATypeInfo, val, EmptyAsAnyType);
  if Result then
    if val.FTypeInfo = nil then
      FillChar(Pointer(@AResult)^, Abs(GetInlineSize(ATypeInfo)), 0)
    else
      val.ExtractRawData(@AResult);
end;

procedure TValue.AsTypeInternal(var AResult; ATypeInfo: Pointer);
var
  val: TValue;
begin
  if TryCast(ATypeInfo, val, True) then
    if val.FTypeInfo = nil then
      FillChar(Pointer(@AResult)^, Abs(GetInlineSize(ATypeInfo)), 0)
    else
      val.ExtractRawData(@AResult)
  else
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValue.TryAsType<T>(out AResult: T; const EmptyAsAnyType: Boolean): Boolean;
begin
  Result := TryAsTypeInternal(AResult, System.TypeInfo(T), EmptyAsAnyType);
end;

function TValue.AsType<T>(const EmptyAsAnyType: Boolean): T;
begin
  if not TryAsTypeInternal(Result, System.TypeInfo(T), EmptyAsAnyType) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValue.Cast<T>(const EmptyAsAnyType: Boolean): TValue;
begin
  if not TryCast(System.TypeInfo(T), Result, EmptyAsAnyType) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValue.GetIsEmpty: Boolean;
begin
  // FTypeInfo = nil but FValueData <> nil => an initialized nil (e.g. TValue.Empty)
  // FValueData = nil => uninitialized, FTypeInfo is garbage
  if (FTypeInfo = nil) or (FValueData = nil) then
    Exit(True);
  case FTypeInfo^.Kind of
{$IFDEF AUTOREFCOUNT}
    tkClass: Result := (FValueData = nil) or
      (PPointer(FValueData.GetReferenceToRawData)^ = nil);
{$ELSE  AUTOREFCOUNT}
    tkClass: Result := FAsObject = nil;
{$ENDIF AUTOREFCOUNT}
    tkInterface: Result := PPointer(FValueData.GetReferenceToRawData)^ = nil;
    tkMethod: Result := FAsMethod.Code = nil;
    tkPointer, tkProcedure: Result := FAsPointer = nil;
    tkDynArray: Result := GetArrayLength = 0;
    tkClassRef: Result := FAsClass = nil;
  else
    Result := False;
  end;
end;

function TValue.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TValue.GetTypeKind: TTypeKind;
begin
  if (FTypeInfo = nil) or (FValueData = nil) then
    Result := tkUnknown
  else
    Result := FTypeInfo^.Kind;
end;

function TValue.GetTypeDataProp: PTypeData;
begin
  if FTypeInfo = nil then
    Exit(nil);
  Result := System.TypInfo.GetTypeData(FTypeInfo);
end;

function TValue.GetDataSize: Integer;
begin
  if FTypeInfo = nil then
    if (FValueData = nil) or (Pointer(FValueData) = Nop_Instance) then
      Exit(0)
    else
      Exit(IValueData(FValueData).GetDataSize)
  else if FValueData = nil then
    Exit(0) // uninitialized
  else
    case FTypeInfo^.Kind of
      tkInteger, tkEnumeration, tkChar, tkWChar:
        case GetTypeData(FTypeInfo)^.OrdType of
          otSByte, otUByte: Exit(1);
          otSWord, otUWord: Exit(2);
          otSLong, otULong: Exit(4);
        else
          Exit(0);
        end;
      tkSet:
        Exit(SizeOfSet(FTypeInfo));
      tkFloat:
        case GetTypeData(FTypeInfo)^.FloatType of
          ftSingle: Exit(4);
          ftDouble: Exit(8);
          ftExtended: Exit(SizeOf(Extended));
          ftComp: Exit(8);
          ftCurr: Exit(8);
        else
          Exit(0);
        end;
      tkClass, tkInterface, tkDynArray, tkWString, tkUString, tkLString,
        tkClassRef, tkPointer: Exit(SizeOf(Pointer));
      tkArray, tkRecord, tkMRecord, tkString: Exit(FValueData.GetDataSize);
      tkProcedure: Exit(SizeOf(TProc));
      tkMethod: Exit(SizeOf(TMethod));
      tkInt64: Exit(8);
      tkVariant: Exit(SizeOf(Variant));
    else
      Exit(0);
    end;
end;

class function TValue.GetEmpty: TValue;
begin
  Result := FEmpty;
end;

class operator TValue.Implicit(const Value: string): TValue;
begin
  Result := TValue.Create(System.TypeInfo(string));
  Result.FValueData := IValueData(TValueDataStringImpl.Create(@Value, @Str_VTable));
end;

class operator TValue.Implicit(Value: Integer): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Integer));
  Result.FAsSLong := Value;
end;

class operator TValue.Implicit(Value: Cardinal): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Cardinal));
  Result.FAsULong := Value;
end;

class operator TValue.Implicit(Value: Single): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Single));
  Result.FAsSingle := Value;
end;

class operator TValue.Implicit(Value: Double): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Double));
  Result.FAsDouble := Value;
end;

class operator TValue.Implicit(Value: Extended): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Extended));
  Result.FAsExtended := Value;
end;

class operator TValue.Implicit(Value: Currency): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Currency));
  Result.FAsCurr := Value;
end;

class operator TValue.Implicit(Value: Int64): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Int64));
  Result.FAsSInt64 := Value;
end;

class operator TValue.Implicit(Value: UInt64): TValue;
begin
  Result := TValue.Create(System.TypeInfo(UInt64));
  Result.FAsUInt64 := Value;
end;

class operator TValue.Implicit(Value: TObject): TValue;
var
  info: PTypeInfo;
begin
  if Value = nil then
    Exit(Empty);
  info := Value.ClassInfo;
  if info <> nil then
  begin
    Make(@Value, info, Result);
    Exit;
  end;
  Result := From<TObject>(Value);
end;

class operator TValue.Implicit(Value: TClass): TValue;
begin
  if Value = nil then
    Exit(Empty);
  Result := TValue.Create(System.TypeInfo(TClass));
  Result.FAsClass := Value;
end;

class operator TValue.Implicit(Value: Boolean): TValue;
begin
  Result := TValue.Create(System.TypeInfo(Boolean));
  Result.FAsUByte := Ord(Value);
end;

class operator TValue.Implicit(const VarRec: TVarRec): TValue;
begin
  Result := TValue.FromVarRec(VarRec)
end;

class operator TValue.Implicit(Value: TDateTime): TValue;
begin
  Result := TValue.Create(System.TypeInfo(TDateTime));
  Result.FAsDouble := Value;
end;

class operator TValue.Implicit(Value: TTime): TValue;
begin
  Result := TValue.Create(System.TypeInfo(TTime));
  Result.FAsDouble := Value;
end;

class operator TValue.Implicit(Value: TDate): TValue;
begin
  Result := TValue.Create(System.TypeInfo(TDate));
  Result.FAsDouble := Value;
end;

class function TValue.FromVariant(const Value: Variant): TValue;
begin
  case TVarData(Value).VType of
    varEmpty, varNull: Result := TValue.From<Variant>(Value);
    varBoolean: Result := TVarData(Value).VBoolean;
    varShortInt: Result := TValue.From(TVarData(Value).VShortInt);
    varSmallint: Result := TValue.From(TVarData(Value).VSmallInt);
    varInteger: Result := TValue.From(TVarData(Value).VInteger);
    varSingle: Result := TVarData(Value).VSingle;
    varDouble: Result := TVarData(Value).VDouble;
    varCurrency: Result := TVarData(Value).VCurrency;
    varDate: Result := TValue.From<TDateTime>(TVarData(Value).VDate);
    varOleStr: Result := TValue.From<WideString>(WideString(Pointer(TVarData(Value).VOleStr)));
    varDispatch: Result := TValue.From<IDispatch>(IDispatch(TVarData(Value).VDispatch));
    varError: Result := TValue.From<HRESULT>(TVarData(Value).VError);
    varUnknown: Result := TValue.From<IInterface>(IInterface(TVarData(Value).VUnknown));
    varByte: Result := TValue.From(TVarData(Value).VByte);
    varWord: Result := TValue.From(TVarData(Value).VWord);
    varUInt32: Result := TValue.From(TVarData(Value).VUInt32);
    varInt64: Result := TVarData(Value).VInt64;
    varUInt64: Result := TVarData(Value).VUInt64;
    varString: Result := TValue.From<RawByteString>(RawByteString(TVarData(Value).VString));
    varUString: Result := UnicodeString(TVarData(Value).VUString);
  else
    raise EVariantTypeCastError.CreateRes(@SInvalidVarCast);
  end;
end;

class function TValue.FromVarRec(const VarRec: TVarRec): TValue;
begin
  case VarRec.VType of
    vtInteger: Result := VarRec.VInteger;
    vtBoolean: Result := VarRec.VBoolean;
    vtWideChar: Result := TValue.From<WideChar>(VarRec.VWideChar);
    vtInt64: Result := VarRec.VInt64^;
{$IF Declared(UTF8Char)}
    vtChar: Result := TValue.From<AnsiChar>(VarRec.VChar);
{$ENDIF}
{$IF Declared(PUTF8Char)}
    vtPChar: Result := string(VarRec.VPChar);
{$ENDIF}
    vtPWideChar: Result := string(VarRec.VPWideChar);
{$IF Declared(ShortString)}
    vtString: Result := TValue.From<ShortString>(ShortString(VarRec.VString^));
{$ENDIF}
{$IF Declared(WideString)}
    vtWideString: Result := TValue.From<WideString>(WideString(VarRec.VWideString));
{$ENDIF}
{$IF Declared(AnsiString)}
    vtAnsiString: Result := TValue.From<AnsiString>(AnsiString(VarRec.VAnsiString));
{$ENDIF}
    vtUnicodeString: Result := UnicodeString(VarRec.VUnicodeString);
    vtObject: Result := TObject(VarRec.VObject);
    vtPointer: Result := VarRec.VPointer;
    vtInterface: Result := TValue.From<IInterface>(IInterface(VarRec.VInterface));
    vtClass: Result := VarRec.VClass;
    vtVariant: Result := TValue.FromVariant(VarRec.VVariant^);
    vtExtended: Result := VarRec.VExtended^;
    vtCurrency: Result := VarRec.VCurrency^;
  end;
end;

function TValue.IsObject: Boolean;
begin
  Result := IsEmpty or (FTypeInfo^.Kind = tkClass);
end;

function TValue.IsObjectInstance: Boolean;
begin
  Result := (FTypeInfo <> nil) and (FTypeInfo^.Kind = tkClass)
end;

function TValue.AsObject: TObject;
begin
  if IsEmpty then
    Result := nil
  else if FTypeInfo^.Kind <> tkClass then
    raise EInvalidCast.CreateRes(@SInvalidCast)
  else
  begin
{$IFDEF AUTOREFCOUNT}
    FValueData.ExtractRawData(@Result);
{$ELSE  AUTOREFCOUNT}
    Result := TObject(FAsObject);
{$ENDIF AUTOREFCOUNT}
  end;
end;

function TValue.IsInstanceOf(AClass: TClass): Boolean;
{$IFDEF AUTOREFCOUNT}
var
  obj: TObject;
begin
  if IsObject and (FValueData <> nil) then
  begin
    FValueData.ExtractRawData(@obj);
    Result := obj.InheritsFrom(AClass);
  end
  else
    Result := False;
end;
{$ELSE  AUTOREFCOUNT}
begin
  Result := IsObject and (FAsObject <> nil) and TObject(FAsObject).InheritsFrom(AClass);
end;
{$ENDIF AUTOREFCOUNT}

class function TValue.FromOrdinal(ATypeInfo: PTypeInfo; AValue: Int64): TValue;
begin
  if (ATypeInfo = nil) or not (ATypeInfo^.Kind in [tkInteger,
    tkChar, tkWChar, tkEnumeration, tkInt64]) then
    raise EInvalidCast.CreateRes(@SInvalidCast);

  TValue.Make(@AValue, ATypeInfo, Result);
end;

function GetDynArrayElType(ATypeInfo: PTypeInfo): PTypeInfo;
var
  ref: PPTypeInfo;
begin
  // Get real element type of dynamic array, even if it's array of array of etc.
  ref := GetTypeData(ATypeInfo).DynArrElType;
  if ref = nil then
    Exit(nil);
  Result := ref^;
end;

class function TValue.FromArray(ArrayTypeInfo: PTypeInfo; const Values: array of TValue): TValue;
  function MakeDynamic: TValue;
  var
    arr: Pointer;
    len: NativeInt;
    i, elSize: Integer;
    elLoc: PByte;
    elType: PTypeInfo;
  begin
    arr := nil;
    len := Length(Values);
    elSize := GetTypeData(ArrayTypeInfo)^.elSize;
    elType := GetDynArrayElType(ArrayTypeInfo);
    if elType = nil then // array elements have no typeinfo???
      raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
    DynArraySetLength(arr, ArrayTypeInfo, 1, @len);
    try
      elLoc := arr;
      for i := 0 to High(Values) do
      begin
        Values[i].Cast(elType).ExtractRawData(elLoc);
        Inc(elLoc, elSize);
      end;
      TValue.Make(@arr, ArrayTypeInfo, Result); // makes copy of array
    finally
      DynArrayClear(arr, ArrayTypeInfo);
    end;
  end;

  function MakeStatic: TValue;
  var
    elLoc: PByte;
    elSize: Integer;
    elTypeRef: PPTypeInfo;
    i, elCount: Integer;
  begin
    TValue.Make(nil, ArrayTypeInfo, Result);
    elLoc := Result.GetReferenceToRawData;
    // If the target array is multidimensional, then Values could be either
    // of two things: a flattened list of ultimate elements, or an array of
    // arrays.
    // An array of arrays has a lot of redundant copying (=> inefficient), and
    // is harder to handle (static array typeinfo is not recursive for
    // each successive dimension, so can't easily call TValue.Cast).
    // So a flattened list of elements is the only thing supported for now.
    elCount := GetTypeData(ArrayTypeInfo)^.ArrayData.ElCount;
    if elCount <> Length(Values) then
      raise EArgumentException.Create('Values'); // do not localize
    elTypeRef := GetTypeData(ArrayTypeInfo)^.ArrayData.ElType;
    if (elTypeRef = nil) or (elTypeRef^ = nil) then
      raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
    elSize := GetTypeData(ArrayTypeInfo)^.ArrayData.Size div elCount;
    Assert(elSize > 0);
    for i := 0 to elCount - 1 do
    begin
      Values[i].Cast(elTypeRef^).ExtractRawData(elLoc);
      Inc(elLoc, elSize);
    end;
  end;

begin
  case ArrayTypeInfo^.Kind of
    tkDynArray:
      Result := MakeDynamic;
    tkArray:
      Result := MakeStatic;
  else
    raise EArgumentException.Create('ArrayTypeInfo'); // do not localize
  end;
end;

function TValue.IsClass: Boolean;
begin
  Result := IsType<TClass>;
end;

function TValue.AsClass: TClass;
begin
  AsTypeInternal(Result, System.TypeInfo(TClass));
end;

function TValue.IsOrdinal: Boolean;
begin
  Result := IsEmpty or
    ((TypeInfo <> nil) and
     (TypeInfo^.Kind in [tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64]));
end;

function TValue.AsOrdinal: Int64;
begin
  if not TryAsOrdinal(Result) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValue.TryAsOrdinal(out AResult: Int64): Boolean;
begin
  Result := IsOrdinal;
  if Result then
  begin
    AResult := 0;
    if not IsEmpty then
      case TypeInfo^.Kind of
        tkInteger,
        tkChar,
        tkWChar,
        tkEnumeration:
          case GetTypeData(TypeInfo)^.OrdType of
            otSByte: AResult := FAsSByte;
            otUByte: AResult := FAsUByte;
            otSWord: AResult := FAsSWord;
            otUWord: AResult := FAsUWord;
            otSLong: AResult := FAsSLong;
            otULong: AResult := FAsULong;
          end;
        tkInt64:
          AResult := FAsSInt64;
      end;
  end;
end;

function TValue.IsType(ATypeInfo: PTypeInfo; const EmptyAsAnyType: Boolean): Boolean;
var
  unused: TValue;
begin
  Result := TryCast(ATypeInfo, unused, EmptyAsAnyType);
end;

function TValue.Cast(ATypeInfo: PTypeInfo; const EmptyAsAnyType: Boolean): TValue;
begin
  if not TryCast(ATypeInfo, Result, EmptyAsAnyType) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValue.AsInteger: Integer;
begin
  if not IsEmpty then
  begin
    if FTypeInfo = System.TypeInfo(Integer) then
      Exit(FAsSLong)
    else if FTypeInfo^.Kind = tkInteger then
      case GetTypeData(FTypeInfo)^.OrdType of
        otSByte: Exit(FAsSByte);
        otSWord: Exit(FAsSWord);
        otSLong: Exit(FAsSLong);
      else
        Exit(FAsULong);
      end;
  end;
  AsTypeInternal(Result, System.TypeInfo(Integer));
end;

function TValue.AsBoolean: Boolean;
begin
  if not IsEmpty and (FTypeInfo = System.TypeInfo(Boolean)) then
    Exit(FAsUByte <> 0);
  AsTypeInternal(Result, System.TypeInfo(Boolean));
end;

function TValue.AsExtended: Extended;
begin
  if not IsEmpty then
  begin
    if FTypeInfo = System.TypeInfo(Extended) then
      Exit(FAsExtended)
    else if FTypeInfo^.Kind = tkFloat then
      case GetTypeData(FTypeInfo)^.FloatType of
        ftSingle: Exit(FAsSingle);
        ftDouble: Exit(FAsDouble);
        ftExtended: Exit(FAsExtended);
      end;
  end;
  AsTypeInternal(Result, System.TypeInfo(Extended));
end;

function TValue.AsInt64: Int64;
begin
  if not IsEmpty then
  begin
    if FTypeInfo = System.TypeInfo(Int64) then
      Exit(FAsSInt64)
    else if FTypeInfo = System.TypeInfo(UInt64) then
      Exit(FAsUInt64)
    else if FTypeInfo = System.TypeInfo(Cardinal) then
      Exit(FAsULong)
    else if FTypeInfo^.Kind = tkInteger then
      Exit(AsInteger);
  end;
  AsTypeInternal(Result, System.TypeInfo(Int64));
end;

function TValue.AsUInt64: UInt64;
begin
  if not IsEmpty then
  begin
    if FTypeInfo = System.TypeInfo(Int64) then
      Exit(FAsSInt64)
    else if FTypeInfo = System.TypeInfo(UInt64) then
      Exit(FAsUInt64)
    else if FTypeInfo = System.TypeInfo(Cardinal) then
      Exit(FAsULong)
    else if FTypeInfo^.Kind = tkInteger then
      Exit(AsInteger);
  end;
  AsTypeInternal(Result, System.TypeInfo(UInt64));
end;

function TValue.AsInterface: IInterface;
begin
  if not IsEmpty and (FTypeInfo^.Kind = tkInterface) then
    FValueData.ExtractRawData(@Result);
  AsTypeInternal(Result, System.TypeInfo(IInterface));
end;

function TValue.AsString: string;
begin
  if not IsEmpty then
    case FTypeInfo^.Kind of
{$IFNDEF NEXTGEN}
      tkString: Exit(UnicodeString(PShortString(GetReferenceToRawData)^));
      tkWString: Exit(UnicodeString(PWideString(GetReferenceToRawData)^));
{$ENDIF !NEXTGEN}
      tkLString: Exit(UnicodeString(PRawByteString(GetReferenceToRawData)^));
      tkUString: Exit(PUnicodeString(GetReferenceToRawData)^);
    end;
  AsTypeInternal(Result, System.TypeInfo(string));
end;

function TValue.AsVariant: Variant;
begin
  AsTypeInternal(Result, System.TypeInfo(Variant));
end;

function TValue.CastToVarRec: TValue;
begin
  case Kind of
    TTypeKind.tkFloat:
      Result := Cast<Extended>;
    TTypeKind.tkString,
    TTypeKind.tkWString,
    TTypeKind.tkLString,
    TTypeKind.tkUString:
      Result := Cast<UnicodeString>;
    TTypeKind.tkInt64:
      Result := Cast<Int64>;
  else
    Result := Self;
  end;
end;

function TValue.AsVarRec: TVarRec;
const
  TypeKindToVType: array[TTypeKind] of Byte = (
    $FF,         // tkUnknown,
    vtInteger,   // tkInteger,
    vtChar,      // tkChar,
    vtInteger,   // tkEnumeration,
    vtExtended,  // tkFloat,
    vtString,    // tkString,
    vtInteger,   // tkSet,
    vtObject,    // tkClass,
    $FF,         // tkMethod,
    vtWideChar,  // tkWChar,
    vtAnsiString,// tkLString,
    vtWideString,// tkWString,
    vtVariant,   // tkVariant,
    $FF,         // tkArray,
    $FF,         // tkRecord,
    vtInterface, // tkInterface,
    vtInt64,     // tkInt64,
    $FF,         // tkDynArray,
    vtUnicodeString, // tkUString,
    vtClass,     // tkClassRef,
    vtPointer,   // tkPointer,
    vtPointer,   // tkProcedure,
    $FF          // tkMRecord
    );
begin
  if (Kind = tkRecord) and (TypeInfo = System.TypeInfo(TVarRec)) then
  begin
    ExtractRawData(@Result);
    Exit;
  end;
  FillChar(Result, SizeOf(Result), 0);
  Result.VType := TypeKindToVType[Kind];
  case Kind of
    TTypeKind.tkInteger: Result.VInteger := AsInteger;
    TTypeKind.tkChar: Result.VChar := UTF8Char(FAsSByte);
    TTypeKind.tkWChar: Result.VWideChar := WideChar(FAsSWord);
    TTypeKind.tkFloat:
      if GetTypeData(FTypeInfo)^.FloatType in [ftSingle {$IF SizeOf(Double) <> SizeOf(Extended)}, ftDouble {$ENDIF}] then
        raise EInvalidCast.CreateRes(@SInvalidCast)
      else
        Result.VExtended := GetReferenceToRawData;
    TTypeKind.tkString: Result.VString := GetReferenceToRawData;
    TTypeKind.tkLString: Result.VAnsiString := PPointer(GetReferenceToRawData)^;
    TTypeKind.tkWString: Result.VWideString := PPointer(GetReferenceToRawData)^;
    TTypeKind.tkUString: Result.VUnicodeString := PPointer(GetReferenceToRawData)^;
    TTypeKind.tkClass: Result.VObject := Pointer(AsObject);
    TTypeKind.tkInt64: Result.VInt64 := GetReferenceToRawData;
    TTypeKind.tkClassRef: Result.VClass := AsClass;
    TTypeKind.tkVariant: Result.VVariant := GetReferenceToRawData;
    TTypeKind.tkInterface: Result.VInterface := Pointer(AsInterface);
    TTypeKind.tkProcedure,
    TTypeKind.tkPointer: AsTypeInternal(Result.VPointer, System.TypeInfo(Pointer));
    TTypeKind.tkEnumeration:
      if IsType<Boolean> then
      begin
        Result.VType := vtBoolean;
        Result.VBoolean := AsBoolean;
      end
      else
        Result.VInteger := AsInteger;
  else
    raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
end;

function TValue.AsCurrency: Currency;
begin
  AsTypeInternal(Result, System.TypeInfo(Currency));
end;

function TValue.IsArray: Boolean;
begin
  Result := (TypeInfo <> nil) and (TypeInfo^.Kind in [tkArray, tkDynArray]);
end;

function TValue.GetArrayLength: Integer;
begin
  if TypeInfo <> nil then
    if TypeInfo^.Kind = tkArray then
      Exit(TypeData^.ArrayData.ElCount)
    else if TypeInfo^.Kind = tkDynArray then
      Exit(DynArraySize(PPointer(GetReferenceToRawData)^));

  raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function GetArrayElType(ATypeInfo: PTypeInfo): PTypeInfo;
var
  ref: PPTypeInfo;
begin
  if ATypeInfo^.Kind = tkArray then
  begin
    ref := GetTypeData(ATypeInfo)^.ArrayData.ElType;
    if ref = nil then
      Result := nil
    else
      Result := ref^;
  end
  else if ATypeInfo^.Kind = tkDynArray then
    Exit(GetDynArrayElType(ATypeInfo))
  else
    Exit(nil);
end;

function TValue.GetArrayElement(Index: Integer): TValue;
begin
  if not IsArray then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  if (Index < 0) or (Index >= GetArrayLength) then
    raise EArgumentOutOfRangeException.Create('Index'); // do not localize

  TValue.Make(
    GetReferenceToRawArrayElement(Index),
    GetArrayElType(TypeInfo),
    Result);
end;

procedure TValue.SetArrayElement(Index: Integer; const AValue: TValue);
begin
  if not IsArray then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  if (Index < 0) or (Index >= GetArrayLength) then
    raise EArgumentOutOfRangeException.Create('Index'); // do not localize

  AValue.Cast(GetArrayElType(TypeInfo)).ExtractRawData(
    GetReferenceToRawArrayElement(Index));
end;

class procedure TValue.Make(ABuffer: Pointer;
  ATypeInfo: PTypeInfo; var Result: TValue);
var
  inlineSize: Integer;

  function GetClassInfo(AClass: TClass): PTypeInfo;
  begin
    if AClass = nil then
      Exit(ATypeInfo);
    Result := AClass.ClassInfo;
  end;

begin
  Result.FTypeInfo := ATypeInfo;
  Result.FValueData := IValueData(Nop_Instance);
  InitData(Result);
  inlineSize := GetInlineSize(ATypeInfo);
  if inlineSize = 0 then
    // exit
  else if inlineSize > 0 then
  begin
    if ABuffer <> nil then
      Move(ABuffer^, Result.FAsUByte, inlineSize);

{$IFNDEF AUTOREFCOUNT}
    // make a better-educated guess about type-info when we can
    case ATypeInfo^.Kind of
      tkClass:
        if Result.FAsObject <> nil then
          Result.FTypeInfo := GetClassInfo(TObject(Result.FAsObject).ClassType);
    end;
{$ENDIF !AUTOREFCOUNT}
  end
  else if ATypeInfo^.Kind = tkUString then
    Result.FValueData := IValueData(TValueDataStringImpl.Create(ABuffer, @Str_VTable))
  else if ATypeInfo^.Kind = tkInterface then
    Result.FValueData := IValueData(TValueDataInterfaceImpl.Create(ABuffer, @Intf_VTable))
  else
    Result.FValueData := TValueDataImpl.Create(ABuffer, -inlineSize, ATypeInfo);
end;

class procedure TValue.Make(AValue: NativeInt; ATypeInfo: PTypeInfo;
  var Result: TValue);
var
  inlineSize: Integer;
begin
  Result.FTypeInfo := ATypeInfo;
  Result.FValueData := IValueData(Nop_Instance);
  InitData(Result);
  inlineSize := GetInlineSize(ATypeInfo);
  if inlineSize > 0 then
    Result.FAsPointer := Pointer(AValue)
  else
    Result.FValueData := TValueDataImpl.Create(@AValue, SizeOf(AValue), ATypeInfo);
end;

class procedure TValue.Make<T>(const Value: T; var Result: TValue);
begin
  Make(@Value, System.TypeInfo(T), Result);
end;

class procedure TValue.MakeWithoutCopy(ABuffer: Pointer; ATypeInfo: PTypeInfo; var Result: TValue; IsMoved: Boolean);
var
  inlineSize: Integer;
begin
  if not IsManaged(ATypeInfo) then
  begin
    Make(ABuffer, ATypeInfo, Result);
    Exit;
  end;
  Result.FTypeInfo := ATypeInfo;
  if ATypeInfo^.Kind = tkUString then
    Result.FValueData := IValueData(TValueDataStringImpl.CreateWithoutCopy(ABuffer, IsMoved, @Str_VTable))
  else if ATypeInfo^.Kind = tkInterface then
    Result.FValueData := IValueData(TValueDataInterfaceImpl.CreateWithoutCopy(ABuffer, IsMoved, @Intf_VTable))
  else
  begin
    inlineSize := GetInlineSize(ATypeInfo);
    Result.FValueData := TValueDataImpl.CreateWithoutCopy(ABuffer, -inlineSize, ATypeInfo, IsMoved);
  end;
  InitData(Result);
end;

procedure TValue.ExtractRawData(ABuffer: Pointer);
var
  inlineSize: Integer;
begin
  inlineSize := GetInlineSize(FTypeInfo);
  if inlineSize = 0 then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  if inlineSize > 0 then
    Move(FAsUByte, ABuffer^, inlineSize)
  else
    FValueData.ExtractRawData(ABuffer);
end;

procedure TValue.ExtractRawDataNoCopy(ABuffer: Pointer);
var
  inlineSize: Integer;
begin
  inlineSize := GetInlineSize(FTypeInfo);
  if inlineSize = 0 then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  if inlineSize > 0 then
    Move(FAsUByte, ABuffer^, inlineSize)
  else
    FValueData.ExtractRawDataNoCopy(ABuffer);
end;

function TValue.GetReferenceToRawData: Pointer;
var
  inlineSize: Integer;
begin
  inlineSize := GetInlineSize(FTypeInfo);
  if inlineSize = 0 then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  if inlineSize > 0 then
    Result := @FAsUByte
  else
    Result := FValueData.GetReferenceToRawData;
end;

function TValue.GetReferenceToRawArrayElement(Index: Integer): Pointer;
var
  elSize: Integer;
begin
  if TypeInfo <> nil then
  begin
    if TypeInfo^.Kind = tkArray then
    begin
      with TypeData^.ArrayData do
        elSize := Size div ElCount;
      Result := PByte(GetReferenceToRawData) + Index * elSize;
      Exit;
    end
    else if TypeInfo^.Kind = tkDynArray then
    begin
      elSize := TypeData^.elSize;
      Result := PByte(PPointer(GetReferenceToRawData)^) + Index * elSize;
      Exit;
    end
  end;

  raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValue.ToString: string;

  procedure DoClass(var AResult: string);
  begin
    AResult := Format('(%s @ %p)', [AsObject.ClassName, Pointer(AsObject)]); // do not localize
  end;

  procedure DoClassRef(var AResult: string);
  begin
    AResult := Format('(class ''%s'' @ %p)', [FAsClass.ClassName, Pointer(FAsClass)]); // do not localize
  end;

  procedure DoArray(const AKind: string; var AResult: string);
  begin
    AResult := Format('(%sarray [0..%d] of %s)', [AKind, GetArrayLength - 1, GetArrayElType(TypeInfo).Name]); // do not localize
  end;

begin
  if IsEmpty then
    Exit('(empty)'); // do not localize

  case FTypeInfo^.Kind of
    tkUnknown: Result := '(unknown)'; // do not localize
    tkInteger:
      case GetTypeData(FTypeInfo)^.OrdType of
        otSByte,
        otSWord,
        otSLong: Result := IntToStr(AsInt64);
        otUByte,
        otUWord,
        otULong: Result := UIntToStr(AsUInt64);
      end;
    tkChar: Result := string(AsType<UTF8Char>);
    tkEnumeration: Result := GetEnumName(FTypeInfo, FAsSLong);
    tkFloat:
      case GetTypeData(FTypeInfo)^.FloatType of
        ftSingle: Result := FloatToStr(FAsSingle);
        ftDouble:
        begin
          if FTypeInfo = System.TypeInfo(TDate) then
            Result := DateToStr(FAsDouble)
          else if FTypeInfo = System.TypeInfo(TTime) then
            Result := TimeToStr(FAsDouble)
          else if FTypeInfo = System.TypeInfo(TDateTime) then
            Result := DateTimeToStr(FAsDouble)
          else
            Result := FloatToStr(FAsDouble);
        end;
        ftExtended: Result := FloatToStr(FAsExtended);
        ftComp: Result := IntToStr(FAsSInt64);
        ftCurr: Result := CurrToStr(FAsCurr);
      end;
    tkString, tkLString, tkUString, tkWString: Result := AsString;
    tkSet:
      Result := SetToString(FTypeInfo, GetReferenceToRawData, True);
    tkClass:
{$IFDEF AUTOREFCOUNT}
      if FValueData = nil then
{$ELSE  AUTOREFCOUNT}
      if FAsObject = nil then
{$ENDIF AUTOREFCOUNT}
        Result := '(empty)' // do not localize
      else
        DoClass(Result);

    tkMethod: Result := Format('(method code=%p, data=%p)', [FAsMethod.Code, FAsMethod.Data]); // do not localize
    tkWChar: Result := AsType<WideChar>;
    tkVariant: Result := '(variant)'; // do not localize
    tkArray: DoArray('', Result);
    tkRecord, tkMRecord: Result := '(record)'; // do not localize
    tkProcedure: Result := Format('(procedure @ %p)', [Pointer(FAsPointer)]); // do not localize
    tkPointer: Result := Format('(pointer @ %p)', [Pointer(FAsPointer)]); // do not localize
    tkInterface: Result := Format('(interface @ %p)', [PPointer(FValueData.GetReferenceToRawData)^]); // do not localize
    tkInt64:
      with GetTypeData(FTypeInfo)^ do
        if MinInt64Value > MaxInt64Value then
          Result := UIntToStr(FAsUInt64)
        else
          Result := IntToStr(FAsSInt64);
    tkDynArray: DoArray('dynamic ', Result); // do not localize
    tkClassRef:
      if FAsClass = nil then
        Result := '(empty)' // do not localize
      else
        DoClassRef(Result);
  end;
end;

function ArrayOfConstToTValueArray(const Params: array of const): TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(Params));
  for I := Low(Result) to High(Result) do
    Result[I] := Params[I];
end;

function TValueArrayToArrayOfConst(const Params: array of TValue): TArray<TVarRec>;
var
  I: Integer;
begin
  SetLength(Result, Length(Params));
  for I := Low(Result) to High(Result) do
    Result[I] := Params[I].AsVarRec;
end;

type
  PListHelperCrack = ^TListHelperCrack;
  TListHelperCrack = record
  private
    FItems: Pointer;
    FCount: Integer;
    FTypeInfo: Pointer;
    [unsafe] FListObj: TObject;
  end;

function GetArrayValueFromTListHelperValue(const ACtx: TRttiContext;
  const AListHelperValue: TValue; out ACount: Integer): TValue;
var
  LpListHelper: PListHelperCrack;
begin
  if (AListHelperValue.TypeInfo <> TypeInfo(System.Generics.Collections.TListHelper)) or
     AListHelperValue.IsEmpty then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  LpListHelper := AListHelperValue.GetReferenceToRawData;
  TValue.Make(@LpListHelper^.FItems, LpListHelper^.FTypeInfo, Result);
  ACount := LpListHelper^.FCount;
end;

procedure SetTListHelperValueFromArrayValue(const ACtx: TRttiContext;
  var AListHelperValue: TValue; const AGetArrValFunc: TGetArrayValueFunc);
var
  LpListHelper: PListHelperCrack;
  LList: TObject;
  LArrType: TRttiType;
  LArrVal: TValue;
begin
  if (AListHelperValue.TypeInfo <> TypeInfo(System.Generics.Collections.TListHelper)) or
     AListHelperValue.IsEmpty or not Assigned(AGetArrValFunc) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  LpListHelper := AListHelperValue.GetReferenceToRawData;
  // Call FListHelper.FListObj.Clear
  LList := LpListHelper^.FListObj;
  ACtx.GetType(LList.ClassInfo).GetMethod('Clear').Invoke(LList, []); // do not localize
  // Get FListHelper.FTypeInfo
  LArrType := ACtx.GetType(LpListHelper^.FTypeInfo);
  // Get new array
  LArrVal := AGetArrValFunc(LArrType);
  // Set new array
  LpListHelper^.FItems := nil;
  DynArrayCopy(LpListHelper^.FItems, PPointer(LArrVal.GetReferenceToRawData)^, LArrType.Handle);
  LpListHelper^.FCount := LArrVal.GetArrayLength;
end;

// Type conversion

type
  TConvertFunc = function(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;

function ConvNone(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
begin
  AResult := ASource;
  Result := True;
end;

function ConvFail(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
begin
  Result := False;
end;

function ConvInt2Int(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: Integer;
begin
  // Careful about sign extension
  case GetTypeData(ASource.FTypeInfo)^.OrdType of
    otSByte: value := ASource.FAsSByte;
    otSWord: value := ASource.FAsSWord;
    otSLong: value := ASource.FAsSLong;
  else
    Cardinal(value) := ASource.FAsULong;
  end;
                              
  TValue.Make(@value, ATarget, AResult);
  Result := True;
end;

function ConvInt2Float(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: Int64;
begin
  case GetTypeData(ASource.FTypeInfo)^.OrdType of
    otSByte: value := ASource.FAsSByte;
    otSWord: value := ASource.FAsSWord;
    otSLong: value := ASource.FAsSLong;
  else
    value := ASource.FAsULong;
  end;

  case GetTypeData(ATarget)^.FloatType of
    ftSingle: AResult := TValue.From<Single>(value);
    ftDouble: AResult := TValue.From<Double>(value);
    ftExtended: AResult := TValue.From<Extended>(value);
    ftCurr: AResult := TValue.From<Currency>(value);
    ftComp: AResult := TValue.From<Comp>(value);
  end;
  Result := True;
end;

function ConvInt2Int64(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: Int64;
begin
  case GetTypeData(ASource.FTypeInfo)^.OrdType of
    otSByte: value := ASource.FAsSByte;
    otSWord: value := ASource.FAsSWord;
    otSLong: value := ASource.FAsSLong;
  else
    value := ASource.FAsULong;
  end;
  TValue.Make(@value, ATarget, AResult);
  Result := True;
end;

function ConvChar2Str(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: Byte;
  rstr: RawByteString;
begin
  value := ASource.FAsUByte;
  case ATarget^.Kind of
    tkChar: TValue.Make(NativeInt(value), ATarget, AResult);
    tkLString:
      begin
        SetAnsiString(@rstr, {$IFDEF NEXTGEN}PUTF8Char{$ELSE}PAnsiChar{$ENDIF}(@value), 1, ATarget^.TypeData.CodePage);
        TValue.Make(@rstr, ATarget, AResult);
      end;
{$IFDEF NEXTGEN}
{$ELSE !NEXTGEN}
    tkString: AResult := TValue.From<ShortString>(AnsiChar(value));
    tkWString: AResult := TValue.From<WideString>(WideString(AnsiChar(value)));
{$ENDIF NEXTGEN}
    tkUString: AResult := TValue.From<UnicodeString>(UnicodeString({$IFDEF NEXTGEN}UTF8Char{$ELSE}AnsiChar{$ENDIF}(value)));
  else
    Exit(False);
  end;
  Result := True;
end;

function ConvWChar2Str(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: WideChar;
  rstr: RawByteString;
begin
  value := WideChar(ASource.FAsUWord);
  case ATarget^.Kind of
    tkWChar: TValue.Make(NativeInt(value), ATarget, AResult);
    tkLString:
      begin
        SetAnsiString(@rstr, PWideChar(string(value)), 1, ATarget^.TypeData.CodePage);
        TValue.Make(@rstr, ATarget, AResult);
      end;
{$IFDEF NEXTGEN}
{$ELSE}
    tkString: AResult := TValue.From<ShortString>(ShortString(String(value)));
    tkWString: AResult := TValue.From<WideString>(value);
{$ENDIF !NEXTGEN}
    tkUString: AResult := TValue.From<UnicodeString>(value);
  else
    Exit(False);
  end;
  Result := True;
end;

function ConvEnum2Enum(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: Integer;
  enumbasetype: PTypeInfo;
begin
                                   
  value := ASource.FAsSLong;
  // Special case ByteBool, WordBool, LongBool because they are not enum subsets
  // but should be mutually convertible.
  if IsBoolType(ASource.FTypeInfo) and IsBoolType(ATarget) then
  begin
    Result := True;
    enumbasetype := GetEnumBaseType(ATarget);
    if not ((enumbasetype = System.TypeInfo(ByteBool)) or
      (enumbasetype = System.TypeInfo(WordBool)) or
      (enumbasetype = System.TypeInfo(LongBool))) then
      value := Ord(value <> 0)
    else if value <> 0 then
      value := -1;
    TValue.Make(NativeInt(value), ATarget, AResult)
  end
  else
  begin
    Result := GetEnumBaseType(ASource.FTypeInfo) = GetEnumBaseType(ATarget);
    if Result then
      TValue.Make(NativeInt(value), ATarget, AResult);
  end;
end;

function ConvFloat2Float(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
begin
  case GetTypeData(ASource.FTypeInfo)^.FloatType of
    ftSingle:
      case GetTypeData(ATarget)^.FloatType of
        ftSingle: AResult := TValue.From<Single>(ASource.FAsSingle);
        ftDouble: AResult := TValue.From<Double>(ASource.FAsSingle);
        ftExtended: AResult := TValue.From<Extended>(ASource.FAsSingle);
        ftComp: AResult := TValue.From<Comp>(ASource.FAsSingle);
        ftCurr: AResult := TValue.From<Currency>(ASource.FAsSingle);
      end;
    ftDouble:
      case GetTypeData(ATarget)^.FloatType of
        ftSingle: AResult := TValue.From<Single>(ASource.FAsDouble);
        ftDouble: AResult := TValue.From<Double>(ASource.FAsDouble);
        ftExtended: AResult := TValue.From<Extended>(ASource.FAsDouble);
        ftComp: AResult := TValue.From<Comp>(ASource.FAsDouble);
        ftCurr: AResult := TValue.From<Currency>(ASource.FAsDouble);
      end;
    ftExtended:
      case GetTypeData(ATarget)^.FloatType of
        ftSingle: AResult := TValue.From<Single>(ASource.FAsExtended);
        ftDouble: AResult := TValue.From<Double>(ASource.FAsExtended);
        ftExtended: AResult := TValue.From<Extended>(ASource.FAsExtended);
        ftComp: AResult := TValue.From<Comp>(ASource.FAsExtended);
        ftCurr: AResult := TValue.From<Currency>(ASource.FAsExtended);
      end;
    ftComp:
      case GetTypeData(ATarget)^.FloatType of
        ftSingle: AResult := TValue.From<Single>(ASource.FAsComp);
        ftDouble: AResult := TValue.From<Double>(ASource.FAsComp);
        ftExtended: AResult := TValue.From<Extended>(ASource.FAsComp);
        ftComp: AResult := TValue.From<Comp>(ASource.FAsComp);
        ftCurr: AResult := TValue.From<Currency>(ASource.FAsComp);
      end;
    ftCurr:
      case GetTypeData(ATarget)^.FloatType of
        ftSingle: AResult := TValue.From<Single>(ASource.FAsCurr);
        ftDouble: AResult := TValue.From<Double>(ASource.FAsCurr);
        ftExtended: AResult := TValue.From<Extended>(ASource.FAsCurr);
        ftComp: AResult := TValue.From<Comp>(ASource.FAsCurr);
        ftCurr: AResult := TValue.From<Currency>(ASource.FAsCurr);
      end;
    end;
  Result := True;
  // This is for TDateTime, TDate, TTime
  AResult.FTypeInfo := ATarget;
end;

function ConvStr2Str(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  value: UnicodeString;
  asRawByte: RawByteString;
{$IFNDEF NEXTGEN}
  asShort: ShortString;
  asWide: WideString;
{$ENDIF}
  codePage: Word;
begin
  case ASource.FTypeInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: value := UnicodeString(PShortString(ASource.GetReferenceToRawData)^);
    tkWString: value := UnicodeString(PWideString(ASource.GetReferenceToRawData)^);
{$ENDIF}
    tkLString: value := UnicodeString(PRawByteString(ASource.GetReferenceToRawData)^);
    tkUString: value := UnicodeString(PUnicodeString(ASource.GetReferenceToRawData)^);
  end;

  case ATarget^.Kind of
{$IFNDEF NEXTGEN}
    tkString:
    begin
      asRawByte := AnsiString(value);
      if Length(asRawByte) > GetTypeData(ATarget)^.MaxLength then
        Exit(False);
      asShort := asRawByte;
      TValue.Make(@asShort, ATarget, AResult);
    end;

    tkWString:
    begin
      asWide := value;
      TValue.Make(@asWide, ATarget, AResult);
    end;

    tkChar:
    begin
      asRawByte := AnsiString(value);
      if Length(asRawByte) <> 1 then
        Exit(False);
      TValue.Make(PAnsiChar(asRawByte), ATarget, AResult);
    end;
{$ELSE}
    tkChar:
    begin
      asRawByte := UTF8String(value);
      if Length(asRawByte) <> 1 then
        Exit(False);
      TValue.Make(PUTF8Char(asRawByte), ATarget, AResult);
    end;
{$ENDIF}

    tkLString:
    begin
      codePage := GetTypeData(ATarget)^.CodePage;
      if codePage = $FFFF then
        SetAnsiString(@asRawByte, PWideChar(value), Length(value), 0)
      else
        SetAnsiString(@asRawByte, PWideChar(value), Length(value), codePage);
      TValue.Make(@asRawByte, ATarget, AResult);
    end;

    tkUString: TValue.Make(@value, ATarget, AResult);

    tkWChar:
    begin
      if Length(value) <> 1 then
        Exit(False);
      TValue.Make(PChar(value), ATarget, AResult);
    end;
  end;
  Result := True;
end;

function ConvClass2Class(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  obj: TObject;
  cls: TClass;
begin
  obj := ASource.AsObject;
  cls := GetTypeData(ATarget)^.ClassType;
  Result := obj.InheritsFrom(cls);
  if Result then
    TValue.Make(IntPtr(obj), ATarget, AResult);
end;

function ConvClassRef2Self(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  src, dest: TClass;
begin
  ASource.ExtractRawData(@src);
  dest := GetTypeData(GetTypeData(ATarget)^.InstanceType^)^.ClassType;
  Result := (src = nil) or src.InheritsFrom(dest);
  if Result then
    TValue.Make(IntPtr(src), ATarget, AResult);
end;

function ConvClass2Intf(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  iid: TGUID;
  obj: Pointer;
{$IFDEF AUTOREFCOUNT}
  srcObj: TObject;
{$ENDIF AUTOREFCOUNT}
begin
  iid := GetTypeData(ATarget)^.Guid;
  if IsEqualGUID(iid, GUID_NULL) then
    Exit(False);
  // Future: consider instance -> interface conversion even without GUID
{$IFDEF AUTOREFCOUNT}
  ASource.FValueData.ExtractRawData(@srcObj);
  Result := srcObj.GetInterface(iid, obj);
{$ELSE  AUTOREFCOUNT}
  Result := TObject(ASource.FAsObject).GetInterface(iid, obj);
{$ENDIF AUTOREFCOUNT}
  if Result then
    TValue.MakeWithoutCopy(@obj, ATarget, AResult);
end;

function ConvIntf2Intf(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  src: Pointer;
  Parent: PTypeData;
begin
  Result := (ATarget = TypeInfo(IInterface)) or (ATarget = ASource.TypeInfo);
  if not Result then
  begin
    Parent := GetTypeData(ASource.TypeInfo);
    while (Parent <> nil) and (Parent.IntfParent <> nil) do
    begin
      if Parent.IntfParent^ = ATarget then
      begin
        Result := True;
        Break;
      end;
      Parent := GetTypeData(Parent.IntfParent^);
    end;
  end;

  if Result then
  begin
    ASource.ExtractRawDataNoCopy(@src);
    TValue.Make(@src, ATarget, AResult);
    Exit(True);
  end;
end;

function ConvInt642Int(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  signed: Int64;
  unsigned: UInt64;
begin
  if GetTypeData(ASource.FTypeInfo)^.MinInt64Value >
    GetTypeData(ASource.FTypeInfo)^.MaxInt64Value then
  begin
    unsigned := ASource.FAsUInt64;
    case GetTypeData(ATarget)^.OrdType of
      otSByte: TValue.Make(NativeInt(Int8(unsigned)), ATarget, AResult);
      otSWord: TValue.Make(NativeInt(Int16(unsigned)), ATarget, AResult);
      otSLong: TValue.Make(NativeInt(Int32(unsigned)), ATarget, AResult);
      otUByte: TValue.Make(NativeInt(UInt8(unsigned)), ATarget, AResult);
      otUWord: TValue.Make(NativeInt(UInt16(unsigned)), ATarget, AResult);
      otULong: TValue.Make(NativeInt(UInt32(unsigned)), ATarget, AResult);
    end;
  end
  else
  begin
    signed := ASource.FAsSInt64;
    case GetTypeData(ATarget)^.OrdType of
      otSByte: TValue.Make(NativeInt(Int8(signed)), ATarget, AResult);
      otSWord: TValue.Make(NativeInt(Int16(signed)), ATarget, AResult);
      otSLong: TValue.Make(NativeInt(Int32(signed)), ATarget, AResult);
      otUByte: TValue.Make(NativeInt(UInt8(signed)), ATarget, AResult);
      otUWord: TValue.Make(NativeInt(UInt16(signed)), ATarget, AResult);
      otULong: TValue.Make(NativeInt(UInt32(signed)), ATarget, AResult);
    end;
  end;
  Result := True;
end;

function ConvInt642Int64(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  signed: Int64;
  unsigned: UInt64;
begin
  if GetTypeData(ASource.FTypeInfo)^.MinInt64Value >
    GetTypeData(ASource.FTypeInfo)^.MaxInt64Value then
  begin
    unsigned := ASource.FAsUInt64;
    if GetTypeData(ATarget)^.MinInt64Value > GetTypeData(ATarget)^.MaxInt64Value then
      AResult := TValue.From<UInt64>(unsigned)
    else
      AResult := TValue.From<Int64>(unsigned);
  end
  else
  begin
    signed := ASource.FAsSInt64;
    if GetTypeData(ATarget)^.MinInt64Value > GetTypeData(ATarget)^.MaxInt64Value then
      AResult := TValue.From<UInt64>(signed)
    else
      AResult := TValue.From<Int64>(signed);
  end;
  Result := True;
end;

function ConvInt642Float(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  signed: Int64;
  unsigned: UInt64;
begin
  if GetTypeData(ASource.FTypeInfo)^.MinInt64Value >
    GetTypeData(ASource.FTypeInfo)^.MaxInt64Value then
  begin
    unsigned := ASource.FAsUInt64;
    case GetTypeData(ATarget)^.FloatType of
      ftSingle: AResult := TValue.From<Single>(unsigned);
      ftDouble: AResult := TValue.From<Double>(unsigned);
      ftExtended: AResult := TValue.From<Extended>(unsigned);
      ftComp: AResult := TValue.From<Comp>(unsigned);
      ftCurr: AResult := TValue.From<Currency>(unsigned);
    end;
  end
  else
  begin
    signed := ASource.FAsSInt64;
    case GetTypeData(ATarget)^.FloatType of
      ftSingle: AResult := TValue.From<Single>(signed);
      ftDouble: AResult := TValue.From<Double>(signed);
      ftExtended: AResult := TValue.From<Extended>(signed);
      ftComp: AResult := TValue.From<Comp>(signed);
      ftCurr: AResult := TValue.From<Currency>(signed);
    end;
  end;
  Result := True;
end;

function ConvFloat2Int(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  val: Int64;
begin
  Result := ASource.TypeData^.FloatType = ftComp;
  if not Result then
    Exit;
  val := ASource.FAsSInt64; // comp always signed
  if ATarget^.Kind = tkInteger then
  begin
    with GetTypeData(ATarget)^ do
      if MinValue > MaxValue then // unsigned
        Result := (val >= Cardinal(MinValue)) and (val <= Cardinal(MaxValue))
      else
        Result := (val >= MinValue) and (val <= MaxValue);
  end
  else
  begin
    Assert(ATarget^.Kind = tkInt64);
    with GetTypeData(ATarget)^ do
      if MinInt64Value > MaxInt64Value then // unsigned
        Result := (val >= 0) and (UInt64(val) >= UInt64(MinInt64Value))
          and (UInt64(val) <= UInt64(MaxInt64Value))
      else
        Result := (val >= MinInt64Value) and (val <= MaxInt64Value)
  end;

  if Result then
    TValue.Make(@val, ATarget, AResult);
end;

function ConvFromVariant(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  VarType: TVarType;
  v: Variant;
  TpInfo: PTypeInfo;
  DataPtr: Pointer;
  b: Boolean;
  e: Extended;
{$IFNDEF NEXTGEN}
  ss: ShortString;
{$ENDIF}
begin
  v := ASource.AsVariant;
  if VarIsNull(v) and NullStrictConvert then
    Exit(False);

  VarType := varEmpty;
  case ATarget^.Kind of
    tkInteger:
      case GetTypeData(ATarget)^.OrdType of
        otSByte: VarType := varShortInt;
        otSWord: VarType := varSmallint;
        otSLong: VarType := varInteger;
        otUByte: VarType := varByte;
        otUWord: VarType := varWord;
        otULong: VarType := varLongWord;
      end;
    tkEnumeration:
      if IsBoolType(ATarget) then
        VarType := varBoolean;
    tkFloat:
      if (ATarget = System.TypeInfo(TDateTime)) or
         (ATarget = System.TypeInfo(TDate)) or
         (ATarget = System.TypeInfo(TTime)) then
        VarType := varDate
      else
        case GetTypeData(ATarget)^.FloatType of
          ftSingle: VarType := varSingle;
          ftDouble: VarType := varDouble;
          ftExtended: VarType := varDouble;
          ftComp: VarType := varInt64;
          ftCurr: VarType := varCurrency;
        end;
{$IFNDEF NEXTGEN}
    tkString,
    tkLString: VarType := varString;
{$ENDIF}
    tkUString: VarType := varUString;
    tkWString: VarType := varOleStr;
    tkVariant: VarType := varVariant;
    tkInterface:
      if ATarget = TypeInfo(IDispatch) then
        VarType := varDispatch
      else
        VarType := varUnknown;
    tkInt64:
      if GetTypeData(ATarget)^.MinInt64Value > GetTypeData(ATarget)^.MaxInt64Value then
        VarType := varUInt64
      else
        VarType := varInt64;
    tkUnknown,
{$IFNDEF NEXTGEN}
    tkChar,
{$ENDIF}
    tkSet,
    tkClass,
    tkMethod,
    tkWChar,
    tkArray,
    tkRecord,
    tkMRecord,
    tkProcedure,
    tkPointer,
    tkDynArray,
    tkClassRef:
      ; // conversion is not supported from Variant to this type
  end;
  if VarType = varEmpty then
    Exit(False);
  try
    v := VarAsType(v, VarType);
  except
    Exit(False);
  end;

  TpInfo := nil;
  DataPtr := @TVarData(v).VBoolean;
  case TVarData(v).VType of
    varEmpty, varNull: AResult := TValue.FEmpty;
    varBoolean:
      begin
        b := TVarData(v).VBoolean = True;
        DataPtr := @b;
        TpInfo := TypeInfo(Boolean);
      end;
    varShortInt: TpInfo := TypeInfo(ShortInt);
    varSmallint: TpInfo := TypeInfo(SmallInt);
    varInteger: TpInfo := TypeInfo(Integer);
    varSingle: TpInfo := TypeInfo(Single);
    varDouble:
      if GetTypeData(ATarget)^.FloatType = ftExtended then
      begin
        e := Extended(TVarData(v).VDouble);
        DataPtr := @e;
        TpInfo := TypeInfo(Extended);
      end
      else
        TpInfo := TypeInfo(Double);
    varCurrency: TpInfo := TypeInfo(Currency);
    varDate: TpInfo := TypeInfo(TDateTime);
    varOleStr: TpInfo := TypeInfo(WideString);
    varDispatch: TpInfo := TypeInfo(IDispatch);
    varError: TpInfo := TypeInfo(HRESULT);
    varUnknown: TpInfo := TypeInfo(IInterface);
    varByte: TpInfo := TypeInfo(Byte);
    varWord: TpInfo := TypeInfo(Word);
    varUInt32: TpInfo := TypeInfo(UInt32);
    varInt64: TpInfo := TypeInfo(Int64);
    varUInt64: TpInfo := TypeInfo(UInt64);
{$IFNDEF NEXTGEN}
    varString:
      if ATarget^.Kind = tkString then
      begin
        ss := RawByteString(TVarData(v).VString);
        DataPtr := @ss;
        TpInfo := TypeInfo(ShortString);
      end
      else
        TpInfo := TypeInfo(RawByteString);
{$ENDIF !NEXTGEN}
    varUString: TpInfo := TypeInfo(UnicodeString);
  else
    Exit(False);
  end;
  if TpInfo <> nil then
    TValue.Make(DataPtr, TpInfo, AResult);
  Result := True;
end;

function Conv2Variant(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  v: Variant;
begin
  case ASource.Kind of
    tkInteger:
      begin
        case ASource.TypeData.OrdType of
          otSByte: v := ASource.FAsSByte;
          otUByte: v := ASource.FAsUByte;
          otSWord: v := ASource.FAsSWord;
          otUWord: v := ASource.FAsUWord;
          otSLong: v := ASource.FAsSLong;
          otULong: v := ASource.FAsULong;
        end;
      end;
{$IFDEF NEXTGEN}
    tkChar: v := UTF8String(ASource.AsType<UTF8Char>);
{$ELSE}
    tkChar: v := ASource.AsType<AnsiChar>;
{$ENDIF}
    tkFloat:
      if (ASource.TypeInfo = System.TypeInfo(TDateTime)) or
         (ASource.TypeInfo = System.TypeInfo(TDate)) or
         (ASource.TypeInfo = System.TypeInfo(TTime)) then
        v := TDateTime(ASource.FAsDouble)
      else
        case ASource.TypeData^.FloatType of
          ftSingle, ftDouble, ftExtended: v := ASource.AsExtended;
          ftComp: v := ASource.FAsComp;
          ftCurr: v := ASource.FAsCurr;
        end;
    tkString, tkLString, tkWString, tkUString:
      v := ASource.AsString;
    tkWChar:
      v := WideChar(ASource.FAsUWord);
    tkInt64:
      if ASource.TypeData^.MinInt64Value > ASource.TypeData^.MaxInt64Value then
        v := ASource.AsUInt64
      else
        v := ASource.AsInt64;
    tkEnumeration:
        if ASource.IsType<Boolean> then
          v := ASource.AsBoolean
        else
          v := ASource.AsOrdinal;
{$IFNDEF NEXTGEN}
    tkClass:
      v := NativeInt(ASource.AsObject);
{$ENDIF !NEXTGEN}
    tkInterface:
      v := ASource.AsInterface;
  else
      Exit(False);
  end;

  if ATarget = TypeInfo(OleVariant) then
    AResult := TValue.From<OleVariant>(v)
  else
    AResult := TValue.From<Variant>(v);
  Result := True;
end;

function ConvVariant2Variant(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
begin
  if ASource.TypeInfo = ATarget then
    AResult := ASource
  else if ATarget = TypeInfo(OleVariant) then
    AResult := TValue.From<OleVariant>(ASource.AsVariant)
  else
    AResult := TValue.From<Variant>(ASource.AsType<OleVariant>);
  Result := True;
end;

function ConvSet2Set(const ASource: TValue; ATarget: PTypeInfo; var AResult: TValue): Boolean;
var
  SrcMin, SrcMax,
  TrgMin, TrgMax: Integer;
begin
  Result := False;
                                                                       
  SrcMin := ASource.TypeData.CompType^.TypeData.MinValue;
  SrcMax := ASource.TypeData.CompType^.TypeData.MaxValue;
  TrgMin := ATarget.TypeData.CompType^.TypeData.MinValue;
  TrgMax := ATarget.TypeData.CompType^.TypeData.MaxValue;

  if (SrcMin = TrgMin) and (SrcMax = TrgMax) then
  begin
    TValue.Make(Asource.GetReferenceToRawData, ATarget, AResult);
    Result := true;
  end
end;

const
  Conversions: array[TTypeKind,TTypeKind] of TConvertFunc = ( // [source, target]
    // tkUnknown
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvNone, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkInteger
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvInt2Int, ConvFail, ConvFail, ConvInt2Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvInt2Int64, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvNone, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvChar2Str, ConvFail, ConvFail, ConvFail, ConvChar2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvChar2Str, ConvChar2Str, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvChar2Str, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkEnumeration
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvEnum2Enum, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkFloat
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFloat2Int, ConvFail, ConvFail, ConvFloat2Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFloat2Int, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvStr2Str, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvStr2Str, ConvFail, ConvFail, ConvFail, ConvStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvStr2Str, ConvStr2Str, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvStr2Str, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkSet
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvSet2Set, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkClass
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvClass2Class, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvClass2Intf, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkWChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvWChar2Str, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvWChar2Str, ConvFail, ConvFail, ConvFail, ConvNone,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvWChar2Str, ConvWChar2Str, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvWChar2Str, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkLString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvStr2Str, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvStr2Str, ConvFail, ConvFail, ConvFail, ConvStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvStr2Str, ConvStr2Str, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvStr2Str, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkWString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvStr2Str, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvStr2Str, ConvFail, ConvFail, ConvFail, ConvStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvStr2Str, ConvStr2Str, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvStr2Str, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFromVariant, ConvFromVariant, ConvFromVariant, ConvFromVariant,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFromVariant, ConvFail, ConvFail, ConvFail, ConvFromVariant,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFromVariant, ConvFromVariant, ConvVariant2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFromVariant, ConvFail, ConvFromVariant, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkInterface // TODO : (GUID search)
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvIntf2Intf, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkInt64
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvInt642Int, ConvFail, ConvFail, ConvInt642Float,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvInt642Int64, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkUString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvStr2Str, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvStr2Str, ConvFail, ConvFail, ConvFail, ConvStr2Str,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvStr2Str, ConvStr2Str, Conv2Variant, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvStr2Str, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkClassRef
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvClassRef2Self,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    ),
    // tkPointer
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvNone, ConvNone, ConvFail
    ),
    // tkProcedure
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvNone, ConvNone, ConvFail
    ),
    // tkMRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkPointer, tkProcedure, tkMRecord
      ConvFail, ConvFail, ConvFail
    )
  );

function TValue.TryCast(ATypeInfo: PTypeInfo; out AResult: TValue; const EmptyAsAnyType: Boolean): Boolean;
var
  inlineSize: Integer;
begin
  if EmptyAsAnyType and IsEmpty then
  begin
    // Empty value converts to all-zeros of the corresponding type, and always
    // succeeds. ATypeInfo being nil indicates a typeless nil.
    AResult := TValue.Empty;
    if ATypeInfo <> nil then
    begin
      AResult.FTypeInfo := ATypeInfo;
      if ATypeInfo^.Kind = tkUString then
        AResult.FValueData := IValueData(TValueDataStringImpl.Create(nil, @Str_VTable))
      else if ATypeInfo^.Kind = tkInterface then
        AResult.FValueData := IValueData(TValueDataInterfaceImpl.Create(nil, @Intf_VTable))
      else
      begin
        inlineSize := GetInlineSize(ATypeInfo);
        if inlineSize < 0 then
          AResult.FValueData := TValueDataImpl.CreateEmpty(-inlineSize, ATypeInfo);
      end;
    end;
    Exit(True);
  end;
  if not EmptyAsAnyType and (FTypeInfo = nil) then
    Exit(False);
  if FTypeInfo = ATypeInfo then
  begin
    AResult := Self;
    Exit(True);
  end;
  if ATypeInfo = nil then
    Exit(False);
  if ATypeInfo = System.TypeInfo(TValue) then
  begin
    TValue.Make(@Self, System.TypeInfo(TValue), AResult);
    Exit(True);
  end;
  Result := Conversions[FTypeInfo^.Kind, ATypeInfo^.Kind](Self, ATypeInfo, AResult);
end;

type
{$POINTERMATH ON}
  PVtablePtr = ^Pointer;
{$POINTERMATH OFF}
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

{ Binary Scanner }

function PeekI16(P: PByte): SmallInt; inline;
begin
  Result := PSmallInt(P)^;
end;

function PeekI32(P: PByte): Integer; inline;
begin
  Result := PInteger(P)^;
end;

function PeekI8(P: PByte): ShortInt; inline;
begin
  Result := PShortInt(P)^;
end;

function PeekPointer(P: PByte): Pointer; inline;
begin
  Result := PPointer(P)^;
end;

function PeekShortString(P: PByte): string; inline;
begin
{$IFNDEF NEXTGEN}
  Result := string(PShortString(P)^);
{$ELSE NEXTGEN}
  Result := string(MarshaledAString(P)^);
{$ENDIF !NEXTGEN}
end;

function PeekU16(P: PByte): Word; inline;
begin
  Result := PWord(P)^;
end;

function PeekU32(P: PByte): Cardinal; inline;
begin
  Result := PCardinal(P)^;
end;

function PeekU8(P: PByte): Byte; inline;
begin
  Result := PByte(P)^;
end;

function ReadI16(var P: PByte): SmallInt; inline;
begin
  Result := PeekI16(P);
  Inc(P, SizeOf(Result));
end;

function ReadI32(var P: PByte): Integer; inline;
begin
  Result := PeekI32(P);
  Inc(P, SizeOf(Result));
end;

function ReadI8(var P: PByte): ShortInt; inline;
begin
  Result := PeekI8(P);
  Inc(P, SizeOf(Result));
end;

function ReadPointer(var P: PByte): Pointer; inline;
begin
  Result := PeekPointer(P);
  Inc(P, SizeOf(Result));
end;

function ReadU16(var P: PByte): Word; inline;
begin
  Result := PeekU16(P);
  Inc(P, SizeOf(Result));
end;

function ReadU32(var P: PByte): Cardinal; inline;
begin
  Result := PeekU32(P);
  Inc(P, SizeOf(Result));
end;

function ReadU8(var P: PByte): Byte; inline;
begin
  Result := PeekU8(P);
  Inc(P, SizeOf(Result));
end;

function ReadShortString(var P: PByte): string;
var
  len: Integer;
begin
  Result := UTF8IdentToString(PShortString(P));
  len := ReadU8(P);
  Inc(P, len);
end;

procedure Skip(var P: PByte; ByteCount: Integer); inline;
begin
  Inc(P, ByteCount);
end;

// P points a length field of ShortString.
function SkipShortString(P: Pointer): PByte; inline;
begin
  Result := PByte(P) + PByte(P)^ + 1;
end;

function GetBitField(Value, Shift, Bits: Integer): Integer;
begin
  Result := (Value shr Shift) and ((1 shl Bits) - 1);
end;

{ TRttiPool }

function GetRttiClass(ATypeInfo: PTypeInfo): TRttiClass;
const
  Classes: array[TTypeKind] of TRttiClass = (
    // tkUnknown, tkInteger, tkChar, tkEnumeration,
    TRttiType, TRttiOrdinalType, TRttiOrdinalType, TRttiEnumerationType,
    // tkFloat, tkString, tkSet, tkClass,
    TRttiFloatType, TRttiStringType, TRttiSetType, TRttiInstanceType,
    // tkMethod, tkWChar, tkLString, tkWString,
    TRttiMethodType, TRttiOrdinalType, TRttiAnsiStringType, TRttiStringType,
    // tkVariant, tkArray, tkRecord, tkInterface,
    TRttiType, TRttiArrayType, TRttiRecordType, TRttiInterfaceType,
    // tkInt64, tkDynArray, tkUString, tkClassRef,
    TRttiInt64Type, TRttiDynamicArrayType, TRttiStringType, TRttiClassRefType,
    // tkPointer, tkProcedure, tkMRecord
    TRttiPointerType, TRttiProcedureType, TRttiRecordType
  );
begin
  Result := Classes[ATypeInfo^.Kind];
end;

type
  TRealPackage = class(TRttiPackage) // as opposed to orphan package
  private
    FTypeInfo: PPackageTypeInfo;
    FTypeToName: TDictionary<PTypeInfo,string>; // immutable, lazy ctor
    FNameToType: TDictionary<string,PTypeInfo>; // immutable, lazy ctor

    function GetName: string; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    procedure MakeTypeLookupTable;
    function GetNameFromType(AType: TRttiType): string; override;
  public
    destructor Destroy; override;
    function GetTypes: TArray<TRttiType>; override;
    function FindType(const AQualifiedName: string): TRttiType; override;
  end;

  { The Orphan package may be used for C++-defined types with type info but
    that don't have a Delphi-style containing package. }
  TOrphanPackage = class(TRttiPackage)
  public
    constructor Create; reintroduce;
    function GetName: string; override;
  public
    function GetTypes: TArray<TRttiType>; override;
    function FindType(const AQualifiedName: string): TRttiType; override;
  end;

  TRttiPool = class(TObject)
  private
    FLatestPackageList: TArray<TRttiPackage>;
    FOrphanPackage: TRttiPackage;
    FPackageVer: Integer;
    function GetPackageFor(AHandle: Pointer): TRttiPackage;
    function TypeOrNil(Value: PPTypeInfo): TRttiType;
    function GetPackageList: TArray<TRttiPackage>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetType(ATypeInfo: Pointer): TRttiType; overload;
    function GetType(AClass: TClass): TRttiType; overload;
    function GetTypes: TArray<TRttiType>;
    function GetPackages: TArray<TRttiPackage>;
    function FindType(const AQualifiedName: string): TRttiType;
  end;

const
{$IFDEF LINUX}
  PackageInfoFuncName = '_GetPackageInfoTable'; // do not localize
{$ELSE !LINUX}
  PackageInfoFuncName = '@GetPackageInfoTable'; // do not localize
{$ENDIF LINUX}

var
  { Reads and writes of the Pool variable during context creation / destruction
    are protected by PoolLock, but reads of the variable from other RTTI objects
    is not, because working with RTTI objects without at least one context being
    alive is an error. Keeping at least one context alive should keep the Pool
    variable valid.

    PoolRefCount is always protected by PoolLock. }
  Pool: TRttiPool;
  PoolRefCount: Integer;
  _PoolLock: TObject;

function PoolLock: TObject; inline;
begin
  if _PoolLock = nil then
    _PoolLock := TObject.Create;
  Result := _PoolLock
end;

{ TRttiPool }

constructor TRttiPool.Create;
begin
  FOrphanPackage := TOrphanPackage.Create;
end;

destructor TRttiPool.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FLatestPackageList) - 1 do
    FLatestPackageList[i].Free;
  FOrphanPackage.Free;
  inherited;
end;

function TRttiPool.GetPackageList: TArray<TRttiPackage>;
                                                                                   
  function DoUpdate: TArray<TRttiPackage>;
  var
    count: Integer;

    procedure MoveOrCreatePackage(Lib: PLibModule);
    var
      i: Integer;
      p: PByte;
      pkg: TRttiPackage;
    begin
      pkg := nil;

      // Try already-loaded packages
      for i := 0 to count - 1 do
      begin
        if Result[i] = nil then
          Continue;
        if Result[i].Handle = Lib^.Instance then
          Exit;
      end;

      // Try previously loaded packages
      for i := 0 to Length(FLatestPackageList) - 1 do
      begin
        if FLatestPackageList[i] = nil then
          Continue;
        if FLatestPackageList[i].Handle <> Lib^.Instance then
          Continue;

        // Found
        pkg := FLatestPackageList[i];
        FLatestPackageList[i] := nil;
        Break;
      end;

      if pkg = nil then
      begin
        p := Pointer(Lib);
        pkg := TRealPackage.Create(nil, nil, p);
      end;

      Result[count] := pkg;
      Inc(count);
    end;

  var
    lib: PLibModule;
    i, ver: Integer;
  begin
    TMonitor.Enter(PoolLock);
    try
      lib := LibModuleList;
      ver := GetModuleListVersion;

      if FPackageVer = ver then
        Exit(FLatestPackageList);

      count := 0;
      while lib <> nil do
      begin
        if (lib^.TypeInfo <> nil) or
            (GetProcAddress(lib^.Instance, PackageInfoFuncName) <> nil) then
          Inc(count);
        lib := lib^.Next;
      end;
      SetLength(Result, count);

      count := 0;

      lib := LibModuleList;
      while lib <> nil do
      begin
        if (lib^.TypeInfo <> nil) or
            (GetProcAddress(lib^.Instance, PackageInfoFuncName) <> nil) then
          MoveOrCreatePackage(lib);
        lib := lib^.Next;
      end;

      for i := 0 to Length(FLatestPackageList) - 1 do
        FLatestPackageList[i].Free; // items moved into Result are already nil here

      SetLength(Result, count); // truncate to real count
      if count > 0 then
        TArray.Sort<TRttiPackage>(Result,
          TComparer<TRttiPackage>.Construct(
            function(const L, R: TRttiPackage): Integer
            begin
              if UIntPtr(L.BaseAddress) < UIntPtr(R.BaseAddress) then
                Result := -1
              else if UIntPtr(L.BaseAddress) > UIntPtr(R.BaseAddress) then
                Result := 1
              else
                Result := 0;
            end));
      FLatestPackageList := Result;
      FPackageVer := ver;
    finally
      TMonitor.Exit(PoolLock);
    end;
  end;
begin
  // You're playing with fire if you try to race package addition and removal
  // in particular against RTTI operations. Please don't do that.
  Result := FLatestPackageList;
  if FPackageVer = GetModuleListVersion then
    Exit;
  Result := DoUpdate;
end;

function TRttiPool.GetPackageFor(AHandle: Pointer): TRttiPackage;
var
  start, finish, sel: Integer;
begin
  if (FPackageVer = GetModuleListVersion) and
     (Length(FLatestPackageList) = 1) and
     (UIntPtr(FLatestPackageList[0].BaseAddress) <= UIntPtr(AHandle)) then
    Exit(FLatestPackageList[0]);

  var pl: TArray<TRttiPackage> := GetPackageList;
  start := 0;
  finish := Length(pl);
  while finish - start > 1 do
  begin
    sel := (start + finish) div 2;
    if UIntPtr(pl[sel].BaseAddress) <= UIntPtr(AHandle) then
      start := sel
    else
      finish := sel;
  end;
  if start < Length(pl) then
    Result := pl[start]
  else
    Result := FOrphanPackage;
end;

function TRttiPool.GetType(ATypeInfo: Pointer): TRttiType;
var
  P: PByte;
begin
  if ATypeInfo = nil then
    Exit(nil);
  P := ATypeInfo;
  Result := TRttiType(GetPackageFor(P).ReadObject(TRttiType, nil, P));
end;

function TRttiPool.GetType(AClass: TClass): TRttiType;
var
  P: PByte;
begin
  if AClass.ClassInfo = nil then
    Exit(nil);
  P := PByte(AClass.ClassInfo);
  Result := TRttiType(GetPackageFor(P).ReadObject(TRttiType, nil, P));
end;

function TRttiPool.GetTypes: TArray<TRttiType>;
var
  i: Integer;
  pkgs: TArray<TRttiPackage>;
  types: TArray<TArray<TRttiType>>;
begin
  pkgs := GetPackageList;
  SetLength(types, Length(pkgs));
  for i := 0 to High(pkgs) do
    types[i] := pkgs[i].GetTypes;
  Result := TArray.Concat<TRttiType>(types);
end;

function TRttiPool.GetPackages: TArray<TRttiPackage>;
begin
  Result := GetPackageList;
end;

function TRttiPool.FindType(const AQualifiedName: string): TRttiType;
var
  r: TArray<TRttiPackage>;
  p: TRttiPackage;
begin
  r := GetPackageList;
  for p in r do
  begin
    Result := p.FindType(AQualifiedName);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TRttiPool.TypeOrNil(Value: PPTypeInfo): TRttiType;
begin
  if Value = nil then
    Exit(nil);
  Result := GetType(Value^);
end;

                                                                             
                                                                               
           
procedure DeletePackageFromPool(HInstance: HINST);
var
  pl: TArray<TRttiPackage>;
  i: Integer;
begin
  TMonitor.Enter(PoolLock);
  try
    if Pool <> nil then
    begin
      pl := Pool.FLatestPackageList;
      for i := 0 to High(pl) do
        if pl[i].Handle = HInstance then
        begin
          pl[i].Free;
          Delete(Pool.FLatestPackageList, i, 1);
          Break;
        end;
      Inc(ModListRemovals);
    end;
  finally
    TMonitor.Exit(PoolLock);
  end;
end;

type
  TPoolToken = class(TInterfacedObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPoolToken.Create;
begin
  TMonitor.Enter(PoolLock);
  try
    if Pool = nil then
    begin
      Pool := TRttiPool.Create;
      PoolRefCount := 1;
    end
    else
      Inc(PoolRefCount);
  finally
    TMonitor.Exit(PoolLock);
  end;
end;

destructor TPoolToken.Destroy;
begin
  TMonitor.Enter(PoolLock);
  try
    Dec(PoolRefCount);
    if PoolRefCount = 0 then
    begin
      Pool.Free;
      Pool := nil;
    end;
  finally
    TMonitor.Exit(PoolLock);
  end;
  inherited;
end;

type
  PInterface = ^IInterface;

{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
{$ELSE}
type
  PGlobalContext = ^TGlobalContext;
  TGlobalContext = record
    FGlobalContextCounter: NativeUInt;
    FGlobalContextToken: IInterface;
  end align 16;

var
  _GlobalContext: TGlobalContext;
{$ENDIF USE_MONITOR_FOR_GLOBALCONTEXT}

                                                                                            
function TRttiContext_UseContext: IInterface; forward;

procedure EnsurePoolToken(TokenRef: PInterface);
var
  sample: Pointer;

  procedure DoCreate;
  var
    tok: IInterface;
  begin
    tok := TRttiContext_UseContext;
    if tok = nil then
      tok := TPoolToken.Create;
    if AtomicCmpExchange(PPointer(TokenRef)^, Pointer(tok), sample) = sample then
    begin
      // We won the race to initialize the TokenRef location, so
      // zero-out interface reference without decrementing reference count.
      PPointer(@tok)^ := nil;
    end;
  end;

begin
  sample := PPointer(TokenRef)^;
  if sample <> nil then
    Exit;
  DoCreate;
end;

{ Attribute Construction }

function ConstructAttributes(P: PByte): TArray<TCustomAttribute>;

  function ParseArgs(AttrType: TRttiInstanceType; Data: PByte; Len: Integer;
      const Params: TArray<TRttiParameter>): TArray<TValue>;

    function ReadUtf8Arg: UTF8String;
    var
      resLen: Integer;
    begin
      resLen := ReadU16(Data);
      SetString(Result, PUTF8Char(Data), resLen);
      Inc(Data, resLen);
      Dec(Len, resLen + SizeOf(Word));
    end;

{$IFNDEF NEXTGEN}
    function ReadShortString: ShortString;
    begin
      // *not* in UTF8 format
      Result[0] := AnsiChar(ReadU8(Data));
      Move(Data^, Result[1], Byte(Result[0]));
      Inc(Data, Byte(Result[0]));
      Dec(Len, Byte(Result[0]) + 1);
    end;
{$ENDIF !NEXTGEN}

  var
    i: Integer;
    size: Integer;
    p: Pointer;
  begin
    SetLength(Result, Length(Params));
    for i := 0 to Length(Params) - 1 do
    begin
      case Params[i].ParamType.TypeKind of
        tkPointer:
        begin
          p := PPointer(Data)^;
          if p = nil then
            Result[i] := TValue.Empty
          else
            if Params[i].ParamType.Handle = TypeInfo(PTypeInfo) then
              TValue.Make(IntPtr(PPTypeInfo(p)^), Params[i].ParamType.Handle, Result[i])
            else
              TValue.Make(@p, Params[i].ParamType.Handle, Result[i]);
          Dec(len, SizeOf(Pointer));
          Inc(Data, SizeOf(Pointer));
        end;

        tkClassRef:
        begin
          p := PPointer(Data)^;
          if p = nil then
            Result[i] := TValue.Empty
          else
          begin
            Assert(PPTypeInfo(p)^.Kind = tkClass);
            Result[i] := TValue(GetTypeData(PPTypeInfo(p)^)^.ClassType).Cast(Params[i].ParamType.Handle);
          end;
          Dec(len, SizeOf(Pointer));
          Inc(Data, SizeOf(Pointer));
        end;

        tkClass: // expected: TRttiType etc., accepting a TypeInfo reference.
        begin
          p := PPointer(Data)^;
          Result[i] := TValue(Pool.TypeOrNil(p)).Cast(Params[i].ParamType.Handle);
          Dec(len, SizeOf(Pointer));
          Inc(Data, SizeOf(Pointer));
        end;

{$IFNDEF NEXTGEN}
        tkString:
          Result[i] := TValue.From<ShortString>(ReadShortString).Cast(Params[i].ParamType.Handle);

        tkWString,
{$ENDIF !NEXTGEN}
        tkLString, tkUString:
          Result[i] := TValue.From<UTF8String>(ReadUtf8Arg).Cast(Params[i].ParamType.Handle);
      else
          size := Params[i].ParamType.TypeSize;
          Dec(len, size);
          TValue.Make(Data, Params[i].ParamType.Handle, Result[i]);
          Inc(Data, size);
        end;
    end;
    Assert(Len = 0);
  end;

  function FindCtor(AttrType: TRttiInstanceType; CtorAddr: Pointer): TRttiMethod;
  type
    PPPointer = ^PPointer;
  var
  {$IFDEF MSWINDOWS}
    p: PByte;
  {$ENDIF}
    Method: TRttiMethod;
    imp: Pointer;
  begin
    Result := nil;

    for Method in AttrType.GetMethods do
      if Method.CodeAddress = CtorAddr then
        Exit(Method);
                                             
    {$IF defined(MACOS) or defined(ANDROID) or defined(LINUX)}
    // no implementation yet for packages, unfortunately
    Exit;
    {$ENDIF MACOS}

    {$IFDEF MSWINDOWS}
    p := CtorAddr;
    // expect a package (i.e. DLL) import
    Assert(p^ = $FF);
    Inc(p);
    Assert(p^ = $25);
    Inc(p);
    {$IFDEF CPUX64}
    // $FF $25 => indirect jump RIP-relative m32
    imp := PPointer((p + 4) + PInteger(p)^)^;
    {$ENDIF CPUX64}
    {$IFDEF CPUX86}
    // $FF $25 => indirect jump m32
    imp := PPPointer(p)^^;
    {$ENDIF CPUX86}
    {$ELSE}
    imp := nil;
    {$ENDIF MSWINDOWS}

    for Method in attrType.GetMethods do
      if Method.CodeAddress = imp then
        Exit(Method);
  end;

  function ConstructAttribute(var P: PByte): TCustomAttribute;
  var
    attrType: TRttiInstanceType;
    paramLen: Word;
    paramData: Pointer;
    ctorAddr: Pointer;
    ctor: TRttiMethod;
  begin
    attrType := Pool.GetType(DerefPointer(ReadPointer(P))) as TRttiInstanceType;
    ctorAddr := ReadPointer(P);
    paramLen := ReadU16(P);
    paramData := P;
    Inc(P, paramLen);

    ctor := FindCtor(attrType, ctorAddr);
    if ctor = nil then
      Result := nil
    else
      Result := ctor.Invoke(attrType.MetaclassType,
        ParseArgs(attrType, paramData, paramLen, ctor.GetParameters)).AsType<TCustomAttribute>;
  end;

var
  list: TList<TCustomAttribute>;
  attr: TCustomAttribute;
  len: Integer;
  finish: PByte;
begin
  len := ReadU16(P);
  Dec(len, SizeOf(Word));
  if len = 0 then
    Exit(nil);
  finish := P + len;

  list := TList<TCustomAttribute>.Create;
  try
    while P < finish do
    begin
      attr := ConstructAttribute(P);
      if attr <> nil then
        list.Add(attr);
    end;
    Result := list.ToArray;
  finally
    list.Free;
  end;
end;

{ Lazy-instantiation of custom attributes }

function LazyLoadAttributes(var P: PByte): TFunc<TArray<TCustomAttribute>>;
  function MakeClosure(data: PByte): TFunc<TArray<TCustomAttribute>>;
  var
    // if data is nil, then "value" is valid (loaded); this is how the
    // closure avoids constructing attributes twice, on second and subsequent
    // calls
    value: TArray<TCustomAttribute>;
    finalizer: IFinalizer; // destroys CAs when result goes out of scope
  begin
    Result := function: TArray<TCustomAttribute>
    var
      ca: TCustomAttribute;
    begin
      if data = nil then
        Exit(value);

      TMonitor.Enter(PoolLock);
      try
        if data = nil then
          Exit(value);

        if PWord(data)^ = 2 then // should never be reached, unless image data modified
        begin
          value := nil;
          data := nil;
          Exit(nil);
        end;

        finalizer := TFinalizer.Create;
        value := ConstructAttributes(data);
        for ca in value do
          finalizer.Add(ca);
        data := nil;
        Result := value;
      finally
        TMonitor.Exit(PoolLock);
      end;
    end;
  end;

var
  size: Integer;
  data: PByte;
begin
  data := P;
  size := PWord(data)^;
  Inc(P, size);
  if size = 2 then // avoid creating closure when not needed
    Exit(nil);

  Result := MakeClosure(data);
end;

{ TRttiContext }

{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
var
  _GlobalContextTokenLock: TObject;

function GCTokenLock: TObject; inline;
begin
  if _GlobalContextTokenLock = nil then
    _GlobalContextTokenLock := TObject.Create;
  Result := _GlobalContextTokenLock
end;
{$ENDIF}

class function TRttiContext.Create: TRttiContext;
begin
  EnsurePoolToken(@Result.FContextToken);
end;

procedure TRttiContext.Free;
begin
  FContextToken := nil;
end;

function TRttiContext.GetType(ATypeInfo: Pointer): TRttiType;
begin
  EnsurePoolToken(@FContextToken);
  Result := Pool.GetType(ATypeInfo);
end;

class procedure TRttiContext.DropContext;
{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
begin
  TMonitor.Enter(GCTokenLock);
  try
    Dec(FGlobalContextCounter);
    if FGlobalContextCounter = 0 then
      FGlobalContextToken := nil;
  finally
    TMonitor.Exit(GCTokenLock);
  end;
end;
{$ELSE}
{$IF Defined(CPU32BITS)}
var
  CurrentContext: UInt64;
begin
  if AtomicDecrement(_GlobalContext.FGlobalContextCounter) = 0 then
  begin
    CurrentContext := PUInt64(@_GlobalContext)^;
    if PGlobalContext(@CurrentContext)^.FGlobalContextCounter = 0 then
    begin
      if AtomicCmpExchange(PUInt64(@_GlobalContext)^, 0, CurrentContext) = CurrentContext then
        if CurrentContext <> 0 then
          PGlobalContext(@CurrentContext)^.FGlobalContextToken := nil;
    end
  end;
end;
{$ELSEIF Defined(CPU64BITS)}
type
  PUInt128 = ^UInt128;
  UInt128 = record
    Lo, Hi: UInt64;
  end align 16;
var
  CurrentContext: UInt128;
begin
  if AtomicDecrement(_GlobalContext.FGlobalContextCounter) = 0 then
  begin
    CurrentContext := PUInt128(@_GlobalContext)^;
    if PGlobalContext(@CurrentContext)^.FGlobalContextCounter = 0 then
    begin
      if AtomicCmpExchange128(PUInt128(@_GlobalContext)^, 0, 0, CurrentContext) then
        if (CurrentContext.Lo <> 0) or (CurrentContext.Hi <> 0) then
          PGlobalContext(@CurrentContext)^.FGlobalContextToken := nil;
    end
  end;
end;
{$ENDIF}
{$ENDIF USE_MONITOR_FOR_GLOBALCONTEXT}

function TRttiContext.FindType(const AQualifiedName: string): TRttiType;
begin
  EnsurePoolToken(@FContextToken);
  Result := Pool.FindType(AQualifiedName);
end;

function TRttiContext.GetType(AClass: TClass): TRttiType;
begin
  EnsurePoolToken(@FContextToken);
  Result := Pool.GetType(AClass);
end;

function TRttiContext.GetTypes: TArray<TRttiType>;
begin
  EnsurePoolToken(@FContextToken);
  Result := Pool.GetTypes;
end;

class procedure TRttiContext.KeepContext;
{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
begin
  TMonitor.Enter(GCTokenLock);
  try
    EnsurePoolToken(@FGlobalContextToken);
    Inc(FGlobalContextCounter);
  finally
    TMonitor.Exit(GCTokenLock);
  end
end;
{$ELSE}
begin
  AtomicIncrement(_GlobalContext.FGlobalContextCounter);
  EnsurePoolToken(@_GlobalContext.FGlobalContextToken);
end;
{$ENDIF}

function TRttiContext.GetPackages: TArray<TRttiPackage>;
begin
  EnsurePoolToken(@FContextToken);
  Result := Pool.GetPackages;
end;

function TRttiContext_UseContext: IInterface;
{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
begin
  TMonitor.Enter(GCTokenLock);
  try
    Inc(TRttiContext.FGlobalContextCounter);
    Result := TRttiContext.FGlobalContextToken;
  finally
    TMonitor.Exit(GCTokenLock);
  end;
  TRttiContext.DropContext;
end;
{$ELSE}
begin
  AtomicIncrement(_GlobalContext.FGlobalContextCounter);
  Result := _GlobalContext.FGlobalContextToken;
  TRttiContext.DropContext;
end;
{$ENDIF}

{ TRttiPackage }

destructor TRttiPackage.Destroy;
begin
  FLock.Free;
  FreeAndNil(FHandleToObject);
  inherited;
end;

function TRttiPackage.GetHandle: HINST;
begin
  Result := HINST(inherited Handle);
end;

function TRttiPackage.GetNameFromType(AType: TRttiType): string;
begin
  Result := '';
end;

function TRttiPackage.ReadObject(ARttiClass: TRttiClass; AParent: TRttiObject;
  var P: PByte): TRttiObject;
var
  start: PByte;
begin
  TMonitor.Enter(FLock);
  try
    if FHandleToObject.TryGetValue(P, Result) then
    begin
      Inc(P, Result.RttiDataSize);
      Exit;
    end;
    if ARttiClass = TRttiType then
      ARttiClass := GetRttiClass(PTypeInfo(P));
    start := P;
    Result := ARttiClass.Create(Self, AParent, P);
    Result.FRttiDataSize := P - start;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TRttiPackage.ReadObjectPointer(ARttiClass: TRttiClass; AParent: TRttiObject; P: Pointer): TRttiObject;
var
  pb: PByte;
begin
  pb := P;
  Result := ReadObject(ARttiClass, AParent, pb);
end;

{ TRttiObject }

constructor TRttiObject.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  FParent := AParent;
  FHandle := P;
  FPackage := APackage;
  if APackage <> nil then // if we are a package, it will be nil
    APackage.FHandleToObject.Add(P, Self); // we are created under package lock
end;

destructor TRttiObject.Destroy;
begin
  if (FPackage <> nil) and (FPackage.FHandleToObject <> nil) then
  begin
    TMonitor.Enter(Package.FLock);
    try
      FPackage.FHandleToObject.ExtractPair(FHandle);
    finally
      TMonitor.Exit(Package.FLock);
    end;
  end;
  inherited;
end;

function TRttiObject.GetAttributes: TArray<TCustomAttribute>;
begin
  if not Assigned(FAttributeGetter) then
    Exit(nil);
  Result := FAttributeGetter;
end;

function TRttiObject.GetAttribute(AAttrClass: TCustomAttributeClass): TCustomAttribute;
var
  LAttr: TCustomAttribute;
begin
  for LAttr in GetAttributes do
    if LAttr is AAttrClass then
      Exit(LAttr);
  Result := nil;
end;

function TRttiObject.HasAttribute(AAttrClass: TCustomAttributeClass): Boolean;
begin
  Result := GetAttribute(AAttrClass) <> nil;
end;

function TRttiObject.GetAttribute<T>: T;
begin
  Result := T(GetAttribute(T));
end;

function TRttiObject.HasAttribute<T>: Boolean;
begin
  Result := HasAttribute(T);
end;

{ TRttiNamedObject }

function TRttiNamedObject.HasName(const AName: string): Boolean;
begin
  Result := SameText(AName, Name);
end;

{ TRttiType }

constructor TRttiType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  if TypeKind = tkVariant then
  begin
    P := @TypeData^.AttrData;
    FAttributeGetter := LazyLoadAttributes(P);
  end;
end;

function TRttiType.GetAsInstance: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
end;

function TRttiType.GetAsOrdinal: TRttiOrdinalType;
begin
  Result := Self as TRttiOrdinalType;
end;

function TRttiType.GetAsRecord: TRttiRecordType;
begin
  Result := Self as TRttiRecordType;
end;

function TRttiType.GetAsSet: TRttiSetType;
begin
  Result := Self as TRttiSetType;
end;

function TRttiType.GetNamedObject<T>(const AName: string; const AGetListFunc: TGetListFunc<T>): T;
var
  tp: TRttiType;
  list: TArray<T>;
  Obj: T;
begin
  tp := Self;
  while tp <> nil do
  begin
    list := AGetListFunc(tp);
    for Obj in list do
      if Obj.HasName(AName) then
        Exit(Obj);
    tp := tp.BaseType;
  end;
  Result := nil;
end;

function TRttiType.GetNamedObjects<T>(const AName: string; const AGetListFunc: TGetListFunc<T>): TArray<T>;
var
  tp: TRttiType;
  list: TArray<T>;
  Obj: T;
  I, Capacity: Integer;
begin
  Result := nil;
  Capacity := 0;
  I := 0;
  tp := Self;
  while tp <> nil do
  begin
    list := AGetListFunc(tp);
    for Obj in list do
      if Obj.HasName(AName) then
      begin
        if I >= Capacity then
        begin
          Capacity := GrowCollection(Capacity, I + 1);
          SetLength(Result, Capacity);
        end;
        Result[I] := Obj;
        Inc(I);
      end;
    tp := tp.BaseType;
  end;
  SetLength(Result, I);
end;

function TRttiType.GetObjectList<T>(const AGetListFunc: TGetListFunc<T>): TArray<T>;
var
  flat: TArray<TArray<T>>;
  tp: TRttiType;
  depth: Integer;
begin
  tp := Self;
  depth := 0;
  while tp <> nil do
  begin
    Inc(depth);
    tp := tp.BaseType;
  end;

  SetLength(flat, depth);
  tp := Self;
  depth := 0;
  while tp <> nil do
  begin
    flat[depth] := AGetListFunc(tp);
    Inc(depth);
    tp := tp.BaseType;
  end;

  Result := TArray.Concat<T>(flat);
end;

function _GetDeclaredFields(AType: TRttiType): TArray<TRttiField>;
begin
  Result := AType.GetDeclaredFields;
end;

function TRttiType.GetField(const AName: string): TRttiField;
begin
  Result := GetNamedObject<TRttiField>(AName, _GetDeclaredFields);
end;

function TRttiType.GetFields: TArray<TRttiField>;
begin
  Result := GetObjectList<TRttiField>(_GetDeclaredFields);
end;

function TRttiType.GetHandle: PTypeInfo;
begin
  Result := PTypeInfo(inherited Handle);
end;

function _GetDeclaredIndexedProperties(AType: TRttiType): TArray<TRttiIndexedProperty>;
begin
  Result := AType.GetDeclaredIndexedProperties;
end;

function TRttiType.GetIndexedProperties: TArray<TRttiIndexedProperty>;
begin
  Result := GetObjectList<TRttiIndexedProperty>(_GetDeclaredIndexedProperties);
end;

function TRttiType.GetIndexedProperty(const AName: string): TRttiIndexedProperty;
begin
  Result := GetNamedObject<TRttiIndexedProperty>(AName, _GetDeclaredIndexedProperties);
end;

function TRttiType.GetIsInstance: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiType.GetIsManaged: Boolean;
begin
  Result := System.Rtti.IsManaged(Handle);
end;

function TRttiType.GetIsOrdinal: Boolean;
begin
  Result := Self is TRttiOrdinalType;
end;

function TRttiType.GetIsRecord: Boolean;
begin
  Result := Self is TRttiRecordType;
end;

function TRttiType.GetIsSet: Boolean;
begin
  Result := Self is TRttiSetType;
end;

function TRttiType.GetIsHFA: Boolean;
begin
  Result := False;
end;

function TRttiType.GetHFAElementType: TRttiFloatType;
begin
  Result := nil;
end;

function TRttiType.GetHFAElementCount: Integer;
begin
  Result := 0;
end;

function _GetDeclaredMethods(AType: TRttiType): TArray<TRttiMethod>;
begin
  Result := AType.GetDeclaredMethods;
end;

function TRttiType.GetMethod(const AName: string): TRttiMethod;
begin
  Result := GetNamedObject<TRttiMethod>(AName, _GetDeclaredMethods);
end;

function TRttiType.GetMethods(const AName: string): TArray<TRttiMethod>;
begin
  Result := GetNamedObjects<TRttiMethod>(AName, _GetDeclaredMethods);
end;

function TRttiType.GetMethods: TArray<TRttiMethod>;
begin
  Result := GetObjectList<TRttiMethod>(_GetDeclaredMethods);
end;

function TRttiType.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiType.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

function _GetDeclaredProperties(AType: TRttiType): TArray<TRttiProperty>;
begin
  Result := AType.GetDeclaredProperties;
end;

function TRttiType.GetProperties: TArray<TRttiProperty>;
begin
  Result := GetObjectList<TRttiProperty>(_GetDeclaredProperties);
end;

function TRttiType.GetProperty(const AName: string): TRttiProperty;
begin
  Result := GetNamedObject<TRttiProperty>(AName, _GetDeclaredProperties);
end;

function TRttiType.GetDeclaredMethods: TArray<TRttiMethod>;
begin
  Exit(nil);
end;

function TRttiType.GetDeclaredProperties: TArray<TRttiProperty>;
begin
  Exit(nil);
end;

function TRttiType.GetDeclaredFields: TArray<TRttiField>;
begin
  Exit(nil);
end;

function TRttiType.GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>;
begin
  Exit(nil);
end;

function TRttiType.GetTypeData: PTypeData;
begin
  Result := System.TypInfo.GetTypeData(Handle);
end;

function TRttiType.GetTypeKind: TTypeKind;
begin
  Result := PTypeInfo(Handle)^.Kind;
end;

function TRttiType.GetTypeSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

function TRttiType.ToString: string;
begin
  Result := Name;
end;

function TRttiType.GetQualifiedName: string;
begin
  Result := Package.GetNameFromType(Self);
  if Result = '' then
    raise ENonPublicType.CreateResFmt(@SNonPublicType, [Name]);
end;

function TRttiType.GetBaseType: TRttiType;
begin
  Result := nil;
end;

function TRttiType.GetIsPublicType: Boolean;
begin
  Result := Package.GetNameFromType(Self) <> '';
end;

{ TRttiInstanceMethodClassic }

type
  TRttiInstanceMethodClassic = class(TRttiMethod)
  private
    FTail: PVmtMethodEntryTail;
    FReturnTypeInfo: PTypeInfo;
    FParams: TArray<TRttiParameter>;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;

    procedure CheckExtended;

    function GetMethodKind: TMethodKind; override;
    function GetCallingConvention: TCallConv; override;
    function GetReturnType: TRttiType; override;
    function GetHasExtendedInfo: Boolean; override;

    function GetName: string; override;
    function GetHandle: PVmtMethodEntry; inline;
    function GetTailHandle: PVmtMethodEntryTail;
    function GetCodeAddress: Pointer; override;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; override;
  public
    function HasName(const AName: string): Boolean; override;
    function GetParameters: TArray<TRttiParameter>; override;

    property Handle: PVmtMethodEntry read GetHandle;
    property TailHandle: PVmtMethodEntryTail read GetTailHandle;
    property ReturnTypeInfo: PTypeInfo read FReturnTypeInfo;
  end;

  TRttiInstMethParameter = class(TRttiParameter)
  private
    FName: string;
    FFlags: TParamFlags;
    FParamType: TRttiType;
    FLocation: Word;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetName: string; override;
    function GetFlags: TParamFlags; override;
    function GetParamType: TRttiType; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Location: Word read FLocation;
  end;

type
  TSpecialMethod = (
    smConstructor,
    smDestructor,
    smOperatorOverload);

const
  mfClassMethod = 1 shl 0;
  mfHasSelf = 1 shl 1;

  // Special methods reinterpret first two bits as TSpecialMethod.
  mfSpecial = 1 shl 2;
  mfSpecialShift = 0;
  mfSpecialBits = 2;

  // 2 bits for first 4 values of TDispatchKind
  mfDispatchKindShift = 3;
  mfDispatchKindBits = 2;

  // 2 bits for visibility
  mfVisibilityShift = 5;
  mfVisibilityBits = 2;

  mfAbstract = 1 shl 7;

{ TRttiInstanceMethodClassic }

function TRttiInstanceMethodClassic.GetHandle: PVmtMethodEntry;
begin
  Result := inherited Handle;
end;

constructor TRttiInstanceMethodClassic.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);
var
  methEnd: PByte;
  i: Integer;
  LParent: TRttiObject;
begin
  LParent := AParent;
  if LParent is TRttiMethod then
    LParent := LParent.Parent;
  inherited Create(APackage, LParent, P);
  methEnd := p + Handle^.Len;
  p := PByte( Handle.Tail );
  if methEnd = p then
    Exit;
  Assert(methEnd > p);
  FTail := Pointer(p);

  Assert(FTail^.Version = 3);

  // Avoid lock priority problem by lazy-loading type
  FReturnTypeInfo := DerefPointer(FTail^.ResultType);
  SetLength(FParams, FTail^.ParamCount); // chopping of 'Self'

  LParent := Self;
  if AParent is TRttiMethod then
    LParent := AParent;
  p := PByte(@FTail^.ParamCount) + 1;
  for i := 0 to High(FParams) do // may include Self if not static
    FParams[i] := APackage.ReadObject(TRttiInstMethParameter, LParent, p) as TRttiInstMethParameter;
  // chop off result parameter - we infer it
  if (Length(FParams) > 0) and (pfResult in FParams[Length(FParams) - 1].Flags) then
    SetLength(FParams, Length(FParams) - 1);

  FAttributeGetter := LazyLoadAttributes(p);
end;

function TRttiInstanceMethodClassic.GetParameters: TArray<TRttiParameter>;
begin
  // Assume 'Self' is included
  // However, we might not have full info.
  Result := Copy(FParams, 1, Length(FParams) - 1);
end;

function TRttiInstanceMethodClassic.DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  code: Pointer;
  argCount: Integer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
  cls: TClass;
  isCons, isDest, isStat, isClas: Boolean;
  callConv: TCallConv;
begin
  GetCommonInvokeParams(isCons, isDest, isStat, isClas, callConv);

  parList := GetParameters;
  argCount := Length(Args);
  if argCount <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  if isCons then
    Inc(argCount);
  if not isStat then
    Inc(argCount);

  SetLength(argList, argCount);
  currArg := 0;
  cls := nil;

  // Classic = $METHODINFO ON. Static methods, constructors, destructors and
  // class methods shouldn't show up here.
  if not isStat then
  begin
    if isClas then
    begin
      cls := Instance.AsClass;
      PushSelfFirst(callConv, argList, currArg, cls);
    end
    else
    begin
      cls := Instance.AsObject.ClassType;
      PushSelfFirst(callConv, argList, currArg, Instance.AsObject);
    end;
  end;

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], callConv);
    Inc(currArg);
  end;

  if not isStat then
  begin
    if isClas then
      PushSelfLast(callConv, argList, currArg, cls)
    else
      PushSelfLast(callConv, argList, currArg, Instance.AsObject);
  end;

  case DispatchKind of
    dkVtable: code := PVtable(cls)^[VirtualIndex];
  else
    code := CodeAddress;
  end;

  CheckCodeAddress(code);

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(code, argList, callConv, ReturnType.Handle, IsStatic)
  else if isCons then
    Result := System.Rtti.Invoke(code, argList, callConv, parList[0].ParamType.Handle, IsStatic, True)
  else
    Result := System.Rtti.Invoke(code, argList, callConv, nil); // Invoke returns TValue.Empty
end;

procedure TRttiInstanceMethodClassic.CheckExtended;
begin
  if not HasExtendedInfo then
    raise InsufficientRtti;
end;

function TRttiInstanceMethodClassic.GetMethodKind: TMethodKind;
begin
  if not HasExtendedInfo then
    Exit(mkProcedure);
  // {$METHODINFO ON} doesn't produce enough information to distinguish
  // effectively between class methods and instance methods.
  // Assume instance method for now.
  if ReturnType = nil then
    Result := mkProcedure
  else
    Result := mkFunction;
end;

function TRttiInstanceMethodClassic.GetCallingConvention: TCallConv;
begin
  Result := TailHandle^.CC;
end;

function TRttiInstanceMethodClassic.GetReturnType: TRttiType;
begin
  CheckExtended;
  Result := Pool.GetType(ReturnTypeInfo);
end;

function TRttiInstanceMethodClassic.GetHasExtendedInfo: Boolean;
begin
  Result := FTail <> nil;
end;

function TRttiInstanceMethodClassic.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiInstanceMethodClassic.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

function TRttiInstanceMethodClassic.GetTailHandle: PVmtMethodEntryTail;
begin
  CheckExtended;
  Result := FTail;
end;

function TRttiInstanceMethodClassic.GetCodeAddress: Pointer;
begin
  Result := Handle^.CodeAddress;
  if (Result <> nil) and (PPointer(Result)^ = nil) then
    Exit(nil);
end;

{ TRttiInstanceMethodEx }

type
  TRttiInstanceMethodEx = class(TRttiMethod)
  private
    FInstanceMethod: TRttiInstanceMethodClassic;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;

    function GetVisibility: TMemberVisibility; override;
    function GetMethodKind: TMethodKind; override;
    function GetCallingConvention: TCallConv; override;
    function GetReturnType: TRttiType; override;
    function GetHasExtendedInfo: Boolean; override;
    function GetVirtualIndex: Smallint; override;
    function GetDispatchKind: TDispatchKind; override;
    function GetIsClassMethod: Boolean; override;
    function GetIsStatic: Boolean; override;
    function GetCodeAddress: Pointer; override;

    function GetName: string; override;
    function GetHandle: PVmtMethodExEntry; inline;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PVmtMethodExEntry read GetHandle;

    function GetParameters: TArray<TRttiParameter>; override;
    function GetAttributes: TArray<TCustomAttribute>; override;
  end;

{ TRttiInstanceMethodEx }

constructor TRttiInstanceMethodEx.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  FInstanceMethod := APackage.ReadObjectPointer(TRttiInstanceMethodClassic,
    Self, Handle^.Entry) as TRttiInstanceMethodClassic;
  Inc(P, SizeOf(Handle^));
end;

function TRttiInstanceMethodEx.GetHandle: PVmtMethodExEntry;
begin
  Result := inherited Handle;
end;

function TRttiInstanceMethodEx.GetVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility(GetBitField(Handle^.Flags, mfVisibilityShift, mfVisibilityBits));
end;

function TRttiInstanceMethodEx.GetMethodKind: TMethodKind;
begin
  if (Handle^.Flags and mfSpecial) <> 0 then
  begin
    case TSpecialMethod(GetBitField(Handle^.Flags, mfSpecialShift, mfSpecialBits)) of
      smConstructor: Result := mkConstructor;
      smDestructor: Result := mkDestructor;
      smOperatorOverload: Result := mkOperatorOverload;
    else
      Result := mkProcedure;
    end;
  end
  else
  begin
    if ((Handle^.Flags and mfHasSelf) = 0) or // static class method
        ((Handle^.Flags and mfClassMethod) <> 0) then
      if FInstanceMethod.ReturnTypeInfo = nil then
        Result := mkClassProcedure
      else
        Result := mkClassFunction
    else
      if FInstanceMethod.ReturnTypeInfo = nil then
        Result := mkProcedure
      else
        Result := mkFunction;
  end;
end;

function TRttiInstanceMethodEx.GetCallingConvention: TCallConv;
begin
  Result := FInstanceMethod.CallingConvention;
end;

function TRttiInstanceMethodEx.GetAttributes: TArray<TCustomAttribute>;
begin
  Result := FInstanceMethod.GetAttributes;
end;

function TRttiInstanceMethodEx.GetReturnType: TRttiType;
begin
  if FInstanceMethod.ReturnTypeInfo = nil then
    Result := nil
  else
    Result := FInstanceMethod.GetReturnType;
end;

function TRttiInstanceMethodEx.GetHasExtendedInfo: Boolean;
begin
  Result := FInstanceMethod.GetHasExtendedInfo;
end;

function TRttiInstanceMethodEx.GetVirtualIndex: Smallint;
begin
  Result := Handle^.VirtualIndex;
end;

function TRttiInstanceMethodEx.GetDispatchKind: TDispatchKind;
begin
  Result := TDispatchKind(GetBitField(
    Handle^.Flags, mfDispatchKindShift, mfDispatchKindBits));
end;

function TRttiInstanceMethodEx.GetIsClassMethod: Boolean;
begin
  Result := IsStatic or ((Handle^.Flags and (mfSpecial or mfClassMethod)) = mfClassMethod);
end;

function TRttiInstanceMethodEx.GetIsStatic: Boolean;
begin
  if (Handle^.Flags and mfSpecial) <> 0 then
    Result := TSpecialMethod(GetBitField(Handle^.Flags, mfSpecialShift, mfSpecialBits)) = smOperatorOverload
  else
    Result := (Handle^.Flags and mfHasSelf) = 0;
end;

function TRttiInstanceMethodEx.GetCodeAddress: Pointer;
begin
  Result := FInstanceMethod.CodeAddress;
end;

function TRttiInstanceMethodEx.GetName: string;
begin
  Result := FInstanceMethod.Name;
end;

function TRttiInstanceMethodEx.HasName(const AName: string): Boolean;
begin
  Result := FInstanceMethod.HasName(AName);
end;

function TRttiInstanceMethodEx.DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  code: Pointer;
  argCount: Integer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
  cls: TClass;
  obj: TObject;
  alloc: Boolean;
  isCons, isDest, isStat, isClas: Boolean;
  callConv: TCallConv;
begin
  GetCommonInvokeParams(isCons, isDest, isStat, isClas, callConv);

  parList := GetParameters;
  argCount := Length(Args);
  if argCount <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  if isCons or isDest then
    Inc(argCount);
  if not isStat then
    Inc(argCount);

  SetLength(argList, argCount);
  currArg := 0;
  cls := nil;

  alloc := True; // avoid warning
  obj := nil; // avoid warning

  if not isStat then
  begin
    // Two jobs: handle special methods like ctor/dtor, and
    // extract metaclass so that vtable lookups can occur.
    if isCons then
    begin
      alloc := Instance.TryAsTypeInternal(cls, System.TypeInfo(TClass)); // flag: should allocate
      if alloc then
        obj := nil // not used
      else
      begin
        obj := Instance.AsObject;
        if obj <> nil then
          cls := obj.ClassType
        else
          cls := nil;
      end;
      if alloc then
        PushSelfFirst(callConv, argList, currArg, cls)
      else
        PushSelfFirst(callConv, argList, currArg, obj);
      argList[currArg] := alloc;
      Inc(currArg);
    end
    else if isDest then
    begin
      cls := Instance.AsObject.ClassType;
      PushSelfFirst(callConv, argList, currArg, Instance);
      argList[currArg] := True;
      Inc(currArg);
    end
    else if isClas then
    begin
      cls := Instance.AsClass;
      PushSelfFirst(callConv, argList, currArg, Instance);
    end
    else
    begin
      cls := Instance.AsObject.ClassType;
      PushSelfFirst(callConv, argList, currArg, Instance);
    end;

    // We are actually more strict than Delphi compiler/RTL here:
    if (cls <> nil) and not cls.InheritsFrom(TRttiInstanceType(Parent).MetaclassType) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], callConv);
    Inc(currArg);
  end;

  if isStat then
    code := CodeAddress
  else
    case DispatchKind of
      dkVtable: code := PVtable(cls)^[VirtualIndex];
      dkDynamic: code := GetDynaMethod(cls, VirtualIndex);
    else
      code := CodeAddress;
    end;

  CheckCodeAddress(code);

  if not isStat then
  begin
    if isCons then
    begin
      if alloc then
        PushSelfLast(callConv, argList, currArg, cls)
      else
        PushSelfLast(callConv, argList, currArg, obj);
    end
    else
      PushSelfLast(callConv, argList, currArg, Instance);
  end;

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(code, argList, callConv, ReturnType.Handle, isStat)
  else if isCons then
    Result := System.Rtti.Invoke(code, argList, callConv, cls.ClassInfo, isStat, True)
  else
    Result := System.Rtti.Invoke(code, argList, callConv, nil);
end;

function TRttiInstanceMethodEx.GetParameters: TArray<TRttiParameter>;
begin
  if IsStatic then
  begin
    Result := FInstanceMethod.FParams;
  end
  else
    Result := FInstanceMethod.GetParameters;
end;

type
  TRttiInstancePropertyClassic = class(TRttiInstanceProperty)
  private
    function GetPropInfo: PPropInfo; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetHandle: PPropInfo; inline;
  public
    property Handle: PPropInfo read GetHandle;
  end;

  TRttiInstancePropertyEx = class(TRttiInstanceProperty)
  private
    function GetPropInfo: PPropInfo; override;
    function GetHandle: PPropInfoEx; inline;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetVisibility: TMemberVisibility; override;
  public
    property Handle: PPropInfoEx read GetHandle;
  end;

  TRttiInstanceFieldClassic = class(TRttiField)
  private
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetName: string; override;
    function GetHandle: PVmtFieldEntry; inline;
    function GetFieldType: TRttiType; override;
    function GetOffset: Integer; override;
    function GetParent: TRttiInstanceType;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PVmtFieldEntry read GetHandle;
    property Parent: TRttiInstanceType read GetParent;
  end;

  TRttiInstanceFieldEx = class(TRttiField)
  private
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetName: string; override;
    function GetHandle: PFieldExEntry; inline;
    function GetFieldType: TRttiType; override;
    function GetOffset: Integer; override;
    function GetVisibility: TMemberVisibility; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PFieldExEntry read GetHandle;
  end;

{ TRttiInstanceType }

constructor TRttiInstanceType.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);
var
  ft: PVmtFieldTable;
begin
  inherited;

  // ClassTab needed for loading classic published fields.
  ft := PPointer(PByte(MetaclassType) + vmtFieldTable)^;
  if ft <> nil then
    FClassTab := ft^.ClassTab;
end;

function TRttiInstanceType.GetAttributes: TArray<TCustomAttribute>;
begin
  if not FReadPropData then
    ReadPropData;
  Result := inherited GetAttributes;
end;

function TRttiInstanceType.GetBaseType: TRttiType;
begin
  Result := GetBaseTyped;
end;

function TRttiInstanceType.GetBaseTyped: TRttiInstanceType;
begin
  Result := Pool.TypeOrNil(TypeData^.ParentInfo) as TRttiInstanceType;
end;

function TRttiInstanceType.GetMetaclassType: TClass;
begin
  Result := TypeData^.ClassType;
end;

procedure TRttiInstanceType.ReadPropData;
var
  p: PByte;

  function ReadClassicProps: TArray<TRttiProperty>;
  var
    i: Integer;
  begin
    if p = nil then
      Exit(nil);
    SetLength(Result, ReadU16(p));
    for i := 0 to Length(Result) - 1 do
      Result[i] := Package.ReadObject(TRttiInstancePropertyClassic, Self, p) as TRttiProperty;
  end;

  function ReadExtendedProps: TArray<TRttiProperty>;
  var
    i: Integer;
  begin
    if p = nil then
      Exit(nil);
    SetLength(Result, ReadU16(p));
    for i := 0 to Length(Result) - 1 do
      Result[i] := Package.ReadObject(TRttiInstancePropertyEx, Self, p) as TRttiProperty;
  end;

  function SubtractClassic(const AClassic, AExtended: TArray<TRttiProperty>): TArray<TRttiProperty>;
  var
    list: TList<TRttiProperty>;

    function ContainsExtended(PropInfo: PPropInfo): Boolean;
    var
      i: Integer;
      p: TRttiProperty;
    begin
      for i := 0 to list.Count - 1 do
      begin
        p := list[i];
        if p is TRttiInstancePropertyEx then
          if TRttiInstanceProperty(p).PropInfo = PropInfo then
            Exit(True);
      end;
      Result := False;
    end;

  var
    i: Integer;
  begin
    list := TList<TRttiProperty>.Create;
    try
      list.AddRange(AExtended);
      for i := 0 to Length(AClassic) - 1 do
        if not ContainsExtended(TRttiInstanceProperty(AClassic[i]).PropInfo) then
          list.Add(AClassic[i]);

      Result := list.ToArray;
    finally
      list.Free;
    end;
  end;

  function ReadIndexedProps: TArray<TRttiIndexedProperty>;
  var
    i: Integer;
  begin
    SetLength(Result, ReadU16(P));
    for i := 0 to High(Result) do
      Result[i] := Package.ReadObject(TRttiIndexedProperty, Self, p) as TRttiIndexedProperty;
  end;

var
  classic, ext: TArray<TRttiProperty>;
begin
  if FReadPropData then
    Exit;

  p := PByte(TypeData.PropData);

  TMonitor.Enter(Package.FLock);
  try
    classic := ReadClassicProps;
    ext := ReadExtendedProps;

    FProps := SubtractClassic(classic, ext);

    FAttributeGetter := LazyLoadAttributes(p);
    FIndexedProps := ReadIndexedProps;
    FReadPropData := True;
  finally
    TMonitor.Exit(Package.FLock);
  end;
end;

function TRttiInstanceType.GetDeclaredProperties: TArray<TRttiProperty>;
begin
  if not FReadPropData then
    ReadPropData;
  Result := FProps;
end;

function TRttiInstanceType.GetDeclaredMethods: TArray<TRttiMethod>;
begin
  if not FReadMethData then
    ReadMethData;
  Result := FMeths;
end;

procedure TRttiInstanceType.ReadMethData;
var
  p: PByte;

  function ReadClassicMeths: TArray<TRttiMethod>;
  var
    i: Integer;
    methLen: Integer;
    next: PByte;
  begin
    if p = nil then
      Exit(nil);
    SetLength(Result, ReadU16(p));
    for i := 0 to Length(Result) - 1 do
    begin
      methLen := PeekU16(p);
      next := p + methLen;
      Result[i] := Package.ReadObject(TRttiInstanceMethodClassic, Self, p) as TRttiInstanceMethodClassic;
      p := next;
    end;
  end;

  function ReadExtendedMeths: TArray<TRttiMethod>;
  var
    i: Integer;
  begin
    if p = nil then
      Exit(nil);
    SetLength(Result, ReadU16(p)); // TVmtMethodTable.ExCount
    for i := 0 to Length(Result) - 1 do
      Result[i] := Package.ReadObject(TRttiInstanceMethodEx, Self, p) as TRttiInstanceMethodEx;
  end;

  function SubtractClassic(const AClassic, AExtended: TArray<TRttiMethod>): TArray<TRttiMethod>;
  var
    list: TList<TRttiMethod>;

    function ContainsExtended(CodeAddress: Pointer): Boolean;
    var
      i: Integer;
      m: TRttiMethod;
    begin
      for i := 0 to list.Count - 1 do
      begin
        m := list[i];
        if m is TRttiInstanceMethodEx then
          if TRttiInstanceMethodEx(m).FInstanceMethod.CodeAddress = CodeAddress then
            Exit(True);
      end;
      Result := False;
    end;

  var
    i: Integer;
  begin
    list := TList<TRttiMethod>.Create;
    try
      list.AddRange(AExtended);
      for i := 0 to Length(AClassic) - 1 do
        if not ContainsExtended(TRttiInstanceMethodClassic(AClassic[i]).CodeAddress) then
          list.Add(AClassic[i]);

      Result := list.ToArray;
    finally
      list.Free;
    end;
  end;

var
  classic, ext: TArray<TRttiMethod>;
begin
  if FReadMethData then
    Exit;

  TMonitor.Enter(Package.FLock);
  try
    p := PPointer(PByte(MetaclassType) + vmtMethodTable)^;

    if p = nil then
      Exit;

    classic := ReadClassicMeths;
    ext := ReadExtendedMeths;
    FVirtCount := ReadU16(p);

    FMeths := SubtractClassic(classic, ext);
    FReadMethData := True;
  finally
    TMonitor.Exit(Package.FLock);
  end;
end;

function TRttiInstanceType.GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
var
  pi: PInterfaceTable;
  p: PByte;
  i: Integer;
begin
  pi := MetaclassType.GetInterfaceTable;

  if pi = nil then
    Exit(nil);

  p := @pi^.Entries[pi^.EntryCount];
  SetLength(Result, pi^.EntryCount);

  for i := 0 to pi.EntryCount - 1 do
    Result[i] := Pool.GetType(DerefPointer(ReadPointer(p))) as TRttiInterfaceType;
end;

function _GetDeclaredImplementedInterfaces(AType: TRttiType): TArray<TRttiInterfaceType>;
begin
  Result := (AType as TRttiInstanceType).GetDeclaredImplementedInterfaces;
end;

function TRttiInstanceType.GetImplementedInterfaces: TArray<TRttiInterfaceType>;
begin
  Result := GetObjectList<TRttiInterfaceType>(_GetDeclaredImplementedInterfaces);
end;

function TRttiInstanceType.GetDeclaredFields: TArray<TRttiField>;
var
  p: PByte;

  function ReadClassicFields: TArray<TRttiField>;
  var
    ft: PVmtFieldTable;
    i: Integer;
  begin
    ft := Pointer(p);
    SetLength(Result, ft^.Count);
    Inc(p, SizeOf(ft^));
    for i := 0 to Length(Result) - 1 do
      Result[i] := Package.ReadObject(TRttiInstanceFieldClassic, Self, p) as TRttiField;
  end;

  function ReadExtendedFields: TArray<TRttiField>;
  var
    i: Integer;
  begin
    SetLength(Result, ReadU16(p));
    for i := 0 to Length(Result) - 1 do
      Result[i] := Package.ReadObject(TRttiInstanceFieldEx, Self, p) as TRttiField;
    end;

  function SubtractClassic(const AClassic, AExtended: TArray<TRttiField>): TArray<TRttiField>;
  var
    list: TList<TRttiField>;

    function ContainsExtended(Offset: Integer): Boolean;
    var
      i: Integer;
      m: TRttiField;
    begin
      for i := 0 to list.Count - 1 do
      begin
        m := list[i];
        if (m is TRttiInstanceFieldEx) and (m.Offset = Offset) then
            Exit(True);
      end;
      Result := False;
    end;

  var
    i: Integer;
  begin
    list := TList<TRttiField>.Create;
    try
      list.AddRange(AExtended);
      for i := 0 to Length(AClassic) - 1 do
        if not ContainsExtended(AClassic[i].Offset) then
          list.Add(AClassic[i]);

      Result := list.ToArray;
    finally
      list.Free;
    end;
  end;

var
  classic, ext: TArray<TRttiField>;
begin
  p := PPointer(PByte(MetaclassType) + vmtFieldTable)^;

  if p = nil then
    Exit(nil);

  classic := ReadClassicFields;
  ext := ReadExtendedFields;

  Result := SubtractClassic(classic, ext);
end;

function TRttiInstanceType.GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>;
begin
  if not FReadPropData then
    ReadPropData;
  Result := FIndexedProps;
end;

function TRttiInstanceType.GetDeclaringUnitName: string;
begin
  Result := TypeData.UnitNameFld.ToString;
end;

function TRttiInstanceType.GetVmtSize: Integer;
begin
  if not FReadMethData then
    ReadMethData;
  Result := FVirtCount * SizeOf(Pointer) - vmtSelfPtr;
end;

{ TRttiProperty }

function TRttiProperty.GetValue(Instance: Pointer): TValue;
begin
  if not IsReadable then
    raise EPropWriteOnly.Create(Name);
  Result := DoGetValue(Instance);
end;

procedure TRttiProperty.SetValue(Instance: Pointer; const AValue: TValue);
begin
  if not IsWritable then
    raise EPropReadOnly.Create(Name);
  DoSetValue(Instance, AValue);
end;

{ TRttiInstanceProperty }

const
  pfVisibilityShift = 0;
  pfVisibilityBits = 2;

function TRttiInstanceProperty.GetDefault: Integer;
begin
  Result := GetPropInfo^.Default;
end;

function TRttiInstanceProperty.GetIndex: Integer;
begin
  Result := GetPropInfo^.Index;
end;

function TRttiInstanceProperty.GetName: string;
begin
  Result := GetPropInfo.NameFld.ToString;
end;

function TRttiInstanceProperty.HasName(const AName: string): Boolean;
begin
  Result := GetPropInfo.NameFld.HasName(AName);
end;

function TRttiInstanceProperty.GetNameIndex: Smallint;
begin
  Result := GetPropInfo^.NameIndex;
end;

function TRttiInstanceProperty.GetPropertyType: TRttiType;
begin
  Result := Pool.TypeOrNil(GetPropInfo^.PropType);
end;

function TRttiInstanceProperty.GetIsReadable: Boolean;
begin
  Result := PropInfo^.GetProc <> nil;
end;

function TRttiInstanceProperty.GetIsWritable: Boolean;
begin
  Result := PropInfo^.SetProc <> nil;
end;

function TRttiInstanceProperty.DoGetValue(Instance: Pointer): TValue;
var
  getter: Pointer;
  code: Pointer;
  args: TArray<TValue>;
begin
  getter := PropInfo^.GetProc;
  if (IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    TValue.Make(PByte(Instance) + (IntPtr(getter) and (not PROPSLOT_MASK)),
      PropertyType.Handle, Result);
    Exit;
  end;

  if (IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
  begin
    // Virtual dispatch, but with offset, not slot
    code := PPointer(PIntPtr(Instance)^ + SmallInt(IntPtr(getter)))^;
  end
  else
  begin
    // Static dispatch
    code := getter;
  end;

  CheckCodeAddress(code);

  if Index = Integer($80000000) then
  begin
    // no index
    SetLength(args, 1);
    args[0] := TObject(Instance);
    Result := Invoke(code, args, ccReg, PropertyType.Handle, False); // not static
  end
  else
  begin
    SetLength(args, 2);
    args[0] := TObject(Instance);
    args[1] := Index;
    Result := Invoke(code, args, ccReg, PropertyType.Handle, False); // not static
  end;
end;

procedure TRttiInstanceProperty.DoSetValue(Instance: Pointer; const AValue: TValue);
var
  setter: Pointer;
  code: Pointer;
  args: TArray<TValue>;
begin
  setter := PropInfo^.SetProc;
  if (IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    AValue.Cast(PropertyType.Handle).ExtractRawData(
      PByte(Instance) + (IntPtr(setter) and (not PROPSLOT_MASK)));
    Exit;
  end;

  if (IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
  begin
    // Virtual dispatch, but with offset, not slot
    code := PPointer(PIntPtr(Instance)^ + SmallInt(IntPtr(setter)))^;
  end
  else
  begin
    // Static dispatch
    code := setter;
  end;

  CheckCodeAddress(code);

  if Index = Integer($80000000) then
  begin
    // no index
    SetLength(args, 2);
    args[0] := TObject(Instance);
    args[1] := AValue.Cast(PropertyType.Handle);
    Invoke(code, args, ccReg, nil);
  end
  else
  begin
    SetLength(args, 3);
    args[0] := TObject(Instance);
    args[1] := Index;
    args[2] := AValue.Cast(PropertyType.Handle);
    Invoke(code, args, ccReg, nil);
  end;
end;

function TRttiInstanceProperty.ToString: string;
begin
  Result := 'property ' + Name + ': ' + PropertyType.Name; // do not localize
end;

{ TRttiInstancePropertyClassic }

function TRttiInstancePropertyClassic.GetHandle: PPropInfo;
begin
  Result := inherited Handle;
end;

constructor TRttiInstancePropertyClassic.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte( Handle.Tail );
end;

function TRttiInstancePropertyClassic.GetPropInfo: PPropInfo;
begin
  Result := Handle;
end;

{ TRttiInstancePropertyEx }

function TRttiInstancePropertyEx.GetHandle: PPropInfoEx;
begin
  Result := inherited Handle;
end;

constructor TRttiInstancePropertyEx.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := @Handle^.AttrData;
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiInstancePropertyEx.GetPropInfo: PPropInfo;
begin
  Result := Handle^.Info;
end;

function TRttiInstancePropertyEx.GetVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility(GetBitField(Handle^.Flags, pfVisibilityShift, pfVisibilityBits));
end;

{ TRttiOrdinalType }

constructor TRttiOrdinalType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte(@TypeData^.MaxValue) + SizeOf(TypeData^.MaxValue);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiOrdinalType.GetMaxValue: Integer;
begin
  Result := TypeData^.MaxValue;
end;

function TRttiOrdinalType.GetMinValue: Integer;
begin
  Result := TypeData^.MinValue;
end;

function TRttiOrdinalType.GetOrdType: TOrdType;
begin
  Result := TypeData^.OrdType;
end;

function TRttiOrdinalType.GetTypeSize: Integer;
const
  Sizes: array[TOrdType] of Integer = (1, 1, 2, 2, 4, 4);
begin
  Result := Sizes[OrdType];
end;

{ TRttiInt64Type }

constructor TRttiInt64Type.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);
begin
  inherited;
  P := @TypeData^.Int64AttrData;
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiInt64Type.GetTypeSize: Integer;
begin
  Result := SizeOf(Int64);
end;

function TRttiInt64Type.GetMaxValue: Int64;
begin
  Result := TypeData^.MaxInt64Value;
end;

function TRttiInt64Type.GetMinValue: Int64;
begin
  Result := TypeData^.MinInt64Value;
end;

{ TRttiClassRefType }

constructor TRttiClassRefType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := @TypeData^.ClassRefAttrData;
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiClassRefType.GetInstanceType: TRttiInstanceType;
begin
  Result := Pool.TypeOrNil(TypeData^.InstanceType) as TRttiInstanceType;
end;

function TRttiClassRefType.GetMetaclassType: TClass;
var
  p: PPTypeInfo;
begin
  p := TypeData^.InstanceType;
  if (p = nil) or (p^ = nil) then
    Exit(nil);
  Result := System.TypInfo.GetTypeData(p^)^.ClassType;
end;

{ TRttiEnumerationType }

function SkipEnumNameList(P: PByte; ACount: Integer): PByte;
begin
  while ACount > 0 do
  begin
    P := SkipShortString(P);
    Dec(ACount);
  end;
  Result := P;
end;

function TRttiEnumerationType.HasEnumNameList: Boolean;
begin
  // No name list if we are a subrange of another enumeration.
  Result := (TypeData^.BaseType = nil) or
    (DerefPointer(TypeData^.BaseType) = Handle) or
    // Special case with built-in Boolean enum
    IsBoolType(Handle);
end;

constructor TRttiEnumerationType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := @TypeData^.NameList;
  if HasEnumNameList then
    P := SkipEnumNameList(P, MaxValue - MinValue + 1);
  // Unit name
  P := SkipShortString(P);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiEnumerationType.GetNames: TArray<string>;
var
  i: Integer;
  p: PByte;
begin
  SetLength(Result, MaxValue - MinValue + 1);
  if HasEnumNameList then
    p := @TypeData^.NameList
  else
    p := SkipEnumNameList(@System.TypInfo.GetTypeData(DerefPointer(TypeData^.BaseType))^.NameList, MinValue);
  for i := 0 to Length(Result) - 1 do
    Result[i] := ReadShortString(p);
end;

class function TRttiEnumerationType.GetName<T{: enum}>(AValue: T): string;
var
  v: Integer;
begin
  case PTypeInfo(TypeInfo(T))^.Kind of
    tkEnumeration:
      case System.TypInfo.GetTypeData(TypeInfo(T))^.OrdType of
        otUByte, otSByte: v := PByte(@AValue)^;
        otUWord, otSWord: v := PWord(@AValue)^;
        otULong, otSLong: v := PInteger(@AValue)^;
      end;
  else
    raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
  Result := System.TypInfo.GetEnumName(TypeInfo(T), v);
end;

class function TRttiEnumerationType.GetValue<T{: enum}>(const AName: string): T;
var
  v: Integer;
begin
  case PTypeInfo(TypeInfo(T))^.Kind of
    tkEnumeration:
      case System.TypInfo.GetTypeData(TypeInfo(T))^.OrdType of
        otUByte, otSByte: PByte(@Result)^ := GetEnumValue(TypeInfo(T), AName);
        otUWord, otSWord: PWord(@Result)^ := GetEnumValue(TypeInfo(T), AName);
        otULong, otSLong: PInteger(@Result)^ := GetEnumValue(TypeInfo(T), AName);
      end;
  else
    raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
end;

function TRttiEnumerationType.GetUnderlyingType: TRttiType;
begin
  Result := Pool.TypeOrNil(TypeData^.BaseType);
end;

function TRttiEnumerationType.GetMaxValue: Integer;
begin
  Result := TypeData^.MaxValue;
  if TypeData^.MinValue < 0 then // one of ByteBool/WordBool/LongBool
    Result := 1;
end;

function TRttiEnumerationType.GetMinValue: Integer;
begin
  Result := TypeData^.MinValue;
  if Result < 0 then // C bool
    Result := 0;
end;

{ TProcSig }

type
  TProcSig = class(TRttiObject)
  private
    function GetHandle: PProcedureSignature; inline;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    function HasInfo: Boolean;
    property Handle: PProcedureSignature read GetHandle;
    function GetParams: TArray<TRttiParameter>;
    function GetReturnType: TRttiType;
    function ToString: string; override;
  end;

  TProcParam = class(TRttiParameter)
  private
    function GetHandle: PProcedureParam; inline;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetFlags: TParamFlags; override;
    function GetParamType: TRttiType; override;
    function GetName: string; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PProcedureParam read GetHandle;
  end;

{ TProcSig }

function TProcSig.GetHandle: PProcedureSignature;
begin
  Result := PProcedureSignature(inherited Handle);
end;

constructor TProcSig.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
var
  i: Integer;
begin
  inherited;
  if not HasInfo then
  begin
    P := @Handle^.Flags;
    Inc(P);
  end
  else
  begin
    P := @Handle^.ParamCount;
    Inc(P);
    for i := 1 to Handle^.ParamCount do
    begin
                                           
      P := PByte(PProcedureParam(P).AttrData);
      Inc(P, PWord(P)^);
    end;
  end;
end;

function TProcSig.HasInfo: Boolean;
begin
  Result := Handle^.Flags <> 255;
end;

function TProcSig.ToString: string;
var
  params: TArray<TRttiParameter>;
  ret: TRttiType;
  i: Integer;
begin
  if not HasInfo then
    Exit('');
  Result := '';
  params := GetParams;
  if Length(params) > 0 then
  begin
    Result := '(';
    for i := 0 to Length(params) - 1 do
    begin
      Result := Result + params[i].ToString;
      if i < Length(params) - 1 then
        Result := Result + '; ';
    end;
    Result := Result + ')';
  end;
  ret := Pool.TypeOrNil(Handle^.ResultType);
  if ret <> nil then
    Result := Result + ': ' + ret.ToString;
end;

function TProcSig.GetReturnType: TRttiType;
begin
  if not HasInfo then
    Exit(nil);
  Result := Pool.TypeOrNil(Handle^.ResultType);
end;

function TProcSig.GetParams: TArray<TRttiParameter>;
var
  i: Integer;
  p: PByte;
begin
  if not HasInfo then
    Exit(nil);
  SetLength(Result, Handle^.ParamCount);
  p := @Handle^.ParamCount;
  Inc(p);
  for i := 0 to Handle^.ParamCount - 1 do
    Result[i] := Package.ReadObject(TProcParam, Self, p) as TRttiParameter;
end;

{ TProcParam }

function TProcParam.GetHandle: PProcedureParam;
begin
  Result := PProcedureParam(inherited Handle);
end;

constructor TProcParam.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte(Handle.AttrData);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TProcParam.GetFlags: TParamFlags;
begin
  Result := TParamFlags(Handle^.Flags);
end;

function TProcParam.GetParamType: TRttiType;
begin
  Result := Pool.TypeOrNil(Handle^.ParamType);
end;

function TProcParam.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TProcParam.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

{ TRttiManagedField }

type
  PManagedField = ^TManagedField;

constructor TRttiManagedField.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := P + SizeOf(TManagedField);
end;

function TRttiManagedField.GetFieldOffset: Integer;
begin
  Result := PManagedField(FHandle)^.FldOffset;
end;

function TRttiManagedField.GetFieldType: TRttiType;
begin
  Result := Pool.TypeOrNil(PManagedField(FHandle)^.TypeRef);
end;

{ TRttiRecordField }

const
  ffVisibilityShift = 0;
  ffVisibilityBits = 2;

type
  TRttiRecordField = class(TRttiField)
  private
    function GetHandle: PRecordTypeField; inline;
    function GetFieldType: TRttiType; override;
    function GetOffset: Integer; override;
    function GetName: string; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetVisibility: TMemberVisibility; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PRecordTypeField read GetHandle;
  end;

{ TRttiRecordField }

function TRttiRecordField.GetHandle: PRecordTypeField;
begin
  Result := inherited Handle;
end;

constructor TRttiRecordField.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte(Handle.AttrData);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiRecordField.GetVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility(GetBitField(Handle^.Flags, ffVisibilityShift, ffVisibilityBits));
end;

function TRttiRecordField.GetFieldType: TRttiType;
begin
  Result := Pool.TypeOrNil(Handle^.Field.TypeRef);
end;

function TRttiRecordField.GetOffset: Integer;
begin
  Result := Handle^.Field.FldOffset;
end;

function TRttiRecordField.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiRecordField.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

{ TRttiRecordMethod }

type
  TRecMethodKind = (
    rmkMethod, // has self, but not included in signature
    rmkStaticMethod,
    rmkConstructor,
    rmkOperator);

const
  // TRecMethodFlags
	rmfKindShift = 0;
	rmfKindBits = 2;
	rmfVisibilityShift = 2;
	rmfVisibilityBits = 2;

type
  TRttiRecordMethod = class(TRttiMethod)
  private
    FSig: TProcSig;
    function GetName: string; override;
    function GetMethodKind: TMethodKind; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;

    function GetCallingConvention: TCallConv; override;
    function GetReturnType: TRttiType; override;
    function GetDispatchKind: TDispatchKind; override;
    function GetHasExtendedInfo: Boolean; override;
    function GetCodeAddress: Pointer; override;
    function GetIsClassMethod: Boolean; override;
    function GetIsStatic: Boolean; override;
    function GetVisibility: TMemberVisibility; override;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; override;
  public
    function HasName(const AName: string): Boolean; override;
    function GetParameters: TArray<TRttiParameter>; override;
    property Signature: TProcSig read FSig;
  end;

{ TRttiRecordMethod }

constructor TRttiRecordMethod.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := SkipShortString(@PRecordTypeMethod(P)^.NameLen);
  FSig := Package.ReadObject(TProcSig, Self, P) as TProcSig;
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiRecordMethod.GetCallingConvention: TCallConv;
begin
  Result := FSig.Handle^.CC;
end;

function TRttiRecordMethod.GetReturnType: TRttiType;
begin
  Result := FSig.GetReturnType;
end;

function TRttiRecordMethod.GetDispatchKind: TDispatchKind;
begin
  Result := dkStatic;
end;

function TRttiRecordMethod.GetHasExtendedInfo: Boolean;
begin
  Result := FSig.HasInfo;
end;

function TRttiRecordMethod.GetCodeAddress: Pointer;
begin
  Result := PRecordTypeMethod(Handle)^.Code;
end;

function TRttiRecordMethod.GetIsClassMethod: Boolean;
begin
  Result := GetMethodKind in [mkClassProcedure, mkClassFunction, mkOperatorOverload];
end;

function TRttiRecordMethod.GetIsStatic: Boolean;
begin
  Result := not (GetMethodKind in [mkProcedure, mkFunction]);
end;

function TRttiRecordMethod.GetVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility(
    GetBitField(PRecordTypeMethod(Handle)^.Flags, rmfVisibilityShift, rmfVisibilityBits));
end;

function TRttiRecordMethod.DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
  inst: TValue;
  deref: Boolean;
  isCons, isDest, isStat, isClas: Boolean;
  callConv: TCallConv;
begin
  GetCommonInvokeParams(isCons, isDest, isStat, isClas, callConv);

  // init new record instance when Instance is empty (nil)
  if isCons and (Instance.Kind in [tkUnknown, tkPointer]) and Instance.IsEmpty then
    TValue.Make(nil, Parent.Handle, Instance);

  deref := False;
  if not isStat or isCons then
    if (Instance.Kind = tkPointer) and
      (
        ( // pointer to the record type
          (Instance.TypeData^.RefType <> nil) and
          (Instance.TypeData^.RefType^ = Parent.Handle)
        )
      or
        ( // untyped pointer (i.e. System.Pointer or equivalent)
          (Instance.TypeData^.RefType = nil) or
          (Instance.TypeData^.RefType^ = nil)
        )
      ) then
    begin
      // passed as is, a pointer
      inst := Instance;
    end
    else
    begin
      // pass a pointer to value inside Instance
      // beware that mutations are lost owing to copies
      if Instance.TypeInfo <> Parent.Handle then
        raise EInvalidCast.CreateRes(@SInvalidCast);
      inst := TValue.From(Instance.GetReferenceToRawData);
      deref := True;
    end;

  parList := GetParameters;
  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  if not isStat or isCons then
    SetLength(argList, Length(Args) + 1)
  else
    SetLength(argList, Length(Args));

  currArg := 0;
  if not isStat or isCons then
    PushSelfFirst(callConv, argList, currArg, inst);

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], callConv);
    Inc(currArg);
  end;

  if not isStat or isCons then
    PushSelfLast(callConv, argList, currArg, inst);

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(CodeAddress, argList, callConv, ReturnType.Handle, IsStatic)
  else if isCons then
  begin
    Result := System.Rtti.Invoke(CodeAddress, argList, callConv, inst.TypeInfo, IsStatic, True);
    if deref then
      TValue.Make(inst.AsType<Pointer>, Instance.TypeInfo, Result);
  end
  else
    Result := System.Rtti.Invoke(CodeAddress, argList, callConv, nil);
end;

function TRttiRecordMethod.GetParameters: TArray<TRttiParameter>;
begin
  Result := FSig.GetParams;
end;

function TRttiRecordMethod.GetMethodKind: TMethodKind;
begin
  case TRecMethodKind(GetBitField(PRecordTypeMethod(Handle)^.Flags, rmfKindShift, rmfKindBits)) of
    rmkMethod:
      if FSig.Handle^.ResultType = nil then
        Result := mkProcedure
      else
        Result := mkFunction;

    rmkStaticMethod:
      if FSig.Handle^.ResultType = nil then
        Result := mkClassProcedure
      else
        Result := mkClassFunction;

    rmkConstructor:
      Result := mkConstructor;

    rmkOperator:
      Result := mkOperatorOverload;
  else
    Result := mkProcedure; // unreachable
  end;
end;

function TRttiRecordMethod.GetName: string;
begin
  Result := UTF8IdentToString(PShortString(@PRecordTypeMethod(Handle)^.NameLen));
end;

function TRttiRecordMethod.HasName(const AName: string): Boolean;
begin
  Result := UTF8IdentStringCompare(PShortString(@PRecordTypeMethod(Handle)^.NameLen), AName);
end;

{ TRttiRecordType }

{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
constructor TRttiRecordType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  GetHFAElement;
end;
{$ENDIF CPUARM64 or LINUX64 or OSX64}

function TRttiRecordType.GetManagedFields: TArray<TRttiManagedField>;
var
  p: PByte;
  cnt: Integer;
  list: TList<TRttiManagedField>;
begin
  list := TList<TRttiManagedField>.Create;
  try
    p := @TypeData^.ManagedFldCount;
    cnt := ReadI32(p);
    while cnt > 0 do
    begin
      list.Add(Package.ReadObject(TRttiManagedField, Self, p) as TRttiManagedField);
      Dec(cnt);
    end;
    Result := list.ToArray;
  finally
    list.Free;
  end;
end;

function TRttiRecordType.GetDeclaredFields: TArray<TRttiField>;
var
  p: PByte;
  cnt, i: Integer;
  numOps: Byte;
begin
  p := @TypeData^.ManagedFldCount;
  cnt := ReadI32(p);
  Inc(p, SizeOf(TManagedField) * cnt);
  numOps := ReadU8(p);
  Inc(p, SizeOf(Pointer) * numOps);
  cnt := ReadI32(p);
  SetLength(Result, cnt);
  for i := 0 to cnt - 1 do
    Result[i] := Package.ReadObject(TRttiRecordField, Self, p) as TRttiRecordField;
  if not Assigned(FAttributeGetter) then
  begin
    TMonitor.Enter(Package.FLock);
    try
      FAttributeGetter := LazyLoadAttributes(p);
    finally
      TMonitor.Exit(Package.FLock);
    end;
  end
  else
    Inc(P, PWord(P)^);
  FMethOfs := p;
end;

function TRttiRecordType.GetDeclaredMethods: TArray<TRttiMethod>;
var
  p: PByte;
  cnt, i: Integer;
begin
  p := FMethOfs;
  if p = nil then
  begin
    GetFields;
    p := FMethOfs;
  end;
  cnt := ReadU16(p);
  SetLength(Result, cnt);
  for i := 0 to cnt - 1 do
    Result[i] := Package.ReadObject(TRttiRecordMethod, Self, p) as TRttiMethod;
end;

function TRttiRecordType.GetAttributes: TArray<TCustomAttribute>;
begin
  if not Assigned(FAttributeGetter) then
    GetFields;
  Result := inherited GetAttributes;
end;

function TRttiRecordType.GetTypeSize: Integer;
begin
  Result := TypeData^.RecSize;
end;

{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
procedure TRttiRecordType.GetHFAElement;

  function CheckField(const [ref] F: TRttiType): Boolean;
  begin
    Result := (F.GetTypeKind = tkFloat) and ((F as TRttiFloatType).FloatType in [ftSingle, ftDouble]);
  end;

var
  ctx: TRttiContext;
  anAttribute: TCustomAttribute;
  aRecordFields: TArray<TRttiField>;
  aFieldType: TRttiType;
  ElementType: TRttiFloatType;
  ElementCount: Integer;
  ind: Integer;
begin
  FHFAHasInsufficientTypeInformation := False;

  for anAttribute in GetAttributes do
    if anAttribute is HFAAttribute then
    begin
      FHFAElementType := ctx.GetType(PPTypeInfo((anAttribute as HFAAttribute).ElementType)^) as TRttiFloatType;
      FHFAElementCount := (anAttribute as HFAAttribute).ElementCount;
      Exit;
    end;

  aRecordFields := GetDeclaredFields;
  if not(Length(aRecordFields) in [1..4]) then Exit;

  aFieldType := aRecordFields[0].FieldType;
  if (aFieldType = nil) or (aFieldType.FHandle = nil) then
  begin
                                    
    FHFAHasInsufficientTypeInformation := True;
    Exit;
  end;

  if CheckField(aFieldType) then
  begin
    ElementType := aFieldType as TRttiFloatType;
    ElementCount := 1;
  end
  else if aFieldType.IsHFA then
  begin
    ElementType := aFieldType.HFAElementType;
    ElementCount := aFieldType.HFAElementCount;
  end
  else
    Exit;

  ind := 1;
  while ind < Length(aRecordFields) do
  begin
    aFieldType := aRecordFields[ind].FieldType;

    if (aFieldType = nil) or (aFieldType.FHandle = nil) then
    begin
                                      
      FHFAHasInsufficientTypeInformation := True;
      Exit;
    end;

    if CheckField(aFieldType) then
    begin
      if ElementType <> aFieldType then
        Exit;
      Inc(ElementCount);
    end
    else if aFieldType.IsHFA then
    begin
      if ElementType <> aFieldType.HFAElementType then
        Exit;
      Inc(ElementCount, aFieldType.HFAElementCount);
    end
    else
      Exit;
    Inc(ind);
  end;

  if ElementCount in [1..4] then
  begin
    FHFAElementType := ElementType;
    FHFAElementCount := ElementCount;
  end;
end;

function TRttiRecordType.GetIsHFA: Boolean;
begin
  Result := FHFAElementType <> nil;
end;

function TRttiRecordType.GetHFAElementType: TRttiFloatType;
begin
  Result := FHFAElementType;
end;

function TRttiRecordType.GetHFAElementCount: Integer;
begin
  Result := FHFAElementCount;
end;
{$ENDIF CPUARM64 or LINUX64 or OSX64}

{ TRttiArrayType }

constructor TRttiArrayType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  p := @TypeData^.ArrayData.Dims[0];
  Inc(p, GetDimensionCount * SizeOf(Pointer));
  FAttributeGetter := LazyLoadAttributes(p);
{$IF Defined(CPUARM64) or Defined(LINUX64)  or Defined(OSX64)}
  GetHFAElement;
{$ENDIF CPUARM64 or LINUX64 or OSX64}
end;

function TRttiArrayType.GetTypeSize: Integer;
begin
  Result := TypeData^.ArrayData.Size;
end;

function TRttiArrayType.GetTotalElementCount: Integer;
begin
  Result := TypeData^.ArrayData.ElCount;
end;

function TRttiArrayType.GetElementType: TRttiType;
begin
  Result := Pool.TypeOrNil(TypeData^.ArrayData.ElType);
end;

function TRttiArrayType.GetDimensionCount: Integer;
begin
  Result := TypeData^.ArrayData.DimCount;
end;

function TRttiArrayType.GetDimension(Index: Integer): TRttiType;
begin
  Result := Pool.TypeOrNil(TypeData^.ArrayData.Dims[Index]);
end;

{$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64) }
procedure TRttiArrayType.GetHFAElement;
var
  elType: TRTTIType;
begin
  elType := ElementType;
  if elType = nil then
    Exit;
  if (elType.TypeKind = tkFloat) and ((elType as TRTTIFloatType).FloatType in [ftSingle, ftDouble]) then
  begin
    if TotalElementCount in [1..4] then
    begin
      FHFAElementType := elType as TRTTIFloatType;
      FHFAElementCount := TotalElementCount;
    end;
  end
  else if elType.IsHFA then
  begin
    if TotalElementCount * elType.HFAElementCount in [1..4] then
    begin
      FHFAElementType := elType.HFAElementType;
      FHFAElementCount := TotalElementCount * elType.HFAElementCount;
    end;
  end;
end;

function TRttiArrayType.GetIsHFA: Boolean;
begin
  Result := FHFAElementType <> nil;
end;

function TRttiArrayType.GetHFAElementType: TRttiFloatType;
begin
  Result := FHFAElementType;
end;

function TRttiArrayType.GetHFAElementCount: Integer;
begin
  Result := FHFAElementCount;
end;
{$ENDIF CPUARM64 or LINUX64 or OSX64}

{ TRttiDynamicArrayType }

constructor TRttiDynamicArrayType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte( TypeData.DynArrAttrData );
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiDynamicArrayType.GetDeclaringUnitName: string;
begin
  Result := TypeData.DynUnitNameFld.ToString;
end;

function TRttiDynamicArrayType.GetElementSize: Integer;
begin
  Result := TypeData^.elSize;
end;

function TRttiDynamicArrayType.GetElementType: TRttiType;
begin
  Result := Pool.GetType(GetDynArrayElType(Handle));
end;

function TRttiDynamicArrayType.GetOleAutoVarType: TVarType;
begin
  Result := TypeData^.varType;
end;

{ TRttiPointerType }

function TRttiPointerType.GetReferredType: TRttiType;
begin
  Result := Pool.TypeOrNil(TypeData^.RefType);
end;

function TRttiPointerType.GetAttributes: TArray<TCustomAttribute>;
var
  p: PByte;
begin
  if not Assigned(FAttributeGetter) then
  begin
    TMonitor.Enter(Package.FLock);
    try
      if not Assigned(FAttributeGetter) then
      begin
        p := @TypeData^.PtrAttrData;
        FAttributeGetter := LazyLoadAttributes(p);
      end;
    finally
      TMonitor.Exit(Package.FLock);
    end;
  end;
  Result := inherited GetAttributes;
end;


{ TRttiInvokableType }

function TRttiInvokableType.GetReturnType: TRttiType;
begin
  if FProcSig = nil then
    Exit(nil);
  Result := TProcSig(FProcSig).GetReturnType;
end;

function TRttiInvokableType.GetCallingConvention: TCallConv;
begin
  if FProcSig = nil then
    Exit(ccReg);
  Result := TProcSig(FProcSig).Handle^.CC;
end;

function TRttiInvokableType.GetParameters: TArray<TRttiParameter>;
begin
  if FProcSig = nil then
    Exit(nil);
  Result := TProcSig(FProcSig).GetParams;
end;

function TRttiInvokableType.ToString: string;
var
  sig: string;
begin
  if FProcSig = nil then
    sig := ''
  else
    sig := TProcSig(FProcSig).ToString;
  if ReturnType = nil then
    Result := Name + ' = procedure' + sig // do not localize
  else
    Result := Name + ' = function' + sig; // do not localize
end;

{ TRttiMethodType }

constructor TRttiMethodType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
var
  cnt: Integer;
begin
  inherited;

  P := @TypeData^.ParamCount;
  cnt := ReadU8(P);
  while cnt > 0 do
  begin
    Inc(P, SizeOf(TParamFlags));
    P := SkipShortString(P); // ParamName
    P := SkipShortString(P); // ParamType
    Dec(cnt);
  end;
  if MethodKind = mkFunction then
  begin
    P := SkipShortString(P); // PType
    Inc(P, SizeOf(Pointer)); // PTypeRef
  end;
  Inc(P, 1 + TypeData^.ParamCount * SizeOf(Pointer)); // CC + ParamRefs
  if PPointer(P)^ <> nil then
    FProcSig := Pointer(Package.ReadObjectPointer(TProcSig, Self, PPointer(P)^));
  Inc(P, SizeOf(Pointer)); // signature

  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiMethodType.Invoke(const Callable: TValue; const Args: array of TValue): TValue;
var
  meth: TMethod;
  argCount: Integer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
begin
  if not Callable.TryAsTypeInternal(meth, System.TypeInfo(TMethod)) then
    Callable.Cast(Handle).ExtractRawData(@meth);

  parList := GetParameters;
  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  argCount := Length(Args);
  SetLength(argList, argCount + 1);

  currArg := 0;
  PushSelfFirst(CallingConvention, argList, currArg, TValue(meth.Data));

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], CallingConvention);
    Inc(currArg);
  end;

  PushSelfLast(CallingConvention, argList, currArg, meth.Data);

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(meth.Code, argList, CallingConvention, ReturnType.Handle, False) // not static
  else
    Result := System.Rtti.Invoke(meth.Code, argList, CallingConvention, nil);
end;

function TRttiMethodType.ToString: string;
begin
  Result := inherited ToString + ' of object'; // do not localize
end;

function TRttiMethodType.GetMethodKind: TMethodKind;
begin
  Result := TypeData^.MethodKind;
end;

function TRttiMethodType.GetTypeSize: Integer;
begin
  Result := SizeOf(TMethod);
end;

{ TRttiProcedureType }

constructor TRttiProcedureType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  if TypeData^.ProcSig <> nil then
    FProcSig := Pointer(Package.ReadObjectPointer(TProcSig, Self, TypeData^.ProcSig));
end;

function TRttiProcedureType.Invoke(const Callable: TValue; const Args: array of TValue): TValue;
var
  code: Pointer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i: Integer;
begin
  Callable.Cast(Handle).ExtractRawData(@code);
  parList := GetParameters;

  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  SetLength(argList, Length(Args));

  for i := 0 to Length(Args) - 1 do
  begin
    if parList[i].ParamType = nil then
      argList[i] := TValue.From<Pointer>(Args[i].GetReferenceToRawData)
    else if parList[i].Flags * [pfVar, pfOut] <> [] then
    begin
      if parList[i].ParamType.Handle <> Args[i].TypeInfo then
        raise EInvalidCast.CreateRes(@SByRefArgMismatch);
      argList[i] := TValue.From<Pointer>(Args[i].GetReferenceToRawData);
    end
    else if Args[i].TypeInfo = parList[i].ParamType.Handle then
      argList[i] := Args[i]
    else
      argList[i] := Args[i].Cast(parList[i].ParamType.Handle);
  end;

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(code, argList, CallingConvention, ReturnType.Handle)                   
  else
    Result := System.Rtti.Invoke(code, argList, CallingConvention, nil);
end;

function TRttiProcedureType.GetAttributes: TArray<TCustomAttribute>;
var
  p: PByte;
begin
  if not Assigned(FAttributeGetter) then
  begin
    TMonitor.Enter(Package.FLock);
    try
      if not Assigned(FAttributeGetter) then
      begin
        p := @TypeData^.ProcAttrData;
        FAttributeGetter := LazyLoadAttributes(p);
      end;
    finally
      TMonitor.Exit(Package.FLock);
    end;
  end;
  Result := inherited;
end;

{ TRttiAnsiStringType }

function TRttiAnsiStringType.GetCodePage: Word;
begin
                                                                                              
{$IFNDEF NEXTGEN}
  Result := TypeData^.CodePage;
{$ELSE  NEXTGEN}
  Result := 0;
{$ENDIF NEXTGEN}
end;

{ TRttiStringType }

constructor TRttiStringType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  case TypeKind of
{$IFNDEF NEXTGEN}
    tkLString: P := PByte(@TypeData^.CodePage) + SizeOf(TypeData^.CodePage);
    tkString: P := PByte(@TypeData^.MaxLength) + SizeOf(TypeData^.MaxLength);
    tkWString,
{$ENDIF !NEXTGEN}
    tkUString: P := @TypeData^.AttrData;
  end;
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiStringType.GetStringKind: TRttiStringKind;
begin
  case TypeKind of
{$IFNDEF NEXTGEN}
    tkLString:
      Result := skAnsiString;
    tkWString:
      Result := skWideString;
    tkString:
      Result := skShortString;
{$ENDIF !NEXTGEN}
    tkUString:
      Result := skUnicodeString;
  else
    Assert(False);
    Result := skAnsiString;
  end;
end;

function TRttiStringType.GetTypeSize: Integer;
begin
{$IFNDEF NEXTGEN}
  if TypeKind = tkString then
    Result := TypeData^.MaxLength + 1
  else
{$ENDIF !NEXTGEN}
    Result := SizeOf(Pointer);
end;

{ TRttiFloatType }

constructor TRttiFloatType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte(@TypeData^.FloatType) + SizeOf(TypeData^.FloatType);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiFloatType.GetFloatType: TFloatType;
begin
  Result := TypeData^.FloatType;
end;

function TRttiFloatType.GetTypeSize: Integer;
const
  Sizes: array[TFloatType] of Integer = (4, 8, SizeOf(Extended), 8, 8);
begin
  Result := Sizes[FloatType];
end;

procedure RaiseSafeCallError(ResultCode: HResult);
begin
  if Assigned(SafeCallErrorProc) then
    SafeCallErrorProc(ResultCode, ReturnAddress);
  System.Error(reSafeCallError);
end;

// If RegFlag doesn't have any bit, -1 is returned.

function RegFlagToIndex(RegFlag: UInt32): Integer;
begin
  Result := -1;
  while (RegFlag <> 0) do
  begin
    inc(Result);
    RegFlag := RegFlag shr 1;
  end;
end;

function RegDoubleFlagToIndex(RegFlag: UInt32): Integer;
begin
  Result := -1;
  while (RegFlag <> 0) do
  begin
    inc(Result);
    RegFlag := RegFlag shr 2;
  end;
end;

{ Raw invocation logic -- 4 CPU based versions follow -- }
{$IF Defined(CPUX86)} // Raw invocation logic ( 1 of 4 )
type
  TFloatReg = record
    case TFloatType of
      ftSingle: (RegSingle: Single);
      ftDouble: (RegDouble: Double);
      ftExtended: (RegExtended: Extended);
      ftComp: (RegComp: Comp);
      ftCurr: (RegCurr: Currency);
  end;

  PParamBlock = ^TParamBlock;
  TParamBlock = record
    RegEAX: Integer;
    RegEDX: Integer;
    RegECX: Integer;
    FP: TFloatReg;
    PopStack: byte;
    PopFP: TFloatType;
    StackData: PByte;
    StackDataSize: Integer;
  end;

                                                                                                         
procedure RawInvoke(CodeAddress: Pointer; ParamBlock: PParamBlock);
asm
      PUSH  EBP
      MOV   EBP, ESP

      PUSH  EAX // EBP - 4 = CodeAddress
      PUSH  EBX
      MOV   EBX, EDX // EBX = ParamBlock

      // Copy block to stack
      MOV   ECX, [EBX].TParamBlock.StackDataSize
      TEST  ECX, ECX
      JNZ   @@do_push
      NOP   // no branch opt
      JMP   @@skip_push

@@do_push:
{$IFDEF ALIGN_STACK}
      MOV   EAX, ECX
      AND   EAX, $F
      JZ    @@no_align
      SUB   EAX, 16
      ADD   ESP, EAX
@@no_align:
{$ENDIF ALIGN_STACK}
      // touch stack pages in case it needs to grow
      // while (count > 0) { touch(stack); stack -= 4096; count -= 4096; }
      MOV   EAX, ECX
      JMP   @@touch_loop_begin

@@touch_loop:
      MOV   [ESP],0
@@touch_loop_begin:
      SUB   ESP, 4096
      SUB   EAX, 4096
      JNS   @@touch_loop
      SUB   ESP, EAX

      MOV   EAX, [EBX].TParamBlock.StackData
      MOV   EDX, ESP
      CALL  Move // EAX=source, EDX=dest, ECX=count

@@skip_push:

      // Do call
      MOV   EAX, [EBX].TParamBlock.RegEAX
      MOV   EDX, [EBX].TParamBlock.RegEDX
      MOV   ECX, [EBX].TParamBlock.RegECX
      CALL  [EBP - 4]
      MOV   [EBX].TParamBlock.RegEAX, EAX
      MOV   [EBX].TParamBlock.RegEDX, EDX

      MOVSX EAX, BYTE PTR [EBX].TParamBlock.PopFP
      INC   EAX
      JNZ   @@do_fp_pop
      NOP
      JMP   @@done_fp_pop

@@do_fp_pop:
      DEC   EAX
      // ftSingle, ftDouble, ftExtended, ftComp, ftCurr
      JZ    @@single
      DEC   EAX
      JZ    @@double
      DEC   EAX
      JZ    @@extended

      // => currency or comp
      FISTP QWORD PTR [EBX].TParamBlock.FP.RegCurr
      JMP   @@done_fp_pop

@@single:
      FSTP  DWORD PTR [EBX].TParamBlock.FP.RegSingle
      JMP   @@done_fp_pop

@@double:
      FSTP  QWORD PTR [EBX].TParamBlock.FP.RegDouble
      JMP   @@done_fp_pop

@@extended:
      FSTP  TBYTE PTR [EBX].TParamBlock.FP.RegExtended

@@done_fp_pop:
      MOV   AL, [EBX].TParamBlock.PopStack // 0:no-pop 1: pop 2: pop - 4
      TEST  AL,AL
      JNZ   @@do_pop
      NOP
      JMP   @@done

@@do_pop:
      DEC   AL
      MOV   EAX,[EBX].TParamBlock.StackDataSize
      JZ    @@do_pop2
      SUB   EAX,4  // PopStack = 2. Caller already release the structure-ret pointer.
@@do_pop2:
      ADD   ESP,EAX

@@done:
{$IFDEF ALIGN_STACK}
      MOV   EAX, [EBX].TParamBlock.StackDataSize
      AND   EAX, $F
      JZ    @@no_realign
      SUB   EAX, 16
      SUB   ESP, EAX
@@no_realign:
{$ENDIF ALIGN_STACK}
      POP   EBX
      POP   EAX
      POP   EBP
end;

// CPUX86 raw invocation logic that expects:
// * CodeAddress calculated => no virtuals etc.
// * Arguments *exactly* match parameter types (no conversions)
// * Hidden arguments are included => c/dtor bool, array lengths
// * Managed return types are inferred - passed last as out by-ref arg
function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean; IsConstructor: Boolean): TValue;
const
  regNone = 0;
  regEAX = 1;
  regEDX = 2;
  regECX = 4;
  regAll = 7;

  function CalcStackSize(CC: TCallConv): Integer;
  var
    i: Integer;
  begin
    // Estimate maximum stack usage, assuming everything goes
    // on the stack with 4-byte alignment.
    Result := SizeOf(Pointer); // for potential managed return-value
    for i := 0 to Length(Args) - 1 do
      if PassByRef(Args[i].TypeInfo, CC) then
        Inc(Result, SizeOf(Pointer))
      else
        Inc(Result, Align4(Args[i].DataSize));
  end;

{$IFDEF MACOS}
  function GetValueStackSize(const AValue: TValue): Integer;
  begin
    if (AValue.FTypeInfo = nil) or (AValue.FValueData = nil) then
      Exit(0);
    if (AValue.FTypeInfo^.Kind = tkFloat) and
      (GetTypeData(AValue.FTypeInfo)^.FloatType = ftExtended) then
      Exit(16);
    Result := Align4(AValue.DataSize);
  end;
{$ENDIF MACOS}

var
  block: TParamBlock;
  top: PByte;
  freeRegs: UInt32;

  procedure PutArg(const Arg: TValue);
  var
    dataSize, stackSize: Integer;
    reg: UInt32;
  begin
    dataSize := Arg.DataSize;
{$IFDEF MACOS}
    stackSize := GetValueStackSize(Arg);
{$ELSE}
    stackSize := Align4(dataSize);
{$ENDIF}
    if (dataSize in [1, 2, 4]) and not (Arg.Kind = tkFloat) then
    begin
      reg := AllocReg(freeRegs);
      if reg <> regNone then
      begin
        case reg of
          regEAX: Arg.ExtractRawDataNoCopy(@block.RegEAX);
          regEDX: Arg.ExtractRawDataNoCopy(@block.RegEDX);
          regECX: Arg.ExtractRawDataNoCopy(@block.RegECX);
        end;
        Exit;
      end;
    end;
    // No register => stack
    Dec(top, stackSize);
    Arg.ExtractRawDataNoCopy(top);
  end;

  procedure PutRefArg(const Loc: Pointer);
  var
    reg: UInt32;
  begin
    reg := AllocReg(freeRegs);
    if reg <> regNone then
    begin
      case reg of
        regEAX: Pointer(block.RegEAX) := Loc;
        regEDX: Pointer(block.RegEDX) := Loc;
        regECX: Pointer(block.RegECX) := Loc;
      end;
      Exit;
    end;
    Dec(top, SizeOf(Pointer));
    PPointer(top)^ := Loc;
  end;

var
  stackSize: Integer;
  stackArr: array [0 .. 511] of Byte;
  stackData: PByte;
  i: Integer;
  argCnt: Integer;
begin
  FillChar(block, SizeOf(block), 0);
  stackSize := CalcStackSize(CallingConvention);
  if stackSize > SizeOf(stackArr) then
    GetMem(stackData, stackSize)
  else
    stackData := @stackArr[0];
  try
    top := @stackData[stackSize];

    if CallingConvention = ccReg then
      freeRegs := regAll
    else
      freeRegs := regNone;

    if (CallingConvention = ccSafeCall) and (AResultType <> nil) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    argCnt := Length(Args);
    // Last argument is Self, but unfortunately, Result parameter (if any) needs
    // to come before Self. Self must be the *very* *last* item.
    if (CallingConvention = ccPascal) and not IsStatic then
      Dec(argCnt);

    if CallingConvention in [ccCdecl, ccStdCall, ccSafeCall] then
      for i := argCnt - 1 downto 0 do
        if PassByRef(Args[i].TypeInfo, CallingConvention) then
          PutRefArg(Args[i].GetReferenceToRawData)
        else
          PutArg(Args[i])
    else
      for i := 0 to argCnt - 1 do
        if PassByRef(Args[i].TypeInfo, CallingConvention) then
          PutRefArg(Args[i].GetReferenceToRawData)
        else
          PutArg(Args[i]);

    if (CallingConvention <> ccSafeCall) and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    // Now that Result parameter (if any) is in place we can finally move Self in.
    if (CallingConvention = ccPascal) and not IsStatic then
      if PassByRef(Args[argCnt].TypeInfo, CallingConvention) then
        PutRefArg(Args[argCnt].GetReferenceToRawData)
      else
        PutArg(Args[argCnt]);

  {$IFDEF MACOS}
    if CallingConvention = ccCdecl then
    begin
      block.PopStack := 1;  // Callee POP the stack.
      if (AResultType <> nil) and (AResultType^.Kind in [tkRecord, tkMRecord]) then
      begin
        case GetTypeData(AResultType)^.RecSize of
          0,1,2,4,8:
           {block.PopStack := 1}; // already initialized.
          else
            block.PopStack := 2;  // Callee POP the stack and struct-ret pointer.
        end;
      end;
    end
    else
      block.PopStack := 0; // Caller POP the stack.
  {$ELSE !MACOS}
    if CallingConvention = ccCdecl then
      block.PopStack := 1  // Callee POP the stack.
    else
      block.PopStack := 0; // Caller POP the stack.
  {$ENDIF MACOS}

    block.StackData := top;
    block.StackDataSize := PByte(@stackData[stackSize]) - top;

    if (AResultType <> nil) and (AResultType^.Kind = tkFloat) and (CallingConvention <> ccSafeCall) then
      block.PopFP := GetTypeData(AResultType)^.FloatType
    else
      block.PopFP := TFloatType(-1);

    RawInvoke(CodeAddress, @block);

    if (CallingConvention = ccSafeCall) and (block.RegEAX < 0)  then
      RaiseSafeCallError(block.RegEAX);

    if AResultType = nil then
      Result := TValue.Empty
    else if CallingConvention <> ccSafeCall then
    begin
      if UseResultPointer(AResultType, IsConstructor, CallingConvention) then
        // do nothing
      else if block.PopFP <> TFloatType(-1) then
        TValue.MakeWithoutCopy(@block.FP, AResultType, Result)
      else
        TValue.Make(@block.RegEAX, AResultType, Result);
    end;
  finally
    if stackData <> @stackArr[0] then
      FreeMem(stackData);
  end;
end; // of CPUX86 Invoke

{$ELSEIF Defined(CPUX64)} // Raw invocation logic ( 2 of 4 )
{$IF Defined(MSWINDOWS)}
type
  PParamBlock = ^TParamBlock;
  TParamBlock = record
    RegRCX: Int64;
    RegRDX: Int64;
    RegR8: Int64;
    RegR9: Int64;
    StackData: PByte;
    StackDataSize: Integer;
    OutRAX: Int64;
    OutXMM0: Double;
  end;

procedure RawInvoke(CodeAddress: Pointer; ParamBlock: PParamBlock);
{$IFDEF ASSEMBLER}
  procedure InvokeError;
  begin
    raise EInvocationError.CreateRes(@SParameterCountExceeded);
  end;
asm
      .PARAMS 62 // There's actually room for 64, assembler is saving locals for CodeAddress & ParamBlock
      MOV     [RBP+$210], CodeAddress                                                 
      MOV     [RBP+$218], ParamBlock
      MOV     EAX, [ParamBlock].TParamBlock.StackDataSize
      TEST    EAX, EAX                                          
      JZ      @@skip_push
      CMP     EAX, 480 // (64-4) params * 8 bytes.
      JBE     @@valid_frame

      Call InvokeError
@@valid_frame:
      // All items on stack should be 16 byte aligned. Caller should
      // have handled that, just copy data here.
      MOV     RCX, [ParamBlock].TParamBlock.StackData
      LEA     RDX, [RBP+$20]
      MOVSX   R8,  EAX                                      
      CALL    Move     // RCX: source, RDX: dest, R8, count
      MOV     RDX, [RBP+$218]

@@skip_push:
      MOV     RCX, [RDX].TParamBlock.RegRCX
      MOV     R8,  [RDX].TParamBlock.RegR8
      MOV     R9,  [RDX].TParamBlock.RegR9

      MOVSD   XMM0,[RDX].TParamBlock.RegRCX
      MOVSD   XMM1,[RDX].TParamBlock.RegRDX
      MOVSD   XMM2,[RDX].TParamBlock.RegR8
      MOVSD   XMM3,[RDX].TParamBlock.RegR9

      MOV     RDX, [RDX].TParamBlock.RegRDX

      CALL    [RBP+$210]

      MOV     RDX, [RBP+$218]
      MOV     [RDX].TParamBlock.OutRAX, RAX
      MOVSD   [RDX].TParamBlock.OutXMM0, XMM0

end;
{$ELSE  ASSEMBLER}
begin
                                                                              
end;
{$ENDIF ASSEMBLER}

// CPUX64 raw invocation logic that expects:
// * CodeAddress calculated => no virtuals etc.
// * Arguments *exactly* match parameter types (no conversions)
// * Hidden arguments are included => c/dtor bool, array lengths
// * Managed return types are inferred - passed either first (IsStatic = True) or second (IsStatic = False)
function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean; IsConstructor: Boolean): TValue;
const
  regNone = 0;
  regRCX  = 1;
  regRDX  = 2;
  regR8   = 4;
  regR9   = 8;
  regAll  = 15;

  function CalcStackSize: Integer;
  var
    i: Integer;
  begin
    // Estimate maximum stack usage, assuming everything goes
    // on the stack with 4-byte alignment.
    Result := SizeOf(Pointer); // for potential managed return-value
    for i := 4 to Length(Args) - 1 do
      if PassByRef(Args[i].TypeInfo, CallingConvention) then
        Inc(Result, SizeOf(Pointer))
      else
        Inc(Result, Align8(Args[i].DataSize));
  end;

var
  stackSize: Integer;
  stackArr: array [0 .. 511] of Byte;
  stackData: PByte;
  block: TParamBlock;
  top: PByte;
  freeRegs: UInt32;

  procedure PutArg(const Arg: TValue);
  var
    dataSize: Integer;
    reg: UInt32;
  begin
    dataSize := Arg.DataSize;
    if (dataSize in [1, 2, 4, 8]) then
    begin
      reg := AllocReg(freeRegs);
      if reg <> regNone then
      begin
        case reg of
          regRCX: Arg.ExtractRawDataNoCopy(@block.RegRCX);
          regRDX: Arg.ExtractRawDataNoCopy(@block.RegRDX);
          regR8: Arg.ExtractRawDataNoCopy(@block.RegR8);
          regR9: Arg.ExtractRawDataNoCopy(@block.RegR9);
        end;
        Exit;
      end;
    end;
    // No register => stack
    Arg.ExtractRawDataNoCopy(top);
    Inc(top, Align8(dataSize));
  end;

  procedure PutRefArg(const Loc: Pointer);
  var
    reg: UInt32;
  begin
    reg := AllocReg(freeRegs);
    if reg <> regNone then
    begin
      case reg of
        regRCX: Pointer(block.RegRCX) := Loc;
        regRDX: Pointer(block.RegRDX) := Loc;
        regR8: Pointer(block.RegR8) := Loc;
        regR9: Pointer(block.RegR9) := Loc;
      end;
      Exit;
    end;
    PPointer(top)^ := Loc;
    Inc(top, SizeOf(Pointer));
  end;

var
  i: Integer;
begin
  FillChar(block, SizeOf(block), 0);
  stackSize := CalcStackSize();
  if stackSize > SizeOf(stackArr) then
    GetMem(stackData, stackSize)
  else
    stackData := @stackArr[0];
  try
    top := @stackData[0];

    freeRegs := regAll;

    if (CallingConvention <> ccSafeCall) and IsStatic and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    if Length(Args) > 0 then
      if PassByRef(Args[0].TypeInfo, CallingConvention) then
        PutRefArg(Args[0].GetReferenceToRawData)
      else
        PutArg(Args[0]);

    if (CallingConvention <> ccSafeCall) and not IsStatic and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    for i := 1 to Length(Args) - 1 do
      if PassByRef(Args[i].TypeInfo, CallingConvention) then
        PutRefArg(Args[i].GetReferenceToRawData)
      else
        PutArg(Args[i]);

    if (CallingConvention = ccSafeCall) and (AResultType <> nil) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    block.StackData := @stackData[0];
    block.StackDataSize := top - PByte(@stackData[0]);

    RawInvoke(CodeAddress, @block);

    if (CallingConvention = ccSafeCall) and (HResult(block.OutRAX) < 0) then
      RaiseSafeCallError(HResult(block.OutRAX));

    if AResultType = nil then
      Result := TValue.Empty
    else
    if CallingConvention <> ccSafeCall then
    begin
      if UseResultPointer(AResultType, IsConstructor, CallingConvention) then
        // do nothing
      else if (AResultType^.Kind = tkFloat) and
        (AResultType^.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
        TValue.MakeWithoutCopy(@block.OutXMM0, AResultType, Result)
      else
        TValue.Make(@block.OutRAX, AResultType, Result);
    end;
  finally
    if stackData <> @stackArr[0] then
      FreeMem(stackData);
  end;
end; // of CPUX64 Invoke

{$ELSEIF Defined(MACOS64) or Defined(LINUX64)}
const
  MaxXMMRegCount = 8;
type
  PParamBlock = ^TParamBlock;
  TParamBlock = record
    RegXMM: array [0..MaxXMMRegCount - 1] of Double; {XMM0-XMM7}
    RegR: array [0..5] of Int64;    {RDI, RSI, RDX, RCX, R8, R9}
    OutRAX: Int64;
    OutRDX: Int64;
    OutFPU: Extended;
    StackData: PByte;
    StackDataSize: Integer;
  end;

procedure RawInvoke(CodeAddress: Pointer; ParamBlock: PParamBlock);
  external RTLHelperLibName name 'rtti_raw_invoke';

type
  TStackHelper = record
    FBuffer: TArray<Byte>;
    FGrowSize: Cardinal;
    FTop: PByte;
    FSize: Cardinal;
    procedure InternalGrow(ANewSize: Cardinal);
    function CalcAlign(ASize, Align: Cardinal): Cardinal; inline;
    function GetInit: PByte;
  public
    { AGrowSize is the block size used to realloc the internal buffer  }
    class function Create(AGrowSize: Integer = 128): TStackHelper; static;
    { Alloc space in the stack, if Align > 0 the stack will be aligned to the given value
      Returns the previous pointer before to perform the alloc, so it can be used to store data }
    function Alloc(ASize: Cardinal; Align: Cardinal = 0): PByte;
    { Aligns the stack to the given value }
    procedure Align(A: Cardinal);
    { Current Stack Pointer }
    property Top: PByte read FTop;
    { Base Stack Pointer }
    property Init: PByte read GetInit;
    { Total size allocated }
    property Size: Cardinal read FSize write FSize;
  end;

  class function TStackHelper.Create(AGrowSize: Integer): TStackHelper;
  begin
    Result.FTop := nil;
    Result.FSize := 0;
    Result.FGrowSize := AGrowSize;
  end;

  procedure TStackHelper.InternalGrow(ANewSize: Cardinal);
  begin
    if ANewSize > (Length(FBuffer) + FGrowSize) then
      SetLength(FBuffer, ANewSize)
    else
      SetLength(FBuffer, (Length(FBuffer) + FGrowSize));
    FTop := PByte(NativeUInt(@FBuffer[0]) + FSize);
  end;

  function TStackHelper.Alloc(ASize: Cardinal; Align: Cardinal): PByte;
  var
    NewSize: Cardinal;
  begin
    NewSize := CalcAlign(FSize + ASize, Align);
    if NewSize > Length(FBuffer) then
      InternalGrow(NewSize);

    Result := FTop;
    FTop := PByte(NativeUInt(@FBuffer[0]) + NewSize);
    FSize := NewSize;
  end;

  function TStackHelper.CalcAlign(ASize, Align: Cardinal): Cardinal;
  begin
   if Align > 0 then
      Result := (ASize + (Align - 1)) and not (Align - 1)
    else
      Result := ASize; // no align
  end;

  function TStackHelper.GetInit: PByte;
  begin
    Result := @FBuffer[0]
  end;

  procedure TStackHelper.Align(A: Cardinal);
  begin
    Alloc(0, A);
  end;

function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>; CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False;
  IsConstructor: Boolean = False ): TValue;

var
  Block: TParamBlock;
  Stack: TStackHelper;
  FreeRRegs, FreeXMMregs: UInt32;
  RttiContext: TRttiContext;

  procedure PutRefArg(const Loc: Pointer); forward;
  procedure PutArg(const Arg: TValue);
  var
    i: Integer;
    DataSize: Integer;
    RttiType: TRttiType;
    Reg: UInt32;
    RegIndex: UInt32;
    NextReg: UInt32;
    NeededRegs: UInt32;
    Offset: Integer;
    tk: TTypeKind;
    src: Pointer;
    dst: Pointer;
  begin
    DataSize := Arg.DataSize;
    RttiType := RttiContext.GetType(Arg.TypeInfo);

    tk := Arg.GetTypeKind;

    // Single / Double
    if (tk = tkFloat) and (Arg.TypeData.FloatType in [ftSingle, ftDouble]) then
    begin
      Reg := AllocReg(FreeXMMregs);
      if Reg <> 0 then
      begin
        Arg.ExtractRawData(@Block.RegXMM[RegFlagToIndex(Reg)]);
        Exit;
      end;
    end
    // Extended
    else if (tk = tkFloat) and (Arg.TypeData.FloatType in [ftExtended]) then
    begin
      Stack.Align(16);
      Arg.ExtractRawDataNoCopy(Stack.Alloc(DataSize, 16));
      Exit;
    end

    // Integer and aggregates
    else if DataSize <= 16 then
    begin
//      CheckHFA(RttiType, False);

      if RttiType.IsHFA then
      begin
        NeededRegs := RttiType.HFAElementCount;
        if RttiType.HFAElementType.FloatType = ftSingle then
          NeededRegs := (NeededRegs + 1) div 2;

        src := Arg.GetReferenceToRawData;
        Offset := 0;
        if RttiType.HFAElementType.FloatType = ftSingle then
        begin
          for i := 0 to NeededRegs - 1 do
          begin
            Reg := AllocReg(FreeXMMregs);
            if Reg <> 0 then
            begin
              RegIndex := RegFlagToIndex(Reg);
              dst := @Block.RegXMM[RegIndex];
            end
            else
              dst := Stack.Alloc(8, 8);
            Move((PByte(src) + Offset)^, dst^, Min(8, DataSize - Offset));
            Inc(Offset, 8);
          end;
        end
        else
        begin
          for i := 0 to NeededRegs - 1 do
          begin
            Reg := AllocReg(FreeXMMregs);
            if Reg <> 0 then
            begin
              RegIndex := RegFlagToIndex(Reg);
              dst := @Block.RegXMM[RegIndex];
            end
            else
              dst := Stack.Alloc(SizeOf(Double), SizeOf(Double));

            Move((PByte(src) + Offset)^, dst^, Min(SizeOf(Double), DataSize - Offset));
            Inc(Offset, SizeOf(Double));
          end;
        end;

        Exit;
      end
      else if (DataSize <= 8) then
      begin
        Reg := AllocReg(FreeRRegs);
        if Reg <> 0 then
        begin
          Arg.ExtractRawDataNoCopy(@Block.RegR[RegFlagToIndex(Reg)]);
          Exit;
        end;
      end
      else
      begin
        Reg := AllocReg(FreeRRegs);
        if Reg <> 0 then
        begin
          NextReg := AllocReg(FreeRRegs);
          if NextReg <> 0 then
          begin
            Arg.ExtractRawDataNoCopy(@Block.RegR[RegFlagToIndex(Reg)]);
            Exit;
          end;
        end;
      end;
    end;
    // No register => stack
    Arg.ExtractRawDataNoCopy(Stack.Alloc(DataSize, 8));
  end;

  procedure PutRefArg(const Loc: Pointer);
  var
    Reg: UInt32;
  begin
    Reg := AllocReg(FreeRRegs);
    if Reg <> 0 then
    begin
      Pointer(Block.RegR[RegFlagToIndex(Reg)]) := Loc;
      Exit;
    end;
    PPointer(Stack.Alloc(SizeOf(Pointer)))^ := Loc;
  end;

var
  i: Integer;
  RttiType: TRttiType;
  IsResultPointer: Boolean;
  sBuf: array [0..15] of UInt32; // for Single HFA result
  dBuf: array [0..7]  of UInt64; // for Double HFA result
begin
  FillChar(Block, SizeOf(Block), 0);
  Stack := TStackHelper.Create;

  FreeRRegs := $3F;
  FreeXMMRegs := $FF;

  RttiContext := TRttiContext.Create;
  try

    IsResultPointer := False;
    if (AResultType <> nil) then
    begin
      TValue.Make(nil, AResultType, Result);
      RttiType := RttiContext.GetType(AResultType);

      IsResultPointer := (CallingConvention <> ccSafeCall)
        and (UseResultPointer(AResultType, IsConstructor, CallingConvention)
        and not (RttiType.IsHFA and (Result.DataSize <= 16)));

      if IsResultPointer then
        PutRefArg(Result.GetReferenceToRawData);
    end
    else
      RttiType := nil;

    for I := 0 to Length(Args) - 1 do
      if PassByRef(Args[I].TypeInfo, CallingConvention) then
        PutRefArg(Args[I].GetReferenceToRawData)
      else
        PutArg(Args[I]);

    if (CallingConvention = ccSafeCall) and (AResultType <> nil)  then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    // If the return parameter is Extended then it will be allocated in FPU stack
    // So put a dummy value to do a check in rtti_raw_invoke
    if (AResultType <> nil) and (AResultType^.Kind = tkFloat) and (AResultType.TypeData.FloatType in [ftExtended]) then
      block.OutFPU := 1;

    // Ensure the stack is aligned to 16
    Stack.Align(16);
    block.StackData := Stack.Init;
    block.StackDataSize := Stack.Size;

    RawInvoke(CodeAddress, @block);

    if (CallingConvention = ccSafeCall) and (HResult(block.OutRAX) < 0) then
      RaiseSafeCallError(HResult(block.OutRAX));

    if AResultType = nil then
      Result := TValue.Empty
    else
    if CallingConvention <> ccSafeCall then
    begin
      if (IsResultPointer) then
        // do nothing
      else
      if (AResultType^.Kind = tkFloat) and (AResultType^.TypeData.FloatType in [ftSingle, ftDouble]) then
        TValue.MakeWithoutCopy(@block.RegXMM[0], AResultType, Result)
      else if (AResultType^.Kind = tkFloat) and (AResultType^.TypeData.FloatType in [ftExtended]) then
        TValue.MakeWithoutCopy(@block.OutFPU, AResultType, Result)
      else
      begin
//        if (Result.DataSize <= 16) then
//        begin
//          CheckHFA(RttiType, True);
//        end;

        if RttiType.IsHFA and (Result.DataSize <= 16) then
        begin
          if RttiType.GetHFAElementType.FloatType = ftSingle then
          begin
            for i := 0 to Length(block.RegXMM) - 1 do
            begin
              Move(block.RegXMM[i], sBuf[i * 2], 8);
            end;
            TValue.MakeWithoutCopy(@sBuf[0], AResultType, Result);
          end
          else // Double
          begin
            for i := 0 to Length(block.RegXMM) - 1 do
            begin
              Move(block.RegXMM[i], dBuf[i], SizeOf(Double));
            end;
            TValue.MakeWithoutCopy(@dBuf[0], AResultType, Result);
          end;
        end
        else
          TValue.Make(@block.OutRAX, AResultType, Result);
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Missing invocation logic for CPU'}
{$ENDIF}

{$ELSEIF Defined(CPUARM32)} // Raw invocation logic ( 3 of 4 )

type
  PParamBlock = ^TParamBlock;
  TParamBlock = record
    RegCR : array[0..3] of Int32;

    StackData: PByte;
    StackDataSize: Integer;

    case Integer of
      0: ( RegD: array[0..7] of Double );
      1: ( RegS: array[0..15] of Single );
  end;

procedure RawInvoke(CodeAddress: Pointer; ParamBlock: PParamBlock);
  external RTLHelperLibName name {$IFDEF LINUX} '_' + {$ENDIF} 'rtti_raw_invoke';

{$IF not defined(IOS)}
  function AllocEvenReg(var Regs: UInt32): UInt32;
  begin
    Result := AllocReg(Regs);
    // If get a odd reg, alloc a reg again
    //  0002 - R1
    //  0008 - R3
    if (Result and ($2 + $8) <> 0) then
      Result := AllocReg(Regs);
  end;
{$ENDIF !IOS}

function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean; IsConstructor: Boolean): TValue;

  function CalcStackSize: Integer;
  var
    i: Integer;
    FreeCR,
    FreeVFP: Integer;
  begin
    // Estimate maximum stack usage, assuming everything goes
    // on the stack with 4-byte alignment.
    Result := SizeOf(Pointer); // for potential managed return-value
//    FreeCR := 4; // Number of core registers. R0 - R3
    FreeCR := 0; // Number of core registers. R0 - R3
    FreeVFP := 0; // Number of VFP(FP) registers. Default is 0
{$IFDEF ANDROID}
    if CallingConvention in [ccReg] then
      FreeVFP := 8; // D0-D7
{$ENDIF ANDROID}

    for i := 0 to Length(Args) - 1 do
      if PassByRef(Args[i].TypeInfo, CallingConvention) then
      begin
        if FreeCR > 0 then
          Dec(FreeCR) // use core register.
        else
          Inc(Result, SizeOf(Pointer))
      end
      else
      begin
        if Args[i].GetTypeKind = tkFloat then
        begin
          if FreeVFP > 0 then // use VFP register.
            Dec(FreeVFP)
          else
            Inc(Result, SizeOf(Double));
        end
        else if Args[i].GetTypeKind = tkInt64 then
        begin
          if FreeCR >= 2 then // use 2 core registers.
            Dec(FreeCR, 2)
          else if FreeCR = 1 then // use last register and stack
          begin
            FreeCR := 0;
            Inc(Result, SizeOf(Int32));
          end
          else
            Inc(Result, SizeOf(Int64));
        end
        else
        begin
          if (Args[i].DataSize <= 4) and (FreeCR > 0) then
            Dec(FreeCR)
          else
            Inc(Result, Align4(Args[i].DataSize));
        end;
      end;
  end;

const
  regNone = $00;
  regCRAll  = $0F;
  regFPRAll  = $FFFF;

var
  stackData: array of byte;
  block: TParamBlock;
  top: PByte;
  freeCRegs: UInt32;  // 4-core registers (32bit)
  freeFPRegs: UInt32; // 16-Single VFP registers (32bit)
                      //  8-Double VFP registers (64bit)
  src: PByte;

  function AllocDoubleReg: UInt32;
  var
    freeDoubleReg: Uint32;
  begin
    Result := 0;
    freeDoubleReg := freeFPRegs and $55555555; // remove odd FP registers.
    // no free Double register.
    if freeDoubleReg = 0 then Exit;
    // get a free single register at even index.
    Result := not(freeDoubleReg and (freeDoubleReg - 1)) and freeDoubleReg;
    // Remove two Single registers from FreeFPRegs;
    freeFPRegs := freeFPRegs and not( Result or Result shl 1);
  end;

  procedure PutArg(const Arg: TValue);
  var
    dataSize: Integer;
    reg,
    regL, regH: UInt32;
    L32, H32: UInt32;
    U64: UInt64;
  begin
    dataSize := Arg.DataSize;
    if (Arg.GetTypeKind = tkFloat) and (Arg.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
    begin
      if dataSize = 4 then // Single
      begin
        // First, allocate one single VFP register.
        reg := AllocReg(freeFPRegs);
        if reg <> 0 then
        begin
          Arg.ExtractRawData(@block.RegS[RegFlagToIndex(reg)]);
          Exit;
        end;
      end
      else if Arg.DataSize = 8 then // Double and Extended
      begin
        // First, allocate one Double VFP register.
        reg := AllocDoubleReg;
        if reg <> 0 then
        begin
          Arg.ExtractRawData(@block.RegD[RegDoubleFlagToIndex(reg)]);
          Exit;
        end;
      end;
    end
    else if (Arg.GetTypeKind in [tkRecord, tkMRecord]) or (Arg.GetTypeKind = tkArray) then
    begin
      src := Arg.GetReferenceToRawData;
      while datasize > 0 do
      begin
        reg := AllocReg(freeCRegs);
        if reg <> regNone then
        begin
          Move(src^, block.RegCR[RegFlagToIndex(reg)], 4);
        end
        else
        begin
          Move(src^, top^, 4);
          //Inc(top, Align4(dataSize));
          Inc(top, 4);
        end;
        Dec(dataSize, 4);
        Inc(Src, 4);
      end;
      Exit;
    end;

    if (dataSize in [1, 2, 4]) then
    begin
      reg := AllocReg(freeCRegs);
      if reg <> regNone then
      begin
        Arg.ExtractRawDataNoCopy(@block.RegCR[RegFlagToIndex(reg)]);
        Exit;
      end;
      Arg.ExtractRawDataNoCopy(top);
      Inc(top, Align4(dataSize));
    end
    else if dataSize = 8 then // 64bit data
    begin
      // Next, allocate two core register
      {$IFDEF IOS}
      regL := AllocReg(freeCRegs);
      {$ELSE}
      regL := AllocEvenReg(freeCRegs);
      {$ENDIF}
      regH := AllocReg(freeCRegs);
      if (Arg.GetTypeKind = tkFloat) then
      begin
        case Arg.TypeData.FloatType of
          ftSingle,
          ftDouble,
          ftExtended:
            PDouble(@U64)^ := Arg.AsExtended;
          ftComp,
          ftCurr:
            Arg.ExtractRawDataNoCopy(@U64);
        end;
      end
      else
        U64 := Arg.AsUInt64;
      L32 := U64 and $FFFFFFFF;
      H32 := (U64 shr 32) and $FFFFFFFF;
      if regL <> 0 then
      begin
        block.RegCR[ RegFlagToIndex(regL)] := L32;
        if regH <> 0 then
          block.RegCR[ RegFlagToIndex(regH)] := H32
        else // regH = 0;
        begin
          PCardinal(top)^ := H32;
          Inc(top, SizeOf(H32)); // 4
        end;
      end
      else // if regL = 0, regH also 0.
      begin
{$IF not defined(IOS)}
        if ((NativeInt(top) - NativeInt(@stackData[0])) mod 8) <> 0 then
          Inc(top, 4); // Set 8 byte align
{$ENDIF !IOS}
        PCardinal(top)^ := L32;
        Inc(top, SizeOf(L32)); // 4
        PCardinal(top)^ := H32;
        Inc(top, SizeOf(H32)); // 4
      end;
    end
    else
      Assert(False, 'somethig wrong');
  end;

  procedure PutRefArg(const Loc: Pointer);
  var
    reg: UInt32;
  begin
    reg := AllocReg(freeCRegs);
    if reg <> regNone then
    begin
      block.RegCR[ RegFlagToIndex(reg)] := UInt32(Loc);
      Exit;
    end;
    PPointer(top)^ := Loc;
    Inc(top, SizeOf(Pointer));
  end;

var
  i : integer;
begin
  FillChar(block, SizeOf(block), 0);
  SetLength(stackData, CalcStackSize);
  top := @stackData[0];

  freeCRegs := regCRAll;
  freeFPRegs := regNone;
{$IF not defined(ARM_NO_VFP_USE)}
  if CallingConvention in [ccReg] then
    freeFPRegs := regFPRAll;
{$ENDIF !ARM_NO_VFP_USE}

  if IsStatic then
  begin
    if (CallingConvention <> ccSafeCall) and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    if Length(Args) > 0 then
      if PassByRef(Args[0].TypeInfo, CallingConvention) then
        PutRefArg(Args[0].GetReferenceToRawData)
      else
        PutArg(Args[0]);
  end
  else
  begin // not IsStatic / It class method

    // Put result first.
    if (CallingConvention <> ccSafeCall) and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      PutRefArg(Result.GetReferenceToRawData);
    end;

    // First arg is "self". place to 2nd.
    if Length(Args) > 0 then
      if PassByRef(Args[0].TypeInfo, CallingConvention) then
        PutRefArg(Args[0].GetReferenceToRawData)
      else
        PutArg(Args[0]);
  end;

  for i := 1 to Length(Args) - 1 do
    if PassByRef(Args[i].TypeInfo, CallingConvention) then
      PutRefArg(Args[i].GetReferenceToRawData)
    else
      PutArg(Args[i]);

  if (CallingConvention = ccSafeCall) and (AResultType <> nil)  then
  begin
    TValue.Make(nil, AResultType, Result);
    PutRefArg(Result.GetReferenceToRawData);
  end;

  block.StackData := @stackData[0];
  block.StackDataSize := top - PByte(@stackData[0]);

// From http://infocenter.arm.com/help/topic/com.arm.doc.ihi0042f/IHI0042F_aapcs.pdf
// 5.2.1.2 Stack constraints at a public interface
// The stack must also conform to the following constraint at a public interface:
//   SP mod 8 = 0. The stack must be double-word aligned.
//
// But for IOS: https://developer.apple.com/library/ios/documentation/Xcode/Conceptual/iPhoneOSABIReference/Articles/ARMv6FunctionCallingConventions.html
// The stack is 4-byte aligned at the point of function calls.
{$IF not defined(IOS)}
  if (block.StackDataSize mod 8) <> 0 then
    Inc(block.StackDataSize, 4); // Set 8 byte align
{$ENDIF !IOS}

  RawInvoke(CodeAddress, @block);

  if (CallingConvention = ccSafeCall) and (block.RegCR[0] < 0)  then
    RaiseSafeCallError(block.RegCR[0]);

  if AResultType = nil then
    Result := TValue.Empty
  else
  if CallingConvention <> ccSafeCall then
  begin
    if UseResultPointer(AResultType, IsConstructor, CallingConvention) then
      // do nothing
{$IF not defined(ARM_NO_VFP_USE)}
    else if (CallingConvention = ccReg) and
            (AResultType^.Kind = tkFloat) and
            (AResultType.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
      TValue.MakeWithoutCopy(@block.RegD[0], AResultType, Result)
{$ENDIF !ARM_NO_VFP_USE}
    else
      TValue.Make(@block.RegCR[0], AResultType, Result);
  end;
end;

{$ELSEIF Defined(CPUARM64)} // Raw invocation logic ( 4 of 4 )
type
  m128 = record
    case Integer of
      1: (Lo, Hi: UInt64);
      2: (LL, LH, HL, HH: UInt32);
  end align 16;

  PParamBlock = ^TParamBlock;
  TParamBlock = record
    RegX : array[0..7] of Int64;
    RegQ : array[0..7] of m128;
    RegX8: Int64;
    StackData: PByte;
    StackDataSize: Integer;
  end;

procedure RawInvoke(CodeAddress: Pointer; ParamBlock: PParamBlock);
  external RTLHelperLibName name 'rtti_raw_invoke';

function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean; IsConstructor: Boolean): TValue;

  function CalcStackSize: Integer;
  begin
    // Estimate maximum stack usage, assuming everything goes
    // on the stack with 16-bytes (SP mod 16 = 0) at public interface (CPPABI64 5.2.2.2)
    Result := SizeOf(Pointer); // for potential managed return-value
    // Dymmy allocation
    Result := Result + 128;
    Result := Align16(Result);
  end;

var
  ctx: TRTTIContext;
  stackData: array of byte;
  block: TParamBlock;
  top: PByte;
  nextGR: Integer;  // Next General-purpose Register Number. 8 (64bits) registers are avaulable.
  nextFR: Integer;  // Next Floating-point Register Number. 8 (128bits) registers are avaulable.
const
  maxRegister = 8;

  procedure PutArg(const Arg: TValue);
  var
    dataSize: Integer;
    i: Integer;
    src: PByte;
    T: TRttiType;
  begin
    dataSize := Arg.DataSize;
    T := ctx.GetType(Arg.TypeInfo);
    if (Arg.GetTypeKind = tkFloat) and (Arg.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
    begin
      if nextFR < maxRegister then
      begin
        Arg.ExtractRawData(@block.RegQ[nextFR]);
        Inc(nextFR);
      end
      else
      begin
{$IF Defined(CPUARM64) and Defined(MACOS64)}
        case Arg.TypeData.FloatType of
          ftSingle:
            begin
              top := AlignPointer(top, Sizeof(Single));
              Arg.ExtractRawDataNoCopy(top);
              Inc(top, Sizeof(Single));
            end;
          ftDouble,
          ftExtended:
            begin
{$IF SizeOf(Double) <> SizeOf(Extended)}
  {$MESSAGE Fatal 'Unknown combination'}
{$ENDIF}
              top := AlignPointer(top, Sizeof(Double));
              Arg.ExtractRawDataNoCopy(top);
              Inc(top, Sizeof(Double));
            end;
        end;
{$ELSEIF Defined(ANDROID64)}
        Arg.ExtractRawDataNoCopy(top);
        Inc(top, 8);
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}
      end;
      Exit;
    end
    else if T.IsHFA then
    begin
      src := Arg.GetReferenceToRawData;
      if nextFR + T.HFAElementCount <= maxRegister then
      begin
        if T.HFAElementType.FloatType = ftSingle then
        begin
          Assert(T.HFAElementType.TypeSize = sizeof(Single));
          Assert(SizeOf(Cardinal) = sizeof(Single));
          for i := 0 to T.HFAElementCount - 1 do
            block.RegQ[nextFR + i].LL := PCardinal(src + i * SizeOf(Single))^;
        end
        else
        begin
          Assert(T.HFAElementType.TypeSize = sizeof(Double));
          Assert(SizeOf(UInt64) = sizeof(Double));
          for i := 0 to T.HFAElementCount - 1 do
            block.RegQ[nextFR + i].Lo := PUInt64(src + i * SizeOf(Double))^;
        end;
        Inc(nextFR, T.HFAElementCount);
      end
      else
      begin
        nextFR := maxRegister;
        Move(src^, top^, T.HFAElementType.TypeSize * T.HFAElementCount);
        Inc(top, T.HFAElementType.TypeSize * T.HFAElementCount);
      end;
    end
    else if (Arg.GetTypeKind in [tkRecord, tkMRecord]) {$IFDEF ANDROID64}or (Arg.GetTypeKind = tkArray){$ENDIF}then
    begin
      src := Arg.GetReferenceToRawData;
      while datasize > 0 do
      begin
        if nextGR < maxRegister then
        begin
          Move(src^, block.RegX[nextGR], SizeOf(Pointer));
          Inc(nextGR);
        end
        else
        begin
{$IF Defined(IOS64)}
          top := AlignPointer(top, SizeOf(Pointer));
{$ENDIF}
          Move(src^, top^, SizeOf(Pointer));
          Inc(top, SizeOf(Pointer));
        end;
        Dec(dataSize, SizeOf(Pointer));
        Inc(Src, SizeOf(Pointer));
      end;
      Exit;
    end;

    if (dataSize in [1, 2, 4, 8]) then
    begin
      if nextGR < maxRegister then
      begin
        Arg.ExtractRawDataNoCopy(@block.RegX[nextGR]);
        Inc(nextGR);
      end
      else
      begin
{$IF Defined(CPUARM64) and Defined(MACOS64)}
        if dataSize = 8 then
          top := AlignPointer(top, 8)
        else if dataSize = 4 then
          top := AlignPointer(top, 4)
        else if dataSize = 2 then
          top := AlignPointer(top, 2);
        Arg.ExtractRawDataNoCopy(top);
        Inc(top, datasize);
{$ELSEIF Defined(ANDROID64)}
        Arg.ExtractRawDataNoCopy(top);
        Inc(top, 8);
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}
      end;
    end
  end;

  procedure PutRefArg(const Loc: Pointer);
  begin
    if nextGR < maxRegister then
    begin
      block.RegX[nextGR] := UIntPtr(Loc);
      Inc(nextGR);
      Exit;
    end;
{$IF Defined(IOS64)}
    top := AlignPointer(top, Sizeof(Pointer));
{$ENDIF}
    PPointer(top)^ := Loc;
    Inc(top, SizeOf(Pointer));
  end;

var
  i : integer;
  T: TRttiType;
  sBuf: array of UInt32; // for Single HFA result
  dBuf: array of UInt64; // for Double HFA result
begin
  FillChar(block, SizeOf(block), 0);
  SetLength(stackData, CalcStackSize);
  top := @stackData[0];
  nextGR := 0;
  nextFR := 0;
  if IsStatic then
  begin
    // Put result first.
    if (CallingConvention <> ccSafeCall) and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      block.RegX8 := UIntPtr(Result.GetReferenceToRawData);
    end;
  end
  else
  begin // not IsStatic / class method
    // Put result first.
    if (CallingConvention <> ccSafeCall) and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
    begin
      TValue.Make(nil, AResultType, Result);
      block.RegX8 := UIntPtr(Result.GetReferenceToRawData);
    end;
    // First arg is "self". place to 2nd.
  end;

  for i := 0 to Length(Args) - 1 do
  begin
    if PassByRef(Args[i].TypeInfo, CallingConvention) then
      PutRefArg(Args[i].GetReferenceToRawData)
    else
      PutArg(Args[i]);
  end;

  if (CallingConvention = ccSafeCall) and (AResultType <> nil)  then
  begin
    TValue.Make(nil, AResultType, Result);
    PutRefArg(Result.GetReferenceToRawData)
  end;

  block.StackData := @stackData[0];
  block.StackDataSize := Align16(top - PByte(@stackData[0]));
  RawInvoke(CodeAddress, @block);

  if (CallingConvention = ccSafeCall) and (HResult(block.RegX[0]) < 0)  then
    RaiseSafeCallError(HResult(block.RegX[0]));
  if AResultType = nil then
    Result := TValue.Empty
  else
  if CallingConvention <> ccSafeCall then
  begin
    T := ctx.GetType(AResultType);

    if UseResultPointer(AResultType, IsConstructor, CallingConvention) then
      // do nothing
    else if (AResultType^.Kind = tkFloat) and
            (AResultType.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
      TValue.MakeWithoutCopy(@block.RegQ[0], AResultType, Result)
    else if T.IsHFA then
    begin
      if T.GetHFAElementType.FloatType = ftSingle then
      begin
        SetLength(sBuf, 4);
        sBuf[0] := block.RegQ[0].LL;
        sBuf[1] := block.RegQ[1].LL;
        sBuf[2] := block.RegQ[2].LL;
        sBuf[3] := block.RegQ[3].LL;
        TValue.MakeWithoutCopy(@sBuf[0], AResultType, Result)
      end
      else // Double
      begin
        SetLength(dBuf, 4);
        dBuf[0] := block.RegQ[0].Lo;
        dBuf[1] := block.RegQ[1].Lo;
        dBuf[2] := block.RegQ[2].Lo;
        dBuf[3] := block.RegQ[3].Lo;
        TValue.MakeWithoutCopy(@dBuf[0], AResultType, Result)
      end;
    end
    else
      TValue.Make(@block.RegX[0], AResultType, Result);
  end;
end;

{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Missing invocation logic for CPU'}
{$ENDIF}

{ TRttiMethod }

destructor TRttiMethod.Destroy;
begin
  if FInvokeInfo <> nil then
    FInvokeInfo.Free;
  inherited;
end;

function TRttiMethod.CreateImplementation(AUserData: Pointer;
  const ACallback: TMethodImplementationCallback): TMethodImplementation;
begin
  Result := TMethodImplementation.Create(AUserData, GetInvokeInfo, ACallback);
end;

function TRttiMethod.GetCodeAddress: Pointer;
begin
  Result := nil;
end;

function TRttiMethod.GetDispatchKind: TDispatchKind;
begin
  Result := dkStatic;
end;

function TRttiMethod.GetHasExtendedInfo: Boolean;
begin
  Result := False;
end;

function TRttiMethod.GetInvokeInfo: TMethodImplementation.TInvokeInfo;
var
  p: TRttiParameter;
  Info: TMethodImplementation.TInvokeInfo;
begin
  if FInvokeInfo <> nil then
    Exit(FInvokeInfo);

  Info := TMethodImplementation.TInvokeInfo.Create(CallingConvention, not IsStatic);
  try
    if not IsStatic then
      if IsClassMethod then
        Info.AddParameter(TypeInfo(TClass), False, True, False, False)
      else
        Info.AddParameter(Parent.Handle, False, True, False, False);

    for p in GetParameters do
      if p.ParamType = nil then
        Info.AddParameter(nil, True, True, False, False)
      else if pfArray in p.Flags then
      begin
        Info.AddParameter(Info.TypeInfos.DefineOpenArray(p.ParamType.Handle, 0), True,
          ([pfVar, pfOut] * p.Flags = []) or (p.ParamType.Handle = System.TypeInfo(TVarRec)),
          [pfOut, pfResult] * p.Flags <> [], True);
        Info.AddParameter(TypeInfo(Integer), False, True, False, False);
      end
      else
        Info.AddParameter(p.ParamType.Handle,
          ([pfVar, pfOut] * p.Flags <> []) or
            PassByRef(p.ParamType.Handle, CallingConvention, pfConst in p.Flags),
          (pfConst in p.Flags) or
            (p.ParamType.Handle^.Kind in [tkRecord, tkMRecord, tkString]) and ([pfVar, pfOut] * p.Flags = []),
          [pfOut, pfResult] * p.Flags <> [], False);

    if ReturnType <> nil then
      Info.ReturnType := ReturnType.Handle;
    Info.Seal;

    if AtomicCmpExchange(Pointer(FInvokeInfo), Pointer(Info), nil) = nil then
      Pointer(Info) := nil // for ARC-based plaforms this will ensure the local var doesn't affect the refcount
    else
      Info.Free;
  except
    Info.Free;
    raise;
  end;
  Result := FInvokeInfo;
end;

function TRttiMethod.GetIsClassMethod: Boolean;
begin
  Result := HasExtendedInfo and (MethodKind in [mkClassProcedure,
    mkClassFunction, mkClassConstructor, mkClassDestructor]);
end;

function TRttiMethod.GetIsConstructor: Boolean;
begin
  Result := HasExtendedInfo and (MethodKind = mkConstructor);
end;

function TRttiMethod.GetIsDestructor: Boolean;
begin
  Result := HasExtendedInfo and (MethodKind = mkDestructor);
end;

function TRttiMethod.GetIsStatic: Boolean;
begin
  Result := False;
end;

function TRttiMethod.GetVirtualIndex: Smallint;
begin
  Result := 0;
end;

function TRttiMethod.Invoke(Instance: TObject; const Args: array of TValue): TValue;
begin
  Result := DispatchInvoke(Instance, Args);
end;

function TRttiMethod.Invoke(Instance: TClass; const Args: array of TValue): TValue;
begin
  Result := DispatchInvoke(Instance, Args);
end;

function TRttiMethod.Invoke(Instance: TValue; const Args: array of TValue): TValue;
begin
  Result := DispatchInvoke(Instance, Args);
end;

procedure TRttiMethod.GetCommonInvokeParams(var isCons, isDest, isStat, isClas: Boolean;
  var callConv: TCallConv);
var
  methKind: TMethodKind;
begin
  if HasExtendedInfo then
  begin
    methKind := MethodKind;
    isCons := methKind = mkConstructor;
    isDest := methKind = mkDestructor;
    isClas := methKind in [mkClassProcedure, mkClassFunction, mkClassConstructor, mkClassDestructor];
  end
  else
  begin
    isCons := False;
    isDest := False;
    isClas := False;
  end;
  isStat := IsStatic;
  callConv := CallingConvention;
end;

function TRttiMethod.ToString: string;
const
  Kind: array[Boolean] of string = ('function ', 'procedure '); // do not localize
var
  paramList: TArray<TRttiParameter>;
  i: Integer;
begin
  Result := '';
  if not HasExtendedInfo then
  begin
    Result := '(basic) procedure ' + Name; // do not localize
    Exit;
  end;

  if IsClassMethod then
    Result := Result + 'class '; // do not localize

  if IsConstructor then
    Result := Result + 'constructor ' + Name // do not localize
  else if IsDestructor then
    Result := Result + 'destructor ' + Name // do not localize
  else if MethodKind = mkOperatorOverload then
    Result := Result + 'operator ' + Name // do not localize
  else
    Result := Result + Kind[ReturnType = nil] + Name;
  paramList := GetParameters;
  if Length(paramList) > 0 then
    Result := Result + '(';
  for i := 0 to Length(paramList) - 1 do
  begin
    if i > 0 then
      Result := Result + '; ';
    Result := Result + paramList[i].ToString;
  end;
  if Length(paramList) > 0 then
    Result := Result + ')';
  if ReturnType <> nil then
  begin
    Result := Result + ': ' + ReturnType.Name;
  end;
end;

{ TRttiParameter }

function TRttiParameter.ToString: string;
begin
  if pfOut in Flags then
    Result := 'out ' // do not localize
  else if pfConst in Flags then
    Result := 'const ' // do not localize
  else if pfVar in Flags then
    Result := 'var ' // do not localize
  else
    Result := '';
  Result := Result + Name;
  if ParamType <> nil then
    Result := Result + ': ' + ParamType.ToString;
end;

{ TRttiInstMethParameter }

constructor TRttiInstMethParameter.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);
begin
  inherited;
  FFlags := TParamFlags(ReadU8(P));
  FParamType := Pool.GetType(DerefPointer(ReadPointer(P)));
  FLocation := ReadU16(P);
  FName := ReadShortString(P);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiInstMethParameter.GetName: string;
begin
  Result := FName;
end;

function TRttiInstMethParameter.HasName(const AName: string): Boolean;
begin
  Result := SameText(FName, AName);
end;

function TRttiInstMethParameter.GetFlags: TParamFlags;
begin
  Result := FFlags;
end;

function TRttiInstMethParameter.GetParamType: TRttiType;
begin
  Result := FParamType;
end;

{ TRttiMember }

function TRttiMember.GetParent: TRttiType;
begin
  Result := TRttiType(inherited Parent);
end;

function TRttiMember.GetVisibility: TMemberVisibility;
begin
  Result := mvPublic;
end;

{ TRttiField }

constructor TRttiField.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);
begin
  inherited Create(APackage, AParent, P);
  FFieldType := GetFieldType;
  FOffset := GetOffset;
end;

function TRttiField.GetFieldType: TRttiType;
begin
  Result := nil;
end;

function TRttiField.GetOffset: Integer;
begin
  Result := -MaxInt - 1;
end;

function TRttiField.GetValue(Instance: Pointer): TValue;
begin
  if FieldType = nil then
    raise InsufficientRtti;
  TValue.Make(PByte(Instance) + Offset, FieldType.Handle, Result);
end;

procedure TRttiField.SetValue(Instance: Pointer; const AValue: TValue);
begin
  if FieldType = nil then
    raise InsufficientRtti;
  if AValue.TypeInfo = FieldType.Handle then
    AValue.ExtractRawData(PByte(Instance) + Offset)
  else
    AValue.Cast(FieldType.Handle).ExtractRawData(PByte(Instance) + Offset);
end;

function TRttiField.ToString: string;
begin
  if FieldType = nil then
    Result := Name + ' @ ' + IntToHex(Offset, 2)
  else
    Result := Name + ': ' + FieldType.Name + ' @ ' + IntToHex(Offset, 2);
end;

{ TRttiInstanceFieldClassic }

function TRttiInstanceFieldClassic.GetHandle: PVmtFieldEntry;
begin
  Result := inherited Handle;
end;

constructor TRttiInstanceFieldClassic.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte(Handle.AttrData);
end;

function TRttiInstanceFieldClassic.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiInstanceFieldClassic.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

function TRttiInstanceFieldClassic.GetFieldType: TRttiType;
begin
  Result := Pool.GetType(Parent.FClassTab^.ClassRef[Handle^.TypeIndex]^);
end;

function TRttiInstanceFieldClassic.GetOffset: Integer;
begin
  Result := Handle^.FieldOffset;
end;

function TRttiInstanceFieldClassic.GetParent: TRttiInstanceType;
begin
  Result := TRttiInstanceType(inherited Parent);
end;

{ TRttiInstanceFieldEx }

function TRttiInstanceFieldEx.GetHandle: PFieldExEntry;
begin
  Result := inherited Handle;
end;

constructor TRttiInstanceFieldEx.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);
begin
  inherited;
  P := PByte( Handle.AttrData );
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiInstanceFieldEx.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiInstanceFieldEx.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

function TRttiInstanceFieldEx.GetOffset: Integer;
begin
  Result := Handle^.Offset;
end;

function TRttiInstanceFieldEx.GetFieldType: TRttiType;
begin
  Result := Pool.TypeOrNil(Handle^.TypeRef);
end;

function TRttiInstanceFieldEx.GetVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility(GetBitField(Handle^.Flags, ffVisibilityShift, ffVisibilityBits));
end;

{ TRttiSetType }

constructor TRttiSetType.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte(@TypeData^.CompType) + SizeOf(TypeData^.CompType);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiSetType.GetElementType: TRttiType;
begin
  Result := Pool.TypeOrNil(TypeData^.CompType);
end;

function TRttiSetType.GetTypeSize: Integer;
begin
  Result := SizeOfSet(PTypeInfo(Handle));
end;

function TRttiSetType.GetByteOffset: Integer;
begin
  Result := ByteOffsetOfSet(PTypeInfo(Handle));
end;

type
  TRttiIntfMethParameter = class(TRttiParameter)
  private
    FName: string;
    FFlags: TParamFlags;
    FParamType: PPTypeInfo;
    function GetName: string; override;
    function GetFlags: TParamFlags; override;
    function GetParamType: TRttiType; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    function HasName(const AName: string): Boolean; override;
  end;

  TRttiIntfMethod = class(TRttiMethod)
  private
    FTail: PIntfMethodEntryTail;
    FParameters: TArray<TRttiParameter>;
    FReturnType: PTypeInfo;
    FVirtualIndex: Integer;
    function GetMethodKind: TMethodKind; override;
    function GetCallingConvention: TCallConv; override;
    function GetReturnType: TRttiType; override;
    function GetHasExtendedInfo: Boolean; override;
    function GetVirtualIndex: Smallint; override;
    function GetDispatchKind: TDispatchKind; override;
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    function GetHandle: PIntfMethodEntry; inline;
    function GetTailHandle: PIntfMethodEntryTail;
    function GetName: string; override;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; override;
  public
    function HasName(const AName: string): Boolean; override;
    property Handle: PIntfMethodEntry read GetHandle;
    property TailHandle: PIntfMethodEntryTail read GetTailHandle;
    function GetParameters: TArray<TRttiParameter>; override;
  end;

{ TRttiInterfaceType }

constructor TRttiInterfaceType.Create(APackage: TRttiPackage; AParent: TRttiObject;
  var P: PByte);

  procedure GetMethods(var P: PByte);
  var
    i, hasRtti, methCnt, index: Integer;
  begin
    if BaseType <> nil then
      index := BaseType.FTotalMethodCount
    else
      index := 0;
    methCnt := ReadU16(P);
    FTotalMethodCount := index + methCnt;
    hasRtti := ReadU16(P);
    if hasRtti = $FFFF then
      Exit;
    SetLength(FMethods, methCnt);
    for i := 0 to methCnt - 1 do
    begin
      FMethods[i] := Package.ReadObject(TRttiIntfMethod, Self, P) as TRttiIntfMethod;
      TRttiIntfMethod(FMethods[i]).FVirtualIndex := index;
      Inc(index);
    end;
  end;

begin
  inherited;
  P := @TypeData^.IntfUnit;
  P := SkipShortString(P);

  GetMethods(P);
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiInterfaceType.GetIntfFlags: TIntfFlags;
begin
  Result := TypeData^.IntfFlags;
end;

function TRttiInterfaceType.GetDeclaringUnitName: string;
begin
  Result :=  TypeData^.IntfUnitFld.ToString;
end;

function TRttiInterfaceType.GetBaseType: TRttiType;
begin
  Result := GetBaseTyped;
end;

function TRttiInterfaceType.GetBaseTyped: TRttiInterfaceType;
begin
  Result := Pool.TypeOrNil(TypeData^.IntfParent) as TRttiInterfaceType;
end;

function TRttiInterfaceType.GetGUID: TGUID;
begin
  Result := TypeData^.Guid;
end;

function TRttiInterfaceType.GetDeclaredMethods: TArray<TRttiMethod>;
begin
  Result := FMethods;
end;

{ TRttiIntfMethParameter }

constructor TRttiIntfMethParameter.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
begin
  inherited;
  FFlags := TParamFlags(ReadU8(P));
  FName := ReadShortString(P);
  P := SkipShortString(P);
  FParamType := PPTypeInfo(ReadPointer(P));
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiIntfMethParameter.GetName: string;
begin
  Result := FName;
end;

function TRttiIntfMethParameter.HasName(const AName: string): Boolean;
begin
  Result := SameText(FName, AName);
end;

function TRttiIntfMethParameter.GetFlags: TParamFlags;
begin
  Result := FFlags;
end;

function TRttiIntfMethParameter.GetParamType: TRttiType;
begin
  Result := Pool.TypeOrNil(FParamType);
end;

{ TRttiIntfMethod }

function TRttiIntfMethod.GetHandle: PIntfMethodEntry;
begin
  Result := inherited Handle;
end;

constructor TRttiIntfMethod.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
var
  i: Integer;
begin
  inherited;
  FTail := Handle.Tail;
  P := PByte(FTail);
  Inc(P, SizeOf(FTail^));
  // Due to mess of interface method table, we have little choice but to
  // read it all in right now.
  SetLength(FParameters, FTail^.ParamCount - 1);
  // Skip first parameter (Self) - not needed
  Package.ReadObject(TRttiIntfMethParameter, Self, P);
  for i := 0 to Length(FParameters) - 1 do
    FParameters[i] := Package.ReadObject(TRttiIntfMethParameter, Self, P) as TRttiParameter;
  if MethodKind = mkFunction then
  begin
    if P^ = 0 then
      Inc(P)
    else
    begin
      P := SkipShortString(P);
      FReturnType := DerefPointer(ReadPointer(P));
    end;
  end;
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiIntfMethod.GetTailHandle: PIntfMethodEntryTail;
begin
  Result := FTail;
end;

function TRttiIntfMethod.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiIntfMethod.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

function TRttiIntfMethod.GetMethodKind: TMethodKind;
begin
  if FTail^.Kind = 0 then
    Result := mkProcedure
  else
    Result := mkFunction;
end;

function TRttiIntfMethod.GetCallingConvention: TCallConv;
begin
  Result := FTail^.CC;
end;

function TRttiIntfMethod.GetReturnType: TRttiType;
begin
  Result := Pool.GetType(FReturnType);
end;

function TRttiIntfMethod.GetHasExtendedInfo: Boolean;
begin
  Result := True;
end;

function TRttiIntfMethod.GetVirtualIndex: Smallint;
begin
  Result := FVirtualIndex;
end;

function TRttiIntfMethod.GetDispatchKind: TDispatchKind;
begin
  Result := dkInterface;
end;

function TRttiIntfMethod.DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue;
var
  code: Pointer;
  argCount: Integer;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i, currArg: Integer;
  inst: PPVtable;
  callConv: TCallConv;
begin
  callConv := CallingConvention;

  parList := GetParameters;
  if Length(Args) <> Length(parList) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  argCount := Length(Args);
  SetLength(argList, argCount + 1);

  currArg := 0;
  inst := PPVtable(Instance.AsInterface);
  PushSelfFirst(callConv, argList, currArg, Instance);

  for i := 0 to Length(Args) - 1 do
  begin
    PassArg(parList[i], Args[i], argList[currArg], callConv);
    Inc(currArg);
  end;

  Assert(DispatchKind = dkInterface);
  code := inst^^[FVirtualIndex];
  CheckCodeAddress(code); // should never happen, I believe

  PushSelfLast(callConv, argList, currArg, Instance);

  if ReturnType <> nil then
    Result := System.Rtti.Invoke(code, argList, callConv, ReturnType.Handle)
  else
    Result := System.Rtti.Invoke(code, argList, callConv, nil);
end;

function TRttiIntfMethod.GetParameters: TArray<TRttiParameter>;
begin
  Result := FParameters;
end;

{ TOrphanPackage }

constructor TOrphanPackage.Create;
begin
  FPackage := nil;
  FParent := nil;
  FHandle := Pointer(-1);
  FLock := TObject.Create;
  FHandleToObject := TObjectDictionary<Pointer,TRttiObject>.Create([doOwnsValues]);
end;

function TOrphanPackage.FindType(const AQualifiedName: string): TRttiType;
begin
  Result := nil;
end;

function TOrphanPackage.GetName: string;
begin
  Result := '(orphan package)'; // do not localize
end;

function TOrphanPackage.GetTypes: TArray<TRttiType>;
begin
  SetLength(Result, 0);
end;

{ TRealPackage }

constructor TRealPackage.Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte);
var
  lib: PLibModule;
  pinfo: GetPackageInfoTable;
{$IFDEF POSIX}
  dlinfo: dl_info;
{$ENDIF POSIX}
begin
  // invariant: P = PLibModule for this package
  lib := PLibModule(P);
  // Make our Handle refer to HInstance rather than LibModule
  P := Pointer(lib^.Instance);
  inherited;
  FLock := TObject.Create;
  FTypeInfo := lib^.TypeInfo;
  if FTypeInfo = nil then
  begin
    pinfo := GetProcAddress(lib^.Instance, PackageInfoFuncName);
    FTypeInfo := @pinfo^.TypeInfo;
  end;
  {$IFDEF MSWINDOWS}
  FBaseAddress := Pointer(lib^.Instance);
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  dladdr(UIntPtr(FTypeInfo), dlinfo);
  FBaseAddress := dlinfo.dli_fbase;
  {$ENDIF POSIX}
  FHandleToObject := TObjectDictionary<Pointer,TRttiObject>.Create([doOwnsValues]);
end;

destructor TRealPackage.Destroy;
begin
  FNameToType.Free;
  FTypeToName.Free;
  inherited;
end;

function TRealPackage.FindType(const AQualifiedName: string): TRttiType;
var
  h: PTypeInfo;
  o: TRttiObject;
begin
  MakeTypeLookupTable;
  if not FNameToType.TryGetValue(AQualifiedName, h) then
    Exit(nil);

  TMonitor.Enter(Flock);
  try
    if FHandleToObject.TryGetValue(h, o) then
      Exit(o as TRttiType);
  finally
    TMonitor.Exit(Flock);
  end;
  Result := ReadObjectPointer(TRttiType, nil, h) as TRttiType;
end;

function TRealPackage.GetName: string;
begin
  Result := GetModuleName(Handle);
end;

function TRealPackage.GetTypes: TArray<TRttiType>;

  function GetType(ATypeInfo: Pointer): TRttiType;
  begin
    if ATypeInfo = nil then
      Exit(nil);
    Result := TRttiType(ReadObjectPointer(TRttiType, nil, ATypeInfo));
  end;

var
  i, valid: Integer;
begin
  valid := 0;
  for i := 0 to FTypeInfo^.TypeCount - 1 do
    if (FTypeInfo^.TypeTable^[i] <> nil) and (NativeInt(FTypeInfo^.TypeTable^[i]) <> 1)
      and (PPointer(FTypeInfo^.TypeTable^[i])^ <> nil) then
      Inc(valid);
  SetLength(Result, valid);

  valid := 0;
  for i := 0 to FTypeInfo^.TypeCount - 1 do
    if (FTypeInfo^.TypeTable^[i] <> nil) and (NativeInt(FTypeInfo^.TypeTable^[i]) <> 1)
      and (PPointer(FTypeInfo^.TypeTable^[i])^ <> nil) then
    begin
      Result[valid] := GetType(PPTypeInfo(FTypeInfo^.TypeTable^[i])^);
      Inc(valid);
    end;
end;

function TRealPackage.GetNameFromType(AType: TRttiType): string;
begin
  MakeTypeLookupTable;
  FTypeToName.TryGetValue(AType.Handle, Result);
end;

procedure TRealPackage.MakeTypeLookupTable;
  function GetUnits: TArray<string>;
  var
    p: PByte;
    i: Integer;
  begin
    SetLength(Result, FTypeInfo^.UnitCount);
    p := Pointer(FTypeInfo^.UnitNames);
    for i := 0 to FTypeInfo^.UnitCount - 1 do
      Result[i] := ReadShortString(p);
  end;

  procedure DoMake;
  var
    units: TArray<string>;
    typeIter: PPTypeInfo;
    currUnit: Integer;
    typeName: string;
    i: Integer;
    nameToType: TDictionary<string,PTypeInfo>;
  begin
    TMonitor.Enter(Flock);
    try
      if FNameToType <> nil then // presumes double-checked locking ok
        Exit;

      units := GetUnits;
      currUnit := 0;
      nameToType := nil;
      try
        nameToType := TDictionary<string,PTypeInfo>.Create;
        FTypeToName := TDictionary<PTypeInfo,string>.Create;
        for i := 0 to FTypeInfo^.TypeCount - 1 do
        begin
          typeIter := FTypeInfo^.TypeTable^[i];
          if typeIter = nil then
            Continue;
          if IntPtr(typeIter) = 1 then
          begin
            // inter-unit boundary
            Inc(currUnit);
            Continue;
          end;
          if typeIter^ = nil then // linker broke or fixup eliminated.
            Continue;
          typeName := units[currUnit] + '.' + typeIter^^.NameFld.ToString;

          if not nameToType.ContainsKey(typeName) then
            nameToType.Add(typeName, typeIter^);
          if not FTypeToName.ContainsKey(typeIter^) then
            FTypeToName.Add(typeIter^, typeName);
        end;
      except
        nameToType.Free;
        FreeAndNil(FTypeToName);
        raise;
      end;

      FNameToType := nameToType;
    finally
      TMonitor.Exit(Flock);
    end;
  end;

begin
  if FNameToType <> nil then
    Exit;
  DoMake;
end;


{ TMethodImplementation.TInvokeInfo }

constructor TMethodImplementation.TInvokeInfo.Create(ACallConv: TCallConv; AHasSelf: Boolean);
begin
  FParamList := TList<TMethodImplementation.TParamLoc>.Create;
  FCallConv := ACallConv;
  FHasSelf := AHasSelf;
end;

destructor TMethodImplementation.TInvokeInfo.Destroy;
begin
  FParamList.Free;
  FTypeInfos.Free;
  inherited;
end;

procedure TMethodImplementation.TInvokeInfo.AddParameter(AType: PTypeInfo;
  AByRef, AConstant, ASetDefault, AOpenArray: Boolean);
begin
  CheckNotSealed;
  FParamList.Add(TParamLoc.Create(AType, AByRef, AConstant, ASetDefault, AOpenArray));
end;

procedure TMethodImplementation.TInvokeInfo.CheckNotSealed;
begin
  Assert(FParamList <> nil, 'InvokeInfo is sealed');
end;

function TMethodImplementation.TInvokeInfo.LoadArguments(AFrame: PInterceptFrame;
  var ATypeInfos: TRuntimeTypeInfos): TArray<TValue>;
var
  i: Integer;
  locs: TArray<TParamLoc>;
  len: Integer;
  realArgs: Integer;
begin
  locs := GetParamLocs;
  len := Length(locs);
  realArgs := 0;
  for i := 0 to len - 1 do
    if not locs[i].FOpenArray then
      Inc(realArgs);
  SetLength(Result, realArgs);
  i := 0;
  realArgs := 0;
  while i < len do
  begin
    if locs[i].FOpenArray then
    begin
      locs[i].GetOpenArrayArg(AFrame, locs[i + 1], Result[realArgs], ATypeInfos);
      Inc(i);
    end
    else
      locs[i].GetArg(AFrame, Result[realArgs]);
    Inc(i);
    Inc(realArgs);
  end;
end;

procedure TMethodImplementation.TInvokeInfo.SaveArguments(AFrame: PInterceptFrame;
  const Args: TArray<TValue>; const Result: TValue);
var
  i: Integer;
  locs: TArray<TParamLoc>;
  len: Integer;
  realArgs: Integer;
begin
  locs := GetParamLocs;
  len := Length(locs);
  i := 0;
  realArgs := 0;
  while i < len do
  begin
    if locs[i].FByRefParam and not locs[i].FConstant then
      locs[i].SetArg(AFrame, Args[realArgs]);
    if locs[i].FOpenArray then
      Inc(i);
    Inc(i);
    Inc(realArgs);
  end;
  // Note: on x64, The RCX slot is overloaded for RAX for the result.
  // Because of this, byref arguments must be saved before the result overwrites this
  if FReturnType <> nil then
    FResultLoc.SetArg(AFrame, Result);
{$IF defined(CPUX64) and (Defined(LINUX64) or Defined(OSX64))}
  // This flag will be checked in rtti_raw_intercept
  if FResultFP = ftExtended then
    AFrame^.UseFPU := 1
  else
    AFrame^.UseFPU := 0;
{$ENDIF}
{$IFDEF CPUX86}
  AFrame^.FP.Kind := FResultFP;
{$ENDIF CPUX86}
end;

function GetParamSize(TypeInfo: PTypeInfo): Integer;
begin
  if TypeInfo = nil then
    Exit(0);

  case TypeInfo^.Kind of
    tkInteger, tkEnumeration, tkChar, tkWChar:
      case GetTypeData(TypeInfo)^.OrdType of
        otSByte, otUByte: Exit(1);
        otSWord, otUWord: Exit(2);
        otSLong, otULong: Exit(4);
      else
        Exit(0);
      end;
    tkSet:
      begin
        Result := SizeOfSet(TypeInfo);
        if Result > SizeOf(NativeInt) then
          Result := -Result;
      end;
    tkFloat:
      case GetTypeData(TypeInfo)^.FloatType of
        ftSingle: Exit(4);
        ftDouble: Exit(8);
        ftExtended: Exit(SizeOf(Extended));
        ftComp: Exit(8);
        ftCurr: Exit(8);
      else
        Exit(0);
      end;
    tkClass, tkClassRef: Exit(SizeOf(Pointer));
    tkInterface: Exit(-SizeOf(Pointer));
    tkMethod: Exit(SizeOf(TMethod));
    tkInt64: Exit(8);
    tkDynArray, tkUString, tkLString, tkWString: Exit(-SizeOf(Pointer));
{$IFNDEF NEXTGEN}
    tkString: Exit(GetTypeData(TypeInfo)^.MaxLength + 1);
{$ENDIF !NEXTGEN}
    tkPointer, tkProcedure: Exit(SizeOf(Pointer));
    tkRecord, tkMRecord:
      if IsManaged(TypeInfo) then
        Exit(-GetTypeData(TypeInfo)^.RecSize)
      else
        Exit(GetTypeData(TypeInfo)^.RecSize);
    tkArray: Exit(GetTypeData(TypeInfo)^.ArrayData.Size);
    tkVariant: Exit(-SizeOf(Variant));
  else
    Exit(0);
  end;
end;


function TMethodImplementation.TInvokeInfo.GetParamLocs: TArray<TParamLoc>;
begin
  Assert(FParamList = nil, 'TInvokeInfo wasn''t sealed'); // DO NOT LOCALIZE
  Result := FParams;
end;

procedure TMethodImplementation.TInvokeInfo.Seal;
{$IF Defined(CPUX64)}
{$IF Defined(WIN64)}
var
  top: Integer;
  freeRegs: UInt32;

  procedure PutArg(var Param: TParamLoc);
  var
    dataSize: Integer;
    reg: UInt32;
  begin
    if Param.FByRefParam then
      dataSize := SizeOf(Pointer)
    else
    begin
      dataSize := GetParamSize(Param.FTypeInfo);
      if dataSize < 0 then
        dataSize := SizeOf(Pointer);
    end;
    if dataSize in [1, 2, 4, 8] then
    begin
      reg := AllocReg(freeRegs);
      if reg <> rsNone then
      begin
        if (not Param.FByRefParam)
        and (Param.FTypeInfo <> nil)
        and (Param.FTypeInfo^.Kind = tkFloat)
        and (Param.FTypeInfo.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
          case reg of
            rsXMM0: Param.FOffset := regXMM0;
            rsXMM1: Param.FOffset := regXMM1;
            rsXMM2: Param.FOffset := regXMM2;
            rsXMM3: Param.FOffset := regXMM3;
          end
        else
          case reg of
            rsRCX: Param.FOffset := regRCX;
            rsRDX: Param.FOffset := regRDX;
            rsR8:  Param.FOffset := regR8;
            rsR9:  Param.FOffset := regR9;
          end;
        Exit;
      end;
    end;
    Param.FOffset := top;
    Inc(top, Align8(dataSize));                                                                   
  end;

var
  I: Integer;
  p: PParamLoc;
begin
  top := 0;
  freeRegs := rsAll;

  SetLength(FParams, FParamList.Count);

  if FHasSelf and (FParamList.Count > 0) then
  begin
    p := @FParams[0];
    p^ := FParamList[0];
    PutArg(p^);
  end;
  if (FCallConv <> ccSafeCall) and (FReturnType <> nil) then
    if UseResultPointer(FReturnType, False, FCallConv) then
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
      PutArg(FResultLoc);
    end
    else
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, False);
      // The RCX slot is overloaded for RAX for the result.
      // Assume Result is in RAX, it will be reset later if it is not.
      FResultLoc.FOffset := regRCX;
    end;
  if not FHasSelf and (FParamList.Count > 0) then
  begin
    p := @FParams[0];
    p^ := FParamList[0];
    PutArg(p^);
  end;
  for I := 1 to FParamList.Count -1 do
  begin
    p := @FParams[I];
    p^ := FParamList[I];
    PutArg(p^);
  end;
  if (FCallConv = ccSafeCall) and (FReturnType <> nil) then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
    PutArg(FResultLoc);
  end;
  FStackSize := top;

  FParamList.Free;
  FParamList := nil;
end;
{$ELSEIF Defined(Linux64) or Defined(MACOS64)}
var
  NextGR: Integer;  // Next General-purpose Register Number. 6 (64bits) registers are available.
  NextFR: Integer;  // Next Floating-point Register Number. 8 (128bits) registers are available.
  Stack: TStackHelper;
const
  maxGRegister = 6;
  maxFRegister = 8;

  procedure PutArg(var Param: TParamLoc);
  var
    DataSize: Integer;
  begin
    if Param.FByRefParam then
      DataSize := SizeOf(Pointer)
    else
    begin
      DataSize := GetParamSize(Param.FTypeInfo);
      if DataSize < 0 then
        DataSize := SizeOf(Pointer);
      if assigned(Param.FTypeInfo) then
      begin
        // Single/Double
        if (Param.FTypeInfo.Kind = tkFloat) and
           (Param.FTypeInfo.TypeData.FloatType in [ftSingle, ftDouble]) then
        begin
          if NextFR < maxFRegister then
          begin
            Param.FOffset := $10000000 + NextFR;
            Inc(NextFR);
            Exit;
          end;
        end
        // Extended
        else if (Param.FTypeInfo.Kind = tkFloat) and
                (Param.FTypeInfo.TypeData.FloatType in [ftExtended]) then
        begin
            Stack.Align(16);
            Param.FOffset := Stack.Size;
            Stack.Alloc(DataSize, 16);
            Exit;
        end
        // Record
        else if Param.FTypeInfo.Kind in [tkRecord, tkMRecord, tkArray] then
        begin
          if (FCallConv in [ccReg]) and (datasize > 4) then
          begin
            datasize := SizeOf(Pointer);
            Param.FByRefParam := True;
          end
          else if (datasize <= 16) then
          begin
            datasize := Align8(datasize);
            if (maxGRegister - nextGR) * SizeOf(NativeUInt) > DataSize then
            begin
              Param.FOffset := $10000008 + NextGR;
              Inc(nextGR, (DataSize + SizeOf(NativeUInt) - 1) div SizeOf(NativeUInt));
            end
            else
            begin
              nextGR := maxGRegister; // C.11
              Param.FOffset := Stack.Size;
              Stack.Alloc(DataSize, 8);
            end;
            Exit;
          end
          else
          begin
            datasize := Align8(datasize);
              Param.FOffset := Stack.Size;
              Stack.Alloc(DataSize, 8);
            Exit;
          end;
        end;
      end;
    end;

    if (dataSize in [1, 2, 4, 8]) then
    begin
      if nextGR < maxGRegister then
      begin
        Param.FOffset := $10000008 + nextGR;
        Inc(nextGR);
        Exit;
      end;
      Param.FOffset := Stack.Size;
      Stack.Alloc(DataSize, 8);
    end
    else
      Assert(False, 'somethig wrong');
  end;

var
  I: Integer;
  p: PParamLoc;
begin
  NextGR := 0;
  NextFR := 0;

  Stack := TStackHelper.Create;

  if FCallConv <> ccSafeCall then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, False);
    FResultLoc.FOffset := $1000000E; // OutRAX
  end;

  SetLength(FParams, FParamList.Count);

  if (FCallConv <> ccSafeCall) and (FReturnType <> nil) then
  begin
    if (FReturnType^.Kind = tkFloat) then
    begin
      FResultFP := FReturnType^.TypeData.FloatType;
      case FResultFP of
        ftSingle,
        ftDouble:
          FResultLoc.FOffset := $10000000;  // XMM0
        ftExtended:
          FResultLoc.FOffset := $10000010;  // OutFPU
        ftComp,
        ftCurr:
          FResultLoc.FOffset := $1000000E;  // RAX
      end;
    end
    else if UseResultPointer(FReturnType, False, FCallConv) then
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
      PutArg(FResultLoc);
    end;
  end;
  for I := 0 to FParamList.Count - 1 do
  begin
    p := @FParams[I];
    p^ := FParamList[I];
    PutArg(p^);
  end;
  if (FCallConv = ccSafeCall) and (FReturnType <> nil) then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
    PutArg(FResultLoc);
  end;

  FStackSize := Stack.Size;
  FParamList.Free;
  FParamList := nil;
end;
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}

// of CPUX64 TMethodImplementation.TInvokeInfo.Seal implementation
{$ELSEIF Defined(CPUX86)}
var
  top: Integer;
  freeRegs: UInt32;

  procedure PutArg(var Param: TParamLoc);
  var
    dataSize: Integer;
    reg: UInt32;
    isFloat: Boolean;
  begin
    if Param.FByRefParam then
      dataSize := SizeOf(Pointer)
    else
    begin
      dataSize := GetParamSize(Param.FTypeInfo);
      if dataSize < 0 then
        if FCallConv in [ccCdecl, ccStdCall, ccSafeCall] then
          dataSize := -dataSize
        else
          dataSize := 4;
    end;

    isFloat := (Param.FTypeInfo <> nil) and (Param.FTypeInfo^.Kind = tkFloat);

    if Param.FByRefParam or ((dataSize in [1, 2, 4]) and not isFloat) then
    begin
      reg := AllocReg(freeRegs);
      if reg <> rsNone then
      begin
        // We grow stack allocations downwards, so invert register values
        // again
        case reg of
          rsEAX: Param.FOffset := not regEAX;
          rsEDX: Param.FOffset := not regEDX;
          rsECX: Param.FOffset := not regECX;
        end;
        Exit;
      end;
    end;
    // No register => stack
    Dec(top, Align4(dataSize));
    Param.FOffset := top;
  end;

  procedure PutRefArg(var Param: TParamLoc);
  var
    reg: UInt32;
  begin
    Param.FByRefParam := True;
    reg := AllocReg(freeRegs);
    if reg <> rsNone then
    begin
      case reg of
        rsEAX: Param.FOffset := not regEAX;
        rsEDX: Param.FOffset := not regEDX;
        rsECX: Param.FOffset := not regECX;
      end;
      Exit;
    end;
    Dec(top, SizeOf(Pointer));
    Param.FOffset := top;
  end;

var
  i: Integer;
  p: PParamLoc;
begin
  // Relative top of stack
  top := 0;

  if FCallConv = ccReg then
    freeRegs := rsAll
  else
    freeRegs := rsNone;

  if (FCallConv = ccSafeCall) and (FReturnType <> nil) then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
    PutRefArg(FResultLoc);
  end;

  SetLength(FParams, FParamList.Count);

  if FCallConv in [ccCdecl, ccStdCall, ccSafeCall] then
  begin
    for i := FParamList.Count - 1 downto 0 do
    begin
      p := @FParams[i];
      p^ := FParamList[i];

      // Consider: always PutArg, but test PassByRef for second evaluation.
      if PassByRef(p^.FTypeInfo, FCallConv) then
        PutRefArg(p^)
      else
        PutArg(p^);
    end
  end
  else
  begin
    for i := 0 to FParamList.Count - 1 do
    begin
      // ccPascal passes Self last.
      if (not FHasSelf) or (FCallConv <> ccPascal) or (i <> 0) then
      begin
        p := @FParams[i];
        p^ := FParamList[i];

        if PassByRef(p^.FTypeInfo, FCallConv) then
          PutRefArg(p^)
        else
          PutArg(p^);
      end;
    end;
  end;

  if (FCallConv <> ccSafeCall) then
  begin
    if UseResultPointer(FReturnType, False, FCallConv) then
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
      PutRefArg(FResultLoc);
    end
    else
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, False);
      FResultLoc.FOffset := not regEAX;
    end;
  end;

  if FHasSelf and (FCallConv = ccPascal) and (FParamList.Count > 0) then
  begin
    p := @FParams[0];
    p^ := FParamList[0];

    if PassByRef(p^.FTypeInfo, FCallConv) then
      PutRefArg(p^)
    else
      PutArg(p^);
  end;

  FCallerPopsStack := FCallConv = ccCdecl;
  FStackSize := -top;

  if (FReturnType <> nil) and (FReturnType^.Kind = tkFloat) and (FCallConv <> ccSafeCall) then
  begin
    FResultFP := GetTypeData(FReturnType)^.FloatType;
    FResultLoc.FOffset := not regFloat;
  end
  else
    FResultFP := TFloatType(-1);

  // Adjust offsets
  for i := 0 to Length(FParams) - 1 do
  begin
    if FParams[i].FOffset >= 0 then // params in regs => convert to neg offsets
      FParams[i].FOffset := not FParams[i].FOffset
    else
      Inc(FParams[i].FOffset, -top);
  end;
  // Adjust return value offset
  if FReturnType <> nil then
    if FResultLoc.FOffset >= 0 then
      FResultLoc.FOffset := not FResultLoc.FOffset
    else
      Inc(FResultLoc.FOffset, -top);

  FParamList.Free;
  FParamList := nil;
end; // of CPUX86 TMethodImplementation.TInvokeInfo.Seal implementation

{$ELSEIF Defined(CPUARM32)}
const
  regNone = $00;
  regCRAll  = $0F;
  regFPRAll  = $FFFF;

var
  top : integer;
  freeCRegs: UInt32;  // 4-core registers (32bit)
  freeFPRegs: UInt32; // 16-Single VFP registers (32bit)
                      //  8-Double VFP registers (64bit)

  function AllocDoubleReg: UInt32;
  var
    freeDoubleReg: Uint32;
  begin
    Result := 0;
    freeDoubleReg := freeFPRegs and $55555555; // remove odd FP registers.
    // no free Double register.
    if freeDoubleReg = 0 then Exit;
    // get a free single register at even index.
    Result := not(freeDoubleReg and (freeDoubleReg - 1)) and freeDoubleReg;
    // Remove two Single registers from FreeFPRegs;
    freeFPRegs := freeFPRegs and not( Result or Result shl 1);
  end;

  // Convert from register flag to core register offset value
  function ToCoreRegisterOffset(const regFlag: UInt32): integer;
  begin
    result := RegFlagToIndex(regFlag) or $10000000;
    // CR0 : 10000000
    // CR3 : 10000003
  end;

  // Convert from register flag to FP register offset value
  function ToSingleFPRegisterOffset(const regFlag: UInt32): integer;
  begin
    result := RegFlagToIndex(regFlag) or $10000010;
    // VFP S00 : 10000010
    // VFP S15 : 1000001F
  end;

  function ToDoubleFPRegisterOffset(const regFlag: UInt32): integer;
  begin
    result := RegDoubleFlagToIndex(regFlag) or $10000020;
    // VFP D0 : 10000020
    // VFP D7 : 10000027
  end;

  procedure PutArg(var Param: TParamLoc);
  var
    dataSize: Integer;
    reg,
    regL, regH: UInt32;
  begin
    if Param.FByRefParam then
      dataSize := SizeOf(Pointer)
    else
    begin
      dataSize := GetParamSize(Param.FTypeInfo);
      if dataSize < 0 then
        dataSize := SizeOf(Pointer);
      if assigned(Param.FTypeInfo) then
      begin
        if (Param.FTypeInfo.Kind = tkFloat) and
           (Param.FTypeInfo.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
        begin
          if dataSize = 4 then // Single
          begin
            reg := AllocReg(freeFPRegs);
            if reg <> 0 then
            begin
              Param.FOffset := ToSingleFPRegisterOffset(reg);
              Exit;
            end;
          end
          else if dataSize = 8 then // Double
          begin
            // First, allocate one Double VFP register.
            reg := AllocDoubleReg;
            if reg <> 0 then
            begin
              Param.FOffset := ToDoubleFPRegisterOffset(reg);
              Exit;
            end;
          end
          else
            Assert(False, 'Unknwon float type');
        end
        else if Param.FTypeInfo.Kind in [tkRecord, tkMRecord] then
        begin
          assert( FCallConv <> ccReg );
          // allocate first 4bytes, and keep it
          reg := AllocReg(freeCRegs);
          if reg <> regNone then
            Param.FOffset := ToCoreRegisterOffset(reg)
          else
          begin
            Param.FOffset := top;
            Inc(top, 4);
          end;
          Dec(dataSize, 4);
          // reserve rest of record block.
          while (datasize > 0) and (freeCRegs <> 0) do
          begin
            reg := AllocReg(freeCRegs);
            if reg = regNone then Inc(top, 4);
            Dec(dataSize, 4);
          end;
          while (datasize > 0) do
          begin
            Inc(top, 4);
            Dec(dataSize, 4);
          end;
          Exit;
        end;
      end;
    end;

    if (dataSize in [1, 2, 4]) then
    begin
      reg := AllocReg(freeCRegs);
      if reg <> regNone then
      begin
        Param.FOffset := ToCoreRegisterOffset(reg);
        Exit;
      end;
      Param.FOffset := top;
      Inc(top, 4);
    end
    else if dataSize = 8 then // 64bit data
    begin
      // Next, allocate two core register
      {$IFDEF IOS}
      regL := AllocReg(freeCRegs);
      {$ELSE}
      regL := AllocEvenReg(freeCRegs);
      {$ENDIF}
      regH := AllocReg(freeCRegs);
      if regL <> 0 then
        Param.FOffset := ToCoreRegisterOffset(regL)
      else
      begin
        Param.FOffset := top;
        Inc(top, 4);
      end;
      if regH = 0 then
        Inc(top, 4);
    end
    else
      Assert(False, 'somethig wrong');
  end;

var
  I: Integer;
  p: PParamLoc;
begin
  top := 0;
  freeCRegs := regCRAll;
  freeFPRegs := regNone;

{$IF not defined(ARM_NO_VFP_USE)}
  if FCallConv in [ccReg] then
    freeFPRegs := regFPRAll;
{$ENDIF !ARM_NO_VFP_USE}

  if FCallConv <> ccSafeCall then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, False);
    FResultLoc.FOffset := ToCoreRegisterOffset($0001); // CR00 as Result register
  end;

  SetLength(FParams, FParamList.Count);

  if (FCallConv <> ccSafeCall) and (FReturnType <> nil) then
  begin
    if IsManaged(FReturnType) or ((FReturnType^.Kind in [tkRecord, tkMRecord]) and
      (not (GetTypeData(FReturnType)^.RecSize in [1]))) then
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
      PutArg(FResultLoc);
    end;
  end;
  for I := 0 to FParamList.Count -1 do
  begin
    p := @FParams[I];
    p^ := FParamList[I];
    PutArg(p^);
  end;
  if (FCallConv = ccSafeCall) and (FReturnType <> nil) then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
    PutArg(FResultLoc);
  end;
  FStackSize := top;

  FParamList.Free;
  FParamList := nil;
end; // of CPUARM32 TMethodImplementation.TInvokeInfo.Seal implementation

{$ELSEIF Defined(CPUARM64)}
var
  top : integer;
  nextGR: Integer;  // Next General-purpose Register Number. 8 (64bits) registers are avaulable.
  nextFR: Integer;  // Next Floating-point Register Number. 8 (128bits) registers are avaulable.
const
  maxRegister = 8;

  procedure PutArg(var Param: TParamLoc);
  var
    dataSize: Integer;
  begin
    if Param.FByRefParam then
      dataSize := SizeOf(Pointer)
    else
    begin
      dataSize := GetParamSize(Param.FTypeInfo);
      if dataSize < 0 then
        dataSize := SizeOf(Pointer);
      if assigned(Param.FTypeInfo) then
      begin
        if (Param.FTypeInfo.Kind = tkFloat) and
           (Param.FTypeInfo.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
        begin
          if nextFR < maxRegister then
          begin
            Param.FOffset := $10000010 + nextFR;
            Inc(nextFR);
            Exit;
          end;
        end
        else if Param.FTypeInfo.Kind in [tkRecord, tkMRecord] then
        begin
          if (datasize > 16) or (FCallConv in [ccReg, ccPascal]) then  // B.3
          begin
            datasize := SizeOf(Pointer);
            Param.FByRefParam := True;
          end
          else
          begin
            Assert(datasize <= 16);
            datasize := Align8(datasize); // B.4
            if (maxRegister - nextGR) * SizeOf(NativeUInt) > dataSize then // C.10
            begin
              Param.FOffset := $10000000 + nextGR;
              Inc(nextGR, (dataSize + SizeOf(NativeUInt) - 1) div SizeOf(NativeUInt));
            end
            else
            begin
              nextGR := maxRegister; // C.11
              Param.FOffset := top;
{$IFDEF MACOS}
              Inc(top, dataSize);
{$ELSE !MACOS}
              Inc(top, Align8(dataSize));
{$ENDIF MACOS}
            end;
            Exit;
          end;
        end;
      end;
    end;

    if (dataSize in [1, 2, 4, 8]) then
    begin
      if nextGR < maxRegister then
      begin
        Param.FOffset := $10000000 + nextGR;
        Inc(nextGR);
        Exit;
      end;
     if dataSize = 8 then
        top := ((top + 7) and not 7)
      else if dataSize = 4 then
        top := ((top + 3) and not 3)
      else if dataSize = 2 then
        top := ((top + 1) and not 1);
      Param.FOffset := top;
{$IFDEF MACOS}
      Inc(top, dataSize);
{$ELSE !MACOS}
      Inc(top, Align8(dataSize));
{$ENDIF MACOS}
    end
    else
      Assert(False, 'somethig wrong');
  end;

var
  I: Integer;
  p: PParamLoc;
begin
  top := 0;
  nextGR := 0;
  nextFR :=0;

  if FCallConv <> ccSafeCall then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, False);
    FResultLoc.FOffset := $10000000; // CR00 as Result register
  end;

  SetLength(FParams, FParamList.Count);

  if (FCallConv <> ccSafeCall) and (FReturnType <> nil) then
  begin
    if IsManaged(FReturnType) or ((FReturnType^.Kind in [tkRecord, tkMRecord]) and
      (GetTypeData(FReturnType)^.RecSize > 16)) then
    begin
      FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
//      PutArg(FResultLoc);
// In this case, ARM64 will use X8
      FResultLoc.FOffset := $10000008;
    end;
    if (FReturnType^.Kind = tkFloat) and
       (FReturnType^.TypeData.FloatType in [ftSingle, ftDouble, ftExtended]) then
      FResultLoc.FOffset := $10000010;
  end;
  for I := 0 to FParamList.Count -1 do
  begin
    p := @FParams[I];
    p^ := FParamList[I];
    PutArg(p^);
  end;
  if (FCallConv = ccSafeCall) and (FReturnType <> nil) then
  begin
    FResultLoc := TParamLoc.CreateReturn(FReturnType, True);
    PutArg(FResultLoc);
  end;

  FStackSize := top;
  FParamList.Free;
  FParamList := nil;
end; // of CPUARM64 TMethodImplementation.TInvokeInfo.Seal implementation
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}

procedure TMethodImplementation.TInvokeInfo.SetReturnType(Value: PTypeInfo);
begin
  CheckNotSealed;
  FReturnType := Value;
end;

function TMethodImplementation.TInvokeInfo.GetTypeInfos: TRuntimeTypeInfos;
begin
  if FTypeInfos = nil then
    FTypeInfos := TRuntimeTypeInfos.Create;
  Result := FTypeInfos;
end;

{ TMethodImplementation.TRuntimeTypeInfos }

constructor TMethodImplementation.TRuntimeTypeInfos.Create;
begin
  inherited Create;
  FList := TList<Pointer>.Create;
end;

destructor TMethodImplementation.TRuntimeTypeInfos.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FreeMem(FList[i]);
  FList.Free;
  inherited Destroy;
end;

function TMethodImplementation.TRuntimeTypeInfos.DefineOpenArray(
  AElType: PTypeInfo; AElCount: Integer): PTypeInfo;
var
  pInfo: ^TOpenArrayTypeInfo;
  s: ShortString;
begin
  GetMem(pInfo, SizeOf(TOpenArrayTypeInfo));
  FList.Add(pInfo);
  pInfo^.FKind := TTypeKind.tkArray;
  pInfo^.FNameLen := SizeOf(pInfo^.FName);
  s := CNameTempl + ShortString(IntToHex(FList.Count, 2));
  Move(s[1], pInfo^.FName[0], pInfo^.FNameLen);
  pInfo^.Size := Abs(GetInlineSize(AElType)) * AElCount;
  pInfo^.ElCount := AElCount;
  pInfo^.ElType := @pInfo^.FElType;
  pInfo^.DimCount := 1;
  pInfo^.Dims[0] := @pInfo^.FIndType;
  pInfo^.FElType := AElType;
  pInfo^.FIndType := TypeInfo(Integer);
  Result := PTypeInfo(pInfo);
end;

class function TMethodImplementation.TRuntimeTypeInfos.IsOpenArray(
  ATypeInfo: PTypeInfo): Boolean;
begin
  Result := (ATypeInfo <> nil) and (ATypeInfo^.Kind = TTypeKind.tkArray) and
    (PByte(@ATypeInfo^.Name)^ = CNameLen) and
    CompareMem(PByte(@ATypeInfo^.Name) + 1, PByte(@CNameTempl) + 1, Length(CNameTempl));
end;

{ TMethodImplementation.TParamLoc }

constructor TMethodImplementation.TParamLoc.Create(AType: PTypeInfo;
  AByRef, AConstant, ASetDefault, AOpenArray: Boolean);
begin
  FTypeInfo := AType;
  FByRefParam := AByRef;
  FConstant := AConstant;
  FSetDefault := ASetDefault;
  FOpenArray := AOpenArray;
  FOffset := 0;
end;

constructor TMethodImplementation.TParamLoc.CreateReturn(AType: PTypeInfo;
  AByRef: Boolean);
begin
  FTypeInfo := AType;
  FByRefParam := AByRef;
  FConstant := False;
  FSetDefault := True;
  FOpenArray := False;
  FOffset := 0;
end;

procedure TMethodImplementation.TParamLoc.GetArg(AFrame: PInterceptFrame;
  var Value: TValue);
var
  loc: Pointer;
begin
  loc := GetArgLoc(AFrame);
  if FTypeInfo = nil then
    TValue.Make(@loc, TypeInfo(Pointer), Value)
  else
  begin
    if (FTypeInfo^.Kind = tkMRecord) and FSetDefault then
      InitializeArray(loc, FTypeInfo, 1);
    TValue.Make(loc, FTypeInfo, Value);
  end;
end;

procedure TMethodImplementation.TParamLoc.GetOpenArrayArg(AFrame: PInterceptFrame;
  const AHighLoc: TParamLoc; var Value: TValue; var ATypeInfos: TRuntimeTypeInfos);
begin
  if ATypeInfos = nil then
    ATypeInfos := TRuntimeTypeInfos.Create;
  FTypeInfo := ATypeInfos.DefineOpenArray(GetArrayElType(FTypeInfo),
    PInteger(AHighLoc.GetArgLoc(AFrame))^ + 1);
  GetArg(AFrame, Value);
end;

function TMethodImplementation.TParamLoc.GetArgLoc(AFrame: PInterceptFrame): Pointer;
{$IF Defined(CPUX64)}
{$IF Defined(WIN64)}
begin
  case FOffset of
    regRCX:  Result := @AFrame^.RegRCX;
    regRDX:  Result := @AFrame^.RegRDX;
    regR8:   Result := @AFrame^.RegR8;
    regR9:   Result := @AFrame^.RegR9;
    regXMM0: Result := @AFrame^.RegXMM0;
    regXMM1: Result := @AFrame^.RegXMM1;
    regXMM2: Result := @AFrame^.RegXMM2;
    regXMM3: Result := @AFrame^.RegXMM3;
  else
    Result := @PByte(@AFrame^.Args)[FOffset];
  end;

  if FByRefParam then
    Result := PPointer(Result)^;
end;
{$ELSEIF Defined(Linux64) or Defined(OSX64) or (Defined(IOSSIMULATOR) and Defined(CPUX64))}
begin
  case FOffset of
    $10000000..$10000007:                         // XMM0-XMM7
      Result := @AFrame^.RegXMM[FOffset and $F];
    $10000008..$1000000D:                         // RDI, RSI, RDX, RCX, R8, R9
      Result := @AFrame^.RegR[FOffset and 7];
    $1000000E:
      Result := @AFrame^.OutRAX;                  // RAX, if size = 16 will include OutRDX
    $10000010:
      Result := @AFrame^.OutFPU;
  else
    Result := @PByte(@AFrame^.Args)[FOffset];     // Stack
  end;

  if FByRefParam then
    Result := PPointer(Result)^;
end;
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}
{$ELSEIF Defined(CPUX86)}
begin
  case FOffset of
    regEAX: Result := @AFrame^.RegEAX;
    regEDX: Result := @AFrame^.RegEDX;
    regECX: Result := @AFrame^.RegECX;
    regFloat:
    begin
      Result := @AFrame^.FP.RegSingle;
    end;
  else
    Result := @PByte(@AFrame^.Args)[FOffset];
  end;

  if FByRefParam then
    Result := PPointer(Result)^;
end;

{$ELSEIF Defined(CPUARM32)}
begin
  case FOffset of
    $10000000: Result := @AFrame^.RegCR0;
    $10000001: Result := @AFrame^.RegCR1;
    $10000002: Result := @AFrame^.RegCR2;
    $10000003: Result := @AFrame^.RegCR3;

    $10000010..$1000001F: // S0..S15
      Result := @AFrame^.RegFP.S[FOffset and $F];
    $10000020..$10000027: // D0..D7
      Result := @AFrame^.RegFP.D[FOffset and $7];
  else
    Result := @PByte(@AFrame^.Args)[FOffset];
  end;

  if FByRefParam then
    Result := PPointer(Result)^;
end;
{$ELSEIF Defined(CPUARM64)}
begin
  case FOffset of
    $10000000..$10000007:
      Result := @AFrame^.RegCR[FOffset and $F];
    $10000008:
      Result := @AFrame^.RegCR8;

    $10000010..$10000017: // Q0..Q7
      Result := @AFrame^.RegFP[FOffset and $F];
  else
    Result := @PByte(@AFrame^.Args)[FOffset];
  end;

  if FByRefParam then
    Result := PPointer(Result)^;
end;
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}

procedure TMethodImplementation.TParamLoc.SetArg(AFrame: PInterceptFrame;
  const Value: TValue);
begin
  if (FTypeInfo <> nil) and (Value.TypeInfo <> FTypeInfo) then
    Value.Cast(FTypeInfo).ExtractRawData(GetArgLoc(AFrame))
  else
    Value.ExtractRawData(GetArgLoc(AFrame));
end;

procedure RawIntercept;
{$IF Defined(CPUX86)}
asm
  // Upon entry:
  //
  // [original arguments]
  // [return address]
  // [frame pointer]            <-- EBP
  // [method impl]              <-- ESP (no PC_MAPPED_EXCEPTIONS)
  // [Bytes To Pop]                     (PC_MAPPED_EXCEPTIONS only)
  // [Fake return addr]         <-- ESP (PC_MAPPED_EXCEPTIONS only)
  PUSH ECX
  PUSH EDX
  PUSH EAX
  SUB ESP, TYPE TFloatReg
  MOV EDX, ESP
  MOV EAX, [EBP-4]

  // Record for stack @ESP now:
  //
  //  TInterceptFrame = record
  //    FP: TFloatReg;
  //    RegEAX: Pointer;
  //    RegEDX: Pointer;
  //    RegECX: Pointer;
  //    FakeRA: Pointer (PC_MAPPED_EXCEPTIONS only)
  //    BytesToPop: Pointer (PC_MAPPED_EXCEPTIONS only)
  //    Implementation: TMethodImplementation;
  //    PreviousFrame: Pointer;
  //    RetAddr: Pointer;
  //    Args: record end;
  //  end;
{$IFDEF ALIGN_STACK}
//  SUB  ESP, 4
{$ENDIF ALIGN_STACK}
  CALL TRttiMethod.TMethodImplementation.Intercept
{$IFDEF ALIGN_STACK}
//  ADD  ESP, 4
{$ENDIF ALIGN_STACK}

  // Now:
  //
  // [original arguments]
  // [return address]
  // [frame pointer]            <-- EBP
  // [TProxyClassInfo instance]
  // [invoke info]
  // [ECX]
  // [EDX]
  // [EAX]                      <-- ESP

  MOV ECX, ESP // ECX: PInterceptFrame

  MOV ESP, EBP
  POP EBP

  // ESP pointing at return address
  POP EAX // pop return address...
  MOV EAX, [ECX].TMethodImplementation.TInterceptFrame.Impl
  MOV EDX, [EAX].TMethodImplementation.FInvokeInfo
  MOVZX EAX, BYTE PTR [EDX].TMethodImplementation.TInvokeInfo.FCallerPopsStack
  TEST EAX,EAX
  JNZ @@no_pop
  MOV EAX, [EDX].TMethodImplementation.TInvokeInfo.FStackSize
  ADD ESP, EAX // ...and arguments

@@no_pop:
  // Push floating-point arguments on FP stack as needed
  MOVSX EAX, BYTE PTR [ECX].TMethodImplementation.TInterceptFrame.FP.Kind
  INC EAX
  JNZ @@do_fp_push
  NOP
  JMP @@done_fp_push

@@do_fp_push:
  DEC EAX
  // ftSingle, ftDouble, ftExtended, ftComp, ftCurr
  JZ @@single
  DEC EAX
  JZ @@double
  DEC EAX
  JZ @@extended

  // => currency or comp
  FILD QWORD PTR [ECX].TMethodImplementation.TInterceptFrame.FP.RegCurr
  JMP @@done_fp_push

@@single:
  FLD DWORD PTR [ECX].TMethodImplementation.TInterceptFrame.FP.RegSingle
  JMP @@done_fp_push

@@double:
  FLD QWORD PTR [ECX].TMethodImplementation.TInterceptFrame.FP.RegDouble
  JMP @@done_fp_push

@@extended:
  FLD TBYTE PTR [ECX].TMethodImplementation.TInterceptFrame.FP.RegExtended

@@done_fp_push:
  MOV EAX, DWORD PTR [ECX].TMethodImplementation.TInterceptFrame.RegEAX
  MOV EDX, DWORD PTR [ECX].TMethodImplementation.TInterceptFrame.RegEDX

  // RTTI

  JMP [ECX].TMethodImplementation.TInterceptFrame.RetAddr
end; // of CPUX86 RawIntercept implementation
{$ELSEIF Defined(CPUX64)}
{$IF Defined(MSWINDOWS)}
{$IFDEF ASSEMBLER}
var
  // TInterceptFrame floating registers in reverse order to keep growing offsets
    regXMM3,
    regXMM2,
    regXMM1,
    regXMM0: Pointer;
asm
    .PARAMS 2

    MOVSD regXMM0, XMM0
    MOVSD regXMM1, XMM1
    MOVSD regXMM2, XMM2
    MOVSD regXMM3, XMM3

    MOV   [RSP+$50], RCX // Save RCX, RDX, R8, R9 in their shadow
    MOV   [RSP+$58], RDX // space
    MOV   [RSP+$60], R8
    MOV   [RSP+$68], R9

    MOV   RCX, RAX
    LEA   RDX, [RSP+$20]
    CALL  TRttiMethod.TMethodImplementation.Intercept

    MOV   RAX,  [RSP+$50] // Overload RCX and RAX
    MOVSD XMM0, [RSP+$50] // Returned floating types are stored in XMM0
end; // of CPUX64 RawIntercept implementation
{$ELSE  ASSEMBLER}
begin
                                                                                 
end;
{$ENDIF ASSEMBLER}
{$ELSE !MSWINDOWS} // Linux
  external RTLHelperLibName name 'rtti_raw_intercept';
{$ENDIF !MSWINDOWS}

{$ELSEIF Defined(CPUARM)}
  external RTLHelperLibName name {$IFDEF LINUX} '_' + {$ENDIF} 'rtti_raw_intercept';
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}

{$IFDEF CPUX86}
var
  _CodeHeap : TPrivateHeap;
{$ENDIF CPUX86}

function AllocFirstStageIntercept(Proc: Pointer; PushVal: Pointer; BytesToPop: Integer): TMethodImplementation.PFirstStageIntercept;
{$IF Defined(CPUX64)}
begin
  CodeHeap.GetMem(Result, SizeOf(Result^));
  Result^.MovRAX_48 := $48;
  Result^.MovRAX_B8 := $B8;
  Result^.SelfVal := PushVal;
  Result^.MovR10_49 := $49;
  Result^.MovR10_BA := $BA;
  Result^.Target := Proc;
  Result^.JmpR10_49 := $49;
  Result^.JmpR10_FF := $FF;
  Result^.JmpR10_E2 := $E2;
end;
{$ELSEIF Defined(CPUX86)}
begin
  _CodeHeap := CodeHeap;
  _CodeHeap.GetMem(Result, SizeOf(Result^) * 2);

  // Setting up stack frame before pushing arguments should make stack unwinding
  // easier for everyone.
  Result^.PushEBP_55 := $55;
  Result^.MovEBP_ESP_1_89 := $89;
  Result^.MovEBP_ESP_2_E5 := $E5;
  Result^.Push_68 := $68;
  Result^.PushVal := PushVal;
{$IFDEF PC_MAPPED_EXCEPTIONS}
  // In the PC Mapped environment, if an exception is thrown past this thunk, we have
  // to play a trick.  The target of this thunk will be unwound by the normal stack
  // unwind process, but this thunk will confuse the unwinder.  To deal with that,
  // we trick the unwinder.  We push a fake return address, which is just the address
  // of this thunk onto the stack before we jump to the thunk target.  The unwinder
  // will then have that fake return address to look up in the PC Map tables.  The
  // private heap will have registered an unwinder for all the memory blocks that
  // it allocates for thunks.  So the unwinder will end up calling the frame unwind
  // function that we provided when we allocated the thunk's memory, and that personality
  // function will handle getting the unwinder past this frame.
  Result^.FakeRA_Push_68 := $68;
  Result^.FakeRA_PushVal := Result;
  Result^.BTP_Push_68 := $68;
  Result^.BTP_PushVal := BytesToPop;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  Result^.JmpRel_E9 := $E9;
  Result^.RelTarget := IntPtr(Proc) -
//  Result^.RelTarget := NativeInt(@RawIntercept) -
    (Integer(@Result^.JmpRel_E9) + 1 + 4); // 1 (jmp) + rel32
end;
{$ELSEIF Defined(CPUARM) and Defined(LINUX)}
begin
                                                                                                                    
  CodeHeap.GetMem(Result, SizeOf(Result^));
//  Result^.MovRAX_48 := $48;
//  Result^.MovRAX_B8 := $B8;
//  Result^.SelfVal := PushVal;
//  Result^.JmpRel_E9 := $E9;
//  Result^.RelTarget := NativeInt(@RawIntercept) -
//  Result^.RelTarget := IntPtr(Proc) -
//    (IntPtr(@Result^.JmpRel_E9) + 1 + 4); // 1 (jmp) + rel32
end;
{$ELSEIF Defined(CPUARM) and (Defined(ANDROID) or Defined(MACOS))}
     cdecl; external RTLHelperLibName name 'allocate_first_stage_intercept';
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}

procedure FreeIntercept(AIntercept: TMethodImplementation.PFirstStageIntercept);
{$IF ((Defined(CPUX86) or Defined(CPUX64)) and (Defined(LINUX) or Defined(MSWINDOWS) or Defined(MACOS))) or (Defined(CPUARM) and Defined(LINUX))}
begin
  CodeHeap.FreeMem(AIntercept);
end;
{$ELSEIF Defined(CPUARM) and (Defined(Android) or Defined(MACOS))}
     cdecl; external RTLHelperLibName name 'free_first_stage_intercept';
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Method not implemented for CPU'}
{$ENDIF}

{ TMethodImplementation }

constructor TMethodImplementation.Create;
begin
  raise EInvalidOpException.CreateRes(@SVarNotImplemented);
end;

constructor TMethodImplementation.Create(AUserData: Pointer; AInvokeInfo: TInvokeInfo;
  const ACallback: TMethodImplementationCallback);
var
  BytesToPop: Integer;
begin
  BytesToPop := 0;
{$IFDEF CPUX86}
  if not AInvokeInfo.FCallerPopsStack then
    BytesToPop := AInvokeInfo.FStackSize;
{$ENDIF CPUX86}
  FStub := AllocFirstStageIntercept(@RawIntercept, Pointer(Self), BytesToPop);
  FInvokeInfo := AInvokeInfo;
  FUserData := AUserData;
  FCallback := ACallback;
end;

destructor TMethodImplementation.Destroy;
begin
  if FStub <> nil then
    FreeIntercept(FStub);
  inherited;
end;

function TMethodImplementation.GetCodeAddress: Pointer;
begin
  Result := Pointer(FStub);
end;

{ This function has been added to be used from .s .c files in order to avoid use mangled names}
procedure TMethodImplementationIntercept(const obj:TMethodImplementation; AFrame: Pointer); cdecl;
begin
  obj.Intercept(AFrame);
end;
exports TMethodImplementationIntercept;

procedure TMethodImplementation.Intercept(AFrame: PInterceptFrame);
var
  args: TArray<TValue>;
  result: TValue;
  SafeCallResult: HResult;
  typeInfos: TRuntimeTypeInfos;
begin
  typeInfos := nil;
  args := FInvokeInfo.LoadArguments(AFrame, typeInfos);
  SafeCallResult := S_OK;
  try
    try
      if Assigned(FCallback) then
        FCallback(FUserData, args, result);
    except
      if (FInvokeInfo.FCallConv = ccSafeCall) and (FSafeExcHandler <> nil) then
        SafeCallResult := FSafeExcHandler.SafeCallException(ExceptObject, ExceptAddr)
      else
        raise;
    end;
  finally
    FInvokeInfo.SaveArguments(AFrame, args, result);
    if FInvokeInfo.FCallConv = ccSafeCall then
{$IF Defined(CPUX86)}
      AFrame^.RegEAX := Pointer(SafeCallResult)
{$ELSEIF Defined(CPUX64)}
  {$IF Defined(WIN64)}
      AFrame^.RegRCX := Pointer(SafeCallResult)
  {$ELSEIF Defined(LINUX64) or Defined(OSX64)}
      AFrame^.OutRAX := SafeCallResult
  {$ENDIF}
{$ELSEIF Defined(CPUARM32)}
      AFrame^.RegCR0 := SafeCallResult
{$ELSEIF Defined(CPUARM64)}
      AFrame^.RegCR[0] := SafeCallResult
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Missing safecall HResult logic for CPU'}
{$ENDIF}
      ;
    // Destroy TArray<TValue> before runtime (eg, open array) type information
    // will be destroyed. Otherwise memory leak / AV is possible.
    args := nil;
    typeInfos.Free;
  end;
end;

{ TVirtualMethodInterceptor }

constructor TVirtualMethodInterceptor.Create(AClass: TClass);
begin
  FOriginalClass := AClass;
  FIntercepts := TObjectList<TInterceptInfo>.Create(True);
  FImplementationCallback := RawCallback;

  CreateProxyClass;
end;

type
  PProxyClassData = ^TProxyClassData;
  TProxyClassData = record
    SelfPtr: TClass;
    IntfTable: Pointer;
    AutoTable: Pointer;
    InitTable: Pointer;
    TypeInfo: PTypeInfo;
    FieldTable: Pointer;
    MethodTable: Pointer;
    DynamicTable: Pointer;
{$IFNDEF NEXTGEN}
    ClassName: PShortString;
{$ELSE NEXTGEN}
    ClassName: MarshaledAString;
{$ENDIF NEXTGEN}
    InstanceSize: Integer;
    Parent: ^TClass;
  end;

procedure TVirtualMethodInterceptor.CreateProxyClass;

  function GetExtraMethodInfo(m: TRttiMethod): TExtraMethodInfo;
  var
    methodName: string;
  begin
    methodName := m.Name;
    // The following conditions are tested by caller.
    // m.DispatchKind is dkVtable
    // m.MethodKind is mkFunction or mkProcedure
    if methodName = 'FreeInstance' then // do not localize
      Result:= eiFreeInstance
{$IFDEF AUTOREFCOUNT}
    else if methodName = '__ObjAddRef' then // do not localize
      Result:= eiObjAddRef
    else if methodName = '__ObjRelease' then // do not localize
      Result:= eiObjRelease
{$ENDIF AUTOREFCOUNT}
    else
      Result:= eiNormal;
  end;

var
  t: TRttiType;
  m: TRttiMethod;
  size, classOfs: Integer;
  ii: TInterceptInfo;
  extraMInfo: TExtraMethodInfo;
begin
  t := FContext.GetType(FOriginalClass);
  size := (t as TRttiInstanceType).VmtSize;
  classOfs := -vmtSelfPtr;
  FProxyClassData := AllocMem(size);
  FProxyClass := TClass(PByte(FProxyClassData) + classOfs);
  Move((PByte(FOriginalClass) - classOfs)^, FProxyClassData^, size);
  PProxyClassData(FProxyClassData)^.Parent := @FOriginalClass;
  PProxyClassData(FProxyClassData)^.SelfPtr := FProxyClass;

  for m in t.GetMethods do
  begin
    if m.DispatchKind <> dkVtable then
      Continue;
    if not (m.MethodKind in [mkFunction, mkProcedure]) then
      Continue;
    if not m.HasExtendedInfo then
      Continue;
    extraMInfo := GetExtraMethodInfo(m);
{$IFDEF AUTOREFCOUNT}
    if extraMInfo in [eiObjAddRef, eiObjRelease] then
      Continue;
{$ENDIF AUTOREFCOUNT}
    ii := TInterceptInfo.Create(PVtablePtr(FOriginalClass)[m.VirtualIndex],
      m, FImplementationCallback, extraMInfo);
    FIntercepts.Add(ii);
    PVtablePtr(FProxyClass)[m.VirtualIndex] := ii.ProxyCode;
  end;
end;

destructor TVirtualMethodInterceptor.Destroy;
begin
  FIntercepts.Free;
  FreeMem(FProxyClassData);
  inherited;
end;

procedure TVirtualMethodInterceptor.DoAfter(Instance: TObject;
  Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
begin
  if Assigned(FOnAfter) then
    FOnAfter(Instance, Method, Args, Result);
end;

procedure TVirtualMethodInterceptor.DoBefore(Instance: TObject;
  Method: TRttiMethod; const Args: TArray<TValue>; out DoInvoke: Boolean;
  out Result: TValue);
begin
  if Assigned(FOnBefore) then
    FOnBefore(Instance, Method, Args, DoInvoke, Result);
end;

procedure TVirtualMethodInterceptor.DoException(Instance: TObject;
  Method: TRttiMethod; const Args: TArray<TValue>; out RaiseException: Boolean;
  TheException: Exception; out Result: TValue);
begin
  if Assigned(FOnException) then
    FOnException(Instance, Method, Args, RaiseException, TheException, Result);
end;

procedure TVirtualMethodInterceptor.Proxify(AInstance: TObject);
begin
  if PPointer(AInstance)^ <> OriginalClass then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  PPointer(AInstance)^ := ProxyClass;
end;

procedure TVirtualMethodInterceptor.Unproxify(AInstance: TObject);
begin
  if PPointer(AInstance)^ <> ProxyClass then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  PPointer(AInstance)^ := OriginalClass;
end;

procedure TVirtualMethodInterceptor.RawCallback(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
var
  inst: TObject;
  ii: TInterceptInfo;
  dlpArgs: TArray<TValue>;
  physArgs: TArray<TValue>;
  physArgsCnt: Integer;
  parList: TArray<TRttiParameter>;
  par: TRttiParameter;
  i: Integer;
  go: Boolean;
  callConv: TCallConv;
begin
  ii := TInterceptInfo(UserData);
  callConv := ii.Method.CallingConvention;
  inst := Args[0].AsObject;

  SetLength(dlpArgs, Length(Args) - 1);
  physArgsCnt := 1;
  for i := 1 to Length(Args) - 1 do
  begin
    if TMethodImplementation.TRuntimeTypeInfos.IsOpenArray(Args[i].TypeInfo) then
      Inc(physArgsCnt);
    dlpArgs[i - 1] := Args[i];
    Inc(physArgsCnt);
  end;

  try
    go := True;
    DoBefore(inst, ii.Method, dlpArgs, go, Result);
    if go then
    begin
      try
        SetLength(physArgs, physArgsCnt);
{$IFDEF CPUX86}
        if callConv = ccPascal then
        begin
          physArgs[physArgsCnt - 1] := Args[0];
          physArgsCnt := 0;
        end
        else
{$ENDIF}
        begin
          physArgs[0] := Args[0];
          physArgsCnt := 1;
        end;
        parList := ii.Method.GetParameters;
        for i := 1 to Length(Args) - 1 do
        begin
          par := parList[i - 1];
          if TMethodImplementation.TRuntimeTypeInfos.IsOpenArray(Args[i].TypeInfo) then
          begin
            physArgs[physArgsCnt] := dlpArgs[i - 1].GetReferenceToRawData;
            Inc(physArgsCnt);
            physArgs[physArgsCnt] := dlpArgs[i - 1].GetArrayLength;
          end
          else
                                                            
{$IF     defined(CPUX86)}
          if (par.ParamType <> nil) and (
               (par.ParamType.TypeKind = tkMRecord)
            or ((callConv in [ccCdecl, ccStdCall, ccSafeCall]) and (pfConst in par.Flags) and (par.ParamType.TypeKind = tkVariant))
            or ((pfConst in par.Flags) and (par.ParamType.TypeSize > SizeOf(Pointer)) and (par.ParamType.TypeKind <> TTypeKind.tkFloat))
            or ([pfVar, pfOut] * par.Flags <> [])) then
{$ELSEIF defined(WIN64)}
          if (par.ParamType <> nil) and (
               (par.ParamType.TypeKind = tkMRecord)
            or ((pfConst in par.Flags) and (par.ParamType.TypeSize > SizeOf(Pointer)))
            or ([pfVar, pfOut] * par.Flags <> [])) then
{$ELSEIF defined(MACOS64) or defined(LINUX64)}
          if (par.ParamType <> nil) and (
               (par.ParamType.TypeKind = tkMRecord)
            or ((par.ParamType.TypeKind in [tkRecord, tkArray]) and
                (callConv in [ccReg, ccPascal]) and
                (par.ParamType.TypeSize > 4))
            or ([pfVar, pfOut] * par.Flags <> [])) then
{$ELSEIF defined(CPUARM)}
          if (par.ParamType <> nil) and (
               (par.ParamType.TypeKind = tkMRecord)
            or ((callConv in [ccReg]) and (par.ParamType.TypeKind in [tkRecord]))
            or ([pfVar, pfOut] * par.Flags <> [])) then
{$ELSE OTHERCPU}
  {$MESSAGE Fatal 'Missing RawCallback logic for CPU'}
{$ENDIF}
            physArgs[physArgsCnt] := dlpArgs[i - 1].GetReferenceToRawData
          else
            physArgs[physArgsCnt] := dlpArgs[i - 1];
          Inc(physArgsCnt);
        end;

        if ii.Method.ReturnType <> nil then
          Result := Invoke(ii.OriginalCode, physArgs, callConv, ii.Method.ReturnType.Handle)
        else
          Result := Invoke(ii.OriginalCode, physArgs, callConv, nil);
      except
        on e: Exception do
        begin
          DoException(inst, ii.Method, dlpArgs, go, e, Result);
          if go then
            raise;
        end;
      end;
      if ii.ExtraMethodInfo = eiFreeInstance then
      begin
        Pointer(inst) := nil;
{$IFDEF AUTOREFCOUNT}
        Pointer(Args[0].FValueData.GetReferenceToRawData^) := nil;
{$ENDIF AUTOREFCOUNT}
      end;
      DoAfter(inst, ii.Method, dlpArgs, Result);
    end;
  finally
    // Set modified by-ref arguments
    for i := 1 to Length(Args) - 1 do
      Args[i] := dlpArgs[i - 1];
  end;
end;

{ TVirtualMethodInterceptor.TInterceptInfo }

constructor TVirtualMethodInterceptor.TInterceptInfo.Create(AOriginalCode: Pointer; AMethod: TRttiMethod;
  const ACallback: TMethodImplementationCallback;
  const ExtraMethodInfo: TExtraMethodInfo);
begin
  FImpl := AMethod.CreateImplementation(Pointer(Self), ACallback);
  FOriginalCode := AOriginalCode;
  FProxyCode := FImpl.CodeAddress;
  FMethod := AMethod;
  FExtraMethodInfo := ExtraMethodInfo;
end;

destructor TVirtualMethodInterceptor.TInterceptInfo.Destroy;
begin
  FImpl.Free;
  inherited;
end;

{ TRawVirtualClass.TVTable }

constructor TRawVirtualClass.TVTable.Create(MethodCount: Integer);
var
  SlotCount: Integer;
begin
//  SlotCount := GetMethodCount + RESERVED_VTABLE_SLOTS;
  SlotCount := MethodCount + RESERVED_VTABLE_SLOTS;
  SetLength(FInterceptors, MethodCount);
  SetLength(FVTable, SlotCount);
  FVTable[0] := @TRawVirtualClass._QIFromIntf;
  FVTable[1] := @TRawVirtualClass._AddRefFromIntf;
  FVTable[2] := @TRawVirtualClass._ReleaseFromIntf;
end;

destructor TRawVirtualClass.TVTable.Destroy;
begin
end;

function TRawVirtualClass.TVTable.GetVTable: TArray<pointer>;
begin
  Result := FVTable;
end;

procedure TRawVirtualClass.TVTable.SetVTableSlot(Idx: Integer; Proc: Pointer; Context: Pointer);
begin
  if Assigned(FInterceptors[Idx]) then
    FreeIntercept(TMethodImplementation.PFirstStageIntercept(FInterceptors[Idx]));
                                                                                      
  // we'll have to change the SetVTableSlot API to include a count of bytes to pop for callee
  // cleanup vtable slot entries.
  FInterceptors[Idx] := AllocFirstStageIntercept(Proc, Context, 0);
  FVTable[Idx + RESERVED_VTABLE_SLOTS] := Pointer(FInterceptors[Idx]);
end;

procedure TRawVirtualClass.TVTable.SetVTableSlot(Idx: Integer; Proc: Pointer);
begin
  if Assigned(FInterceptors[Idx]) then
  begin
    FreeIntercept(TMethodImplementation.PFirstStageIntercept(FInterceptors[Idx]));
    FInterceptors[Idx] := nil;
  end;
  FVTable[Idx + RESERVED_VTABLE_SLOTS] := Proc;
end;

class function TRawVirtualClass.TVTable.AllocateRawThunk(Proc: Pointer; Context: Pointer; BytesToPop: Integer): Pointer;
begin
  Result := Pointer(AllocFirstStageIntercept(Proc, Context, BytesToPop));
end;

class procedure TRawVirtualClass.TVTable.FreeRawThunk(Thunk: Pointer);
begin
  FreeIntercept(TMethodImplementation.PFirstStageIntercept(Thunk));
end;

{ TRawVirtualClass }

constructor TRawVirtualClass.Create(Guids: TArray<TGUID>; VTable: TRawVirtualClass.TVTable);
begin
  FIIDs := Guids;
  FVTable.Obj := Pointer(Self);
  FVTable.VTable := VTable.VTable;
end;

destructor TRawVirtualClass.Destroy;
begin
                                                                    
  inherited Destroy;
end;

function TRawVirtualClass._AddRefFromIntf: Integer; stdcall;
begin
  Result := TRawVirtualClass(GetInstanceFromInterface(Pointer(Self)))._AddRef;
end;

function TRawVirtualClass._ReleaseFromIntf: Integer; stdcall;
begin
  Result := TRawVirtualClass(GetInstanceFromInterface(Pointer(Self)))._Release;
end;

function TRawVirtualClass._QIFromIntf(const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := TRawVirtualClass(GetInstanceFromInterface(Pointer(Self))).QueryInterface(IID, Obj);
end;

function TRawVirtualClass.FindInterface(IID: TGUID): Pointer;
var
  I: Integer;
begin
  for I := 0 to Length(FIIDs) - 1 do
    if IID = FIIDs[I] then
      Exit(@FVTable.VTable);
  Result := nil;
end;

function TRawVirtualClass.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
var
  P: Pointer;
begin
  P := FindInterface(IID);
  if Assigned(P) then
  begin
    _AddRef;
    Pointer(Obj) := P;
    Exit(S_OK);
  end;
  Result := inherited;
end;

class function TRawVirtualClass.GetInstanceFromInterface(Intf: Pointer): TRawVirtualClass;
var
  Bound: PBoundInterface;
begin
  // The interface was stored in a BoundInterface record, so that we could
  // do pointer math to get back to a pointer to the instance that holds it.
  Bound := PBoundInterface(PByte(Intf) - SizeOf(Pointer));
  Result := TRawVirtualClass(Bound^.Obj);
end;

{ TVirtualInterface }

constructor TVirtualInterface.Create(PIID: PTypeInfo);
var
  Methods: TArray<TRttiMethod>;
  Method: TRttiMethod;
  Typ: TRttiType;
  MaxIndex, I: Integer;
  ImplInfo: TImplInfo;
begin
  FIntercepts := TObjectList<TImplInfo>.Create(True);
  Typ := FContext.GetType(PIID);
  FIID := TRttiInterfaceType(Typ).GUID;
                              
  Methods := Typ.GetMethods;
  MaxIndex := 2;  // Is this the best way to do this?
  for Method in Methods do
  begin
    if MaxIndex < Method.VirtualIndex then
      MaxIndex := Method.VirtualIndex;
    ImplInfo := TImplInfo.Create(Method, RawCallBack);
    ImplInfo.FImpl.FSafeExcHandler := Self;
    FIntercepts.Add(ImplInfo);
  end;

  VTable := AllocMem(SizeOf(Pointer)* (MaxIndex+1));
  PVtablePtr(VTable)[0] := @TVirtualInterface._QIFromIntf;
  PVtablePtr(VTable)[1] := @TVirtualInterface._AddRefFromIntf;
  PVtablePtr(VTable)[2] := @TVirtualInterface._ReleaseFromIntf;
  for I := 0 to FIntercepts.Count-1 do
    PVtablePtr(VTable)[FIntercepts[I].VirtualIndex] := FIntercepts[I].CodeAddress;
  for I := 3 to MaxIndex do
    if PVtablePtr(VTable)[I] = nil then
      PVtablePtr(VTable)[I] := @TVirtualInterface.ErrorProc;
end;

constructor TVirtualInterface.Create(PIID: PTypeInfo;
  InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(PIID);
  FOnInvoke := InvokeEvent;
end;

destructor TVirtualInterface.Destroy;
begin
  if VTable <> nil then
    FreeMem(VTable);
  FIntercepts.Free;
  inherited;
end;

procedure TVirtualInterface.RawCallback(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
begin
  if Assigned(FOnInvoke) then
    FOnInvoke(TImplInfo(UserData).FMethod, Args, Result);
end;

procedure TVirtualInterface.ErrorProc;
begin
  raise InsufficientRtti;
end;

function TVirtualInterface._AddRefFromIntf: Integer;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.VTable) - PByte(Self)))._AddRef;
end;

function TVirtualInterface._ReleaseFromIntf: Integer;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.VTable) - PByte(Self)))._Release;
end;

function TVirtualInterface._QIFromIntf(const IID: TGUID; out Obj): HResult;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.VTable) - PByte(Self))).QueryInterface(IID, Obj);
end;

function TVIrtualInterface._AddRef: Integer;
begin
  Result := inherited
end;

function TVIrtualInterface._Release: Integer;
begin
  Result := inherited
end;

function TVirtualInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if iid = FIID then
  begin
    _AddRef;
    Pointer(Obj) := @VTable;
    Result := S_OK;
  end
  else
    Result := inherited
end;

{ TVirtualInterface.TImplInfo }

constructor TVirtualInterface.TImplInfo.Create(AMethod: TRttiMethod;
  const ACallback: TMethodImplementationCallback);
begin
  FImpl := AMethod.CreateImplementation(Pointer(Self), ACallback);
  FMethod := AMethod;
end;

destructor TVirtualInterface.TImplInfo.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TVirtualInterface.TImplInfo.GetCodeAddress: Pointer;
begin
  Result := FImpl.CodeAddress;
end;

function TVirtualInterface.TImplInfo.GetVirtualIndex: SmallInt;
begin
  Result := FMethod.VirtualIndex;
end;

{ TRttiIndexedProperty }

const
  pafIsDefault = 1 shl 0;
  pafVisibilityShift = 1;
  pafVisibilityBits = 2;

function TRttiIndexedProperty.GetHandle: PArrayPropInfo;
begin
  Result := PArrayPropInfo(inherited Handle);
end;

constructor TRttiIndexedProperty.Create(APackage: TRttiPackage;
  AParent: TRttiObject; var P: PByte);
begin
  inherited;
  P := PByte( Handle.AttrData );
  FAttributeGetter := LazyLoadAttributes(P);
end;

function TRttiIndexedProperty.GetIsDefault: Boolean;
begin
  Result := (Handle^.Flags and pafIsDefault) <> 0;
end;

function TRttiIndexedProperty.GetIsReadable: Boolean;
begin
  Result := Handle^.ReadIndex <> $FFFF;
end;

function TRttiIndexedProperty.GetIsWritable: Boolean;
begin
  Result := Handle^.WriteIndex <> $FFFF;
end;

procedure TRttiIndexedProperty.GetAccessors;

  procedure DoGetAccessors;
  var
    x: TArray<TRttiMethod>;
    i, count: Integer;
  begin
    x := Parent.GetDeclaredMethods;
    i := 0;
    count := 0;
    while i < Length(x) do
    begin
      if x[i] is TRttiInstanceMethodEx then
      begin
        if i <> count then
          x[count] := x[i];
        Inc(count);
      end;
      Inc(i)
    end;
    if IsReadable then
      FReadMethod := x[Handle^.ReadIndex];
    if IsWritable then
      FWriteMethod := x[Handle^.WriteIndex];
  end;

begin
  if Assigned(FReadMethod) or Assigned(FWriteMethod) or
     not IsReadable and not IsWritable then
    Exit;
  DoGetAccessors;
end;

function TRttiIndexedProperty.GetReadMethod: TRttiMethod;
begin
  if not IsReadable then
    Result := nil
  else
  begin
    if FReadMethod = nil then
      GetAccessors;
    Result := FReadMethod;
  end;
end;

function TRttiIndexedProperty.GetWriteMethod: TRttiMethod;
begin
  if not IsWritable then
    Result := nil
  else
  begin
    if FWriteMethod = nil then
      GetAccessors;
    Result := FWriteMethod;
  end;
end;

function TRttiIndexedProperty.GetName: string;
begin
  Result := Handle.NameFld.ToString;
end;

function TRttiIndexedProperty.HasName(const AName: string): Boolean;
begin
  Result := Handle.NameFld.HasName(AName);
end;

function TRttiIndexedProperty.GetPropertyType: TRttiType;
var
  p: TArray<TRttiParameter>;
begin
  GetAccessors;
  if FReadMethod <> nil then
    Result := FReadMethod.ReturnType
  else if FWriteMethod <> nil then
  begin
    p := FWriteMethod.GetParameters;
    Result := p[Length(p) - 1].ParamType;
  end
  else
    Result := nil;
end;

function TRttiIndexedProperty.GetValue(Instance: Pointer;
  const Args: array of TValue): TValue;
var
  getter: TRttiMethod;
begin
  getter := ReadMethod;
  if getter = nil then
    raise EPropWriteOnly.Create(Name);
  if getter.IsStatic or getter.IsClassMethod then
    Result := getter.Invoke(TClass(Instance), Args)
  else
    Result := getter.Invoke(TObject(Instance), Args);
end;

function TRttiIndexedProperty.GetVisibility: TMemberVisibility;
begin
  Result := TMemberVisibility(
    GetBitField(Handle^.Flags, pafVisibilityShift, pafVisibilityBits));
end;

procedure TRttiIndexedProperty.SetValue(Instance: Pointer;
  const Args: array of TValue; const Value: TValue);
var
  setter: TRttiMethod;
  argsV: TArray<TValue>;
  i: Integer;
begin
  setter := WriteMethod;
  if setter = nil then
    raise EPropReadOnly.Create(Name);
  SetLength(argsV, Length(Args) + 1);
  for i := 0 to High(Args) do
    argsV[i] := Args[i];
  argsV[Length(Args)] := Value;
  if setter.IsStatic or setter.IsClassMethod then
    setter.Invoke(TClass(Instance), argsV)
  else
    setter.Invoke(TObject(Instance), argsV);
end;

function TRttiIndexedProperty.ToString: string;
var
  m: TRttiMethod;
  p: TArray<TRttiParameter>;
  i: Integer;
begin
  Result := 'property ' + Name + '['; // do not localize
  m := ReadMethod;
  if m <> nil then
  begin
    p := m.GetParameters;
    for i := 0 to Length(p) - 2 do
      Result := Result + p[i].ToString + ', ';
    Result := Result + p[Length(p) - 1].ToString;
  end
  else
  begin
    m := WriteMethod;
    if m = nil then
      Exit(Result + ']');
    p := m.GetParameters;
    for i := 0 to Length(p) - 3 do
      Result := Result + p[i].ToString + ', ';
    Result := Result + p[Length(p) - 2].ToString;
  end;
  Result := Result + ']: ';
  if m.ReturnType <> nil then // getter
    Result := Result + m.ReturnType.ToString
  else // setter
    Result := Result + p[Length(p) - 1].ToString;
end;

function IsStoredPropCA(Instance: TObject; PropInfo: PPropInfo): Boolean;

  function FindProperty(var t: TRttiType; const PropInfo: PPropInfo): TRttiProperty;
  var
    R: TRttiProperty;
  begin
    while t <> nil do
    begin
      for R in t.GetDeclaredProperties do
        if TRttiInstanceProperty(R).PropInfo.NameFld = PropInfo.NameFld then
          exit(R);
      t := t.BaseType;
    end;
    Result := nil;
  end;

var
  RttiType: TRttiType;

  function _FindField(const AName: String): TRttiField;
  var
    R: TRttiField;
    t: TRttiType;
  begin
    t := RttiType;
    while t <> nil do
    begin
      for R in t.GetDeclaredFields do
        if PFieldExEntry(R.Handle)^.NameFld.ToString = AName then
          exit(R);
      t := t.BaseType;
    end;
    Result := nil;
  end;

  function FindMethod(const AName: String): TRttiMethod;
  var
    R: TRttiMethod;
    t: TRttiType;
    s: string;
  begin
    t := RttiType;
    s := AName;
    while t <> nil do
    begin
      for R in t.GetDeclaredMethods do
        if R.Name = s then
          exit(R);
      t := t.BaseType;
    end;
    Result := nil;
  end;

var
  Context: TRttiContext;
  RttiField: TRttiField;
  RttiMethod: TRttiMethod;
  Attribute: TCustomAttribute;
  StoredAtri: StoredAttribute;
  StorageName: String;
  aRttiProperty: TRttiProperty;
begin
  Result := True; // Default is True.
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Instance.ClassInfo);
    while RttiType <> nil do
    begin
      aRttiProperty := FindProperty(RttiType, PropInfo);
      if aRttiProperty <> nil then
      begin
        for Attribute in aRttiProperty.GetAttributes do
        begin
          if Attribute is StoredAttribute then
          begin
            StoredAtri := StoredAttribute(Attribute);
            if StoredAtri.Name <> '' then
            begin
              StorageName := StoredAtri.Name;
              RttiField := _FindField(StorageName);
              if RttiField <> nil then
                Result := RttiField.GetValue(Instance).AsBoolean
              else
              begin
                RttiMethod := FindMethod(StorageName);
                if RttiMethod <> nil then
                  Result := RttiMethod.Invoke(Instance, []).AsBoolean;
              end;
            end
            else
              Result := StoredAtri.Flag;
            Exit;
          end;
        end;
      end;
      if RttiType <> nil then
        RttiType := RttiType.BaseType;
    end;
  finally
    Context.Free;
  end;
end;

initialization
  System.TypInfo.IsStoredPropCA := @IsStoredPropCA;
  RegisterWeakRefTypeInfo(TypeInfo(TValue), System.HasWeakRef(TValue), 0);
  AddModuleUnloadProc(OnUnloadModule);
  PoolLock;
{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
  GCTokenLock;
{$ENDIF}
finalization
{$IFDEF USE_MONITOR_FOR_GLOBALCONTEXT}
  FreeAndNil(_GlobalContextTokenLock);
{$ENDIF}
  FreeAndNil(_PoolLock);
  RemoveModuleUnloadProc(OnUnloadModule);
  FreeAndNil(FCodeHeap);
end.


