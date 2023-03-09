{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Generics.Defaults;

{$R-,T-,X+,H+,B-}

(*$HPPEMIT '#pragma option -w-8022'*)

interface

uses
  System.TypInfo;

type
  IComparer<T> = interface
    function Compare(const Left, Right: T): Integer;
  end;

  IEqualityComparer<T> = interface
    function Equals(const Left, Right: T): Boolean;
    function GetHashCode(const Value: T): Integer;
  end;

  TComparison<T> = reference to function(const Left, Right: T): Integer;

  // Abstract base class for IComparer<T> implementations, and a provider
  // of default IComparer<T> implementations.
  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  public
    class function _Default: Pointer; static; inline;
    class function _Construct(const Comparison: TComparison<T>): Pointer; static; inline;
    class function Default: IComparer<T>; static; inline;
    class function Construct(const Comparison: TComparison<T>): IComparer<T>; static; inline;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  TEqualityComparison<T> = reference to function(const Left, Right: T): Boolean;
  THasher<T> = reference to function(const Value: T): Integer;

  // Abstract base class for IEqualityComparer<T> implementations, and a provider
  // of default IEqualityComparer<T> implementations.
  TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
  public
    class function _Default: Pointer; static; inline;
    class function Default: IEqualityComparer<T>; static; inline;
    class function Construct(const EqualityComparison: TEqualityComparison<T>;
      const Hasher: THasher<T>): IEqualityComparer<T>; static;

    function Equals(const Left, Right: T): Boolean;
      reintroduce; overload; virtual; abstract;
    function GetHashCode(const Value: T): Integer;
      reintroduce; overload; virtual; abstract;
  end;

  // A non-reference-counted IInterface implementation.
  TSingletonImplementation = class(TNoRefCountObject)
  end;

  TDelegatedEqualityComparer<T> = class(TEqualityComparer<T>)
  private
    FEquals: TEqualityComparison<T>;
    FGetHashCode: THasher<T>;
  public
    constructor Create(const AEquals: TEqualityComparison<T>;
      const AGetHashCode: THasher<T>);
    function Equals(const Left, Right: T): Boolean; overload; override;
    function GetHashCode(const Value: T): Integer; overload; override;
  end;

  TDelegatedComparer<T> = class(TComparer<T>)
  private
    FCompare: TComparison<T>;
  public
    constructor Create(const ACompare: TComparison<T>);
    function Compare(const Left, Right: T): Integer; override;
  end;

  TCustomComparer<T> = class(TSingletonImplementation, IComparer<T>, IEqualityComparer<T>)
  protected
    function Compare(const Left, Right: T): Integer; virtual; abstract;
    function Equals(const Left, Right: T): Boolean;
      reintroduce; overload; virtual; abstract;
    function GetHashCode(const Value: T): Integer;
      reintroduce; overload; virtual; abstract;
  end;

  TStringComparer = class(TCustomComparer<string>)
  private
    class var
      FOrdinal: TCustomComparer<string>;
  public
    class destructor Destroy;
    class function Ordinal: TStringComparer;
  end;

  TIStringComparer = class(TCustomComparer<string>)
  private
    class var
      FOrdinal: TCustomComparer<string>;
  public
    class destructor Destroy;
    class function Ordinal: TStringComparer;
  end;

  TOrdinalIStringComparer = class(TStringComparer)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean;
      reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer;
      reintroduce; overload; override;
  end;

function BobJenkinsHash(const Data; Len, InitData: Integer): Integer; deprecated 'Use System.Hash.THashBobJenkins.GetHashValue';
function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;

// Must be in interface section to be used by generic method. For internal use only.
type
  TDefaultGenericInterface = (giComparer, giEqualityComparer);

function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo; size: Integer): Pointer;
function _AreAnonMethodsCapturing(const AnonMethodPtrs: array of const): Boolean;
function _MakeInterfaceInstance(const AnonMethodPtrs: array of const): Pointer;

implementation

uses
  System.SysUtils, System.Hash, System.Math, System.Variants;

type
  PSimpleInstance = ^TSimpleInstance;
  TSimpleInstance = record
    Vtable: Pointer;
    Info: PTypeInfo;
    RefCount: Integer;
    Size: Integer;
  end;

  TInfoFlags = set of (ifVariableSize, ifSelector);
  PVtableInfo = ^TVtableInfo;
  TVtableInfo = record
    Flags: TInfoFlags;
    Data: Pointer;
  end;

  TTypeInfoSelector = function(info: PTypeInfo; size: Integer): Pointer;

  TThreeByteData = array [0..2] of Byte;

function MakeInstance(vtable: Pointer; info: PTypeInfo; sizeField: Integer): Pointer;
var
  inst: PSimpleInstance;
begin
  GetMem(inst, SizeOf(inst^));
  inst^.Vtable := vtable;
  inst^.RefCount := 0;
  inst^.Size := sizeField;
  inst^.Info := info;
  Result := inst;
end;

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function MemAddref(inst: PSimpleInstance): Integer; stdcall;
begin
  Result := AtomicIncrement(inst^.RefCount);
end;

function MemRelease(inst: PSimpleInstance): Integer; stdcall;
begin
  Result := AtomicDecrement(inst^.RefCount);
  if Result = 0 then
    FreeMem(inst);
end;

// I/U 1-4

function Compare_I1(Inst: Pointer; const Left, Right: Shortint): Integer;
begin
  { Use subtraction }
  Result := Left - Right;
end;

function Equals_I1(Inst: Pointer; const Left, Right: Shortint): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_I1(Inst: Pointer; const Value: Shortint): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

function Compare_I2(Inst: Pointer; const Left, Right: Smallint): Integer;
begin
  { Use subtraction }
  Result := Left - Right;
end;

function Equals_I2(Inst: Pointer; const Left, Right: Smallint): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_I2(Inst: Pointer; const Value: Smallint): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

function Compare_I4(Inst: Pointer; const Left, Right: Integer): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_I4(Inst: Pointer; const Left, Right: Integer): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_I4(Inst: Pointer; const Value: Integer): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

{$IFDEF EABI_RECORD_ARRAY_BYREF}
// I/U 1-4

function Equals_Ref_I1(Inst: Pointer; const [Ref] Left, Right: Shortint): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_Ref_I1(Inst: Pointer; const [Ref] Value: Shortint): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Shortint));
end;

function Equals_Ref_I2(Inst: Pointer; const [Ref] Left, Right: Smallint): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_Ref_I2(Inst: Pointer; const [Ref] Value: Smallint): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Smallint));
end;

function Equals_Ref_I4(Inst: Pointer; const [Ref] Left, Right: Integer): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_Ref_I4(Inst: Pointer; const [Ref] Value: Integer): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Integer));
end;
{$ENDIF EABI_RECORD_ARRAY_BYREF}

function Compare_U1(Inst: Pointer; const Left, Right: Byte): Integer;
begin
  { Use subtraction }
  Result := Integer(Left) - Integer(Right);
end;

function Compare_U2(Inst: Pointer; const Left, Right: Word): Integer;
begin
  { Use subtraction }
  Result := Integer(Left) - Integer(Right);
end;

function Compare_U4(Inst: Pointer; const Left, Right: Cardinal): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

{$IFDEF EABI_RECORD_ARRAY_BYREF}
function Compare_Ref_U1(Inst: Pointer; const [Ref] Left, Right: Byte): Integer;
begin
  { Use subtraction }
  Result := Integer(Left) - Integer(Right);
end;

function Compare_Ref_U2(Inst: Pointer; const [Ref] Left, Right: Word): Integer;
begin
  { Use subtraction }
  Result := Integer(Left) - Integer(Right);
end;

function Compare_Ref_U4(Inst: Pointer; const [Ref] Left, Right: Cardinal): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;
{$ENDIF EABI_RECORD_ARRAY_BYREF}

const
  Comparer_Vtable_I1: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_I1
  );

  Comparer_Vtable_U1: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_U1
  );

  Comparer_Vtable_I2: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_I2
  );

  Comparer_Vtable_U2: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_U2
  );

  Comparer_Vtable_I4: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_I4
  );

  Comparer_Vtable_U4: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_U4
  );

  Comparer_Instance_I1: Pointer = @Comparer_Vtable_I1;
  Comparer_Instance_U1: Pointer = @Comparer_Vtable_U1;
  Comparer_Instance_I2: Pointer = @Comparer_Vtable_I2;
  Comparer_Instance_U2: Pointer = @Comparer_Vtable_U2;
  Comparer_Instance_I4: Pointer = @Comparer_Vtable_I4;
  Comparer_Instance_U4: Pointer = @Comparer_Vtable_U4;

{$IFDEF EABI_RECORD_ARRAY_BYREF}
  Comparer_Vtable_Ref_U1: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Ref_U1
  );

  Comparer_Vtable_Ref_U2: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Ref_U2
  );

  Comparer_Vtable_Ref_U4: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Ref_U4
  );

  Comparer_Instance_Ref_U1: Pointer = @Comparer_Vtable_Ref_U1;
  Comparer_Instance_Ref_U2: Pointer = @Comparer_Vtable_Ref_U2;
  Comparer_Instance_Ref_U4: Pointer = @Comparer_Vtable_Ref_U4;
{$ENDIF}

  EqualityComparer_Vtable_I1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_I1,
    @GetHashCode_I1
  );

  EqualityComparer_Vtable_I2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_I2,
    @GetHashCode_I2
  );

  EqualityComparer_Vtable_I4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_I4,
    @GetHashCode_I4
  );

  EqualityComparer_Instance_I1: Pointer = @EqualityComparer_VTable_I1;
  EqualityComparer_Instance_I2: Pointer = @EqualityComparer_VTable_I2;
  EqualityComparer_Instance_I4: Pointer = @EqualityComparer_VTable_I4;

{$IFDEF EABI_RECORD_ARRAY_BYREF}
  EqualityComparer_Vtable_Ref_I1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Ref_I1,
    @GetHashCode_Ref_I1
  );

  EqualityComparer_Vtable_Ref_I2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Ref_I2,
    @GetHashCode_Ref_I2
  );

  EqualityComparer_Vtable_Ref_I4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Ref_I4,
    @GetHashCode_Ref_I4
  );

  EqualityComparer_Instance_Ref_I1: Pointer = @EqualityComparer_VTable_Ref_I1;
  EqualityComparer_Instance_Ref_I2: Pointer = @EqualityComparer_VTable_Ref_I2;
  EqualityComparer_Instance_Ref_I4: Pointer = @EqualityComparer_VTable_Ref_I4;
{$ENDIF EABI_RECORD_ARRAY_BYREF}

function Comparer_Selector_Integer(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetTypeData(info)^.OrdType of
    otSByte: Result := @Comparer_Instance_I1;
    otUByte: Result := @Comparer_Instance_U1;
    otSWord: Result := @Comparer_Instance_I2;
    otUWord: Result := @Comparer_Instance_U2;
    otSLong: Result := @Comparer_Instance_I4;
    otULong: Result := @Comparer_Instance_U4;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

function EqualityComparer_Selector_Integer(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetTypeData(info)^.OrdType of
    otSByte, otUByte: Result := @EqualityComparer_Instance_I1;
    otSWord, otUWord: Result := @EqualityComparer_Instance_I2;
    otSLong, otULong: Result := @EqualityComparer_Instance_I4;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

// I8 & U8

function Equals_I8(Inst: Pointer; const Left, Right: Int64): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_I8(Inst: Pointer; const Value: Int64): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

function Compare_I8(Inst: Pointer; const Left, Right: Int64): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Compare_U8(Inst: Pointer; const Left, Right: UInt64): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

{$IF Defined(CPUX64) and defined(MSWINDOWS)}
function Equals_Ref_I8(Inst: Pointer; const Left, Right: PInt64): Boolean;
begin
  Result := Left^ = Right^;
end;

function Compare_Ref_U8_Swap(Inst: Pointer; const Left, Right: PUInt64): Integer;
asm
  .NOFRAME
  //Self: RCX Left: RDX, Right: R8
  mov RAX, [RDX] // Left
  mov RCX, [R8]  // Right
  bswap RAX
  bswap RCX
  sub  RAX,RCX
  setz CL
  sbb  EAX, EAX
  sbb  EAX, -1
  shr  EAX, CL
end;

function GetHashCode_Ref_I8(Inst: Pointer; const Value: PInt64): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value^, SizeOf(Value^));
end;
{$ENDIF}

const
  Comparer_Vtable_I8: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_I8
  );
  Comparer_Instance_I8: Pointer = @Comparer_Vtable_I8;

  Comparer_Vtable_U8: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_U8
  );
  Comparer_Instance_U8: Pointer = @Comparer_Vtable_U8;

{$IF defined(CPUX64) and defined(MSWINDOWS)}
  Comparer_Vtable_Ref_U8_Swap: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Ref_U8_Swap
  );
  Comparer_Instance_Ref_U8_Swap: Pointer = @Comparer_Vtable_Ref_U8_Swap;

  EqualityComparer_Vtable_Ref_I8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Ref_I8,
    @GetHashCode_Ref_I8
  );
  EqualityComparer_Instance_Ref_I8: Pointer = @EqualityComparer_Vtable_Ref_I8;

{$ENDIF}

  EqualityComparer_Vtable_I8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_I8,
    @GetHashCode_I8
  );
  EqualityComparer_Instance_I8: Pointer = @EqualityComparer_Vtable_I8;

function Comparer_Selector_Int64(info: PTypeInfo; size: Integer): Pointer;
begin
  if GetTypeData(info)^.MaxInt64Value > GetTypeData(info)^.MinInt64Value then
    Result := @Comparer_Instance_I8
  else
    Result := @Comparer_Instance_U8;
end;

// Float

function Compare_R4(Inst: Pointer; const Left, Right: Single): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_R4(Inst: Pointer; const Left, Right: Single): Boolean;
begin
  Result := Left = Right;
end;

const
{$IF (SizeOf(Extended) > 10) and (Defined(CPUX86) or Defined(CPUX64))}
  // Intel FPU case. GetHashValue needs actual Extended value bytes.
  BYTESIZEOFEXTENDED = 10;
{$ELSE}
  BYTESIZEOFEXTENDED = SizeOf(Extended);
{$ENDIF}

function GetHashCode_R4(Inst: Pointer; const Value: Single): Integer;
var
  m: Extended;
  e: Integer;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, m, e);
  if m = 0 then
    m := Abs(m);
  Result := THashFNV1a32.GetHashValue(m, BYTESIZEOFEXTENDED);
  Result := THashFNV1a32.GetHashValue(e, SizeOf(e), Result);
end;

const
  Comparer_Vtable_R4: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_R4
  );
  Comparer_Instance_R4: Pointer = @Comparer_Vtable_R4;

  EqualityComparer_Vtable_R4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_R4,
    @GetHashCode_R4
  );
  EqualityComparer_Instance_R4: Pointer = @EqualityComparer_Vtable_R4;

function Compare_R8(Inst: Pointer; const Left, Right: Double): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_R8(Inst: Pointer; const Left, Right: Double): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_R8(Inst: Pointer; const Value: Double): Integer;
var
  m: Extended;
  e: Integer;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, m, e);
  if m = 0 then
    m := Abs(m);
  Result := THashFNV1a32.GetHashValue(m, BYTESIZEOFEXTENDED);
  Result := THashFNV1a32.GetHashValue(e, SizeOf(e), Result);
end;

const
  Comparer_Vtable_R8: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_R8
  );
  Comparer_Instance_R8: Pointer = @Comparer_Vtable_R8;

  EqualityComparer_Vtable_R8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_R8,
    @GetHashCode_R8
  );
  EqualityComparer_Instance_R8: Pointer = @EqualityComparer_Vtable_R8;

function Compare_R10(Inst: Pointer; const Left, Right: Extended): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_R10(Inst: Pointer; const Left, Right: Extended): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_R10(Inst: Pointer; const Value: Extended): Integer;
var
  m: Extended;
  e: Integer;
begin
  // Denormalized floats and positive/negative 0.0 complicate things.
  Frexp(Value, m, e);
  if m = 0 then
    m := Abs(m);
  Result := THashFNV1a32.GetHashValue(m, BYTESIZEOFEXTENDED);
  Result := THashFNV1a32.GetHashValue(e, SizeOf(e), Result);
end;

const
  Comparer_Vtable_R10: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_R10
  );
  Comparer_Instance_R10: Pointer = @Comparer_Vtable_R10;

  EqualityComparer_Vtable_R10: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_R10,
    @GetHashCode_R10
  );
  EqualityComparer_Instance_R10: Pointer = @EqualityComparer_Vtable_R10;

function Compare_RI8(Inst: Pointer; const Left, Right: Comp): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_RI8(Inst: Pointer; const Left, Right: Comp): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_RI8(Inst: Pointer; const Value: Comp): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

const
  Comparer_Vtable_RI8: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_RI8
  );
  Comparer_Instance_RI8: Pointer = @Comparer_Vtable_RI8;

  EqualityComparer_Vtable_RI8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_RI8,
    @GetHashCode_RI8
  );
  EqualityComparer_Instance_RI8: Pointer = @EqualityComparer_Vtable_RI8;

function Compare_RC8(Inst: Pointer; const Left, Right: Currency): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_RC8(Inst: Pointer; const Left, Right: Currency): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_RC8(Inst: Pointer; const Value: Currency): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

const
  Comparer_Vtable_RC8: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_RC8
  );
  Comparer_Instance_RC8: Pointer = @Comparer_Vtable_RC8;

  EqualityComparer_Vtable_RC8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_RC8,
    @GetHashCode_RC8
  );
  EqualityComparer_Instance_RC8: Pointer = @EqualityComparer_Vtable_RC8;

function Comparer_Selector_Float(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetTypeData(info)^.FloatType of
    ftSingle: Result := @Comparer_Instance_R4;
    ftDouble: Result := @Comparer_Instance_R8;
    ftExtended: Result := @Comparer_Instance_R10;
    ftComp: Result := @Comparer_Instance_RI8;
    ftCurr: Result := @Comparer_Instance_RC8;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

function EqualityComparer_Selector_Float(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetTypeData(info)^.FloatType of
    ftSingle: Result := @EqualityComparer_Instance_R4;
    ftDouble: Result := @EqualityComparer_Instance_R8;
    ftExtended: Result := @EqualityComparer_Instance_R10;
    ftComp: Result := @EqualityComparer_Instance_RI8;
    ftCurr: Result := @EqualityComparer_Instance_RC8;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

// Binary

function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;
var
  pl, pr: PByte;
  len: Integer;
begin
  pl := Left;
  pr := Right;
  len := Size;
  while len > 0 do
  begin
    Result := pl^ - pr^;
    if Result <> 0 then
      Exit;
    Dec(len);
    Inc(pl);
    Inc(pr);
  end;
  Result := 0;
end;

function Compare_Binary(Inst: PSimpleInstance; const Left, Right): Integer;
begin
  Result := BinaryCompare(@Left, @Right, Inst^.Size);
end;

function Compare_Binary_ThreeByteData(Inst: PSimpleInstance; const Left, Right: TThreeByteData): Integer;
begin
  Result := Left[0] - Right[0];
  if Result <> 0 then exit;
  Result := Left[1] - Right[1];
  if Result <> 0 then exit;
  Result := Left[2] - Right[2];
end;

function Equals_Binary(Inst: PSimpleInstance; const Left, Right): Boolean;
begin
  Result := CompareMem(@Left, @Right, Inst^.Size);
end;

function GetHashCode_Binary(Inst: PSimpleInstance; const Value): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, Inst^.Size);
end;

function Equals_Binary_ThreeByteData(Inst: PSimpleInstance; const Left, Right: TThreeByteData): Boolean;
begin
  Result := CompareMem(@Left, @Right, 3);
end;

function GetHashCode_Binary_ThreeByteData(Inst: PSimpleInstance; const Value: TThreeByteData): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, 3);
end;

const
  Comparer_Vtable_Binary: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @Compare_Binary
  );

  Comparer_Vtable_Binary_ThreeByteData: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Binary_ThreeByteData
  );
  Comparer_Instance_ThreeByteData: Pointer = @Comparer_Vtable_Binary_ThreeByteData;


  EqualityComparer_Vtable_Binary: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @Equals_Binary,
    @GetHashCode_Binary
  );

  EqualityComparer_Vtable_Binary_ThreeByteData: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Binary_ThreeByteData,
    @GetHashCode_Binary_ThreeByteData
  );
  EqualityComparer_Instance_ThreeByteData: Pointer = @EqualityComparer_Vtable_Binary_ThreeByteData;

function Comparer_Selector_Binary(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
    // NOTE: Little-endianness may cause counterintuitive results,
    // but the results will at least be consistent.
{$IFDEF EABI_RECORD_ARRAY_BYREF}
    1:
      if (info <> nil) and ((info^.Kind = tkArray) or (info^.Kind = tkRecord)) then
        Result := @Comparer_Instance_Ref_U1
      else
        Result := @Comparer_Instance_U1;
    2:
      if (info <> nil) and ((info^.Kind = tkArray) or (info^.Kind = tkRecord)) then
        Result := @Comparer_Instance_Ref_U2
      else
        Result := @Comparer_Instance_U2;
    4:
      if (info <> nil) and ((info^.Kind = tkArray) or (info^.Kind = tkRecord)) then
        Result := @Comparer_Instance_Ref_U4
      else
        Result := @Comparer_Instance_U4;
{$ELSE}
    1: Result := @Comparer_Instance_U1;
    2: Result := @Comparer_Instance_U2;
    4: Result := @Comparer_Instance_U4;
{$ENDIF}
    3: // Array [0..2] of byte, or Record with SizeOf() = 3 special case
       Result := @Comparer_Instance_ThreeByteData;
    {$IF defined(CPUX64) and defined(MSWINDOWS)}
    // 64-bit will pass const args in registers
    8: Result := @Comparer_Instance_Ref_U8_Swap;
    {$ENDIF}
  else
    Result := MakeInstance(@Comparer_Vtable_Binary, info, size);
  end;
end;

function EqualityComparer_Selector_Binary(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
{$IFDEF EABI_RECORD_ARRAY_BYREF}
    1:
      if (info <> nil) and ((info^.Kind = tkArray) or (info^.Kind = tkRecord)) then
        Result := @EqualityComparer_Instance_Ref_I1
      else
        Result := @EqualityComparer_Instance_I1;
    2:
      if (info <> nil) and ((info^.Kind = tkArray) or (info^.Kind = tkRecord)) then
        Result := @EqualityComparer_Instance_Ref_I2
      else
        Result := @EqualityComparer_Instance_I2;
    4:
      if (info <> nil) and ((info^.Kind = tkArray) or (info^.Kind = tkRecord)) then
        Result := @EqualityComparer_Instance_Ref_I4
      else
        Result := @EqualityComparer_Instance_I4;
{$ELSE}
    1: Result := @EqualityComparer_Instance_I1;
    2: Result := @EqualityComparer_Instance_I2;
    4: Result := @EqualityComparer_Instance_I4;
{$ENDIF}
    3: // Array [0..2] of byte, or Record with SizeOf() = 3 special case
       Result := @EqualityComparer_Instance_ThreeByteData;
    {$IF defined(CPUX64) and defined(MSWINDOWS)}
    // 64-bit will pass const args in registers
    8: Result := @EqualityComparer_Instance_Ref_I8;
    {$ENDIF}
  else
    Result := MakeInstance(@EqualityComparer_Vtable_Binary, info, size);
  end;
end;

// Record

type
  TRecStack = record
    f: array[0 .. 3] of Byte;
    constructor Create(a: Byte);
  end;

constructor TRecStack.Create(a: Byte);
begin
end;

function Compare_Record_Stack(Inst: PSimpleInstance; const Left, Right: TRecStack): Integer;
begin
  Result := BinaryCompare(@Left, @Right, Inst^.Size);
  System.FinalizeRecord(@Left, Inst^.Info);
  System.FinalizeRecord(@Right, Inst^.Info);
end;

function Equals_Record_Stack(Inst: PSimpleInstance; const Left, Right: TRecStack): Boolean;
begin
  Result := CompareMem(@Left, @Right, Inst^.Size);
  System.FinalizeRecord(@Left, Inst^.Info);
  System.FinalizeRecord(@Right, Inst^.Info);
end;

function GetHashCode_Record_Stack(Inst: PSimpleInstance; const Value: TRecStack): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, Inst^.Size);
  System.FinalizeRecord(@Value, Inst^.Info);
end;

const
  Comparer_Vtable_Record_Stack: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @Compare_Record_Stack
  );

  EqualityComparer_Vtable_Record_Stack: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @Equals_Record_Stack,
    @GetHashCode_Record_Stack
  );

function Comparer_Selector_Record(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetConstRecordArgMode(info) of
  rmByValueStack:
    Result := MakeInstance(@Comparer_Vtable_Record_Stack, info, size);
  rmByValueReg:
    Result := Comparer_Selector_Binary(info, size);
  rmByRef:
    Result := MakeInstance(@Comparer_Vtable_Binary, info, size);
  else
    Result := nil;
    Assert(False);
  end;
end;

function EqualityComparer_Selector_Record(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetConstRecordArgMode(info) of
  rmByValueStack:
    Result := MakeInstance(@EqualityComparer_Vtable_Record_Stack, info, size);
  rmByValueReg:
    Result := EqualityComparer_Selector_Binary(info, size);
  rmByRef:
    Result := MakeInstance(@EqualityComparer_Vtable_Binary, info, size);
  else
    Result := nil;
    Assert(False);
  end;
end;

// Class (i.e. instances)

function Equals_Class(Inst: PSimpleInstance; const Left, Right: TObject): Boolean;
begin
  if Left <> nil then
    Result := Left.Equals(Right)
  else if Right <> nil then
    Result := Right.Equals(Left)
  else
    Result := True;
end;

function Compare_Class(Inst: PSimpleInstance; Left, Right: TObject): Integer;
begin
  if Equals_Class(Inst, Left, Right) then
    Result := 0
  else if NativeInt(Left) < NativeInt(Right) then
    Result := -1
  else if NativeInt(Left) > NativeInt(Right) then
    Result := 1
  else
    Result := 0;
end;

function GetHashCode_Class(Inst: PSimpleInstance; const Value: TObject): Integer;
begin
  if Value = nil then
    Result := 42
  else
    Result := Value.GetHashCode;
end;

// DynArray

function DynLen(Arr: Pointer): NativeInt; inline;
begin
  if Arr = nil then
    Exit(0);
  Result := PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(Inst: PSimpleInstance; Left, Right: Pointer): NativeInt;
var
  len, lenDiff: NativeInt;
begin
  len := DynLen(Left);
  lenDiff := len - DynLen(Right);
  if lenDiff > 0 then
    Dec(len, lenDiff);
  Result := BinaryCompare(Left, Right, Inst^.Size * len);
  if Result = 0 then
    Result := lenDiff;
end;

function Equals_DynArray(Inst: PSimpleInstance; Left, Right: Pointer): Boolean;
var
  lenL, lenR: NativeInt;
begin
  lenL := DynLen(Left);
  lenR := DynLen(Right);
  if lenL <> lenR then
    Exit(False);
  Result := CompareMem(Left, Right, Inst^.Size * lenL);
end;

function GetHashCode_DynArray(Inst: PSimpleInstance; Value: Pointer): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value^, Inst^.Size * DynLen(Value));
end;

const
  Comparer_Vtable_DynArray: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @Compare_DynArray
  );

  EqualityComparer_Vtable_DynArray: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @Equals_DynArray,
    @GetHashCode_DynArray
  );

function Comparer_Selector_DynArray(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := MakeInstance(@Comparer_Vtable_DynArray, info, GetTypeData(info)^.elSize);
end;

function EqualityComparer_Selector_DynArray(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := MakeInstance(@EqualityComparer_Vtable_DynArray, info, GetTypeData(info)^.elSize);
end;

// PStrings

type
{$IFNDEF NEXTGEN}
  TPS1 = string[1];
  TPS2 = string[2];
  TPS3 = string[3];
{$ELSE NEXTGEN}
  OpenString = type string;
  TPS1 = string;
  TPS2 = string;
  TPS3 = string;
{$ENDIF !NEXTGEN}

function Compare_PS1(Inst: PSimpleInstance; const Left, Right: TPS1): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Compare_PS2(Inst: PSimpleInstance; const Left, Right: TPS2): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Compare_PS3(Inst: PSimpleInstance; const Left, Right: TPS3): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Compare_PSn(Inst: PSimpleInstance; const Left, Right: OpenString): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_PS1(Inst: PSimpleInstance; const Left, Right: TPS1): Boolean;
begin
  Result := Left = Right;
end;

function Equals_PS2(Inst: PSimpleInstance; const Left, Right: TPS2): Boolean;
begin
  Result := Left = Right;
end;

function Equals_PS3(Inst: PSimpleInstance; const Left, Right: TPS3): Boolean;
begin
  Result := Left = Right;
end;

function Equals_PSn(Inst: PSimpleInstance; const Left, Right: OpenString): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_PS1(Inst: PSimpleInstance; const Value: TPS1): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[Low(string)], Length(Value));
end;

function GetHashCode_PS2(Inst: PSimpleInstance; const Value: TPS2): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[Low(string)], Length(Value));
end;

function GetHashCode_PS3(Inst: PSimpleInstance; const Value: TPS3): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[Low(string)], Length(Value));
end;

function GetHashCode_PSn(Inst: PSimpleInstance; const Value: OpenString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[Low(string)], Length(Value));
end;

const
  Comparer_Vtable_PS1: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_PS1
  );
  Comparer_Instance_PS1: Pointer = @Comparer_Vtable_PS1;

  Comparer_Vtable_PS2: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_PS2
  );
  Comparer_Instance_PS2: Pointer = @Comparer_Vtable_PS2;

  Comparer_Vtable_PS3: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_PS3
  );
  Comparer_Instance_PS3: Pointer = @Comparer_Vtable_PS3;

  Comparer_Vtable_PSn: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_PSn
  );
  Comparer_Instance_PSn: Pointer = @Comparer_Vtable_PSn;

  EqualityComparer_Vtable_PS1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_PS1,
    @GetHashCode_PS1
  );
  EqualityComparer_Instance_PS1: Pointer = @EqualityComparer_Vtable_PS1;

  EqualityComparer_Vtable_PS2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_PS2,
    @GetHashCode_PS2
  );
  EqualityComparer_Instance_PS2: Pointer = @EqualityComparer_Vtable_PS2;

  EqualityComparer_Vtable_PS3: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_PS3,
    @GetHashCode_PS3
  );
  EqualityComparer_Instance_PS3: Pointer = @EqualityComparer_Vtable_PS3;

  EqualityComparer_Vtable_PSn: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_PSn,
    @GetHashCode_PSn
  );
  EqualityComparer_Instance_PSn: Pointer = @EqualityComparer_Vtable_PSn;

function Comparer_Selector_String(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
    2: Result := @Comparer_Instance_PS1;
    3: Result := @Comparer_Instance_PS2;
    4: Result := @Comparer_Instance_PS3;
  else
    Result := @Comparer_Instance_PSn;
  end;
end;

function EqualityComparer_Selector_String(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
    2: Result := @EqualityComparer_Instance_PS1;
    3: Result := @EqualityComparer_Instance_PS2;
    4: Result := @EqualityComparer_Instance_PS3;
  else
    Result := @EqualityComparer_Instance_PSn;
  end;
end;

function CompareRawByteString(const Left, Right: RawByteString): Integer; inline;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PByte;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - PInteger(PByte(Right) - 4)^ // Length(Right)
  else if Pointer(Right) = nil then
    Result := PInteger(PByte(Left) - 4)^ // Length(Left)
  else
  begin
    Result := Integer(PByte(Left)^) - Integer(PByte(Right)^);
    if Result <> 0 then
      Exit;
    LLen := PInteger(PByte(Left) - 4)^ - 1;  // Length(Left);
    RLen := PInteger(PByte(Right) - 4)^ - 1; // Length(Right);
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PByte(Left) + 1;
    RPtr := PByte(Right) + 1;
    while Len > 0 do
    begin
      Result := Integer(LPtr^) - Integer(RPtr^);
      if Result <> 0 then
        Exit;
      if Len = 1 then break;
      Result := Integer(LPtr[1]) - Integer(RPtr[1]);
      if Result <> 0 then
        Exit;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len, 2);
    end;
    Result := LLen - RLen;
  end;
end;

function Compare_LString(Inst: PSimpleInstance; const Left, Right: RawByteString): Integer;
begin
  Result := CompareRawByteString(Left, Right);
end;

function Equals_LString(Inst: PSimpleInstance; const Left, Right: RawByteString): Boolean;
begin
  Result := CompareRawByteString(Left, Right) = 0;
end;

function GetHashCode_LString(Inst: PSimpleInstance; const Value: RawByteString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[1], Length(Value) * SizeOf(Value[1]));
end;

// UStrings

function Compare_UString(Inst: PSimpleInstance; const Left, Right: UnicodeString): Integer;
begin
  Result := CompareStr(Left, Right);
end;

function Equals_UString(Inst: PSimpleInstance; const Left, Right: UnicodeString): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_UString(Inst: PSimpleInstance; const Value: UnicodeString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[Low(string)], Length(Value) * SizeOf(Char));
end;

// Methods

type
  TMethodPointer = procedure of object;

function Compare_Method(Inst: PSimpleInstance; const Left, Right: TMethodPointer): Integer;
var
  LMethod, RMethod: TMethod;
begin
  LMethod := TMethod(Left);
  RMethod := TMethod(Right);
  if LMethod < RMethod then
    Result := -1
  else if LMethod > RMethod then
    Result := 1
  else
    Result := 0;
end;

function Equals_Method(Inst: PSimpleInstance; const Left, Right: TMethodPointer): Boolean;
begin
  Result := TMethod(Left) = TMethod(Right);
end;

function GetHashCode_Method(Inst: PSimpleInstance; const Value: TMethodPointer): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(TMethodPointer));
end;

// WStrings

{$IFNDEF NEXTGEN}
function Compare_WString(Inst: PSimpleInstance; const Left, Right: WideString): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_WString(Inst: PSimpleInstance; const Left, Right: WideString): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_WString(Inst: PSimpleInstance; const Value: WideString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value[1], Length(Value) * SizeOf(Value[1]));
end;
{$ENDIF !NEXTGEN}

// Variants

function Compare_Variant(Inst: PSimpleInstance; Left, Right: Pointer): Integer;
var
  l, r: Variant;
  lAsString, rAsString: string;
begin
  Result := 0; // Avoid warning.
  l := PVariant(Left)^;
  r := PVariant(Right)^;
  try
    case VarCompareValue(l, r) of
      vrEqual:        Exit(0);
      vrLessThan:     Exit(-1);
      vrGreaterThan:  Exit(1);
      vrNotEqual:
      begin
        if VarIsEmpty(L) or VarIsNull(L) then
          Exit(1)
        else
          Exit(-1);
      end;
    end;
  except // if comparison failed with exception, compare as string.
    try
      lAsString := PVariant(Left)^;
      rAsString := PVariant(Right)^;
      Result := Compare_UString(nil, lAsString, rAsString);
    except  // if comparison fails again, compare bytes.
      Result := BinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
end;

function Equals_Variant(Inst: PSimpleInstance; Left, Right: Pointer): Boolean;
var
  l, r: Variant;
begin
  l := PVariant(Left)^;
  r := PVariant(Right)^;
  Result := VarCompareValue(l, r) = vrEqual;
end;

function GetHashCode_Variant(Inst: PSimpleInstance; Value: Pointer): Integer;
var
  v: string;
begin
  try
    v := PVariant(Value)^;
    Result := GetHashCode_UString(nil, v);
  except
    Result := THashFNV1a32.GetHashValue(Value^, SizeOf(Variant));
  end;
end;

// Pointers

function Compare_Pointer(Inst: PSimpleInstance; Left, Right: NativeUInt): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

function Equals_Pointer(Inst: Pointer; const Left, Right: NativeUInt): Boolean;
begin
  Result := Left = Right;
end;

function GetHashCode_Pointer(Inst: Pointer; const Value: NativeUInt): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value, SizeOf(Value));
end;

{$IFDEF NEXTGEN}
const
  Compare_WString: Pointer = nil;
  Equals_WString: Pointer = nil;
  GetHashCode_WString: Pointer = nil;
{$ENDIF !NEXTGEN}

const
  Comparer_Vtable_Pointer: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Pointer
  );

  Comparer_Vtable_Class: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Class
  );

  Comparer_Vtable_LString: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_LString
  );

  Comparer_Vtable_WString: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_WString
  );

  Comparer_Vtable_Variant: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Variant
  );

  Comparer_Vtable_UString: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_UString
  );

  Comparer_Vtable_Method: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Method
  );

  Comparer_Instance_Pointer: Pointer = @Comparer_Vtable_Pointer;
  Comparer_Instance_Class: Pointer = @Comparer_Vtable_Class;
  Comparer_Instance_LString: Pointer = @Comparer_Vtable_LString;
  Comparer_Instance_WString: Pointer = @Comparer_Vtable_WString;
  Comparer_Instance_Variant: Pointer = @Comparer_Vtable_Variant;
  Comparer_Instance_UString: Pointer = @Comparer_Vtable_UString;
  Comparer_Instance_Method: Pointer = @Comparer_Vtable_Method;

  EqualityComparer_Vtable_Pointer: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Pointer,
    @GetHashCode_Pointer
  );

  EqualityComparer_Vtable_Class: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Class,
    @GetHashCode_Class
  );

  EqualityComparer_Vtable_LString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_LString,
    @GetHashCode_LString
  );

  EqualityComparer_Vtable_WString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_WString,
    @GetHashCode_WString
  );

  EqualityComparer_Vtable_Variant: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Variant,
    @GetHashCode_Variant
  );

  EqualityComparer_Vtable_UString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_UString,
    @GetHashCode_UString
  );

  EqualityComparer_Vtable_Method: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Equals_Method,
    @GetHashCode_Method
  );

  EqualityComparer_Instance_Pointer: Pointer = @EqualityComparer_Vtable_Pointer;
  EqualityComparer_Instance_Class: Pointer = @EqualityComparer_Vtable_Class;
  EqualityComparer_Instance_LString: Pointer = @EqualityComparer_Vtable_LString;
  EqualityComparer_Instance_WString: Pointer = @EqualityComparer_Vtable_WString;
  EqualityComparer_Instance_Variant: Pointer = @EqualityComparer_Vtable_Variant;
  EqualityComparer_Instance_UString: Pointer = @EqualityComparer_Vtable_UString;
  EqualityComparer_Instance_Method: Pointer = @EqualityComparer_Vtable_Method;

  VtableInfo: array[TDefaultGenericInterface, TTypeKind] of TVtableInfo =
  (
    // IComparer
    (
      // tkUnknown
      (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
      // tkInteger
      (Flags: [ifSelector]; Data: @Comparer_Selector_Integer),
      // tkChar
      (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
      // tkEnumeration
      (Flags: [ifSelector]; Data: @Comparer_Selector_Integer),
      // tkFloat
      (Flags: [ifSelector]; Data: @Comparer_Selector_Float),
      // tkString
      (Flags: [ifSelector]; Data: @Comparer_Selector_String),
      // tkSet
      (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
      // tkClass
      (Flags: []; Data: @Comparer_Instance_Class),
      // tkMethod
      (Flags: []; Data: @Comparer_Instance_Method),
      // tkWChar
      (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
      // tkLString
      (Flags: []; Data: @Comparer_Instance_LString),
      // tkWString
      (Flags: []; Data: @Comparer_Instance_WString),
      // tkVariant
      (Flags: []; Data: @Comparer_Instance_Variant),
      // tkArray
      (Flags: [ifSelector]; Data: @Comparer_Selector_Binary),
      // tkRecord
      (Flags: [ifSelector]; Data: @Comparer_Selector_Record),
      // tkInterface
      (Flags: []; Data: @Comparer_Instance_Pointer),
      // tkInt64
      (Flags: [ifSelector]; Data: @Comparer_Selector_Int64),
      // tkDynArray
      (Flags: [ifSelector]; Data: @Comparer_Selector_DynArray),
      // tkUString
      (Flags: []; Data: @Comparer_Instance_UString),
      // tkClassRef
      (Flags: []; Data: @Comparer_Instance_Pointer),
      // tkPointer
      (Flags: []; Data: @Comparer_Instance_Pointer),
      // tkProcedure
      (Flags: []; Data: @Comparer_Instance_Pointer),
      // tkMRecord
      (Flags: [ifSelector]; Data: @Comparer_Selector_Record)
    ),
    // IEqualityComparer
    (
      // tkUnknown
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
      // tkInteger
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Integer),
      // tkChar
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
      // tkEnumeration
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Integer),
      // tkFloat
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Float),
      // tkString
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_String),
      // tkSet
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
      // tkClass
      (Flags: []; Data: @EqualityComparer_Instance_Class),
      // tkMethod
      (Flags: []; Data: @EqualityComparer_Instance_Method),
      // tkWChar
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
      // tkLString
      (Flags: []; Data: @EqualityComparer_Instance_LString),
      // tkWString
      (Flags: []; Data: @EqualityComparer_Instance_WString),
      // tkVariant
      (Flags: []; Data: @EqualityComparer_Instance_Variant),
      // tkArray
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Binary),
      // tkRecord
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Record),
      // tkInterface
      (Flags: []; Data: @EqualityComparer_Instance_Pointer),
      // tkInt64
      (Flags: []; Data: @EqualityComparer_Instance_I8),
      // tkDynArray
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_DynArray),
      // tkUString
      (Flags: []; Data: @EqualityComparer_Instance_UString),
      // tkClassRef
      (Flags: []; Data: @EqualityComparer_Instance_Pointer),
      // tkPointer
      (Flags: []; Data: @EqualityComparer_Instance_Pointer),
      // tkProcedure
      (Flags: []; Data: @EqualityComparer_Instance_Pointer),
      // tkMRecord
      (Flags: [ifSelector]; Data: @EqualityComparer_Selector_Record)
    )
  );

function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo; size: Integer): Pointer;
var
  pinfo: PVtableInfo;
begin
  if info <> nil then
  begin
    pinfo := @VtableInfo[intf, info^.Kind];
    Result := pinfo^.Data;
    if ifSelector in pinfo^.Flags then
      Result := TTypeInfoSelector(Result)(info, size);
    if ifVariableSize in pinfo^.Flags then
      Result := MakeInstance(Result, info, size);
  end
  else
  begin
    case intf of
      giComparer: Result := Comparer_Selector_Binary(info, size);
      giEqualityComparer: Result := EqualityComparer_Selector_Binary(info, size);
    else
      System.Error(reRangeError);
      Result := nil;
    end;
  end;
end;

{ Delegated Comparers }

constructor TDelegatedEqualityComparer<T>.Create(const AEquals: TEqualityComparison<T>; const AGetHashCode: THasher<T>);
begin
  FEquals := AEquals;
  FGetHashCode := AGetHashCode;
end;

function TDelegatedEqualityComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  Result := FEquals(Left, Right);
end;

function TDelegatedEqualityComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result := FGetHashCode(Value);
end;

constructor TDelegatedComparer<T>.Create(const ACompare: TComparison<T>);
begin
  FCompare := ACompare;
end;

function TDelegatedComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := FCompare(Left, Right);
end;

{ TOrdinalStringComparer }

type
  TOrdinalStringComparer = class(TStringComparer)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean;
      reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer;
      reintroduce; overload; override;
  end;

function TOrdinalStringComparer.Compare(const Left, Right: string): Integer;
var
  len, lenDiff: Integer;
begin
  len := Length(Left);
  lenDiff := len - Length(Right);
  if Length(Right) < len then
    len := Length(Right);
  Result := BinaryCompare(PChar(Left), PChar(Right), len * SizeOf(Char));
  if Result = 0 then
    Exit(lenDiff);
end;

function TOrdinalStringComparer.Equals(const Left, Right: string): Boolean;
var
  len: Integer;
begin
  len := Length(Left);

  Result := (len - Length(Right) = 0) and CompareMem(PChar(Left), PChar(Right), len * SizeOf(Char));
end;

function TOrdinalStringComparer.GetHashCode(const Value: string): Integer;
begin
  Result := THashFNV1a32.GetHashValue(PChar(Value)^, SizeOf(Char) * Length(Value));
end;

{ TStringComparer }

class destructor TStringComparer.Destroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TStringComparer.Ordinal: TStringComparer;
begin
  if FOrdinal = nil then
    FOrdinal := TOrdinalStringComparer.Create;
  Result := TStringComparer(FOrdinal);
end;

{ TOrdinalIStringComparer }

function TOrdinalIStringComparer.Compare(const Left, Right: string): Integer;
var
  L, R: string;
  len, lenDiff: Integer;
begin
  L := AnsiLowerCase(Left);
  R := AnsiLowerCase(Right);
  len := Length(L);
  lenDiff := len - Length(R);
  if Length(R) < len then
    len := Length(R);
  Result := BinaryCompare(PChar(L), PChar(R), len * SizeOf(Char));
  if Result = 0 then
    Exit(lenDiff);
end;

function TOrdinalIStringComparer.Equals(const Left, Right: string): Boolean;
var
  len: Integer;
  L, R: string;
begin
  L := AnsiLowerCase(Left);
  R := AnsiLowerCase(Right);
  len := Length(L);
  Result := (len - Length(R) = 0) and CompareMem(PChar(L), PChar(R), len * SizeOf(Char));
end;

function TOrdinalIStringComparer.GetHashCode(const Value: string): Integer;
var
  S: string;
begin
  S := AnsiLowerCase(Value);
  Result := THashFNV1a32.GetHashValue(PChar(S)^, SizeOf(Char) * Length(S));
end;

{ TIStringComparer }

class destructor TIStringComparer.Destroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TIStringComparer.Ordinal: TStringComparer;
begin
  if FOrdinal = nil then
    FOrdinal := TOrdinalIStringComparer.Create;
  Result := TStringComparer(FOrdinal);
end;

{ TComparer<T> }

class function TComparer<T>._Default: Pointer;
begin
  Result := _LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T));
end;

class function TComparer<T>.Default: IComparer<T>;
begin
  Result := IComparer<T>(_Default);
end;

type
  TAnonymousInterfaceInstance = record
    Vtable: Pointer; // points to meths
    RefCount: Integer;
    MethCount: Integer;
    // intfs: array [0 .. MethCount - 1] of IInterface
    // meths: array [0 .. 3 + MethCount - 1] of Pointer
  end;
  PAnonymousInterfaceInstance = ^TAnonymousInterfaceInstance;

  TAnonymousMethodVTable = array [0 .. 3] of Pointer;
  PAnonymousMethodVTable = ^TAnonymousMethodVTable;
  TAnonymousMethodInstance = record
    Vtable: PAnonymousMethodVTable;
    RefCount: Integer;
  end;
  PAnonymousMethodInstance = ^TAnonymousMethodInstance;

  PIInterface = ^IInterface;

function AnonAddref(inst: PAnonymousInterfaceInstance): Integer; stdcall;
begin
  Result := AtomicIncrement(inst^.RefCount);
end;

function AnonRelease(inst: PAnonymousInterfaceInstance): Integer; stdcall;
var
  pIntf: PIInterface;
  I: Integer;
begin
  Result := AtomicDecrement(inst^.RefCount);
  if Result = 0 then
  begin
    pIntf := PIInterface(NativeUInt(inst) + SizeOf(TAnonymousInterfaceInstance));
    for I := 0 to inst^.MethCount - 1 do
    begin
      pIntf^ := nil;
      Inc(pIntf);
    end;
    FreeMem(inst);
  end;
end;

function _MakeInterfaceInstance(const AnonMethodPtrs: array of const): Pointer;
var
  inst: PAnonymousInterfaceInstance;
  pIntf: PIInterface;
  pMeth: PPointer;
  I: Integer;
begin
  GetMem(inst, SizeOf(TAnonymousInterfaceInstance) +
    Length(AnonMethodPtrs) * SizeOf(IInterface) +
    (3 + Length(AnonMethodPtrs)) * SizeOf(Pointer));
  inst^.RefCount := 0;
  inst^.MethCount := Length(AnonMethodPtrs);

  pIntf := PIInterface(NativeUInt(inst) + SizeOf(TAnonymousInterfaceInstance));
  for I := Low(AnonMethodPtrs) to High(AnonMethodPtrs) do
  begin
    Pointer(pIntf^) := nil;
    pIntf^ := PIInterface(AnonMethodPtrs[I].VPointer)^;
    Inc(pIntf);
  end;

  pMeth := PPointer(pIntf);
  inst^.Vtable := pMeth;
  pMeth^ := @NopQueryInterface;
  Inc(pMeth);
  pMeth^ := @AnonAddref;
  Inc(pMeth);
  pMeth^ := @AnonRelease;
  Inc(pMeth);
  for I := Low(AnonMethodPtrs) to High(AnonMethodPtrs) do
  begin
    if PIInterface(AnonMethodPtrs[I].VPointer)^ <> nil then
      pMeth^ := PAnonymousMethodInstance(PIInterface(AnonMethodPtrs[I].VPointer)^)^.Vtable^[3]
    else
      pMeth^ := nil;
    Inc(pMeth);
  end;

  Result := inst;
end;

function _AreAnonMethodsCapturing(const AnonMethodPtrs: array of const): Boolean;
var
  obj: TObject;
  I: Integer;
begin
  for I := Low(AnonMethodPtrs) to High(AnonMethodPtrs) do
    if AnonMethodPtrs[I].VPointer <> nil then
    begin
      obj := TObject(PIInterface(AnonMethodPtrs[I].VPointer)^);
      if (obj <> nil) and
         (obj.InstanceSize >
          TInterfacedObject.InstanceSize + obj.GetInterfaceTable.EntryCount * SizeOf(Pointer)) then
        Exit(True);
    end;
  Result := False;
end;

class function TComparer<T>._Construct(const Comparison: TComparison<T>): Pointer;
begin
  Result := PPointer(@Comparison)^;
end;

class function TComparer<T>.Construct(const Comparison: TComparison<T>): IComparer<T>;
begin
  Result := IComparer<T>(_Construct(Comparison));
end;

{ TEqualityComparer<T> }

class function TEqualityComparer<T>._Default: Pointer;
begin
  Result := _LookupVtableInfo(giEqualityComparer, TypeInfo(T), SizeOf(T));
end;

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
  Result := IEqualityComparer<T>(_Default);
end;

class function TEqualityComparer<T>.Construct(
  const EqualityComparison: TEqualityComparison<T>;
  const Hasher: THasher<T>): IEqualityComparer<T>;
begin
  if _AreAnonMethodsCapturing([@EqualityComparison, @Hasher]) then
    Result := TDelegatedEqualityComparer<T>.Create(EqualityComparison, Hasher)
  else
    Result := IEqualityComparer<T>(_MakeInterfaceInstance([@EqualityComparison, @Hasher]));
end;

{ BobJenkinsHash }

function BobJenkinsHash(const Data; Len, InitData: Integer): Integer;
begin
  Result := THashBobJenkins.GetHashValue(Data, Len, InitData);
end;

end.

