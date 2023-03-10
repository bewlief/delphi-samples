{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit System; { Predefined constants, types, procedures, }
             { and functions (such as True, Integer, or }
             { Writeln) do not have actual declarations.}
             { Instead they are built into the compiler }
             { and are treated as if they were declared }
             { at the beginning of the System unit.     }

{$H+,I-,R-,O+,W-}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN UNSAFE_TYPE OFF}


{ L- should never be specified.

  The IDE needs to find DebugHook (through the C++
  compiler sometimes) for integrated debugging to
  function properly.

  ILINK will generate debug info for DebugHook if
  the object module has not been compiled with debug info.

  ILINK will not generate debug info for DebugHook if
  the object module has been compiled with debug info.

  Thus, the Pascal compiler must be responsible for
  generating the debug information for that symbol
  when a debug-enabled object file is produced.
}

interface

{$IFDEF CPUX86}
  {$IFNDEF PC_MAPPED_EXCEPTIONS}
    {$DEFINE STACK_BASED_EXCEPTIONS}
  {$ENDIF !PC_MAPPED_EXCEPTIONS}
{$ELSE !CPUX86}
  {$DEFINE PUREPASCAL}
  {$IFDEF WIN64}
    {$DEFINE TABLE_BASED_EXCEPTIONS}
  {$ENDIF WIN64}
{$ENDIF !CPUX86}

(* You can use RTLVersion in $IF expressions to test the runtime library
  version level independently of the compiler version level.
  Example:  {$IF RTLVersion >= 16.2} ... {$IFEND}                  *)

const
  RTLVersion = 23.00;

{$EXTERNALSYM CompilerVersion}

(*
const
  CompilerVersion = 0.0;

  CompilerVersion is assigned a value by the compiler when
  the system unit is compiled.  It indicates the revision level of the
  compiler features / language syntax, which may advance independently of
  the RTLVersion.  CompilerVersion can be tested in $IF expressions and
  should be used instead of testing for the VERxxx conditional define.
  Always test for greater than or less than a known revision level.
  It's a bad idea to test for a specific revision level.
*)

{$IFDEF DECLARE_GPL}
(* The existence of the GPL symbol indicates that the System unit
  and the rest of the Delphi runtime library were compiled for use
  and distribution under the terms of the GNU General Public License (GPL).
  Under the terms of the GPL, all applications compiled with the
  GPL version of the Delphi runtime library must also be distributed
  under the terms of the GPL.
  For more information about the GNU GPL, see
  http://www.gnu.org/copyleft/gpl.html

  The GPL symbol does not exist in the Delphi runtime library
  purchased for commercial/proprietary software development.

  If your source code needs to know which licensing model it is being
  compiled into, you can use {$IF DECLARED(GPL)}...{$IFEND} to
  test for the existence of the GPL symbol.  The value of the
  symbol itself is not significant.   *)

const
  GPL = True;
{$ENDIF}

{ Delphi built-in types for .hpp/.obj support }
{   Most of built-in types are defined in sysmac.h }
{   Pointer types should be mangled by the compiler for constness}
{$EXTERNALSYM Boolean     'bool'             } {$OBJTYPENAME Boolean    'Bo'}
{$NODEFINE    ShortInt    'Int8'             } {$OBJTYPENAME ShortInt   'Bzc'} { signed char }
{-EXTERNALSYM ShortInt    'signed char'      } {-OBJTYPENAME ShortInt   'Bzc'}
{$EXTERNALSYM SmallInt    'short'            } {$OBJTYPENAME SmallInt   'Bs'}
{$EXTERNALSYM Integer     'int'              } {$OBJTYPENAME Integer    'Bi'}
{$NODEFINE    Byte        'Byte'             } {$OBJTYPENAME Byte       'Buc'} { unsigned char }
{$NODEFINE    Word        'Word'             } {$OBJTYPENAME Word       'Bus'} { unsigned short }
{$EXTERNALSYM Cardinal    'unsigned'         } {$OBJTYPENAME Cardinal   'Bui'}
{$EXTERNALSYM Int64       '__int64'          } {$OBJTYPENAME Int64      'Bj'}
{$EXTERNALSYM UInt64      'unsigned __int64' } {$OBJTYPENAME UInt64     'Buj'}
{$EXTERNALSYM NativeInt   'int'              } {$OBJTYPENAME NativeInt  'Bi'}
{$EXTERNALSYM NativeUInt  'unsigned'         } {$OBJTYPENAME NativeUInt 'Bui'}
{$EXTERNALSYM Single      'float'            } {$OBJTYPENAME Single     'Bf'}
{$EXTERNALSYM Double      'double'           } {$OBJTYPENAME Double     'Bd'}
{$NODEFINE    Extended    'Extended'         } {$OBJTYPENAME Extended   'Bg'} { long double }
{$NODEFINE    Currency    'Currency'    'CurrencyBase'    } {$OBJTYPENAME Currency    'NCurrency'}
{$NODEFINE    Comp        'Comp'        'CompBase'        } {$OBJTYPENAME Comp        'NComp'}
{$EXTERNALSYM Real        'double'                        } {$OBJTYPENAME Real        'Bd'}
{$NODEFINE    ShortString 'ShortString' 'ShortStringBase' } {$OBJTYPENAME ShortString 'N%SmallString$iuc$255%'}
{$NODEFINE    OpenString  'OpenString'       } {$OBJTYPENAME OpenString 'Bxpc'} { char * const }
{$NODEFINE    File        'file'             } {$OBJTYPENAME File       'Nfile'}
{$NODEFINE    Text        'TextFile'         } {$OBJTYPENAME Text       'NTextfile'}
{$NODEFINE    ByteBool    'ByteBool'         } {$OBJTYPENAME ByteBool   'Buc'} { unsigned char }
{$NODEFINE    WordBool    'WordBool'         } {$OBJTYPENAME WordBool   'Bus'} { unsigned short }
{$EXTERNALSYM LongBool    'BOOL'             } {$OBJTYPENAME LongBool   'Bi'}  { int } { from windef.h }
{$NODEFINE    Real48      } { not supported in C++ }
{$NODEFINE    Extended80  } { not supported in C++ }
{$EXTERNALSYM Pointer     'void *'    }
{$NODEFINE    PWideChar   'WideChar *'}
{$EXTERNALSYM PAnsiChar   'char *'    }
{$NODEFINE    Variant     } { defined in sysvari.h }
{$NODEFINE    OleVariant  } { defined in sysvari.h }
{$NODEFINE    LongInt     } { alias of Integer     }
{$NODEFINE    LongWord    } { alias of Cardinal    }
{$NODEFINE    TextFile    } { alias of Text        }
{$EXTERNALSYM AnsiChar     'char'          } {$OBJTYPENAME AnsiChar 'Bc'}
{$IFDEF MSWINDOWS}
  {$NODEFINE  Char         'WideChar'      } {$OBJTYPENAME Char     'Bb'}  { wchar_t }
{$ELSE}
  {$NODEFINE  Char         'WideChar'      } {$OBJTYPENAME Char     'BCs'} { char16_t }
{$ENDIF}
{$NODEFINE    string       'UnicodeString' } {$OBJTYPENAME string   'NUnicodeString'} { defined in vcl/ustring.h }
{-NODEFINE    string       'String'        } {$OBJTYPENAME string   'NUnicodeString'} { defined in vcl/ustring.h }
{$NODEFINE    AnsiString   } { defined in vcl/dstring.h }
{$NODEFINE    WideString   } { defined in vcl/wstring.h }
{$NODEFINE    PChar        } { alias of PWideChar  }
{$NODEFINE    WideChar     } { alias of Char       }
{$NODEFINE    UnicodeString} { alias of string     }

(*$HPPEMIT 'namespace System' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '  // Shortint is a source of confusion in C++' *)
(*$HPPEMIT '  // typedef Shortint ShortInt;' *)
(*$HPPEMIT '  typedef Smallint SmallInt;' *)
(*$HPPEMIT '  typedef Longint LongInt;' *)
(*$HPPEMIT '}' *)

type
  CppLongInt  = type LongInt;  {$EXTERNALSYM CppLongInt  'long'         } {$OBJTYPENAME CppLongInt  'Bl'}
  CppULongInt = type LongWord; {$EXTERNALSYM CppULongInt 'unsigned long'} {$OBJTYPENAME CppULongInt 'Bul'}

{ Useful alias types }
type
  {$NODEFINE Int8} // We map 'Shortint' to 'Int8' for C++ above
  Int8    = ShortInt;
  Int16   = SmallInt;
  Int32   = Integer;
  IntPtr  = NativeInt;
  UInt8   = Byte;
  UInt16  = Word;
  UInt32  = Cardinal;
  UIntPtr = NativeUInt;

const
{ Variant type codes (wtypes.h) }

  varEmpty    = $0000; { vt_empty        0 }
  varNull     = $0001; { vt_null         1 }
  varSmallint = $0002; { vt_i2           2 }
  varInteger  = $0003; { vt_i4           3 }
  varSingle   = $0004; { vt_r4           4 }
  varDouble   = $0005; { vt_r8           5 }
  varCurrency = $0006; { vt_cy           6 }
  varDate     = $0007; { vt_date         7 }
  varOleStr   = $0008; { vt_bstr         8 }
  varDispatch = $0009; { vt_dispatch     9 }
  varError    = $000A; { vt_error       10 }
  varBoolean  = $000B; { vt_bool        11 }
  varVariant  = $000C; { vt_variant     12 }
  varUnknown  = $000D; { vt_unknown     13 }
//varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
//varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
  varShortInt = $0010; { vt_i1          16 }
  varByte     = $0011; { vt_ui1         17 }
  varWord     = $0012; { vt_ui2         18 }
  varLongWord = $0013; { vt_ui4         19 }
  varInt64    = $0014; { vt_i8          20 }
  varUInt64   = $0015; { vt_ui8         21 }
  varRecord   = $0024; { VT_RECORD      36 }
{  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

  varStrArg   = $0048; { vt_clsid        72 }
  varObject   = $0049; {                 73 }
  varUStrArg  = $004A; {                 74 }
  varString   = $0100; { Pascal string  256 } {not OLE compatible }
  varAny      = $0101; { Corba any      257 } {not OLE compatible }
  varUString  = $0102; { Unicode string 258 } {not OLE compatible }
  // custom types range from $110 (272) to $7FF (2047)

  varTypeMask = $0FFF;
  varArray    = $2000;
  varByRef    = $4000;

{ TVarRec.VType values }

  vtInteger       = 0;
  vtBoolean       = 1;
  vtChar          = 2;
  vtExtended      = 3;
  vtString        = 4;
  vtPointer       = 5;
  vtPChar         = 6;
  vtObject        = 7;
  vtClass         = 8;
  vtWideChar      = 9;
  vtPWideChar     = 10;
  vtAnsiString    = 11;
  vtCurrency      = 12;
  vtVariant       = 13;
  vtInterface     = 14;
  vtWideString    = 15;
  vtInt64         = 16;
  vtUnicodeString = 17;

{ Virtual method table entries }
{$IF defined(CPUX64)}
  vmtSelfPtr           = -176;
  vmtIntfTable         = -168;
  vmtAutoTable         = -160;
  vmtInitTable         = -152;
  vmtTypeInfo          = -144;
  vmtFieldTable        = -136;
  vmtMethodTable       = -128;
  vmtDynamicTable      = -120;
  vmtClassName         = -112;
  vmtInstanceSize      = -104;
  vmtParent            = -96;
  vmtEquals            = -88 deprecated 'Use VMTOFFSET in asm code';
  vmtGetHashCode       = -80 deprecated 'Use VMTOFFSET in asm code';
  vmtToString          = -72 deprecated 'Use VMTOFFSET in asm code';
  vmtSafeCallException = -64 deprecated 'Use VMTOFFSET in asm code';
  vmtAfterConstruction = -56 deprecated 'Use VMTOFFSET in asm code';
  vmtBeforeDestruction = -48 deprecated 'Use VMTOFFSET in asm code';
  vmtDispatch          = -40 deprecated 'Use VMTOFFSET in asm code';
  vmtDefaultHandler    = -32 deprecated 'Use VMTOFFSET in asm code';
  vmtNewInstance       = -24 deprecated 'Use VMTOFFSET in asm code';
  vmtFreeInstance      = -16 deprecated 'Use VMTOFFSET in asm code';
  vmtDestroy           =  -8 deprecated 'Use VMTOFFSET in asm code';

  vmtQueryInterface    =  0 deprecated 'Use VMTOFFSET in asm code';
  vmtAddRef            =  8 deprecated 'Use VMTOFFSET in asm code';
  vmtRelease           = 16 deprecated 'Use VMTOFFSET in asm code';
  vmtCreateObject      = 24 deprecated 'Use VMTOFFSET in asm code';
{$ELSE !CPUX64}
  vmtSelfPtr           = -88;
  vmtIntfTable         = -84;
  vmtAutoTable         = -80;
  vmtInitTable         = -76;
  vmtTypeInfo          = -72;
  vmtFieldTable        = -68;
  vmtMethodTable       = -64;
  vmtDynamicTable      = -60;
  vmtClassName         = -56;
  vmtInstanceSize      = -52;
  vmtParent            = -48;
  vmtEquals            = -44 deprecated 'Use VMTOFFSET in asm code';
  vmtGetHashCode       = -40 deprecated 'Use VMTOFFSET in asm code';
  vmtToString          = -36 deprecated 'Use VMTOFFSET in asm code';
  vmtSafeCallException = -32 deprecated 'Use VMTOFFSET in asm code';
  vmtAfterConstruction = -28 deprecated 'Use VMTOFFSET in asm code';
  vmtBeforeDestruction = -24 deprecated 'Use VMTOFFSET in asm code';
  vmtDispatch          = -20 deprecated 'Use VMTOFFSET in asm code';
  vmtDefaultHandler    = -16 deprecated 'Use VMTOFFSET in asm code';
  vmtNewInstance       = -12 deprecated 'Use VMTOFFSET in asm code';
  vmtFreeInstance      = -8 deprecated 'Use VMTOFFSET in asm code';
  vmtDestroy           = -4 deprecated 'Use VMTOFFSET in asm code';

  vmtQueryInterface    = 0 deprecated 'Use VMTOFFSET in asm code';
  vmtAddRef            = 4 deprecated 'Use VMTOFFSET in asm code';
  vmtRelease           = 8 deprecated 'Use VMTOFFSET in asm code';
  vmtCreateObject      = 12 deprecated 'Use VMTOFFSET in asm code';
{$IFEND !CPUX64}

  { Hidden TObject field info }
  hfFieldSize          = SizeOf(Pointer);
  hfMonitorOffset      = 0;

{ RTTI Visibility }
type
  TVisibilityClasses = set of (vcPrivate, vcProtected, vcPublic, vcPublished);

const
  { These constants represent the default settings built into the compiler.
    For classes, these settings are normally inherited from TObject. }
  DefaultMethodRttiVisibility = [vcPublic, vcPublished];
  DefaultFieldRttiVisibility = [vcPrivate..vcPublished];
  DefaultPropertyRttiVisibility = [vcPublic, vcPublished];

type
  { Default RTTI settings }
  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

  { Minimal RTTI generation henceforth in this file }
  {.$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}

  TArray<T> = array of T;

  TObject = class;
  {$NODEFINE TObject}   { defined in systobj.h }

  TClass = class of TObject;
  {$NODEFINE TClass}    { defined in systobj.h }

  HRESULT = type Longint;  { from wtypes.h }
  {$EXTERNALSYM HRESULT} {$OBJTYPENAME HRESULT 'Bl'} { long }

  PGUID = ^TGUID;
  TGUID = packed record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
    class operator Equal(const Left, Right: TGUID): Boolean;
    class operator NotEqual(const Left, Right: TGUID): Boolean;
    class function Empty: TGUID; static;
  end;
  {$NODEFINE PGUID}             { defined in sysmac.h }
  {$EXTERNALSYM TGUID 'GUID' }  { defined in sysmac.h }
  {$OBJTYPENAME TGUID 'N_GUID'}
  { Type 'GUID' in C++ is alias of '_GUID' and defined in guiddef.h (wtypes.h) }

  PInterfaceEntry = ^TInterfaceEntry;
  TInterfaceEntry = packed record
    IID: TGUID;
    VTable: Pointer;
    IOffset: Integer;
    {$IF defined(CPUX64)}
    _Filler: LongWord;
    {$IFEND}
    ImplGetter: NativeUInt;
  end;

  PInterfaceTable = ^TInterfaceTable;
  TInterfaceTable = packed record
    EntryCount: Integer;
    {$IF defined(CPUX64)}
    _Filler: LongWord;
    {$IFEND}
    Entries: array[0..9999{EntryCount - 1}] of TInterfaceEntry;
   {Intfs: array[0..EntryCount - 1] of PPTypeInfo;}
  end;

  TMethod = record
    Code, Data: Pointer;
  end;

{ TObject.Dispatch accepts any data type as its Message parameter.  The
  first 2 bytes of the data are taken as the message id to search for
  in the object's message methods.  TDispatchMessage is an example of
  such a structure with a word field for the message id.
}
  TDispatchMessage = record
    MsgID: Word;
  end;

  TObject = class
  public
    constructor Create;
    procedure Free;
    class function InitInstance(Instance: Pointer): TObject;
    procedure CleanupInstance;
    function ClassType: TClass; inline;
    class function ClassName: string;
    class function ClassNameIs(const Name: string): Boolean;
    class function ClassParent: TClass;
    class function ClassInfo: Pointer; inline;
    class function InstanceSize: Longint; inline;
    class function InheritsFrom(AClass: TClass): Boolean;
    class function MethodAddress(const Name: ShortString): Pointer; overload;
    class function MethodAddress(const Name: string): Pointer; overload;
    class function MethodName(Address: Pointer): string;
    class function QualifiedClassName: string;
    function FieldAddress(const Name: ShortString): Pointer; overload;
    function FieldAddress(const Name: string): Pointer; overload;
    function GetInterface(const IID: TGUID; out Obj): Boolean;
    class function GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
    class function GetInterfaceTable: PInterfaceTable;
    class function UnitName: string;
    class function UnitScope: string;
    function Equals(Obj: TObject): Boolean; virtual;
    function GetHashCode: Integer; virtual;
    function ToString: string; virtual;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; virtual;
    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;
    procedure Dispatch(var Message); virtual;
    procedure DefaultHandler(var Message); virtual;
    class function NewInstance: TObject; virtual;
    procedure FreeInstance; virtual;
    destructor Destroy; virtual;
  end;

{$IF Defined(MSWINDOWS)}
  TThreadID = LongWord;
{$IFEND}
{$IF Defined(MACOS) or Defined(LINUX)}
  TThreadID = NativeUInt;
{$IFEND}

  { TMonitor is an implementation of the concept invented by C.A.R Hoare and Per Brinch Hansen.
    See http://en.wikipedia.org/wiki/Monitor_%28synchronization%29 for more information.

    Every TObject derived instance can be used as a monitor. However, it is recommended that privately
    constructed objects be used rather than the publicly available instance itself. This will allow the
    developer to better control access to the lock and to ensure that the locking rules are adhered to.
    If a publicly available instance were to be used a the lock, such as a TComponent derivative, then
    deadlocks are more likely when external code is locking and unlocking the monitor in addition to
    the code internal to the class. In many cases, a mutex/critical section/condition variable can be
    created by simply constructing a variable of type TObject and calling the TMonitor.XXXX(ObjInstance)
    methods. }

  PPMonitor = ^PMonitor;
  PMonitor = ^TMonitor;
  TMonitor = record
  strict private
    type
      PWaitingThread = ^TWaitingThread;
      TWaitingThread = record
        Next: PWaitingThread;
        Thread: TThreadID;
        WaitEvent: Pointer;
      end;
      { TSpinWait implements an exponential backoff algorithm for TSpinLock. The algorithm is as follows:
        If the CPUCount > 1, then the first 10 (YieldThreshold) spin cycles (calls to SpinCycle) will use a base 2
        exponentially increasing spin count starting at 4. After 10 cycles, then the behavior reverts to the same
        behavior as when CPUCount = 1.
        If the CPUCount = 1, then it will sleep 1ms every modulus 20 cycles and sleep 0ms every modulus 5 cycles.
        All other cycles simply yield (SwitchToThread - Windows, sched_yield - POSIX). }
      TSpinWait = record
      private const
        YieldThreshold = 10;
        Sleep1Threshold = 20;
        Sleep0Threshold = 5;
      private
        FCount: Integer;
      public
        procedure Reset; inline;
        procedure SpinCycle;
      end;
      { TSpinLock implements a very simple non-reentrant lock. This lock does not block the calling thread using a
        synchronization object. Instead it opts to burn a few extra CPU cycles using the above TSpinWait type. This
        is typically far faster than fully blocking since the length of time the lock is held is relatively few
        cycles and the thread switching overhead will usually far outpace the few cycles burned by simply spin
        waiting. }
      TSpinLock = record
      private
        FLock: Integer;
      public
        procedure Enter;
        procedure Exit;
      end;
    var
      FLockCount: Integer;
      FRecursionCount: Integer;
      FOwningThread: TThreadID;
      FLockEvent: Pointer;
      FSpinCount: Integer;
      FWaitQueue: PWaitingThread;
      FQueueLock: TSpinLock;
    class var CacheLineSize: Integer;
    class procedure Spin(Iterations: Integer); static;
    class function GetCacheLineSize: Integer; static;
    procedure QueueWaiter(var WaitingThread: TWaitingThread);
    procedure RemoveWaiter(var WaitingThread: TWaitingThread);
    function DequeueWaiter: PWaitingThread;
    function GetEvent: Pointer;
    function CheckOwningThread: TThreadID;
    class procedure CheckMonitorSupport; static; inline;
    class function Create: PMonitor; static;
    // Make sure the following Destroy overload is always
    // listed first since it is called from an asm block
    // and there is no overload-resolution done from an
    // basm symbol reference
  private
    class procedure Destroy(AObject: TObject); overload; static;
  strict private
    class function GetFieldAddress(AObject: TObject): PPMonitor; inline; static;
    class function GetMonitor(AObject: TObject): PMonitor; static;
    procedure Destroy; overload;
    function Enter(Timeout: Cardinal): Boolean; overload;
    procedure Exit; overload;
    function TryEnter: Boolean; overload;
    function Wait(ALock: PMonitor; Timeout: Cardinal): Boolean; overload;
    procedure Pulse; overload;
    procedure PulseAll; overload;
  public
    { In multi-core/multi-processor systems, it is sometimes desirable to spin for a few cycles instead of blocking
      the current thread when attempting to Enter the monitor. Use SetSpinCount to set a reasonable number of times to
      spin before fully blocking the thread. This value usually obtained through empirical study of the particular
      situation.  }
    class procedure SetSpinCount(AObject: TObject; ASpinCount: Integer); static;
    { Enter locks the monitor object with an optional timeout (in ms) value. Enter without a timeout will wait until
      the lock is obtained. If the procedure returns it can be assumed that the lock was acquired. Enter with a
      timeout will return a boolean status indicating whether or not the lock was obtained (True) or the attempt timed
      out prior to acquire the lock (False). Calling Enter with an INFINITE timeout is the same as calling Enter
      without a timeout.
      TryEnter will simply attempt to obtain the lock and return immediately whether or not the lock was acuired.
      Enter with a 0ms timeout is functionally equivalent to TryEnter.
      Exit will potentially release the lock acquired by a call to Enter or TryEnter. Since Enter/TryEnter are
      rentrant, you must balance each of those calls with a corresponding call to Exit. Only the last call to Exit will
      release the lock and allow other threads to obtain it. Runtime error, reMonitorNoLocked, is generated if Exit is
      called and the calling thread does not own the lock. }
    class procedure Enter(AObject: TObject); overload; static; inline;
    class function Enter(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Exit(AObject: TObject); overload; static;
    class function TryEnter(AObject: TObject): Boolean; overload; static;
    { Wait will atomically fully release the lock (regardless of the recursion count) and block the calling thread
      until another thread calls Pulse or PulseAll. The first overloaded Wait function will assume the locked object
      and wait object are the same and thus the calling thread must own the lock. The second Wait allows the given
      monitor to atomically unlock the separate monitor lock object and block with the calling thread on the first
      given wait object. Wait will not return (even if it times out) until the monitor lock can be acquired again. It
      is possible for wait to return False (the timeout expired) after a much longer period of time has elapsed if
      the locking object was being held by another thread for an extended period. When Wait returns the recursion
      level of the lock has been restored.
      Pulse must be called on the exact same instance passed to Wait in order to properly release one waiting thread.
      PulseAll works the same as Pulse except that it will release all currently waiting threads.
      Wait/Pulse/PulseAll are the same as a traditional condition variable.
    }
    class function Wait(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class function Wait(AObject, ALock: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Pulse(AObject: TObject); overload; static;
    class procedure PulseAll(AObject: TObject); overload; static;
  end;

const
  INFINITE = Cardinal($FFFFFFFF);       {$EXTERNALSYM INFINITE}

function MonitorEnter(AObject: TObject; Timeout: Cardinal = INFINITE): Boolean; inline;
function MonitorTryEnter(AObject: TObject): Boolean; inline;
procedure MonitorExit(AObject: TObject); inline;
function MonitorWait(AObject: TObject; Timeout: Cardinal): Boolean; inline; overload;
function MonitorWait(AObject, ALock: TObject; Timeout: Cardinal): Boolean; inline; overload;
procedure MonitorPulse(AObject: TObject); inline;
procedure MonitorPulseAll(AObject: TObject); inline;
procedure MemoryBarrier;

procedure YieldProcessor; {$EXTERNALSYM YieldProcessor }

const
  S_OK = 0;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM S_OK}
{$ENDIF}
  S_FALSE = $00000001;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM S_FALSE}
{$ENDIF}
  E_NOINTERFACE = HRESULT($80004002);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_NOINTERFACE}
{$ENDIF}
  E_UNEXPECTED = HRESULT($8000FFFF);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_UNEXPECTED}
{$ENDIF}
  E_NOTIMPL = HRESULT($80004001);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_NOTIMPL}
{$ENDIF}

type
  IInterface = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;
  {$NODEFINE IInterface}        { defined in sysmac.h }

  IUnknown = IInterface;
  {$EXTERNALSYM IUnknown}       { from unknwn.h or sysmac.h }
{$M+}
  IInvokable = interface(IInterface)
  end;
{$M-}
  {$NODEFINE IInvokable}        { defined in sysmac.h }

  IEnumerator = interface(IInterface)
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  IEnumerable = interface(IInterface)
    function GetEnumerator: IEnumerator;
  end;

  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function GetEnumerator: IEnumerator<T>;
  end;

  IComparable = interface(IInterface)
    function CompareTo(Obj: TObject): Integer;
  end;

  IComparable<T> = interface(IComparable)
    function CompareTo(Value: T): Integer;
  end;

  IEquatable<T> = interface(IInterface)
    function Equals(Value: T): Boolean;
  end;

  IDispatch = interface(IUnknown)
    ['{00020400-0000-0000-C000-000000000046}']
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM IDispatch}      { from oaidl.h (oleauto.h) }
{$ENDIF}

{ TInterfacedObject provides a threadsafe default implementation
  of IInterface.  You should use TInterfaceObject as the base class
  of objects implementing interfaces.  }

  TInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;
  {$NODEFINE TInterfacedObject}         { defined in systobj.h }

  TInterfacedClass = class of TInterfacedObject;

{ TAggregatedObject and TContainedObject are suitable base
  classes for interfaced objects intended to be aggregated
  or contained in an outer controlling object.  When using
  the "implements" syntax on an interface property in
  an outer object class declaration, use these types
  to implement the inner object.

  Interfaces implemented by aggregated objects on behalf of
  the controller should not be distinguishable from other
  interfaces provided by the controller.  Aggregated objects
  must not maintain their own reference count - they must
  have the same lifetime as their controller.  To achieve this,
  aggregated objects reflect the reference count methods
  to the controller.

  TAggregatedObject simply reflects QueryInterface calls to
  its controller.  From such an aggregated object, one can
  obtain any interface that the controller supports, and
  only interfaces that the controller supports.  This is
  useful for implementing a controller class that uses one
  or more internal objects to implement the interfaces declared
  on the controller class.  Aggregation promotes implementation
  sharing across the object hierarchy.

  TAggregatedObject is what most aggregate objects should
  inherit from, especially when used in conjunction with
  the "implements" syntax.  }

  TAggregatedObject = class(TObject)
  private
    FController: Pointer;  // weak reference to controller
    function GetController: IInterface;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;
  {$NODEFINE TAggregatedObject} { defined in systobj.h }

  { TContainedObject is an aggregated object that isolates
    QueryInterface on the aggregate from the controller.
    TContainedObject will return only interfaces that the
    contained object itself implements, not interfaces
    that the controller implements.  This is useful for
    implementing nodes that are attached to a controller and
    have the same lifetime as the controller, but whose
    interface identity is separate from the controller.
    You might do this if you don't want the consumers of
    an aggregated interface to have access to other interfaces
    implemented by the controller - forced encapsulation.
    This is a less common case than TAggregatedObject.  }

  TContainedObject = class(TAggregatedObject, IInterface)
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;
  {$NODEFINE TContainedObject}  { defined in systobj.h }

  TClassHelperBase = class(TInterfacedObject, IInterface)
  protected
    FInstance: TObject;
    constructor _Create(Instance: TObject);
  end;

  TClassHelperBaseClass = class of TClassHelperBase;
  {$NODEFINE TClassHelperBaseClass}
  {$NODEFINE TClassHelperBase}

  { The base class for all custom attributes. Attribute
    instances created by the RTTI unit are owned by those
    members to which they apply. }
  TCustomAttribute = class(TObject)
  end;
  {$NODEFINE TCustomAttribute}

  PShortString = ^ShortString;
  PAnsiString = ^AnsiString;
  PWideString = ^WideString;
  PUnicodeString = ^UnicodeString;
  PString = PUnicodeString;
  {$NODEFINE PShortString}      { defined in sysmac.h }
  {$NODEFINE PAnsiString}       { defined in sysmac.h }
  {$NODEFINE PWideString}       { defined in sysmac.h }
  {$NODEFINE PUnicodeString}    { defined in sysmac.h }
  {$NODEFINE PString}           { defined in sysmac.h }

  UCS2Char = WideChar;
  PUCS2Char = PWideChar;
  UCS4Char = type LongWord;
  {$NODEFINE UCS4Char}          { defined in sysmac.h }
  {-OBJTYPENAME UCS4Char 'BCt'}
  PUCS4Char = ^UCS4Char;
  {$NODEFINE PUCS4Char}         { defined in sysmac.h }

  TUCS4CharArray = array [0..$effffff] of UCS4Char;
  PUCS4CharArray = ^TUCS4CharArray;
  {$NODEFINE TUCS4CharArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef UCS4Char* TUCS4CharArray;' *)
  (*$HPPEMIT '}' *)

  UCS4String = array of UCS4Char;
  {$NODEFINE UCS4String}        { defined in sysmac.h }

  UTF8String = type AnsiString(65001);
  PUTF8String = ^UTF8String;

  RawByteString = type AnsiString($ffff);
  PRawByteString = ^RawByteString;

  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  {$NODEFINE IntegerArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int* IntegerArray;' *)
  (*$HPPEMIT '}' *)

  Int64Array  = array[0..$0ffffffe] of Int64;
  PInt64Array = ^Int64Array;
  {$NODEFINE Int64Array}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef __int64* Int64Array;' *)
  (*$HPPEMIT '}' *)

  {$IFDEF CPUX64}
  PointerArray = array [0..256*1024*1024 - 2] of Pointer;
  {$ELSE !CPUX64}
  PointerArray = array [0..512*1024*1024 - 2] of Pointer;
  {$ENDIF !CPUX64}
  PPointerArray = ^PointerArray;
  {$NODEFINE PointerArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef void *PointerArray;' *)
  (*$HPPEMIT '}' *)

  TBoundArray = array of NativeInt;

  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
  PPCharArray = ^TPCharArray;
  {$NODEFINE TPCharArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef PChar TPCharArray;' *)
  (*$HPPEMIT '}' *)

  PLongInt      = ^LongInt;
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int *PLongInt;' *)
  (*$HPPEMIT '  typedef PLongInt PLongint;' *)
  (*$HPPEMIT '}' *)
  {$NODEFINE PLongInt}

  PInteger      = ^Integer;     {$NODEFINE PInteger}    { defined in sysmac.h }
  PCardinal     = ^Cardinal;
  PWord         = ^Word;
  PSmallInt     = ^SmallInt;    {$NODEFINE PSmallInt}   { defined in sysmac.h }
  {$POINTERMATH ON}
  PByte         = ^Byte;        {$NODEFINE PByte}       { defined in sysmac.h }
  {$POINTERMATH OFF}
  PShortInt     = ^ShortInt;    {$NODEFINE PShortInt}   { defined in sysmac.h }
  PInt64        = ^Int64;       {$NODEFINE PInt64}      { defined in sysmac.h }
  PUInt64       = ^UInt64;
  PLongWord     = ^LongWord;    {$NODEFINE PLongWord}   { defined in sysmac.h }
  PSingle       = ^Single;      {$NODEFINE PSingle}     { defined in sysmac.h }
  PDouble       = ^Double;      {$NODEFINE PDouble}     { defined in sysmac.h }
  PDate         = ^Double;
  PDispatch     = ^IDispatch;
  PPDispatch    = ^PDispatch;
  {$NODEFINE PDispatch}  // due to avoid compile error
  {$NODEFINE PPDispatch} // due to avoid compile error
  PError        = ^LongWord;
  PWordBool     = ^WordBool;
  PUnknown      = ^IUnknown;
  PPUnknown     = ^PUnknown;
  PPWideChar    = ^PWideChar;
  PPAnsiChar    = ^PAnsiChar;
  PPChar = PPWideChar;          {$NODEFINE PPChar}      { defined in sysmac.h }
  PExtended     = ^Extended;    {$NODEFINE PExtended}   { defined in sysmac.h }
  PComp         = ^Comp;
  PCurrency     = ^Currency;    {$NODEFINE PCurrency}   { defined in sysmac.h }
  PVariant      = ^Variant;     {$NODEFINE PVariant}    { defined in sysmac.h }
  POleVariant   = ^OleVariant;  {$NODEFINE POleVariant} { defined in sysmac.h }
  PPointer      = ^Pointer;     {$NODEFINE PPointer}    { defined in sysmac.h }
  PBoolean      = ^Boolean;     {$NODEFINE PBoolean}    { defined in sysmac.h }
  PNativeInt    = ^NativeInt;
  PNativeUInt   = ^NativeUInt;

  TDateTime = type Double;
  PDateTime = ^TDateTime;
  {$NODEFINE TDateTime 'TDateTime' 'TDateTimeBase'}     { defined in systdate.h }
  {$OBJTYPENAME TDateTime 'NTDateTime' }

  TDate = type TDateTime;
  TTime = type TDateTime;
  {$NODEFINE TDate 'TDate' 'TDateTimeBase'}
  {$NODEFINE TTime 'TTime' 'TDateTimeBase'}
  {$OBJTYPENAME TDate 'NTDateTime' }
  {$OBJTYPENAME TTime 'NTDateTime' }
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    typedef System::TDateTime TDate;' *)
  (*$HPPEMIT '    typedef System::TDateTime TTime;' *)
  (*$HPPEMIT '}' *)


{$IF Defined(POSIX)}
  THandle = Cardinal;
{$IFEND}
{$IF Defined(MSWINDOWS)}
  THandle = NativeUInt;
{$IFEND}
  {$NODEFINE THandle}

  PVarArrayBound = ^TVarArrayBound;
  TVarArrayBound = record
    ElementCount: Longint;
    LowBound: Longint;
  end;
  TVarArrayBoundArray = array [0..0] of TVarArrayBound;
  PVarArrayBoundArray = ^TVarArrayBoundArray;
  TVarArrayCoorArray = array [0..0] of Integer;
  PVarArrayCoorArray = ^TVarArrayCoorArray;

  PVarArray = ^TVarArray;
  TVarArray = record
    DimCount: Word;
    Flags: Word;
    ElementSize: Integer;
    LockCount: Integer;
    Data: Pointer;
    Bounds: TVarArrayBoundArray;
  end;

  PVarRecord = ^TVarRecord;
  TVarRecord = record
    PRecord: Pointer;
    RecInfo: Pointer;
  end;

  TLargestVarData = record
    _Reserved1: Pointer;
    _Reserved2: Pointer;
  end;

  TVarType = Word;
  PVarData = ^TVarData;
  TVarData = packed record
    case Integer of
      0: (VType: TVarType;
          case Integer of
            0: (Reserved1: Word;
                case Integer of
                  0: (Reserved2, Reserved3: Word;
                      case Integer of
                        varSmallInt: (VSmallInt: SmallInt);
                        varInteger:  (VInteger: Integer);
                        varSingle:   (VSingle: Single);
                        varDouble:   (VDouble: Double);
                        varCurrency: (VCurrency: Currency);
                        varDate:     (VDate: TDateTime);
                        varOleStr:   (VOleStr: PWideChar);
                        varDispatch: (VDispatch: Pointer);
                        varError:    (VError: HRESULT);
                        varBoolean:  (VBoolean: WordBool);
                        varUnknown:  (VUnknown: Pointer);
                        varShortInt: (VShortInt: ShortInt);
                        varByte:     (VByte: Byte);
                        varWord:     (VWord: Word);
                        varLongWord: (VLongWord: LongWord);
                        varInt64:    (VInt64: Int64);
                        varUInt64:   (VUInt64: UInt64);
                        varString:   (VString: Pointer);
                        varAny:      (VAny: Pointer);
                        varArray:    (VArray: PVarArray);
                        varByRef:    (VPointer: Pointer);
                        varUString:  (VUString: Pointer);
                        varRecord:   (VRecord: TVarRecord);
                        //$ffff:     (VLargest: TLargestVarData);
                     );
                  1: (VLongs: array[0..{$IFDEF CPUX86}2{$ELSE}4{$ENDIF}] of LongInt);
               );
            2: (VWords: array [0..{$IFDEF CPUX86}6{$ELSE}10{$ENDIF}] of Word);
            3: (VBytes: array [0..{$IFDEF CPUX86}13{$ELSE}21{$ENDIF}] of Byte);
          );
      1: (RawData: array [0..{$IFDEF CPUX86}3{$ELSE}5{$ENDIF}] of LongInt);
  end;
  {$EXTERNALSYM TVarData}
  {$EXTERNALSYM PVarData}

type
  TVarOp = Integer;

const
  opAdd =        0;
  opSubtract =   1;
  opMultiply =   2;
  opDivide =     3;
  opIntDivide =  4;
  opModulus =    5;
  opShiftLeft =  6;
  opShiftRight = 7;
  opAnd =        8;
  opOr =         9;
  opXor =        10;
  opCompare =    11;
  opNegate =     12;
  opNot =        13;

  opCmpEQ =      14;
  opCmpNE =      15;
  opCmpLT =      16;
  opCmpLE =      17;
  opCmpGT =      18;
  opCmpGE =      19;

  {The number of small block types employed by the default memory manager}
{$ifdef CPU386}
   NumSmallBlockTypes = 55;
{$else}
   NumSmallBlockTypes = 46;
{$endif}

type
  { Dispatch call descriptor }
  PCallDesc = ^TCallDesc;
  TCallDesc = packed record
    CallType: Byte;
    ArgCount: Byte;
    NamedArgCount: Byte;
    ArgTypes: array[0..255] of Byte;
  end;

  PDispDesc = ^TDispDesc;
  TDispDesc = packed record
    DispID: Integer;
    ResType: Byte;
    CallDesc: TCallDesc;
  end;

  PVariantManager = ^TVariantManager;
  TVariantManager = record
    VarClear: procedure(var V : Variant);
    VarCopy: procedure(var Dest: Variant; const Source: Variant);
    VarCopyNoInd: procedure; // ARGS PLEASE!
    VarCast: procedure(var Dest: Variant; const Source: Variant; VarType: Integer);
    VarCastOle: procedure(var Dest: Variant; const Source: Variant; VarType: Integer);

    VarToInt: function(const V: Variant): Integer;
    VarToInt64: function(const V: Variant): Int64;
    VarToBool: function(const V: Variant): Boolean;
    VarToReal: function(const V: Variant): Extended;
    VarToCurr: function(const V: Variant): Currency;
    VarToPStr: procedure(var S; const V: Variant);
    VarToLStr: procedure(var S: string; const V: Variant);
    VarToWStr: procedure(var S: WideString; const V: Variant);
    VarToIntf: procedure(var Unknown: IInterface; const V: Variant);
    VarToDisp: procedure(var Dispatch: IDispatch; const V: Variant);
    VarToDynArray: procedure(var DynArray: Pointer; const V: Variant; TypeInfo: Pointer);

    VarFromInt: procedure(var V: Variant; const Value: Integer; const Range: ShortInt);
    VarFromInt64: procedure(var V: Variant; const Value: Int64);
    VarFromBool: procedure(var V: Variant; const Value: Boolean);
    VarFromReal: procedure; // var V: Variant; const Value: Real
    VarFromTDateTime: procedure; // var V: Variant; const Value: TDateTime
    VarFromCurr: procedure; // var V: Variant; const Value: Currency
    VarFromPStr: procedure(var V: Variant; const Value: ShortString);
    VarFromLStr: procedure(var V: Variant; const Value: string);
    VarFromWStr: procedure(var V: Variant; const Value: WideString);
    VarFromIntf: procedure(var V: Variant; const Value: IInterface);
    VarFromDisp: procedure(var V: Variant; const Value: IDispatch);
    VarFromDynArray: procedure(var V: Variant; const DynArray: Pointer; TypeInfo: Pointer);
    OleVarFromPStr: procedure(var V: OleVariant; const Value: ShortString);
    OleVarFromLStr: procedure(var V: OleVariant; const Value: string);
    OleVarFromVar: procedure(var V: OleVariant; const Value: Variant);
    OleVarFromInt: procedure(var V: OleVariant; const Value: Integer; const Range: ShortInt);
    OleVarFromInt64: procedure(var V: OleVariant; const Value: Int64);

    VarOp: procedure(var Left: Variant; const Right: Variant; OpCode: TVarOp);
    VarCmp: procedure(const Left, Right: TVarData; const OpCode: TVarOp); { result is set in the flags }
    VarNeg: procedure(var V: Variant);
    VarNot: procedure(var V: Variant);

    DispInvoke: procedure(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); cdecl;
    VarAddRef: procedure(var V: Variant);

    VarArrayRedim: procedure(var A : Variant; HighBound: Integer);
    VarArrayGet: function(var A: Variant; IndexCount: Integer;
      Indices: Integer): Variant; cdecl;
    VarArrayPut: procedure(var A: Variant; const Value: Variant;
      IndexCount: Integer; Indices: Integer); cdecl;

    WriteVariant: function(var T: Text; const V: Variant; Width: Integer): Pointer;
    Write0Variant: function(var T: Text; const V: Variant): Pointer;
  end deprecated;

  { Dynamic array support }
  PDynArrayTypeInfo = ^TDynArrayTypeInfo;
  {$EXTERNALSYM PDynArrayTypeInfo}
  TDynArrayTypeInfo = packed record
    kind: Byte;
    name: string[0];
    elSize: Longint;
    elType: ^PDynArrayTypeInfo;
    varType: Integer;
  end;
  {$EXTERNALSYM TDynArrayTypeInfo}

  PVarRec = ^TVarRec;
  TVarRec = record { do not pack this record; it is compiler-generated }
    case Integer of
      0: (case Byte of
            vtInteger:       (VInteger: Integer);
            vtBoolean:       (VBoolean: Boolean);
            vtChar:          (VChar: AnsiChar);
            vtExtended:      (VExtended: PExtended);
            vtString:        (VString: PShortString);
            vtPointer:       (VPointer: Pointer);
            vtPChar:         (VPChar: PAnsiChar);
            vtObject:        (VObject: TObject);
            vtClass:         (VClass: TClass);
            vtWideChar:      (VWideChar: WideChar);
            vtPWideChar:     (VPWideChar: PWideChar);
            vtAnsiString:    (VAnsiString: Pointer);
            vtCurrency:      (VCurrency: PCurrency);
            vtVariant:       (VVariant: PVariant);
            vtInterface:     (VInterface: Pointer);
            vtWideString:    (VWideString: Pointer);
            vtInt64:         (VInt64: PInt64);
            vtUnicodeString: (VUnicodeString: Pointer);
         );
      1: (_Reserved1: NativeInt;
          VType:      Byte;
         );
  end;
  {$NODEFINE PVarRec}   { defined in systvar.h }
  {$NODEFINE TVarRec}   { defined in systvar.h }

  {The old memory manager structure (for backward compatibility)}
  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    GetMem: function(Size: NativeInt): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: NativeInt): Pointer;
  end deprecated 'Use TMemoryManagerEx';

  {The new memory manager structure with expanded functionality}
  PMemoryManagerEx = ^TMemoryManagerEx;
  TMemoryManagerEx = record
    {The basic (required) memory manager functionality}
    GetMem: function(Size: NativeInt): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: NativeInt): Pointer;
    {Extended (optional) functionality.}
    AllocMem: function(Size: NativeInt): Pointer;
    RegisterExpectedMemoryLeak: function(P: Pointer): Boolean;
    UnregisterExpectedMemoryLeak: function(P: Pointer): Boolean;
  end;

  THeapStatus = record
    TotalAddrSpace: NativeUInt;
    TotalUncommitted: NativeUInt;
    TotalCommitted: NativeUInt;
    TotalAllocated: NativeUInt;
    TotalFree: NativeUInt;
    FreeSmall: NativeUInt;
    FreeBig: NativeUInt;
    Unused: NativeUInt;
    Overhead: NativeUInt;
    HeapErrorCode: Cardinal;
  end deprecated;

  TSmallBlockTypeState = packed record
    {The internal size of the block type}
    InternalBlockSize: Cardinal;
    {Useable block size: The number of non-reserved bytes inside the block.}
    UseableBlockSize: Cardinal;
    {The number of allocated blocks}
    AllocatedBlockCount: NativeUInt;
    {The total address space reserved for this block type (both allocated and
     free blocks)}
    ReservedAddressSpace: NativeUInt;
  end;
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;

  TMemoryManagerState = packed record
    {Small block type states}
    SmallBlockTypeStates: TSmallBlockTypeStates;
    {Medium block stats}
    AllocatedMediumBlockCount: Cardinal;
    TotalAllocatedMediumBlockSize: NativeUInt;
    ReservedMediumBlockAddressSpace: NativeUInt;
    {Large block stats}
    AllocatedLargeBlockCount: Cardinal;
    TotalAllocatedLargeBlockSize: NativeUInt;
    ReservedLargeBlockAddressSpace: NativeUInt;
  end;

  PMonitorSupport = ^TMonitorSupport;
  TMonitorSupport = record
    // Obtain a synchronization object - usually an auto-reset event or semaphore
    NewSyncObject: function: Pointer;
    // Free the synchronization object obtained from NewSyncObject
    FreeSyncObject: procedure (SyncObject: Pointer);
    // Obtain a wait object - usually an auto-reset event or semaphore - these should be cached
    NewWaitObject: function: Pointer;
    // Return the wait object from NewWaitObject back to the cache
    FreeWaitObject: procedure (WaitObject: Pointer);
    // Wait for either a SyncObject or WaitObject or signal an object
    // o WaitOrSignalObject(nil, Obj, Timeout); - Wait for <Timeout> time or until <Obj> is signaled
    // o WaitOrSignalObject(Obj, nil, 0); - Signal <Obj> and return. Timeout and WaitObject params ignored.
    WaitOrSignalObject: function (SignalObject, WaitObject: Pointer; Timeout: Cardinal): Cardinal;
  end;

  {Memory map}
  TChunkStatus = (csUnallocated, csAllocated, csReserved,
    csSysAllocated, csSysReserved);
  TMemoryMap = array[0..65535] of TChunkStatus;

  {Block alignment options}
  TMinimumBlockAlignment = (mba8Byte, mba16Byte);

{$IFDEF PC_MAPPED_EXCEPTIONS}
  PUnwinder = ^TUnwinder;
  TUnwinder = record
    RaiseException: function(Exc: Pointer): LongBool; cdecl;
    RegisterIPLookup: function(fn: Pointer; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
    UnregisterIPLookup: procedure(StartAddr: LongInt) cdecl;
    DelphiLookup: function(Addr: LongInt; Context: Pointer): Pointer; cdecl;
    ClosestHandler: function(Context: Pointer): LongWord; cdecl;
  end;
  TFrameUnwinder = Pointer;
  TUnwinderLookup = function(Addr: LongInt; Context: Pointer): TFrameUnwinder; cdecl;
{$ENDIF PC_MAPPED_EXCEPTIONS}

  PackageUnitEntry = packed record
    Init, FInit : Pointer;
  end;

  { Compiler generated table to be processed sequentially to init & finit all package units }
  { Init: 0..Max-1; Final: Last Initialized..0                                              }
  UnitEntryTable = array [0..9999999] of PackageUnitEntry;
  PUnitEntryTable = ^UnitEntryTable;
  { Pointer in this table is PPTypeInfo, except when it's not; if the value is 1,
    then it's a "unit boundary" marker, indicating that following types are in
    the next unit along in the TPackageTypeInfo.UnitNames unit name list sequence. }
  TTypeTable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
  PTypeTable = ^TTypeTable;

  {$NODEFINE UnitEntryTable}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    typedef struct PackageUnitEntry UnitEntryTable;' *)
  (*$HPPEMIT '}' *)

  PPackageTypeInfo = ^TPackageTypeInfo;
  TPackageTypeInfo = record
    TypeCount: Integer;
    TypeTable: PTypeTable;
    UnitCount: Integer;
    UnitNames: PShortString; { concatenation of Pascal strings, one for each unit }
  end;

  PackageInfoTable = record
    UnitCount: Integer;      { number of entries in UnitInfo array; always > 0 }
    UnitInfo: PUnitEntryTable;
    TypeInfo: TPackageTypeInfo;
  end;

  PackageInfo = ^PackageInfoTable;

  { Each package exports a '@GetPackageInfoTable' which can be used to retrieve }
  { the table which contains compiler generated information about the package DLL }
  GetPackageInfoTable = function : PackageInfo;

{$IFDEF DEBUG_FUNCTIONS}
{ Inspector Query; implementation in GETMEM.INC; no need to conditionalize that }
  THeapBlock = record
    Start: Pointer;
    Size: Cardinal;
  end;

  THeapBlockArray = array of THeapBlock;
  TObjectArray = array of TObject;

function GetHeapBlocks: THeapBlockArray;
function FindObjects(AClass: TClass; FindDerived: Boolean): TObjectArray;
{ Inspector Query }
{$ENDIF}

{
  When an exception is thrown, the exception object that is thrown is destroyed
  automatically when the except clause which handles the exception is exited.
  There are some cases in which an application may wish to acquire the thrown
  object and keep it alive after the except clause is exited.  For this purpose,
  we have added the AcquireExceptionObject and ReleaseExceptionObject functions.
  These functions maintain a reference count on the most current exception object,
  allowing applications to legitimately obtain references.  If the reference count
  for an exception that is being thrown is positive when the except clause is exited,
  then the thrown object is not destroyed by the RTL, but assumed to be in control
  of the application.  It is then the application's responsibility to destroy the
  thrown object.  If the reference count is zero, then the RTL will destroy the
  thrown object when the except clause is exited.
}
function AcquireExceptionObject: Pointer;
procedure ReleaseExceptionObject;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure GetUnwinder(var Dest: TUnwinder);
procedure SetUnwinder(const NewUnwinder: TUnwinder);
function IsUnwinderSet: Boolean;

//function SysRegisterIPLookup(ModuleHandle, StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
{
  Do NOT call these functions.  They are for internal use only:
    SysRegisterIPLookup
    SysUnregisterIPLookup
    BlockOSExceptions
    UnblockOSExceptions
    AreOSExceptionsBlocked
}
function SysRegisterIPLookup(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
function SysRegisterIPLookupFunc(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt;
                            UnwinderLookup: TUnwinderLookup): LongBool;
procedure SysUnregisterIPLookup(StartAddr: LongInt);
//function SysAddressIsInPCMap(Addr: LongInt): Boolean;
function SysClosestDelphiHandler(Context: Pointer): LongWord;
procedure BlockOSExceptions;
procedure UnblockOSExceptions;
function AreOSExceptionsBlocked: Boolean;

{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF STACK_BASED_EXCEPTIONS}
// These functions are not portable.  Use AcquireExceptionObject above instead
function RaiseList: Pointer; deprecated 'Use AcquireExceptionObject';  { Stack of current exception objects }
function SetRaiseList(NewPtr: Pointer): Pointer; deprecated 'Use AcquireExceptionObject';  { returns previous value }
{$ENDIF STACK_BASED_EXCEPTIONS}

function ExceptObject: TObject;
function ExceptAddr: Pointer;

{$IFDEF MSWINDOWS}
{
  Coverage support.  These are internal use structures referenced by compiler
  helper functions for QA coverage support.
}
type
    TCVModInfo = packed record
        ModName: PAnsiChar;
        LibName: PAnsiChar;
        UserData: Pointer;
        end;
    PCVModInfo = ^TCVModInfo;

{$EXTERNALSYM _CVR_PROBE}
procedure _CVR_PROBE(mi: PCVModInfo; probeNum: Cardinal); cdecl;
{$EXTERNALSYM _CVR_STMTPROBE}
function _CVR_STMTPROBE(mi: PCVModInfo; probeNum: Cardinal; TrueFalse: Cardinal): Boolean; cdecl;
{$ENDIF MSWINDOWS}

type
  TAssertErrorProc = procedure (const Message, Filename: string;
    LineNumber: Integer; ErrorAddr: Pointer);
  TSafeCallErrorProc = procedure (ErrorCode: HResult; ErrorAddr: Pointer);
  TRaiseExceptionProc = procedure (ExceptionCode, ExceptionFlags: LongWord;
    NumberOfArguments: LongWord; Args: Pointer); stdcall;

{$IFDEF DEBUG}
{
  This variable is just for debugging the exception handling system.  See
  _DbgExcNotify for the usage.
}
var
  ExcNotificationProc : procedure(NotificationKind: Integer;
                                  ExceptionObject: Pointer;
                                  ExceptionName: PShortString;
                                  ExceptionLocation: Pointer;
                                  HandlerAddr: Pointer) = nil;
{$ENDIF DEBUG}

var
  DispCallByIDProc: Pointer;
  ExceptProc: Pointer;    { Unhandled exception handler }
  ErrorProc: procedure (ErrorCode: Byte; ErrorAddr: Pointer);     { Error handler procedure }
{$IFDEF MSWINDOWS}
  ExceptClsProc: Pointer; { Map an OS Exception to a Delphi class reference }
  ExceptObjProc: Pointer; { Map an OS Exception to a Delphi class instance }
{$IF defined(CPU386)}
  RaiseExceptionProc: Pointer;
{$ELSE}
  RaiseExceptionProc: TRaiseExceptionProc;
{$IFEND}
  RTLUnwindProc: Pointer;
{$ENDIF MSWINDOWS}
  RaiseExceptObjProc: Pointer; { notify of the raise of an exception object }
  ExceptionAcquired: Pointer; { notification that a given exception object has been "acquired" (C++)}
  ExceptionClass: TClass; { Exception base class (must be Exception) }
  SafeCallErrorProc: TSafeCallErrorProc; { Safecall error handler }
  AssertErrorProc: TAssertErrorProc; { Assertion error handler }
  ExitProcessProc: procedure; { Hook to be called just before the process actually exits }
  AbstractErrorProc: procedure; { Abstract method error handler }
  HPrevInst: LongWord deprecated;    { Handle of previous instance - HPrevInst cannot be tested for multiple instances in Win32}
  MainInstance: THandle;    { Handle of the main(.EXE) HInstance }
{$IFDEF MSWINDOWS}
  {$NODEFINE MainInstance}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    extern PACKAGE HINSTANCE MainInstance;' *)
  (*$HPPEMIT '}' *)
{$ENDIF}
  MainThreadID: TThreadID;  { ThreadID of thread that module was initialized in }
  IsLibrary: Boolean;       { True if module is a DLL }
{$IFDEF MSWINDOWS}
  CmdShow: Integer platform;{ CmdShow parameter for CreateWindow }
  CmdLine: PChar platform;  { Command line pointer }
{$ENDIF MSWINDOWS}
  InitProc: Pointer;        { Last installed initialization procedure }
  ExitCode: Integer = 0;    { Program result }
  ExitProc: Pointer;        { Last installed exit procedure }
  ErrorAddr: Pointer = nil; { Address of run-time error }
  RandSeed: Longint = 0;    { Base for random number generator }
  IsConsole: Boolean;       { True if compiled as console app }
  IsMultiThread: Boolean;   { True if more than one thread }
  FileMode: Byte = 2;       { Standard mode for opening files }
{$IF defined(LINUX) or defined(MACOS)}
  FileAccessRights: Integer platform; { Default access rights for opening files }
  ArgCount: Integer platform;
  ArgValues: PPAnsiChar platform;
  envp: PPChar platform;
{$IFEND LINUX or MACOS}
  Test8086: Byte;           { CPU family (minus one) See consts below }
  Test8087: Byte = 3;       { assume 80387 FPU or OS supplied FPU emulation }
  TestFDIV: Shortint;       { -1: Flawed Pentium, 0: Not determined, 1: Ok }
  TestSSE: Cardinal;        { 0: no SSE, 1st bit: SSE available, 2nd bit: SSE2 available }
  CPUCount: Integer;        { Number of CPU Cores detected }
  Input: Text;              { Standard input }
  Output: Text;             { Standard output }
  ErrOutput: Text;          { Standard error output }

  VarClearProc:  procedure (var v: TVarData) = nil; // for internal use only
  VarAddRefProc: procedure (var v: TVarData) = nil; // for internal use only
  VarCopyProc:   procedure (var Dest: TVarData; const Source: TVarData) = nil; // for internal use only
  VarToLStrProc: procedure (var Dest: AnsiString; const Source: TVarData) = nil;   // for internal use only
  VarToWStrProc: procedure (var Dest: WideString; const Source: TVarData) = nil;   // for internal use only
  VarToUStrProc: procedure (var Dest: UnicodeString; const Source: TVarData) = nil;// for internal use only

  MonitorSupport: PMonitorSupport;
  {$EXTERNALSYM MonitorSupport}

const
  CPUi386     = 2;
  CPUi486     = 3;
  CPUPentium  = 4;

var
  Default8087CW: Word = $1332;{ Default 8087 control word.  FPU control
                                register is set to this value.
                                CAUTION:  Setting this to an invalid value
                                could cause unpredictable behavior. }
  DefaultMXCSR: UInt32 = $1900; { Default MXCSR control word.  SSE control
                                register is set to this value.
                                CAUTION:  Setting this to an invalid value
                                could cause unpredictable behavior. }
  HeapAllocFlags: Word platform = 2;   { Heap allocation flags, gmem_Moveable }
  DebugHook: Byte platform = 0;        { 1 to notify debugger of non-Delphi exceptions
                                >1 to notify debugger of exception unwinding }
  JITEnable: Byte platform = 0;        { 1 to call UnhandledExceptionFilter if the exception
                                is not a Pascal exception.
                                >1 to call UnhandledExceptionFilter for all exceptions }
  NoErrMsg: Boolean platform = False;  { True causes the base RTL to not display the message box
                                when a run-time error occurs }
{$IF defined(LINUX) or defined(MACOS)}
                              { CoreDumpEnabled = True will cause unhandled
                                exceptions and runtime errors to raise a
                                SIGABRT signal, which will cause the OS to
                                coredump the process address space.  This can
                                be useful for postmortem debugging. }
  CoreDumpEnabled: Boolean platform = False;
{$IFEND LINUX or MACOS}
  DefaultSystemCodePage: Integer;
  DefaultUnicodeCodePage: Integer; { Used by _NewUnicodeString to set the codePage field of strRec }
{$IFDEF MSWINDOWS}
  UTF8CompareLocale: Cardinal;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
  function UTF8CompareLocale: Pointer;
  function SetUTF8CompareLocale(const LocaleName: string): Boolean; platform;
{$ENDIF}

type
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);

var   { Text output line break handling.  Default value for all text files }
  DefaultTextLineBreakStyle: TTextLineBreakStyle = {$IFDEF LINUX} tlbsLF {$ENDIF}
                                                 {$IFDEF MSWINDOWS} tlbsCRLF {$ENDIF}
                                                 {$IFDEF MACOS} tlbsLF {$ENDIF};
const
   sLineBreak = {$IFDEF POSIX} AnsiChar(#10) {$ENDIF}
       {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};


type
  HRSRC = THandle;              { from windef.h / winnt.h }
  TResourceHandle = HRSRC;   // make an opaque handle type
  HINST = THandle;              { HINSTANCE from widnef.h }
  HMODULE = HINST;              { from windef.h }
  HGLOBAL = THandle;            { from windef.h }
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM HRSRC}
  {$NODEFINE HINST}
  {$EXTERNALSYM HMODULE}
  {$EXTERNALSYM HGLOBAL}
{$ENDIF}

{$HPPEMIT '#if defined(FindResource)'}
{$HPPEMIT '#define Save_FindResource'}
{$HPPEMIT '#undef FindResource'}
{$HPPEMIT '#endif'}
{$HPPEMIT END '#if defined(Save_FindResource)'}
{$HPPEMIT END '#ifdef UNICODE'}
{$HPPEMIT END '#define FindResource FindResourceW'}
{$HPPEMIT END '#else'}
{$HPPEMIT END '#define FindResource FindResourceA'}
{$HPPEMIT END '#endif'}
{$HPPEMIT END '#undef Save_FindResource'}
{$HPPEMIT END '#endif'}
{$HPPEMIT '#if defined(UnlockResource)'}
{$HPPEMIT '#define Save_UnlockResource'}
{$HPPEMIT '#undef UnlockResource'}
{$HPPEMIT '#endif'}
{$HPPEMIT END '#if defined(Save_UnlockResource)'}
{$HPPEMIT END '#define UnlockResource(hResData) ((hResData), 0)'}
{$HPPEMIT END '#undef Save_UnlockResource'}
{$HPPEMIT END '#endif'}
function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
function LockResource(ResData: HGLOBAL): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
function UnlockResource(ResData: HGLOBAL): LongBool; inline;
function FreeResource(ResData: HGLOBAL): LongBool; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}

{ Memory manager support }

procedure GetMemoryManager(var MemMgr: TMemoryManager); overload; deprecated;
procedure SetMemoryManager(const MemMgr: TMemoryManager); overload; deprecated;
procedure GetMemoryManager(var MemMgrEx: TMemoryManagerEx); overload;
procedure SetMemoryManager(const MemMgrEx: TMemoryManagerEx); overload;
function IsMemoryManagerSet: Boolean;

function SysGetMem(Size: NativeInt): Pointer;
function SysFreeMem(P: Pointer): Integer;
function SysReallocMem(P: Pointer; Size: NativeInt): Pointer;
function SysAllocMem(Size: NativeInt): Pointer;
function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;

{ AllocMem allocates a block of the given size on the heap. Each byte in
  the allocated buffer is set to zero. To dispose the buffer, use the
  FreeMem standard procedure. }

function AllocMem(Size: NativeInt): Pointer;

var

  AllocMemCount: Integer deprecated; {Unsupported}
  AllocMemSize: Integer deprecated; {Unsupported}

{Set this variable to true to report memory leaks on shutdown. This setting
 has no effect if this module is sharing a memory manager owned by another
 module.}
  ReportMemoryLeaksOnShutdown: Boolean;

{Set this variable to true to employ a "busy waiting" loop instead of putting
 the thread to sleep if a thread contention occurs inside the memory manager.
 This may improve performance on multi-CPU systems with a relatively low thread
 count, but will hurt performance otherwise.}
  NeverSleepOnMMThreadContention: Boolean;

{$IFDEF MSWINDOWS}
function GetHeapStatus: THeapStatus; platform; deprecated; {Unsupported}

{Returns information about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);

{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);

{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited in size, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(P: Pointer): boolean;

{Removes expected memory leaks. Returns true if the previously registered leak
 was found and removed.}
function UnregisterExpectedMemoryLeak(P: Pointer): boolean;

{Set the minimum block alignment. In the current implementation blocks >=160
 bytes will always be at least 16 byte aligned, even if only 8-byte alignment
 (the default) is required.}
function GetMinimumBlockAlignment: TMinimumBlockAlignment;
procedure SetMinimumBlockAlignment(AMinimumBlockAlignment: TMinimumBlockAlignment);

{Searches the current process for a shared memory manager. If no memory has
 been allocated using this memory manager it will switch to using the shared
 memory manager instead. Returns true if another memory manager was found and
 this module is now sharing it.}
function AttemptToUseSharedMemoryManager: Boolean;

{Makes this memory manager available for sharing to other modules in the
 current process. Only one memory manager may be shared per process, so this
 function may fail.}
function ShareMemoryManager: Boolean;

{$ENDIF}

{ Thread support }
type
  TThreadFunc = function(Parameter: Pointer): Integer;

{$IFDEF POSIX}

{$IFDEF LINUX}
type
  TSize_T = Cardinal;

  TSchedParam = record
    sched_priority: Integer;
  end;
  {$DEFINE _PTHREAD_ATTR_T_DEFINED}
  pthread_attr_t = record
    __detachstate,
    __schedpolicy: Integer;
    __schedparam: TSchedParam;
    __inheritsched,
    __scope: Integer;
    __guardsize: TSize_T;
    __stackaddr_set: Integer;
    __stackaddr: Pointer;
    __stacksize: TSize_T;
  end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF LINUX}
{$IFDEF MACOS}
const
   PTHREAD_ATTR_SIZE = 36;
   SCHED_PARAM_SIZE = 4;
type
  TSchedParam = record
    sched_priority: Integer;
    opaque: array [0..SCHED_PARAM_SIZE] of Byte;
  end;
  {$DEFINE _PTHREAD_ATTR_T_DEFINED}
   pthread_attr_t = record
      __sig: Longint;
      opaque: array [0..PTHREAD_ATTR_SIZE] of Byte;
   end;
  {$EXTERNALSYM pthread_attr_t}  // Defined in signal.h
{$ENDIF MACOS}

type
  TThreadAttr = pthread_attr_t;
  PThreadAttr = ^TThreadAttr;

  TBeginThreadProc = function (Attribute: PThreadAttr;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    var ThreadId: TThreadID): Integer;
  TEndThreadProc = procedure(ExitCode: Integer);

var
  BeginThreadProc: TBeginThreadProc = nil;
  EndThreadProc: TEndThreadProc = nil;
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}

type
  TSystemThreadFuncProc = function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
  TSystemThreadEndProc = procedure(ExitCode: Integer);
  {$NODEFINE TSystemThreadFuncProc}
  {$NODEFINE TSystemThreadEndProc}

  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef void * (__fastcall * TSystemThreadFuncProc)(void *, void * );' *)
  (*$HPPEMIT '  typedef void (__fastcall * TSystemThreadEndProc)(int);' *)
  (*$HPPEMIT '}' *)

var
  // SystemThreadFuncProc and SystemThreadEndProc are set during the startup
  // code by the C++ RTL when running in a C++Builder VCL application.
  SystemThreadFuncProc: TSystemThreadFuncProc = nil;
  SystemThreadEndProc: TSystemThreadEndProc = nil;

function BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: TThreadID): Integer;
{$ENDIF}
{$IFDEF POSIX}
function BeginThread(Attribute: PThreadAttr; ThreadFunc: TThreadFunc;
                     Parameter: Pointer; var ThreadId: TThreadID): Integer;

{$ENDIF}
procedure EndThread(ExitCode: Integer);

{ Standard procedures and functions }

const
{ File mode magic numbers }

  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

{ Text file flags         }
  tfCRLF   = $1;    // Dos compatibility flag, for CR+LF line breaks and EOF checks

type
{ Typed-file and untyped-file record }

  TFileRec = packed record (* must match the size the compiler generates: 592 bytes (616 bytes for x64) *)
    Handle: NativeInt;
    Mode: Word;
    Flags: Word;
    case Byte of
      0: (RecSize: Cardinal);   //  files of record
      1: (BufSize: Cardinal;    //  text files
          BufPos: Cardinal;
          BufEnd: Cardinal;
          BufPtr: PAnsiChar;
          OpenFunc: Pointer;
          InOutFunc: Pointer;
          FlushFunc: Pointer;
          CloseFunc: Pointer;
          UserData: array[1..32] of Byte;
          Name: array[0..259] of WideChar;
      );
  end;

{ Text file record structure used for Text files }
  PTextBuf = ^TTextBuf;
  TTextBuf = array[0..127] of AnsiChar;
  TTextRec = packed record (* must match the size the compiler generates: 730 bytes (754 bytes for x64) *)
    Handle: NativeInt;       (* must overlay with TFileRec *)
    Mode: Word;
    Flags: Word;
    BufSize: Cardinal;
    BufPos: Cardinal;
    BufEnd: Cardinal;
    BufPtr: PAnsiChar;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..32] of Byte;
    Name: array[0..259] of WideChar;
    Buffer: TTextBuf;
    CodePage: Word;
    MBCSLength: ShortInt;
    MBCSBufPos: Byte;
    case Integer of
      0: (MBCSBuffer: array[0..5] of AnsiChar);
      1: (UTF16Buffer: array[0..2] of WideChar);
  end;

  TTextIOFunc = function (var F: TTextRec): Integer;
  TFileIOFunc = function (var F: TFileRec): Integer;

procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
function GetTextCodePage(const T: Text): Word;
procedure SetTextCodePage(var T: Text; CodePage: Word);
procedure __IOTest;
procedure SetInOutRes(NewValue: Integer);
procedure ChDir(const S: string); overload;
procedure ChDir(P: PChar); overload;
function Flush(var t: Text): Integer;
procedure _UGetDir(D: Byte; var S: UnicodeString);
procedure _LGetDir(D: Byte; var S: AnsiString);
procedure _WGetDir(D: Byte; var S: WideString);
procedure _SGetDir(D: Byte; var S: ShortString);
function IOResult: Integer;
procedure MkDir(const S: string); overload;
procedure MkDir(P: PChar); overload;
procedure Move(const Source; var Dest; Count: NativeInt);
procedure MoveChars(const Source; var Dest; Length: Integer); inline;
function ParamCount: Integer;
function ParamStr(Index: Integer): string;
procedure RmDir(const S: string); overload;
procedure RmDir(P: PChar); overload;
function UpCase(Ch: AnsiChar): AnsiChar; overload; inline;
function UpCase(Ch: WideChar): WideChar; overload; inline;

{ random functions }
procedure Randomize;

function Random(const ARange: Integer): Integer; overload;
function Random: Extended; overload;


{ Control 8087 control word }

procedure Reset8087CW; // Resets to Default8087CW
procedure Set8087CW(NewCW: Word);
function Get8087CW: Word;

{ Control MXCSR control word }

procedure ResetMXCSR; // Resets to DefaultMXCSR
procedure SetMXCSR(NewMXCSR: UInt32);
function GetMXCSR: UInt32;

{ Wide character support procedures and functions for C++ }
{ These functions should not be used in Delphi code!
 (conversion is implicit in Delphi code)      }

function WideCharToString(Source: PWideChar): UnicodeString;
function WideCharLenToString(Source: PWideChar; SourceLen: Integer): UnicodeString;
procedure WideCharToStrVar(Source: PWideChar; var Dest: UnicodeString);
procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: UnicodeString); overload;
procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: AnsiString); overload;
function StringToWideChar(const Source: UnicodeString; Dest: PWideChar;
  DestSize: Integer): PWideChar;

{ PUCS4Chars returns a pointer to the UCS4 char data in the
  UCS4String array, or a pointer to a null char if UCS4String is empty }

function PUCS4Chars(const S: UCS4String): PUCS4Char;

{ Widestring <-> UCS4 conversion }

function WideStringToUCS4String(const S: WideString): UCS4String;
function UCS4StringToWideString(const S: UCS4String): WideString;

{ PAnsiChar/PWideChar Unicode <-> UTF8 conversion }

// UnicodeToUTF8(3):
// UTF8ToUnicode(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer; overload; deprecated;
function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer; overload; deprecated;

// UnicodeToUtf8(4):
// UTF8ToUnicode(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal; overload;

{ WideString <-> UTF8 conversion }

function UTF8Encode(const WS: WideString): RawByteString; overload;
function UTF8Encode(const US: UnicodeString): RawByteString; overload;
function UTF8Encode(const A: RawByteString): RawByteString; overload;
function UTF8EncodeToShortString(const WS: WideString): ShortString; overload;
function UTF8EncodeToShortString(const US: UnicodeString): ShortString; overload;
function UTF8EncodeToShortString(const A: RawByteString): ShortString; overload;
function UTF8Decode(const S: RawByteString): WideString; deprecated 'Use UTF8ToWideString or UTF8ToString';
function UTF8ToWideString(const S: RawByteString): WideString; inline;
function UTF8ToUnicodeString(const S: RawByteString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
function UTF8ToString(const S: RawByteString): string; inline; overload;
function UTF8ToString(const S: ShortString): string; inline; overload;
function UTF8ToString(const S: PAnsiChar): string; inline; overload;
function UTF8ToString(const S: array of AnsiChar): string; overload;
//function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;

{ Ansi <-> UTF8 conversion }

                                                                  
function AnsiToUtf8(const S: string): RawByteString;
function Utf8ToAnsi(const S: RawByteString): string;

{ OLE string support procedures and functions }

function OleStrToString(Source: PWideChar): UnicodeString;
procedure OleStrToStrVar(Source: PWideChar; var Dest: AnsiString); overload;
procedure OleStrToStrVar(Source: PWideChar; var Dest: UnicodeString); overload;
function StringToOleStr(const Source: AnsiString): PWideChar; overload;
function StringToOleStr(const Source: UnicodeString): PWideChar; overload;

{ Variant manager support procedures and functions (obsolete - see Variants.pas) }

procedure GetVariantManager(var VarMgr: TVariantManager); deprecated;
procedure SetVariantManager(const VarMgr: TVariantManager); deprecated;
function IsVariantManagerSet: Boolean; deprecated;

{ Interface dispatch support }

{$IFNDEF CPUX86}
procedure _IntfDispCall(Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer); cdecl;
procedure _IntfVarCall(Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer); cdecl;
{$ELSE CPUX86}
// these functions are actually varargs, which dcc32 cannot handle, so these
// functions cannot be invoked from pure pascal code with parameters
procedure _IntfDispCall{Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer}; cdecl;
procedure _IntfVarCall{Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer}; cdecl;
{$ENDIF CPUX86}

{ Dynamic method dispatch support }

function GetDynaMethod(vmt: TClass; selector: SmallInt): Pointer;

{ Package/Module registration and unregistration }

type
  PLibModule = ^TLibModule;
  TLibModule = record
    Next: PLibModule;
    Instance: HINST;
    CodeInstance: HINST;
    DataInstance: HINST;
    ResInstance: HINST;
    TypeInfo: PPackageTypeInfo;
    Reserved: NativeInt;
{$IF defined(LINUX) or defined(MACOS)}
    InstanceVar: Pointer platform;
    InitTable: Pointer platform;
    GOT: NativeUInt platform;
{$IFEND LINUX or MACOS}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    CodeSegStart: LongWord platform;
    CodeSegEnd: LongWord platform;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  end;

  TEnumModuleFunc = function (HInstance: NativeInt; Data: Pointer): Boolean;
  TEnumModuleFuncLW = function (HInstance: THandle; Data: Pointer): Boolean;
  TModuleUnloadProc = procedure (HInstance: NativeInt);
  TModuleUnloadProcLW = procedure (HInstance: THandle);

  PModuleUnloadRec = ^TModuleUnloadRec;
  TModuleUnloadRec = record
    Next: PModuleUnloadRec;
    Proc: TModuleUnloadProcLW;
  end;

var
  LibModuleList: PLibModule = nil;
  ModuleUnloadList: PModuleUnloadRec = nil;

procedure RegisterModule(LibModule: PLibModule);
procedure UnregisterModule(LibModule: PLibModule);
function FindHInstance(Address: Pointer): HINST;
function FindClassHInstance(ClassType: TClass): HINST;
function FindResourceHInstance(Instance: HINST): HINST;
{$IFDEF MSWINDOWS}
function GetResourceModuleName(HostAppName, ModuleName: string): string;
{$ENDIF}
function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean = True): LongWord;
procedure EnumModules(Func: TEnumModuleFunc; Data: Pointer); overload;
procedure EnumResourceModules(Func: TEnumModuleFunc; Data: Pointer); overload;
procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer); overload;
procedure EnumResourceModules(Func: TEnumModuleFuncLW; Data: Pointer); overload;
procedure AddModuleUnloadProc(Proc: TModuleUnloadProc); overload;
procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProc); overload;
procedure AddModuleUnloadProc(Proc: TModuleUnloadProcLW); overload;
procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProcLW); overload;
{$IF defined(LINUX) or defined(MACOS)}
{ Given an HMODULE, this function will return its fully qualified name.  There is
  no direct equivalent in Linux so this function provides that capability. }
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
{$IFEND LINUX or MACOS}

{ ResString support function/record }

type
  PResStringRec = ^TResStringRec;
  // 32bit = 8 bytes
  // 64bit = 16 bytes
  TResStringRec = packed record
    Module: ^Cardinal;
    Identifier: NativeUint;
  end;

function LoadResString(ResStringRec: PResStringRec): string;

{ floating number support }
type
  TFloatSpecial = ( fsZero, fsNZero, fsDenormal, fsNDenormal,
    fsPositive, fsNegative, fsInf, fsNInf, fsNaN );

  PSingleRec = ^TSingleRec;
  TSingleRec = packed record
  private
    function GetExp: UInt64; inline;
    function GetFrac: UInt64; inline;
    function GetSign: Boolean; inline;
    procedure SetExp(NewExp: UInt64);
    procedure SetFrac(NewFrac: UInt64);
    procedure SetSign(NewSign: Boolean);
  public
    function Exponent: Integer;
    function Fraction: Extended;
    function Mantissa: UInt64;

    property Sign: Boolean read GetSign write SetSign;
    property Exp: UInt64 read GetExp write SetExp;
    property Frac: UInt64 read GetFrac write SetFrac;

    function SpecialType: TFloatSpecial;
    procedure BuildUp(const SignFlag: Boolean; const Mantissa: UInt64; const Exponent: Integer);
    class operator Explicit(a: Extended): TSingleRec;
    class operator Explicit(a: TSingleRec): Extended;
    case Integer of
    0: (Words: array [0..1] of UInt16);
    1: (Bytes: array[0..3] of UInt8);
  end;

  PDoubleRec = ^TDoubleRec;
  TDoubleRec = packed record
  private
    function GetExp: UInt64; inline;
    function GetFrac: UInt64; inline;
    function GetSign: Boolean; inline;
    procedure SetExp(NewExp: UInt64);
    procedure SetFrac(NewFrac: UInt64);
    procedure SetSign(NewSign: Boolean);
  public
    function Exponent: Integer;
    function Fraction: Extended;
    function Mantissa: UInt64;

    property Sign: Boolean read GetSign write SetSign;
    property Exp: UInt64 read GetExp write SetExp;
    property Frac: UInt64 read GetFrac write SetFrac;

    function SpecialType: TFloatSpecial;
    procedure BuildUp(const SignFlag: Boolean; const Mantissa: UInt64; const Exponent: Integer);
    class operator Explicit(a: Extended): TDoubleRec;
    class operator Explicit(a: TDoubleRec): Extended;
    case Integer of
    0: (Words: array [0..3] of UInt16);
    1: (Bytes: array[0..7] of UInt8);
  end;

  PExtended80Rec = ^TExtended80Rec;
  TExtended80Rec = packed record
  private
    function GetExp: UInt64; inline;
    function GetSign: Boolean; inline;
    procedure SetExp(NewExp: UInt64);
    procedure SetSign(NewSign: Boolean);
  public
    function Exponent: Integer;
    function Fraction: Extended;
    function Mantissa: UInt64;

    property Sign: Boolean read GetSign write SetSign;
    property Exp: UInt64 read GetExp write SetExp;

    function SpecialType: TFloatSpecial;
    procedure BuildUp(const SignFlag: Boolean; const Mantissa: UInt64; const Exponent: Integer);
    class operator Explicit(a: Extended): TExtended80Rec;
    class operator Explicit(a: TExtended80Rec): Extended;
    case Integer of
    0: (Words: array [0..4] of UInt16);
    1: (Bytes: array[0..9] of UInt8);
    2: (Frac: UInt64; _Exp: Uint16;);
  end;

{$IFDEF CPUX86}
  PExtendedRec = PExtended80Rec;
  TExtendedRec = TExtended80Rec;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
  PExtendedRec = PDoubleRec;
  TExtendedRec = TDoubleRec;
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function Int(const X: Double): Double; overload;
function Frac(const X: Double): Double; overload;
function Exp(const X: Double): Double; overload;
function Cos(const X: Double): Double; overload;
function Sin(const X: Double): Double; overload;
function Ln(const X: Double): Double; overload;
function ArcTan(const X: Double): Double; overload;
function Sqrt(const X: Double): Double; overload;

function Tangent(const X: Double): Double; overload;
procedure SineCosine(const X: Double; var Sin, Cos: Double); overload;
function ExpMinus1(const X: Double): Double; overload;
function LnXPlus1(const X: Double): Double; overload;
{$ELSE CPUX86}
function Int(const X: Extended): Extended;
function Frac(const X: Extended): Extended;
function Exp(const X: Extended): Extended;
function Cos(const X: Extended): Extended;
function Sin(const X: Extended): Extended;
function Ln(const X: Extended): Extended;
function ArcTan(const X: Extended): Extended;
function Sqrt(const X: Extended): Extended;

function Tangent(const X: Extended): Extended;
procedure SineCosine(const X: Extended; var Sin, Cos: Extended);
function ExpMinus1(const X: Extended): Extended;
function LnXPlus1(const X: Extended): Extended;
{$ENDIF}

{ Procedures and functions that need compiler magic }

{$IFDEF CPU386}
procedure _ROUND;
procedure _TRUNC;
{$ENDIF}
{$IFDEF CPUX64}  // Int64 types
function _Round(Val: Extended): Int64;
function _RoundCurrency(Val: Currency): Int64;
function _Trunc(Val: Extended): Int64;
function _Abs(Val: Extended): Extended;
{$ENDIF}

procedure _AbstractError;
procedure _Assert(const Message, Filename: string; LineNumber: Integer);
function _Append(var t: TTextRec): Integer;
function _AssignFile(var t: TFileRec; const s: PChar): Integer;
function _AssignText(var t: TTextRec; const s: PChar; const CP: word): Integer;
function _BlockRead(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsRead: Longint): Longint;
function _BlockWrite(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsWritten: Longint): Longint;
function _Close(var t: TTextRec): Integer;
function _EofFile(var f: TFileRec): Boolean;
function _EofText(var t: TTextRec): Boolean;
function _Eoln(var t: TTextRec): Boolean;
procedure _Erase(var f: TFileRec);
{$IFDEF TRIAL_EDITION}
procedure _Expired;
{$ENDIF}
function _FilePos(var f: TFileRec): Longint;
function _FileSize(var f: TFileRec): Longint;
function _Flush(var t: TTextRec): Integer;
procedure _FillChar(var Dest; Count: NativeInt; Value: AnsiChar);
function _FreeMem(P: Pointer): Integer;
function _GetMem(Size: NativeInt): Pointer;
function _ReallocMem(var P: Pointer; NewSize: NativeInt): Pointer;
procedure _Halt(Code: Integer);
procedure _Halt0;
{$IFDEF TRIAL_EDITION}
{$IFDEF MSWINDOWS}
function _InitUnitPrep: Int64;
{$ENDIF}
{$IFDEF LINUX}
function _InitUnitPrep: Integer;
{$ENDIF}
{$ENDIF}
procedure Mark; deprecated;
function _ReadRec(var f: TFileRec; Buffer: Pointer): Integer;
function _ReadChar(var t: TTextRec): AnsiChar;
function _ReadLong(var t: TTextRec): Longint;
procedure _ReadString(var t: TTextRec; s: PShortString; maxLen: Longint);
procedure _ReadCString(var t: TTextRec; s: PAnsiChar; maxLen: Longint);
procedure _ReadLString(var t: TTextRec; var s: AnsiString; CodePage: Word);
procedure _ReadUString(var t: TTextRec; var s: UnicodeString);
procedure _ReadWString(var t: TTextRec; var s: WideString);
procedure _ReadWCString(var t: TTextRec; s: PWideChar; maxBytes: Longint);
function _ReadWChar(var t: TTextRec): WideChar;
function _ReadExt(var t: TTextRec): Extended;
procedure _ReadLn(var t: TTextRec);
procedure _Rename(var f: TFileRec; newName: PChar);
procedure Release; deprecated;
function _ResetText(var t: TTextRec): Integer;
function _ResetFile(var f: TFileRec; recSize: Longint): Integer;
function _RewritText(var t: TTextRec): Integer;
function _RewritFile(var f: TFileRec; recSize: Longint): Integer;
procedure _RunError(errorCode: Byte);
procedure _Run0Error;
procedure _Seek(var f: TFileRec; recNum: Cardinal);
function _SeekEof(var t: TTextRec): Boolean;
function _SeekEoln(var t: TTextRec): Boolean;
procedure _SetTextBuf(var t: TTextRec; p: Pointer; size: Longint);
function _StrLong(val, width: Longint): ShortString;
function _Str0Long(val: Longint): ShortString;

procedure _Truncate(var f: TFileRec);
function _ValLong(const s: string; var code: Integer): Longint;
function _WriteRec(var f: TFileRec; buffer: Pointer): Pointer;
function _WriteChar(var t: TTextRec; c: AnsiChar; width: Integer): Pointer;
function _Write0Char(var t: TTextRec; c: AnsiChar): Pointer;
function _WriteBool(var t: TTextRec; val: Boolean; width: Longint): Pointer;
function _Write0Bool(var t: TTextRec; val: Boolean): Pointer;
function _WriteLong(var t: TTextRec; val, width: Longint): Pointer;
function _Write0Long(var t: TTextRec; val: Longint): Pointer;
function _WriteString(var t: TTextRec; const s: ShortString; width: Longint): Pointer;
function _Write0String(var t: TTextRec; const s: ShortString): Pointer;
function _WriteCString(var t: TTextRec; s: PAnsiChar; width: Longint): Pointer;
function _Write0CString(var t: TTextRec; s: PAnsiChar): Pointer;
function _WriteLString(var t: TTextRec; const s: AnsiString; width: Longint): Pointer;
function _Write0LString(var t: TTextRec; const s: AnsiString): Pointer;
function _WriteWString(var t: TTextRec; const s: WideString; width: Longint): Pointer;
function _Write0WString(var t: TTextRec; const s: WideString): Pointer;
function _WriteWCString(var t: TTextRec; s: PWideChar; width: Longint): Pointer;
function _Write0WCString(var t: TTextRec; s: PWideChar): Pointer;
function _WriteWChar(var t: TTextRec; c: WideChar; width: Integer): Pointer;
function _Write0WChar(var t: TTextRec; c: WideChar): Pointer;
function _WriteVariant(var T: TTextRec; const V: TVarData; Width: Integer): Pointer;
function _Write0Variant(var T: TTextRec; const V: TVarData): Pointer;
{$IFNDEF CPUX86}
function _Write2Ext(var T: TTextRec; val: Extended; Width, Prec: LongInt): Pointer;
function _Write1Ext(var T: TTextRec; val: Extended; Width: LongInt): Pointer;
function _Write0Ext(var T: TTextRec; val: Extended): Pointer;
{$ELSE CPUX86}
// On x86, val is passed on the FPU stack. dcc32 passes floating point
// parameters by reference, so the parameters cannot be added. These
// methods cannot be invoked from pure pascal code
procedure _Write2Ext{(var T: TTextRec; val: Extended; Width, Prec: LongInt): Pointer};
procedure _Write1Ext{(var T: TTextRec; val: Extended; Width: LongInt): Pointer};
procedure _Write0Ext{(var T: TTextRec; val: Extended): Pointer};
{$ENDIF}
{$IFNDEF CPUX86}
function _Write2Comp(var T: TTextRec; Val: Comp; Width, Prec: LongInt): Pointer;
function _Write1Comp(var T: TTextRec; Val: Comp; Width: LongInt): Pointer;
function _Write0Comp(var T: TTextRec; Val: Comp): Pointer;
function _Write2Currency(var T: TTextRec; Val: Currency; Width, Prec: LongInt): Pointer;
function _Write1Currency(var T: TTextRec; Val: Currency; Width: LongInt): Pointer;
function _Write0Currency(var T: TTextRec; Val: Currency): Pointer;
{$ENDIF !CPUX86}
function _WriteLn(var t: TTextRec): Pointer;

var
  AlternateWriteUnicodeStringProc: function(var t: TTextRec; s: UnicodeString): Pointer = nil;

procedure __CToPasStr(Dest: PShortString; const Source: PAnsiChar);
procedure _WCharToString(Dest: PShortString; const Source: WideChar; MaxLen: Integer);
procedure __CLenToPasStr(Dest: PShortString; const Source: PAnsiChar; MaxLen: NativeInt);
procedure __ArrayToPasStr(Dest: PShortString; const Source: PAnsiChar; Len: NativeInt);
procedure __PasToCStr(const Source: PShortString; const Dest: PAnsiChar);

{ Compiler helper for set type support }
{$IFNDEF CPUX86}
procedure _SetElem(var Dest {:Set}; Elem, Size: Integer);
procedure _SetRange(Lo, Hi, Size: Integer; var Dest {:Set});
function _SetEq(L, R: Pointer{PSet}; Size: Integer): Boolean;
function _SetLe(L, R: Pointer{PSet}; Size: Integer): Boolean;
procedure _SetIntersect(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
procedure _SetIntersect3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
procedure _SetUnion(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
procedure _SetUnion3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
procedure _SetSub(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
procedure _SetSub3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
procedure _SetExpand(Src: Pointer{PSet}; var Dest {:Set}; Lo, Hi: Integer);
{$ELSE CPUX86}
// On x86, these functions have special calling conventions that can't be 
// invoked from pure pascal code.
procedure _SetElem;
procedure _SetRange;
procedure _SetEq;
procedure _SetLe;
procedure _SetIntersect;
procedure _SetIntersect3; { BEG only }
procedure _SetUnion;
procedure _SetUnion3; { BEG only }
procedure _SetSub;
procedure _SetSub3; { BEG only }
procedure _SetExpand;
{$ENDIF}

{ Helper routines for standard procedure }
function _Str2Ext(val: Extended; width, precision: LongInt): ShortString;
function _Str1Ext(val: Extended; width: LongInt): ShortString;
function _Str0Ext(val: Extended): ShortString;
{$IFNDEF CPUX86}
function _Str2Comp(Val: Comp; Width, Precision: LongInt): ShortString;
function _Str1Comp(Val: Comp; Width: LongInt): ShortString;
function _Str0Comp(Val: Comp): ShortString;
function _Str2Currency(Val: Currency; Width, Precision: LongInt): ShortString;
function _Str1Currency(Val: Currency; Width: LongInt): ShortString;
function _Str0Currency(Val: Currency): ShortString;
function _Ext80ToDouble(Val: Pointer {PExtended80}): Double;
procedure _DoubleToExt80(Dest: Pointer {PExtended80}; Val: Double);
function _CompDiv(Dividend, Divisor: Comp): Double;
{$ENDIF !CPUX86}

{$IFNDEF CPUX86}
function _ValExt(s: string; var code: Integer): Extended;
function _Pow10(val: Extended; Power: Integer): Extended;
function _Real2Ext(val: Pointer {PReal48}): Extended;
procedure _Ext2Real(Dest: Pointer {PReal48}; Val: Extended);
{$ELSE CPUX86}
// Floating point values are passed on the FPU stack here.
// dcc32 expects them to be by reference arguments as normal 
// parameters. These cannot be invoked from pure pascal code.
procedure _ValExt;
procedure _Pow10;
procedure _Real2Ext;
procedure _Ext2Real;
{$ENDIF CPUX86}

{ Compiler helpers for object type support }
{$IFNDEF CPUX86}
function _ObjSetup(Self: Pointer; var VmtPtrAndAllocFlag: Pointer): Pointer;
procedure _ObjCopy(Dest, Source: Pointer; VmtPtrOffs: LongInt);
{$ELSE CPUX86}
// On x86, it is expected that ZF is set upon failure for these methods
// which cannot be implemented in pure pascal
procedure _ObjSetup;
procedure _ObjCopy;
{$ENDIF CPUX86}
function _Fail(Self: Pointer; AllocFlag: NativeInt): Pointer;
procedure _BoundErr;
procedure _IntOver;

{ Module initialization context.  For internal use only. }

type
  PInitContext = ^TInitContext;
  TInitContext = record
    OuterContext:   PInitContext;     { saved InitContext   }
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    ExcFrame:       Pointer;          { bottom exc handler  }
{$ENDIF}
    InitTable:      PackageInfo;      { unit init info      }
    InitCount:      Integer;          { how far we got      }
    Module:         PLibModule;       { ptr to module desc  }
{$IFNDEF TABLE_BASED_EXCEPTIONS}
    DLLSaveEBP:     Pointer;          { saved regs for DLLs }
    DLLSaveEBX:     Pointer;          { saved regs for DLLs }
    DLLSaveESI:     Pointer;          { saved regs for DLLs }
    DLLSaveEDI:     Pointer;          { saved regs for DLLs }
{$ENDIF !TABLE_BASED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
    ExitProcessTLS: procedure;        { Shutdown for TLS    }
{$ENDIF}
    DLLInitState:   Byte;             { 0 = package, 1 = DLL shutdown, 2 = DLL startup }
    ThreadID:       TThreadID;        { Initializing Thread }
  end platform;

{$IFDEF TABLE_BASED_EXCEPTIONS}
type
  _TExitDllException = class(TObject)
  private
    FExitCode: Integer;
  public
    constructor Create(ExitCode: Integer);
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    property ExitCode: Integer read FExitCode;
  end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

type
  TDLLProc = procedure (Reason: Integer);
  // TDLLProcEx provides the reserved param returned by WinNT
  TDLLProcEx = procedure (Reason: Integer; Reserved: Pointer);

{$IF defined(LINUX) or defined(MACOS)}
procedure _StartExe(InitTable: PackageInfo; Module: PLibModule; Argc: Integer; Argv: Pointer);
procedure _StartLib(Context: PInitContext; Module: PLibModule; DLLProc: TDLLProcEx);
{$IFEND LINUX or MACOS}
{$IFDEF MSWINDOWS}
procedure _StartExe(InitTable: PackageInfo; Module: PLibModule);
{$IFDEF CPUX86}
procedure _StartLib;
{$ENDIF CPUX86}
{$IFDEF CPUX64} // Int64 Types
procedure _StartLib(ContextBuf: PInitContext; InitTable: PackageInfo; Module: PLibModule; TlsProc: Pointer; DllProc: TDllProcEx; AHInst: HINST; Reason: LongWord; Reserved: Pointer);
{$ENDIF}
{$ENDIF MSWINDOWS}
procedure _PackageLoad(const Table : PackageInfo; Module: PLibModule);
procedure _PackageUnload(const Table : PackageInfo; Module: PLibModule);

{$IFDEF MSWINDOWS}
type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: Cardinal;
    ExceptionFlags: Cardinal;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: Cardinal;
    case {IsOsException:} Boolean of
      True:  (ExceptionInformation : array [0..14] of NativeUInt);
      False: (ExceptAddr: Pointer; ExceptObject: Pointer);
  end;
  TExceptClsProc = function(P: PExceptionRecord): Pointer{ExceptClass};
  TExceptObjProc = function(P: PExceptionRecord): Pointer{Exception};
  TRaiseExceptObjProc = procedure(P: PExceptionRecord);

{$IFDEF TABLE_BASED_EXCEPTIONS}
  _PContext = Pointer{^TContext};
  PExceptionPointers = ^TExceptionPointers;
  TExceptionPointers = record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: _PContext;
  end;
  _TDelphiFinallyHandlerProc = function(ExceptionPointers: PExceptionPointers;
                                        EstablisherFrame: NativeUInt): Integer;
  _TExceptionHandlerProc = function(ExceptionPointers: PExceptionPointers;
                                    EstablisherFrame: NativeUInt): Integer;
  _TDelphiSafeCallCatchHandlerProc =
    function(ExceptionPointers: PExceptionPointers;
             EstablisherFrame: NativeUInt; ExceptionObject: Pointer;
             ExceptionAddress: Pointer): NativeUInt;
{$ENDIF TABLE_BASED_EXCEPTIONS}
{$ENDIF MSWINDOWS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
type
  PRaisedException = ^TRaisedException;
  TRaisedException = packed record
    RefCount: Integer;
    ExceptObject: TObject;
    ExceptionAddr: Pointer;
    HandlerEBP: LongWord;
    Flags: LongWord;
    Cleanup: Pointer;
    Prev: PRaisedException;
    ReleaseProc: Pointer;
  end;
  PExceptionRecord = PRaisedException;
  TExceptionRecord = TRaisedException;
{$ENDIF}

procedure _InitResStrings(InitTable: Pointer);
procedure _InitResStringImports(InitTable: Pointer);
procedure _InitImports(InitTable: Pointer);
{$IFDEF MSWINDOWS}
procedure _InitWideStrings(InitTable: Pointer);
{$ENDIF}

function _ClassCreate(InstanceOrVMT: Pointer; Alloc: ShortInt): TObject;
{$IFNDEF CPUX86}
procedure _BeforeDestruction(Instance: TObject; OuterMost: ShortInt);
{$ELSE CPUX86}
{ dcc32 generated code depends on DL being preserved through the chain of 
  calls up the inherited dtor chain.  Thus a PUREPASCAL implementation 
  leads to non-deterministic results at destruction time with the current
  code generator.}
function _BeforeDestruction(Instance: TObject; OuterMost: ShortInt): TObject;
{$ENDIF CPUX86}
procedure _ClassDestroy(Instance: TObject);
function _AfterConstruction(Instance: TObject): TObject;
function _IsClass(Child: TObject; Parent: TClass): Boolean;
function _AsClass(Child: TObject; Parent: TClass): TObject;
function _GetHelperIntf(Instance: TObject; HelperClass: TClass): IInterface;
function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;

{ Exception Support }

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _RaiseAtExcept;
procedure _RaiseExcept;
procedure _RaiseAgain;
procedure _DestroyException;
procedure _DoneExcept;
procedure _HandleAnyException;
procedure _HandleAutoException;
procedure _HandleOnException;
procedure _HandleFinally;
procedure _HandleOnExceptionPIC;
procedure _ClassHandleException;
procedure _UnhandledException;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF STACK_BASED_EXCEPTIONS}
procedure _RaiseExcept;
procedure _RaiseAgain;
procedure _DoneExcept;
procedure _TryFinallyExit;
procedure _HandleAnyException;
procedure _HandleAutoException;
procedure _HandleOnException;
procedure _HandleFinally;
procedure _UnhandledException;
{$ENDIF STACK_BASED_EXCEPTIONS}

{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure _RaiseExcept(Obj: TObject);
procedure _RaiseAtExcept(Obj: TObject; Address: Pointer);
procedure _RaiseAgain;
procedure _DestroyException(ExceptionPointers: PExceptionPointers; EstablisherFrame: NativeUInt);
procedure _DoneExcept;
procedure _TryFinallyExit(EstablisherFrame: NativeUInt; TargetAddr: NativeUInt);
procedure _UnhandledException;
function _HandleExitDllException: Integer;
function _DelphiExceptionHandler(ExceptionRecord: PExceptionRecord;
  EstablisherFrame: NativeUInt; ContextRecord: _PContext;
  DispatcherContext: Pointer{PDispatcherContext}):
  LongInt{TExceptionDisposition}; stdcall;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{ Class Support }
function _FindDynaInst(Self: TObject; Selector: SmallInt): Pointer;
function _FindDynaClass(Vmt: TClass; Selector: SmallInt): Pointer;
{$IFDEF CPUX86}
// These functions use ESI as the Selector, which doesn't map to any
// of dcc32's known calling conventions. These cannot be called from
// pure pascal code.
procedure _CallDynaInst;
procedure _CallDynaClass;
{$ENDIF CPUX86}

{ String Support }

{ Compiler helper for string allocation and release }
function _NewUnicodeString(CharLength: LongInt): Pointer;
function _NewAnsiString(CharLength: LongInt; CodePage: Word): Pointer;
function _NewWideString(CharLength: LongInt): Pointer;
{$IFDEF CPUX64}
// dcc64 generated code expects the result to remain in RAX,
// so we have to return S
function _UStrClr(var S): Pointer;
function _LStrClr(var S): Pointer;
function _WStrClr(var S): Pointer;
{$ELSE !CPUX64}
procedure _UStrClr(var S);
procedure _LStrClr(var S);
procedure _WStrClr(var S);
{$ENDIF !CPUX64}
procedure _UStrArrayClr(var StrArray; Count: Integer);
procedure _LStrArrayClr(var StrArray; Count: Integer);
procedure _WStrArrayClr(var StrArray; Count: Integer);
function _UStrAddRef(Str: Pointer): Pointer;
function _LStrAddRef(Str: Pointer): Pointer;
{$IFDEF MSWINDOWS}
function _WStrAddRef(var Str: WideString): Pointer;
{$ELSE}
function _WStrAddRef(Str: Pointer): Pointer;
{$ENDIF}

{ Compiler helper for basic string constructors }
procedure _UStrFromPWCharLen(var Dest: UnicodeString; Source: PWideChar; CharLength: Integer);
procedure _WStrFromPWCharLen(var Dest: WideString; Source: PWideChar; CharLength: Integer);
procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
//procedure InternalUStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
procedure _UStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
//procedure InternalWStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
procedure _WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
procedure _LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);

{ Compiler helper for string assignment }
procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrLAsg(var Dest: UnicodeString; const Source: UnicodeString);
procedure _WStrAsg(var Dest: WideString; const Source: WideString);
procedure _WStrLAsg(var Dest: WideString; const Source: WideString);
procedure _LStrAsg(var Dest: AnsiString; const Source: AnsiString);
procedure _LStrLAsg(var Dest: AnsiString; const Source: AnsiString);

{ string info utilities }
function StringElementSize(const S: UnicodeString): Word; overload; inline;
function StringElementSize(const S: RawByteString): Word; overload; inline;
function StringCodePage(const S: UnicodeString): Word; overload; inline;
function StringCodePage(const S: RawByteString): Word; overload; inline;
function StringRefCount(const S: UnicodeString): Integer; overload; inline;
function StringRefCount(const S: RawByteString): Integer; overload; inline;
{$IFNDEF MSWINDOWS}
function StringElementSize(const S: WideString): Word; overload; inline;
function StringCodePage(const S: WideString): Word; overload; inline;
function StringRefCount(const S: WideString): Integer; overload; inline;
{$ENDIF}

{ Compiler helper for string length }
function _UStrLen(const S: UnicodeString): Integer; inline;
function _WStrLen(const S: WideString): Longint; inline;
// Note: VCLE.LIB|dstring.cpp refers mangled names of _LStrLen
function _LStrLen(const S: AnsiString): Longint; inline;
//function _PStrLen(const S: ShortString): Integer; inline;
function _PCharLen(P: PAnsiChar): Integer;
function _PWCharLen(P: PWideChar): Integer;

{ Compiler helper for _UniqueString* functions }
function _UniqueStringU(var Str: UnicodeString): Pointer;
{$IFNDEF MSWINDOWS}
function _UniqueStringW(var Str: WideString): Pointer;
{$ENDIF}
function _UniqueStringA(var Str: AnsiString): Pointer;

{ UniqueString* functions }
procedure UniqueString(var Str: UnicodeString); overload;
procedure UniqueString(var Str: WideString); overload;
procedure UniqueString(var Str: AnsiString); overload;

{ Compiler helper for comparing array of characters }
{$IFNDEF CPUX86}
function _PStrCmp(const Left, Right: ShortString): Integer;
function _AStrCmp(const Left, Right: PAnsiChar; Len: NativeInt): Integer;
function _WStrLCmp(const Left, Right: PWideChar; Len: NativeInt): Integer;
{$ELSE CPUX86}
// On dcc32, these return their result in the flag register rather
// than a typical return value.
// They cannot be invoked from pure pascal code.
procedure _PStrCmp;
procedure _AStrCmp;
procedure _WStrLCmp;
{$ENDIF CPUX86}

{ Compiler helper for ShortString support }
procedure _PStrCpy(Dest: PShortString; Source: PShortString);
procedure _PStrNCpy(Dest: PShortString; Source: PShortString; MaxLen: Byte);
procedure _PStrCat(Dest: PShortString; const Src: ShortString);
procedure _PStrNCat(Dest: PShortString; const Src: ShortString; Size:Integer);

function _Copy(const S: ShortString; Index, Count: Integer): ShortString;
{$IFDEF CPUX64}
// var ShortString is actually two parameters, Pointer to the string and size of the string.
procedure _Delete(var s: ShortString; Index, Count: Integer);
{$ELSE CPUX86}
procedure _Delete(s: PShortString; Index, Count: Integer);
{$ENDIF}
procedure _Insert(const Source: ShortString; var S: OpenString; Index: Integer);
procedure _SetLength(s: PShortString; newLength: Byte);
procedure _SetString(s: PShortString; buffer: PAnsiChar; len: Byte);

{ Compiler helper for AnsiString support }
// Note: VCLE.LIB|dstring.cpp refers mangled names of _LStrFromArray, _LStrFromWStr, _LStrFromUStr, _LStrFromPChar and _LStrFromPWChar
function _LStrToPChar(const S: AnsiString): PAnsiChar;
procedure _LStrToString(Dest: PShortString; const Source: AnsiString; MaxLen: Integer);
procedure _LStrFromChar(var Dest: AnsiString; Source: AnsiChar; CodePage: Word);
procedure _LStrFromWChar(var Dest: AnsiString; Source: WideChar; CodePage: Word);
procedure _LStrFromPChar(var Dest: AnsiString; Source: PAnsiChar; CodePage: Word);
procedure _LStrFromPWChar(var Dest: AnsiString; Source: PWideChar; CodePage: Word);
procedure _LStrFromArray(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
procedure _LStrFromWArray(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure _LStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word);
procedure _LStrFromWStr(var Dest: AnsiString; const Source: WideString; CodePage: Word);
procedure _LStrFromString(var Dest: AnsiString; const Source: ShortString; CodePage: Word);
procedure _LStrCat(var Dest: AnsiString; const Source: AnsiString);
procedure _LStrCat3(var Dest: AnsiString; const Source1, Source2: AnsiString);

{$IFNDEF CPUX86}
procedure _LStrCatN(var Dest: AnsiString; ArgCnt: Integer; const Strs: AnsiString); varargs;
function _LStrCmp(const Left, Right: AnsiString): Integer;
function _LStrEqual(const Left, Right: AnsiString): Integer;
{$ELSE CPUX86}
// dcc32 cannot handle varargs. This function cannot be invoked 
// from pure pascal code.
procedure _LStrCatN{var dest:AnsiString; argCnt: Integer; ...};
// These functions return their result in the flags register. They
// cannot be invoked from pure pascal code.
procedure _LStrCmp{left: AnsiString; right: AnsiString};
procedure _LStrEqual{const Left, Right: AnsiString};
{$ENDIF CPUX86}
function _LStrCopy(const S: AnsiString; Index, Count: Integer): AnsiString;
procedure _LStrDelete(var S: AnsiString; Index, Count: Integer);
procedure _LStrInsert(const Source: AnsiString; var S: AnsiString; Index: Integer);
procedure _LStrSetLength(var Str: AnsiString; NewLength: Integer; CodePage: Word);

{ Compiler helper for WideString support }
function _WStrToPWChar(const S: WideString): PWideChar;
procedure _WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
procedure _WStrFromChar(var Dest: WideString; Source: AnsiChar);
procedure _WStrFromWChar(var Dest: WideString; Source: WideChar);
procedure _WStrFromPChar(var Dest: WideString; Source: PAnsiChar);
procedure _WStrFromPWChar(var Dest: WideString; Source: PWideChar);
procedure _WStrFromArray(var Dest: WideString; Source: PAnsiChar; Length: Integer);
procedure _WStrFromWArray(var Dest: WideString; Source: PWideChar; Length: Integer);
procedure _WStrFromLStr(var Dest: WideString; const Source: AnsiString);
procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
procedure _WStrFromString(var Dest: WideString; const Source: ShortString);
procedure _WStrCat(var Dest: WideString; const Source: WideString);
procedure _WStrCat3(var Dest: WideString; const Source1, Source2: WideString);

{$IFNDEF CPUX86}
procedure _WStrCatN(var Dest: WideString; ArgCnt: Integer; const Strs: WideString); varargs;
function _WStrCmp(const Left, Right: WideString): Integer;
function _WStrEqual(const Left, Right: WideString): Integer;
{$ELSE CPUX86}
// dcc32 cannot handle varargs. This function cannot be invoked 
// from pure pascal code.
procedure _WStrCatN{var dest:WideString; argCnt: Integer; ...};
// These functions return their result in the flags register. They
// cannot be invoked from pure pascal code.
procedure _WStrCmp{left: WideString; right: WideString};
procedure _WStrEqual{const Left, Right: WideString};
{$ENDIF CPUX86}
function _WStrCopy(const S: WideString; Index, Count: Integer): WideString;
procedure _WStrDelete(var S: WideString; Index, Count: Integer);
procedure _WStrInsert(const Source: WideString; var Dest: WideString; Index: Integer);
procedure _WStrSetLength(var S: WideString; NewLength: Integer);

{ Compiler helper for UnicodeString support }
function _UStrToPWChar(const S: UnicodeString): PWideChar;
procedure _UStrToString(Dest: PShortString; const Source: UnicodeString; MaxLen: Integer);
procedure _UStrFromChar(var Dest: UnicodeString; Source: AnsiChar);
procedure _UStrFromWChar(var Dest: UnicodeString; Source: WideChar);
procedure _UStrFromPChar(var Dest: UnicodeString; Source: PAnsiChar);
procedure _UStrFromPWChar(var Dest: UnicodeString; Source: PWideChar);
procedure _UStrFromArray(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
procedure _UStrFromWArray(var Dest: UnicodeString; Source: PWideChar; Length: Integer);
procedure _UStrFromLStr(var Dest: UnicodeString; const Source: AnsiString);
procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
procedure _UStrFromString(var Dest: UnicodeString; const Source: ShortString);
procedure _UStrCat(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrCat3(var Dest: UnicodeString; const Source1, Source2: UnicodeString);

{$IFNDEF CPUX86}
procedure _UStrCatN(var Dest: UnicodeString; ArgCnt: Integer; const Strs: UnicodeString); varargs;
function _UStrCmp(const Left, Right: UnicodeString): Integer;
function _UStrEqual(const Left, Right: UnicodeString): Integer;
{$ELSE CPUX86}
// dcc32 cannot handle varargs. This function cannot be invoked 
// from pure pascal code.
procedure _UStrCatN{var dest:UnicodeString; argCnt: Integer; ...};
// These functions return their result in the flags register. They
// cannot be invoked from pure pascal code.
procedure _UStrCmp{const Left, Right: UnicodeString};
procedure _UStrEqual{const Left, Right: UnicodeString};
{$ENDIF CPUX86}
function _UStrCopy(const S: UnicodeString; Index, Count: Integer): UnicodeString;
procedure _UStrDelete(var S: UnicodeString; Index, Count: Integer);
procedure _UStrInsert(const Source: UnicodeString; var Dest: UnicodeString; Index: Integer);
procedure _UStrSetLength(var Str: UnicodeString; NewLength: Integer);

{ string utilities }
function Pos(const SubStr, Str: ShortString): Integer; overload;
function Pos(const SubStr, Str: UnicodeString): Integer; overload;
function Pos(const SubStr, Str: WideString): Integer; overload;
function Pos(const SubStr, Str: RawByteString): Integer; overload;
function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
function StringOfChar(Ch: AnsiChar; Count: Integer): AnsiString; overload;
procedure SetAnsiString(Dest: PAnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure SetCodePage(var S: RawByteString; CodePage: Word; Convert: Boolean = True);
function UnicodeStringToUCS4String(const S: UnicodeString): UCS4String;
function UCS4StringToUnicodeString(const S: UCS4String): UnicodeString;
function WideCharToUCS4String(S: PWideChar; Len: Integer = MaxInt): UCS4String;
//function UTF8Encode(const WS: UnicodeString): UTF8String;
//function UTF8Decode(const S: UTF8String): UnicodeString;

function _Write0UString(var t: TTextRec; const s: UnicodeString): Pointer;
function _WriteUString(var t: TTextRec; const s: UnicodeString; width: Longint): Pointer;


{ Compiler helper for initializing/finalizing variable }
procedure InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: NativeUInt);
procedure _Initialize(p: Pointer; typeInfo: Pointer);
procedure _InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: NativeUInt);
procedure _InitializeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF CPUX64}
// dcc64 generated code expects P to remain in RAX on exit from this function.
function _Finalize(P: Pointer; TypeInfo: Pointer): Pointer;
function _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: NativeUInt): Pointer;
function _FinalizeRecord(P: Pointer; TypeInfo: Pointer): Pointer;
{$ELSE !CPUX64}
procedure _Finalize(p: Pointer; typeInfo: Pointer);
procedure _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: NativeUInt);
procedure _FinalizeRecord(P: Pointer; TypeInfo: Pointer);
{$ENDIF !CPUX64}
{$IFNDEF CPUX86}
procedure _CopyRecord(Dest, Source, TypeInfo: Pointer);
procedure _CopyObject(Dest, Source: Pointer; vmtPtrOffs: LongInt; TypeInfo: Pointer);
procedure _CopyArray(Dest, Source, TypeInfo: Pointer; Count: NativeUInt);
{$ELSE CPUX86}
// The asm implementation of these functions uses EBP as a general
// purpose register, and assumes a frame is not added. Adding parameters
// adds the frame, so in order to define the parameters for these functions,
// the ASM needs to be updated.
procedure _CopyRecord;
procedure _CopyObject;
procedure _CopyArray;
{$ENDIF CPUX86}
procedure _AddRef(P: Pointer; TypeInfo: Pointer);
procedure _AddRefArray(P: Pointer; TypeInfo: Pointer; ElemCount: NativeUInt);
procedure _AddRefRecord(P: Pointer; TypeInfo: Pointer);

function _New(Size: NativeInt; TypeInfo: Pointer): Pointer;
procedure _Dispose(P: Pointer; TypeInfo: Pointer);

procedure CopyArray(Dest, Source, TypeInfo: Pointer; Count: NativeInt);
procedure FinalizeArray(P: Pointer; TypeInfo: Pointer; Count: NativeUInt);


{ 64-bit Integer helper routines }
{$IF defined(CPU386)}
procedure __llmul;
procedure __lldiv;
procedure __lludiv;
procedure __llmod;
procedure __llmulo;
procedure __lldivo;
procedure __llmodo;
procedure __llumod;
procedure __llshl;
procedure __llushr;
{$IFEND}
function _WriteInt64(var t: TTextRec; val: Int64; width: LongInt): Pointer;
function _Write0Int64(var t: TTextRec; val: Int64): Pointer;
function _WriteUInt64(var t: TTextRec; val: UInt64; width: LongInt): Pointer;
function _Write0UInt64(var t: TTextRec; val: UInt64): Pointer;
function _ReadInt64(var t: TTextRec): Int64;
function _StrInt64(val: Int64; width: Integer): ShortString;
function _Str0Int64(val: Int64): ShortString;
function _ValInt64(const s: string; var code: Integer): Int64;
function _StrUInt64(val: UInt64; width: Integer): ShortString;
function _Str0UInt64(val: Int64): ShortString;


{ Compiler helper for Dynamic array support }

function _DynArrayLength(const A: Pointer): NativeInt;
function _DynArrayHigh(const A: Pointer): NativeInt;

procedure DynArrayClear(var A: Pointer; TypeInfo: Pointer);

procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: NativeInt; lengthVec: PNativeInt);

{$IFNDEF CPUX86}
procedure _DynArraySetLength(var A: Pointer; TypeInfo: Pointer; DimCnt: NativeInt; LengthVec: NativeInt); varargs;
{$ELSE CPUX86}
// dcc32 does not support varargs params. This cannot be called from 
// pure pascal code
procedure _DynArraySetLength;
{$ENDIF CPUX86}

{$IFDEF CPUX86}
procedure _DynArrayCopy(a: Pointer; typeInfo: Pointer; var Result: Pointer);
procedure _DynArrayCopyRange(A: Pointer; TypeInfo: Pointer; Index, Count : Integer; var Result: Pointer);
{$ELSE !CPUX86}
procedure _DynArrayCopy(var Result: Pointer; a: Pointer; typeInfo: Pointer);
procedure _DynArrayCopyRange(var Result: Pointer; A: Pointer; TypeInfo: Pointer; Index, Count: NativeInt);
{$ENDIF CPUX86}

{$IFDEF CPUX64}
// dcc64 expects RAX to maintain a pointer to A on return, so we must return it
function _DynArrayClear(var A: Pointer; TypeInfo: Pointer): Pointer;
{$ELSE !CPUX64}
procedure _DynArrayClear(var a: Pointer; typeInfo: Pointer);
{$ENDIF !CPUX64}
procedure _DynArrayAsg(var Dest: Pointer; Src: Pointer; TypeInfo: Pointer);
procedure _DynArrayAddRef(P: Pointer);

function DynArrayIndex(P: Pointer; const Indices: array of NativeInt; TypInfo: Pointer): Pointer; overload;
function DynArrayIndex(P: Pointer; const Indices: array of Integer; TypInfo: Pointer): Pointer; overload;

function DynArrayDim(typeInfo: Pointer): Integer;

function DynArraySize(A: Pointer): NativeInt;

function IsDynArrayRectangular(const DynArray: Pointer; typeInfo: Pointer): Boolean;
function DynArrayBounds(const DynArray: Pointer; typeInfo: Pointer): TBoundArray;

function _IntfClear(var Dest: IInterface): Pointer;
procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
procedure _IntfAddRef(const Dest: IInterface);

{$IFDEF WIN32}
procedure _FSafeDivide;
procedure _FSafeDivideR;
{$ENDIF}

function _CheckAutoResult(ResultCode: HResult): HResult;

{$IFNDEF CPUX86}
function FPower10(val: Extended; power: Integer): Extended; deprecated 'Use Power10';
{$ELSE CPUX86}
// Publically accessable function that cannot be called propperly
// from pure pascal code deprecated.
procedure FPower10; deprecated 'Use Power10';
{$ENDIF CPUX86}
function Power10(val: Extended; power: Integer): Extended;

procedure TextStart; deprecated;

// Conversion utility routines for C++ convenience.  Not for Delphi code.
function  CompToDouble(Value: Comp): Double; cdecl;
procedure DoubleToComp(Value: Double; var Result: Comp); cdecl;
function  CompToCurrency(Value: Comp): Currency; cdecl;
procedure CurrencyToComp(Value: Currency; var Result: Comp); cdecl;

function GetMemory(Size: NativeInt): Pointer; cdecl;
function FreeMemory(P: Pointer): Integer; cdecl;
function ReallocMemory(P: Pointer; Size: NativeInt): Pointer; cdecl;


{ Internal runtime error codes }

type
  TRuntimeError = (reNone, reOutOfMemory, reInvalidPtr, reDivByZero,
  reRangeError, reIntOverflow, reInvalidOp, reZeroDivide, reOverflow,
  reUnderflow, reInvalidCast, reAccessViolation, rePrivInstruction,
  reControlBreak, reStackOverflow,
  { reVar* used in Variants.pas }
  reVarTypeCast, reVarInvalidOp,
  reVarDispatch, reVarArrayCreate, reVarNotArray, reVarArrayBounds,
  reAssertionFailed,
  reExternalException, { not used here; in SysUtils }
  reIntfCastError, reSafeCallError,
  reMonitorNotLocked, reNoMonitorSupport
{$IF defined(LINUX) or defined(MACOS)}
  , reQuit
{$IFEND LINUX or MACOS}
{$IFDEF POSIX}
  , reCodesetConversion
{$ENDIF POSIX}
  , rePlatformNotImplemented
  );
{$NODEFINE TRuntimeError}

procedure Error(errorCode: TRuntimeError);
{$NODEFINE Error}

{ GetLastError returns the last error reported by an OS API call.  Calling
  this function usually resets the OS error state.
}

function GetLastError: Integer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
{$EXTERNALSYM GetLastError}

{ SetLastError writes to the thread local storage area read by GetLastError. }

procedure SetLastError(ErrorCode: Integer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF}

{$IF defined(LINUX) or defined(MACOS)}
{  To improve performance, some RTL routines cache module handles and data
   derived from modules.  If an application dynamically loads and unloads
   shared object libraries, packages, or resource packages, it is possible for
   the handle of the newly loaded module to match the handle of a recently
   unloaded module.  The resource caches have no way to detect when this happens.

   To address this issue, the RTL maintains an internal counter that is
   incremented every time a module is loaded or unloaded using RTL functions
   (like LoadPackage).  This provides a cache version level signature that
   can detect when modules have been cycled but have the same handle.

   If you load or unload modules "by hand" using dlopen or dlclose, you must call
   InvalidateModuleCache after each load or unload so that the RTL module handle
   caches will refresh themselves properly the next time they are used.  This is
   especially important if you manually tinker with the LibModuleList list of
   loaded modules, or manually add or remove resource modules in the nodes
   of that list.

   ModuleCacheID returns the "current generation" or version number kept by
   the RTL.  You can use this to implement your own refresh-on-next-use
   (passive) module handle caches as the RTL does.  The value changes each
   time InvalidateModuleCache is called.
}

function ModuleCacheID: Cardinal;
procedure InvalidateModuleCache;
{$IFEND LINUX or MACOS}

procedure SetMultiByteConversionCodePage(CodePage: Integer);

function GetUILanguages(const LANGID: WORD): string;
function GetLocaleOverride(const AppName: string): string;
procedure SetLocaleOverride(const NewPreferredLanguages: string);

type
  PLongBool = ^LongBool;

{ LocaleCharsFromUnicode is a cross-platform wrapper for WideCharToMultiByte
  with an emulated implemention on non-Windows platforms. The Flags parameter
  isn't supported on non-Windows platforms }

function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;

{$IFDEF POSIX}
function LocaleCharsFromUnicode(const LocaleName: AnsiString; Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
{$ENDIF}

{ UnicodeFromLocaleChars is a cross-platform wrapper for MultiByteToWideChar
  with an emulated implemention on non-Windows platforms. The Flags parameter
  only supports MB_ERR_INVALID_CHARS on non-Windows platforms }

function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer; overload;

{$IFDEF POSIX}
function UnicodeFromLocaleChars(const LocaleName: AnsiString; Flags: Cardinal;
  LocaleStr: PAnsiChar; LocaleStrLen: Integer; UnicodeStr: PWideChar;
  UnicodeStrLen: Integer): Integer; overload;
{$ENDIF}

{$IFDEF POSIX}
{ GetACP returns the equivalent Windows codepage identifier based on the
  current LANG environment variable (Linux) or CFLocaleGetIdentifier (Mac OS) }

function GetACP: Cardinal;

{ Compatibility consts for LocaleCharsFromUnicode/UnicodeFromLocaleChars }

const
  CP_ACP  = 0;
  CP_UTF7 = 65000;
  CP_UTF8 = 65001;

{$ENDIF}


(* =================================================================== *)

implementation

uses
  SysInit;

{$IFDEF POSIX}
{$I PosixAPIs.inc}
{$ENDIF POSIX}

{$IFDEF MACOS}
{$I CoreServicesAPIs.inc}
{$I CoreFoundationAPIs.inc}
{$ENDIF MACOS}

{$I InterlockedAPIs.inc}

{$IFDEF LINUX}
const
  MAX_PATH = 1024;
{$ENDIF LINUX}

{$IFDEF MACOS}
const
  MAX_PATH = 1024;
{$ENDIF MACOS}

type
  PStrRec = ^StrRec;
  StrRec = packed record
  {$IF defined(CPUX64)}
    _Padding: LongInt; // Make 16 byte align for payload..
  {$IFEND}
    codePage: Word;
    elemSize: Word;
    refCnt: Longint;
    length: Longint;
  end;

type
  PMethRec = ^MethRec;
  MethRec = packed record
    recSize: Word;
    methAddr: Pointer;
    nameLen: Byte;
    { nameChars[nameLen]: AnsiChar }
  end;

const
  skew = SizeOf(StrRec);
  rOff = SizeOf(StrRec); { codePage offset }
  overHead = SizeOf(StrRec) + SizeOf(Char);
  CP_UTF16 = 1200;
{$IFDEF MSWINDOWS}
  CP_ACP  = 0;
  CP_UTF8 = 65001;
{$ENDIF MSWINDOWS}

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$IFDEF CPUX64}
    _Padding: LongInt; // Make 16 byte align for payload..
  {$ENDIF}
    RefCnt: LongInt;
    Length: NativeInt;
  end;

const
  STATUS_WAIT_0 = Cardinal($00000000);
  WAIT_OBJECT_0 = (STATUS_WAIT_0 + 0);
  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';

{ This procedure should be at the very beginning of the }
{ text segment. It used to be used by _RunError to find    }
{ start address of the text segment, but is not used anymore.  }

procedure TextStart;
begin
end;

{$IFDEF PIC}
function GetGOT: Pointer; export;
begin
  asm
        MOV Result,EBX
  end;
end;
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS}
var
  Unwinder: TUnwinder;

const
  UNWINDFI_TOPOFSTACK =   $BE00EF00;
  
type
  UNWINDPROC = Pointer;
   
const
{$IFDEF MSWINDOWS}
  unwind = 'unwind.dll';
  {$IFDEF CPUX86}
  _PU = '_';
  {$ELSE !CPUX86}
  _PU = '';
  {$ENDIF !CPUX86}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  unwind = 'libcgunwind.so.1';
{$ENDIF LINUX}
{$IFDEF MACOS}
  unwind = 'libcgunwind.1.0.dylib';
{$ENDIF MACOS}

function _BorUnwind_RegisterIPLookup(fn: Pointer; StartAddr, EndAddr: LongInt;
  Context: Pointer; GOT: LongInt): LongBool; cdecl; external 
  {$IFNDEF STATIC_UNWIND} unwind name _PU + '_BorUnwind_RegisterIPLookup'{$ENDIF STATIC_UNWIND};

procedure _BorUnwind_UnregisterIPLookup(StartAddr: LongInt); cdecl; external
  {$IFNDEF STATIC_UNWIND} unwind name _PU + '_BorUnwind_UnregisterIPLookup'{$ENDIF STATIC_UNWIND};

function _BorUnwind_DelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl; external
  {$IFNDEF STATIC_UNWIND} unwind name _PU + '_BorUnwind_DelphiLookup'{$ENDIF STATIC_UNWIND};

function _BorUnwind_RaiseException(Exc: Pointer): LongBool; cdecl; external
  {$IFNDEF STATIC_UNWIND} unwind name _PU + '_BorUnwind_RaiseException'{$ENDIF STATIC_UNWIND};

function _BorUnwind_ClosestDelphiHandler(Context: Pointer): LongWord; cdecl; external
  {$IFNDEF STATIC_UNWIND} unwind name _PU + '_BorUnwind_ClosestDelphiHandler'{$ENDIF STATIC_UNWIND};

{$IFDEF STATIC_UNWIND}
{$IFDEF PIC}
{$L 'objs/arith.pic.o'}
{$L 'objs/diag.pic.o'}
{$L 'objs/delphiuw.pic.o'}
{$L 'objs/unwind.pic.o'}
{$ELSE !PIC}
{$L 'objs/arith.o'}
{$L 'objs/diag.o'}
{$L 'objs/delphiuw.o'}
{$L 'objs/unwind.o'}
{$ENDIF}
{$ENDIF STATIC_UNWIND}
{$ENDIF PC_MAPPED_EXCEPTIONS}

const { copied from xx.h }
  cContinuable        = 0;
  cNonContinuable     = 1;
  cUnwinding          = 2;
  cUnwindingForExit   = 4;
  cUnwindInProgress   = cUnwinding or cUnwindingForExit;
  cDelphiException    = $0EEDFADE;
  cDelphiReRaise      = $0EEDFADF;
  cDelphiExcept       = $0EEDFAE0;
  cDelphiFinally      = $0EEDFAE1;
  cDelphiTerminate    = $0EEDFAE2;
  cDelphiUnhandled    = $0EEDFAE3;
  cNonDelphiException = $0EEDFAE4;
  cDelphiExitFinally  = $0EEDFAE5;
  cCppException       = $0EEFFACE; { used by BCB }
  EXCEPTION_CONTINUE_SEARCH    = 0;
  EXCEPTION_EXECUTE_HANDLER    = 1;
  EXCEPTION_CONTINUE_EXECUTION = -1;

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  excIsBeingHandled     = $00000001;
  excIsBeingReRaised    = $00000002;
{$ENDIF}

{$IF defined(CPU386) and not defined(PC_MAPPED_EXCEPTIONS)}
type
  JmpInstruction =
  packed record
    opCode:   Byte;
    distance: Longint;
  end;
{$IFEND CPU386 and !PC_MAPPED_EXCEPTIONS}

{$IFDEF CPU386}
type
  PExcDescEntry = ^TExcDescEntry;
  TExcDescEntry = record
    vTable:  Pointer;
    handler: Pointer;
  end;
  PExcDesc = ^TExcDesc;
  TExcDesc = packed record
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    jmp: JmpInstruction;
{$ENDIF}
    case Integer of
    0:      (instructions: array [0..0] of Byte);
    1{...}: (cnt: Integer; excTab: array [0..0{cnt-1}] of TExcDescEntry);
  end;
{$ENDIF}

{$IFDEF TABLE_BASED_EXCEPTIONS}
// Language specific exception data
type
  PExcDescEntry = ^TExcDescEntry;
  TExcDescEntry = record
    VTable:  LongWord; // 32 bit RVA
    Handler: LongWord; // 32 bit RVA
  end;
  PExcDesc = ^TExcDesc;
  TExcDesc = record
    DescCount: Integer;
    DescTable: array [0..0{DescCount-1}] of TExcDescEntry;
  end;
  PExcScope = ^TExcScope;
  TExcScope = record
    BeginOffset:  LongWord;  // 32 bit RVA
    EndOffset:    LongWord;  // 32 bit RVA
    TableOffset:  LongWord;  // 32 bit RVA. 0:TargetOffset=finally block
                             //             1:TargetOffset=safecall catch block
                             //             2:TargetOffset=catch block
                             //             other:TableOffset=TExcDesc
    TargetOffset: LongWord;  // 32 bit RVA. start of finally/catch block.
                             //   TableOffset=0: signature is _TDelphiFinallyHandlerProc
                             //   TableOffset=1: signature is _TDelphiSafeCallCatchHandlerProc
                             //   TableOffset=2: Location to the catch block
                             //   other: TargetOffset=0
  end;
  PExcData = ^TExcData;
  TExcData = record
    ScopeCount: Integer;
    ScopeTable: array [0..0{ScopeCount-1}] of TExcScope;
  end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  UW_EXC_CLASS_BORLANDCPP = $FBEE0001;
  UW_EXC_CLASS_BORLANDDELPHI = $FBEE0101;

type
  // The following _Unwind_* types represent unwind.h
  _Unwind_Word = NativeUInt;
  _Unwind_Exception_Cleanup_Fn = Pointer;
  _Unwind_Exception = packed record
    exception_class: _Unwind_Word;
    exception_cleanup: _Unwind_Exception_Cleanup_Fn;
    private_1: _Unwind_Word;
    private_2: _Unwind_Word;
  end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF STACK_BASED_EXCEPTIONS}
type
  PExcFrame = ^TExcFrame;
  TExcFrame = record
    next: PExcFrame;
    desc: PExcDesc;
    hEBP: Pointer;
    case Integer of
    0:  ( );
    1:  ( ConstructedObject: Pointer );
    2:  ( SelfOfMethod: Pointer );
  end;

  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = packed record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
  end;
  PPRaiseFrame = ^PRaiseFrame;
{$ENDIF TABLE_BASED_EXCEPTIONS}

const
  cCR = $0D;
  cLF = $0A;
  cEOF = $1A;

{$IFDEF POSIX}
function GetLastError: Integer;
begin
  Result := __error^;
end;

procedure SetLastError(ErrorCode: Integer);
begin
  __error^ := ErrorCode;
end;
{$ENDIF POSIX}

{$IF defined(LINUX) or defined(MACOS)}
var
  ModuleCacheVersion: Cardinal = 0;

function ModuleCacheID: Cardinal;
begin
  Result := ModuleCacheVersion;
end;

procedure InvalidateModuleCache;
begin
  InterlockedIncrement(Integer(ModuleCacheVersion));
end;
{$IFEND LINUX or MACOS}

{$IFDEF MSWINDOWS}
{$I WindowsAPIs.INC}

function GetCmdShow: Integer;
var
  SI: TStartupInfo;
begin
  Result := 10;                  { SW_SHOWDEFAULT }
  SI.cb := SizeOf(TStartupInfo);
  GetStartupInfo(SI);
  if SI.dwFlags and 1 <> 0 then  { STARTF_USESHOWWINDOW }
    Result := SI.wShowWindow;
end;
{$ENDIF MSWINDOWS}

function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer; CodePage: Integer): Integer; forward;
function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer; CodePage: Integer): Integer; overload; forward;
function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer; overload; forward;

{ ----------------------------------------------------- }
{       Memory manager                                  }
{ ----------------------------------------------------- }

{$IFDEF MSWINDOWS}
{$IFDEF SIMPLEHEAP}
{$I SimpleHeap.inc}
{$ELSE}
{$I GETMEM.INC}
{$ENDIF}
{$ENDIF}

{$IFDEF POSIX}
function SysGetMem(Size: NativeInt): Pointer;
begin
  Result := __malloc(size);
end;

function SysFreeMem(P: Pointer): Integer;
begin
  __free(P);
  Result := 0;
end;

function SysReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := realloc(P, Size);
end;

function SysAllocMem(Size: NativeInt): Pointer;
begin
  // Alocate and zero memory for 1 item of Size bytes
  Result := calloc(1, Size);
end;

function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // not implemented for POSIX
  Result := False;
end;

function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // not implemented for POSIX
  Result := False;
end;
{$ENDIF POSIX}

var
  MemoryManager: TMemoryManagerEx = (
    GetMem: SysGetMem;
    FreeMem: SysFreeMem;
    ReallocMem: SysReallocMem;
    AllocMem: SysAllocMem;
    RegisterExpectedmemoryLeak: SysRegisterExpectedMemoryLeak;
    UnregisterExpectedmemoryLeak: SysUnregisterExpectedMemoryLeak);

function AllocMem(Size: NativeInt): Pointer;
{$IFDEF PUREPASCAL}
begin
  if Size > 0 then
  begin
    Result := MemoryManager.AllocMem(Size);
    if Result = nil then
      Error(reOutOfMemory);
  end
  else
    Result := nil;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JZ      @@allocmemdone
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, [EAX].OFFSET MemoryManager.AllocMem
        POP     EAX
        CALL    EBX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.AllocMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        TEST    EAX,EAX
        JZ      @@allocmemerror
@@allocmemdone:
        REP     RET  // Optimization for branch prediction
@@allocmemerror:
        MOV     AL,reOutOfMemory
        JMP     Error
end;
{$ENDIF}

function RegisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := (P <> nil) and MemoryManager.RegisterExpectedMemoryLeak(P);
end;

function UnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := (P <> nil) and MemoryManager.UnregisterExpectedMemoryLeak(P);
end;

                                                                                                                                                             
function _GetMem(Size: NativeInt): Pointer;
{$IFDEF PUREPASCAL}
begin
  if Size <= 0 then
    Exit(nil);
  Result := MemoryManager.GetMem(Size);
  if Result = nil then
    Error(reOutOfMemory);
end;
{$ELSE !PUREPASCAL}
asm //StackAlignSafe
        TEST    EAX,EAX
        JLE     @@negativeorzerosize
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, [EAX].OFFSET MemoryManager.GetMem
        POP     EAX
        CALL    EBX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.GetMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        TEST    EAX,EAX
        JZ      @@getmemerror
        REP     RET // Optimization for branch prediction
@@getmemerror:
        MOV     AL, reOutOfMemory
        JMP     Error
@@negativeorzerosize:
        XOR     EAX, EAX
        DB      $F3 // REP RET
end;
{$ENDIF !PUREPASCAL}

function _FreeMem(P: Pointer): Integer;
{$IFDEF PUREPASCAL}
begin
  if P = nil then
    Exit(0);
  Result := MemoryManager.FreeMem(P);
  if Result <> 0 then
    Error(reInvalidPtr);
end;
{$ELSE !PUREPASCAL}
asm //StackAlignSafe
        TEST    EAX,EAX
        JZ      @@freememdone
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, [EAX].OFFSET MemoryManager.FreeMem
        POP     EAX
        CALL    EBX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        TEST    EAX,EAX
        JNZ     @@freememerror
@@freememdone:
        REP     RET // Optimization for branch prediction
@@freememerror:
        MOV     AL,reInvalidPtr
        JMP     ERROR
end;
{$ENDIF !PUREPASCAL}

function _ReallocMem(var P: Pointer; NewSize: NativeInt): Pointer;
{$IFDEF PUREPASCAL}
begin
  if P <> nil then
  begin
    if NewSize > 0 then
    begin
      Result := MemoryManager.ReallocMem(P, NewSize);
      if Result = nil then
        Error(reOutOfMemory);
    end
    else
    begin
       if MemoryManager.FreeMem(P) <> 0 then
        Error(reInvalidPtr);
      Result := nil;
    end;
    P := Result;
  end else
  begin
    if NewSize <= 0 then
      Exit(nil);
    Result := MemoryManager.GetMem(NewSize);
    if Result = nil then
      Error(reOutOfMemory);
    P := Result;
  end;
end;
{$ELSE !PUREPASCAL}

asm
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
{$ENDIF PIC}
        MOV     ECX, [EAX]
        TEST    ECX, ECX
        JE      @@alloc
        TEST    EDX, EDX
        JE      @@free
@@resize:
        PUSH    EAX
        MOV     EAX, ECX
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        MOV     EBX, [EBX].OFFSET MemoryManager.ReallocMem
        CALL    EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.ReallocMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ENDIF PIC}
        POP     ECX
        OR      EAX, EAX
        JE      @@allocError
        MOV     [ECX], EAX
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
        RET
@@freeError:
        MOV     AL, reInvalidPtr
        JMP     Error
@@free:
        MOV     [EAX], EDX
        MOV     EAX, ECX
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EBX, [EBX].OFFSET MemoryManager.FreeMem
        CALL    EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        OR      EAX, EAX
        JNE     @@freeError
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
        RET
@@allocError:
        MOV     AL, reOutOfMemory
        JMP     Error
@@alloc:
        TEST    EDX, EDX
        JE      @@exit
        PUSH    EAX
        MOV     EAX, EDX
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        MOV     EBX, [EBX].OFFSET MemoryManager.GetMem
        CALL    EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.GetMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        POP     ECX
        OR      EAX, EAX
        JE      @@allocError
        MOV     [ECX], EAX
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
@@exit:
end;
{$ENDIF !PUREPASCAL}

{ The default AllocMem implementation - for older memory managers that do not
  implement this themselves. }
function DefaultAllocMem(Size: NativeInt): Pointer;
begin
  Result := _GetMem(Size);
  if (Result <> nil) then
    FillChar(Result^, Size, 0)
end;

{ The default (do nothing) leak registration function for backward compatibility
  with older memory managers. }
function DefaultRegisterAndUnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := False;
end;

{ Backward compatible GetMemoryManager implementation }
procedure GetMemoryManager(var MemMgr: TMemoryManager);
begin
  MemMgr.GetMem := MemoryManager.GetMem;
  MemMgr.FreeMem := MemoryManager.FreeMem;
  MemMgr.ReallocMem := MemoryManager.ReallocMem;
end;

{ Backward compatible SetMemoryManager implementation }
procedure SetMemoryManager(const MemMgr: TMemoryManager);
begin
  MemoryManager.GetMem := MemMgr.GetMem;
  MemoryManager.FreeMem := MemMgr.FreeMem;
  MemoryManager.ReallocMem := MemMgr.ReallocMem;
  MemoryManager.AllocMem := DefaultAllocMem;
  MemoryManager.RegisterExpectedMemoryLeak :=
    DefaultRegisterAndUnregisterExpectedMemoryLeak;
  MemoryManager.UnregisterExpectedMemoryLeak :=
    DefaultRegisterAndUnregisterExpectedMemoryLeak;
end;

procedure GetMemoryManager(var MemMgrEx: TMemoryManagerEx);
begin
  MemMgrEx := MemoryManager;
end;

procedure SetMemoryManager(const MemMgrEx: TMemoryManagerEx);
begin
  MemoryManager := MemMgrEx;
end;

function IsMemoryManagerSet: Boolean;
begin
  with MemoryManager do
    Result := (@GetMem <> @SysGetMem) or (@FreeMem <> @SysFreeMem) or
      (@ReallocMem <> @SysReallocMem) or (@AllocMem <> @SysAllocMem) or
      (@RegisterExpectedMemoryLeak <> @SysRegisterExpectedMemoryLeak) or
      (@UnregisterExpectedMemoryLeak <> @SysUnregisterExpectedMemoryLeak);
end;

procedure RunErrorAt(ErrCode: Integer; ErrorAtAddr: Pointer); forward;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure GetUnwinder(var Dest: TUnwinder);
begin
  Dest := Unwinder;
end;

procedure SetUnwinder(const NewUnwinder: TUnwinder);
begin
  Unwinder := NewUnwinder;
end;

function IsUnwinderSet: Boolean;
begin
  with Unwinder do
    Result := (@RaiseException <> @_BorUnwind_RaiseException) or
      (@RegisterIPLookup <> @_BorUnwind_RegisterIPLookup) or
      (@UnregisterIPLookup <> @_BorUnwind_UnregisterIPLookup) or
      (@DelphiLookup <> @_BorUnwind_DelphiLookup);
end;

procedure InitUnwinder;
var
  Addr: Pointer;
begin
  { We look to see if we can find a dynamic version of the unwinder.  This
    will be the case if the application used ShareExcept.pas.  If it is
    present, then we fire it up.  Otherwise, we use our static copy. }
  Addr := dlsym(RTLD_DEFAULT, '_BorUnwind_RegisterIPLookup');
  if Addr <> nil then
  begin
    Unwinder.RegisterIPLookup := Addr;
    Addr := dlsym(RTLD_DEFAULT, '_BorUnwind_UnregisterIPLookup');
    Unwinder.UnregisterIPLookup := Addr;
    Addr := dlsym(RTLD_DEFAULT, '_BorUnwind_RaiseException');
    Unwinder.RaiseException := Addr;
    Addr := dlsym(RTLD_DEFAULT, '_BorUnwind_DelphiLookup');
    Unwinder.DelphiLookup := Addr;
    Addr := dlsym(RTLD_DEFAULT, '_BorUnwind_ClosestDelphiHandler');
    Unwinder.ClosestHandler := Addr;
  end
  else
  begin
    dlerror;   // clear error state;  dlsym doesn't
    Unwinder.RegisterIPLookup := _BorUnwind_RegisterIPLookup;
    Unwinder.DelphiLookup := _BorUnwind_DelphiLookup;
    Unwinder.UnregisterIPLookup := _BorUnwind_UnregisterIPLookup;
    Unwinder.RaiseException := _BorUnwind_RaiseException;
    Unwinder.ClosestHandler := _BorUnwind_ClosestDelphiHandler;
  end;
end;

function SysClosestDelphiHandler(Context: Pointer): LongWord;
begin
  if not Assigned(Unwinder.ClosestHandler) then
    InitUnwinder;
  Result := Unwinder.ClosestHandler(Context);
end;

function SysRegisterIPLookup(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
begin
//  xxx
  if not Assigned(Unwinder.RegisterIPLookup) then
  begin
    InitUnwinder;
  end;
  Result := Unwinder.RegisterIPLookup(@Unwinder.DelphiLookup, StartAddr, EndAddr, Context, GOT);
end;

function SysRegisterIPLookupFunc(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt;
                            UnwinderLookup: TUnwinderLookup): LongBool;
begin
  if not Assigned(Unwinder.RegisterIPLookup) then
  begin
    InitUnwinder;
  end;
  Result := Unwinder.RegisterIPLookup(@UnwinderLookup, StartAddr, EndAddr, Context, GOT);
end;

procedure SysUnregisterIPLookup(StartAddr: LongInt);
begin
  Unwinder.UnregisterIPLookup(StartAddr);
end;

function SysRaiseException(Exc: Pointer): LongBool; export;
var
  uexc: _Unwind_Exception;
begin
  uexc.exception_class := UW_EXC_CLASS_BORLANDDELPHI;
  uexc.private_1 := _Unwind_Word(Exc);
  uexc.private_2 := 0;
  Result := Unwinder.RaiseException(@uexc);
end;

//  SysRaiseCPPException
//    Called to reraise a C++ exception that is unwinding through pascal code.
function SysRaiseCPPException(Exc: Pointer; priv2: Pointer; cls: LongWord): LongBool;
var
  uexc: _Unwind_Exception;
begin
  uexc.exception_class := cls;
  uexc.private_1 := _Unwind_Word(Exc);
  uexc.private_2 := _Unwind_Word(priv2);
  Result := Unwinder.RaiseException(@uexc);
end;

const
  MAX_NESTED_EXCEPTIONS = 16;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
const
  MAX_NESTED_EXCEPTIONS = 16;
{$ENDIF TABLE_BASED_EXCEPTIONS}

threadvar
{$IFDEF PC_MAPPED_EXCEPTIONS}
  ExceptionObjects: array[0..MAX_NESTED_EXCEPTIONS-1] of TRaisedException;
  ExceptionObjectCount: Integer;
  OSExceptionsBlocked: Integer;
  ExceptionList: PRaisedException;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF STACK_BASED_EXCEPTIONS}
  RaiseListPtr: Pointer;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
  ExceptionObjects: array[0..MAX_NESTED_EXCEPTIONS-1] of TRaiseFrame;
  ExceptionObjectCount: Integer;
  RaiseListPtr: PRaiseFrame;
{$ENDIF TABLE_BASED_EXCEPTIONS}

threadvar
  InOutRes: Integer;

{$IFDEF PC_MAPPED_EXCEPTIONS}

procedure BlockOSExceptions;
asm  //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    SysInit.@GetTLS
        MOV     [EAX].OSExceptionsBlocked, 1
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
end;

procedure UnblockOSExceptions;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     [EAX].OSExceptionsBlocked, 0
end;

// Access to a TLS variable.  Note the comment in BeginThread before
// you change the implementation of this function.
function AreOSExceptionsBlocked: Boolean;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EAX, [EAX].OSExceptionsBlocked
end;

const
  TRAISEDEXCEPTION_SIZE = SizeOf(TRaisedException);

                                                                   
function CurrentException: PRaisedException;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
        LEA     EDX, [EAX].ExceptionObjects
        MOV     EAX, [EAX].ExceptionObjectCount
        OR      EAX, EAX
        JE      @@Done
        DEC     EAX
        IMUL    EAX, TRAISEDEXCEPTION_SIZE
        ADD     EAX, EDX
        JMP     @@Exit
@@Done:
        CALL    SysInit.@GetTLS
        MOV     EAX,[EAX].ExceptionList
@@Exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;

                                                                          
function CurrentPrivateException: PRaisedException;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
        LEA     EDX, [EAX].ExceptionObjects
        MOV     EAX, [EAX].ExceptionObjectCount
        OR      EAX, EAX
        JE      @@Done
        DEC     EAX
        IMUL    EAX, TRAISEDEXCEPTION_SIZE
        ADD     EAX, EDX
@@Done:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;

{ In the interests of code size here, this function is slightly overloaded.
  It is responsible for freeing up the current exception record on the
  exception stack, and it conditionally returns the thrown object to the
  caller.  If the object has been acquired through AcquireExceptionObject,
  we don't return the thrown object. }
                                                                
function FreeException: Pointer;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentPrivateException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        OR      EAX, EAX
        JE      @@Error
        { EAX -> the TRaisedException }
        XOR     ECX, ECX
        { If the exception object has been referenced, we don't return it. }
        CMP     [EAX].TRaisedException.RefCount, 0
        JA      @@GotObject
        MOV     ECX, [EAX].TRaisedException.ExceptObject
@@GotObject:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ECX
        CALL    SysInit.@GetTLS
        POP     ECX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        DEC     [EAX].ExceptionObjectCount
        MOV     EAX, ECX
        RET
@@Error:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
        MOV     EAX, [EAX].ExceptionList
        CALL    [EAX].TRaisedException.Cleanup
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        RET
end;

procedure ReleaseDelphiException;
begin
  FreeException;
end;

                                                                          
function AllocateException(Exception: Pointer; ExceptionAddr: Pointer): PRaisedException;
asm
        PUSH    EBX // This is to simplify stack aligment for PIC and non PIC.
        PUSH    EAX
        PUSH    EDX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EBX, EAX
{$ELSE !PIC}
        XOR     EBX, EBX
{$ENDIF !PIC}
        CALL    SysInit.@GetTLS
        CMP     [EAX].ExceptionObjectCount, MAX_NESTED_EXCEPTIONS
        JE      @@TooManyNestedExceptions
        INC     [EAX].ExceptionObjectCount
        CALL    CurrentException
        POP     EDX
        POP     ECX
        MOV     [EAX].TRaisedException.ExceptObject, ECX
        MOV     [EAX].TRaisedException.ExceptionAddr, EDX
        MOV     [EAX].TRaisedException.RefCount, 0
        MOV     [EAX].TRaisedException.HandlerEBP, $FFFFFFFF
        MOV     [EAX].TRaisedException.Flags, 0
        MOV     [EAX].TRaisedException.Prev, 0
        LEA     EDX, [EBX].OFFSET FreeException
        MOV     [EAX].TRaisedException.Cleanup, EDX
        LEA     EDX, [EBX].OFFSET ReleaseDelphiException
        MOV     [EAX].TRaisedException.ReleaseProc, EDX
        POP     EBX
        RET
@@TooManyNestedExceptions:
        ADD     ESP, 12  // Throw away EDX, EBX and EAX.
        MOV     EAX, 231
        JMP     _RunError
end;

function AcquireExceptionObject: Pointer;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        OR      EAX, EAX
        JE      @@Error
        INC     [EAX].TRaisedException.RefCount
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        RET
@@Error:
   RET // windows version doesn't generate an error, and Halt0 calls this always
        { This happens if there is no exception pending }
//        JMP     _Run0Error
end;

procedure ReleaseExceptionObject;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        OR      EAX, EAX
        JE      @@Error
        CMP     [EAX].TRaisedException.RefCount, 0
        JE      @@Error
        DEC     [EAX].TRaisedException.RefCount
        RET
@@Error:
{ This happens if there is no exception pending, or
  if the reference count on a pending exception is
  zero. }
        JMP   _Run0Error
end;

function ExceptObject: TObject;
var
  Exc: PRaisedException;
begin
  Exc := CurrentException;
  if Exc <> nil then
    Result := TObject(Exc^.ExceptObject)
  else
    Result := nil;
end;

{ Return current exception address }
function ExceptAddr: Pointer;
var
  Exc: PRaisedException;
begin
  Exc := CurrentException;
  if Exc <> nil then
    Result := Exc^.ExceptionAddr
  else
    Result := nil;
end;

{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF STACK_BASED_EXCEPTIONS}

function ExceptObject: TObject;
begin
  if RaiseListPtr <> nil then
    Result := PRaiseFrame(RaiseListPtr)^.ExceptObject
  else
    Result := nil;
end;

{ Return current exception address }
function ExceptAddr: Pointer;
begin
  if RaiseListPtr <> nil then
    Result := PRaiseFrame(RaiseListPtr)^.ExceptAddr
  else
    Result := nil;
end;

function AcquireExceptionObject: Pointer;
type
  ExceptionAcquiredProc = procedure (Obj: Pointer);
var
  RaiseFrame: PRaiseFrame;
begin
  RaiseFrame := RaiseListPtr;
  if RaiseFrame <> nil then
  begin
    Result := RaiseFrame^.ExceptObject;
    RaiseFrame^.ExceptObject := nil;
    if Assigned(ExceptionAcquired) then
      ExceptionAcquiredProc(ExceptionAcquired)(Result);
  end
  else
    Result := nil;
end;

procedure ReleaseExceptionObject;
begin
end;

function RaiseList: Pointer;
begin
  Result := RaiseListPtr;
end;

                                                                                        
function SetRaiseList(NewPtr: Pointer): Pointer;
begin
  Result := RaiseListPtr;
  RaiseListPtr := NewPtr;
end;

procedure _UnhandledException;
type
  TExceptProc = procedure (Obj: TObject; Addr: Pointer);
begin
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptObject, ExceptAddr)
  else
    RunErrorAt(230, ExceptAddr);
end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
{
function CurrentException: PRaiseFrame;
var
  Index: Integer;
begin
  Index := ExceptionObjectCount;
  if Index > 0 then 
    Result := @ExceptionObjects[Index - 1]
  else
    Result := RaiseListPtr;
end;
}


function AllocateRaiseFrame: PRaiseFrame;
var
  Index: Integer;
  RaiseFrame: PRaiseFrame;
begin
  Index := ExceptionObjectCount;
  if Index >= MAX_NESTED_EXCEPTIONS then
    RunErrorAt(231, ReturnAddress);

  ExceptionObjectCount := Index + 1;
  RaiseFrame := @ExceptionObjects[Index];
  RaiseFrame^.NextRaise := nil;
  RaiseFrame^.ExceptObject := nil;
  RaiseFrame^.ExceptAddr := nil;
  Result := RaiseFrame;
end;

procedure ReleaseRaiseFrame(RaiseFrame: PRaiseFrame);
var
  Index: Integer;
begin
  if (UIntPtr(RaiseFrame) >= UIntPtr(@ExceptionObjects)) and
     (UIntPtr(RaiseFrame) <= UIntPtr(@ExceptionObjects[MAX_NESTED_EXCEPTIONS-1])) then
  begin
    Index := (UIntPtr(RaiseFrame) - UIntPtr(@ExceptionObjects[0])) div SizeOf(TRaiseFrame);
    if Index < ExceptionObjectCount then
      ExceptionObjectCount := Index;
  end;
end;

procedure LinkRaiseFrame(RaiseFrame: PRaiseFrame); inline;
var
  List: PPRaiseFrame;
begin
  // Note: Taking an address of RaiseListPtr may not be allowed if
  //       per-thread memory space is allocated in another space.
  //       It depends on threadvar implementation. However,
  //       current implementation can take an address of RaiseListPtr.
  List := @RaiseListPtr;
  RaiseFrame^.NextRaise := List^;
  List^ := RaiseFrame;
end;

function PopRaiseFrame: PRaiseFrame; inline;
var
  List: PPRaiseFrame;
begin
  List := @RaiseListPtr;
  Result := List^;
  if Result = nil then
    _RunError(216); // reAccessViolation
  List^ := Result^.NextRaise;
end;

{ Return current exception object }
function ExceptObject: TObject;
var
  RaiseFrame: PRaiseFrame;
begin
  RaiseFrame := RaiseListPtr;
  if RaiseFrame <> nil then
    Result := RaiseFrame^.ExceptObject
  else
    Result := nil;
end;

{ Return current exception address }
function ExceptAddr: Pointer;
var
  RaiseFrame: PRaiseFrame;
begin
  RaiseFrame := RaiseListPtr;
  if RaiseFrame <> nil then
    Result := Pointer(RaiseFrame^.ExceptAddr)
  else
    Result := nil;
end;

                                                                                   
function AcquireExceptionObject: Pointer;
type
  ExceptionAcquiredProc = procedure (Obj: Pointer);
var
  RaiseFrame: PRaiseFrame;
begin
  RaiseFrame := RaiseListPtr;
  if RaiseFrame <> nil then
  begin
    Result := RaiseFrame^.ExceptObject;
    RaiseFrame^.ExceptObject := nil;
    if Assigned(ExceptionAcquired) then
      ExceptionAcquiredProc(ExceptionAcquired)(Result);
  end
  else
    Result := nil;
end;

                                                                           
procedure ReleaseExceptionObject;
begin
end;

{$ENDIF TABLE_BASED_EXCEPTIONS}


{$IFDEF MSWINDOWS}
{
  Coverage helper glue - just go directly to the external coverage
  library.  NEVER put code in here, because we sometimes want to run
  coverage analysis on the System unit.
}
                                                                                         
procedure _CVR_PROBE; external 'coverage.dll' name '__CVR_PROBE';
function _CVR_STMTPROBE; external 'coverage.dll' name '__CVR_STMTPROBE';
{$ENDIF MSWINDOWS}
{ ----------------------------------------------------- }
{    local functions & procedures of the system unit    }
{ ----------------------------------------------------- }

procedure RunErrorAt(ErrCode: Integer; ErrorAtAddr: Pointer);
begin
  ErrorAddr := ErrorAtAddr;
  _Halt(ErrCode);
end;

procedure ErrorAt(ErrorCode: Byte; ErrorAddr: Pointer);

const
  reMap: array [TRunTimeError] of Byte = (
    0,   { reNone }
    203, { reOutOfMemory }
    204, { reInvalidPtr }
    200, { reDivByZero }
    201, { reRangeError }
{   210    Abstract error }
    215, { reIntOverflow }
    207, { reInvalidOp }
    200, { reZeroDivide }
    205, { reOverflow }
    206, { reUnderflow }
    219, { reInvalidCast }
    216, { reAccessViolation }
    218, { rePrivInstruction }
    217, { reControlBreak }
    202, { reStackOverflow }
    220, { reVarTypeCast }
    221, { reVarInvalidOp }
    222, { reVarDispatch }
    223, { reVarArrayCreate }
    224, { reVarNotArray }
    225, { reVarArrayBounds }
{   226    Thread init failure }
    227, { reAssertionFailed }
    0,   { reExternalException not used here; in SysUtils }
    228, { reIntfCastError }
    229, { reSafeCallError }
    235, { reMonitorNotLocked }
    236  { reNoMonitorSupport }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{   230   Reserved by the compiler for unhandled exceptions }
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IF defined(PC_MAPPED_EXCEPTIONS) or defined(STACK_BASED_EXCEPTIONS)}
{   231   Too many nested exceptions }
{$IFEND}
{$IF Defined(LINUX) or Defined(MACOS)}
{   232   Fatal signal raised on a non-Delphi thread }
    , 233 { reQuit }
{$IFEND LINUX or MACOS}
{$IFDEF POSIX}
    , 234  { reCodesetConversion }
{$ENDIF POSIX}
    , 237 { rePlatformNotImplemented }
);

begin
  errorCode := errorCode and 127;
  if Assigned(ErrorProc) then
    ErrorProc(errorCode, ErrorAddr);
  if errorCode = 0 then
    errorCode := InOutRes
  else if errorCode <= Byte(High(TRuntimeError)) then
    errorCode := reMap[TRunTimeError(errorCode)];
  RunErrorAt(errorCode, ErrorAddr);
end;

                                                                                  
procedure Error(errorCode: TRuntimeError);
begin
  ErrorAt(Byte(errorCode), ReturnAddress);
end;

procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  if TTextRec(T).Mode = fmClosed then
    TTextRec(T).Flags := (TTextRec(T).Flags and not tfCRLF) or (tfCRLF * Byte(Style))
  else
    SetInOutRes(107);  // can't change mode of open file
end;

function GetTextCodePage(const T: Text): Word;
begin
  Result := TTextRec(T).CodePage;
end;

procedure SetTextCodePage(var T: Text; CodePage: Word);
begin
  TTextRec(T).CodePage := CodePage;
end;

procedure __IOTest;
{$IFNDEF CPUX86}
begin
  if InOutRes <> 0 then
    ErrorAt(byte(reNone), ReturnAddress);
end;
{$ELSE CPUX86}
// __IOTest must preserve EAX, EDX, ECX on exit with the current dcc32 codegen
asm
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    SysInit.@GetTLS
        CMP     [EAX].InOutRes,0
        POP     ECX
        POP     EDX
        POP     EAX
        JNE     @error
        RET
@error:
        XOR     EAX,EAX
        JMP     Error
end;
{$ENDIF CPUX86}

procedure SetInOutRes(NewValue: Integer);
begin
  InOutRes := NewValue;
end;

procedure InOutError;
begin
  SetInOutRes(GetLastError);
end;

procedure ChDir(const S: string);
begin
  // U-OK
  ChDir(PChar(S));
end;

procedure ChDir(P: PChar);
{$IFDEF MSWINDOWS}
begin
  // U-OK
  if not SetCurrentDirectory(P) then
    InOutError;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
   us: UTF8String;
begin
  // U-OK
  us := UTF8String(P);
  if __chdir(PAnsiChar(us)) <> 0 then
    InOutError;
end;
{$ENDIF POSIX}

procedure _UGetDir(D: Byte; var S: UnicodeString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of WideChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of WideChar;
begin
  if D <> 0 then
  begin
    Drive[0] := WideChar(D + Ord('A') - 1);
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryW(SizeOf(SaveBuf) div SizeOf(WideChar), SaveBuf);
    SetCurrentDirectoryW(Drive);
  end;
  GetCurrentDirectoryW(SizeOf(DirBuf) div SizeOf(WideChar), DirBuf);
  if D <> 0 then SetCurrentDirectoryW(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  DirBuf: array[0..MAX_PATH] of AnsiChar;
begin
  getcwd(DirBuf, sizeof(DirBuf));
  S := UTF8ToString(DirBuf);
{$ENDIF POSIX}
end;

procedure _LGetDir(D: Byte; var S: AnsiString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of AnsiChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of AnsiChar;
begin
  if D <> 0 then
  begin
    Drive[0] := AnsiChar(Chr(D + Ord('A') - 1));
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryA(SizeOf(SaveBuf), SaveBuf);
    SetCurrentDirectoryA(Drive);
  end;
  GetCurrentDirectoryA(SizeOf(DirBuf), DirBuf);
  if D <> 0 then SetCurrentDirectoryA(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  DirBuf: array[0..MAX_PATH] of AnsiChar;
begin
  getcwd(DirBuf, sizeof(DirBuf));
  S := AnsiString(UTF8ToString(DirBuf));
{$ENDIF POSIX}
end;

procedure _WGetDir(D: Byte; var S: WideString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of WideChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of WideChar;
begin
  if D <> 0 then
  begin
    Drive[0] := WideChar(Chr(D + Ord('A') - 1));
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryW(Length(SaveBuf), SaveBuf);
    SetCurrentDirectoryW(Drive);
  end;
  GetCurrentDirectoryW(Length(DirBuf), DirBuf);
  if D <> 0 then SetCurrentDirectoryW(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOS)}
var
   U: UnicodeString;
begin
   _UGetDir(D, U);
   S := U;
{$IFEND}
end;

procedure _SGetDir(D: Byte; var S: ShortString);
var
  L: AnsiString;
begin
  _LGetDir(D, L);
  S := L;
end;

function IOResult: Integer;
begin
  Result := InOutRes;
  InOutRes := 0;
end;

procedure MkDir(const S: string);
begin
  MkDir(PChar(s));
end;

procedure MkDir(P: PChar);
begin
{$IFDEF MSWINDOWS}
  if not CreateDirectory(P, nil) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if __mkdir(PAnsiChar(UTF8Encode(P)), mode_t(-1)) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;

                                                                              
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly function Move is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure Move(const Source; var Dest; Count: NativeInt);
{$IFDEF PUREPASCAL}
{$POINTERMATH ON}
var
  S, D, I: PByte;
  Temp: NativeInt;
  C: NativeUInt;
  L: PNativeInt;
begin
  S := PByte(@Source);
  D := PByte(@Dest);
  if S = D then
    Exit;
  if Count <= SizeOf(NativeInt) then
    case Count of
      1: D[0] := S[0];
      2: PWord(D)[0] := PWord(S)[0];
      3: if D > S then
         begin
           D[2] := S[2];
           PWord(D)[0] := PWord(S)[0];
         end
         else
         begin
           PWord(D)[0] := PWord(S)[0];
           D[2] := S[2];
         end;
      4: PInteger(D)[0] := PInteger(S)[0];
      5: if D > S then
         begin
           D[4] := S[4];
           PInteger(D)[0] := PInteger(S)[0];
         end
         else
         begin
           PInteger(D)[0] := PInteger(S)[0];
           D[4] := S[4];
         end;
      6: if D > S then
         begin
           PWord(D)[2] := PWord(S)[2];
           PInteger(D)[0] := PInteger(S)[0];
         end
         else
         begin
           PInteger(D)[0] := PInteger(S)[0];
           PWord(D)[2] := PWord(S)[2];
         end;
      7: if D > S then
         begin
           D[6] := S[6];
           PWord(D)[2] := PWord(S)[2];
           PInteger(D)[0] := PInteger(S)[0];
         end
         else
         begin
           PInteger(D)[0] := PInteger(S)[0];
           PWord(D)[2] := PWord(S)[2];
           D[6] := S[6];
         end;
      8: PInt64(D)[0] := PInt64(S)[0];
    else
      Exit; {Count <= 0}
    end
  else
    if D > S then
    begin
      Temp := PNativeInt(S)^;
      I := D;
      C := Count - SizeOf(NativeInt);
      L := PNativeInt(D + C);
      Inc(S, C);
      repeat
        L^ := PNativeInt(S)^;
        if Count <= 2 * Sizeof(NativeInt) then
          Break;
        Dec(Count, Sizeof(NativeInt));
        Dec(S, Sizeof(NativeInt));
        Dec(L);
      until False;
      PNativeInt(I)^ := Temp;
    end
    else
    begin
      C := Count - Sizeof(NativeInt);
      Temp := PNativeInt(S + C)^;
      I := D + C;
      L := PNativeInt(D);
      repeat
        L^ := PNativeInt(S)^;
        if Count <= 2 * Sizeof(NativeInt) then
          Break;
        Dec(Count, Sizeof(NativeInt));
        Inc(S, Sizeof(NativeInt));
        Inc(L);
      until False;
      PNativeInt(I)^ := Temp;
    end;
end;
{$POINTERMATH OFF}
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        CMP     EAX, EDX
        JE      @@Exit {Source = Dest}
        CMP     ECX, 32
        JA      @@LargeMove {Count > 32 or Count < 0}
        SUB     ECX, 8
        JG      @@SmallMove
@@TinyMove: {0..8 Byte Move}
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
        MOV     EBX, EAX
        ADD     EBX, offset @@JumpTable+32
        MOV     ECX, [EBX+ECX*4]
        ADD     ECX, EAX
        POP     EAX
        POP     EBX
        JMP     ECX
{$ELSE}
        JMP     DWORD PTR [@@JumpTable+32+ECX*4]
{$ENDIF}
@@SmallMove: {9..32 Byte Move}
        FILD    QWORD PTR [EAX+ECX] {Load Last 8}
        FILD    QWORD PTR [EAX] {Load First 8}
        CMP     ECX, 8
        JLE     @@Small16
        FILD    QWORD PTR [EAX+8] {Load Second 8}
        CMP     ECX, 16
        JLE     @@Small24
        FILD    QWORD PTR [EAX+16] {Load Third 8}
        FISTP   QWORD PTR [EDX+16] {Save Third 8}
@@Small24:
        FISTP   QWORD PTR [EDX+8] {Save Second 8}
@@Small16:
        FISTP   QWORD PTR [EDX] {Save First 8}
        FISTP   QWORD PTR [EDX+ECX] {Save Last 8}
@@Exit:
        RET
        NOP {4-Byte Align JumpTable}
        NOP
@@JumpTable: {4-Byte Aligned}
        DD      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
        PUSH    EDX
        FILD    QWORD PTR [EAX] {First 8}
        LEA     EAX, [EAX+ECX-8]
        LEA     ECX, [ECX+EDX-8]
        FILD    QWORD PTR [EAX] {Last 8}
        PUSH    ECX
        NEG     ECX
        AND     EDX, -8 {8-Byte Align Writes}
        LEA     ECX, [ECX+EDX+8]
        POP     EDX
@FwdLoop:
        FILD    QWORD PTR [EAX+ECX]
        FISTP   QWORD PTR [EDX+ECX]
        ADD     ECX, 8
        JL      @FwdLoop
        FISTP   QWORD PTR [EDX] {Last 8}
        POP     EDX
        FISTP   QWORD PTR [EDX] {First 8}
        RET
@@LargeMove:
        JNG     @@LargeDone {Count < 0}
        CMP     EAX, EDX
        JA      @@LargeForwardMove
        SUB     EDX, ECX
        CMP     EAX, EDX
        LEA     EDX, [EDX+ECX]
        JNA     @@LargeForwardMove
        SUB     ECX, 8 {Backward Move}
        PUSH    ECX
        FILD    QWORD PTR [EAX+ECX] {Last 8}
        FILD    QWORD PTR [EAX] {First 8}
        ADD     ECX, EDX
        AND     ECX, -8 {8-Byte Align Writes}
        SUB     ECX, EDX
@BwdLoop:
        FILD    QWORD PTR [EAX+ECX]
        FISTP   QWORD PTR [EDX+ECX]
        SUB     ECX, 8
        JG      @BwdLoop
        POP     ECX
        FISTP   QWORD PTR [EDX] {First 8}
        FISTP   QWORD PTR [EDX+ECX] {Last 8}
@@LargeDone:
        RET
@@M01:
        MOVZX   ECX, [EAX]
        MOV     [EDX], CL
        RET
@@M02:
        MOVZX   ECX, WORD PTR [EAX]
        MOV     [EDX], CX
        RET
@@M03:
        MOV     CX, [EAX]
        MOV     AL, [EAX+2]
        MOV     [EDX], CX
        MOV     [EDX+2], AL
        RET
@@M04:
        MOV     ECX, [EAX]
        MOV     [EDX], ECX
        RET
@@M05:
        MOV     ECX, [EAX]
        MOV     AL, [EAX+4]
        MOV     [EDX], ECX
        MOV     [EDX+4], AL
        RET
@@M06:
        MOV     ECX, [EAX]
        MOV     AX, [EAX+4]
        MOV     [EDX], ECX
        MOV     [EDX+4], AX
        RET
@@M07:
        MOV     ECX, [EAX]
        MOV     EAX, [EAX+3]
        MOV     [EDX], ECX
        MOV     [EDX+3], EAX
        RET
@@M08:
        FILD    QWORD PTR [EAX]
        FISTP   QWORD PTR [EDX]
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure MoveChars(const Source; var Dest; Length: Integer);
begin
  Move(Source, Dest, Length * SizeOf(Char));
end;

{$IFDEF MSWINDOWS}
function GetParamStr(P: PChar; var Param: string): PChar;
var
  i, Len: Integer;
  Start, S: PChar;
begin
  // U-OK
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Inc(Len);
      Inc(P);
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
      if P[0] <> #0 then Inc(P);
    end
    else
    begin
      S[i] := P^;
      Inc(P);
      Inc(i);
    end;
  end;

  Result := P;
end;
{$ENDIF}

function ParamCount: Integer;
{$IFDEF MSWINDOWS}
var
  P: PChar;
  S: string;
begin
  // U-OK
  Result := 0;
  P := GetParamStr(GetCommandLine, S);
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOS)}
begin
  if ArgCount > 1 then
    Result := ArgCount - 1
  else Result := 0;
{$IFEND LINUX or MACOS}
end;

type
  PAnsiCharArray = array[0..0] of PAnsiChar;

function ParamStr(Index: Integer): string;
{$IFDEF MSWINDOWS}
var
  P: PChar;
  Buffer: array[0..260] of Char;
begin
  Result := '';
  if Index = 0 then
    SetString(Result, Buffer, GetModuleFileName(0, Buffer, Length(Buffer)))
  else
  begin
    P := GetCommandLine;
    while True do
    begin
      P := GetParamStr(P, Result);
      if (Index = 0) or (Result = '') then Break;
      Dec(Index);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOS)}
begin
  if Index < ArgCount then
    Result := string(PAnsiCharArray(ArgValues^)[Index])
  else
    Result := '';
{$IFEND LINUX or MACOS}
end;

procedure Randomize;
{$IFDEF MSWINDOWS}
var
  Counter: Int64;
begin
  if QueryPerformanceCounter(Counter) then
    RandSeed := Counter
  else
    RandSeed := GetTickCount;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
var
  TimeOfDay: timeval;
begin
  if gettimeofday(TimeOfDay, nil) = 0 then
    RandSeed := TimeOfDay.tv_sec * 1000000 + TimeOfDay.tv_usec
  else
    RandSeed := time(nil);
end;
{$ENDIF LINUX}
{$IFDEF MACOS}
begin
  RandSeed := AbsoluteToNanoseconds(UpTime);
end;
{$ENDIF MACOS}

// Random integer, implemented as a deterministic linear congruential generator
// with 134775813 as a and 1 as c.
function Random(const ARange: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  Temp: Longint;
begin
  Temp := RandSeed * $08088405 + 1;
  RandSeed := Temp;
  Result := (UInt64(Cardinal(ARange)) * UInt64(Cardinal(Temp))) shr 32;
end;
{$ELSE !PUREPASCAL}
asm
{     ->EAX     Range   }
{     <-EAX     Result  }
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
        MOV     ECX,[EBX].RandSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE !PIC}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].RandSeed,08088405H
        INC     EDX
        MOV     [EBX].RandSeed,EDX
{$ENDIF !PIC}

        MUL     EDX
        MOV     EAX,EDX
        POP     EBX
end;
{$ENDIF !PUREPASCAL}

function Random: Extended;
const
  two2neg32: double = ((1.0/$10000) / $10000);  // 2^-32
{$IFDEF PUREPASCAL}
var
  Temp: Longint;
  F: Extended;
begin
  Temp := RandSeed * $08088405 + 1;
  RandSeed := Temp;
  F  := Int64(Cardinal(Temp));
  Result := F * two2neg32;
end;
{$ELSE !PUREPASCAL}
asm
{       FUNCTION _RandExt: Extended;    }

        PUSH    EBX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EBX,EAX
        MOV     ECX,[EBX].OFFSET RandSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE !PIC}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].RandSeed,08088405H
        INC     EDX
        MOV     [EBX].RandSeed,EDX
{$ENDIF !PIC}

        FLD     [EBX].two2neg32
        PUSH    0
        PUSH    EDX
        FILD    qword ptr [ESP]
        ADD     ESP,8
        FMULP   ST(1), ST(0)
        POP     EBX
end;
{$ENDIF !PUREPASCAL}

procedure RmDir(const S: string);
begin
  // U-OK
  RmDir(PChar(s));
end;

procedure RmDir(P: PChar);
{$IFDEF POSIX}
var
   us: UTF8String;
{$ENDIF POSIX}
begin
  // U-OK
{$IFDEF MSWINDOWS}
  if not RemoveDirectory(P) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  us := UTF8String(P);
  if __rmdir(PAnsiChar(us)) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;

function UpCase(ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['a'..'z'] then
    Dec(Result, Ord('a')-Ord('A'));
end;

function UpCase(Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Ch of
    'a'..'z':
      Result := WideChar(Word(Ch) and $FFDF);
  end;
end;

procedure Reset8087CW;
begin
  Set8087CW(Default8087CW);
end;

procedure Set8087CW(NewCW: Word);
{$IFDEF CPUX86}
begin
  Default8087CW := NewCW;
  asm
        FNCLEX  // don't raise pending exceptions enabled by the new flags
{$IFDEF PIC}
        MOV     EAX,[EBX].OFFSET Default8087CW
        FLDCW   [EAX]
{$ELSE !PIC}
        FLDCW  Default8087CW
{$ENDIF !PIC}
  end;
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
asm
        MOV     Default8087CW, CX
        FNCLEX  // don't raise pending exceptions enabled by the new flags
        FLDCW   Default8087CW
end;
{$ENDIF CPUX64}

function Get8087CW: Word;
{$IFDEF CPUX86}
asm
        PUSH    0
        FNSTCW  [ESP].Word
        POP     EAX
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
asm
        PUSH    0
        FNSTCW  [RSP].Word
        POP     RAX
end;
{$ENDIF CPUX64}

const
  cInfinity   =  1.0 / 0.0;
  cNInfinity  = -1.0 / 0.0;
  cNaN        =  0.0 / 0.0;

  mInvalidOpEFlag  = $0001; // Invalid Operation exception flag
  mDenormalEFlag   = $0002; // Denormal exception flag
  mZeroDivideEFlag = $0004; // Divide-by-Zero exception flag
  mOverflowEFlag   = $0008; // Overflow flag
  mUnderflowEFlag  = $0010; // Underflow exception flag
  mPrecisionEFlag  = $0020; // Precision exception flag

  mInvalidOpMask   = $0080; // Invalid Operation mask
  mDenormalMask    = $0100; // Denormal mask
  mZeroDivideMask  = $0200; // Divide-by-Zero mask
  mOverflowMask    = $0400; // Overflow mask
  mUnderflowMask   = $0800; // Underflow mask
  mPrecisionMask   = $1000; // Precision mask

procedure ResetMXCSR;
begin
  if TestSSE <> 0 then
    SetMXCSR(DefaultMXCSR);
end;

procedure SetMXCSR(NewMXCSR: UInt32);
{$IFDEF CPUX86}
begin
  if TestSSE = 0 then exit;
  DefaultMXCSR := NewMXCSR and $FFC0;  // Remove status flag bits
  asm
{$IFDEF PIC}
        MOV     EAX,[EBX].OFFSET DefaultMXCSR
        LDMXCSR [EAX]
{$ELSE !PIC}
        LDMXCSR DefaultMXCSR
{$ENDIF !PIC}
  end;
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
asm
        AND     ECX, $FFC0 // Remove flag bits
        MOV     DefaultMXCSR, ECX
        LDMXCSR DefaultMXCSR
end;
{$ENDIF CPUX64}

procedure SetMXCSRStatus(ExceptionFlag: UInt32);
{$IFDEF CPUX86}
var
  MXCSR: UInt32;
asm
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EAX, [EAX].OFFSET TestSSE
        CMP     [EAX], 0
        POP     EAX
{$ELSE !PIC}
        CMP     TestSSE, 0
{$ENDIF !PIC}
        JE      @@NOSSE
        STMXCSR MXCSR
        OR      EAX, $003F
        OR      MXCSR, EAX
        LDMXCSR MXCSR
@@NOSSE:
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
var
  MXCSR: UInt32;
asm
        STMXCSR MXCSR
        OR      ECX, $003F
        OR      MXCSR, ECX
        LDMXCSR MXCSR
end;
{$ENDIF CPUX64}

function GetMXCSR: UInt32;
{$IFDEF CPUX86}
asm
{$IFDEF PIC}
        XOR     EAX, EAX
        PUSH    EAX
        CALL    GetGOT
        MOV     EAX, [EAX].OFFSET TestSSE
        CMP     [EAX], 0
        POP     EAX
        JE      @@NOSSE
{$ELSE !PIC}
        XOR     EAX, EAX
        CMP     TestSSE, EAX
        JE      @@NOSSE
{$ENDIF !PIC}
        PUSH    EAX
        STMXCSR [ESP].DWord
        POP     EAX
@@NOSSE:
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
asm
        PUSH    0
        STMXCSR [RSP].DWord
        POP     RAX
end;
{$ENDIF CPUX64}

function TSingleRec.GetSign: Boolean;
begin
  Result := Bytes[3] >= $80;
end;

procedure TSingleRec.SetSign(NewSign: Boolean);
begin
  if NewSign then
    Bytes[3] := Bytes[3] or $80
  else
    Bytes[3] := Bytes[3] and $7f;
end;

function TSingleRec.GetExp: UInt64;
begin
  Result := (Words[1] shr 7) and $FF;
end;

procedure TSingleRec.SetExp(NewExp: UInt64);
begin
  Words[1] := (Words[1] and $807F) or ((NewExp and $FF) shl 7);
end;

function TSingleRec.GetFrac: UInt64;
begin
  Result := LongWord(Self) and $007FFFFF;
end;

procedure TSingleRec.SetFrac(NewFrac: UInt64);
begin
  LongWord(Self) :=
    (LongWord(Self) and $FF800000)
        or (NewFrac and $007FFFFF);
end;

function TSingleRec.Fraction: Extended;
begin
  if Exp = $FF then
  begin
    if Frac = 0 then
    begin // +/- INF.
      PExtended(@Result)^ :=  cInfinity;
      PExtendedRec(@Result).Sign := Sign;
    end
    else // NaN
      PExtended(@Result)^ :=  cNan;
  end
  else if Exp = 0 then
    Result := (Frac / $00800000)
  else
    Result := 1.0 + (Frac / $00800000);
end;

function TSingleRec.Mantissa: UInt64;
begin
  Result := Frac;
  if (0 < Exp) and (Exp < $FF) then
    Result := Result or (UInt64(1) shl 23);
end;

function TSingleRec.Exponent: Integer;
var
  E: UInt64;
begin
  E := Exp;
  if (0 < E) and (E < $FF) then
    Result := E - $7F
  else if (E = 0) and (Frac <> 0) then
    Result := -126 // Denormal
  else if (E = 0) and (Frac = 0) then
    Result := 0 // +/-Zero
  else
    Result := 0; // +/-INF, NaN
end;

function TSingleRec.SpecialType: TFloatSpecial;
var
  I: LongWord;
  W: Word;
begin
  I := PLongWord(@self)^;
  W := Words[1];

  if ($0080 <= W) and (W <= $7F7F) then
    Result := fsPositive
  else if ($8080 <= W) and (W <= $FF7F) then
    Result := fsNegative
  else if I = 0 then
    Result := fsZero
  else if I = $80000000 then
    Result := fsNZero
  else if w <= $007F then
    Result := fsDenormal
  else if ($8000 <= w) and (w <= $807F) then
    Result := fsNDenormal
  else if I = $7F800000 then
    Result := fsInf
  else if I = $FF800000 then
    Result := fsNInf
  else
    Result := fsNan;
end;

procedure TSingleRec.BuildUp(const SignFlag: Boolean; const Mantissa: UInt64; const Exponent: Integer);
begin
  PLongWord(@self)^ := 0;
  Sign := SignFlag;
  Exp := Exponent + $7F;
  Frac := Mantissa and $007FFFFF;
end;

class operator TSingleRec.Explicit(a: Extended): TSingleRec;
begin
  PSingle(@Result)^ := a;
end;

class operator TSingleRec.Explicit(a: TSingleRec): Extended;
begin
  Result := PSingle(@a)^;
end;

function TDoubleRec.GetSign: Boolean;
begin
  Result := Bytes[7] >= $80;
end;

procedure TDoubleRec.SetSign(NewSign: Boolean);
begin
  if NewSign then
    Bytes[7] := Bytes[7] or $80
  else
    Bytes[7] := Bytes[7] and $7f;
end;

function TDoubleRec.GetExp: UInt64;
begin
  Result := (Words[3] shr 4) and $7FF;
end;

procedure TDoubleRec.SetExp(NewExp: UInt64);
begin
  Words[3] := (Words[3] and $800F) or ((NewExp and $7FF) shl 4);
end;

function TDoubleRec.GetFrac: UInt64;
begin
  Result := UInt64(Self) and $000FFFFFFFFFFFFF;
end;

procedure TDoubleRec.SetFrac(NewFrac: UInt64);
begin
  UInt64(Self) :=
    (UInt64(Self) and $FFF0000000000000)
      or (NewFrac and $000FFFFFFFFFFFFF)
end;

function TDoubleRec.Fraction: Extended;
begin
  if Exp = $7FF then
  begin
    if Frac = 0 then
    begin // +/- INF.
      PExtended(@Result)^ :=  cInfinity;
      PExtendedRec(@Result).Sign := Sign;
    end
    else // NaN
      PExtended(@Result)^ :=  cNaN;
  end
  else if Exp = 0 then
    Result := (Frac / $0010000000000000)
  else
    Result := 1.0 + (Frac / $0010000000000000);
end;

function TDoubleRec.Mantissa: UInt64;
begin
  Result := Frac;
  if (0 < Exp) and (Exp < $7FF) then
    Result := Result or (UInt64(1) shl 52);
end;

function TDoubleRec.Exponent: Integer;
begin
  if (0 < Exp) and (Exp < $7FF) then
    Result := Exp - $3FF
  else if (Exp = 0) and (Frac <> 0) then
    Result := -1022 // Denormal
  else if (Exp = 0) and (Frac = 0) then
    Result := 0 // +/-Zero
  else
    Result := 0; // +/-INF, NaN
end;

function TDoubleRec.SpecialType: TFloatSpecial;
var
  I: UInt64;
  W: Word;
begin
  I := PUInt64(@self)^;
  W := Words[3];

  if ($0010 <= W) and (W <= $7FEF) then
    Result := fsPositive
  else if ($8010 <= W) and (W <= $FFEF) then
    Result := fsNegative
  else if I = 0 then
    Result := fsZero
  else if I = $8000000000000000 then
    Result := fsNZero
  else if w <= $000F then
    Result := fsDenormal
  else if ($8000 <= w) and (w <= $800F) then
    Result := fsNDenormal
  else if I = $7FF0000000000000 then
    Result := fsInf
  else if I = $FFF0000000000000 then
    Result := fsNInf
  else
    Result := fsNan;
end;

procedure TDoubleRec.BuildUp(const SignFlag: Boolean; const Mantissa: UInt64; const Exponent: Integer);
begin
  PUInt64(@self)^ := 0;
  Sign := SignFlag;
  Exp := Exponent + $3FF;
  Frac := Mantissa and $000FFFFFFFFFFFFF;
end;

class operator TDoubleRec.Explicit(a: Extended): TDoubleRec;
begin
  PDouble(@Result)^ := a;
end;

class operator TDoubleRec.Explicit(a: TDoubleRec): Extended;
begin
  Result := PDouble(@a)^;
end;

function TExtended80Rec.GetSign: Boolean;
begin
  Result := Bytes[9] >= $80;
end;

procedure TExtended80Rec.SetSign(NewSign: Boolean);
begin
  if NewSign then
    Bytes[9] := Bytes[9] or $80
  else
    Bytes[9] := Bytes[9] and $7f;
end;

function TExtended80Rec.GetExp: UInt64;
begin
  Result := _Exp and $7FFF;
end;

procedure TExtended80Rec.SetExp(NewExp: UInt64);
begin
  _Exp := (_Exp and $8000) or (NewExp and $7FFF);
end;

function TExtended80Rec.Fraction: Extended;
begin
  if Exp = $7FFF then
  begin
    if Frac = 0 then
    begin // +/- INF.
      PExtended(@Result)^ :=  cInfinity;
      PExtendedRec(@Result).Sign := Sign;
    end
    else // NaN
      PExtended(@Result)^ :=  cNaN;
  end
  else
    Result := Frac / 9223372036854775808.0; // 2^63
end;

function TExtended80Rec.Mantissa: UInt64;
begin
  Result := Frac;
end;

function TExtended80Rec.Exponent: Integer;
begin
  if (0 < Exp) and (Exp < $7FFF) then
    Result := Exp - $3FFF
  else if (Exp = 0) and (Frac <> 0) then
    Result := -16382 // Denormal
  else if (Exp = 0) and (Frac = 0) then
    Result := 0 // +/-Zero
  else
    Result := 0; // +/-INF, NaN
end;

function TExtended80Rec.SpecialType: TFloatSpecial;
var
  I: UInt64;
  W: Word;
begin
  I := Frac;
  W := _Exp;

  if ($0001 <= W) and (W <= $7FFE) then
    Result := fsPositive
  else if ($8001 <= W) and (W <= $FFFE) then
    Result := fsNegative
  else if (I = 0) and (W = 0) then
    Result := fsZero
  else if (I = 0) and (W = $8000) then
    Result := fsNZero
  else if w = $0000 then
    Result := fsDenormal
  else if w = $8000 then
    Result := fsNDenormal
  else if (I = $8000000000000000) and (W = $7FFF) then
    Result := fsInf
  else if (I = $8000000000000000) and (W = $FFFF)then
    Result := fsNInf
  else
    Result := fsNan;
end;

procedure TExtended80Rec.BuildUp(const SignFlag: Boolean; const Mantissa: UInt64; const Exponent: Integer);
begin
  fillchar(self, 10, 0);
  Sign := SignFlag;
  Exp := Exponent + $3FFF;
  Frac := Mantissa;
end;

class operator TExtended80Rec.Explicit(a: Extended): TExtended80Rec;
{$IFDEF CPUX86}
begin
  Result := PExtended80Rec(@a)^;
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
var
  U: UInt64;
  E: Integer;
begin
  case PDoubleRec(@a).SpecialType of
    fsNInf,
    fsInf:
      Result.BuildUp( PDoubleRec(@a).Sign, $8000000000000000, $4000);
    fsNZero,
    fsZero:
      Result.BuildUp( PDoubleRec(@a).Sign, 0, -16383);
    fsNDenormal,
    fsDenormal:
      begin
        U := PDoubleRec(@a).Frac; // 52bits fraction
        U := U shl (64-53);
        E := -1022;
        while (U and $8000000000000000) = 0 do
        begin
          U := U * 2;
          Dec(E);
        end;
        PExtended80Rec(@Result).BuildUp( PDoubleRec(@a).Sign, U, E);
      end;
    fsNegative,
    fsPositive:
      Result.BuildUp( TDoubleRec(a).Sign, TDoubleRec(a).Mantissa shl (64-53), TDoubleRec(a).Exponent);
    fsNaN:
      Result.BuildUp( True, $C000000000000000, $4000);
  end;
end;
{$ENDIF CPUX64}

{$IFDEF CPUX64}
procedure RaiseInvalidOpException;
begin
  SetMXCSRStatus(mInvalidOpEFlag);
  if (GetMXCSR and mInvalidOpMask) = 0 then
    ErrorAt(Byte(reInvalidOp), ReturnAddress);
end;

procedure RaiseZeroDivideException;
begin
  SetMXCSRStatus(mZeroDivideEFlag);
  if (GetMXCSR and mZeroDivideMask) = 0 then
    ErrorAt(Byte(reZeroDivide), ReturnAddress);
end;

procedure RaiseOverflowException;
begin
  SetMXCSRStatus(mOverflowEFlag);
  if (GetMXCSR and mOverflowMask) = 0 then
    ErrorAt(Byte(reOverflow), ReturnAddress);
end;

procedure RaiseUnderflowException;
begin
  SetMXCSRStatus(mUnderflowEFlag);
  if (GetMXCSR and mUnderflowMask) = 0 then
    ErrorAt(Byte(reUnderflow), ReturnAddress);
end;
{$ELSE !CPUX64}

procedure RaiseOverflowException;
begin
                               
    ErrorAt(Byte(reOverflow), ReturnAddress);
end;
{$ENDIF !CPUX64}

class operator TExtended80Rec.Explicit(a: TExtended80Rec): Extended;
{$IFDEF CPUX86}
begin
  Result := PExtended(@a)^;
end;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
var
  T: Double;
begin
  case a.SpecialType of
    fsNInf,
    fsInf:
      PDoubleRec(@Result).BuildUp( a.Sign, 0, $400);
    fsNZero,
    fsZero:
      PDoubleRec(@Result).BuildUp( a.Sign, 0, -1023);
    fsNDenormal,
    fsDenormal:
      PDoubleRec(@Result).BuildUp( a.Sign, 0, -1023);
    fsNegative,
    fsPositive:
      begin
        if a.Exponent > 1023 then
        begin
          RaiseOverflowException;
          if a.Sign then Result := cNInfinity
          else Result := cInfinity;
        end
        else if a.Exponent < -(1023 + 52) then
          PDoubleRec(@Result).BuildUp( a.Sign, 0, -1023)
        else if a.Exponent < -1022 then
        begin
          // Result := Minimum normalized Double number.
          PDoubleRec(@Result).BuildUp( a.Sign, $10000000000000, -1022);
          // Exteneded80Rec.Mantissa has 64bit integer.
          Result := Result * (a.Mantissa / 9223372036854775808.0); // 2 ^ 63;
          PDoubleRec(@T).BuildUp( False, $10000000000000, a.Exponent + 1022);
          Result := Result * T;
        end
        else
        begin
          PDoubleRec(@Result).BuildUp( a.Sign, $10000000000000, a.Exponent);
          Result := Result * (a.Mantissa / 9223372036854775808.0); // 2 ^ 63;
        end;
      end;
    fsNaN:
      PDoubleRec(@Result).BuildUp( True, $8000000000000, $400);
  end;
end;
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function Int(const X: Double): Double;
asm
        .NOFRAME
        MOVSD     [RSP+$08], XMM0
        MOV       EAX, [RSP+$0C]
        AND       EAX, $7FF00000
        CMP       EAX, $43300000
        JGE       @@EXIT
        CMP       EAX, $3FE00000
        JBE       @@LOW
        CVTTSD2SI RAX, XMM0
        CVTSI2SD  XMM0, RAX
        JMP       @@EXIT
@@LOW:
        XORPD     XMM0, XMM0
@@EXIT:
end;
{$ELSE CPUX64}
function Int(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
  Result := Trunc(X);
end;
{$ELSE !PUREPASCAL}
asm
        FLD     X
        SUB     ESP,4
        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FRNDINT
        FWAIT
        FLDCW   [ESP].Word
        ADD     ESP,4
end;
{$ENDIF PUREPASCAL}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function Frac(const X: Double): Double;
asm
        .NOFRAME
        MOVSD     [RSP+$08], XMM0
        MOV       EAX, [RSP+$0C]
        AND       EAX, $7FF00000
        CMP       EAX, $43300000
        JGE       @@HIGH
        CMP       EAX, $3FE00000
        JBE       @@EXIT
        CVTTSD2SI RAX, XMM0
        CVTSI2SD  XMM4, RAX
        SUBSD     XMM0, XMM4
        JMP       @@EXIT
@@HIGH:
        XORPD     XMM0, XMM0
@@EXIT:
end;
{$ELSE  CPUX64}
function Frac(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
  Result := X - Trunc(X);
end;
{$ELSE !PUREPASCAL}
asm
        FLD     X
        FLD     ST(0)
        SUB     ESP,4
        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FRNDINT
        FWAIT
        FLDCW   [ESP].Word
        ADD     ESP,4
        FSUB
end;
{$ENDIF PUREPASCAL}
{$ENDIF CPUX64}


{$IF defined(CPUX64) or defined(PUREPASCAL) }
type
  coeffType = (cHi, cLo);
const
  ExpM1Iof64 : array[-64 .. 64, coeffType] of UInt64 = (
    ($bfe43a54e4e98864, $bc6ca8a4270fadf5), ($bfe40adf8d149383, $3c7bbea189925bac),
    ($bfe3daaae2395759, $bc57e46d4e1d1416), ($bfe3a9b3e10921cd, $bc8b67124284ea97),
    ($bfe377f77a0fcb45, $3c4c14c2e7d1c9a4), ($bfe345729182bf1f, $bc8eb70d83f2dfd5),
    ($bfe31221ff0f3ecc, $3c69a4cddcf699b7), ($bfe2de028da7dc59, $bc5edef1c1e9d2e5),
    ($bfe2a910fb51295a, $bc87fb56248af1d0), ($bfe27349f8ed96eb, $bc8c3e2eeddb6e92),
    ($bfe23caa2a088391, $3c8eccb4dac0c150), ($bfe2052e24a073a5, $3c8161c3e082442f),
    ($bfe1ccd270f070f9, $bc8f7cca01c669f0), ($bfe1939389388e3d, $bc74fb4ada0efe97),
    ($bfe1596dd9858ab1, $bc5ad6ddc4792f50), ($bfe11e5dbf7792a9, $bc561c5f183ad4be),
    ($bfe0e25f8a081941, $3c74bbf0f4cffc58), ($bfe0a56f794ec7a4, $bc895d31f71cd295),
    ($bfe06789be457e3b, $3c8f0422c59b7d45), ($bfe028aa7a8b63f2, $3c896b8384f1d3ee),
    ($bfdfd19b804dffbf, $bc63dcfaabcf5d2c), ($bfdf4fdf228eb2ad, $3c437f6cecd799a3),
    ($bfdecc17c0083500, $3c7be9229f8beebe), ($bfde463d1c396301, $bc43f3d38f393576),
    ($bfddbe46d96cd831, $3c75958a5d1795c7), ($bfdd342c7833133a, $bc6333023ef6f763),
    ($bfdca7e556da7e48, $bc7229736e5c8d3a), ($bfdc1968b0e55333, $bc7a21677c651b54),
    ($bfdb88ad9e7d52ea, $bbfb61343fc21a3b), ($bfdaf5ab13e5474f, $3c703c6e249a5882),
    ($bfda6057e0e846a4, $bc655b1912202da3), ($bfd9c8aab046af7a, $bc050456628532f0),
    ($bfd92e9a0720d3ec, $bc285314b9559e64), ($bfd8921c445f4add, $3c618bc865c78e3b),
    ($bfd7f327a018ddb2, $bc61a901b1ee2bd6), ($bfd751b22af608f0, $bc70a0249fec9629),
    ($bfd6adb1cd9205ee, $bc74edd8a92eb584), ($bfd6071c47d953b2, $bc7a771b057709c4),
    ($bfd55de73065b4df, $3c6572ec15fec83d), ($bfd4b207f3d79870, $bc6759d4ac130208),
    ($bfd40373d42ce2e3, $3c753d8db804c224), ($bfd3521fe8150d2b, $bc6b0648a5e34bb6),
    ($bfd29e011a428ec6, $bc6dabf5975c0c02), ($bfd1e70c28b987f3, $3c74e91dbb1734bd),
    ($bfd12d35a41ba104, $3c63445f7544e0ef), ($bfd07071eef11388, $bc309aa682553231),
    ($bfcf616a79dda3a8, $bc66b2eab63020c1), ($bfcddbe7247382af, $bc431eb13933e894),
    ($bfcc5041854df7d4, $bc6797d4686c5393), ($bfcabe60e1f21836, $bc36f8b82e653e2d),
    ($bfc9262c1c3430a1, $bc646ff6ec4a4251), ($bfc78789b0a5e0c0, $3c5e3a6bdaece8f9),
    ($bfc5e25fb4fde211, $3c064eec82915df3), ($bfc43693d679612d, $bc69da94a869862a),
    ($bfc2840b5836cf67, $bc685405051eb425), ($bfc0caab118a1278, $3c26ad4c353465b0),
    ($bfbe14aed893eef4, $3c4e1f58934f97af), ($bfba85e8c62d9c13, $bc5adf7745e77188),
    ($bfb6e8caff341fea, $bc59573ded7888b2), ($bfb33d1bb17df2e7, $bc4e19c873b1d6a8),
    ($bfaf0540438fd5c3, $bc2a1ce01f9f6ca7), ($bfa7723950130405, $3c2c677ad8fa478d),
    ($bf9f8152aee9450e, $3c24b00abf977627), ($bf8fc055004416db, $bc282ef422ab152a),
    ($0000000000000000, $0000000000000000),
    ($3f90202ad5778e46, $bc151e6d305beec6), ($3fa040ac0224fd93, $3c2c17a107575019),
    ($3fa89246d053d178, $3c44967f31eb2595), ($3fb082b577d34ed8, $bc45272ff30eed1b),
    ($3fb4cd4fc989cd64, $3c5557a8671b89e7), ($3fb92937074e0cd7, $bc5db0b9cc915fc5),
    ($3fbd96b0eff0e794, $bc475385b2cdf93d), ($3fc10b022db7ae68, $bc58c4a5df1ec7e5),
    ($3fc353bc9fb00b21, $3c66bae618011342), ($3fc5a5ac59b963cb, $bc6fd91307e74c50),
    ($3fc800f67b00d7b8, $3c27ab912c69ffeb), ($3fca65c0b85ac1a9, $3c6a9c189196f8cd),
    ($3fccd4315e9e0833, $bc2172c31a1781f1), ($3fcf4c6f5508ee5d, $3c646ef7b808180a),
    ($3fd0e7510fd7c564, $bc71c5b2e8735a43), ($3fd22d78f0fa061a, $bc789843c4964554),
    ($3fd378c3b0847980, $3c73b5223eca1712), ($3fd4c946033eb3de, $bc735d267d66dc96),
    ($3fd61f14f169ebc1, $bc789e2d87fd0d92), ($3fd77a45d8117fd5, $bc52bb36e6b3a2af),
    ($3fd8daee6a60c961, $3c6a4e618fb92468), ($3fda4124b2fe50cb, $3c6fb5f3020a46f5),
    ($3fdbacff156c79d7, $bc66c5366444681a), ($3fdd1e944f6fbdaa, $bc7dcb8749a64f6e),
    ($3fde95fb7a7a88f8, $bc7cc04a2491ed0a), ($3fe009a6068f6a8c, $bc8a29a322473bb6),
    ($3fe0cb4eee42c98b, $bc8f511ef22f5ad5), ($3fe190048ef60020, $bc81ed925f893d67),
    ($3fe257d334137dff, $3c8b99bfe7ce9f9e), ($3fe322c75a963b98, $3c73718f70534e8a),
    ($3fe3f0edb1d18acd, $bc87d3f40a146f34), ($3fe4c2531c3c0d38, $bc8b4690082a4906),
    ($3fe59704b03ddca9, $bc867241360f5dc1), ($3fe66f0fb901f2bd, $3c8176670eb83154),
    ($3fe74a81b74adcac, $bc7690fec61c7ca4), ($3fe82968624ac88d, $bc8de6aad7622def),
    ($3fe90bd1a87ef9a1, $bc5175cf7e67dfff), ($3fe9f1cbb08eb151, $3c8bf66c74a26ecd),
    ($3feadb64da2d9acf, $bc3c71005d68edd6), ($3febc8abbf01c781, $bc81abb427c68493),
    ($3fecb9af338d4a9c, $3c8912542fa88dd4), ($3fedae7e481b8284, $bc5e2a138ec80097),
    ($3feea72849b21ebd, $bc7a892e30b5188f), ($3fefa3bcc305f191, $3c7d2c3b6d08c659),
    ($3ff05225beb9ce55, $bc9d02df8ccf4707), ($3ff0d47240fe1412, $3c95ff940cd08c4d),
    ($3ff158cc0d22ca02, $bc94a9212ef0b185), ($3ff1df3b68cfb9ef, $3c9ea61ab771f73c),
    ($3ff267c8bb05d2a3, $3c5a3d1e659a0ad8), ($3ff2f27c8ca598a0, $3c7d4fe81eb0becb),
    ($3ff37f5f88f7b4e5, $3c9adcd8bb9947d9), ($3ff40e7a7e37aa30, $bc2bb8b0f3c94f34),
    ($3ff49fd65e20b96f, $3c93a5153fb80a4a), ($3ff5337c3e7cfe38, $3c7708e2df602f1d),
    ($3ff5c97559b6cc28, $bc78aecd517b37d5), ($3ff661cb0f6c564f, $3c8c221ff57b390a),
    ($3ff6fc86e505a9dd, $bc9a7d609b716bf6), ($3ff799b2864d0569, $bc9ba2fe6f7de08c),
    ($3ff83957c6099668, $bc54d703cb17b345), ($3ff8db809e9ca670, $3c92896ff654d054),
    ($3ff9803732a14221, $3c76a828a835e066), ($3ffa2785cd8e63ad, $3c787f474dbeb603),
    ($3ffad176e45bab25, $3c9af15018e5e729), ($3ffb7e151628aed3, $bc9655023a9dfd8c) );
{$IFEND defined(CPUX64) or defined(PUREPASCAL) }

{$IFDEF CPUX64}
function Exp(const X: Double): Double;
{$ELSE !CPUX64}
function Exp(const X: Extended): Extended;
{$ENDIF CPUX64}
{$IF defined(CPUX64) or defined(PUREPASCAL) }
const
  C64      : Double =  64.0;
  C1of2    : Double =  1/2;
  C1of64   : Double =  1/64;
  CExp : array[3..6] of UInt64 =
  ( $3fc5555555554855, $3fa5555555553b54,
    $3f811112b12b282f, $3f56c16e86e89028 );
{$IFEND defined(CPUX64) or defined(PUREPASCAL) }
{$IF defined(CPUX64)}
type
  m128 = record
    Hi, Lo : UInt64
  end align 16;
const
  CLog2ofE   : UInt64 = $3ff71547652b82fe;
  CLogEof2Hi : UInt64 = $3FE62E42FEFA4000;
  CLogEof2Lo : UInt64 = $3D48432A1B0E2634;
  C2powM1074p5 = $00000000000000001; // 2^(-1074.5)

  C1024    : Double =  1024.0;
  C1023p5  : Double =  1023.5;
  CM1075   : Double = -1075;
  CM1074p5 : Double = -1074.5;
  CM1of2   : Double = -1/2;
  C1       : Double =  1.0;
  CInf     : Double = cInfinity;
  CSignBit : UInt64 = $8000000000000000;
  CSignBitPD: m128 = (Hi:$8000000000000000; Lo:$8000000000000000);
asm
        .PARAMS 6
        .SAVENV XMM6
        .SAVENV XMM7

        MOVSD   [RBP+060H],XMM0
        MOVAPS  XMM5, XMM0
        MOVSD   XMM4, CLog2ofE
        MOVSD   XMM3, C1of2
        MOVZX   EAX, WORD PTR [RBP+066H]
        XOR     EDX, EDX
        TEST    AX, 07FF0H
        JZ      @@One
        CMP     AX, DX
        JL      @@X_IS_NEGATIVE

@@X_IS_POSITIVE:
        CMP     AX, 07FF0h
        JAE     @@X_IS_INF_OR_NAN
        CMP     AX, 04090H
        JAE     @@Overflow
        MULSD   XMM0, XMM4 // CLog2ofE
        MOVAPS  XMM7, XMM0
        ADDSD   XMM0, XMM3 // C1of2

        CVTTSD2SI EAX, XMM0
        CVTSI2SD  XMM6, EAX
        CMP     EAX, 03FFh
        JNG     @@REDUCTION0

@@X_IS_LARGE:
        COMISD  XMM7, C1024
        JNC     @@Overflow
        MOVSD   XMM6, C1023p5
        MOV     RAX, $7FE6A09E667F3BCD // C2pow1023p5
        MOV     [rbp+028h], RAX
        JMP     @@REDUCTION

@@X_IS_NEGATIVE:
        CMP     AX, 0FFF0H
        JAE     @@X_IS_INF_OR_NAN
        CMP     AX,0C090H
        JAE     @@Zero

        MULSD   XMM0, XMM4 // CLog2ofE
        MOVAPS  XMM7, XMM0
        SUBSD   XMM0, XMM3 // C1of2

        CVTTSD2SI EAX, XMM0
        CVTSI2SD  XMM6, EAX
        CMP     EAX, -0000003FEh
        JNL     @@REDUCTION0

@@X_IS_SMALL:
        MOVSD   XMM0, CM1075
        COMISD  XMM0, XMM7
        JNC     @@Zero
        MOVSD   XMM0, CM1074p5
        COMISD  XMM0, XMM7
        JC      @@X_IS_TOO_SMALL

        MOV     RAX,C2powM1074p5
        MOV     [RBP+028h], RAX
        MOVSD   XMM6, CM1074p5
        JMP     @@REDUCTION

@@X_IS_TOO_SMALL:
        MOV     ECX, -1022
        MOV     R9, 010000000000000h
        SUB     ECX, EAX
        SHR     R9, CL
        MOV     [RBP+028h], R9
        JMP     @@REDUCTION

@@One:  MOVSD   xmm0,C1
        JMP     @@exit

@@X_IS_INF_OR_NAN:
        MOV     RAX, [RBP+060h]
        MOV     RCX, 07FF0000000000000h
        CMP     RAX, RCX
        JZ      @@Overflow
        MOV     RCX, 0FFF0000000000000h
        CMP     RAX, RCX
        JNZ     @@Self

@@Zero: XORPD   XMM0, XMM0
        JMP     @@exit

@@Self: MOVAPS  XMM0, XMM5
        JMP     @@exit

@@Overflow:
        CALL    RaiseOverflowException
        MOVSD   XMM0, CInf
        JMP     @@exit

@@REDUCTION0:
        ADD     EAX, 03FFh
        MOVSXD  RAX, EAX
        SHL     RAX, 034H
        MOV     [RBP+028h], RAX

@@REDUCTION:
        LEA     RDX, [CExp]

        MOVAPS  XMM7, XMM6
        MULSD   XMM7, CLogEof2Hi
        MOVAPS  XMM2, XMM5
        SUBSD   XMM2, XMM7
        MULSD   XMM6, CLogEof2Lo
        MOVAPS  XMM0, XMM2
        ADDSD   XMM0, XMM6
        MOVAPD  XMM1, CSignBitPD
        MOVAPS  XMM4, XMM0
        MULSD   XMM0, C64
        ANDPD   XMM4, XMM1
        ORPD    XMM3, XMM4
        ADDSD   XMM0, XMM3

        MOVSD   XMM1, C1of64
        CVTTSD2SI EAX, XMM0

        MOVAPS  XMM3, XMM6
        MOVSD   XMM4, QWORD PTR [RDX]
        MOVSD   XMM5, QWORD PTR [RDX+08H]
        MOVSD   XMM6, QWORD PTR [RDX+10H]
        MOVSD   XMM7, QWORD PTR [RDX+18H]

        CVTSI2SD XMM0, EAX
        MOVSXD  RDX, EAX
        ADD     RDX, RDX
        LEA     RCX, [ExpM1Iof64 + 400H]
        LEA     RCX, [RCX + RDX * 8]

        MULSD   XMM0, XMM1
        SUBSD   XMM2, XMM0
        MOVAPS  XMM0, XMM2
        ADDSD   XMM0, XMM3
        MOVAPS  XMM1, XMM0
        MULSD   XMM1, XMM0
        MULSD   XMM6, XMM1
        MULSD   XMM7, XMM1
        ADDSD   XMM4, XMM6
        MOVSD   XMM6, C1of2
        ADDSD   XMM5, XMM7
        MULSD   XMM5, XMM0
        ADDSD   XMM4, XMM5
        MULSD   XMM0, XMM4
        MOVSD   XMM4, [RCX]
        MOVSD   XMM5, [RCX + 08h]
        ADDSD   XMM6, XMM0
        MOVSD   XMM0, c1
        MULSD   XMM1, XMM6
        ADDSD   XMM3, XMM1


        MOVAPS  XMM1, XMM4
        ADDSD   XMM4, XMM0
        MOVAPS  XMM6, XMM4
        SUBSD   XMM4, XMM0
        SUBSD   XMM1, XMM4
        ADDSD   XMM1, XMM5
        MOVAPS  XMM7, XMM6
        ADDSD   XMM7, XMM1
        MOVAPS  XMM4, XMM7
        SUBSD   XMM7, XMM6
        SUBSD   XMM1, XMM7

        MOVSD   XMM0, [RBP+028H]
        MOVAPS  XMM7, XMM3
        MULSD   XMM3, XMM1
        MOVAPS  XMM6, XMM2
        MULSD   XMM6, XMM1
        ADDSD   XMM3, XMM6
        ADDSD   XMM3, XMM1
        MULSD   XMM7, XMM4
        ADDSD   XMM3, XMM7
        MULSD   XMM2, XMM4
        ADDSD   XMM3, XMM2
        ADDSD   XMM3, XMM4
        MULSD   XMM0, XMM3
@@Exit:
end;
{$ELSEIF defined(PUREPASCAL) }

const
  ExpUpperLimit = 710.0; // 1024 * Ln(2) + epsilon
  ExpLowerLimit = -745.0; //-1074 * Ln(2) + epsilon
  Log2ofE   : UInt64 = $3ff71547652b82fe;
  Log2ofEHi : UInt64 = $3ff71547652b0000;
  Log2ofELo : UInt64 = $3da05fc2eefa1ffb;
  LogEof2Hi : UInt64 = $3FE62E42FEFA4000;
  LogEof2Lo : UInt64 = $3D48432A1B0E2634;
  C2pow1023p5 : UInt64 = $7fe6a09e667f3bcd; // 2^(1023.5)
  C2powM1074p5 : UInt64 = $00000000000000001; // 2^(-1074.5)
type
  TWords = Array[0..3] of Word;
  PWords = ^TWords;
var
  q: Integer;
  qd, X0, X1, X2, X3, f: Double;
  I, Ind: UInt64;
  W: Word;
  yHi, yLo, y, Y2,
  q0, q1, q2, qHi, qLo, pLo, pHi : Double;
  t, u, v, r: Double;
begin
  W := PWords(@X)^[3] and $7FF0;
  if (W = $0000) then Exit(1.0); // +/-Zero and +/-Denormal
  if (W = $7FF0) then
  begin
    I := PUInt64(@X)^;
    if I = $7FF0000000000000 then
    begin
      RaiseOverflowException;
      Exit( cInfinity );
    end
    else if I = $FFF0000000000000 then
      Exit(0.0)
    else
      Exit(X);
  end;

  if X >= 0 then
  begin
    if X > ExpUpperLimit then
    begin
      RaiseOverflowException;
      Exit( cInfinity );
    end;
    X0 := X * PDouble(@Log2ofE)^;
    q := Trunc(X0 + 0.5); qd := q;
    if q <= 1023 then
    begin
      PUInt64(@f)^ := (UInt64(q + 1023)) shl 52; //    f := 2 ^ q;
    end
    else
    begin
      if X0 >= 1024.0 then
      begin
        RaiseOverflowException;
        Exit( cInfinity ); // +Inf
      end;
      qd := 1023.5;
      f := PDouble(@C2pow1023p5)^;
    end;
  end
  else
  begin
    if X < ExpLowerLimit then
      Exit( 0.0 );
    X0 := X * PDouble(@Log2ofE)^;
    q := Trunc(X0 - 0.5); qd := q;
    if q >= -1022 then
    begin
      PUInt64(@f)^ := (UInt64(q + 1023)) shl 52; //    f := 2 ^ q;
    end
    else
    begin
      if X0 <= -1075.0 then
        Exit( 0.0 )
      else if X0 <= -1074.5 then
      begin
        f := PDouble(@C2powM1074p5)^;
        qd := -1074.5;
      end
      else
        PUInt64(@f)^ := UInt64($10000000000000) shr (-1022 -  q);
    end;
  end;

  x1 := X - qd * PDouble(@LogEof2Hi)^;
  x2 := qd * PDouble(@LogEof2Lo)^;
  X3 := x1 + x2;
  if X3 >= 0 then
    Ind := Trunc(X3 * C64 + C1of2)
  else
    Ind := Trunc(X3 * C64 - C1of2);
  yHi := x1 - Ind * C1of64;
  yLo := x2;

  y :=  yHi + yLo;
  y2 := y * y;

  q1 :=           PDouble(@CExp[6])^;
  q2 :=           PDouble(@CExp[5])^;
  q1 := q1 * y2 + PDouble(@CExp[4])^;
  q2 := q2 * y2 + PDouble(@CExp[3])^;
  q0 := (q1 * y + q2) * y + c1of2;

  qLo := q0 * y2 + yLo;
  qHi := yHi;

  pHi :=  PDouble(@ExpM1Iof64[Ind, cHi])^;
  pLo :=  PDouble(@ExpM1Iof64[Ind, cLo])^;

  t := pHi + 1.0; v := t - 1.0; r := pHi - v; u := r + pLo;
  pHi := t + u; v := pHi - t;
  pLo := u - v;

  Result := 0;
  Result := Result + qLo * pLo;
  Result := Result + qHi * pLo;
  Result := Result + pLo;
  Result := Result + qLo * pHi;
  Result := Result + qHi * pHi;
  Result := Result + pHi;
  Result := f * Result;
end;
{$ELSE}
asm
        {       e**x = 2**(x*log2(e))   }
        FLD     X
        FLDL2E              { y := x*log2e;      }
        FMUL
        FLD     ST(0)       { i := round(y);     }
        FRNDINT
        FSUB    ST(1), ST   { f := y - i;        }
        FXCH    ST(1)       { z := 2**f          }
        F2XM1
        FLD1
        FADD
        FSCALE              { result := z * 2**i }
        FSTP    ST(1)
end;
{$IFEND}

{$IFDEF CPUX64}
function ExpMinus1(const X: Double): Double;
{$ELSE !CPUX64}
function ExpMinus1(const X: Extended): Extended;
{$ENDIF CPUX64}

{$IF defined(CPUX64) or defined(PUREPASCAL) }
const
  CExp : array[3..6] of UInt64 =
  ( $3fc5555555554855, $3fa5555555553b54,
    $3f811112b12b282f, $3f56c16e86e89028 );
  C64 : Double = 64.0;
  C1of2  : Double= 1/2;
  C1of64 : Double = 1/64;
  CLogEof2 : UInt64= $3fe62e42fefa39ef; // Ln(2);
  CZero : Double = 0.0;
  C1 : Double = 1.0;
{$IFEND defined(CPUX64) or defined(PUREPASCAL) }
{$IF defined(CPUX64)}
type
  m128 = record
    Hi, Lo : UInt64
  end align 16;
const
  CSignBitPD: m128 = (Hi:$8000000000000000; Lo:$8000000000000000);
asm
        .PARAMS 1
        .SAVENV XMM6
        .SAVENV XMM7

        MOVSD   [RBP+050H],XMM0
        MOVZX   EAX, WORD PTR [RBP+056H]

        MOVAPD  XMM2, CSignBitPD
        MOVAPS  XMM4, XMM2
        XORPD   XMM5, XMM5 // Zero
        MOVAPS  XMM6, XMM0
        ANDNPD  XMM2, XMM0 // Abs(X) : XMM2 := (Not XMM2) and XMM0

        UCOMISD xmm0, xmm5 // 0.0
        JPE     @@01
        JZ      @@Zero

@@01:
        AND     EAX, 07FF0H // Is X Denormal ?
        JZ      @@Exit
        CMP     AX, 07FF0H
        JE      @@UseEXP
        COMISD  XMM2, CLogEof2
        JC      @@ExpMinusOne

@@UseEXP:
        MOVSD   XMM7, C1
        CALL    Exp
        SUBSD   XMM0, XMM7
        JMP     @@Exit

@@Zero:
        MOVAPS  XMM0, XMM5 // 0.0
        JMP     @@Exit

@@ExpMinusOne:
        ANDPD   XMM4, XMM6 // XMM4 has sign bit.

        MOVSD   XMM7, C1of2
        ORPD    XMM7, XMM4

        MULSD   XMM6, C64
        ADDSD   XMM6, XMM7
        CVTTSD2SI EAX, XMM6

        LEA     RDX, [CExp]
        CVTSI2SD XMM7, EAX
        MOVSXD  RAX, EAX
        ADD     RAX, RAX
        LEA     RCX, [ExpM1Iof64 + 400H]
        LEA     RCX, [RCX + RAX * 8]

        MULSD   XMM7, C1of64
        MOVSD   XMM4, QWORD PTR [rdx]
        MOVSD   XMM5, QWORD PTR [rdx + 08H]
        SUBSD   XMM0, XMM7
        MOVAPS  XMM1, XMM0
        MULSD   XMM0, XMM0
        MOVSD   XMM6, QWORD PTR [rdx + 10H]
        MOVSD   XMM7, QWORD PTR [rdx + 18H]
        MOVSD   XMM2,[C1of2]

        MULSD   XMM7, XMM0
        MULSD   XMM6, XMM0
        ADDSD   XMM7, XMM5
        ADDSD   XMM6, XMM4

        MOVSD   XMM3, [RCX]
        MOVSD   xmm5, [RCX+08h]

        MULSD   XMM7, XMM0
        MULSD   XMM6, XMM1
        ADDSD   XMM7, XMM6
        ADDSD   XMM7, XMM2
        MULSD   XMM7, XMM0

        ADDSD   XMM5, XMM7
        MOVAPS  XMM2, XMM1
        ADDSD   XMM2, XMM7
        MOVAPS  XMM0, XMM3
        MULSD   XMM0, XMM2
        ADDSD   XMM0, XMM5
        ADDSD   XMM0, XMM1
        ADDSD   XMM0, XMM3
@@Exit:
end;

{$ELSEIF defined(PUREPASCAL) }
var
  y, Y2,
  q0, q1,
  q, qHi, qLo,
  pLo, pHi  : Double;
  I : Integer;
  W: Word;
type
  TWords = Array[0..3] of Word;
  PWords = ^TWords;
begin
  if (Abs(x) = 0.0) then Exit(0.0); // +/-Zero
  W := PWords(@X)^[3] and $7FF0;
  if W = 0 then Exit(X) // Denormal
  else if (W = $7FF0) or (Abs(X) >= PDouble(@CLogEof2)^) then
    Result := Exp(X) - 1
  else
  begin
    if X >= 0 then        // ABs(X) * 64 <= 22.18..
      I := Trunc(X * C64 + C1of2)
    else
      I := Trunc(X * C64 - C1of2);

    y := X - I * C1of64; // abs(y) <= 1/128
    y2 := y * y;

    q0 :=           PDouble(@CExp[6])^;
    q1 :=           PDouble(@CExp[5])^;
    q0 := q0 * y2 + PDouble(@CExp[4])^;
    q1 := q1 * y2 + PDouble(@CExp[3])^;

    q0 := q0 * y2;
    q1 := q1 * y;
    q := q0 + q1;
    q := q + c1of2;

    qLo := q * y2;
    qHi := y;

    pHi :=  PDouble(@ExpM1Iof64[I, cHi])^;
    pLo :=  PDouble(@ExpM1Iof64[I, cLo])^;

    Result := 0;
    Result := Result + pLo;
    Result := Result + qLo;
    Result := Result + pHi * (qHi + qLo);
    Result := Result + qHi;
    Result := Result + pHi;
  end;
end;
{$ELSE}
asm
        FLD     X
        FLDL2E              { y := x*log2e;      }
        FMUL
        FLD     ST(0)
        FABS
        FLD1
        FCOMPP
        FSTSW   AX
        SAHF
        JAE     @@1

        FLD     ST(0)       { i := round(y);     }
        FRNDINT
        FSUB    ST(1), ST   { f := y - i;        }
        FXCH    ST(1)       { z := 2**f          }
        F2XM1
        FLD1
        FADD
        FSCALE              { result := z * 2**i }
        FSTP    ST(1)
        FLD1
        FSUBP
        JMP     @@EXIT
@@1:    F2XM1
@@EXIT:
end;
{$IFEND}

{$IFDEF PUREPASCAL}
{$IFDEF CPUX64}
function pRemDouble(D: Double; var X, Y: Double): integer;
var
  PiOf2H, PiOf2M, PiOf2L : Double;
  lowestPart: double;
const
  PiOf2Hi  : UInt64 = $3FF921FB54442D18; // Pi/2 : first 53bits
  PiOf2Mi  : UInt64 = $3C91A62633145C04; // Pi/2 : Middle 53bits
  PiOf2Lo  : UInt64 = $396707344A409382; // Pi/2 : Lowest 53bits
  ExpOffset1 =  54; // $3FF - $3C9
  ExpOffset2 = 105; // $3FF - $396

  procedure adddouble(const x: double; var rh, rl : double); inline;
  var
    hh, hl: double;
    temp : double;
  begin
    hh := x + rh;
    temp := hh - x;
    hl := (x - (hh - temp)) + (rh - temp);
    hl := hl + rl;
    rh := hh + hl;
    rl := hl - (rh - hh);
  end;

  procedure AddDouble2(const xh, xl: Double; var rh, rl : Double); inline;
  var
    hh, hl: Double;
    temp : double;
  begin
    hh := xh + rh;
    temp := hh - xh;
    hl := (xh - (hh - temp)) + (rh - temp);
    hl := hl + xl + rl;
    rh := hh + hl;
    rl := hl - (rh - hh);
  end;

  // 3*Double precision reduction
  procedure ReductionPi2n(n: integer);
  var
    X0,
    T0, T1, T2, T3: Double;
  begin
    PDoubleRec(@PiOf2H).Exp := n + $3FF;
    PDoubleRec(@PiOf2M).Exp := n + $3FF - ExpOffset1;
    PDoubleRec(@PiOf2L).Exp := n + $3FF - ExpOffset2;

    T0 := -PiOf2L;
    T1 := 0;
    AddDouble( lowestPart, T0, T1);
    AddDouble(-PiOf2M,     T0, T1);
    AddDouble( Y,          T0, T1); // T0&1 - partial sum

    T2 := T0;
    T3 := T1;
    AddDouble(-PiOf2H, T0, T1);
    AddDouble( X,      T0, T1); // T0&1 - X&Y

    X0 := X;
    X := T0;
    Y := T1;

    T0 := -T0;
    T1 := -T1;
    AddDouble( X0,     T0, T1);
    AddDouble(-PiOf2H, T0, T1);
    AddDouble2(T2,T3,  T0, T1);

    lowestPart := T0;
  end;

const
  nPi2High  : UInt64 = $BFF921FB54400000; // Pi/2 : Upper 32bit.
  nPi2Low   : UInt64 = $BDD0B4611A600000; // Pi/2 : Lower 32bit
  nPi2Small : UInt64 = $BBA3198A2E037073; // Pi - Pi2High - Pi2Low
  TwoOfPi  : UInt64 = $3FE45F306DC9C883; // 2 / Pi
  TwoPow22 : UInt64 = $4150000000000000; // 2 ^ 22
  TwoPow54 : UInt64 = $4350000000000000; // 2 ^ 54
var
  D2 : Double;
  Q: Int64;
  n: integer;
  hh, hl,
  pHi, pLow, pSmall,
  X2, Y2,
  temp : double;
begin
  Result := 0;
  X := D;
  Y := 0;

  D2 := Abs(D);
  if D2 <= Pi / 4 then Exit
  else if D2 <= 5 * Pi/4 then
  begin
    if D2 <= 3 * Pi/4 then Q := 1
    else Q := 2;
    if D < 0 then Q := -Q;

    pHi := Q * PDouble(@PiOf2Hi)^;
    pLow := Q * PDouble(@PiOf2Mi)^;
    pSmall := Q * PDouble(@PiOf2Lo)^;

    X2 := D - pHi;
    X := X2 - pLow;
    Y := - pLow - (X - X2);
    Y := Y - pSmall;
  end
  else if D2 < PDouble(@TwoPow22)^ then
  begin
    // Reduction for small/mid number (up to 2^22)
    Q := Trunc((D2 - Pi/4) * PDouble(@TwoOfPi)^) + 1;

    if D < 0 then Q := -Q;
    pHi := Q * PDouble(@nPi2High)^;
    pLow := Q * PDouble(@nPi2Low)^;
    pSmall := Q * PDouble(@nPi2Small)^;

    X2 := D + pHi;

    hh := pLow + X2;
    temp := hh - pLow;
    hl := (pLow - (hh - temp)) + (X2 - temp);
    X2 := hh + hl;
    Y2 := hl - (X2 - hh);

    hh := pSmall + X2;
    temp := hh - pSmall;
    hl := (pSmall - (hh - temp)) + (X2 - temp);
    hl := hl + Y2;
    X2 := hh + hl;
    Y2 := hl - (X2 - hh);

    X := X2;
    Y := Y2;
  end
  else
  begin
    // Reduction for large numbber between 2^22 to 2^53
    n := PDoubleRec(@D2).Exp - $3FF;
    PUInt64(@PiOf2H)^ := PiOf2Hi;
    PUInt64(@PiOf2M)^ := PiOf2Mi;
    PUInt64(@PiOf2L)^ := PiOf2Lo;

    X := D2;
    Y := 0;
    lowestPart := 0;

    while n >= 3 do
    begin
      PDoubleRec(@PiOf2H).Exp := n + $3FF;
      if PiOf2H < X then
        ReductionPi2n(n);
      Dec(n);
    end;

    Q := 0;
    while n >= 0 do
    begin
      PDoubleRec(@PiOf2H).Exp := n + $3FF;
      if PiOf2H < X then
      begin
        Q := Q or (1 shl n);
        ReductionPi2n(n);
      end;
      Dec(n);
    end;

    if X > Pi/4 then
    begin
      Q := Q + 1;
      ReductionPi2n(0);
    end;
    if D < 0 then
    begin
      X := -X;
      Y := -Y;
      Q := -Q;
    end;
  end;
  Result := Q and 3;
end;
{$ENDIF CPUX64}

{$IFDEF CPUX86}
function pRemExtended(D: Extended; var R: Extended): integer;
const
  Pi2High = 1.5707963267341256;    // 3FF921FB54400000
  Pi2Low  = 6.077100506303966e-11; // 3DD0B4611A600000
  TwoOfPi = 0.636619772367581343;   // $3FE45F306DC9C883
var
  T : Double;
begin
  Result := 0;
  R := D;

  if Abs(D) <= Pi / 4 then
    Exit

  else if Abs(D) <= 3 * Pi/4 then Result := 1
  else if Abs(D) <= 5 * Pi/4 then Result := 2
  else
    Result := Trunc((Abs(D) - Pi/4) * TwoOfPi) + 1;

  if D < 0 then Result  := -Result;

  T := D - Pi2High * Result;
  R := T - Pi2Low * Result;
end;
{$ENDIF CPUX86}

{$IFDEF CPUX64}
function pCosDouble(const x, y: Double) : Double;
const
  CCos : ARRAY[0..5] OF UINT64 =
  ( $BDA8FA6A8A7D84DF,
    $3E21EE9DC12C88AC,
    $BE927E4F7F1EE922,
    $3EFA01A019C8F945,
    $BF56C16C16C15018,
    $3FA555555555554B );
var
  r1, r2, s, t, u, v,
  L, L1, L2,
  D2, D4 : Double;
begin
  D2 := x * x;
  D4 := D2 * D2;

  L1 :=           PDouble(@CCos[0])^;
  L2 :=           PDouble(@CCos[1])^;
  L1 := L1 * D4 + PDouble(@CCos[2])^;
  L2 := L2 * D4 + PDouble(@CCos[3])^;
  L1 := L1 * D4 + PDouble(@CCos[4])^;
  L2 := L2 * D4 + PDouble(@CCos[5])^;

  L := L2 + L1 * D2;
  L := L * D4;

  s := 1.0;
  t := D2 * 0.5;
  u := s - t;
  v := u - s;
  r1 := t + v;

  r2 := x * y;
  r2 := L - r2;
  r2 := r2 - r1;
  Result := u + r2;
end;
{$ENDIF CPUX64}

{$IFDEF CPUX86}
function pCosExtended(const D: extended) : extended;
const
  CCos : array[0..7] of TExtendedRec =
  ( (Words:($0000, $0000, $0000, $0000, $0000)),
    (Words:($FF10, $FFFF, $FFFF, $FFFF, $BFFD)),
    (Words:($AA60, $AAA9, $AAAA, $AAAA, $3FFA)),
    (Words:($FE00, $09CC, $60B6, $B60B, $BFF5)),
    (Words:($8000, $9377, $00CD, $D00D, $3FEF)),
    (Words:($0000, $C3A0, $7B99, $93F2, $BFE9)),
    (Words:($0000, $F000, $BB10, $8F74, $3FE2)),
    (Words:($0000, $0000, $8030, $C7BD, $BFDA)) );
var
  I: Integer;
  D2 : extended;
begin
  D2 := D * D;
  Result := PExtended(@CCos[High(CCos)])^;
  for I := High(CCos)-1 downto 0 do Result := Result * D2 + PExtended(@CCos[I])^;
end;
{$ENDIF CPUX86}

{$IFDEF CPUX64}
function pSinDouble(const x,y: Double) : Double;
const
  CSin : array[0..7] of UInt64 =
  ( $3DE5E0A28E7FA626,
    $BE5AE60081AA5E86,
    $3EC71DE37936614A,
    $BF2A01A019E80E58,
    $BC29D73D633765DD, $3F81111111110BA5,
    $3C6A74A934AD37D5, $BFC5555555555555 );
var
  D2, D3, D4,
  L, L1, L2: Double;
begin
  D2 := x * x;
  D4 := D2 * D2;
  D3 := D2 * x;

  L1 :=           PDouble(@CSin[0])^;
  L2 :=           PDouble(@CSin[1])^;
  L1 := L1 * D4 + PDouble(@CSin[2])^;
  L2 := L2 * D4 + PDouble(@CSin[3])^;

  L1 := L1 * D4 + PDouble(@CSin[4])^; // C1_Low
  L1 := L1      + PDouble(@CSin[5])^; // C1_Hi
  L2 := L2 * D4 + PDouble(@CSin[6])^; // C0_Low
  L  := L1 * D2 + L2;
  L  := L       + PDouble(@CSin[7])^; // C0_Hi

  L := L * D3;
  L := L + (1 - D2 *0.5) * y;

  Result := x + L;
end;
{$ENDIF CPUX64}

{$IFDEF CPUX86}
function pSinExtended(const D: extended) : extended;
const
  CSin : array[0..7] of TExtendedRec =
  ( (Words:($0000, $0000, $0000, $0000, $0000)),
    (Words:($AAD8, $AAAA, $AAAA, $AAAA, $BFFC)),
    (Words:($00C0, $8889, $8888, $8888, $3FF8)),
    (Words:($3000, $0E4B, $00D0, $D00D, $BFF2)),
    (Words:($0000, $21B8, $1D2C, $B8EF, $3FEC)),
    (Words:($0000, $8C00, $2C81, $D732, $BFE5)),
    (Words:($0000, $0000, $450C, $B092, $3FDE)),
    (Words:($0000, $0000, $D500, $D645, $BFD6)) );
var
  I: Integer;
  D2 : extended;
begin
  D2 := D * D;
  Result := PExtended(@CSin[High(CSin)])^;
  for I := High(CSin)-1 downto 0 do Result := Result * D2 + PExtended(@CSin[I])^;
  Result := D * Result;
end;
{$ENDIF CPUX86}
{$ENDIF PUREPASCAL}

{$IFDEF CPUX64}
function Cos(const X: Double): Double;
var
  Q: integer;
  Y,Z: Double;
begin
  if Abs(x) < Pi/4 then
    Result := pCosDouble(X, 0)
  else
  begin
    Q := pRemDouble(X, Y, Z);
    case Q of
      0: Result :=  pCosDouble(Y, Z);
      1: Result := -pSinDouble(Y, Z);
      2: Result := -pCosDouble(Y, Z);
      3: Result :=  pSinDouble(Y, Z);
      else Result := 0; // avoid warning W1035 Return value of function '%s' might be undefined
    end;
  end;
end;
{$ELSE  CPUX64}
function Cos(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
var
  Q: integer;
  R: Extended;
begin
  if Abs(x) < Pi/4 then
    Result := pCosExtended(X)
  else
  begin
    Q := pRemExtended(X, R);
    case Q and $3 of
      0: Result :=  pCosExtended(R);
      1: Result := -pSinExtended(R);
      2: Result := -pCosExtended(R);
      3: Result :=  pSinExtended(R);
      else Result := 0; // avoid warning W1035 Return value of function '%s' might be undefined
    end;
  end;
end;
{$ELSE  PUREPASCAL}
asm
        FLD     X
        FCOS
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function Sin(const X: Double): Double;
var
  Q: integer;
  Y,Z: Double;
begin
  if Abs(x) < Pi/4 then
    Result := pSinDouble(X, 0)
  else
  begin
    Q := pRemDouble(X, Y, Z);
    case Q of
      0: Result :=  pSinDouble(Y, Z);
      1: Result :=  pCosDouble(Y, Z);
      2: Result := -pSinDouble(Y, Z);
      3: Result := -pCosDouble(Y, Z);
      else Result := 0; // avoid warning W1035 Return value of function '%s' might be undefined
    end;
  end;
end;
{$ELSE  CPUX64}
function Sin(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
var
  Q: integer;
  R: Extended;
begin
  if Abs(x) < Pi/4 then
    Result := pSinExtended(X)
  else
  begin
    Q := pRemExtended(X, R);
    case Q and $3 of
      0: Result :=  pSinExtended(R);
      1: Result :=  pCosExtended(R);
      2: Result := -pSinExtended(R);
      3: Result := -pCosExtended(R);
      else Result := 0; // avoid warning W1035 Return value of function '%s' might be undefined
    end;
  end;
end;
{$ELSE  PUREPASCAL}
asm
        FLD     X
        FSIN
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function Tangent(const X: Double): Double;
var
  Q: integer;
  Y,Z: Double;
begin
  if Abs(x) < Pi/4 then
    Result := pSinDouble(X,0) / pCosDouble(X,0)
  else
  begin
    Q := pRemDouble(X, Y, Z);
    case Q of
      0: Result :=  pSinDouble(Y, Z) /  pCosDouble(Y, Z);
      1: Result :=  pCosDouble(Y, Z) / -pSinDouble(Y, Z);
      2: Result := -pSinDouble(Y, Z) / -pCosDouble(Y, Z);
      3: Result := -pCosDouble(Y, Z) /  pSinDouble(Y, Z);
      else Result := 0; // avoid warning W1035 Return value of function '%s' might be undefined
    end;
  end;
end;
{$ELSE CPUX86}
function Tangent(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
var
  Q: integer;
  R: Extended;
begin
  if Abs(x) < Pi/4 then
    Result := pSinExtended(X) / pCosExtended(X)
  else
  begin
    Q := pRemExtended(X, R);
    case Q and $3 of
      0: Result :=  pSinExtended(R) /  pCosExtended(R);
      1: Result :=  pCosExtended(R) / -pSinExtended(R);
      2: Result := -pSinExtended(R) / -pCosExtended(R);
      3: Result := -pCosExtended(R) /  pSinExtended(R);
      else Result := 0; // avoid warning W1035 Return value of function '%s' might be undefined
    end;
  end;
end;
{$ELSE  PUREPASCAL}
asm // StackAlignSafe
        FLD    X
        FPTAN
        FSTP   ST(0)      { FPTAN pushes 1.0 after result }
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
procedure SineCosine(const X: Double; var Sin, Cos: Double);
var
  Q: integer;
  Y,Z: Double;
begin
  if Abs(x) < Pi/4 then
  begin
    Sin := pSinDouble(X, 0);
    Cos := pCosDouble(X, 0);
  end
  else
  begin
    Q := pRemDouble(X, Y, Z);
    case Q of
      0:
        begin
          Sin :=  pSinDouble(Y, Z);
          Cos :=  pCosDouble(Y, Z);
        end;
      1:
        begin
          Sin :=  pCosDouble(Y, Z);
          Cos := -pSinDouble(Y, Z);
        end;
      2:
        begin
          Sin := -pSinDouble(Y, Z);
          Cos := -pCosDouble(Y, Z);
        end;
      3:
        begin
          Sin := -pCosDouble(Y, Z);
          Cos :=  pSinDouble(Y, Z);
        end;
    end;
  end;
end;
{$ELSE CPUX86}
procedure SineCosine(const X: Extended; var Sin, Cos: Extended);
{$IFDEF PUREPASCAL}
begin
  Sin := System.Sin(X);
  Cos := System.Cos(X);
end;
{$ELSE  PUREPASCAL}
asm // StackAlignSafe
        FLD     X
        FSINCOS
        FSTP    tbyte ptr [edx]    // Cos
        FSTP    tbyte ptr [eax]    // Sin
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function pLnXP1Double(frac: Double) : Double;
{$ELSE !CPUX64}
function pLnXP1(frac: extended) : extended;
{$ENDIF CPUX64}
{$IF defined(CPUX64) or defined(PUREPASCAL) }
const
  CLn : array[0..11] of UInt64 =
  ( $3fb0c835bd538f8c,
    $3fa988ab1ef94f29,
    $3fae3c098f53b5ee,
    $3fb11053fca919d4,
    $3fb3b140d1e4050f,
    $3fb745d159460dd7,
    $3fbc71c71cbb0290,
    $3fc249249248f262,
    $3c602e384367005f, $3fc99999999999b4,
    $3c750aaac32dac90, $3fd5555555555555 );
{$IFEND defined(CPUX64) or defined(PUREPASCAL) }
{$IF defined(CPUX64)}
const
  C2 : Double= 2.0;
  C1of2 : Double = 0.5;
asm
        .SAVENV XMM6
        .SAVENV XMM7

        LEA     RCX,  [C2]
        MOVAPD  XMM1, XMM0
        ADDSD   XMM0, [RCX]
        LEA     RDX,  [C1of2]
        MOVAPD  XMM2, XMM1
        DIVSD   XMM2, XMM0
        LEA     RAX,  [CLn]
        MOVAPD  XMM0, XMM1
        MULSD   XMM0, [RDX]
        MOVAPD  XMM4, XMM1
        MULSD   XMM4, XMM0
        MOVAPD  XMM3, XMM4
        MULSD   XMM3, XMM0
        MOVSD   XMM6, [RAX]
        MOVSD   XMM0, [RAX + 008H]
        MOVAPD  XMM5, XMM2
        MULSD   XMM5, XMM2
        MOVAPD  XMM7, XMM5
        MULSD   XMM7, XMM5
        MULSD   XMM6, XMM7
        MULSD   XMM0, XMM7
        ADDSD   XMM6, [RAX + 010H]
        ADDSD   XMM0, [RAX + 018H]
        MULSD   XMM6, XMM7
        MULSD   XMM0, XMM7
        ADDSD   XMM6, [RAX + 020H]
        ADDSD   XMM0, [RAX + 028H]
        MULSD   XMM6, XMM7
        MULSD   XMM0, XMM7
        ADDSD   XMM6, [RAX + 030H]
        ADDSD   XMM0, [RAX + 038H]
        MULSD   XMM6, XMM7
        ADDSD   XMM6, [RAX + 040H]
        ADDSD   XMM6, [RAX + 048H]
        MULSD   XMM0, XMM7
        ADDSD   XMM0, [RAX + 050H]
        MULSD   XMM6, XMM5
        ADDSD   XMM0, XMM6
        ADDSD   XMM0, [RAX + 058H]
        MULSD   XMM0, [RCX]
        MULSD   XMM0, XMM5
        MULSD   XMM0, XMM2
        MULSD   XMM2, XMM3
        ADDSD   XMM0, XMM3
        SUBSD   XMM0, XMM4
        SUBSD   XMM0, XMM2
        ADDSD   XMM0, XMM1
end;
{$ELSEIF defined(PUREPASCAL) }
var
  Y, Y2, Y4: Double;
  R1, R2, F1, F2, F3: Double;
begin
  Y := frac /(frac + 2.0);
  Y2 := Y * Y;
  Y4 := Y2 * Y2;

  R1 :=         + PDouble(@CLn[0])^;
  R2 :=         + PDouble(@CLn[1])^;
  R1 := R1 * Y4 + PDouble(@CLn[2])^;
  R2 := R2 * Y4 + PDouble(@CLn[3])^;
  R1 := R1 * Y4 + PDouble(@CLn[4])^;
  R2 := R2 * Y4 + PDouble(@CLn[5])^;
  R1 := R1 * Y4 + PDouble(@CLn[6])^;
  R2 := R2 * Y4 + PDouble(@CLn[7])^;
  R1 := R1 * Y4 + PDouble(@CLn[8])^;
  R1 := R1      + PDouble(@CLn[9])^;
  R2 := R2 * Y4 + PDouble(@CLn[10])^;
  R2 := R2 + R1 * Y2;
  R2 := R2      + PDouble(@CLn[11])^;

  F1 := frac * 0.5;
  F2 := frac * F1;
  F3 := F2 * F1;
  Result := ((((R2 * 2 * Y2 * Y) + F3)- F2) - Y * F3)+ frac;
end;
{$ELSE}
const
  CLn : array[0..8] of TExtendedRec =
  ( (Words:($0000, $0000, $0000, $0000, $0000)),
    (Words:($AD00, $AAAA, $AAAA, $AAAA, $3FF9)),
    (Words:($E800, $CCD2, $CCCC, $CCCC, $3FF4)),
    (Words:($C000, $30E9, $2492, $9249, $3FF0)),
    (Words:($0000, $EA68, $3912, $E38E, $3FEB)),
    (Words:($0000, $1200, $75AB, $BA2E, $3FE7)),
    (Words:($0000, $C800, $6569, $9D8F, $3FE3)),
    (Words:($0000, $8000, $6B7E, $87C0, $3FDF)),
    (Words:($0000, $0000, $D5A8, $8723, $3FDB)) );
var
  y, y2 : extended;
  I : integer;
begin
  y := frac /(frac + 2);
  y2 := y* y;
  Result := PExtended(@CLn[High(CLn)])^;
  for I := High(CLn)-1 downto 0 do
    Result := Result * y2 + PExtended(@CLn[I])^;
  Result := (Result * 2 * y) + 2 * y;
end;
{$IFEND}

{$IFDEF CPUX64}
function Ln(const X: Double): Double;
const
//  Ln2 = 0.69314718055994530941723212145818; // Ln(2)
  Ln2Hi: UInt64  = $3FE62E42FC000000; // High 27bits of Ln(2)
  Ln2Lo: UInt64  = $3E37D1CF79ABC9E4; // rest of Ln(2)
var
  X2: Double;
  frac : UInt64;
  exponent : integer;
  E: Double;
  U: UInt64;
  W: Word;
begin
  X2 := X;
  U := PUInt64(@X2)^;
  W := PDoubleRec(@X2)^.Words[3];
  if W >= $8000 then
  begin
    if U = $8000000000000000 then
    begin
      RaiseZeroDivideException;
      Exit(cNInfinity);
    end
    else if U <= $FFF0000000000000 then
      RaiseInvalidOpException;
    Exit(cNaN);
  end
  else
  begin
    if U = $0000000000000000 then
    begin
      RaiseZeroDivideException;
      Exit(cNInfinity);
    end
    else if U < $7FF0000000000000 then
    begin
      W := (W and $7FF0) shr 4;
      if W = 0 then // Denormal
      begin
        frac := U and $000FFFFFFFFFFFFF;
        exponent := -1022;
        if frac and (UInt64($FFFFFFFF) shl 21) = 0 then
        begin
          frac := frac shl 32;
          Dec(exponent, 32);
        end;
        if frac and (UInt64($FFFF) shl 37) = 0 then
        begin
          frac := frac shl 16;
          Dec(exponent, 16);
        end;
        if frac and (UInt64($FF) shl 45) = 0 then
        begin
          frac := frac shl 8;
          Dec(exponent, 8);
        end;
        if frac and (UInt64($F) shl 49)= 0 then
        begin
          frac := frac shl 4;
          Dec(exponent, 4);
        end;
        if frac and (UInt64($3) shl 51) = 0 then
        begin
          frac := frac shl 2;
          Dec(exponent, 2);
        end;
        if frac and (UInt64(1) shl 52) = 0 then
        begin
          frac := frac shl 1;
          Dec(exponent, 1);
        end;
        frac := frac and $000FFFFFFFFFFFFF;
      end
      else
      begin
        frac := U and $000FFFFFFFFFFFFF;
        exponent := W - $3FF;
      end;

      if frac <= $6A09E667F3BCC then // sqrt(0.5) * 2^53
      begin
        // (0.5 .. 0.7 (sqrt(2)/2)) * 2 - 1 => (0.0 .. 0.4 (SQRT(2) - 1))
        PUInt64(@E)^ := $3FF0000000000000 + frac;
      end
      else
      begin
        // (0.7 (sqrt(2)/2) .. 1.0) - 1 => (-0.3 .. 0.0)
        PUInt64(@E)^ := $3FE0000000000000 + frac;
        Inc(exponent);
      end;
      Result := pLnXP1Double(E-1) + exponent * PDouble(@Ln2Lo)^;
      Result := Result + exponent * PDouble(@Ln2Hi)^;
    end
    else if U = $7FF0000000000000 then
    begin
      Exit(cInfinity);
    end
    else
      Exit(cNaN);
  end;
end;
{$ELSE  CPUX64}
function Ln(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
const
  Ln2 = 0.69314718055994530941723212145818; // Ln(2)
var
  frac : UInt64;
  exponent : integer;
  E : Extended;
begin
  Result := X; // supress warning
  case PExtendedRec(@X).SpecialType of
    fsNInf, fsNegative, fsNDenormal:
      Error(reInvalidOp);
    fsZero, fsNZero:
      Error(reZeroDivide);
    fsInf,
    fsNaN:
      Exit(X);
    fsDenormal, fsPositive:
    begin
      frac := PExtendedRec(@X).Mantissa; // (0.5 * 2^64)<= frac < (1 * 2^64)
      exponent := PExtendedRec(@X).Exponent + 1;

      if frac <= $B504F333F9DE6480 then // sqrt(0.5) * 2^64
      begin
        // (0.5 .. 0.7 (sqrt(2)/2)) * 2 - 1 => (0.0 .. 0.4 (SQRT(2) - 1))
        PExtendedRec(@E).BuildUp(False, frac, 0);
        exponent := exponent - 1;
      end
      else
      begin
        // (0.7 (sqrt(2)/2) .. 1.0) - 1 => ( 0.3 .. 0.0)
        PExtendedRec(@E).BuildUp(False, frac, -1);
      end;

      E := E - 1;
      Result := pLnXP1(E) + exponent * Ln2;
    end;
  end;
end;
{$ELSE  PUREPASCAL}
asm
        FLD     X
        FLDLN2
        FXCH
        FYL2X
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function LnXPlus1(const X: Double): Double;
begin
  if Abs(X) < (sqrt(2.0) - 1) then
    Result := pLnXP1Double(X)
  else
    Result := Ln(1 + X);
end;
{$ELSE CPUX86}
function LnXPlus1(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
  if Abs(X) < (sqrt(2.0) - 1) then
    Result := pLnXP1(X)
  else
    Result := Ln(1 + X);
end;
{$ELSE  PUREPASCAL}
asm
        FLD     X
        FLD1
        FADD
        FLDLN2
        FXCH
        FYL2X
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF PUREPASCAL}
{$IFDEF CPUX64}
function internalArcTan(D : Double ): Double;
const
  CArcTan: array[0..7] of UINT64 =
  (  $3FB2F5A7353A1B01,
     $BFB7429E98FF380E,
     $3FBC71C037153954,
     $BFC249248E85912A,
     $BC53C41AEC2E7942, $3FC999999997B5A2,
     $3C635CC7570C5D0A, $BFD555555555552D );
var
  L1, L2, D2, D4 : Double;
begin
  D2 := D *D ;
  D4 := D2 * D2;

  L1 :=           PDouble(@CArcTan[0])^;
  L2 :=           PDouble(@CArcTan[1])^;
  L1 := L1 * D4 + PDouble(@CArcTan[2])^;
  L2 := L2 * D4 + PDouble(@CArcTan[3])^;
  L1 := L1 * D4 + PDouble(@CArcTan[4])^;
  L1 := L1      + PDouble(@CArcTan[5])^;
  L2 := L2 * D4 + PDouble(@CArcTan[6])^;
  Result := L2 + L1 * D2;
  Result := Result + PDouble(@CArcTan[7])^;
  Result := Result * D2 * D + D;
end;

function emu_PAtan( z: Double): Double;
const
  topsOf: array[0..3] of TDoubleRec =
  ( (Words:($0000, $0000, $E000, $3FBC)),
    (Words:($7AD5, $9DCF, $6B8C, $3FD6)),
    (Words:($88F1, $0E9F, $2226, $3FE4)),
    (Words:($0000, $0000, $0000, $3FF0)) );
  middles: array[0..2] of TDoubleRec =
  ( (Words:($FB2E, $37A3, $3F41, $3FCD)),
    (Words:($A6AF, $8B77, $DBB0, $3FDE)),
    (Words:($46BB, $1B60, $8E70, $3FE9)) );
  ArcTans: array[0..2, coeffType] of UInt64 =
  (($3FCCC0E3B6560C9F, $3C6D41911026EB34),
   ($3FDCC0E3B6560C9F, $BC4158A052703F90),
   ($3FE590AAC8C08978, $BC841E1865A76537) );
var
  theta : integer;
begin
  theta := 0;
  while (theta < High(topsOf)) and (z > PDouble(@topsOf[theta])^) do
    Inc(theta);

  if theta = 0 then
    Result := internalArcTan(z)
  else
  begin
    theta := theta - 1;
    z := (z - PDouble(@middles[theta])^) / ( 1 + z * PDouble(@middles[theta])^);
    Result := internalArcTan(z) + PDouble(@ArcTans[theta, cLo])^;
    Result := Result + PDouble(@ArcTans[theta, cHi])^;
  end;
end;
{$ENDIF CPUX64}

{$IFDEF CPUX86}
function internalArcTan(E : Extended ): Extended;
const
  CArcTan: array[0..7] of TExtendedRec =
  ( (Words:($0000, $0000, $0000, $8000, $3FFF)),
    (Words:($AB80, $AAAA, $AAAA, $AAAA, $BFF7)),
    (Words:($8000, $CCAF, $CCCC, $CCCC, $3FF0)),
    (Words:($0000, $E7D0, $2491, $9249, $BFEA)),
    (Words:($0000, $E000, $37F5, $E38E, $3FE3)),
    (Words:($0000, $0000, $FEB8, $BA2D, $BFDD)),
    (Words:($0000, $0000, $EC00, $9D5D, $3FD7)),
    (Words:($0000, $0000, $4000, $8181, $BFD1)) );
var
  I: Integer;
  E2 : EXtended;
begin
  E2 := E * 8;
  E2 := E2 * E2;
  Result := PExtended(@CArcTan[High(CArcTan)])^;
  for I := High(CArcTan)-1 downto 0 do Result := Result * E2 + PExtended(@CArcTan[I])^;

  Result := E * Result;
end;

function emu_PAtan( z: Extended): Extended;
const
  topsOf: array[0..3] of TExtendedRec =
  ( (Words:($0000, $0000, $0000, $E700, $3FFB)),
    (Words:($A4BD, $7BD6, $64EE, $B35C, $3FFD)),
    (Words:($85B5, $FC47, $3074, $A111, $3FFE)),
    (Words:($0000, $0000, $0000, $8000, $3FFF)) );
  middles: array[0..2] of TExtendedRec =
  ( (Words:($6EE6, $1FD9, $09BD, $E9FA, $3FFC)),
    (Words:($7B8D, $BD35, $845B, $F6DD, $3FFD)),
    (Words:($D57F, $0235, $80DB, $CC73, $3FFE)) );
  ArcTans: array[0..2] of TExtendedRec =
  ( (Words:($FA9C, $B064, $1DB2, $E607, $3FFC)),
    (Words:($FA9C, $B064, $1DB2, $E607, $3FFD)),
    (Words:($BBF5, $044B, $5646, $AC85, $3FFE)) );
var
  theta : integer;
begin
  theta := 0;
  while (theta < High(topsOf)) and (z > PExtended(@topsOf[theta])^) do
    Inc(theta);

  if theta = 0 then
    Result := internalArcTan(z)
  else
  begin
    theta := theta - 1;
    z := (z - PExtended(@middles[theta])^) / ( 1 + z * PExtended(@middles[theta])^);
    Result := internalArcTan(z) + PExtended(@ArcTans[theta])^;
  end;
end;
{$ENDIF CPUX86}
{$ENDIF}

{$IFDEF CPUX64}
function ArcTan(const X: Double): Double;
var
  A: Double;
begin
  Result := X;
  case PDoubleRec(@X).SpecialType of
    fsInf:
      Result := Pi / 2;
    fsNInf:
      Result := - Pi / 2;
    fsNZero,
    fsZero,
    fsNAN:
      ;
  else
    if X = 0 then Exit;
    A := Abs(X);
    if A > 1 then
      Result := Pi / 2 - emu_PAtan( 1 / A )
    else
      Result := emu_PAtan( A );
    if X < 0 then Result := -Result;
  end;
end;
{$ELSE  CPUX64}
function ArcTan(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
var
  A: Extended;
begin
  Result := X;
  case PExtendedRec(@X).SpecialType of
    fsInf:
      Result := Pi / 2;
    fsNInf:
      Result := - Pi / 2;
    fsNZero,
    fsZero,
    fsNAN:
      ;
  else
    if X = 0 then Exit;
    A := Abs(X);
    if A > 1 then
      Result := Pi / 2 - emu_PAtan( 1 / A )
    else
      Result := emu_PAtan( A );
    if X < 0 then Result := -Result;
  end;
end;
{$ELSE  PUREPASCAL}
asm
        FLD    X
        FLD1
        FPATAN
        FWAIT
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{$IFDEF CPUX64}
function Sqrt(const X: Double): Double;
asm
        .NOFRAME
        SQRTSD  XMM0, XMM0
end;
{$ELSE  CPUX64}
function Sqrt(const X: Extended): Extended;
{$IFDEF CPUX86}
asm
        FLD     X
        FSQRT
        FWAIT
end;
{$ELSE  CPUX86}
begin
  Error(rePlatformNotImplemented);
  Result := 0;
end;
{$ENDIF CPUX86}
{$ENDIF CPUX64}

{ ----------------------------------------------------- }
{       functions & procedures that need compiler magic }
{ ----------------------------------------------------- }


{$IF     defined(CPUX64)}
function _Round(Val: Extended): Int64;
asm
        .NOFRAME
        CVTSD2SI        RAX, XMM0
end;
{$ELSEIF defined(CPUX86)}
procedure _ROUND;
asm
        { ->    FST(0)  Extended argument       }
        { <-    EDX:EAX Result                  }

        SUB     ESP,8
        FISTP   qword ptr [ESP]
        FWAIT
        POP     EAX
        POP     EDX
end;
{$ELSE}
function _Round(Val: Extended): Int64;
begin
  Error(rePlatformNotImplemented);
  Result := 0;
end;
{$IFEND}

{$IF defined(CPUX64)}
function _RoundCurrency(Val: Currency): Int64;
var
  I, R: Int64;
  RoundingMode : Word;
begin
  I := PInt64(@Val)^;
  Result := I div 10000;
  R := I mod 10000;

  RoundingMode := (GetMXCSR shr 13) and 3; // Math.GetSSERoundMode

  if Result > 0 then
  begin
    case RoundingMode of
      0: // rmNearest
        if (R > 5000) or ((R = 5000) and odd(Result)) then
          Result := Result + 1;
      2: // rmUp
        if R > 0 then Result := Result + 1;
    end;
  end
  else
  begin
    case RoundingMode of
      0: // rmNearest
        if (R < -5000) or ((R = -5000) and odd(Result)) then
          Result := Result - 1;
      1: // rmDown
        if R < 0 then Result := Result - 1;
    end;
  end
end;
{$IFEND}

{$IF     defined(CPUX64)}
function _Trunc(Val: Extended): Int64;
asm
        .NOFRAME
        CVTTSD2SI RAX, XMM0
end;
{$ELSEIF defined(CPUX86)}
procedure _Trunc;
asm
        { ->    FST(0)  Extended argument       }
        { <-    EDX:EAX Result                  }

        SUB     ESP,12
        FNSTCW  [ESP].Word          // save
        FNSTCW  [ESP+2].Word        // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FISTP   qword ptr [ESP+4]
        FWAIT
        FLDCW   [ESP].Word
        POP     ECX
        POP     EAX
        POP     EDX
end;
{$IFEND}

{$IFDEF CPUX64}
function _Abs(Val: Extended): Extended;
const
  ABSMASK : UInt64 = $7FFFFFFFFFFFFFFF;
asm
        .NOFRAME
        MOVSD   XMM1, ABSMASK
        PAND    XMM0, XMM1
end;
{$ENDIF CPUX64}

procedure _AbstractError;
begin
  if Assigned(AbstractErrorProc) then
    AbstractErrorProc;
  RunErrorAt(210, ReturnAddress);
end;

function TextOpen(var t: TTextRec): Integer; forward;

procedure __FlushMBCSBuffer(var t: TTextRec);
begin
  t.MBCSLength := 0;
  t.MBCSBufPos := 0;
end;

function OpenText(var t: TTextRec; Mode: Word): Integer;
begin
  if (t.Mode < fmClosed) or (t.Mode > fmInOut) then
    Result := 102
  else
  begin
    __FlushMBCSBuffer(t);
    if t.Mode <> fmClosed then _Close(t);
    t.Mode := Mode;
    if (t.Name[0] = #0) and (t.OpenFunc = nil) then  // stdio
      t.OpenFunc := @TextOpen;
    Result := TTextIOFunc(t.OpenFunc)(t);
  end;
  if Result <> 0 then SetInOutRes(Result);
end;

function _ResetText(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmInput);
end;

function _RewritText(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmOutput);
end;

function _Append(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmInOut);
end;

function TextIn(var t: TTextRec): Integer;
const
  ERROR_BROKEN_PIPE = 109;
begin
  t.BufEnd := 0;
  t.BufPos := 0;
{$IFDEF MSWINDOWS}
  if not ReadFile(t.Handle, t.BufPtr, t.BufSize, t.BufEnd, nil) then
  begin
    Result := GetLastError;
    if Result = ERROR_BROKEN_PIPE then
      Result := 0; // NT quirk: got "broken pipe"? it's really eof
  end
  else
    Result := 0;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  t.BufEnd := __read(t.Handle, t.BufPtr, t.BufSize);
  if Integer(t.BufEnd) = -1 then
  begin
    t.BufEnd := 0;
    Result := GetLastError;
  end
  else
    Result := 0;
{$ENDIF POSIX}
end;

function FileNOPProc(var t): Integer;
begin
  Result := 0;
end;

function TextOut(var t: TTextRec): Integer;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
{$ENDIF}
begin
  if t.BufPos = 0 then
    Result := 0
  else
  begin
{$IFDEF MSWINDOWS}
    if not WriteFile(t.Handle, t.BufPtr, t.BufPos, Dummy, nil) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    if __write(t.Handle, t.BufPtr, t.BufPos) = ssize_t(-1) then
{$ENDIF POSIX}
      Result := GetLastError
    else
      Result := 0;
    t.BufPos := 0;
  end;
end;

function InternalClose(Handle: THandle): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := CloseHandle(Handle);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Result := __close(Handle) = 0;
{$ENDIF POSIX}
end;

function TextClose(var t: TTextRec): Integer;
begin
  t.Mode := fmClosed;
  if not InternalClose(t.Handle) then
    Result := GetLastError
  else
    Result := 0;
end;

function TextOpenCleanup(var t: TTextRec): Integer;
begin
  InternalClose(t.Handle);
  t.Mode := fmClosed;
  Result := GetLastError;
end;

function TextOpen(var t: TTextRec): Integer;
{$IFDEF MSWINDOWS}
var
  OpenMode: DWORD;
  Flags: Integer;
  Std: DWORD;
  Temp: THandle;
  TempSize: Integer;
  I, BytesRead: Cardinal;
begin
  Result := 0;
  t.BufPos := 0;
  t.BufEnd := 0;
  case t.Mode of
    fmInput: // called by Reset
      begin
        OpenMode := GENERIC_READ;
        Flags := OPEN_EXISTING;
        t.InOutFunc := @TextIn;
      end;
    fmOutput: // called by Rewrite
      begin
        OpenMode := GENERIC_WRITE;
        Flags := CREATE_ALWAYS;
        t.InOutFunc := @TextOut;
      end;
    fmInOut:  // called by Append
      begin
        OpenMode := GENERIC_READ or GENERIC_WRITE;
        Flags := OPEN_EXISTING;
        t.InOutFunc := @TextOut;
      end;
  else
    Exit;
  end;

  t.FlushFunc := @FileNOPProc;

  if t.Name[0] = #0 then  // stdin or stdout
  begin
    if t.BufPtr = nil then  // don't overwrite bufptr provided by SetTextBuf
    begin
      t.BufPtr := @t.Buffer;
      t.BufSize := SizeOf(t.Buffer);
    end;
    t.CloseFunc := @FileNOPProc;
    if t.Mode = fmOutput then
    begin
      if @t = @ErrOutput then
        Std := STD_ERROR_HANDLE
      else
        Std := STD_OUTPUT_HANDLE;
      t.Handle := GetStdHandle(Std);
    end
    else
      t.Handle := GetStdHandle(STD_INPUT_HANDLE);
    if t.CodePage = 0 then
    begin
      if GetFileType(t.Handle) = 2 then
      begin
        if t.Mode = fmOutput then
          t.CodePage := GetConsoleOutputCP
        else
          t.CodePage := GetConsoleCP;
      end
      else
        t.CodePage := DefaultSystemCodePage;
    end
  end
  else
  begin
    t.CloseFunc := @TextClose;
    Temp := CreateFile(t.Name, OpenMode, FILE_SHARE_READ, nil, Flags, FILE_ATTRIBUTE_NORMAL, 0);
    if Temp = THandle(-1) then
    begin
      t.Mode := fmClosed;
      Result := GetLastError;
      Exit;
    end;
    t.Handle := Temp;
    if t.Mode = fmInOut then      // Append mode
    begin
      t.Mode := fmOutput;

      TempSize := GetFileSize(t.Handle, nil);
      if TempSize = -1 then
      begin
        Result := TextOpenCleanup(t);
        Exit;
      end;

      Dec(TempSize, 128);
      if TempSize < 0 then TempSize := 0;

      if (Longint(SetFilePointer(t.Handle, TempSize, nil, FILE_BEGIN)) = -1) or
         (not ReadFile(t.Handle, @(t.Buffer), 128, BytesRead, nil)) then
      begin
        Result := TextOpenCleanup(t);
        Exit;
      end;

      if (t.Flags and tfCRLF) <> 0 then  // DOS mode, EOF significant
      begin  // scan for EOF char in last 128 byte sector.
        if BytesRead > 0 then
          for I := 0 to BytesRead - 1 do
          begin
            if t.Buffer[I] = AnsiChar(cEOF) then
            begin  // truncate the file here
              if (Longint(SetFilePointer(t.Handle, I - BytesRead, nil, FILE_END)) = -1) or
                (not SetEndOfFile(t.Handle)) then
              begin
                Result := TextOpenCleanup(t);
                Exit;
              end;
              Break;
            end;
          end;
      end;
    end;
    if t.CodePage = 0 then
      t.CodePage := DefaultSystemCodePage;
  end;
  if t.Mode <> fmInput then
  begin
    case GetFileType(t.Handle) of
      0: begin  // bad file type
           TextOpenCleanup(t);
           Result := 105;
           Exit;
         end;
      2: t.FlushFunc := @TextOut;
    end;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  Flags: Integer;
  Temp, I: Integer;
  BytesRead: Integer;
  us: UTF8String;
begin
  Result := 0;
  t.BufPos := 0;
  t.BufEnd := 0;
  case t.Mode of
    fmInput: // called by Reset
      begin
        Flags := O_RDONLY;
        t.InOutFunc := @TextIn;
      end;
    fmOutput: // called by Rewrite
      begin
        Flags := O_CREAT or O_TRUNC or O_WRONLY;
        t.InOutFunc := @TextOut;
      end;
    fmInOut:  // called by Append
      begin
        Flags := O_APPEND or O_RDWR;
        t.InOutFunc := @TextOut;
      end;
  else
    Exit;
    Flags := 0;
  end;

  t.FlushFunc := @FileNOPProc;

  if t.Name[0] = #0 then  // stdin or stdout
  begin
    if t.BufPtr = nil then  // don't overwrite bufptr provided by SetTextBuf
    begin
      t.BufPtr := @t.Buffer;
      t.BufSize := sizeof(t.Buffer);
    end;
    t.CloseFunc := @FileNOPProc;
    if t.Mode = fmOutput then
    begin
      if @t = @ErrOutput then
        t.Handle := STDERR_FILENO
      else
        t.Handle := STDOUT_FILENO;
      t.FlushFunc := @TextOut;
    end
    else
      t.Handle := STDIN_FILENO;
    t.CodePage := CP_UTF8;
  end
  else
  begin
    t.CloseFunc := @TextClose;

    us := UTF8String(t.Name);
    Temp := open(PAnsiChar(us), Flags, FileAccessRights);
    if Temp = -1 then
    begin
      t.Mode := fmClosed;
      Result := GetLastError;
      Exit;
    end;

    t.Handle := Temp;

    if t.Mode = fmInOut then      // Append mode
    begin
      t.Mode := fmOutput;

      if (t.Flags and tfCRLF) <> 0 then  // DOS mode, EOF significant
      begin  // scan for EOF char in last 128 byte sector.
        Temp := lseek(t.Handle, 0, SEEK_END);
        if Temp = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        Dec(Temp, 128);
        if Temp < 0 then Temp := 0;

        if lseek(t.Handle, Temp, SEEK_SET) = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        BytesRead := __read(t.Handle, t.BufPtr, 128);
        if BytesRead = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        for I := 0 to BytesRead - 1 do
        begin
          if t.Buffer[I] = AnsiChar(cEOF) then
          begin  // truncate the file here
            if ftruncate(t.Handle, lseek(t.Handle, I - BytesRead, SEEK_END)) = -1 then
            begin
              Result := TextOpenCleanup(t);
              Exit;
            end;
            Break;
          end;
        end;
      end;
    end;
    if t.CodePage = 0 then
      t.CodePage := DefaultSystemCodePage;
  end;
end;
{$ENDIF POSIX}

const
  fNameLen = 259;

function _AssignFile(var t: TFileRec; const s: PChar): Integer;
var
  Len: Integer;
begin
  FillChar(t, SizeOf(TFileRec), 0);
  t.BufPtr := NIL;
  t.Mode := fmClosed;
  t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
  t.BufSize := 0;
  t.OpenFunc := @TextOpen;
  Len := _PWCharLen(s);
  if Len >  fNameLen then
  begin
    SetInOutRes(3);
    Len :=  fNameLen;
  end;
  MoveChars(s^, t.Name, Len);
  t.Name[Len] := #0;
  Result := 0;
end;

function _Assigntext(var t: TTextRec; const s: PChar; const CP: word): Integer;
var
  Len: Integer;
begin
  FillChar(t, SizeOf(TTextRec), 0);
  t.BufPtr := @t.Buffer;
  t.Mode := fmClosed;
  t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
  if CP = 0 then
    t.CodePage := DefaultSystemCodePage
  else
    t.CodePage := CP; 
  t.BufSize := SizeOf(t.Buffer);
  t.OpenFunc := @TextOpen;
  Len := _PWCharLen(s);
  if Len > fNameLen then
  begin
    SetInOutRes(3);
    Len := fNameLen;
  end;
  MoveChars(s^, t.Name, Len);
  t.Name[Len] := #0;
  t.MBCSLength := 0;
  Result := 0;
end;

function InternalFlush(var t: TTextRec; Func: TTextIOFunc): Integer;
begin
  case t.Mode of
    fmOutput,
    fmInOut  : Result := Func(t);
    fmInput  : Result := 0;
  else
    if (@t = @Output) or (@t = @ErrOutput) then
      Result := 0
    else
      Result := 103;
  end;
  if Result <> 0 then SetInOutRes(Result);
end;

function Flush(var t: Text): Integer;
begin
  Result := InternalFlush(TTextRec(t), TTextRec(t).InOutFunc);
end;

function _Flush(var t: TTextRec): Integer;
begin
  Result := InternalFlush(t, t.FlushFunc);
end;

type
{$IFDEF MSWINDOWS}
  TIOProc = function (hFile: THandle; Buffer: Pointer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: Pointer): BOOL; stdcall;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  TIOProc = function (Handle: Integer; Buffer: Pointer; Count: size_t): ssize_t; cdecl;
{$ENDIF POSIX}

function BlockIO(var f: TFileRec; buffer: Pointer; recCnt: Cardinal; var recsDone: Longint;
  ModeMask: Integer; IOProc: TIOProc; ErrorNo: Integer): Cardinal;
// Note:  RecsDone ptr can be nil!
begin
  if (f.Mode and ModeMask) = ModeMask then  // fmOutput or fmInOut / fmInput or fmInOut
  begin
{$IFDEF POSIX}
    Result := IOProc(f.Handle, buffer, recCnt * f.RecSize);
    if Integer(Result) = -1 then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if not IOProc(f.Handle, buffer, recCnt * f.RecSize, Result, nil) then
{$ENDIF MSWINDOWS}
    begin
      SetInOutRes(GetLastError);
      Result := 0;
    end
    else
    begin
      Result := Result div f.RecSize;
      if @RecsDone <> nil then
        RecsDone := Result
      else if Result <> recCnt then
      begin
        SetInOutRes(ErrorNo);
        Result := 0;
      end
    end;
  end
  else
  begin
    SetInOutRes(103);  // file not open
    Result := 0;
  end;
end;

function _BlockRead(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsRead: Longint): Longint;
begin
  Result := BlockIO(f, buffer, recCnt, recsRead, fmInput,
    {$IFDEF MSWINDOWS} ReadFile, {$ENDIF}
    {$IFDEF POSIX} __read, {$ENDIF}
    100);
end;

function _BlockWrite(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsWritten: Longint): Longint;
begin
  Result := BlockIO(f, buffer, recCnt, recsWritten, fmOutput,
  {$IFDEF MSWINDOWS} WriteFile, {$ENDIF}
  {$IFDEF POSIX} __write, {$ENDIF}
  101);
end;

function _Close(var t: TTextRec): Integer;
begin
  Result := 0;
  if (t.Mode >= fmInput) and (t.Mode <= fmInOut) then
  begin
    if (t.Mode and fmOutput) = fmOutput then  // fmOutput or fmInOut
      Result := TTextIOFunc(t.InOutFunc)(t);
    if Result = 0 then
      Result := TTextIOFunc(t.CloseFunc)(t);
    if Result <> 0 then
      SetInOutRes(Result);
  end
  else
  if @t <> @Input then
    SetInOutRes(103);
end;

function _EofFile(var f: TFileRec): Boolean;
begin
  Result := _FilePos(f) >= _FileSize(f);
end;

function _GetAnsiChar(var t: TTextRec; var IsEof: Boolean; codepage: word): Byte; forward;
function _GetWideChar(var t: TTextRec; var IsEof: Boolean): Word; forward;
procedure _SkipAnsiChar(var t: TTextRec); forward;
procedure _SkipWideChar(var t: TTextRec); forward;

function _ReadByte(var t: TTextRec; var IsEof: Boolean): Byte; forward;

function _EofText(var t: TTextRec): Boolean;
begin
  if t.MBCSLength <> 0 then Exit(False)
  else
  begin
    _ReadByte(t, Result);
    if Not Result then
      Dec(t.BufPos);
  end;
end;

function _Eoln(var t: TTextRec): Boolean;
var
  c: Word;
  eof: Boolean;
begin
  if t.MBCSLength <> 0 then
  begin
    if t.MBCSLength > 0 then
      c := Word(_GetAnsiChar(t, eof, DefaultSystemCodePage))
    else
      c := _GetWideChar(t, eof);
    if eof then Exit(True);
  end
  else
  begin
    c := word(_ReadByte(t, eof));
    if eof then Exit(True)
    else Dec(t.BufPos);
  end;

  if (t.Flags and tfCRLF) <> 0 then
    Result := (c = cCR) or (c = cEOF)
  else
    Result := (c = cLF) or (c = cEOF);
end;

procedure _Erase(var f: TFileRec);
begin
  if (f.Mode < fmClosed) or (f.Mode > fmInOut) then
    SetInOutRes(102)  // file not assigned
  else begin
{$IFDEF MSWINDOWS}
    if not DeleteFile(f.Name) then
      SetInOutRes(GetLastError);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    if remove(PAnsiChar(UTF8Encode(f.Name))) < 0 then
       SetInOutRes(GetLastError);
{$ENDIF POSIX}
  end;
end;

{$IFDEF WIN32}
// Floating-point divide reverse routine
// ST(1) = ST(0) / ST(1), pop ST

procedure _FSafeDivideR;
asm
  FXCH
  JMP _FSafeDivide
end;

// Floating-point divide routine
// ST(1) = ST(1) / ST(0), pop ST

procedure _FSafeDivide;
type
  Z = packed record  // helper type to make parameter references more readable
    Dividend: Extended;   // (TBYTE PTR [ESP])
    Pad: Word;
    Divisor: Extended;    // (TBYTE PTR [ESP+12])
  end;
asm
        CMP       TestFDIV,0        //Check FDIV indicator
        JLE       @@FDivideChecked  //Jump if flawed or don't know
        FDIV                        //Known to be ok, so just do FDIV
        RET

// FDIV constants
@@FDIVRiscTable: DB 0,1,0,0,4,0,0,7,0,0,10,0,0,13,0,0;

@@FDIVScale1:    DD $3F700000             // 0.9375
@@FDIVScale2:    DD $3F880000             // 1.0625
@@FDIV1SHL63:    DD $5F000000             // 1 SHL 63

@@TestDividend:  DD $C0000000,$4150017E   // 4195835.0
@@TestDivisor:   DD $80000000,$4147FFFF   // 3145727.0
@@TestOne:       DD $00000000,$3FF00000   // 1.0

// Flawed FDIV detection
@@FDivideDetect:
        MOV     TestFDIV,1                //Indicate correct FDIV
        PUSH    EAX
        SUB     ESP,12
        FSTP    TBYTE PTR [ESP]           //Save off ST
        FLD     QWORD PTR @@TestDividend  //Ok if x - (x / y) * y < 1.0
        FDIV    QWORD PTR @@TestDivisor
        FMUL    QWORD PTR @@TestDivisor
        FSUBR   QWORD PTR @@TestDividend
        FCOMP   QWORD PTR @@TestOne
        FSTSW   AX
        SHR     EAX,7
        AND     EAX,002H          //Zero if FDIV is flawed
        DEC     EAX
        MOV     TestFDIV,AL       //1 means Ok, -1 means flawed
        FLD     TBYTE PTR [ESP]   //Restore ST
        ADD     ESP,12
        POP     EAX
        JMP     _FSafeDivide

@@FDivideChecked:
        JE      @@FDivideDetect   //Do detection if TestFDIV = 0
@@1:
        PUSH    EAX
        SUB     ESP,24
        FSTP    [ESP].Z.Divisor     //Store Divisor and Dividend
        FSTP    [ESP].Z.Dividend
        FLD     [ESP].Z.Dividend
        FLD     [ESP].Z.Divisor
@@2:
        MOV     EAX,DWORD PTR [ESP+4].Z.Divisor   //Is Divisor a denormal?
        ADD     EAX,EAX
        JNC     @@20            //Yes, @@20
        XOR     EAX,0E000000H   //If these three bits are not all
        TEST    EAX,0E000000H   //ones, FDIV will work
        JZ      @@10            //Jump if all ones
@@3:
        FDIV                    //Do FDIV and exit
        ADD     ESP,24
        POP     EAX
        RET
@@10:
        SHR     EAX,28      //If the four bits following the MSB
                            //of the mantissa have a decimal
                            //of 1, 4, 7, 10, or 13, FDIV may
        CMP     byte ptr @@FDIVRiscTable[EAX],0 //not work correctly
        JZ      @@3     //Do FDIV if not 1, 4, 7, 10, or 13
        MOV     EAX,DWORD PTR [ESP+8].Z.Divisor //Get Divisor exponent
        AND     EAX,7FFFH
        JZ      @@3     //Ok to FDIV if denormal
        CMP     EAX,7FFFH
        JE      @@3     //Ok to FDIV if NAN or INF
        MOV     EAX,DWORD PTR [ESP+8].Z.Dividend //Get Dividend exponent
        AND     EAX,7FFFH
        CMP     EAX,1     //Small number?
        JE      @@11      //Yes, @@11
        FMUL    DWORD PTR @@FDIVScale1  //Scale by 15/16
        FXCH
        FMUL    DWORD PTR @@FDIVScale1
        FXCH
        JMP     @@3     //FDIV is now safe
@@11:
        FMUL    DWORD PTR @@FDIVScale2    //Scale by 17/16
        FXCH
        FMUL    DWORD PTR @@FDIVScale2
        FXCH
        JMP     @@3     //FDIV is now safe

@@20:
        MOV     EAX,DWORD PTR [ESP].Z.Divisor     //Is entire Divisor zero?
        OR      EAX,DWORD PTR [ESP+4].Z.Divisor
        JZ      @@3               //Yes, ok to FDIV
        MOV     EAX,DWORD PTR [ESP+8].Z.Divisor   //Get Divisor exponent
        AND     EAX,7FFFH         //Non-zero exponent is invalid
        JNZ     @@3               //Ok to FDIV if invalid
        MOV     EAX,DWORD PTR [ESP+8].Z.Dividend  //Get Dividend exponent
        AND     EAX,7FFFH         //Denormal?
        JZ      @@21              //Yes, @@21
        CMP     EAX,7FFFH         //NAN or INF?
        JE      @@3               //Yes, ok to FDIV
        MOV     EAX,DWORD PTR [ESP+4].Z.Dividend  //If MSB of mantissa is zero,
        ADD     EAX,EAX           //the number is invalid
        JNC     @@3               //Ok to FDIV if invalid
        JMP     @@22
@@21:
        MOV     EAX,DWORD PTR [ESP+4].Z.Dividend  //If MSB of mantissa is zero,
        ADD     EAX,EAX                           //the number is invalid
        JC      @@3                               //Ok to FDIV if invalid
@@22:
        FXCH                  //Scale stored Divisor image by
        FSTP    ST(0)         //1 SHL 63 and restart
        FLD     ST(0)
        FMUL    DWORD PTR @@FDIV1SHL63
        FSTP    [ESP].Z.Divisor
        FLD     [ESP].Z.Dividend
        FXCH
        JMP     @@2
end;
{$ENDIF WIN32}

function _FilePos(var f: TFileRec): Longint;
begin
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
{$IFDEF MSWINDOWS}
    Result := Longint(SetFilePointer(f.Handle, 0, nil, FILE_CURRENT));
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
                                                                         
    Result := lseek(f.Handle, 0, SEEK_CUR);
{$ENDIF POSIX}
    if Result = -1 then
      InOutError
    else
      Result := Cardinal(Result) div f.RecSize;
  end
  else
  begin
    SetInOutRes(103);
    Result := -1;
  end;
end;

function _FileSize(var f: TFileRec): Longint;
{$IFDEF MSWINDOWS}
begin
  Result := -1;
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
    Result := GetFileSize(f.Handle, nil);
    if Result = -1 then
      InOutError
    else
      Result := Cardinal(Result) div f.RecSize;
  end
  else
    SetInOutRes(103);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  stat: _stat;
begin
  Result := -1;
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
//    if _fxstat(STAT_VER_LINUX, f.Handle, stat) <> 0 then
    if fstat(f.Handle, stat) <> 0 then
      InOutError
    else
      Result := stat.st_size div f.RecSize;
  end
  else
    SetInOutRes(103);
{$ENDIF POSIX}
end;

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly procedure FillChar is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure _FillChar(var Dest; Count: NativeInt; Value: AnsiChar);
{$ifdef PUREPASCAL}
var
  I: NativeInt;
  V: Int64;
  PB: PByte;
  P: PInt64;
  Total: NativeInt;
begin
  if Count >= 8 then
  begin
    V := Byte(Value) or (Byte(Value) shl 8) or
      (Byte(value) shl 16) or (Byte(value) shl 24);
    V := V or (V shl 32);
    P := PInt64(@Dest);
    Total := Count shr 3;

    for I := 0 to Total - 1 do
    begin
      P^ := V;
      Inc(P);
    end;
    PB := Pointer(P);
    { Get the remainder (mod 8) }
    Total := Count and $07;
  end
  else
  begin
    PB := PByte(@Dest);
    Total := Count;
  end;

  for I := Total - 1 downto 0 do
    PB[I] := Byte(Value);
end;
{$ELSE !PUREPASCAL}
asm                                  // Size = 153 Bytes
        CMP   EDX, 32
        MOV   CH, CL                 // Copy Value into both Bytes of CX
        JL    @@Small
        MOV   [EAX  ], CX            // Fill First 8 Bytes
        MOV   [EAX+2], CX
        MOV   [EAX+4], CX
        MOV   [EAX+6], CX
        SUB   EDX, 16
        FLD   QWORD PTR [EAX]
        FST   QWORD PTR [EAX+EDX]    // Fill Last 16 Bytes
        FST   QWORD PTR [EAX+EDX+8]
        MOV   ECX, EAX
        AND   ECX, 7                 // 8-Byte Align Writes
        SUB   ECX, 8
        SUB   EAX, ECX
        ADD   EDX, ECX
        ADD   EAX, EDX
        NEG   EDX
@@Loop:
        FST   QWORD PTR [EAX+EDX]    // Fill 16 Bytes per Loop
        FST   QWORD PTR [EAX+EDX+8]
        ADD   EDX, 16
        JL    @@Loop
        FFREE ST(0)
        FINCSTP
        RET
        NOP
        NOP
        NOP
@@Small:
        TEST  EDX, EDX
        JLE   @@Done
        MOV   [EAX+EDX-1], CL        // Fill Last Byte
        AND   EDX, -2                // No. of Words to Fill
        NEG   EDX
{$IFDEF PIC}
        PUSH  EAX
        PUSH  EBX
        PUSH  ECX
        CALL  GetGOT
        ADD   EAX, offset @@SmallFill + 60
        LEA   EDX, [EAX + EDX * 2]
        POP   ECX
        POP   EBX
        POP   EAX
{$ELSE !PIC}
        LEA   EDX, [@@SmallFill + 60 + EDX * 2]
{$ENDIF !PIC}
        JMP   EDX
{$IFNDEF PIC}
        NOP                          // Align Jump Destinations
        NOP
{$ENDIF !PIC}
@@SmallFill:
        MOV   [EAX+28], CX
        MOV   [EAX+26], CX
        MOV   [EAX+24], CX
        MOV   [EAX+22], CX
        MOV   [EAX+20], CX
        MOV   [EAX+18], CX
        MOV   [EAX+16], CX
        MOV   [EAX+14], CX
        MOV   [EAX+12], CX
        MOV   [EAX+10], CX
        MOV   [EAX+ 8], CX
        MOV   [EAX+ 6], CX
        MOV   [EAX+ 4], CX
        MOV   [EAX+ 2], CX
        MOV   [EAX   ], CX
        RET                          // DO NOT REMOVE - This is for Alignment
@@Done:
end;
{$ENDIF !PUREPASCAL}

procedure       Mark;
begin
  Error(reInvalidPtr);
end;

function _ReadRec(var f: TFileRec; Buffer: Pointer): Integer;
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
begin
  if (f.Mode = fmInput) or (f.Mode = fmInOut) then
  begin
    if not ReadFile(f.Handle, Buffer, f.RecSize, Cardinal(Result), nil) then
      SetInOutRes(GetLastError)
    else if Cardinal(Result) <> f.RecSize then
      SetInOutRes(100);
  end
  else
  begin
    SetInOutRes(103);  // file not open for input
    Result := 0;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
// -> EAX Pointer to file variable
//    EDX Pointer to buffer

        PUSH    EBX
        XOR     ECX,ECX
        MOV     EBX,EAX
        MOV     CX,[EAX].TFileRec.Mode   // File must be open
        SUB     ECX,fmInput
        JE      @@skip
        SUB     ECX,fmInOut-fmInput
        JNE     @@fileNotOpen
@@skip:

//  ReadFile(f.Handle, buffer, f.RecSize, @result, Nil);

        PUSH    0     // space for OS result
        MOV     EAX,ESP

        PUSH    0     // pass lpOverlapped
        PUSH    EAX     // pass @result

        PUSH    [EBX].TFileRec.RecSize    // pass nNumberOfBytesToRead

        PUSH    EDX     // pass lpBuffer
        PUSH    [EBX].TFileRec.Handle   // pass hFile
        CALL    ReadFile
        POP     EDX     // pop result
        DEC     EAX     // check EAX = TRUE
        JNZ     @@error

        CMP     EDX,[EBX].TFileRec.RecSize  // result = f.RecSize ?
        JE      @@exit

@@readError:
        MOV EAX,100
        JMP @@errExit

@@fileNotOpen:
        MOV EAX,103
        JMP @@errExit

@@error:
        CALL  GetLastError
@@errExit:
        CALL  SetInOutRes
@@exit:
        POP EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  if (f.Mode and fmInput) = fmInput then  // fmInput or fmInOut
  begin
    Result := __read(f.Handle, Buffer, f.RecSize);
    if Result = -1 then
      InOutError
    else if Cardinal(Result) <> f.RecSize then
      SetInOutRes(100);
  end
  else
  begin
    SetInOutRes(103);  // file not open for input
    Result := 0;
  end;
end;
{$ENDIF POSIX}

// If the file is Input std variable, try to open it
// Otherwise, runtime error.
function TryOpenForInput(var t: TTextRec): Boolean;
begin
  if @t = @Input then
  begin
    t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
    _ResetText(t);
  end;

  Result := t.Mode = fmInput;
  if not Result then
    SetInOutRes(104);
end;

// For eof, #$1A is returned.
// For errors, InOutRes is set and #$1A is returned.
function _ReadByte(var t: TTextRec; var IsEof: Boolean): Byte;
var
  Res: Integer;
begin
  Result := cEof;
  IsEof := True;

  if t.Mode <> fmInput then
    if not TryOpenForInput(t) then
      exit;

  if t.BufPos >= t.BufEnd then
  begin
    Res := TTextIOFunc(t.InOutFunc)(t);
    if Res <> 0 then
    begin
      SetInOutRes(Res);
      exit;
    end;
    if t.BufPos >= t.BufEnd then
    begin
      //  We didn't get characters. Must be eof then.
      if (t.Flags and tfCRLF) <> 0 then
      begin
        //  In DOS CRLF compatibility mode, synthesize an EOF char
        //  Store one eof in the buffer and increment BufEnd
        t.BufPtr[t.BufPos] := AnsiChar(cEof);
        inc(t.BufEnd);
      end;
      exit;
    end;
  end;

  Result := Byte(t.BufPtr[t.BufPos]);
  // Check for EOF char in DOS mode
  if ((t.Flags and tfCRLF) <> 0) and (Result = cEof) then
    Exit;

  Inc(t.BufPos);
  IsEof := False;
end;

// For eof, #$1A is returned.
// For errors, InOutRes is set and #$1A is returned.
function _ReadWord(var t: TTextRec; var IsEof: Boolean): Word;
var
  Res: Integer;
begin
  Result := cEof;
  IsEof := True;

  if t.Mode <> fmInput then
    if not TryOpenForInput(t) then
      exit;

  if t.BufPos >= t.BufEnd then
  begin
    Res := TTextIOFunc(t.InOutFunc)(t);
    if Res <> 0 then
    begin
      SetInOutRes(Res);
      Exit;
    end;
    if t.BufPos >= t.BufEnd then
    begin
      //  We didn't get characters. Must be eof then.
      if (t.Flags and tfCRLF) <> 0 then
      begin
        //  In DOS CRLF compatibility mode, synthesize an EOF char
        //  Store one eof in the buffer and increment BufEnd
        t.BufPtr[t.BufPos] := AnsiChar(cEof);
        inc(t.BufEnd);
      end;
      Exit;
    end;
  end;

  Result := PWord(t.BufPtr + t.BufPos)^;
  // Check for EOF char in DOS mode
  if ((t.Flags and tfCRLF) <> 0) and (Result = cEof) then
    Exit;

  Inc(t.BufPos, 2);
  IsEof := False;
end;

//    For eof, #$1A is returned in both lower and upper 8 bits.
//    For errors, InOutRes is set and #$1A is returned.
function _GetAnsiChar(var t: TTextRec; var IsEof: Boolean; codepage: word): Byte;
var
  len : integer;
  W : array[0..2] of WideChar;
  A : AnsiString;
  aBuf : array[0..6] of AnsiChar;
begin
  IsEof := False;

  if t.MBCSLength < 0 then
  begin
    // Wide -> A
    if t.MBCSBufPos <> 0 then
    begin
      // Remove partial character.
      t.MBCSLength := 0;
    end
    else
    begin
      // Wide -> A
      len := CharFromWChar(PAnsiChar(@abuf[0]), 6, PWideChar(@t.UTF16Buffer[0]), -t.MBCSLength, codepage);
      t.MBCSLength := len;
      move(aBuf[0], t.MBCSBuffer[0], len);
    end;
  end;

  if t.MBCSLength > 0 then
  begin
    Result := Byte(t.MBCSBuffer[t.MBCSBufPos]);
    Exit;
  end;
  if (t.CodePage = codepage) then
  begin
    Result := _ReadByte(t, IsEof);
    if Not IsEof then
      Dec(t.BufPos);
  end
  else
  begin
    len := 1;
    W[0] := Char(_GetWideChar(t, IsEof));
    if Not IsEof then
    begin
      if (W[0] >= #$D800) and (W[0] < #$DC00) then
      begin
        Inc(Len);
        _SkipWideChar(t);
        W[1] := Char(_GetWideChar(t, IsEof));
        if IsEof then
        begin
          Result := cEOF;
          Exit;
        end;
      end;
      _SkipWideChar(t);
      _LStrFromPWCharLen(A, PWideChar(@W[0]), Len, CodePage);
      len := Length(A);
      if len = 0 then
        Result := 0
      else
      begin
        Move(A[1], t.MBCSBuffer[0], Len);
        t.MBCSBufPos := 0;
        t.MBCSLength := Len;
        Result := Byte(A[1]);
      end
    end
    else
      Result := cEOF;
  end;
end;

procedure _SkipAnsiChar(var t: TTextRec);
begin
  if t.MBCSLength > 0 then
  begin
    Inc(t.MBCSBufPos);
    if byte(t.MBCSLength) = t.MBCSBufPos then
      t.MBCSLength := 0;
  end
  else
    Inc(t.BufPos)
end;

{$IFDEF POSIX}
function IsDBCSLeadByteEx(CodePage: cardinal; TestChar: Byte): LongBool;
const
  MB_ERR_INVALID_CHARS = 8;
var
  Dest: WideChar;
begin
  Result := False;
  if (TestChar > $7F) and
     (UnicodeFromLocaleChars(CodePage, MB_ERR_INVALID_CHARS,
         @TestChar, 1, @Dest, 1) = 0) then
    Result := True
end;
{$ENDIF POSIX}

//    For eof, #$1A is returned in both lower and upper 8 bits.
//    For errors, InOutRes is set and #$1A is returned.
function _GetWideChar(var t: TTextRec; var IsEof: Boolean): Word;
var
  c: byte;
  ind, len: integer;
  wBuf : array[0..1] of WideChar;
  buf: RawByteString;
  U : UnicodeString;
begin
  IsEof := False;

  if t.MBCSLength > 0 then
  begin
    // A -> Wide
    if t.MBCSBufPos <> 0 then
    begin
      // Remove partial character.
      t.MBCSLength := 0;
    end
    else
    begin
      len := WCharFromChar(PWideChar(@wBuf[0]), 2, PAnsiChar(@t.MBCSBuffer[0]), t.MBCSLength, DefaultSystemCodePage);
      t.MBCSLength := -len;
      move(wBuf[0], t.UTF16Buffer[0], len*2);
    end;
  end;


  if t.MBCSLength < 0 then
  begin
    Result := Word(t.UTF16Buffer[t.MBCSBufPos]);
    Exit;
  end
  else if (t.CodePage = CP_UTF16) then
  begin
    Result := _ReadWord(t, IsEof);
    if Not IsEof then
      Dec(t.BufPos, 2);
  end
  else
  begin
    c := _ReadByte(t, IsEof);
    if IsEof then
    begin
      Result := cEOF;
      Exit;
    end;

    if t.CodePage = CP_UTF8 then
    begin
      if byte(c) in [$C2..$DF] then
        Len := 2
      else if byte(c) in [$E0..$EF] then
        Len := 3
      else if byte(c) in [$F0..$F4] then
        Len := 4
      else
        Len := 1;
    end
    else if IsDBCSLeadByteEx(Cardinal(DefaultSystemCodePage), c) then
      Len := 2
    else
      Len := 1;

    SetLength(buf, len);
    SetCodePage(buf, t.CodePage, False);

    buf[1] := AnsiChar(c);
    ind := 2;
    dec(Len);
    while len > 0 do
    begin
      c := _ReadByte(t, IsEof);
      buf[ind] := AnsiChar(c);
      Inc(Ind);
      Dec(Len);
    end;

    U := UnicodeString(buf);
    len := Length(U);
    if Len > 0 then
    begin
      Move(U[1], t.UTF16Buffer[0], Len*2);

      t.MBCSBufPos := 0;
      t.MBCSLength := -Length(U);
      Result := Word(U[1]);
    end
    else
      Result := 0;
  end;
end;

function _GetWideChar2(var t: TTextRec; var IsEof: Boolean): Word;
begin
  if t.MBCSLength <> 0 then
    Exit(_GetWideChar(t, IsEof));

  Result := _ReadWord(t, IsEof);
  if Not IsEof then
    Dec(t.BufPos, 2);
end;

procedure _SkipWideChar(var t: TTextRec);
begin
  if t.MBCSLength < 0 then
  begin
    Inc(t.MBCSBufPos);
    if -t.MBCSLength = t.MBCSBufPos then
      t.MBCSLength := 0;
  end
  else
    Inc(t.BufPos, 2)
end;

function _ReadChar(var t: TTextRec): AnsiChar;
var
  eof: boolean;
begin
  Result := AnsiChar(_GetAnsiChar(t, eof, DefaultSystemCodePage));
  _SkipAnsiChar(t);
end;

function _ValLongL(const s: AnsiString; var code: Integer): Longint;
begin
  Result := _ValLong(string(s), code);
end;

function _ReadLong(var t: TTextRec): Longint;
type
  TStrRec32 = packed record
    hdr: StrRec;
    data: array[0..35] of Byte;
  end;
var
  s: TStrRec32;
  c: Byte;
  p: PByte;
  eof: Boolean;
  count: Integer;
  code: Integer;
begin
  if _SeekEof(t) then
    Result := 0
  else
  begin
    p := @s.data[0];
    for count := 1 to 32 do
    begin
      c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
      if c <= $20 then Break;
      p^ := c;
      _SkipAnsiChar(t);
      Inc(p);
    end;
    p^ := 0;
    s.hdr.codePage := CP_ACP;
    s.hdr.elemSize := 1;
    s.hdr.refCnt := -1;
    s.hdr.length := p - PByte(@s.data[0]);
    Result := _ValLongL(PAnsiChar(@s.data), code);
    if code <> 0 then
      SetInOutRes(106);
  end;
end;

procedure _PushCRLF(var t: TTextRec);
begin
  t.MBCSBuffer[0] := AnsiChar(cCR);
  t.MBCSBuffer[1] := AnsiChar(cLF);
  t.MBCSLength := 2; //Ansi 2 byets.
  t.MBCSBufPos := 0;
end;

function ReadAnsiLineEx(var t: TTextRec; buf: Pointer; maxLen: Longint; var Count: Integer; CodePage: Word): Pointer;
var
  c : Byte;
  eof: Boolean;
  p : PByte;
begin
  Result := @t;
  Count := 0;

  if t.Mode <> fmInput then
    if not TryOpenForInput(t) then
      exit;

  if maxLen <= 0 then Exit;
  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;

  p := PByte(buf);
  while (maxLen > 0) do
  begin
    c := _GetAnsiChar(t, eof, CodePage);
    if eof then Exit;
    if c = cLF then
    begin
      Exit;
    end
    else if c = cCR then
    begin
      _SkipAnsiChar(t);
      c := _GetAnsiChar(t, eof, CodePage);
      if c = cLF then
      begin
        _SkipAnsiChar(t);
        _PushCRLF(t);
        Exit;
      end
      else
      continue;
    end
    else
    begin
      p^ := c;
      _SkipAnsiChar(t);
      Inc(p);
      Dec(maxLen);
      inc(Count);
    end;
  end;
end;

function ReadAnsiLineEx2(var t: TTextRec; buf: Pointer; maxLen: Longint; var Count: Integer): Pointer;
var
  c : Byte;
  eof: Boolean;
  p : PByte;
begin
  if t.MBCSLength <> 0 then
    Exit(ReadAnsiLineEx(t, buf, maxLen, Count, t.CodePage));

  Result := @t;
  Count := 0;

  if t.Mode <> fmInput then
    if not TryOpenForInput(t) then
      exit;

  if maxLen <= 0 then Exit;

  p := PByte(buf);
  while (maxLen > 0) do
  begin
    c := _ReadByte(t, eof);
    if eof then Exit;
    if c = cLF then
    begin
      Dec(t.BufPos);
      Exit;
    end
    else if c = cCR then
    begin
      c := _ReadByte(t, eof);
      if c = cLF then
      begin
        _PushCRLF(t);
        Exit;
      end
      else
      Dec(t.BufPos);
      continue;
    end
    else
    begin
      p^ := c;
      Inc(p);
      Dec(maxLen);
      inc(Count);
    end;
  end;
end;

// maxLen and Count are the number of (wide)char.
function ReadWideLineEx(var t: TTextRec; buf: Pointer; maxLen: Longint; var Count: Integer): Pointer;
var
  w : Word;
  eof: Boolean;
  p : PWord;
begin
  Result := @t;
  Count := 0;

  if t.Mode <> fmInput then
    if not TryOpenForInput(t) then
      exit;

  if maxLen <= 0 then Exit;

  p := PWord(buf);
  while (maxLen > 0) do
  begin
    w := _GetWideChar(t, eof);
    if eof then Exit;
    if w = cLF then
    begin
      Exit;
    end
    else if w = cCR then
    begin
      _SkipWideChar(t);
      w := _GetWideChar(t, eof);
      if w = cLF then
      begin
        _SkipWideChar(t);
        _PushCRLF(t);
        Exit;
      end
      else
      continue;
    end
    else
    begin
      p^ := w;
      _SkipWideChar(t);
      Inc(p);
      Dec(maxLen);
      inc(Count);
    end;
  end;
end;

function ReadWideLineEx2(var t: TTextRec; buf: Pointer; maxLen: Longint; var Count: Integer): Pointer;
var
  w : Word;
  eof: Boolean;
  p : PWord;
begin
  if t.MBCSLength <> 0 then
    Exit(ReadAnsiLineEx(t, buf, maxLen, Count, t.CodePage));

  Result := @t;
  Count := 0;

  if t.Mode <> fmInput then
    if not TryOpenForInput(t) then
      exit;

  if maxLen <= 0 then Exit;

  p := PWord(buf);
  while (maxLen > 0) do
  begin
    w := _ReadWord(t, eof);
    if eof then Exit;
    if w = cLF then
    begin
      Dec(t.BufPos, 2);
      Exit;
    end
    else if w = cCR then
    begin
      w := _ReadWord(t, eof);
      if w = cLF then
      begin
        _PushCRLF(t);
        Exit;
      end
      else
      Dec(t.BufPos, 2);
      continue;
    end
    else
    begin
      p^ := w;
      Inc(p);
      Dec(maxLen);
      inc(Count);
    end;
  end;
end;

procedure _ReadString(var t: TTextRec; s: PShortString; maxLen: Longint);
var
  Count: Integer;
begin
  ReadAnsiLineEx(t, @s^[1], maxLen, Count, DefaultSystemCodePage);
  Byte(s^[0]) := Count;
end;

procedure _ReadCString(var t: TTextRec; s: PAnsiChar; maxLen: Longint);
var
  Count: Integer;
begin
  ReadAnsiLineEx(t, s, maxLen, Count, DefaultSystemCodePage);
  s[Count] := #0;
end;

procedure _ReadLString(var t: TTextRec; var s: AnsiString; CodePage: Word);
var
  Count: Integer;
  Temp, Buf: RawByteString;
  U: UnicodeString;
begin
  s := '';

  if t.CodePage = CP_UTF16 then
  begin
    _ReadUString(t, U);
    _LStrFromUStr(s, U, CodePage);
  end
  else
  begin
    SetLength(Buf, 255);
    SetCodePage(Buf, t.CodePage, False);

    ReadAnsiLineEx2(t, @buf[1], 255, Count);
    SetLength(Buf, Count);
    Temp := buf;
    while Count = 255 do
    begin
      SetLength(Buf, 255);
      SetCodePage(Buf, t.CodePage, False);
      ReadAnsiLineEx2(t, @buf[1], 255, Count);
      SetLength(Buf, Count);
      Temp := Temp + Buf;
    end;
    if CodePage <> t.CodePage then
    begin
      if Temp <> '' then
      begin
        _UStrFromLStr(U, Temp);
        _LStrFromUStr(s, U, CodePage);
      end;
    end
    else
      s := Temp;
  end;
end;

procedure _ReadUString(var t: TTextRec; var s: UnicodeString);
var
  Count: Integer;
  Buf: UnicodeString;
  a: AnsiString;
begin
  s := '';

  if t.CodePage = CP_UTF16 then
  begin
    SetLength(Buf, 255);
    ReadWideLineEx2(t, Pointer(Buf), 255, Count);
    SetLength(Buf, Count);
    s := Buf;

    while Count = 255 do
    begin
      SetLength(Buf, 255);
      ReadWideLineEx2(t, PWideChar(Buf), 255, Count);
      SetLength(Buf, Count);
      s := s + Buf;
      Buf := '';
    end;
  end
  else
  begin
    _ReadLString(t, a, t.CodePage);
    if a <> '' then
      _UStrFromLStr(s, a);
  end;
end;

function _ReadWChar(var t: TTextRec): WideChar;
var
  eof: boolean;
begin
  Result := WideChar(_GetWideChar(t, eof));
  _SkipWideChar(t);
end;

procedure _ReadWCString(var t: TTextRec; s: PWideChar; maxBytes: Longint);
var
  Count: Integer;
begin
  ReadWideLineEx(t, s, maxBytes div 2, Count);
  s[Count] := #0;
end;

procedure _ReadWString(var t: TTextRec; var s: WideString);
var
  Temp: UnicodeString;
begin
  _ReadUString(t, Temp);
  s := WideString(Temp);
end;

function _ValExtL(const s: AnsiString; var code: Integer): Extended;
begin
  Val(string(s), Result, code);
end;

function _ReadExt(var t: TTextRec): Extended;
var
  p: PWord;
  count: Integer;
  c: Byte;
  eof: Boolean;
  code: Integer;
  u: UnicodeString;
begin
  if _SeekEof(t) then
    Result := 0
  else
  begin
    SetLength(U, 64);
    p := PWord(U);
    for count := 1 to 64 do
    begin
      c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
      if c <= $20 then break;
      p^ := c;
      _SkipAnsiChar(t);
      Inc(p);
    end;
    SetLength(U, (PByte(p) - PByte(U)) div sizeof(word));
    Val(u, Result, code);
    if code <> 0 then
      SetInOutRes(106);
  end;
end;

procedure _ReadLn(var t: TTextRec);
var
  c: Byte;
  eof: boolean;
begin
  c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
  _SkipAnsiChar(t);
  while True do
  begin
    if c = cLF then break; // accept LF as end of line
    if eof then break;
    if c = cCR then
    begin
      c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
      _SkipAnsiChar(t);
      if c = cLF then break; // accept CR+LF as end of line
      if eof then break; // accept CR+EOF as end of line
      // else CR+ anything else is not a line break.
    end
    else
    begin
      c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
      _SkipAnsiChar(t);
    end;
  end;
end;

procedure _Rename(var f: TFileRec; newName: PChar);
var
  I: Integer;
  oldName: string;
{$IFDEF POSIX}
  usOldName: UTF8String;
  usNewName: UTF8String;
{$ENDIF POSIX}
begin
  if f.Mode = fmClosed then
  begin
    if newName = nil then newName := '';
    oldName := f.Name;
{$IFDEF POSIX}
    usNewName := UTF8String(newName);
    usOldName := UTF8String(oldName);
    if __rename(PAnsiChar(usOldName), PAnsiChar(usNewName)) = 0 then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if MoveFile(PChar(oldName), newName) then
{$ENDIF MSWINDOWS}
    begin
      I := 0;
      while (newName[I] <> #0) and (I < High(f.Name)) do
      begin
        f.Name[I] := newName[I];
        Inc(I);
      end
    end
    else
      SetInOutRes(GetLastError);
  end
  else
    SetInOutRes(102);
end;

procedure Release;
begin
  Error(reInvalidPtr);
end;

function _CloseFile(var f: TFileRec): Integer;
begin
  f.Mode := fmClosed;
  Result := 0;
  if not InternalClose(f.Handle) then
  begin
    InOutError;
    Result := 1;
  end;
end;

function OpenFile(var f: TFileRec; recSiz: Longint; mode: Longint): Integer;
{$IFDEF POSIX}
var
   Flags: Integer;
   uName: UTF8String;
begin
  Result := 0;
  if (f.Mode >= fmClosed) and (f.Mode <= fmInOut) then
  begin
    if f.Mode <> fmClosed then // not yet closed: close it
    begin
      Result := TFileIOFunc(f.CloseFunc)(f);
      if Result <> 0 then
        SetInOutRes(Result);
    end;

    if recSiz <= 0 then
      SetInOutRes(106);

    f.RecSize := recSiz;
    f.InOutFunc := @FileNopProc;

    if f.Name[0] <> #0 then
    begin
      f.CloseFunc := @_CloseFile;
      case mode of
        1: begin
             Flags := O_APPEND or O_WRONLY;
             f.Mode := fmOutput;
           end;
        2: begin
             Flags := O_RDWR;
             f.Mode := fmInOut;
           end;
        3: begin
             Flags := O_CREAT or O_TRUNC or O_RDWR;
             f.Mode := fmInOut;
           end;
      else
        Flags := O_RDONLY;
        f.Mode := fmInput;
      end;

      uName := UTF8String(f.Name);
      f.Handle := __open(PAnsiChar(uName), Flags, FileAccessRights);
    end
    else  // stdin or stdout
    begin
      f.CloseFunc := @FileNopProc;
      if mode = 3 then
        f.Handle := STDOUT_FILENO
      else
        f.Handle := STDIN_FILENO;
    end;

    if f.Handle = -1 then
    begin
      f.Mode := fmClosed;
      InOutError;
    end;
  end
  else
    SetInOutRes(102);
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
const
  ShareTab: array [0..7] of Integer =
    (FILE_SHARE_READ OR FILE_SHARE_WRITE,  // OF_SHARE_COMPAT     0x00000000
     0,                                    // OF_SHARE_EXCLUSIVE  0x00000010
     FILE_SHARE_READ,                      // OF_SHARE_DENY_WRITE 0x00000020
     FILE_SHARE_WRITE,                     // OF_SHARE_DENY_READ  0x00000030
     FILE_SHARE_READ OR FILE_SHARE_WRITE,  // OF_SHARE_DENY_NONE  0x00000040
     0,0,0);
{$IFDEF PUREPASCAL}
var
   DesiredAccess: DWORD;
   SharedMode: Integer;
   CreationDisposition: Integer;
begin
  Result := 0;
  if (f.Mode >= fmClosed) and (f.Mode <= fmInOut) then
  begin
    if f.Mode <> fmClosed then // not yet closed: close it
    begin
      Result := TFileIOFunc(f.CloseFunc)(f);
      if Result <> 0 then
        SetInOutRes(Result);
    end;

    if recSiz <= 0 then
      SetInOutRes(106);

    f.RecSize := recSiz;
    f.InOutFunc := @FileNopProc;

    if f.Name[0] <> #0 then
    begin
      f.CloseFunc := @_CloseFile;
      SharedMode := shareTab[(FileMode and $70) shr 4];
      case mode of
        1: begin
             CreationDisposition := OPEN_EXISTING;
             DesiredAccess := GENERIC_WRITE;
             f.Mode := fmOutput;
           end;
        2: begin
             CreationDisposition := OPEN_EXISTING;
             DesiredAccess := GENERIC_READ OR GENERIC_WRITE;
             f.Mode := fmInOut;
           end;
        3: begin
             DesiredAccess := GENERIC_READ OR GENERIC_WRITE;
             CreationDisposition := CREATE_ALWAYS;
             f.Mode := fmInOut;
           end;
      else
        CreationDisposition := OPEN_EXISTING;
        DesiredAccess := GENERIC_READ;
        f.Mode := fmInput;
      end;
      f.Handle := CreateFile(f.Name, DesiredAccess, SharedMode, nil,
                             CreationDisposition, FILE_ATTRIBUTE_NORMAL, 0);
    end
    else  // stdin or stdout
    begin
      f.CloseFunc := @FileNopProc;
      if mode = 3 then
        f.Handle := GetStdHandle(STD_OUTPUT_HANDLE)
      else
        f.Handle := GetStdHandle(STD_INPUT_HANDLE);;
    end;
    if f.Handle = -1 then
    begin
      f.Mode := fmClosed;
      InOutError;
    end;
  end
  else
    SetInOutRes(102);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
//->  EAX Pointer to file record
//    EDX Record size
//    ECX File mode

        PUSH     EBX
        PUSH     ESI
        PUSH     EDI

        MOV      ESI,EDX
        MOV      EDI,ECX
        XOR      EDX,EDX
        MOV      EBX,EAX

        MOV      DX,[EAX].TFileRec.Mode
        SUB      EDX,fmClosed
        JE       @@alreadyClosed
        CMP      EDX,fmInOut-fmClosed
        JA       @@notAssignedError

//  not yet closed: close it. File parameter is still in EAX

        CALL     [EBX].TFileRec.CloseFunc
        TEST     EAX,EAX
        JE       @@alreadyClosed
        CALL     SetInOutRes

@@alreadyClosed:

        MOV     [EBX].TFileRec.Mode,fmInOut
        MOV     [EBX].TFileRec.RecSize,ESI
        MOV     [EBX].TFileRec.CloseFunc,offset _CloseFile
        MOV     [EBX].TFileRec.InOutFunc,offset FileNopProc

        CMP     word ptr [EBX].TFileRec.Name,0
        JE      @@isCon

        MOV     EAX,GENERIC_READ OR GENERIC_WRITE
        MOV     DL,FileMode
        AND     EDX,070H
        SHR     EDX,4-2
        MOV     EDX,dword ptr [shareTab+EDX]
        MOV     ECX,CREATE_ALWAYS

        SUB     EDI,3
        JE      @@calledByRewrite

        MOV     ECX,OPEN_EXISTING
        INC     EDI
        JE      @@skip

        MOV     EAX,GENERIC_WRITE
        INC     EDI
        MOV     [EBX].TFileRec.Mode,fmOutput
        JE      @@skip

        MOV     EAX,GENERIC_READ
        MOV     [EBX].TFileRec.Mode,fmInput

@@skip:
@@calledByRewrite:

//  CreateFile(t.FileName, EAX, EDX, Nil, ECX, FILE_ATTRIBUTE_NORMAL, 0);

        PUSH     0
        PUSH     FILE_ATTRIBUTE_NORMAL
        PUSH     ECX
        PUSH     0
        PUSH     EDX
        PUSH     EAX
        LEA      EAX,[EBX].TFileRec.Name
        PUSH     EAX
        CALL     CreateFile
@@checkHandle:
        CMP      EAX,-1
        JZ       @@error

        MOV      [EBX].TFileRec.Handle,EAX
        JMP      @@exit

@@isCon:
        MOV      [EBX].TFileRec.CloseFunc,offset FileNopProc
        CMP      EDI,3
        JE       @@output
        PUSH     STD_INPUT_HANDLE
        JMP      @@1
@@output:
        PUSH     STD_OUTPUT_HANDLE
@@1:
        CALL     GetStdHandle
        JMP      @@checkHandle

@@notAssignedError:
        MOV      EAX,102
        JMP      @@errExit

@@error:
        MOV      [EBX].TFileRec.Mode,fmClosed
        CALL     GetLastError
@@errExit:
        CALL     SetInOutRes

@@exit:
        POP      EDI
        POP      ESI
        POP      EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

function _ResetFile(var f: TFileRec; recSize: Longint): Integer;
var
  m: Byte;
begin
  m := FileMode and 3;
  if m > 2 then m := 2;
  Result := OpenFile(f, recSize, m);
end;

function _RewritFile(var f: TFileRec; recSize: Longint): Integer;
begin
  Result := OpenFile(f, recSize, 3);
end;

procedure _Seek(var f: TFileRec; recNum: Cardinal);
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Val64: UInt64;
  Val32Low: UInt32;
  Val32High: UInt32;
  Res: LongInt;
begin
  if (f.Mode >= fmInput) and (f.Mode <= fmInOut) then
  begin
    Val64 := UInt64(recNum) * f.RecSize;
    Val32High := UInt32(Val64 shr 32);
    Val32Low := UInt32(Val64);

    Res := SetFilePointer(f.Handle, Val32Low, @Val32High, FILE_BEGIN);
    if Res = -1 then InOutError()
    else
      if Res < 0 then SetInOutRes(131);
    //if Longint(SetFilePointer(f.Handle, Val32Low, @Val32High, FILE_BEGIN)) = -1 then
    //  InOutError();

  end else
    SetInOutRes(103);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
// -> EAX Pointer to file variable
//    EDX Record number

        MOV      ECX,EAX
        MOVZX    EAX,[EAX].TFileRec.Mode // check whether file is open
        SUB      EAX,fmInput
        CMP      EAX,fmInOut-fmInput
        JA       @@fileNotOpen

//  SetFilePointer(f.Handle, recNum*f.RecSize, FILE_BEGIN)
        PUSH     FILE_BEGIN    // pass dwMoveMethod
        MOV      EAX,[ECX].TFileRec.RecSize
        MUL      EDX
        PUSH     0           // pass lpDistanceToMoveHigh
        PUSH     EAX           // pass lDistanceToMove
        PUSH     [ECX].TFileRec.Handle   // pass hFile
        CALL     SetFilePointer          // get current position
        INC      EAX
        JZ       InOutError
        JMP      @@exit

@@fileNotOpen:
        MOV      EAX,103
        JMP      SetInOutRes

@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  if Longint(recNum) < 0 then  // Match windows behaviour. Only 0 .. MaxInt sizes are allowed.
    SetInOutRes(131);          // Negative seeks are dissablowed.
  if (f.Mode >= fmInput) and (f.Mode <= fmInOut) then
  begin
                                               
    if lseek(f.Handle, f.RecSize * recNum, SEEK_SET) = -1 then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ENDIF POSIX}

function _SeekEof(var t: TTextRec): Boolean;
var
  c: Byte;
  eof: Boolean;
begin
  Result := False;
  while True do
  begin
    c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
    if c > $20 then break;
    if eof then
    begin
      Result := True;
      break;
    end;
    _SkipAnsiChar(t);
  end;
end;

function _SeekEoln(var t: TTextRec): Boolean;
var
  c: Byte;
  eof: Boolean;
begin
  Result := False;
  while True do
  begin
    c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
    if c > $20 then break;
    if eof then
    begin
      Result := True;
      break;
    end;
    if (c = cLF) or (c = cCR) then
    begin
      Result := True;
      break;
    end;
    _SkipAnsiChar(t);
  end;
end;

procedure _SetTextBuf(var t: TTextRec; p: Pointer; size: Longint);
begin
  if size < 0 then
    Error(reRangeError);
  t.BufPtr := P;
  t.BufSize := size;
  t.BufPos := 0;
  t.BufEnd := 0;
end;

function _StrLong(val, width: Longint): ShortString;
{$IFDEF PUREPASCAL}
var
  I: Integer;
  sign: Longint;
  a: array [0..19] of AnsiChar;
  P: PAnsiChar;
begin
  // U-OK
  sign := val;
  val := Abs(val);
  I := 0;
  repeat
    a[I] := AnsiChar((Cardinal(val) mod 10) + Ord('0'));
    Inc(I);
    val := Cardinal(val) div 10;
  until val = 0;

  if sign < 0 then
  begin
    a[I] := '-';
    Inc(I);
  end;

  if width < I then
    width := I;
  if width > 255 then
    width := 255;
  Result[0] := AnsiChar(width);
  P := @Result[1];
  while width > I do
  begin
    P^ := ' ';
    Inc(P);
    Dec(width);
  end;
  repeat
    Dec(I);
    P^ := a[I];
    Inc(P);
  until I <= 0;
end;
{$ELSE}
asm
{       PROCEDURE _StrLong( val: Longint; width: Longint; VAR s: ShortString );
      ->EAX     Value
        EDX     Width
        ECX     Pointer to string       }

        PUSH    EBX             { VAR i: Longint;               }
        PUSH    ESI             { VAR sign : Longint;           }
        PUSH    EDI
        PUSH    EDX             { store width on the stack      }
        SUB     ESP,20          { VAR a: array [0..19] of Char; }

        MOV     EDI,ECX

        MOV     ESI,EAX         { sign := val                   }

        CDQ                     { val := Abs(val);  canned sequence }
        XOR     EAX,EDX
        SUB     EAX,EDX

        MOV     ECX,10
        XOR     EBX,EBX         { i := 0;                       }

@@repeat1:                      { repeat                        }
        XOR     EDX,EDX         {   a[i] := Chr( val MOD 10 + Ord('0') );}

        DIV     ECX             {   val := val DIV 10;          }

        ADD     EDX,'0'
        MOV     [ESP+EBX],DL
        INC     EBX             {   i := i + 1;                 }
        TEST    EAX,EAX         { until val = 0;                }
        JNZ     @@repeat1

        TEST    ESI,ESI
        JGE     @@2
        MOV     byte ptr [ESP+EBX],'-'
        INC     EBX
@@2:
        MOV     [EDI],BL        { s^++ := Chr(i);               }
        INC     EDI

        MOV     ECX,[ESP+20]    { spaceCnt := width - i;        }
        CMP     ECX,255
        JLE     @@3
        MOV     ECX,255
@@3:
        SUB     ECX,EBX
        JLE     @@repeat2       { for k := 1 to spaceCnt do s^++ := ' ';        }
        ADD     [EDI-1],CL
        MOV     AL,' '
        REP     STOSB

@@repeat2:                      { repeat                        }
        MOV     AL,[ESP+EBX-1]  {   s^ := a[i-1];               }
        MOV     [EDI],AL
        INC     EDI             {   s := s + 1                  }
        DEC     EBX             {   i := i - 1;                 }
        JNZ     @@repeat2       { until i = 0;                  }

        ADD     ESP,20+4
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

function _Str0Long(val: Longint): ShortString;
begin
  Result := _StrLong(val, 0);
end;

procedure _Truncate(var f: TFileRec);
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
begin
  if (f.Mode = fmOutput) or (f.Mode = fmInOut) then
  begin
    if not SetEndOfFile(f.Handle) then
      InOutError;
  end else
    SetInOutRes(103);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
// -> EAX Pointer to text or file variable

       MOVZX   EDX,[EAX].TFileRec.Mode   // check whether file is open
       SUB     EDX,fmInput
       CMP     EDX,fmInOut-fmInput
       JA      @@fileNotOpen

       PUSH    [EAX].TFileRec.Handle
       CALL    SetEndOfFile
       DEC     EAX
       JZ      @@exit
       JMP     InOutError

@@fileNotOpen:
       MOV     EAX,103
       JMP     SetInOutRes

@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  if (f.Mode and fmOutput) = fmOutput then  // fmOutput or fmInOut
  begin
    if ftruncate(f.Handle, lseek(f.Handle, 0, SEEK_CUR)) = -1 then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ENDIF POSIX}

// Hex : ( '$' | 'X' | 'x' | '0X' | '0x' ) [0-9A-Fa-f]*
// Dec : ( '+' | '-' )? [0-9]*
function _ValLong(const s: string; var code: Integer): Longint;
{$IFDEF PUREPASCAL}
var
  I, Len, Digit: Integer;
  Negative, Hex: Boolean;
begin
  // U-OK
  I := 1;
  code := -1;
  Result := 0;
  Negative := False;
  Hex := False;
  Len := Length(s);
  while (I <= Len) and (s[I] = ' ') do
    Inc(I);
  if I > Len then
    Exit;
  case s[I] of
    '$',
    'x',
    'X':
      begin
        if I = Len then
        begin
          Code := I + 2; // Emulate Win32 _ValLong behaviour
          Exit;
        end;
        Hex := True;
        Inc(I);
      end;
    '0':
      begin
        Hex := (Len > I) and ((s[I+1] = 'X') or (s[I+1] = 'x'));
        if Hex then
          Inc(I, 2);
      end;
    '-':
      begin
        if I = Len then
        begin
          Code := I + 1; // Emulate Win32 _ValLong behaviour
          Exit;
        end;
        Negative := True;
        Inc(I);
      end;
    '+':
      begin
        if I = Len then
        begin
          Code := I + 1; // Emulate Win32 _ValLong behaviour
          Exit;
        end;
        Inc(I);
      end;
  end;
  if Hex then
    while I <= Len do
    begin
      // check for overflow
      if Result > (High(Result) shr 3) then
      begin
        code := I;
        Exit;
      end;
      case s[I] of
        '0'..'9': Result := Result * 16 + Ord(s[I]) - Ord('0');
        'a'..'f': Result := Result * 16 + Ord(s[I]) - Ord('a') + 10;
        'A'..'F': Result := Result * 16 + Ord(s[I]) - Ord('A') + 10;
      else
        code := I;
        Exit;
      end;
      Inc(I);
    end
  else
    while I <= Len do
    begin
      // check for overflow
      if Result > (High(Result) div 10) then
      begin
        code := I;
        Exit;
      end;
      Digit := Ord(s[I]) - Ord('0');
      if (Digit < 0) or (Digit > 9) then begin
         Code := I;
         Exit;
      end;
      Result := Result * 10 + Ord(s[I]) - Ord('0');
      Inc(I);
    end;
  if Negative then
    Result := -Result;
  code := 0;
end;
{$ELSE !PUREPASCAL}
asm
{       FUNCTION _ValLong( s: string; VAR code: Integer ) : Longint;        }
{     ->EAX     Pointer to string       }
{       EDX     Pointer to code result  }
{     <-EAX     Result                  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        PUSH    EAX             { save for the error case       }

        TEST    EAX,EAX
        JE      @@empty

        XOR     EAX,EAX
        XOR     EBX,EBX
        MOV     EDI,07FFFFFFFH / 10     { limit }

@@blankLoop:
        MOV     BX,[ESI]
        ADD     ESI, 2
        CMP     BX,' '
        JE      @@blankLoop

@@endBlanks:
        MOV     CH,0
        CMP     BX,'-'
        JE      @@minus
        CMP     BX,'+'
        JE      @@plus

@@checkDollar:
        CMP     BX,'$'
        JE      @@dollar

        CMP     BX, 'x'
        JE      @@dollar
        CMP     BX, 'X'
        JE      @@dollar
        CMP     BX, '0'
        JNE     @@firstDigit
        MOV     BX, [ESI]
        ADD     ESI, 2
        CMP     BX, 'x'
        JE      @@dollar
        CMP     BX, 'X'
        JE      @@dollar
        TEST    BX, BX
        JE      @@endDigits
        JMP     @@digLoop

@@firstDigit:
        TEST    BX,BX
        JE      @@error

@@digLoop:
        SUB     BX,'0'
        CMP     BX,9
        JA      @@error
        CMP     EAX,EDI         { value > limit ?       }
        JA      @@overFlow
        LEA     EAX,[EAX+EAX*4]
        ADD     EAX,EAX
        ADD     EAX,EBX         { fortunately, we can't have a carry    }
        MOV     BX,[ESI]
        ADD     ESI, 2
        TEST    BX,BX
        JNE     @@digLoop

@@endDigits:
        DEC     CH
        JE      @@negate
        TEST    EAX,EAX
        JGE     @@successExit
        JMP     @@overFlow

@@empty:
        ADD     ESI, 2
        JMP     @@error

@@negate:
        NEG     EAX
        JLE     @@successExit
        JS      @@successExit           { to handle 2**31 correctly, where the negate overflows }

@@error:
@@overFlow:
        POP     EBX
        SUB     ESI,EBX
        JMP     @@exit

@@minus:
        INC     CH
@@plus:
        MOV     BX,[ESI]
        ADD     ESI, 2
        JMP     @@checkDollar

@@dollar:
        MOV     EDI,0FFFFFFFH
        MOV     BX,[ESI]
        ADD     ESI, 2
        TEST    BX,BX
        JZ      @@empty

@@hDigLoop:
        CMP     BX,'a'
        JB      @@upper
        SUB     BX,'a' - 'A'
@@upper:
        SUB     BX,'0'
        CMP     BX,9
        JBE     @@digOk
        SUB     BX,'A' - '0'
        CMP     BX,5
        JA      @@error
        ADD     BX,10
@@digOk:
        CMP     EAX,EDI
        JA      @@overFlow
        SHL     EAX,4
        ADD     EAX,EBX
        MOV     BX,[ESI]
        ADD     ESI, 2
        TEST    BX,BX
        JNE     @@hDigLoop

        DEC     CH
        JNE     @@successExit
        NEG     EAX

@@successExit:
        POP     ECX                     { saved copy of string pointer  }
        XOR     ESI,ESI         { signal no error to caller     }

@@exit:
        SHR     ESI, 1
        MOV     [EDX],ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

function _WriteRec(var f: TFileRec; buffer: Pointer): Pointer;
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  NumWritten: Cardinal;
begin
  if (f.Mode = fmOutput) or (f.Mode = fmInOut) then
  begin
    if not WriteFile(f.Handle, Buffer, f.RecSize, NumWritten, nil) then
      SetInOutRes(GetLastError)
    else
    begin
      if f.RecSize <> NumWritten then
        SetInOutRes(101);
    end;
  end
  else
    SetInOutRes(5);
  Result := @F;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
// -> EAX Pointer to file variable
//    EDX Pointer to buffer
// <- EAX Pointer to file variable
        PUSH    EBX

        MOV     EBX,EAX

        MOVZX   EAX,[EAX].TFileRec.Mode
        SUB     EAX,fmOutput
        CMP     EAX,fmInOut-fmOutput  // File must be fmInOut or fmOutput
        JA      @@fileNotOpen

//  WriteFile(f.Handle, buffer^, f.RecSize, @result, Nil);

        PUSH    0                       // space for OS result
        MOV     EAX,ESP

        PUSH    0                       // pass lpOverlapped
        PUSH    EAX                     // pass @result
        PUSH    [EBX].TFileRec.RecSize  // pass nNumberOfBytesToRead
        PUSH    EDX                     // pass lpBuffer
        PUSH    [EBX].TFileRec.Handle   // pass hFile
        CALL    WriteFile
        POP     EDX                     // pop result
        DEC     EAX                     // check EAX = TRUE
        JNZ     @@error

        CMP     EDX,[EBX].TFileRec.RecSize  // result = f.RecSize ?
        JE      @@exit

@@writeError:
        MOV     EAX,101
        JMP     @@errExit

@@fileNotOpen:
        MOV     EAX,5
        JMP     @@errExit

@@error:
        CALL    GetLastError
@@errExit:
        CALL    SetInOutRes
@@exit:
        MOV     EAX,EBX
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  Dummy: Integer;
begin
  _BlockWrite(f, Buffer, 1, Dummy);
  Result := @F;
end;
{$ENDIF POSIX}

// If the file is Output or ErrOutput std variable, try to open it
// Otherwise, runtime error.
function TryOpenForOutput(var t: TTextRec): Boolean;
begin
  if (@t = @Output) or (@t = @ErrOutput) then
  begin
    t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
    _RewritText(t);
  end;

  Result := t.Mode = fmOutput;
  if not Result then
    SetInOutRes(105);
end;

function _WriteBytes(var t: TTextRec; const b; cnt : Longint): Pointer;
{$IFDEF PUREPASCAL}
var
  Dest, Source: PAnsiChar;
  RemainingBytes: Longint;
  Temp: Integer;
begin
  // U-OK
  Result := @t;
  if (t.Mode <> fmOutput) and not TryOpenForOutput(t) then Exit;

  Source := Pointer(@b);
  Dest := t.BufPtr + t.BufPos;
  RemainingBytes := t.BufSize - t.BufPos;
  while RemainingBytes <= cnt do
  begin
    Inc(t.BufPos, RemainingBytes);
    Dec(cnt, RemainingBytes);
    Move(Source^, Dest^, RemainingBytes);
    Inc(Source, RemainingBytes);
    Temp := TTextIOFunc(t.InOutFunc)(t);
    if Temp <> 0 then
    begin
      SetInOutRes(Temp);
      Exit;
    end;
    Dest := t.BufPtr + t.BufPos;
    RemainingBytes := t.BufSize - t.BufPos;
  end;
  Inc(t.BufPos, cnt);
  Move(Source^, Dest^, cnt);
end;
{$ELSE !PUREPASCAL}
asm //StackAlignSafe
// -> EAX Pointer to file record
//    EDX Pointer to buffer
//    ECX Number of bytes to write
// <- EAX Pointer to file record

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX

        CMP     [EAX].TTextRec.Mode,fmOutput
        JE      @@loop
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    TryOpenForOutput
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        TEST    AL,AL
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit

@@loop:
        MOV     EDI,[EAX].TTextRec.BufPtr
        ADD     EDI,[EAX].TTextRec.BufPos

//  remainingBytes = t.bufSize - t.bufPos

        MOV     EDX,[EAX].TTextRec.BufSize
        SUB     EDX,[EAX].TTextRec.BufPos

//  if (remainingBytes <= cnt)

        CMP     EDX,ECX
        JG      @@1

//  t.BufPos += remainingBytes, cnt -= remainingBytes

        ADD     [EAX].TTextRec.BufPos,EDX
        SUB     ECX,EDX

//  copy remainingBytes, advancing ESI

        PUSH    EAX
        PUSH    ECX
        MOV     ECX,EDX
        REP     MOVSB

{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    [EAX].TTextRec.InOutFunc
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JNZ     @@error

        POP     ECX
        POP     EAX
        JMP     @@loop

@@error:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SetInOutRes
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     ECX
        POP     EAX
        JMP     @@exit
@@1:
        ADD     [EAX].TTextRec.BufPos,ECX
        REP     MOVSB

@@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF}

function _WriteSpaces(var t: TTextRec; cnt: Longint): Pointer;
var
  A: AnsiString;
begin
  Result := @t;
  if cnt > 64 then
  begin
    SetLength(A, 64);
    FillChar(A[1], 64, #$20);
    while cnt > 64 do
    begin
      _Write0LString(t, A);
      if InOutRes <> 0 then Exit;
      Dec(cnt, 64);
    end;
  end;
  if cnt > 0 then
  begin
    SetLength(A, cnt);
    FillChar(A[1], cnt, #$20);
    _Write0LString(t, A);
    if InOutRes <> 0 then Exit;
  end;
end;

procedure InternalUStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer; CodePage: Integer); forward;


function _Write0Char(var t: TTextRec; c: AnsiChar): Pointer;

  procedure WriteUnicodeFromMBCSBuffer;
  var
    U : UnicodeString;
  begin
    InternalUStrFromPCharLen(U, t.MBCSBuffer, t.MBCSLength, DefaultSystemCodePage);
    _Write0UString(t, U);
    t.MBCSLength := 0;
    t.MBCSBufPos := 0;
  end;

begin
  if t.CodePage = 0 then TryOpenForOutput(t);
  if (t.CodePage = DefaultSystemCodePage) then
  begin
    Result := _WriteBytes(t, c, 1);
    exit;
  end;

  if t.MBCSLength = 0 then
  begin
    t.MBCSLength := 1;
    if DefaultSystemCodePage = CP_UTF8 then
    begin
      if byte(c) in [$C2..$DF] then
        t.MBCSLength := 2
      else if byte(c) in [$E0..$EF] then
        t.MBCSLength := 3
      else if byte(c) in [$F0..$F4] then
        t.MBCSLength := 4;
    end
{$IFDEF MSWINDOWS}
    else if IsDBCSLeadByteEx(UINT(DefaultSystemCodePage), Byte(c)) then
      t.MBCSLength := 2;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
                                                     
    else
      t.MBCSLength := 1;
{$ENDIF POSIX}
    t.MBCSBufPos := 0;
  end;

  if t.MBCSLength > 0 then
  begin
    t.MBCSBuffer[t.MBCSBufPos] := c;
    Inc(t.MBCSBufPos);
    if t.MBCSBufPos = byte(t.MBCSLength) then
      WriteUnicodeFromMBCSBuffer;
  end;
  Result := @t;
end;

function _WriteChar(var t: TTextRec; c: AnsiChar; width: Integer): Pointer;
var
  A: AnsiString;
begin
  if width <= 1 then
    Result := _Write0Char(t, c)
  else
  begin
    __FlushMBCSBuffer(t);
    if t.CodePage = 0 then TryOpenForOutput(t);
    if t.CodePage = DefaultSystemCodePage then
    begin
      _WriteSpaces(t, width-1);
      Result := _WriteBytes(t, c, 1);
    end
    else
    begin
      _LStrFromChar(A, c, DefaultSystemCodePage);
      Result := _WriteLString(t, A, width);
    end;
  end;
end;

function _WriteBool(var t: TTextRec; val: Boolean; width: Longint): Pointer;
const
  BoolStrs: array [Boolean] of ShortString = ('FALSE', 'TRUE');
begin
  Result := _WriteString(t, BoolStrs[val], width);
end;

function _Write0Bool(var t: TTextRec; val: Boolean): Pointer;
begin
  Result := _WriteBool(t, val, 0);
end;

function _WriteLong(var t: TTextRec; val, width: Longint): Pointer;
var
  S: string[31];
begin
  Str(val:0, S);
  Result := _WriteString(t, S, width);
end;

function _Write0Long(var t: TTextRec; val: Longint): Pointer;
begin
  Result := _WriteLong(t, val, 0);
end;

function _Write0String(var t: TTextRec; const s: ShortString): Pointer;
begin
  result := _WriteString(t, s, 0);
end;

function _WriteString(var t: TTextRec; const s: ShortString; width: Longint): Pointer;
begin
  Result := _WriteLString(t, s, width);
end;

function _Write0CString(var t: TTextRec; s: PAnsiChar): Pointer;
begin
  Result := _WriteCString(t, s, 0);
end;

function _WriteCString(var t: TTextRec; s: PAnsiChar; width: Longint): Pointer;
var
  A: AnsiString;
begin
  _LStrFromPChar(A, s, DefaultSystemCodePage);
  Result := _WriteLString(t, A, width);
end;

function _Write0LString(var t: TTextRec; const s: AnsiString): Pointer;
begin
  __FlushMBCSBuffer(t);
  Result := @t;
  if s <> '' then
  begin
    if t.CodePage = 0 then TryOpenForOutput(t);
    if t.CodePage = PWord(PByte(s) - 12)^ then
      Result := _WriteBytes(t, s[1], Length(s))
    else
      Result := _Write0UString(t, UnicodeString(s));
  end;
end;

function _WriteLString(var t: TTextRec; const s: AnsiString; width: Longint): Pointer;
var
  i: Integer;
begin
  __FlushMBCSBuffer(t);
  if s = '' then
    Result := _WriteSpaces(t, width)
  else
  begin
    if t.CodePage = 0 then TryOpenForOutput(t);
    if t.CodePage = PWord(PByte(s) - 12)^ then
    begin
      i := Length(s);
      _WriteSpaces(t, width - i);
      Result := _WriteBytes(t, s[1], i);
    end
    else
      Result := _WriteUString(t, UnicodeString(s), width);
  end;
end;

function _Write0UString(var t: TTextRec; const s: UnicodeString): Pointer;
begin
  Result := _WriteUString(t, s, 0);
end;

function _WriteUString(var t: TTextRec; const s: UnicodeString; width: Longint): Pointer;
var
  i: Integer;
  A: AnsiString;
begin
  if s = '' then
    Result := _WriteSpaces(t, width )
  else
  begin
    if t.CodePage = 0 then TryOpenForOutput(t);
    if t.CodePage = CP_UTF16 then // Output is UTF16
    begin
      i := Length(s);
      _WriteSpaces(t, width - i);
      Result := _WriteBytes(t, s[1], i*sizeof(WideChar));
    end
    else
    begin
      if assigned(AlternateWriteUnicodeStringProc) then
        Result := AlternateWriteUnicodeStringProc(t, s)
      else
      begin
        _LStrFromUStr(A, s, t.CodePage);
        i := Length(A);
        _WriteSpaces(t, width - i);
        Result := _WriteBytes(t, A[1], i);
      end;
    end;
  end;
end;

function _Write0WString(var t: TTextRec; const s: WideString): Pointer;
begin
  Result := _WriteUString(t, s, 0);
end;

function _WriteWString(var t: TTextRec; const s: WideString; width: Longint): Pointer;
begin
  Result := _WriteUString(t, UnicodeString(s), width);
end;

function _Write0WCString(var t: TTextRec; s: PWideChar): Pointer;
begin
  Result := _WriteUString(t, UnicodeString(s), 0);
end;

function _WriteWCString(var t: TTextRec; s: PWideChar; width: Longint): Pointer;
begin
  Result := _WriteUString(t, UnicodeString(s), width);
end;

function _Write0WChar(var t: TTextRec; c: WideChar): Pointer;
begin
  Result := @t;
  if t.CodePage = 0 then TryOpenForOutput(t);
  if t.CodePage = CP_UTF16 then
  begin
    _WriteBytes(t, c, sizeof(WideChar));
    exit;
  end;

  if t.MBCSLength > 0 then
  begin
    if (Word(c) >= $DC00) and (Word(c) < $E000) then
    begin
      t.UTF16Buffer[1] := c;
      t.UTF16Buffer[2] := #0;

      _WriteUString(t, UnicodeString(t.UTF16Buffer), 0);
    end;
    t.MBCSLength := 0;
    exit;
  end;

  if (Word(c) >= $D800) and (Word(c) < $DC00) then
  begin
    t.MBCSLength := 2;
    t.UTF16Buffer[0] := c;
  end
  else
    _WriteUString(t, UnicodeString(c), 0);
end;

function _WriteWChar(var t: TTextRec; c: WideChar; width: Integer): Pointer;
begin
  if width <= 1 then
    result := _Write0WChar(t, c)
  else
  begin
    if t.UTF16Buffer[0] <> #0 then
    begin
      _Write0WChar(t, '?');
      t.UTF16Buffer[0] := #0;
    end;

    _WriteSpaces(t, width - 1);
    Result := _Write0WChar(t, c);
  end;
end;

function _Write0Variant(var T: TTextRec; const V: TVarData): Pointer;
begin
  Result := _WriteVariant(T, V, 0);
end;

function _WriteVariant(var T: TTextRec; const V: TVarData; Width: Integer): Pointer;
var
  S: AnsiString;
  U: UnicodeString;
begin
  if (V.VType = varString) and Assigned(VarToLStrProc) then
  begin
    VarToLStrProc(S, V);
    _WriteLString(T, S, Width);
  end
  else if Assigned(VarToUStrProc) then
  begin
    VarToUStrProc(U, V);
    _WriteUString(T, U, Width);
  end
  else
    Error(reVarInvalidOp);
  Result := @T;
end;

{$IFNDEF CPUX86}
function _Write2Ext(var T: TTextRec; Val: Extended; Width, Prec: LongInt): Pointer;
var
 S : ShortString;
begin
  S := _Str2Ext(Val, Width, Prec);
  Result := _WriteString(T, S, Width);
end;
{$ELSE CPUX86}
procedure _Write2Ext;
const
{$IFDEF MACOS}
  SizeOfExtendedOnStack = 16;
{$ELSE}
  SizeOfExtendedOnStack = 12;
{$ENDIF}
asm
{       PROCEDURE _Write2Ext( VAR t: Text; val: Extended; width, prec: Longint);
      ->EAX     Pointer to file record
        [ESP+4] Extended value
        EDX     Field width
        ECX     precision (<0: scientific, >= 0: fixed point)   }

        FLD     tbyte ptr [ESP+4]       { load value            }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF}
        SUB     ESP,256                 { VAR s: String;        }

        PUSH    EAX
        PUSH    EDX

{       Str( val, width, prec, s );     }

        SUB     ESP, SizeOfExtendedOnStack
        FSTP    tbyte ptr [ESP]         { pass value                    }
        MOV     EAX,EDX                 { pass field width              }
        MOV     EDX,ECX                 { pass precision                }
                                        { pass destination string    }
        LEA     ECX,[ESP+8+SizeOfExtendedOnStack] 
        CALL    _Str2Ext

{       Write( t, s, width );   }

        POP     ECX                     { pass width    }
        POP     EAX                     { pass text     }
        MOV     EDX,ESP                 { pass string   }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF}
        CALL    _WriteString

        ADD     ESP,256
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF}
        RET     SizeOfExtendedOnStack
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _Write1Ext(var T: TTextRec; Val: Extended; Width: LongInt): Pointer;
begin
  Result := _Write2Ext(T, Val, Width, -1);
end;
{$ELSE CPUX86}
procedure _Write1Ext;
asm
{       PROCEDURE _Write1Ext( VAR t: Text; val: Extended; width: Longint);
  ->    EAX     Pointer to file record
        [ESP+4] Extended value
        EDX     Field width             }

        OR      ECX,-1
        JMP     _Write2Ext
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _Write0Ext(var T: TTextRec; Val: Extended): Pointer;
begin
  Result := _Write2Ext(T, Val, 23, -1);
end;
{$ELSE CPUX86}
procedure _Write0Ext;
asm
{       PROCEDURE _Write0Ext( VAR t: Text; val: Extended);
      ->EAX     Pointer to file record
        [ESP+4] Extended value  }

        MOV     EDX,23  { field width   }
        OR      ECX,-1
        JMP     _Write2Ext
end;
{$ENDIF CPUX86}


{$IFNDEF CPUX86}
function _Write2Comp(var T: TTextRec; Val: Comp; Width, Prec: LongInt): Pointer;
var
 S : ShortString;
begin
  S := _Str2Comp(Val, Width, Prec);
  Result := _WriteString(T, S, Width);
end;

function _Write1Comp(var T: TTextRec; Val: Comp; Width: LongInt): Pointer;
begin
  Result := _Write2Comp(T, Val, Width, -1);
end;

function _Write0Comp(var T: TTextRec; Val: Comp): Pointer;
begin
  Result := _Write2Comp(T, Val, 23, -1);
end;

function _Write2Currency(var T: TTextRec; Val: Currency; Width, Prec: LongInt): Pointer;
var
 S : ShortString;
begin
  S := _Str2Currency(Val, Width, Prec);
  Result := _WriteString(T, S, Width);
end;

function _Write1Currency(var T: TTextRec; Val: Currency; Width: LongInt): Pointer;
begin
  Result := _Write2Currency(T, Val, Width, -1);
end;

function _Write0Currency(var T: TTextRec; Val: Currency): Pointer;
begin
  Result := _Write2Currency(T, Val, 23, -1);
end;
{$ENDIF !CPUX86}


function _WriteLn(var t: TTextRec): Pointer;
begin
  if (t.Flags and tfCRLF) <> 0 then
    _Write0Char(t, AnsiChar(cCR));
  Result := _Write0Char(t, AnsiChar(cLF));
  _Flush(t);
end;

procedure __CToPasStr(Dest: PShortString; const Source: PAnsiChar);
begin
  __CLenToPasStr(Dest, Source, 255);
end;

procedure __CLenToPasStr(Dest: PShortString; const Source: PAnsiChar; MaxLen: NativeInt);
{$IFDEF PUREPASCAL}
var
  I: NativeInt;
begin
  I := 0;
  if Source <> nil then
  begin
    if MaxLen > 255 then MaxLen := 255;
    while (Source[I] <> #0) and (I < MaxLen) do
    begin
      Dest^[I+1] := Source[I];
      Inc(I);
    end;
  end;
  Byte(Dest^[0]) := I;
end;
{$ELSE !PUREPASCAL}
asm
{     ->EAX     Pointer to destination  }
{       EDX     Pointer to source       }
{       ECX     cnt                     }

        PUSH    EBX
        PUSH    EAX             { save destination      }

        TEST    EDX,EDX
        JZ      @@nilStr
        CMP     ECX,255
        JBE     @@loop
        MOV     ECX,255
@@loop:
        MOV     BL,[EDX]        { ch = *src++;          }
        INC     EDX
        TEST    BL,BL           { if (ch == 0) break    }
        JE      @@endLoop
        INC     EAX             { *++dest = ch;         }
        MOV     [EAX],BL
        DEC     ECX             { while (--cnt != 0)    }
        JNZ     @@loop

@@endLoop:
        POP     EDX
        SUB     EAX,EDX

@@setLength:
        MOV     [EDX],AL
        POP     EBX
        RET

@@nilStr:
        POP     EDX
        XOR     EAX,EAX
        JMP     @@setLength
end;
{$ENDIF !PUREPASCAL}

procedure __ArrayToPasStr(Dest: PShortString; const Source: PAnsiChar; Len: NativeInt);
begin
  if Len > 255 then Len := 255;
  Byte(Dest^[0]) := Len;
  Move(Source^, Dest^[1], Len);
end;

procedure __PasToCStr(const Source: PShortString; const Dest: PAnsiChar);
begin
  Move(Source^[1], Dest^, Byte(Source^[0]));
  Dest[Byte(Source^[0])] := #0;
end;


{ ----------------------------------------------------- }
{       Compiler helper for set type support            }
{ ----------------------------------------------------- }

{$IFNDEF CPUX86}
procedure _SetElem(var Dest {:Set}; Elem, Size: Integer);
var
  P: PByte;
  I: Integer;
begin
  P := @Dest;
  for I := 0 to Size - 1 do
    P[I] := 0;
  if (Elem >= 0) and ((Elem div 8) < Size) then
    P[Elem div 8] := 1 shl (Elem mod 8);
end;
{$ELSE CPUX86}
procedure       _SetElem;
asm
        {       PROCEDURE _SetElem( VAR d: SET; elem, size: Byte);      }
        {       EAX     =       dest address                            }
        {       DL      =       element number                          }
        {       CL      =       size of set                             }

        PUSH    EBX
        PUSH    EDI

        MOV     EDI,EAX

        XOR     EBX,EBX { zero extend set size into ebx }
        MOV     BL,CL
        MOV     ECX,EBX { and use it for the fill       }

        XOR     EAX,EAX { for zero fill                 }
        REP     STOSB

        SUB     EDI,EBX { point edi at beginning of set again   }

        INC     EAX             { eax is still zero - make it 1 }
        MOV     CL,DL
        ROL     AL,CL   { generate a mask               }
        SHR     ECX,3   { generate the index            }
        CMP     ECX,EBX { if index >= siz then exit     }
        JAE     @@exit
        OR      [EDI+ECX],AL{ set bit                   }

@@exit:
        POP     EDI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetRange(Lo, Hi, Size: Integer; var Dest {:Set});
var
  P: PByte;
  I: Integer;
  LoIndex, HiIndex: Integer;
  LoMask, HiMask: Byte;
begin
  P := @dest;
  for I := 0 to Size - 1 do
    P[I] := 0;
  if Hi >= Size * 8 then
    Hi := Size * 8 - 1;
  if Lo < 0 then
    Lo := 0;
  if Lo <= Hi then
  begin
    LoMask := $ff shl (Lo mod 8);
    LoIndex := Lo div 8;
    HiMask := LongWord($FF) shr (7 - (Hi mod 8));
    HiIndex := Hi div 8;
    P[LoIndex] := LoMask;
    for I := LoIndex+1 to HiIndex do
      P[I] := $ff;
    P[HiIndex] := P[HiIndex] and HiMask;
  end;
end;
{$ELSE CPUX86}
procedure _SetRange;
asm
{       PROCEDURE _SetRange( lo, hi, size: Byte; VAR d: SET );  }
{ ->    AL      low limit of range      }
{       DL      high limit of range     }
{       ECX     Pointer to set          }
{       AH      size of set             }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        XOR     EBX,EBX { EBX = set size                }
        MOV     BL,AH
        MOVZX   ESI,AL  { ESI = low zero extended       }
        MOVZX   EDX,DL  { EDX = high zero extended      }
        MOV     EDI,ECX

{       clear the set                                   }

        MOV     ECX,EBX
        XOR     EAX,EAX
        REP     STOSB

{       prepare for setting the bits                    }

        SUB     EDI,EBX { point EDI at start of set     }
        SHL     EBX,3   { EBX = highest bit in set + 1  }
        CMP     EDX,EBX
        JB      @@inrange
        LEA     EDX,[EBX-1]     { ECX = highest bit in set      }

@@inrange:
        CMP     ESI,EDX { if lo > hi then exit;         }
        JA      @@exit

        DEC     EAX     { loMask = 0xff << (lo & 7)             }
        MOV     ECX,ESI
        AND     CL,07H
        SHL     AL,CL

        SHR     ESI,3   { loIndex = lo >> 3;            }

        MOV     CL,DL   { hiMask = 0xff >> (7 - (hi & 7));      }
        NOT     CL
        AND     CL,07
        SHR     AH,CL

        SHR     EDX,3   { hiIndex = hi >> 3;            }

        ADD     EDI,ESI { point EDI to set[loIndex]     }
        MOV     ECX,EDX
        SUB     ECX,ESI { if ((inxDiff = (hiIndex - loIndex)) == 0)     }
        JNE     @@else

        AND     AL,AH   { set[loIndex] = hiMask & loMask;       }
        MOV     [EDI],AL
        JMP     @@exit

@@else:
        STOSB           { set[loIndex++] = loMask;      }
        DEC     ECX
        MOV     AL,0FFH { while (loIndex < hiIndex)     }
        REP     STOSB   {   set[loIndex++] = 0xff;      }
        MOV     [EDI],AH        { set[hiIndex] = hiMask;        }

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _SetEq(L, R: Pointer; Size: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Size - 1 do
    if PByte(L)[I] <> PByte(R)[I] then Exit;
  Result := True;
end;
{$ELSE CPUX86}
procedure _SetEq;
asm
{       FUNCTION _SetEq( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand    }
{       EDX     =       right operand   }
{       CL      =       size of set     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        AND     ECX,0FFH

@@Loop:
        DEC     ECX
        JZ      @@ByteCheck
        MOVZX   EAX,WORD PTR [ESI+ECX-1]
        MOVZX   EDX,WORD PTR [EDI+ECX-1]
        CMP     EAX,EDX
        JNE     @@Leave
        DEC     ECX
        JNZ     @@Loop
@@Leave:

        POP     EDI
        POP     ESI
        RET

@@ByteCheck:
        MOV     AL,[ESI+ECX]
        MOV     DL,[EDI+ECX]
        CMP     AL,DL
        JNE     @@Leave
        OR      ECX,ECX // set zero flag

        POP     EDI
        POP     ESI
        RET
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _SetLe(L, R: Pointer; Size: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Size - 1 do
    if (PByte(L)[I] and not PByte(R)[I]) <> 0 then Exit;
  Result := True;
end;
{$ELSE CPUX86}
procedure _SetLe;
asm
{       FUNCTION _SetLe( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand            }
{       EDX     =       right operand           }
{       CL      =       size of set (>0 && <= 32)       }

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        AND     CH,[EAX]
        JNE     @@exit
        INC     EDX
        INC     EAX
        DEC     CL
        JNZ     @@loop
@@exit:
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetIntersect(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Size - 1 do
    PD[I] := PD[I] and PS[I];
end;
{$ELSE CPUX86}
procedure _SetIntersect;
asm
{       PROCEDURE _SetIntersect( VAR dest: Set; CONST src: Set; size: Byte);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        INC     EDX
        AND     [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetIntersect3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PL, PR: PByte;
begin
  PD := PByte(@Dest);
  PL := PByte(L);
  PR := PByte(R);
  for I := 0 to Size - 1 do
    PD[I] := PL[I] and PR[I];
end;
{$ELSE CPUX86}
procedure _SetIntersect3;
asm
{       PROCEDURE _SetIntersect3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{       [ESP+4] = 2nd source operand                    }

        PUSH    EBX
        PUSH    ESI
        MOV     ESI,[ESP+8+4]
@@loop:
        MOV     BL,[EDX+ECX-1]
        AND     BL,[ESI+ECX-1]
        MOV     [EAX+ECX-1],BL
        DEC     ECX
        JNZ     @@loop

        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetUnion(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Size - 1 do
    PD[I] := PD[I] or PS[I];
end;
{$ELSE CPUX86}
procedure _SetUnion;
asm
{       PROCEDURE _SetUnion( VAR dest: Set; CONST src: Set; size: Byte);        }
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        INC     EDX
        OR      [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetUnion3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PL, PR: PByte;
begin
  PD := PByte(@Dest);
  PL := PByte(L);
  PR := PByte(R);
  for I := 0 to Size - 1 do
    PD[I] := PL[I] or PR[I];
end;
{$ELSE CPUX86}
procedure _SetUnion3;
asm
{       PROCEDURE _SetUnion3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{ [ESP+4] = 2nd source operand    }

      PUSH  EBX
      PUSH  ESI
      MOV   ESI,[ESP+8+4]
@@loop:
      MOV   BL,[EDX+ECX-1]
      OR    BL,[ESI+ECX-1]
      MOV   [EAX+ECX-1],BL
      DEC   ECX
      JNZ   @@loop

      POP   ESI
      POP   EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetSub(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Size - 1 do
    PD[I] := PD[I] and not PS[I];
end;
{$ELSE CPUX86}
procedure _SetSub;
asm
{       PROCEDURE _SetSub( VAR dest: Set; CONST src: Set; size: Byte);  }
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        INC     EDX
        AND     [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetSub3(var Dest {:set}; L, R: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PL, PR: PByte;
begin
  PD := PByte(@Dest);
  PL := PByte(L);
  PR := PByte(R);
  for I := 0 to Size - 1 do
    PD[I] := PL[I] and not PR[I];
end;
{$ELSE CPUX86}
procedure _SetSub3;
asm
{       PROCEDURE _SetSub3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{       [ESP+4] = 2nd source operand                    }

        PUSH    EBX
        PUSH    ESI
        MOV     ESI,[ESP+8+4]
@@loop:
        MOV     BL,[ESI+ECX-1]
        NOT     BL
        AND     BL,[EDX+ECX-1]
        MOV     [EAX+ECX-1],BL
        DEC     ECX
        JNZ     @@loop

        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _SetExpand(Src: Pointer{PSet}; var Dest {:Set}; Lo, Hi: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Lo - 1 do
    PD[I] := 0;
  for I := Lo to Hi - 1 do
    PD[I] := PS[I - Lo];
  for I := Hi to 31 do
    PD[I] := 0;
end;
{$ELSE CPUX86}
procedure _SetExpand;
asm
{       PROCEDURE _SetExpand( CONST src: Set; VAR dest: Set; lo, hi: Byte);     }
{     ->EAX     Pointer to source (packed set)          }
{       EDX     Pointer to destination (expanded set)   }
{       CH      high byte of source                     }
{       CL      low byte of source                      }

{       algorithm:              }
{       clear low bytes         }
{       copy high-low+1 bytes   }
{       clear 31-high bytes     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        MOV     EDX,ECX { save low, high in dl, dh      }
        XOR     ECX,ECX
        XOR     EAX,EAX

        MOV     CL,DL   { clear low bytes               }
        REP     STOSB

        MOV     CL,DH   { copy high - low bytes }
        SUB     CL,DL
        REP     MOVSB

        MOV     CL,32   { copy 32 - high bytes  }
        SUB     CL,DH
        REP     STOSB

        POP     EDI
        POP     ESI
end;
{$ENDIF CPUX86}

function ScaleExt(var val: Extended): Integer;
var
  expBase10: Integer;
  ExpBase2: Int16;
begin
  Result := 0;
  while (val <> 0) do
  begin
    ExpBase2 := PExtendedRec(@val)^.Exponent;
    // exp10 * 2 ** 16 = exp2 * log10(2) * 2**16. Log10(2) * 2 ** 16 ~= 19728
    expBase10 := ExpBase2 * 19728;
    // Temp = High 16 bits of result, sign extended
    expBase10 := PSmallInt(PByte(@expBase10)+2)^;
    if expBase10 = 0 then break;
    Inc(Result, expBase10);
    val := Power10(val, -expBase10);
  end;
end;

procedure RoundDigits(digBuf: PAnsiChar; position: Integer);
var
  I: Integer;
begin
  if (digBuf[position+1] >= '5') then
  begin
    I := position;
    while True do
    begin
      if digBuf[I] = '9' then
      begin
        digBuf[I] := '0';
        dec(I);
      end
      else
      begin
        Inc(digBuf[I]);
        break;
      end;
    end;
  end;
end;

procedure EmitDigits(val: Extended; digCnt: Integer; digBuf: PAnsiChar; var Exponent: SmallInt);
type
    TBCDBytes = array [0..9] of Byte;

  function GetBcdBytes(val:Extended): TBcdBytes;
{$IF defined(CPUX86)}
  asm
      FLD       Val.Extended
      FBSTP     [Result]
      FWAIT
  end;
{$ELSE}
  var
    I: Int64;
    Ind, D: Integer;
  begin
    FillChar(Result, SizeOf(Result), 0);
    if val < 0 then Result[9] := $80;
    I := Round(Abs(val));
    Ind := 0;
    while (I > 0) and (Ind < 9) do
    begin
      D := I mod 100;
      Result[Ind] := (D mod 10) + ((D div 10) shl 4);
      I := I div 100;
      Inc(Ind);
    end;
  end;
{$IFEND}

var
  bcdBytes: TBcdBytes;
  I: Integer;
begin
  Exponent := ScaleExt(val);
  digBuf[0] := '0';
  val := Round(abs(val) * 1E17);
  if val >= 1E18 then
  begin
    val := val - 1E18;
    digBuf[0] := '1';
    Dec(digCnt);
  end;
  bcdBytes := GetBcdBytes(val);
  for I := 8 downto 0 do
    PWord(@digBuf[17-I*2])^ := $3030 +
      ((bcdBytes[I] and $0F) SHL 8) +
      ((bcdBytes[I] and $F0) SHR 4);
  digBuf[19] := #0;
  if (digCnt <= 18) then
    RoundDigits(digBuf, digCnt);
end;

function _Str2Ext(val: Extended; width, precision: Integer): ShortString;
var
  digBuf : array[0..19] of AnsiChar;
  PBuf, PResult: PAnsiChar;
  exp: SmallInt;
  I: Integer;
  MinWidth: Integer;
begin
  if Width > 255 then Width := 255;

  case PExtendedRec(@val).SpecialType of
    fsNAN:  Exit(ShortString(StringOfChar(' ', Width - 3) + 'Nan'));
    fsInf:  Exit(ShortString(StringOfChar(' ', Width - 4) + '+Inf'));
    fsNInf: Exit(ShortString(StringOfChar(' ', Width - 4) + '-Inf'));
  end;
  // 6.64613997892457936E35 ~= 2**119, which is the cutof for displaying scientific notation
  if (precision < 0) or (val > 6.64613997892457936E35) then
  begin
    if Width < 10 then  // Emit digits generates between 2 and 18 characters
      Width := 10;      // 8 characters for sign, decimal, and exponent parts.
    EmitDigits(val, Width - 8, digBuf, exp);
    SetLength(Result, Width);
    PResult := @Result[1];
    if Width > 18+8 then  // Pad with whitespace if wider than 18+8 characters
    begin
      FillChar(Result[1], Width-18 - 8, ' ');
      PResult := @result[Width - 18 - 7];
    end;
    if val < 0 then // Emit sign
      PResult[0] := '-'
    else
      PResult[0] := ' ';
    PBuf := @digBuf[1];
    if digBuf[0] = '1' then  // Special case for when ScaleExt overflowed.
    begin
      PBuf := @digBuf[0];
      inc(exp);
    end;
    PResult[1] := PBuf^; // Emit decimal
    PResult[2] := '.';
    if Width - 9 < 17 then // Digits from EmitDigits
      Move(PBuf[1], PResult[3], Width - 9)
    else
      Move(PBuf[1], PResult[3], 17);

    Result[width-5] := 'E'; // Exponent part
    if exp < 0 then
    begin
      Result[width-4] := '-';
      exp := -exp;
    end
    else
      Result[Width-4] := '+';
    for I := 3 downto 0 do
    begin
      Result[Width-3+I] := AnsiChar(Ord('0') + (exp mod 10));
      exp := exp div 10;
    end;
  end
  else // fixed notation.
  begin
    EmitDigits(val, 18, digBuf, exp);
    PBuf := @digBuf[0];
    if digBuf[0] = '1' then // It's possbile the log 2 -> log 10 exponent overflows
      inc(exp)
    else
      Inc(PBuf);

    if precision > 256 - 40 then
      precision := 256 - 40;

    if (18 > precision + exp) and (0 <= precision + exp) then // Check if emitted digits are in decimal
    begin                        // and round if they are.
      RoundDigits(PBuf, exp + precision);
      if (PBuf <> @digBuf[0]) and (PBuf[0] = '0') and (PBuf[-1] <> '0') then
      begin //Rounding could overflow
        Dec(PBuf);
        inc(exp);
      end;
    end;

    MinWidth := 0;
    if precision > 0 then
      Inc(MinWidth, 1 + precision);
    if exp <= 0 then
      Inc(MinWidth)
    else
      Inc(MinWidth, exp+1);
    if val < 0 then
      inc(MinWidth);

    if MinWidth > width then
    begin
      SetLength(Result, MinWidth);
      PResult := @Result[1];
    end
    else
    begin
      SetLength(Result, width);
      FillChar(Result[1], width - MinWidth, ' '); // Fill with leading whitespace
      PResult := @Result[Width - MinWidth+1];
    end;

    if val < 0 then // Emit Sign
    begin
      PResult^ := '-';
      Inc(PResult);
    end;

    if exp < 0 then  //Emit Integer part
      PResult^ := '0'
    else
    begin
      if exp < 18 then // Up to 18 digits were emitted
        Move(pbuf^, PResult^, exp+1)
      else
      begin // If larger than 10**18, need to emit extra 0s.
        Move(pbuf^, PResult^, 18);
        FillChar(PResult[18], exp-17, '0');
      end;
      Inc(PResult, exp);
    end;

    if precision > 0 then  // Emit decimal part
    begin
      PResult[1] := '.';
      Inc(PResult, 2);
      FillChar(PResult^, -exp-1, '0'); // emit leading 0s.
      if -exp - 1 >0 then
        inc(PResult, - exp - 1);
      if exp < 0 then
        I := 18
      else if (exp < 18) then
      begin
        Inc(PBuf, exp + 1);
        I := 17 - exp;
      end
      else
        I := 0;
      if I >= precision then
        Move(PBuf^, PResult^, Precision) // all digits from EmitDigits
      else
      begin
        Move(PBuf^, PResult^, I); //remainder of digits from EmitDigits
        FillChar(PResult[I], precision - I + exp + 1, '0'); // trailing 0s.
      end;
    end;
  end;
end;

function _Str1Ext(val: Extended; width: LongInt): ShortString;
begin
  Result := _Str2Ext(val, width, -1);
end;

function _Str0Ext(val: Extended): ShortString;
begin
  Result := _Str2Ext(val, 23, -1);
end;

{$IFNDEF CPUX86}
function _StrC64Digits(Val: UInt64; Width, Precision: Integer; scale: Integer; sign: Boolean): ShortString;

  procedure AppendChar(var P: PAnsiChar; const ch : AnsiChar); inline;
  begin
    P^ := ch;
    Inc(P);
  end;

  procedure AppendString(var P: PAnsiChar; const S: PAnsiChar; const Count: integer); inline;
  begin
    Move(S^, P^, Count);
    Inc(P, Count);
  end;

var
  Ind,
  IntInd, E, w : Integer;
  P: PAnsiChar;
  IntPart: array[0..18] of AnsiChar; // 9223372036854775807 - 19 digits.
  IntLen: Integer;
  ExpPart: array[0..5] of AnsiChar;

  function CheckNR: Boolean;
  var
    Ind2: Integer;
  begin
    Ind2 := Ind + 1;
    while (Ind2 <= High(intPart)) and (IntPart[Ind2] = '0') do Inc(Ind2);
    Result := (Ind2 <= High(intPart)) or Odd(Ord(IntPart[Ind-1]));
  end;

begin
  if sign then Val := not Val + 1;

  if Val = 0 then
  begin
    IntInd := High(IntPart);
    IntPart[IntInd] := '0';
  end
  else
  begin
    IntInd := High(IntPart) + 1;
    while Val <> 0 do
    begin
      Dec(IntInd);
      IntPart[IntInd] := AnsiChar($30 + Val mod 10);
      Val := Val div 10;
    end;
  end;

  if Precision < 0 then
  begin // scientific notation
    IntLen := High(IntPart) - IntInd + 1;
    P := @Result[1];

    if sign then AppendChar(P, '-')
    else AppendChar(P, ' ');

    if Width < 10 then Width := 10;
    Ind := 2 + (Width - 10);
    if 2 + (Width - 10) < IntLen then
    begin
      Ind := IntInd + 2 + (Width - 10);
      if (IntPart[Ind] > '5') or ((IntPart[Ind] = '5') and CheckNR) then
      begin
        Dec(Ind);
        while Ind >= IntInd do
        begin
          if IntPart[Ind] = '9' then
          begin
            IntPart[Ind] := '0';
            Dec(Ind);
          end
          else
          begin
            IntPart[Ind] := Succ(IntPart[Ind]);
            Break;
          end;
        end;
        if Ind < IntInd then
        begin
          IntPart[Ind] := '1';
          Dec(IntInd);
          Inc(IntLen);
        end;
      end;
    end;

    AppendChar(P, IntPart[IntInd]); Inc(IntInd);
    AppendChar(P, '.');
    if IntLen = 1 then AppendChar(P, '0')
    else               AppendChar(P, IntPart[IntInd]);
    Inc(IntInd);

    Ind := 1;
    while (Ind <= Width -10) and (IntInd <= High(IntPart)) do
    begin
      AppendChar(P, IntPart[IntInd]);
      Inc(Ind);
      Inc(IntInd);
    end;
    while (Ind <= Width -10) do
    begin
      AppendChar(P, '0');
      Inc(Ind);
    end;

    E := IntLen - 1 - Scale;
    ExpPart := 'E+0000';
    if E < 0 then
    begin
      ExpPart[1] := '-';
      E := -E;
    end;
    ExpPart[5] := AnsiChar($30 + (E mod 10));
    E := E div 10;
    ExpPart[4] := AnsiChar($30 + (E mod 10));
    AppendString(P, ExpPart, Length(ExpPart));
  end
  else
  begin // fixed notation
    if High(IntPart) - Scale < IntInd then
    begin
      for Ind := IntInd - 1 downto High(IntPart) - Scale do
        IntPart[Ind] := '0';
      IntInd := High(IntPart) - Scale;
    end;

    IntLen := High(IntPart) - IntInd + 1;

    P := @Result[1];

    if IntLen > Scale then
      w := IntLen - Scale // 123
    else
      w := 1; // 0
    if Precision > 0 then
      w := w + Precision + 1; // 0.123
    if sign then Inc(w);

    for Ind := 1 to Width - w do AppendChar(P, ' '); // fill leading space
    if sign then AppendChar(P, '-');

    for Ind := IntInd to High(IntPart) - Scale do AppendChar(P, IntPart[Ind]);
    if Precision > 0 then
    begin
      AppendChar(P, '.');

      if Precision > Scale then
        for Ind := High(IntPart) - Scale + 1 to High(IntPart) do AppendChar(P, IntPart[Ind])
      else
        for Ind := High(IntPart) - Scale + 1 to High(IntPart) - Scale + Precision do AppendChar(P, IntPart[Ind]);
        
      for Ind := 1 to Precision - Scale do AppendChar(P, '0');
    end;
  end;
  SetLength(Result, P - @Result[1]);
end;

function _Str2Comp(Val: Comp; Width, Precision: Integer): ShortString;
begin
  Result := _StrC64Digits(PUInt64(@Val)^, Width, Precision, 0, Val < 0);
end;

function _Str1Comp(Val: Comp; Width: Integer): ShortString;
begin
  Result := _Str2Comp(Val, Width, -1);
end;

function _Str0Comp(Val: Comp): ShortString;
begin
  Result := _Str2Comp(Val, 23, -1);
end;


function _Str2Currency(Val: Currency; Width, Precision: Integer): ShortString;
begin
  if Val = 0 then
    Result := _StrC64Digits(PUInt64(@Val)^, Width, Precision, 0, False)
  else
    Result := _StrC64Digits(PUInt64(@Val)^, Width, Precision, 4, Val < 0);
end;

function _Str1Currency(Val: Currency; Width: Integer): ShortString;
begin
  Result := _Str2Currency(Val, Width, -1);
end;

function _Str0Currency(Val: Currency): ShortString;
begin
  Result := _Str2Currency(Val, 23, -1);
end;

{$ENDIF !CPUX86}


                                                                                             
{$IFNDEF CPUX86}
function _ValExt(s: string; var code: Integer): Extended;
var
  Ch: Char;
  Digits, ExpValue: Integer;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code := 0;
  if S = '' then
  begin
    Inc(Code);
    Exit;
  end;
  Neg := False;
  NegExp := False;
  Valid := False;
  while S[Code + 1] = ' ' do
    Inc(Code);
  Ch := S[Code + 1];
  if (Ch = '+') or (Ch = '-') then
  begin
    Inc(Code);
    Neg := (Ch = '-');
  end;
  while True do
  begin
    Ch := S[Code + 1];
    Inc(Code);
    if not ((Ord(Ch) >= Ord('0')) and (Ord(Ch) <= Ord('9'))) then
      Break;
    Result := (Result * 10) + Ord(Ch) - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Ch = '.' then
  begin
    while True do
    begin
      Ch := S[Code + 1];
      Inc(Code);
      if not ((Ord(Ch) >= Ord('0')) and (Ord(Ch) <= Ord('9'))) then
      begin
        if not Valid then {Starts with '.'}
        begin
          if Ch = #0 then
          begin
            Dec(Code); {S = '.'}
            Valid := True; // SB: Added for compatibility with x86 asm version
          end;
        end;
        Break;
      end;
      Result := (Result * 10) + Ord(Ch) - Ord('0');
      Dec(Digits);
      Valid := True;
    end;
  end;
  ExpValue := 0;
  if (Ord(Ch) or $20) = Ord('e') then
    begin {Ch in ['E','e']}
      Valid := False;
      Ch := S[Code + 1];
      if (Ch = '+') or (Ch = '-') then
      begin
        Inc(Code);
        NegExp := (Ch = '-');
      end;
      while True do
      begin
        Ch := S[Code + 1];
        Inc(Code);
        if not ((Ord(Ch) >= Ord('0')) and (Ord(Ch) <= Ord('9'))) then
          Break;
        ExpValue := (ExpValue * 10) + Ord(Ch) - Ord('0');
        Valid := True;
      end;
     if NegExp then
       ExpValue := -ExpValue;
    end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    Result := Power10(Result, Digits);
  if Neg then
    Result := -Result;
  if Valid and (Ch = #0) then
    Code := 0;
end;
{$ELSE CPUX86}
procedure _ValExt;
const
  Ten: Double = 10.0;
asm
// -> EAX Pointer to string
//  EDX Pointer to code result
// <- FST(0)  Result

      PUSH    EBX
{$IFDEF PIC}
      PUSH    EAX
      CALL    GetGOT
      MOV     EBX,EAX
      POP     EAX
{$ELSE}
      XOR     EBX,EBX
{$ENDIF}
      PUSH    ESI
      PUSH    EDI

      PUSH    EBX     // SaveGOT = ESP+8
      MOV     ESI,EAX
      PUSH    EAX     // save for the error case

      FLDZ
      XOR     EAX,EAX
      XOR     EBX,EBX
      XOR     EDI,EDI

      PUSH    EBX     // temp to get digs to fpu

      TEST    ESI,ESI
      JE      @@empty

@@blankLoop:
      MOV     BX,[ESI]
      ADD     ESI, 2
      CMP     BX,' '
      JE      @@blankLoop

@@endBlanks:
      MOV     CH,0
      CMP     BX,'-'
      JE      @@minus
      CMP     BX,'+'
      JE      @@plus
      JMP     @@firstDigit

@@minus:
      INC     CH
@@plus:
      MOV     BX,[ESI]
      ADD     ESI, 2

@@firstDigit:
      TEST    BX,BX
      JE      @@error

      MOV     EDI,[ESP+8]     // SaveGOT

@@digLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@dotExp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JNE     @@digLoop
      JMP     @@prefinish

@@dotExp:
      CMP     BX,'.' - '0'
      JNE     @@exp
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JE      @@prefinish

//  EDI = SaveGot
@@fracDigLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@exp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      DEC     EAX
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JNE     @@fracDigLoop

@@prefinish:
      XOR     EDI,EDI
      JMP     @@finish

@@exp:
      CMP     BX,'E' - '0'
      JE      @@foundExp
      CMP     BX,'e' - '0'
      JNE     @@error
@@foundExp:
      MOV     BX,[ESI]
      ADD     ESI, 2
      MOV     AH,0
      CMP     BX,'-'
      JE      @@minusExp
      CMP     BX,'+'
      JE      @@plusExp
      JMP     @@firstExpDigit
@@minusExp:
      INC     AH
@@plusExp:
      MOV     BX,[ESI]
      ADD     ESI, 2
@@firstExpDigit:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@error
      MOV     EDI,EBX
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JZ      @@endExp
@@expDigLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@error
      LEA     EDI,[EDI+EDI*4]
      ADD     EDI,EDI
      ADD     EDI,EBX
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JNZ     @@expDigLoop
@@endExp:
      DEC     AH
      JNZ     @@expPositive
      NEG     EDI
@@expPositive:
      MOVSX   EAX,AL

@@finish:
      ADD     EAX,EDI
      PUSH    EDX
      PUSH    ECX
      CALL    _Pow10
      POP     ECX
      POP     EDX

      DEC     CH
      JE      @@negate

@@successExit:
      ADD     ESP,12   // pop temp and saved copy of string pointer

      XOR     ESI,ESI   // signal no error to caller

@@exit:
      SHR     ESI,1
      MOV     [EDX],ESI
      POP     EDI
      POP     ESI
      POP     EBX
      RET

@@negate:
      FCHS
      JMP     @@successExit

@@empty:
      ADD     ESI,2

@@error:
      POP     EAX
      POP     EBX
      SUB     ESI,EBX
      ADD     ESP,4
      JMP     @@exit
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function FPower10(val: Extended; power: Integer): Extended;
begin
  Result := Power10(val, power);
end;
{$ELSE CPUX86}
procedure FPower10;
asm
  JMP  _Pow10
end;
{$ENDIF CPuX86}

const
{$IFDEF CPUX86}
  Pow10Tab0: array[0..31] of Extended = (
    1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,  1e8,  1e9,
    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
    1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29,
    1e30, 1e31);
  Pow10Tab1: array[0..14] of Extended = (
    1e32,  1e64,  1e96,  1e128, 1e160, 1e192, 1e224, 1e256, 1e288, 1e320,
    1e352, 1e384, 1e416, 1e448, 1e480);
  Pow10Tab2: array[0..8] of Extended = (
    1e512, 1e1024, 1e1536, 1e2048, 1e2560, 1e3072, 1e3584, 1e4096, 1e4608);
{$ELSE !CPUX86}
  Pow10Tab0: array[0..31] of Double = (
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
    1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29,
    1e30, 1e31);
  Pow10Tab1: array[0..7] of Double = (
    1e0, 1e32, 1e64, 1e96, 1e128, 1e160, 1e192, 1e224);
{$ENDIF CPUX86}
function Power10(val: Extended; power: Integer): Extended;
{$IFDEF PUREPASCAL}
{$IFDEF CPUX86}
var
  I, P: Integer;
begin
  Result := Val;
  if Power > 0 then
  begin
    if Power >= 5120 then
      Exit(cInfinity);
    Result := Result * Pow10Tab0[Power and $1F];
    P := Power shr 5;
    if P <> 0 then
    begin
      I := P and $F;
      if I <> 0 then
        Result := Result * Pow10Tab1[I - 1];
      I := P shr 4;
      if I <> 0 then
        Result := Result * Pow10Tab2[I - 1];
    end;
  end
  else if Power < 0 then
  begin
    P := -Power;
    if P >= 5120 then
      Exit(0);
    Result := Result / Pow10Tab0[P and $1F];
    P := P shr 5;
    if P <> 0 then
    begin
      I := P and $F;
      if I <> 0 then
        Result := Result / Pow10Tab1[I - 1];
      I := P shr 4;
      if I <> 0 then
        Result := Result / Pow10Tab2[I - 1];
    end;
  end;
end;
{$ELSE !CPUX86}
var
  I, P: Integer;
begin
  Result := Val;
  if Power > 0 then
  begin
    if Power >= 632 then // +308 - (-324) (Exp of  MinDouble)
    begin
      RaiseOverflowException;
      Exit(cInfinity);
    end;
    Result := Result * Pow10Tab0[Power and $1F];
    P := Power shr 5;
    if P <> 0 then
    begin
      I := P and $7;
      if I <> 0 then
        Result := Result * Pow10Tab1[I];
      I := P shr 3;
      if I >= 1 then // 256 - 511
        Result := Result * 1E256;
      if I = 2 then // 512 - 631 (767)
        Result := Result * 1E256;
    end;
  end
  else if Power < 0 then
  begin
    P := -Power;
    if P >= 632 then
    begin
      RaiseUnderflowException;
      Exit(0);
    end;
    Result := Result / Pow10Tab0[P and $1F];
    P := P shr 5;
    if P <> 0 then
    begin
      I := P and $7;
      if I <> 0 then
        Result := Result / Pow10Tab1[I];
      I := P shr 3;
      if I >= 1 then // 256 - 511
        Result := Result * 1E-256;
      if I = 2 then // 512 - 631 (767)
        Result := Result * 1E-256;
    end;
  end;
end;
{$ENDIF CPUX86}
{$ELSE !PUREPASCAL}
asm
    FLD   val.Extended
    MOV   EAX, power
    CALL  _Pow10
    FSTP  Result.Extended
end;
{$ENDIF !PUREPASCAL}


{$IFNDEF CPUX86}
function _Pow10(val: Extended; Power: Integer): Extended;
begin
  Result := Power10(val, Power);
end;
{$ELSE CPUX86}
//function _Pow10(val: Extended; Power: Integer): Extended;
procedure _Pow10;
asm
// -> FST(0)  val
// -> EAX Power
// <- FST(0)  val * 10**Power

//  This routine generates 10**power with no more than two
//  floating point multiplications. Up to 10**31, no multiplications
//  are needed.

      PUSH   EBX
{$IFDEF PIC}
      PUSH   EAX
      CALL   GetGOT
      MOV    EBX, EAX
      POP    EAX
{$ELSE}
      XOR    EBX, EBX
{$ENDIF}
      TEST   EAX, EAX
      JL     @@neg
      JE     @@exit
      CMP    EAX, 5120
      JGE    @@inf
      MOV    EDX, EAX
      AND    EDX, 01FH
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr Pow10Tab0[EBX+EDX*2]

      FMULP

      SHR    EAX,5
      JE     @@exit

      MOV    EDX, EAX
      AND    EDX, 0FH
      JE     @@skip2ndMul
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr Pow10Tab1-10[EBX+EDX*2]
      FMULP

@@skip2ndMul:
      SHR    EAX, 4
      JE     @@exit
      LEA    EAX, [EAX+EAX*4]
      FLD    tbyte ptr Pow10Tab2-10[EBX+EAX*2]
      FMULP
      JMP    @@exit

@@neg:
      NEG    EAX
      CMP    EAX, 5120
      JGE    @@zero
      MOV    EDX, EAX
      AND    EDX, 01FH
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr Pow10Tab0[EBX+EDX*2]
      FDIVP

      SHR    EAX, 5
      JE     @@exit

      MOV    EDX, EAX
      AND    EDX, 0FH
      JE     @@skip2ndDiv
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr Pow10Tab1-10[EBX+EDX*2]
      FDIVP

@@skip2ndDiv:
      SHR    EAX, 4
      JE     @@exit
      LEA    EAX, [EAX+EAX*4]
      FLD    tbyte ptr Pow10Tab2-10[EBX+EAX*2]
      FDIVP

      JMP    @@exit

@@inf:
      FSTP   ST(0)
      FLD    tbyte ptr @@infval[EBX]
      JMP    @@exit

@@zero:
      FSTP   ST(0)
      FLDZ

@@exit:
      POP    EBX
      RET

@@infval:  DW  $0000,$0000,$0000,$8000,$7FFF
end;
{$ENDIF CPUX86}

const
  RealBias = $81;
  DoubleBias = $3FF;
  ExtBias  = $3FFF;

{$IFNDEF CPUX86}
function _Real2Ext(val: Pointer {PReal48}): Extended;
var
  LBytes: PByte;
  LDouble: Double;
begin
  LBytes := val; { [0 .. 5] }

  { Decompose the Real48. Check if the exponent is non-zero. }
  if LBytes[0] <> 0 then
  begin
    PUInt64(@LDouble)^ :=
      ((UInt64(LBytes[5]) and $80) shl 56) or                 { Sign Bit }
      ((UInt64(LBytes[0]) + DoubleBias - RealBias) shl 52) or { Exponent }
      (
        ((UInt64(LBytes[5]) and $7F) shl 32) or
        (UInt64(LBytes[4]) shl 24) or
        (UInt64(LBytes[3]) shl 16) or
        (UInt64(LBytes[2]) shl 8) or
        (UInt64(LBytes[1]) shl 0)
      ) shl 13;                                               { Mantissa }
  end else
    LDouble := 0;

  { On 386, this will convert the Double to Extended, on x64 Extended = Double. }
  Result := LDouble;
end;
{$ELSE CPUX86}
procedure _Real2Ext;//( val : Real ) : Extended;
asm
// -> EAX Pointer to value
// <- FST(0)  Result

//  the REAL data type has the following format:
//  8 bit exponent (bias 129), 39 bit fraction, 1 bit sign

        MOV    DH, [EAX+5]  // isolate the sign bit
        AND    DH, 80H
        MOV    DL, [EAX]  // fetch exponent
        TEST   DL, DL   // exponent zero means number is zero
        JE     @@zero

        ADD    DX, ExtBias - RealBias // adjust exponent bias

        PUSH   EDX   // the exponent is at the highest address

        MOV    EDX, [EAX+2] // load high fraction part, set hidden bit
        OR     EDX, 80000000H
        PUSH   EDX   // push high fraction part

        MOV    DL, [EAX+1]  // load remaining low byte of fraction
        SHL    EDX, 24    // clear low 24 bits
        PUSH   EDX

        FLD    tbyte ptr [ESP] // pop result onto chip
        ADD    ESP, 12

        RET

@@zero:
        FLDZ
        RET
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _Ext2Real(Dest: Pointer {PReal48}; Val: Extended);
type
  PReal48Rec = ^TReal48Rec;
  TReal48Rec = packed record
    Exp: Byte;
    MLo: Byte;
    MHi: UInt32;
  end;
  {$IF SizeOf(Extended) = 10}
  PExt80Rec = ^TExt80Rec;
  TExt80Rec = packed record
    MLo: UInt32;
    MHi: UInt32;
    SignExp: UInt16;
  end;
  {$ELSE}
  PExt64Rec = ^TExt64Rec;
  TExt64Rec = packed record
  case Integer of
  0: (Bytes: array[0..7] of Byte;);
  1: (Words: array[0..3] of UInt16;);
  2: (DWords: array[0..1] of UInt32;);
  3: (QWordVal: UInt64;);
  end;
  {$IFEND}
var
  MHi: UInt32;
  MLo: Byte;
  Exp: Integer;
  {$IF SizeOf(Extended) = 10}
  ExtVal : PExt80Rec;
  {$ELSE}
  ExtVal : PExt64Rec;
  {$IFEND}
begin
  {$IF SizeOf(Extended) = 10}

  ExtVal := PExt80Rec(@Val);
  Exp := Integer(ExtVal^.SignExp and $7fff) - ExtBias + RealBias;
  MHi := ExtVal^.MHi and $7fffffff;
  MLo := Byte(ExtVal^.MLo shr 24);
  if (ExtVal^.MLo and $ffffff) > $7fffff then
  begin
    Inc(MLo);
    if MLo = 0 then
    begin
      Inc(MHi);
      if MHi = $80000000 then
      begin
        MHi := 0;
        Inc(Exp);
      end;
    end;
  end;
  if (ExtVal^.SignExp and $8000) <> 0 then
    MHi := MHi or $80000000;

  {$ELSE}

  ExtVal := PExt64Rec(@Val);
  Exp := Integer((ExtVal^.Words[3] shr 4) and $7ff) - DoubleBias + RealBias;
  MHi := UInt32(ExtVal^.QWordVal shr 21) and $7fffffff;
  MLo := Byte(ExtVal^.DWords[0] shr 13);
  if (ExtVal^.DWords[0] and $1fff) > $fff then
  begin
    Inc(MLo);
    if MLo = 0 then
    begin
      Inc(MHi);
      if MHi = $80000000 then
      begin
        MHi := 0;
        Inc(Exp);
      end;
    end;
  end;
  if (ExtVal^.Bytes[7] and $80) <> 0 then
    MHi := MHi or $80000000;

  {$IFEND}

  if Exp < 0 then
    Exp := 0
  else if Exp > 255 then
    Error(reOverflow);
  PReal48Rec(Dest)^.Exp := Exp;
  PReal48Rec(Dest)^.MLo :=  MLo;
  PReal48Rec(Dest)^.MHi :=  MHi;
end;
{$ELSE CPUX86}
procedure _Ext2Real;//( val : Extended ) : Real;
asm
// -> FST(0)  Value
//  EAX Pointer to result

        PUSH  EBX

        SUB   ESP,12
        FSTP  tbyte ptr [ESP]

        POP   EBX     // EBX is low half of fraction
        POP   EDX     // EDX is high half of fraction
        POP   ECX     // CX is exponent and sign

        SHR   EBX,24  // set carry to last bit shifted out
        ADC   BL,0    // if bit was 1, round up
        ADC   EDX,0
        ADC   CX,0
        JO    @@overflow

        ADD   EDX,EDX // shift fraction 1 bit left
        ADD   CX,CX   // shift sign bit into carry
        RCR   EDX,1   // attach sign bit to fraction
        SHR   CX,1    // restore exponent, deleting sign

        SUB   CX,ExtBias-RealBias // adjust exponent
        JLE   @@underflow
        TEST  CH,CH     // CX must be in 1..255
        JG    @@overflow

        MOV   [EAX],CL
        MOV   [EAX+1],BL
        MOV   [EAX+2],EDX

        POP   EBX
        RET

@@underflow:
        XOR   ECX,ECX
        MOV   [EAX],ECX
        MOV   [EAX+4],CX
        POP   EBX
        RET

@@overflow:
        POP   EBX
        MOV   AL,8
        JMP   Error
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _Ext80ToDouble(Val: Pointer {PExtended80}): Double;
begin
  Result := Double(PExtended80Rec(Val)^);
end;

procedure _DoubleToExt80(Dest: Pointer {PExtended80}; Val: Double);
begin
  PExtended80Rec(Dest)^ := TExtended80Rec(Val);
end;
{$ENDIF !CPUX86}

{$IFNDEF CPUX86}
function _CompDiv(Dividend, Divisor: Comp): Double;
var
  X, Y: Double;
begin
  X := Dividend;
  Y := Divisor;
  Result := X / Y;
end;
{$ENDIF !CPUX86}

const
{$IF defined(CPUX64)}
    ovtInstanceSize = -16;  { Offset of instance size in OBJECTs    }
    ovtVmtPtrOffs   = -8;
{$ELSE !CPUX64}
    ovtInstanceSize = -8;   { Offset of instance size in OBJECTs    }
    ovtVmtPtrOffs   = -4;
{$IFEND !CPUX64}

{$IFNDEF CPUX86}
// Returning Pointer(NativeInt(-1)) indicates failure.
// Otherwise, successful and Return Self.
// AllocFlag indicates an object which was allocated.
function _ObjSetup(Self: Pointer; var VmtPtrAndAllocFlag: Pointer): Pointer;
var
  Size: NativeInt;
  VmtPtrOffs: NativeInt;
  VmtPtr: Pointer;
begin
  Result := Self;
  VmtPtr := VmtPtrAndAllocFlag;
  if VmtPtr = nil then
    Exit; // Successful, not allocated
  if Result = nil then
  begin
    Size := PNativeInt(PByte(VmtPtr) + ovtInstanceSize)^;
    if Size = 0 then
    begin
      VmtPtrAndAllocFlag := nil;
      Exit; // Successful, not allocated
    end;
    Result := _GetMem(Size);
    if Result = nil then
    begin
      Result := Pointer(PByte(-1));
      VmtPtrAndAllocFlag := nil;
      Exit; // Failure, not allocated
    end;
    FillChar(Result^, Size, 0);
    // VmtPtrAndAllocFlag is already non-zero = allocated
  end
  else
    VmtPtrAndAllocFlag := nil; // not allocated
  VmtPtrOffs := PNativeInt(PByte(VmtPtr) + ovtVmtPtrOffs)^;
  if VmtPtrOffs >= 0 then
  begin
    // store vmt in object at this offset
    PPointer(PByte(Result) + VmtPtrOffs)^ := VmtPtr;
  end;
  // Successful, VmtPtrAndAllocFlag is non-zero if allocated
end;
{$ELSE CPUX86}
procedure       _ObjSetup;
asm //StackAlignSafe
{       FUNCTION _ObjSetup( self: ^OBJECT; vmt: ^VMT): ^OBJECT; }
{     ->EAX     Pointer to self (possibly nil)  }
{       EDX     Pointer to vmt  (possibly nil)  }
{     <-EAX     Pointer to self                 }
{       EDX     <> 0: an object was allocated   }
{       Z-Flag  Set: failure, Cleared: Success  }

        CMP     EDX,1           { is vmt = 0, indicating a call         }
        JAE     @@skip1         { from a constructor?                   }
        RET                     { return immediately with Z-flag cleared}

@@skip1:
        PUSH    ECX
        TEST    EAX,EAX         { is self already allocated?            }
        JNE     @@noAlloc
        MOV     EAX,[EDX].ovtInstanceSize
        TEST    EAX,EAX
        JE      @@zeroSize
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _GetMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        TEST    EAX,EAX
        JZ      @@fail

        {       Zero fill the memory }

        PUSH    EDI
        MOV     ECX,[EDX].ovtInstanceSize
        MOV     EDI,EAX
        PUSH    EAX
        XOR     EAX,EAX
        SHR     ECX,2
        REP     STOSD
        MOV     ECX,[EDX].ovtInstanceSize
        AND     ECX,3
        REP     STOSB
        POP     EAX
        POP     EDI

        MOV     ECX,[EDX].ovtVmtPtrOffs
        TEST    ECX,ECX
        JL      @@skip
        MOV     [EAX+ECX],EDX   { store vmt in object at this offset    }
@@skip:
        TEST    EAX,EAX         { clear zero flag                       }
        POP     ECX
        RET

@@fail:
        XOR     EDX,EDX
        POP     ECX
        RET

@@zeroSize:
        XOR     EDX,EDX
        CMP     EAX,1   { clear zero flag - we were successful (kind of)}
        POP     ECX
        RET

@@noAlloc:
        MOV     ECX,[EDX].ovtVmtPtrOffs
        TEST    ECX,ECX
        JL      @@exit
        MOV     [EAX+ECX],EDX   { store vmt in object at this offset    }
@@exit:
        XOR     EDX,EDX         { clear allocated flag                  }
        TEST    EAX,EAX         { clear zero flag                       }
        POP     ECX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _ObjCopy(Dest, Source: Pointer; VmtPtrOffs: LongInt);
var
  VmtPtr: Pointer;
  Size: NativeInt;
begin
  VmtPtr := PPointer(PByte(Dest) + VmtPtrOffs)^;
  Size := PNativeInt(PByte(VmtPtr) + ovtInstanceSize)^;
  Move(Source^, Dest^, Size);
  PPointer(PByte(Dest) + VmtPtrOffs)^ := VmtPtr;
end;
{$ELSE CPUX86}
procedure _ObjCopy;
asm
{       PROCEDURE _ObjCopy( dest, src: ^OBJECT; vmtPtrOff: Longint);    }
{     ->EAX     Pointer to destination          }
{       EDX     Pointer to source               }
{       ECX     Offset of vmt in those objects. }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX
        MOV     EDI,EAX

        LEA     EAX,[EDI+ECX]   { remember pointer to dest vmt pointer  }
        MOV     EDX,[EAX]       { fetch dest vmt pointer        }

        MOV     EBX,[EDX].ovtInstanceSize

        MOV     ECX,EBX { copy size DIV 4 dwords        }
        SHR     ECX,2
        REP     MOVSD

        MOV     ECX,EBX { copy size MOD 4 bytes }
        AND     ECX,3
        REP     MOVSB

        MOV     [EAX],EDX       { restore dest vmt              }

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

// _Fail is a helper function used by Fail standard procedure.
// It can be used in old style object constructor.
// -> Self = Pointer to self. (possibly nil or invlaid pointer if AllocFlag=0)
//    AllocFlag <> 0: Object must be deallocated
// <- Returns nil
function _Fail(Self: Pointer; AllocFlag: NativeInt): Pointer;
begin
  if AllocFlag <> 0 then
    FreeMem(Self);
  Result := nil;
end;

// GetBrifSSEType checks only SSE and SSE2 availability.
// To check full SSE information, use Math unit.
// $00000000: No SSE
// $00000001: SSE supported
// $00000002: SSE2 supported
{$IFDEF CPUX86}
function GetBriefSSEType: Cardinal;
asm
        PUSH    EBX
        PUSHFD
        POP     EAX
        MOV     ECX, EAX
        XOR     EAX, $200000    // flip CPUID bit in EFLAGS
        PUSH    EAX
        POPFD
        PUSHFD
        POP     EAX
        XOR     EAX, ECX    // zero = NO CPUID instruction.
        JZ      @@Exit

        // Use CPUID instruction to get SSE/SSE2 extension flags
        MOV     EAX, 1
        CPUID
        XOR     EAX, EAX
        TEST    EDX, $02000000  // EDX 25 bits - SSE bit
        JZ      @@CheckSSE2
        OR      EAX, 1          // Set SSE flag
@@CheckSSE2:
        TEST    EDX, $04000000  // EDX 26 bits - SSE2 bit
        JZ      @@Exit
        OR      EAX, 2          // Set SSE2 flag
@@Exit:
        POP     EBX
end;
{$ENDIF CPUX64}

procedure _InitializeControlWord;
begin
{$IFDEF CPUX86}
  TestSSE := GetBriefSSEType;
  DefaultMXCSR := GetMXCSR and $FFC0;  // Remove flag bits;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
  TestSSE := $3; // SSE & SSE2 are available on X64
  Default8087CW := Get8087CW and $1F3F; // Remove reserved bits.
{$ENDIF CPUX64}
end;

                                                            
procedure _FpuInit;
{$IF defined(CPUX64)}
asm
        LDMXCSR DefaultMXCSR
end;
{$ELSEIF defined(CPUX86)}
asm
        FNINIT
        FWAIT
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EAX,[EAX].OFFSET Default8087CW
        FLDCW   [EAX]
{$ELSE}
        FLDCW   Default8087CW
{$ENDIF}
end;
{$ELSE}
begin
  Error(rePlatformNotImplemented);
end;
{$IFEND}

procedure _BoundErr;
{$IFDEF PUREPASCAL}
begin
  ErrorAt(Byte(reRangeError), ReturnAddress);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        MOV     AL,reRangeError
        JMP     Error
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _IntOver;
{$IFDEF PUREPASCAL}
begin
  ErrorAt(Byte(reIntOverflow), ReturnAddress);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        MOV     AL,reIntOverflow
        JMP     Error
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFDEF POSIX}
function InternalCompareText(const S1, S2: string): Boolean;
var
  I: Integer;
  US1, US2: UCS4String;
  LCompareLocale: Pointer;
begin
  if Length(S1) <> Length(S2) then
    Exit(False);

  // Convert to UCS4
  US1 := UnicodeStringToUCS4String(S1);
  US2 := UnicodeStringToUCS4String(S2);

  // Convert to upper case for case insensitivity
  LCompareLocale := UTF8CompareLocale;
  for I := 0 to Length(US1) - 1 do
    US1[I] := UCS4Char(towupper_l(wint_t(US1[I]), LCompareLocale));
  for I := 0 to Length(US2) - 1 do
    US2[I] := UCS4Char(towupper_l(wint_t(US2[I]), LCompareLocale));

  // Clear error info and compare strings
  SetLastError(0);
  Result := (wcscoll_l(pwchar_t(@US1[0]), pwchar_t(@US2[0]), LCompareLocale) = 0) and
            (GetLastError = 0);
end;
{$ENDIF}

function TObject.ClassType: TClass;
begin
  Pointer(Result) := PPointer(Self)^;
end;

class function TObject.ClassName: string;
begin
  Result := UTF8ToString(PShortString(PPointer(PByte(Self) + vmtClassName)^)^);
end;

class function TObject.QualifiedClassName: string;
var
  LScope: string;
begin
  LScope := UnitScope;
  if LScope = '' then
    Result := ClassName
  else
    Result := LScope + '.' + ClassName;
end;

class function TObject.ClassNameIs(const Name: string): Boolean;
{$IFDEF MSWINDOWS}
var
  LClassName: string;
begin
  LClassName := ClassName;
  Result := CompareString(UTF8CompareLocale, NORM_IGNORECASE, PChar(LClassName),
    Length(LClassName), PChar(Name), Length(Name)) = CSTR_EQUAL;
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := InternalCompareText(ClassName, Name);
end;
{$ENDIF}

class function TObject.ClassParent: TClass;
{$IFDEF PUREPASCAL}
begin
  Pointer(Result) := PPointer(PByte(Self) + vmtParent)^;
  if Result <> nil then
    Pointer(Result) := PPointer(Result)^;
end;
{$ELSE !PUREPASCAL}
asm
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JE      @@exit
        MOV     EAX,[EAX]
@@exit:
end;
{$ENDIF !PUREPASCAL}

class function TObject.NewInstance: TObject;
begin
  Result := InitInstance(_GetMem(InstanceSize));
end;

procedure TObject.FreeInstance;
begin
  CleanupInstance;
  _FreeMem(Self);
end;

class function TObject.InstanceSize: Longint;
begin
  Result := PInteger(PByte(Self) + vmtInstanceSize)^;
end;

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin
end;

procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;

class function TObject.InitInstance(Instance: Pointer): TObject;
{$IFDEF PUREPASCAL}
var
  IntfTable: PInterfaceTable;
  ClassPtr: TClass;
  I: Integer;
begin
  FillChar(Instance^, InstanceSize, 0);
  PPointer(Instance)^ := Pointer(Self);
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
        with IntfTable.Entries[I] do
        begin
          if VTable <> nil then
            PPointer(@PByte(Instance)[IOffset])^ := VTable;
        end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Result := Instance;
end;
{$ELSE !PUREPASCAL}
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        STOSD
        MOV     ECX,[EBX].vmtInstanceSize
        XOR     EAX,EAX
        PUSH    ECX
        SHR     ECX,2
        DEC     ECX
        REP     STOSD
        POP     ECX
        AND     ECX,3
        REP     STOSB
        MOV     EAX,EDX
        MOV     EDX,ESP
@@0:    MOV     ECX,[EBX].vmtIntfTable
        TEST    ECX,ECX
        JE      @@1
        PUSH    ECX
@@1:    MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JE      @@2
        MOV     EBX,[EBX]
        JMP     @@0
@@2:    CMP     ESP,EDX
        JE      @@5
@@3:    POP     EBX
        MOV     ECX,[EBX].TInterfaceTable.EntryCount
        ADD     EBX,4
@@4:    MOV     ESI,[EBX].TInterfaceEntry.VTable
        TEST    ESI,ESI
        JE      @@4a
        MOV     EDI,[EBX].TInterfaceEntry.IOffset
        MOV     [EAX+EDI],ESI
@@4a:   ADD     EBX,TYPE TInterfaceEntry
        DEC     ECX
        JNE     @@4
        CMP     ESP,EDX
        JNE     @@3
@@5:    POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

procedure TObject.CleanupInstance;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
  InitTable: Pointer;
begin
  ClassPtr := ClassType;
  repeat
    InitTable := PPointer(PByte(ClassPtr) + vmtInitTable)^;
    if InitTable <> nil then
      _FinalizeRecord(Self, InitTable);
    ClassPtr := ClassPtr.ClassParent;
  until ClassPtr = nil;
  TMonitor.Destroy(Self);
end;
{$ELSE !PUREPASCAL}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
@@loop:
        MOV     ESI,[ESI]
        MOV     EDX,[ESI].vmtInitTable
        MOV     ESI,[ESI].vmtParent
        TEST    EDX,EDX
        JE      @@skip
        CALL    _FinalizeRecord
        MOV     EAX,EBX
@@skip:
        TEST    ESI,ESI
        JNE     @@loop

        MOV     EAX,EBX
        CALL    TMonitor.Destroy;
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}

        POP     ESI
        POP     EBX
end;
{$ENDIF !PUREPASCAL}

function InvokeImplGetter(Self: TObject; ImplGetter: NativeUInt): IInterface;
{$IFDEF PUREPASCAL}
var
  M: function: IInterface of object;
begin
  TMethod(M).Data := Self;
  {$IF SizeOf(NativeUInt) = 8}
  if (ImplGetter and $FF00000000000000) = $FF00000000000000 then // Field
    Result := IInterface(PPointer(PByte(Self) + (ImplGetter and $00FFFFFFFFFFFFFF))^)
  else if (ImplGetter and $FF00000000000000) = $FE00000000000000 then // virtual method
  begin
    // sign extend vmt slot offset = smallint cast
    TMethod(M).Code := PPointer(PNativeInt(Self)^ + SmallInt(ImplGetter))^;
    Result := M;
  end
  else // static method
  begin
    TMethod(M).Code := Pointer(ImplGetter);
    Result := M;
  end;
  {$ELSE SizeOf(NativeUInt) <> 8}
  case LongWord(ImplGetter) of
    $FF000000..$FFFFFFFF:  // Field
        Result := IInterface(PPointer(PByte(Self) + (ImplGetter and $00FFFFFF))^);
    $FE000000..$FEFFFFFF:  // virtual method
      begin
        // sign extend vmt slot offset = smallint cast
        TMethod(M).Code := PPointer(PNativeInt(Self)^ + SmallInt(ImplGetter))^;
        Result := M;
      end;
  else // static method
    TMethod(M).Code := Pointer(ImplGetter);
    Result := M;
  end;
  {$IFEND}
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        XCHG    EDX,ECX
        CMP     ECX,$FF000000
        JAE     @@isField
        CMP     ECX,$FE000000
        JB      @@isStaticMethod

        {       the GetProc is a virtual method }
        MOVSX   ECX,CX                  { sign extend slot offs }
        ADD     ECX,[EAX]               { vmt   + slotoffs      }
        JMP     dword ptr [ECX]         { call vmt[slot]        }

@@isStaticMethod:
        JMP     ECX

@@isField:
        AND     ECX,$00FFFFFF
        ADD     ECX,EAX
        MOV     EAX,EDX
        MOV     EDX,[ECX]
        JMP     _IntfCopy
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function TObject.Equals(Obj: TObject): Boolean;
begin
  Result := Obj = Self;
end;

function TObject.GetHashCode: Integer;
begin
{$IFDEF CPUX64}
  Result := Integer(IntPtr(Self)) xor Integer(IntPtr(Self) shr 32);
{$ELSE !CPUX64}
  Result := Integer(IntPtr(Self));
{$ENDIF !CPUX64}
end;

function TObject.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  Pointer(Obj) := nil;
  InterfaceEntry := GetInterfaceEntry(IID);
  if InterfaceEntry <> nil then
  begin
    if InterfaceEntry^.IOffset <> 0 then
    begin
      Pointer(Obj) := Pointer(PByte(Self) + InterfaceEntry^.IOffset);
      if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    end
    else
      IInterface(Obj) := InvokeImplGetter(Self, InterfaceEntry^.ImplGetter);
  end else if ObjCastGUID = IID then
    Pointer(Obj) := Self;
  Result := Pointer(Obj) <> nil;
end;

class function TObject.GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
  IntfTable: PInterfaceTable;
  I: Integer;
begin
  ClassPtr := Self;
  repeat
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
      begin
        Result := @IntfTable.Entries[I];
        if Result^.IID = IID then Exit;
      end;
    ClassPtr := ClassPtr.ClassParent;
  until ClassPtr = nil;
  Result := nil;
end;
{$ELSE}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
@@1:    MOV     EAX,[EBX].vmtIntfTable
        TEST    EAX,EAX
        JE      @@4
        MOV     ECX,[EAX].TInterfaceTable.EntryCount
        ADD     EAX,4
@@2:    MOV     ESI,[EDX].Integer[0]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[0]
        JNE     @@3
        MOV     ESI,[EDX].Integer[4]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[4]
        JNE     @@3
        MOV     ESI,[EDX].Integer[8]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[8]
        JNE     @@3
        MOV     ESI,[EDX].Integer[12]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[12]
        JE      @@5
@@3:    ADD     EAX,type TInterfaceEntry
        DEC     ECX
        JNE     @@2
@@4:    MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JE      @@4a
        MOV     EBX,[EBX]
        JMP     @@1
@@4a:   XOR     EAX,EAX
@@5:    POP     ESI
        POP     EBX
end;
{$ENDIF}

class function TObject.GetInterfaceTable: PInterfaceTable;
begin
  Result := PPointer(PByte(Self) + vmtIntfTable)^;
end;

type
  PClassData = ^TClassData;
  TClassData = record
    ClassType: TClass;
    ParentInfo: Pointer;
    PropCount: SmallInt;
    UnitName: ShortString;
  end;

class function TObject.UnitName: string;
var
  LClassInfo: Pointer;
  S: PShortString;
begin
  LClassInfo := ClassInfo;
  if LClassInfo <> nil then
  begin
    S := @PClassData(PByte(LClassInfo) + 2 + PByte(PByte(LClassInfo) + 1)^).UnitName;
    if S^[1] <> '@' then
      Result := UTF8ToString(S^)
    else
      Result := UTF8ToString(Copy(S^, Pos(ShortString(':'), S^) + 1, MaxInt));
  end else
    Result := '';
end;

class function TObject.UnitScope: string;
var
  LClassInfo: Pointer;
  S: PShortString;
begin
  LClassInfo := ClassInfo;
  if LClassInfo <> nil then
  begin
    S := @PClassData(PByte(LClassInfo) + 2 + PByte(PByte(LClassInfo) + 1)^).UnitName;
    if S^[1] <> '@' then
      Result := UTF8ToString(S^)
    else
      Result := UTF8ToString(Copy(S^, 2, Pos(ShortString(':'), S^) - 2));
  end else
    Result := '';
end;

function _IsClass(Child: TObject; Parent: TClass): Boolean;
begin
  Result := (Child <> nil) and Child.InheritsFrom(Parent);
end;

function _AsClass(Child: TObject; Parent: TClass): TObject;
begin
  Result := Child;
  if (Child <> nil) and not (Child is Parent) then
    ErrorAt(Byte(reInvalidCast), ReturnAddress);
end;

                                                        
function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
{$IF DEFINED(PUREPASCAL) or DEFINED(PIC)}
var
  Temp: Pointer;
begin
  Temp := nil;
  _IntfCast(IInterface(Temp), Intf, ObjCastGUID);
  Result := _AsClass(TObject(Temp), Parent);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        PUSH    0
        MOV     EDX, EAX
        LEA     ECX, ObjCastGUID
        MOV     EAX, ESP
        CALL    _IntfCast
        POP     EAX
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP    _AsClass
end;
{$ENDIF CPUX86}
{$IFEND !PUREPASCAL or !PIC}

function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
begin
  if (Intf <> nil) and (Intf.QueryInterface(ObjCastGUID, Result) = S_OK) and (Result is Parent) then
    Exit;
  Result := nil;
end;

function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;
begin
  Result := _SafeIntfAsClass(Intf, Parent) <> nil;
end;

function _GetHelperDelegate(Instance: TObject; HelperClass: TClass): TObject;
begin
  Result := TClassHelperBaseClass(HelperClass)._Create(Instance);
end;

function _GetHelperIntf(Instance: TObject; HelperClass: TClass): IInterface;
var
  IntfTable: PInterfaceTable;
  P: PInterfaceEntry;
begin
  IntfTable := HelperClass.GetInterfaceTable;
  if IntfTable <> nil then
  begin
    if IntfTable.EntryCount > 0 then
    begin
      P := @IntfTable.Entries[0];
      if Instance.GetInterfaceEntry(P.IID) <> nil then
      begin
        Result := TClassHelperBase(Instance);
        Exit;
      end;
    end;
  end;
  Result := TClassHelperBase(_GetHelperDelegate(Instance, HelperClass));
end;

function GetDynaMethod(vmt: TClass; selector: SmallInt): Pointer;
{$IFDEF PUREPASCAL}
type
  TDynaMethodTable = record
    Count: Word;
    Selectors: array[0..9999999] of SmallInt;
    {Addrs: array[0..0] of Pointer;}
  end;
  PDynaMethodTable = ^TDynaMethodTable;
var
  dynaTab: PDynaMethodTable;
  Parent: Pointer;
  Addrs: PPointer;
  I: Cardinal;
begin
  while True do
  begin
    dynaTab := PPointer(PByte(vmt) + vmtDynamicTable)^;
    if dynaTab <> nil then
    begin
      for I := 0 to dynaTab.Count - 1 do
        if dynaTab.Selectors[I] = selector then
        begin
          Addrs := PPointer(PByte(@dynaTab.Selectors) + dynaTab.Count * SizeOf(dynaTab.Selectors[0]));
          Result := PPointer(PByte(Addrs) + I * SizeOf(Pointer))^;
          Exit;
        end;
    end;
    Parent := PPointer(PByte(vmt) + vmtParent)^;
    if Parent = nil then Break;
    vmt := PPointer(Parent)^;
  end;
  Result := nil;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{       function        GetDynaMethod(vmt: TClass; selector: Smallint) : Pointer;       }
        { ->    EAX     vmt of class            }
        {       EDX     dynamic method index    }
        { <-    EAX     pointer to routine      }
        {       trashes ECX, EDX                }
        PUSH    EDI

        XCHG    EAX, EDX
        JMP     @@haveVMT
@@outerLoop:
        MOV     EDX,[EDX]
@@haveVMT:
        MOV     EDI,[EDX].vmtDynamicTable
        TEST    EDI,EDI
        JE      @@parent
        MOVZX   ECX,word ptr [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@found
        POP     ECX
@@parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@outerLoop
        XOR     EAX, EAX
        JMP     @@exit

@@found:
        POP     EAX
        ADD     EAX,EAX
        SUB     EAX,ECX
        MOV     EAX,[EDI+EAX*2-4]

@@exit:
        POP     EDI
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFDEF CPUX86}
procedure _CallDynaInst;
asm
        { ->    EAX     vmt of class                 }
        {       ESI     dynamic method index         }
        {       trashes: ESI but compiler knows that }

        PUSH    EAX
        PUSH    ECX
        XCHG    ESI, EDX
        MOV     EAX,[EAX]
        CALL    GetDynaMethod { Safe to call unaligned }
        MOV     EDX, EAX
        XCHG    ESI, EDX
        POP     ECX
        POP     EAX
        TEST    ESI, ESI
        JE      @@Abstract
        JMP     ESI

@@Abstract:
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
end;

procedure _CallDynaClass;
asm
        { ->    EAX     vmt of class                 }
        {       ESI     dynamic method index         }
        {       trashes: ESI but compiler knows that }

        PUSH    EAX
        PUSH    ECX
        XCHG    ESI, EDX
        CALL    GetDynaMethod { Safe to call unaligned }
        MOV     EDX, EAX
        XCHG    ESI, EDX
        POP     ECX
        POP     EAX
        TEST    ESI, ESI
        JE      @@Abstract
        JMP     ESI

@@Abstract:
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
end;
{$ENDIF CPUX86}

function _FindDynaInst(Self: TObject; Selector: SmallInt): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := GetDynaMethod(PPointer(Self)^, Selector);
  if Result = nil then
    _AbstractError;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     vmt of class                 }
        {       EDX     dynamic method index         }
        { <-    EAX     pointer to method            }

        MOV     EAX,[EAX]
        CALL    GetDynaMethod { Safe to call unaligned }
        TEST    EAX, EAX
        JNE     @@exit
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function _FindDynaClass(Vmt: TClass; Selector: SmallInt): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := GetDynaMethod(Pointer(Vmt), Selector);
  if Result = nil then
    _AbstractError;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     vmt of class                 }
        {       EDX     dynamic method index         }
        { <-    EAX     pointer to method            }

        CALL    GetDynaMethod { Safe to call unaligned }
        TEST    EAX, EAX
        JNE     @@exit
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

class function TObject.InheritsFrom(AClass: TClass): Boolean;
{$IFDEF PUREPASCAL}
var
  ClassPtr: Pointer;
  P: Pointer;
begin
  Result := False;
  ClassPtr := Pointer(Self);
  while True do
  begin
    if ClassPtr = Pointer(AClass) then
    begin
      Result := True;
      break;
    end;
    P := PPointer(PByte(ClassPtr) + vmtParent)^;
    if P = nil then break;
    ClassPtr := PPointer(P)^;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to our class    }
        {       EDX     Pointer to AClass       }
        { <-    AL      Boolean result          }
        JMP     @@haveVMT
@@loop:
        MOV     EAX,[EAX]
@@haveVMT:
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


class function TObject.ClassInfo: Pointer;
begin
  Result := PPointer(PByte(Self) + vmtTypeInfo)^;
end;

function TObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HResult($8000FFFF); { E_UNEXPECTED }
end;

function TObject.ToString: string;
begin
  Result := ClassName;
end;

procedure TObject.DefaultHandler(var Message);
begin
end;

procedure TObject.AfterConstruction;
begin
end;

procedure TObject.BeforeDestruction;
begin
end;

procedure TObject.Dispatch(var Message);
{$IFDEF PUREPASCAL}
type
  //THandlerProc = procedure(Self: Pointer; var Message) { of object };
  THandlerProc = procedure(var Message) of object;
var
  MsgID: Word;
  Addr: Pointer;
  M: THandlerProc;
begin
  MsgID := TDispatchMessage(Message).MsgID;
  if (MsgID <> 0) and (MsgID < $C000) then
  begin
    Addr := GetDynaMethod(PPointer(Self)^, MsgID);
    if Addr <> nil then
    begin
      //THandlerProc(Addr)(Self, Message)
      TMethod(M).Data := Self;
      TMethod(M).Code := Addr;
      M(Message);
    end
    else
      Self.DefaultHandler(Message);
  end
  else
    Self.DefaultHandler(Message);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        PUSH    EDX
        MOV     DX,[EDX]
        OR      DX,DX
        JE      @@default
        CMP     DX,0C000H
        JAE     @@default
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        PUSH    EAX
{$ENDIF ALIGN_STACK}
        MOV     EAX,[EAX]
        CALL    GetDynaMethod
        MOV     ECX, EAX
{$IFDEF ALIGN_STACK}
        POP     EAX
{$ENDIF ALIGN_STACK}
        POP     EAX
        TEST    ECX, ECX
        JE      @@default
        POP     EDX
        JMP     ECX

@@default:
        POP     EDX
        MOV     ECX,[EAX]
        JMP     DWORD PTR [ECX] + VMTOFFSET TObject.DefaultHandler
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function UTF8Compare(const Str1, Str2: ShortString): Boolean;
{$IFDEF MSWINDOWS}
var
  Len1, Len2: Integer;
  LStr1, LStr2: array[0..255] of WideChar;
begin
  Len1 := MultiByteToWideChar(CP_UTF8, 0, @Str1[1], Length(Str1), LStr1, Length(LStr1));
  Len2 := MultiByteToWideChar(CP_UTF8, 0, @Str2[1], Length(Str2), LStr2, Length(LStr2));
  Result := CompareString(UTF8CompareLocale, NORM_IGNORECASE, LStr1, Len1, LStr2, Len2) = CSTR_EQUAL;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := InternalCompareText(UTF8ToUnicodeString(Str1), UTF8ToUnicodeString(Str2));
end;
{$ENDIF POSIX}

class function TObject.MethodAddress(const Name: ShortString): Pointer;
{$IFDEF PUREPASCAL}
var
  LMethTablePtr: Pointer;
  LMethCount: Word;
  LMethEntry: PMethRec;
  LSelf: TClass;
begin
  Result := nil;

  LSelf := Pointer(Self);
  while True do
  begin
    { Obtain the method table and count }
    LMethTablePtr := PPointer(PByte(LSelf) + vmtMethodTable)^;
    if LMethTablePtr <> nil then
    begin
      LMethCount := PWord(LMethTablePtr)^;
      Inc(PWord(LMethTablePtr), 1);
    end else
      LMethCount := 0;

    { Search for the method if we have more than one. Also tested for a correct table ptr }
    if LMethCount > 0 then
    begin
      LMethEntry := LMethTablePtr;

      while LMethCount > 0 do
      begin
        if (LMethEntry^.nameLen = Byte(Name[0])) and
           (UTF8Compare(PShortString(@LMethEntry^.nameLen)^, Name)) then
        begin
          Result := LMethEntry.methAddr;
          Exit;
        end else
        begin
          Dec(LMethCount);
          LMethEntry := Pointer(PByte(LMethEntry) + LMethEntry.recSize);
        end;
      end;
    end;

    { Go to the parent class }
    LSelf := LSelf.ClassParent;
    if LSelf = nil then
      Exit;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        { ->    EAX     Pointer to class        }
        {       EDX     Pointer to name         }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        { STACK ALIGN = 16 }
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]
        JMP     @@haveVMT
@@outer:                                { upper 16 bits of ECX are 0 !  }
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { EDI := method count           }
        TEST    EDI,EDI
        JZ      @@parent
        ADD     ESI,2
@@inner:                                { upper 16 bits of ECX are 0 !  }
        MOV     CL,[ESI+6]              { compare length of strings     }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:                                 { upper 16 bits of ECX are 0 !  }
        MOV     CX,[ESI]                { fetch length of method desc   }
        ADD     ESI,ECX                 { point ESI to next method      }
        DEC     EDI
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent     { fetch parent vmt              }
        TEST    EAX,EAX
        JNE     @@outer
        JMP     @@exit                  { return NIL                    }

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        JMP     @@cont

@@utf8Cmp:
        { STACK ALIGN = 16 }
        PUSH    EAX
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF}
        LEA     EAX,[ESI+6]
        CALL    UTF8Compare
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF}
        XOR     ECX,ECX
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JZ      @@notEqual
        JMP     @@foundIt

@@cmpChar:                              { upper 16 bits of ECX are 0 !  }
        MOV     CH,0                    { upper 24 bits of ECX are 0 !  }
@@cmpCharLoop:
        MOV     BL,[ESI+ECX+6]          { case insensitive string cmp   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        XOR     BL,[EDX+ECX+0]          { last char is compared first   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpCharLoop

@@foundIt:
        { found it }
        MOV     EAX,[ESI+2]

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function UTF8ShortStringToString(const Str: ShortString): string;
begin
  Result := UTF8ToString(Str);
end;

class function TObject.MethodAddress(const Name: string): Pointer;
begin
  Result := MethodAddress(UTF8EncodeToShortString(Name));
end;

class function TObject.MethodName(Address: Pointer): string;
{$IFDEF PUREPASCAL}
var
  LMethTablePtr: Pointer;
  LMethCount: Word;
  LMethEntry: PMethRec;
  LSelf: TClass;
begin
  Result := '';

  LSelf := Pointer(Self);
  while True do
  begin
    { Obtain the method table and count }
    LMethTablePtr := PPointer(PByte(LSelf) + vmtMethodTable)^;
    if LMethTablePtr <> nil then
    begin
      LMethCount := PWord(LMethTablePtr)^;
      Inc(PWord(LMethTablePtr), 1);
    end else
      LMethCount := 0;

    { Search for the method if we have more than one. Also tested for a correct table ptr }
    if LMethCount > 0 then
    begin
      LMethEntry := LMethTablePtr;

      while LMethCount > 0 do
      begin
        if LMethEntry^.methAddr = Address then
        begin
          Result := UTF8ShortStringToString(PShortString(@LMethEntry.nameLen)^);
          Exit;
        end else
        begin
          Dec(LMethCount);
          LMethEntry := Pointer(PByte(LMethEntry) + LMethEntry.recSize);
        end;
      end;
    end;

    { Go to the parent class }
    LSelf := LSelf.ClassParent;
    if LSelf = nil then
      Exit;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm     //StackAlignSafe
        { ->    EAX     Pointer to class        }
        {       EDX     Address                 }
        {       ECX     Pointer to result       }
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     EDI,ECX
        XOR     EBX,EBX
        XOR     ECX,ECX
        JMP     @@haveVMT
@@outer:
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable { fetch pointer to method table }
        TEST    ESI,ESI
        JE      @@parent
        MOV     CX,[ESI]
        TEST    ECX,ECX
        JZ      @@parent
        ADD     ESI,2
@@inner:
        CMP     EDX,[ESI+2]
        JE      @@found
        MOV     BX,[ESI]
        ADD     ESI,EBX
        DEC     ECX
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@outer
        LEA     ESI,@@emptyStr
{$IFDEF PIC}
        CALL    GetGOT
        ADD     ESI, EAX
{$ENDIF PIC}
        JMP     @@exit

@@emptyStr:
        DB      0

@@found:
        ADD     ESI,6
@@exit:
        MOV     EAX,ESI
        MOV     EDX,EDI
        POP     EBX
        POP     EDI
        POP     ESI
        JMP     UTF8ShortStringToString
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function TObject.FieldAddress(const Name: ShortString): Pointer;
{$IFDEF PUREPASCAL}
var
  LFieldTablePtr: Pointer;
  LFldCount: Word;
  LName: PShortString;
  LClass: TClass;
begin
  Result := nil;

  LClass := PPointer(Self)^;
  while True do
  begin
    { Obtain the field table and count }
    LFieldTablePtr := PPointer(PByte(LClass) + vmtFieldTable)^;
    if LFieldTablePtr <> nil then
    begin
      LFldCount := PWord(LFieldTablePtr)^;
      Inc(PWord(LFieldTablePtr), 1);  { Count: Word }
      Inc(PPointer(LFieldTablePtr), 1); { ClassTab: Pointer }
    end else
      LFldCount := 0;

    { Search for the field if we have more than one. Also tested for a correct table ptr }
    if LFldCount > 0 then
    begin
      while LFldCount > 0 do
      begin
        LName := PShortString(PByte(LFieldTablePtr) + SizeOf(Word) + SizeOf(Longword));

        if (LName^[0] = Name[0]) and
           (UTF8Compare(LName^, Name)) then
        begin
          Result := Pointer(PByte(Self) + PLongword(LFieldTablePtr)^);
          Exit;
        end else
        begin
          Dec(LFldCount);
          { Skip 1 word, 1 Pointer, the length of the name (1 Byte) and the characters of the name }
          Inc(PByte(LFieldTablePtr), SizeOf(Word) + SizeOf(Longword) + Byte(LName^[0]) + 1);
        end;
      end;
    end;

    { Go to the parent class }
    LClass := LClass.ClassParent;
    if LClass = nil then
      Exit;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to instance     }
        {       EDX     Pointer to name         }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]
        PUSH    EAX                     { save instance pointer         }

@@outer:
        MOV     EAX,[EAX]               { fetch class pointer           }
        MOV     ESI,[EAX].vmtFieldTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { fetch count of fields         }
        TEST    EDI,EDI
        JZ      @@parent                { fieldExTab ref only           }
        ADD     ESI,6                   { count:U2 + classTab:P         }
@@inner:
        MOV     CL,[ESI+6]              { compare string lengths        }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:
        LEA     ESI,[ESI+ECX+7]         { point ESI to next field       }
        DEC     EDI
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent     { fetch parent VMT              }
        TEST    EAX,EAX
        JNE     @@outer
        POP     EDX                     { forget instance, return Nil   }
        JMP     @@exit

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        MOV     CL,[ESI+6]              { ECX := length of field name   }
        JMP     @@cont

@@utf8Cmp:
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,[ESI+6]
        CALL    UTF8Compare
        XOR     ECX,ECX
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JZ      @@notEqual
        JMP     @@foundIt

@@cmpChar:
        MOV     BL,[ESI+ECX+6]         { case insensitive string cmp    }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        XOR     BL,[EDX+ECX+0]         { starting with last char        }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpChar

@@foundIt:
        { found it }
        MOV     EAX,[ESI]               { result is field offset plus ...   }
        POP     EDX
        ADD     EAX,EDX                 { instance pointer              }

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX

end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function TObject.FieldAddress(const Name: string): Pointer;
begin
  Result := FieldAddress(UTF8EncodeToShortString(Name));
end;

function _ClassCreate(InstanceOrVMT: Pointer; Alloc: ShortInt): TObject;
{$IFNDEF CPUX86}
begin
  if Alloc >= 0 then
    InstanceOrVMT := Pointer(TClass(InstanceOrVMT).NewInstance);
  Result := TObject(InstanceOrVMT);
end;
{$ELSE CPUX86}
asm
        { ->    EAX = pointer to VMT      }
        { <-    EAX = pointer to instance }
        PUSH    EDX
        PUSH    ECX
        PUSH    EBX
        TEST    DL,DL
        JL      @@noAlloc
        CALL    DWORD PTR [EAX] + VMTOFFSET TObject.NewInstance
@@noAlloc:
{$IFDEF STACK_BASED_EXCEPTIONS}
        XOR     EDX,EDX
        LEA     ECX,[ESP+16]
        MOV     EBX,FS:[EDX]
        MOV     [ECX].TExcFrame.next,EBX
        MOV     [ECX].TExcFrame.hEBP,EBP
        MOV     [ECX].TExcFrame.desc,offset @desc
        MOV     [ECX].TexcFrame.ConstructedObject,EAX   { trick: remember copy to instance }
        MOV     FS:[EDX],ECX
{$ENDIF STACK_BASED_EXCEPTIONS}
        POP     EBX
        POP     ECX
        POP     EDX
        RET

{$IFDEF STACK_BASED_EXCEPTIONS}
@desc:
        JMP     _HandleAnyException

  {       destroy the object                                                      }

        MOV     EAX,[ESP+8+9*4]
        MOV     EAX,[EAX].TExcFrame.ConstructedObject
        TEST    EAX,EAX
        JE      @@skip
        MOV     ECX,[EAX]
        MOV     DL,$81
        PUSH    EAX
        CALL    DWORD PTR [ECX] + VMTOFFSET TObject.Destroy
        POP     EAX
        CALL    _ClassDestroy
@@skip:
  {       reraise the exception   }
        CALL    _RaiseAgain
{$ENDIF STACK_BASED_EXCEPTIONS}
end;
{$ENDIF CPUX86}

procedure _ClassDestroy(Instance: TObject);
begin
  Instance.FreeInstance;
end;


function _AfterConstruction(Instance: TObject): TObject;
begin
  try
    Instance.AfterConstruction;
    Result := Instance;
  except
    _BeforeDestruction(Instance, 1);
    raise;
  end;
end;

{$IFNDEF CPUX86}
procedure _BeforeDestruction(Instance: TObject; OuterMost: ShortInt);
begin
  if OuterMost > 0 then
    Instance.BeforeDestruction;
end;
{$ELSE CPUX86}
function _BeforeDestruction(Instance: TObject; OuterMost: ShortInt): TObject;
// Must preserve DL on return!
asm //StackAlignSafe
       { ->  EAX  = pointer to instance }
       {      DL  = dealloc flag        }
       { <-  EAX  = pointer to instance }  //  Result := Instance;
        TEST    DL,DL
        JG      @@outerMost                //  if OuterMost > 0 then Exit;
        RET
@@outerMost:
{$IFDEF ALIGN_STACK}
        PUSH    ECX     // 4 byte adjustment, and ECX is convenient
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,[EAX]                  //  Instance.BeforeDestruction;
        CALL    DWORD PTR [EDX] + VMTOFFSET TObject.BeforeDestruction
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        POP     ECX     // 4 byte adjustment, and ECX is convenient
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}

{ TMonitor }

{$IFDEF POSIX}
procedure Sleep(Timeout: Integer); inline;
begin
  usleep(Timeout * 1000);
end;

function GetTickCount: Cardinal; inline;
{$IFDEF LINUX}
var
  t: tms;
begin
  Result := Cardinal(Int64(Cardinal(times(t)) * 1000) div sysconf(_SC_CLK_TCK));
end;
{$ENDIF}
{$IFDEF MACOS}
begin
  Result := AbsoluteToNanoseconds(UpTime) div 1000000;
end;
{$ENDIF MACOS}

{$ENDIF POSIX}

{ TMonitor.TSpinWait }

procedure TMonitor.TSpinWait.Reset;
begin
  FCount := 0;
end;

procedure TMonitor.TSpinWait.SpinCycle;
var
  SpinCount: Integer;
begin
  if (FCount > YieldThreshold) or (CPUCount <= 1) then
  begin
    if FCount >= YieldThreshold then
      SpinCount := FCount - 10
    else
      SpinCount := FCount;
    if SpinCount mod Sleep1Threshold = Sleep1Threshold - 1 then
      Sleep(1)
    else if SpinCount mod Sleep0Threshold = Sleep0Threshold - 1 then
      Sleep(0)
    else
{$IFDEF MSWINDOWS}
      Yield;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
      sched_yield;
{$ENDIF POSIX}
  end else
    Spin(4 shl FCount);
  Inc(FCount);
  if FCount < 0 then
    FCount := 10;
end;

{ TMonitor.TSpinLock }

procedure TMonitor.TSpinLock.Enter;
var
  LLock: Integer;
  Wait: TSpinWait;
begin
  Wait.Reset;
  while True do
  begin
    LLock := FLock;
    if LLock = 0 then
    begin
      if InterlockedCompareExchange(FLock, 1, LLock) = LLock then
        System.Exit;
    end;
    Wait.SpinCycle;
  end;
end;

procedure TMonitor.TSpinLock.Exit;
begin
  InterlockedExchange(FLock, 0);
end;

class procedure TMonitor.Spin(Iterations: Integer);
{$IF defined(CPUX86) or defined(CPUX64)}
asm
    CMP  Iterations, 0
    JNG  @Done
@Loop:
    PAUSE
    DEC  Iterations
    CMP  Iterations, 0
    JG   @LOOP
@Done:
end;
{$ELSE}
begin
  while Iterations > 0 do
  begin
    YieldProcessor;
    Dec(Iterations);
  end;
end;
{$IFEND}

class function TMonitor.GetCacheLineSize: Integer;
{$IFDEF MSWINDOWS}
{$POINTERMATH ON}
var
  ProcInfo, CurInfo: PSystemLogicalProcessorInformation;
  Len: DWORD;
begin
  Len := 0;
  if (GetProcAddress(GetModuleHandle(kernel), 'GetLogicalProcessorInformation') <> nil) and
    not GetLogicalProcessorInformation(nil, Len) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    GetMem(ProcInfo, Len);
    try
      GetLogicalProcessorInformation(ProcInfo, Len);
      CurInfo := ProcInfo;
      while Len > 0 do
      begin
        if (CurInfo.Relationship = RelationCache) and (CurInfo.Cache.Level = 1) then
          System.Exit(CurInfo.Cache.LineSize);
        Inc(CurInfo);
        Dec(Len, SizeOf(CurInfo^));
      end;
    finally
      FreeMem(ProcInfo);
    end;
  end;
  Result := 64; // Use a reasonable default cache line size.
end;
{$POINTERMATH OFF}
{$ENDIF}
{$IFDEF POSIX}
var
  LineSize: UInt64;
  Size: Integer;
begin
  Size := SizeOf(LineSize);
  if sysctlbyname('hw.cachelinesize', @LineSize, @Size, nil, 0) = 0 then
    Result := LineSize
  else
    Result := 64;
end;
{$ENDIF}

class procedure TMonitor.CheckMonitorSupport;
begin
  if MonitorSupport = nil then
    Error(reNoMonitorSupport);
end;

function TMonitor.CheckOwningThread: TThreadID;
begin
  Result := FOwningThread;
  if Result <> GetCurrentThreadId then
    Error(reMonitorNotLocked)
end;

class function TMonitor.Create: PMonitor;
begin
  if CacheLineSize = 0 then
    InterlockedExchange(CacheLineSize, GetCacheLineSize);
  if CacheLineSize > SizeOf(Result^) then
    Result := AllocMem(CacheLineSize)
  else
    Result := AllocMem(SizeOf(Result^));
end;

class procedure TMonitor.Destroy(AObject: TObject);
var
  MonitorFld: PPMonitor;
  Monitor: PMonitor;
begin
  MonitorFld := GetFieldAddress(AObject);
  if MonitorFld^ <> nil then
  begin
    Monitor := MonitorFld^;
    MonitorFld^ := nil;
    Monitor.Destroy;
  end;
end;

procedure TMonitor.Destroy;
begin
  if (MonitorSupport <> nil) and (FLockEvent <> nil) then
    MonitorSupport.FreeSyncObject(FLockEvent);
  FreeMem(@Self);
end;

class procedure TMonitor.Enter(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Enter(INFINITE);
end;

class function TMonitor.Enter(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).Enter(Timeout);
end;

function TMonitor.DequeueWaiter: PWaitingThread;
begin
  FQueueLock.Enter;
  try
    Result := FWaitQueue;
    if (Result = nil) or (Result.Next = Result) then
    begin
      FWaitQueue := nil;
      System.Exit;
    end else
    begin
      Result := FWaitQueue.Next;
      FWaitQueue.Next := FWaitQueue.Next.Next;
    end;
  finally
    FQueueLock.Exit;
  end;
end;

function TMonitor.Enter(Timeout: Cardinal): Boolean;
label
  TryAgain;
var
  Done: Boolean;
  LockCount: Integer;
  StartCount, EndCount: Cardinal;
  SpinCount: Integer;
begin
  SpinCount := FSpinCount;
// Return here if signaled and lock wasn't acquired
TryAgain:
  Result := TryEnter;
  if not Result and (Timeout <> 0) then
  begin
    Done := False;
    // Get the spin count
    if SpinCount > 0 then
    begin
      StartCount := GetTickCount;
      while SpinCount > 0 do
      begin
        if (Timeout <> INFINITE) and ((GetTickCount - StartCount) >= Timeout) then
        begin
          Result := False;
          System.Exit;
        end;
        // if there are already waiters, don't bother spinning
        if FLockCount > 1 then
          Break;
        // Try to get the lock
        if FLockCount = 0 then
          if InterlockedCompareExchange(FLockCount, 1, 0) = 0 then
          begin
            FOwningThread := GetCurrentThreadId;
            FRecursionCount := 1;
            Result := True;
            System.Exit;
          end;
        YieldProcessor;
        Dec(SpinCount);
        // Keep trying until the spin count expires
      end;
      // Adjust the timeout in case the spin-lock expired above.
      if Timeout <> INFINITE then
      begin
        EndCount := GetTickCount;
        if EndCount - StartCount >= Timeout then
        begin
          Result := False;
          System.Exit;
        end;
        Dec(Timeout, EndCount - StartCount);
      end;
    end;
    // Before we can block, we add our count to the lock
    while True do
    begin
      LockCount := FLockCount;
      if LockCount = 0 then
        goto TryAgain;
      if InterlockedCompareExchange(FLockCount, LockCount + 2, LockCount) = LockCount then
        Break;
    end;
    while True do
    begin
      StartCount := GetTickCount;
      // We're not the owner, so blocking is needed
      // GetEvent does a "safe" allocation of the Event
      Result := MonitorSupport.WaitOrSignalObject(nil, GetEvent, Timeout) = WAIT_OBJECT_0;
      if Timeout <> INFINITE then
      begin
        EndCount := GetTickCount;
        if EndCount - StartCount < Timeout then
          Dec(Timeout, EndCount - StartCount)
        else
          Timeout := 0;
      end;
      if Result then
      begin
        // Event was signaled, so try to acquire the lock since this could be a spurious condition
        while True do
        begin
          LockCount := FLockCount;
          if LockCount and 1 <> 0 then
            Break;
          if InterlockedCompareExchange(FLockCount, (LockCount - 2) or 1, LockCount) = LockCount then
          begin
            Done := True;
            Break;
          end;
        end;
      end else
      begin
        // We timed out, remove our presence from the lock count
        repeat
          LockCount := FLockCount;
        until InterlockedCompareExchange(FLockCount, LockCount - 2, LockCount) = LockCount;
        Done := True;
      end;
      if Done then
        Break;
    end;
    if Result then
    begin
      FOwningThread := GetCurrentThreadId;
      FRecursionCount := 1;
    end;
  end;
end;

procedure TMonitor.Exit;
var
  LockCount: Integer;
begin
  CheckOwningThread;
  Dec(FRecursionCount);
  if FRecursionCount = 0 then
  begin
    FOwningThread := 0;
    while True do
    begin
      LockCount := FLockCount;
      if InterlockedCompareExchange(FLockCount, LockCount - 1, LockCount) = LockCount then
      begin
        // if LockCount is <> 0 after we dropped our lock, there were waiters, so signal them
        if LockCount and not 1 <> 0 then
          MonitorSupport.WaitOrSignalObject(GetEvent, nil, 0);
        Break;
      end;
    end;
  end;
end;

class procedure TMonitor.Exit(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Exit;
end;

function TMonitor.GetEvent: Pointer;
var
  SleepTime: Integer;
  Event: Pointer;
begin
  SleepTime := 1;
  Result := FLockEvent;
  if Result = nil then
    while True do
    begin
      Event := MonitorSupport.NewSyncObject;
      Result := InterlockedCompareExchangePointer(FLockEvent, Event, nil);
      if Result = nil then
        // We won!  Nobody else was trying to allocate the Event.
        Result := Event
      else if Event <> nil then
        // Oh Well. We tried. Close the handle if someone got to it first.
        MonitorSupport.FreeSyncObject(Event);
      // Check if we actually were able to allocate the event without fail
      if Result <> nil then
        System.Exit;
      // We failed to allocate the event, so wait a bit to see if one becomes available
      Sleep(SleepTime);
      // Don't let it run-away, so return to a reasonable value and keep trying
      if SleepTime > 512 then
        SleepTime := 1
      else
        // Next time wait a little longer
        SleepTime := SleepTime shl 1;
    end;
end;

class function TMonitor.GetFieldAddress(AObject: TObject): PPMonitor;
begin
  Result := PPMonitor(PByte(AObject) + AObject.InstanceSize - hfFieldSize + hfMonitorOffset);
end;

class function TMonitor.GetMonitor(AObject: TObject): PMonitor;
var
  MonitorFld: PPMonitor;
  Monitor: PMonitor;
begin
  MonitorFld := GetFieldAddress(AObject);
  Result := MonitorFld^;
  if Result = nil then
  begin
    Monitor := TMonitor.Create;
    Result := InterlockedCompareExchangePointer(Pointer(MonitorFld^), Monitor, nil);
    if Result = nil then
      Result := Monitor
    else
      FreeMem(Monitor);
  end;
end;

procedure TMonitor.Pulse;
var
  WaitingThread: PWaitingThread;
begin
  WaitingThread := DequeueWaiter;
  if WaitingThread <> nil then
    MonitorSupport.WaitOrSignalObject(WaitingThread.WaitEvent, nil, 0);
end;

class procedure TMonitor.Pulse(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Pulse;
end;

procedure TMonitor.PulseAll;
var
  WaitingThread: PWaitingThread;
begin
  WaitingThread := DequeueWaiter;
  while WaitingThread <> nil do
  begin
    MonitorSupport.WaitOrSignalObject(WaitingThread.WaitEvent, nil, 0);
    WaitingThread := DequeueWaiter;
  end;
end;

class procedure TMonitor.PulseAll(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).PulseAll;
end;

procedure TMonitor.QueueWaiter(var WaitingThread: TWaitingThread);
begin
  FQueueLock.Enter;
  try
    if FWaitQueue = nil then
    begin
      FWaitQueue := @WaitingThread;
      WaitingThread.Next := @WaitingThread;
    end else
    begin
      WaitingThread.Next := FWaitQueue.Next;
      FWaitQueue.Next := @WaitingThread;
      FWaitQueue := @WaitingThread;
    end;
  finally
    FQueueLock.Exit;
  end;
end;

procedure TMonitor.RemoveWaiter(var WaitingThread: TWaitingThread);
var
  Last, Walker: PWaitingThread;
begin
  // Perform a check, lock, check
  if FWaitQueue <> nil then
  begin
    FQueueLock.Enter;
    try
      if FWaitQueue <> nil then
      begin
        Last := FWaitQueue;
        Walker := Last.Next;
        while Walker <> FWaitQueue do
        begin
          if Walker = @WaitingThread then
          begin
            Last.Next := Walker.Next;
            Break;
          end;
          Last := Walker;
          Walker := Walker.Next;
        end;
        if (Walker = FWaitQueue) and (Walker = @WaitingThread) then
          if Walker.Next = Walker then
            FWaitQueue := nil
          else
          begin
            FWaitQueue := Walker.Next;
            Last.Next := FWaitQueue;
          end;
      end;
    finally
      FQueueLock.Exit;
    end;
  end;
end;

class procedure TMonitor.SetSpinCount(AObject: TObject; ASpinCount: Integer);
var
  Monitor: PMonitor;
begin
  if CPUCount > 1 then
  begin
    Monitor := GetMonitor(AObject);
    InterlockedExchange(Monitor.FSpinCount, ASpinCount);
  end;
end;

class function TMonitor.TryEnter(AObject: TObject): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).TryEnter;
end;

function TMonitor.TryEnter: Boolean;
begin
  if FOwningThread = GetCurrentThreadId then  // check for recursion
  begin
    // Only the owning thread can increment this value so no need to guard it
    Inc(FRecursionCount);
    Result := True;
  // check to see if we can gain ownership
  end else if (FLockCount = 0) and (InterlockedCompareExchange(FLockCount, 1, 0) = 0) then
  begin
    //  Yep, got it.  Now claim ownership
    FOwningThread := GetCurrentThreadId;
    FRecursionCount := 1;
    Result := True;
  end else
    Result := False;
end;

function TMonitor.Wait(ALock: PMonitor; Timeout: Cardinal): Boolean;
var
  RecursionCount: Integer;
  WaitingThread: TWaitingThread;
begin
  WaitingThread.Next := nil;
  WaitingThread.Thread := ALock.CheckOwningThread;
  // This event should probably be cached someplace.
  // Probably not on the instance since this is a per-thread-per-instance resource
  WaitingThread.WaitEvent := MonitorSupport.NewWaitObject;
  try
    // Save the current recursion count for later
    RecursionCount := ALock.FRecursionCount;
    // Add the current thread to the waiting queue
    QueueWaiter(WaitingThread);
    // Set it back to almost released so the next Exit call actually drops the lock
    ALock.FRecursionCount := 1;
    // Now complete the exit and signal any waiters
    ALock.Exit;
    // Get in line for someone to do a Pulse or PulseAll
    Result := MonitorSupport.WaitOrSignalObject(nil, WaitingThread.WaitEvent, Timeout) = WAIT_OBJECT_0;
    // Got to get the lock back and block waiting for it.
    ALock.Enter(INFINITE);
    // Remove any dangling waiters from the list
    RemoveWaiter(WaitingThread);
    // Lets restore the recursion to return to the proper nesting level
    ALock.FRecursionCount := RecursionCount;
  finally
    MonitorSupport.FreeWaitObject(WaitingThread.WaitEvent);
  end;
end;

class function TMonitor.Wait(AObject: TObject; Timeout: Cardinal): Boolean;
var
  Monitor: PMonitor;
begin
  CheckMonitorSupport;
  Monitor := GetMonitor(AObject);
  Result := Monitor.Wait(Monitor, Timeout);
end;

class function TMonitor.Wait(AObject, ALock: TObject; Timeout: Cardinal): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).Wait(GetMonitor(ALock), Timeout);
end;

function MonitorEnter(AObject: TObject; Timeout: Cardinal = INFINITE): Boolean;
begin
  Result := TMonitor.Enter(AObject, Timeout);
end;

function MonitorTryEnter(AObject: TObject): Boolean;
begin
  Result := TMonitor.TryEnter(AObject);
end;

procedure MonitorExit(AObject: TObject);
begin
  TMonitor.Exit(AObject);
end;

function MonitorWait(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  Result := TMonitor.Wait(AObject, AObject, Timeout);
end;

function MonitorWait(AObject: TObject; ALock: TObject; Timeout: Cardinal): Boolean;
begin
  Result := TMonitor.Wait(AObject, ALock, Timeout);
end;

procedure MonitorPulse(AObject: TObject);
begin
  TMonitor.Pulse(AObject);
end;

procedure MonitorPulseAll(AObject: TObject);
begin
  TMonitor.PulseAll(AObject);
end;

procedure MemoryBarrier;
{$IF defined(CPUX64)}
asm
      MFENCE
end;
{$ELSEIF defined(CPUX86)}
asm
      PUSH EAX
      XCHG [ESP],EAX
      POP  EAX
end;
{$ELSE}
begin
  Error(rePlatformNotImplemented);
end;
{$IFEND}

procedure YieldProcessor;
{$IF defined(CPUX86) or defined(CPUX64)}
asm
  PAUSE
end;
{$ELSE}
begin
  Error(rePlatformNotImplemented);
end;
{$IFEND}

{
  The following NotifyXXXX routines are used to "raise" special exceptions
  as a signaling mechanism to an interested debugger.  If the debugger sets
  the DebugHook flag to 1 or 2, then all exception processing is tracked by
  raising these special exceptions.  The debugger *MUST* respond to the
  debug event with DBG_CONTINUE so that normal processing will occur.
}
{$IF defined(LINUX) or defined(MACOS)}
const
  excRaise      = 0; { an exception is being raised by the user (could be a reraise) }
  excCatch      = 1; { an exception is about to be caught }
  excFinally    = 2; { a finally block is about to be executed because of an exception }
  excUnhandled  = 3; { no user exception handler was found (the app will die) }

procedure _DbgExcNotify(
  NotificationKind: Integer;
  ExceptionObject: Pointer;
  ExceptionName: PShortString;
  ExceptionLocation: Pointer;
  HandlerAddr: Pointer); cdecl; export;
begin
{$IFDEF DEBUG}
  {
    This code is just for debugging the exception handling system.  The debugger
    needs _DbgExcNotify, however to place breakpoints in, so the function itself
    cannot be removed.
  }
  asm
{$IFDEF ALIGN_STACK}
    SUB  ESP, 8
{$ENDIF ALIGN_STACK}
    PUSH EAX
    PUSH EDX
  end;
  if Assigned(ExcNotificationProc) then
    ExcNotificationProc(NotificationKind, ExceptionObject, ExceptionName, ExceptionLocation, HandlerAddr);
  asm
    POP EDX
    POP EAX
{$IFDEF ALIGN_STACK}
    ADD  ESP, 8
{$ENDIF ALIGN_STACK}
  end;
{$ENDIF DEBUG}
end;

{
  The following functions are used by the debugger for the evaluator.  If you
  change them IN ANY WAY, the debugger will cease to function correctly.
}
procedure _DbgEvalMarker;
begin
end;

procedure _DbgEvalExcept(E: TObject);
begin
end;

procedure _DbgEvalEnd;
begin
end;

{
  This function is used by the debugger to provide a soft landing spot
  when evaluating a function call that may raise an unhandled exception.
  The return address of _DbgEvalMarker is pushed onto the stack so that
  the unwinder will transfer control to the except block.
}
procedure _DbgEvalFrame;
begin
  try
    _DbgEvalMarker;
  except on E: TObject do
    _DbgEvalExcept(E);
  end;
  _DbgEvalEnd;
end;

{
  These export names need to match the names that will be generated into
  the .symtab section, so that the debugger can find them if stabs
  debug information is being generated.
}
exports
  _DbgExcNotify   name  '@DbgExcNotify',
  _DbgEvalFrame   name  '@DbgEvalFrame',
  _DbgEvalMarker  name  '@DbgEvalMarker',
  _DbgEvalExcept  name  '@DbgEvalExcept',
  _DbgEvalEnd     name  '@DbgEvalEnd';
{$IFEND LINUX or MACOS}

{ tell the debugger that the next raise is a re-raise of the current non-Delphi
  exception }
                                                                 
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure       NotifyReRaise;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
{     ->EAX     Pointer to exception object }
{       EDX     location of exception       }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    0                   { handler addr }
        PUSH    EDX                 { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excRaise            { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 28
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}
{$ELSE !PC_MAPPED_EXCEPTIONS}
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    0
        PUSH    0
        PUSH    cContinuable
        PUSH    cDelphiReRaise
        CALL    RaiseExceptionProc
@@1:
{$ENDIF !PC_MAPPED_EXCEPTIONS}
end;
{$ELSE TABLE_BASED_EXCEPTIONS}
procedure NotifyReRaise(Obj: TObject; Address: Pointer);
begin
  if DebugHook > 1 then
    RaiseExceptionProc(cDelphiReRaise, cContinuable, 0, nil);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{ tell the debugger about the raise of a non-Delphi exception }
{$IFDEF MSWINDOWS}
                                                                            
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure       NotifyNonDelphiException;
asm
{     ->EAX     Pointer to exception object }
{       EDX     Context record              }
        CMP     BYTE PTR DebugHook,0
        JE      @@1
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ESP
        PUSH    2
        PUSH    cContinuable
        PUSH    cNonDelphiException
        CALL    RaiseExceptionProc
        ADD     ESP,8
        POP     EAX
@@1:
end;
{$ELSE TABLE_BASED_EXCEPTIONS}
procedure NotifyNonDelphiException(ExceptionObject: Pointer; ContextRecord: PContext);
var
  Params: array[0..1] of Pointer;
begin
  if DebugHook <> 0 then
  begin
    Params[0] := ContextRecord;
    Params[1] := ExceptionObject;
    RaiseExceptionProc(cNonDelphiException, cContinuable, 2, @Params);
  end;
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}
{$ENDIF MSWINDOWS}

{ Tell the debugger where the handler for the current exception is located }
                                                                
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure NotifyExcept;
asm
{$IFDEF POSIX}
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
        PUSH    EAX
        MOV     EAX, [EAX].TRaisedException.ExceptObject

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excCatch            { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 24
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}

        POP     EAX
{$ELSE !POSIX}
        PUSH    ESP
        PUSH    1
        PUSH    cContinuable
        PUSH    cDelphiExcept           { our magic exception code }
        CALL    RaiseExceptionProc
        ADD     ESP,4
        POP     EAX
{$ENDIF !POSIX}
end;
{$ELSE TABLE_BASED_EXCEPTIONS}
procedure NotifyExcept(HandlerAddress: NativeUInt);
begin
  RaiseExceptionProc(cDelphiExcept, cContinuable, 1, @HandlerAddress);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

                                                                  
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure NotifyOnExcept;
asm
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excCatch            { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 28
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}
{$ELSE !PC_MAPPED_EXCEPTIONS}
{     ->EAX     Pointer to exception object             }
{       EBX     exception descriptor table entry        }
{       preserves EAX                                   }
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    [EBX].TExcDescEntry.handler
        JMP     NotifyExcept
@@1:
{$ENDIF !PC_MAPPED_EXCEPTIONS}
end;
{$ELSE TABLE_BASED_EXCEPTIONS}
procedure NotifyOnExcept(HandlerAddress: NativeUInt);
begin
  if DebugHook > 1 then
    NotifyExcept(HandlerAddress);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF MSWINDOWS}
                                                                   
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure NotifyAnyExcept;
asm
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    EBX
        JMP     NotifyExcept
@@1:
end;
{$ELSE TABLE_BASED_EXCEPTIONS}
procedure NotifyAnyExcept(HandlerAddress: NativeUInt);
begin
  if DebugHook > 1 then
    NotifyExcept(HandlerAddress);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}
{$ENDIF}

                                                            
{$IFDEF MSWINDOWS}
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure       CheckJmp;
asm
        TEST    ECX,ECX
        JE      @@3
        MOV     EAX,[ECX + 1]
        CMP     BYTE PTR [ECX],0E9H { near jmp }
        JE      @@1
        CMP     BYTE PTR [ECX],0EBH { short jmp }
        JNE     @@3
        MOVSX   EAX,AL
        INC     ECX
        INC     ECX
        JMP     @@2
@@1:
        ADD     ECX,5
@@2:
        ADD     ECX,EAX
@@3:
end;
{$ENDIF !TABLE_BASED_EXCEPTIONS}
{$ENDIF MSWINDOWS}

{ Notify debugger of a finally during an exception unwind }
                                                                       
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure NotifyExceptFinally;
asm
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        PUSH    0                   { exception name }
        PUSH    0                   { exception object }
        PUSH    excFinally          { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 28
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}
{$ELSE !PC_MAPPED_EXCEPTIONS}
{     ->ECX     Pointer to exception object }
{       preserves: EAX, ECX and EDX.        }
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    CheckJmp
        PUSH    ECX
        PUSH    ESP                     { pass pointer to arguments }
        PUSH    1                       { there is 1 argument }
        PUSH    cContinuable            { continuable execution }
        PUSH    cDelphiFinally          { our magic exception code }
        CALL    RaiseExceptionProc
        POP     ECX
        POP     ECX
        POP     EDX
        POP     EAX
@@1:
{$ENDIF !PC_MAPPED_EXCEPTIONS}
end;
{$ELSE TABLE_BASED_EXCEPTIONS}
                                                       
procedure NotifyExceptFinally(TargetIp: NativeUInt);
begin
  if DebugHook > 1 then
    RaiseExceptionProc(cDelphiFinally, cContinuable, 1, @TargetIp);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}


{ Tell the debugger that the current exception is handled and cleaned up.
  Also indicate where execution is about to resume. }
{$IFDEF MSWINDOWS}
                                                                   
                                                      
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure       NotifyTerminate;
asm
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EDX
        PUSH    ESP
        PUSH    1
        PUSH    cContinuable
        PUSH    cDelphiTerminate        { our magic exception code }
        CALL    RaiseExceptionProc
        POP     EDX
@@1:
end;
{$ENDIF !TABLE_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure NotifyTerminate(TargetIp: NativeUInt);
begin
  if DebugHook > 1 then
    RaiseExceptionProc(cDelphiTerminate, cContinuable, 1, @TargetIp);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}
{$ENDIF MSWINDOWS}

{ Tell the debugger that there was no handler found for the current exception
  and we are about to go to the default handler }
                                                                   
                                                      
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure       NotifyUnhandled;
asm
{     ->EAX     Pointer to exception object }
{       EDX     location of exception       }
{$IFDEF MSWINDOWS}
        PUSH    EAX
        PUSH    EDX
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    ESP
        PUSH    2
        PUSH    cContinuable
        PUSH    cDelphiUnhandled
        CALL    RaiseExceptionProc
@@1:
        POP     EDX
        POP     EAX
{$ELSE !MSWINDOWS}
        PUSH    EAX
        MOV     EAX, [EAX].TRaisedException.ExceptObject

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    0                   { handler addr }
        PUSH    EDX                 { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excUnhandled        { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 24
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}

        POP     EAX
{$ENDIF !MSWINDOWS}
end;
{$ENDIF !TABLE_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure NotifyUnhandled(ExceptObject, ExceptAddr: Pointer);
{$IFDEF MSWINDOWS}
var
  Params: array[0..1] of Pointer;
begin
  if DebugHook <> 0 then
  begin
    Params[0] := ExceptAddr;
    Params[1] := ExceptObject;
    RaiseExceptionProc(cDelphiUnhandled, cContinuable, 2, @Params);
  end;
end;
{$ELSE !MSWINDOWS}
var
  Obj: Pointer;
begin
  Obj := Pointer(TRaisedException(ExceptObject).ExceptObject);
  _DbgExcNotify(excUnhandled, Obj,
                PShortString(PPointer(PNativeInt(Obj)^ + vmtClassName)^),
                ExceptAddr, nil);
end;
{$ENDIF !MSWINDOWS}
{$ENDIF TABLE_BASED_EXCEPTIONS}


{$IFDEF PC_MAPPED_EXCEPTIONS}
//  MaybeCooptException
//    If a Delphi exception is thrown from C++, a TRaisedException object
//    will not be allocated yet on this side.  We need to keep things sane,
//    so we have to intercept such exceptions from the C++ side, and convert
//    them so that they appear to have been thrown from this RTL.  If we
//    throw a Delphi exception, then we set the private_2 member of
//    _Unwind_Exception to 0.  If C++ throws it, it sets it to the address
//    of the throw point.  We use this to distinguish the two cases, and
//    adjust data structures as appropriate.  On entry to this function,
//    EDX is the private_2 member, as set from SysRaiseException, and
//    EAX is the exception object in question.
//
procedure MaybeCooptException;
asm
        // If this exception is from C++, then private_2 will be a
        // throw address.  If not, then it will be zero.  private_1
        // will be either the exception object itself, or a TRaisedException.
        OR      EDX, EDX            // From C++?
        JZ      @@ExcAllocated

        // We've decided that the exception is from C++, but it is a
        // Delphi exception object.  We will coopt the exception now
        // by installing a TRaisedException into the unwinder exception,
        // and setting private_2 to 0.  Then the exception will look
        // like it was truly thrown from this RTL.
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    AllocateException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
@@ExcAllocated:
end;

function LinkException(Exc: PRaisedException): PRaisedException;
asm //StackAlignSafe
        PUSH    EDX     // preserve EDX because of HandleOnException
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        MOV     ECX, [EAX].ExceptionList
        MOV     [EDX].TRaisedException.Prev, ECX
        MOV     [EAX].ExceptionList, EDX
        MOV     EAX, EDX
        POP     EDX
end;

function UnlinkException: PRaisedException;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EDX, [EAX].ExceptionList
        MOV     EDX, [EDX].TRaisedException.Prev
        MOV     [EAX].ExceptionList, EDX
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF TABLE_BASED_EXCEPTIONS}
const // for TExceptionRecord.ExceptionFlags
  EXCEPTION_NONCONTINUABLE  = $0001; // Noncontinuable exception
  EXCEPTION_UNWINDING       = $0002; // Unwind is in progress
  EXCEPTION_EXIT_UNWIND     = $0004; // Exit unwind is in progress
  EXCEPTION_STACK_INVALID   = $0008; // Stack out of limits or unaligned
  EXCEPTION_NESTED_CALL     = $0010; // Nested exception handler call
  EXCEPTION_TARGET_UNWIND   = $0020; // Execute termination handler for it
  EXCEPTION_COLLIDED_UNWIND = $0040; // unwind through unwind dispatcher
  EXCEPTION_UNWIND          = ( EXCEPTION_UNWINDING or
                                EXCEPTION_EXIT_UNWIND or
                                EXCEPTION_TARGET_UNWIND or
                                EXCEPTION_COLLIDED_UNWIND);

function FindOnExceptionDescEntry(DispatcherContext: PDispatcherContext;
  ExceptionClass: Pointer; ExcDesc: PExcDesc): PExcDescEntry;
var
  ExcDescEntry: PExcDescEntry;
  DescIndex: Integer;
  ClassPtr1, TabClassPtr: Pointer;
  Name1, Name2: PAnsiChar;
begin
  for DescIndex := 0 to ExcDesc^.DescCount - 1 do
  begin
    ExcDescEntry := @ExcDesc.DescTable[DescIndex];
    Result := ExcDescEntry;
    if ExcDescEntry^.VTable = 0 then
      Exit;
    if ExceptionClass <> nil then
    begin
      ClassPtr1 := ExceptionClass;
      while True do
      begin
        TabClassPtr := PPointer(DispatcherContext.ImageBase + UIntPtr(ExcDescEntry^.VTable))^;
        if TabClassPtr = ClassPtr1 then
          Exit;
        if PInteger(PByte(TabClassPtr) + vmtInstanceSize)^ =
           PInteger(PByte(ClassPtr1) + vmtInstanceSize)^ then
        begin
          Name1 := PPAnsiChar(PByte(TabClassPtr) + vmtClassName)^;
          Name2 := PPAnsiChar(PByte(ClassPtr1) + vmtClassName)^;
          if (Name1^ = Name2^) and
             (_AStrCmp(PAnsiChar(PByte(Name1) + 1),
                       PAnsiChar(PByte(Name2) + 1), PByte(Name1)^) = 0) then
            Exit;
        end;
        ClassPtr1 := PPointer(PByte(ClassPtr1) + vmtParent)^;
        if ClassPtr1 = nil then
          Break;
        ClassPtr1 := PPointer(ClassPtr1)^;
      end;
    end;
  end;
  Result := nil;
end;

function _DelphiExceptionHandler(
  ExceptionRecord: PExceptionRecord;
  EstablisherFrame: NativeUInt;
  ContextRecord: _PContext;
  DispatcherContext: Pointer{PDispatcherContext}): LongInt{TExceptionDisposition};
var
  ImageBase: NativeUInt;
  ControlPcRVA: NativeUInt;
  ExceptionPointers: TExceptionPointers;
  ScopeIndex: Integer;
  TargetIpRVA: NativeUInt;
  ExceptionObject: Pointer;
  ExceptionClass: Pointer;
  ExceptionAddress: Pointer;
  ExcDescEntry: PExcDescEntry;
  ExcDesc: PExcDesc;
  ExcScope: PExcScope;
  ExcData: PExcData;
  TargetIp: NativeUInt;
  FilterRes: LongWord;
  JITCheckVal: Byte;
  LocalRaiseFrame: TRaiseFrame;
  PrevRaiseFramePtr: PRaiseFrame;
  ThisRaiseFramePtr: PRaiseFrame;
  RaiseFramePtr: PRaiseFrame;
begin
  Result := DISPOSITION_CONTINUE_SEARCH;
  ExcData := PExcData(PDispatcherContext(DispatcherContext).HandlerData);
  ImageBase := PDispatcherContext(DispatcherContext).ImageBase;
  ControlPcRVA := PDispatcherContext(DispatcherContext).ControlPc - ImageBase;
  if (ExceptionRecord.ExceptionFlags and EXCEPTION_UNWIND) = 0 then
  begin
    for ScopeIndex := 0 to ExcData^.ScopeCount - 1 do
    begin
      ExcScope := @ExcData^.ScopeTable[ScopeIndex];
      if (ControlPcRVA >= UIntPtr(ExcScope^.BeginOffset)) and
         (ControlPcRVA < UIntPtr(ExcScope^.EndOffset)) and
         (ExcScope^.TableOffset <> 0) then // safecall, catch_any or catch_table
      begin
        _FpuInit;
        ExcDescEntry := nil;
        if ExcScope^.TableOffset > 2 then
        begin
          // catch_table
          if ExceptionRecord.ExceptionCode = cDelphiException then
            ExceptionClass := PPointer(ExceptionRecord.ExceptObject)^
          else
          begin
            if not Assigned(ExceptClsProc) then
              Continue;
            ExceptionClass := TExceptClsProc(ExceptClsProc)(ExceptionRecord);
            if ExceptionClass = nil then
              Continue;
          end;
          ExcDesc := PExcDesc(ImageBase + UIntPtr(ExcScope^.TableOffset));
          ExcDescEntry := FindOnExceptionDescEntry(
                            PDispatcherContext(DispatcherContext),
                            ExceptionClass, ExcDesc);
          if ExcDescEntry = nil then
            Continue;
          TargetIp := ImageBase + UIntPtr(ExcDescEntry^.Handler);
        end
        else if ExcScope^.TableOffset = 1 then
          // safecall
          TargetIp := ImageBase  + NativeUInt(ExcScope^.EndOffset)
        else
          // ExcScope^.TableOffset = 2 // catch_any
          TargetIp := ImageBase  + NativeUInt(ExcScope^.TargetOffset);

        if ExceptionRecord.ExceptionCode = cDelphiException then
        begin
          ExceptionObject := ExceptionRecord.ExceptObject;
          ExceptionAddress := ExceptionRecord.ExceptAddr;
          JITCheckVal := 1;
        end
        else
        begin
          if not Assigned(ExceptObjProc) then
            Continue;
          ExceptionObject := TExceptObjProc(ExceptObjProc)(ExceptionRecord);
          if ExceptionObject = nil then
            Continue;
          ExceptionAddress := ExceptionRecord.ExceptionAddress;
          JITCheckVal := 1;
          if ExceptionRecord.ExceptionCode <> cCppException then
          begin
            NotifyNonDelphiException(ExceptionObject, ContextRecord);
            JITCheckVal := 0;
          end;
        end;
        if (JITEnable > JITCheckVal) and (DebugHook <= 0) then
        begin
          ExceptionPointers.ExceptionRecord := ExceptionRecord;
          ExceptionPointers.ContextRecord := ContextRecord;
          FilterRes := UnhandledExceptionFilter(@ExceptionPointers);
          if FilterRes = EXCEPTION_CONTINUE_SEARCH then
            Continue;
        end;
        ExceptionRecord.ExceptionFlags :=
          ExceptionRecord.ExceptionFlags or EXCEPTION_UNWINDING;
        if ExcScope^.TableOffset >= 2 then // catch_any or catch_table
          RaiseFramePtr := AllocateRaiseFrame
        else
          // Make the RaiseList entry on the stack
          RaiseFramePtr := @LocalRaiseFrame;
        RaiseFramePtr^.ExceptAddr := ExceptionAddress;
        RaiseFramePtr^.ExceptObject := ExceptionObject;
        LinkRaiseFrame(RaiseFramePtr);
        RtlUnwindEx(EstablisherFrame, TargetIp,
                    ExceptionRecord,
                    IntPtr(ExceptionObject),
                    PDispatcherContext(DispatcherContext).ContextRecord,
                    PDispatcherContext(DispatcherContext).HistoryTable);
      end;
    end;
  end
  else
  begin
    TargetIpRVA := PDispatcherContext(DispatcherContext).TargetIp - ImageBase;
    for ScopeIndex := 0 to ExcData^.ScopeCount - 1 do
    begin
      ExcScope := @ExcData^.ScopeTable[ScopeIndex];
      if (ControlPcRVA >= NativeUInt(ExcScope^.BeginOffset)) and
         (ControlPcRVA < NativeUInt(ExcScope^.EndOffset)) then
      begin
        if (TargetIpRVA >= NativeUInt(ExcScope^.BeginOffset)) and
           (TargetIpRVA < NativeUInt(ExcScope^.EndOffset)) and
           ((ExceptionRecord.ExceptionFlags and EXCEPTION_TARGET_UNWIND) <> 0) then
          Exit;
        if ExcScope^.TableOffset < 2 then
        begin // finally or safecall
          ThisRaiseFramePtr := RaiseListPtr;
          try
            case ExcScope^.TableOffset of
            0: // finally block
              begin
                //if TargetIpRVA = NativeUInt(ExcScope^.TargetOffset) then
                //  Exit; // DISPOSITION_CONTINUE_SEARCH
                PDispatcherContext(DispatcherContext).ControlPc := ImageBase + NativeUInt(ExcScope^.EndOffset);
                TargetIp := ImageBase + NativeUInt(ExcScope^.TargetOffset);
                NotifyExceptFinally(TargetIp);
                ExceptionPointers.ExceptionRecord := ExceptionRecord;
                ExceptionPointers.ContextRecord := ContextRecord;
                _TDelphiFinallyHandlerProc(TargetIp)(@ExceptionPointers, EstablisherFrame);
              end;
            1: // safecall / handle auto exception
              begin
                TargetIp := ImageBase + NativeUInt(ExcScope^.TargetOffset);
                PDispatcherContext(DispatcherContext).ControlPc :=
                  ImageBase + NativeUInt(ExcScope^.EndOffset);
                ExceptionPointers.ExceptionRecord := ExceptionRecord;
                ExceptionPointers.ContextRecord := ContextRecord;
                ExceptionObject := ThisRaiseFramePtr^.ExceptObject;
                ExceptionAddress := ThisRaiseFramePtr^.ExceptAddr;
                TargetIp := _TDelphiSafeCallCatchHandlerProc(TargetIp)(
                              @ExceptionPointers, EstablisherFrame,
                              ExceptionObject, ExceptionAddress);
                PDispatcherContext(DispatcherContext).ControlPc := TargetIp;
                // safecall handler doesn't need to call _DoneExcept.
                if ThisRaiseFramePtr^.ExceptObject <> nil then
                begin
                  ThisRaiseFramePtr^.ExceptObject.Free;
                  ThisRaiseFramePtr^.ExceptObject := nil;
                end;
              end;
            end;
          finally
            PrevRaiseFramePtr := nil;
            RaiseFramePtr := RaiseListPtr;
            while RaiseFramePtr <> nil do
            begin
              if RaiseFramePtr = ThisRaiseFramePtr then
              begin
                if (PrevRaiseFramePtr <> nil) or
                   (ExcScope^.TableOffset = 1) then // 1=safecall
                begin
                  // We come here if an finalization handler has thrown yet
                  // another exception we need to destroy the exception
                  // object and unlink the raise list.
                  if PrevRaiseFramePtr = nil then
                    RaiseListPtr := nil
                  else
                    PrevRaiseFramePtr^.NextRaise := RaiseFramePtr^.NextRaise;
                  if RaiseFramePtr^.ExceptObject <> nil then
                  begin
                    RaiseFramePtr^.ExceptObject.Free;
                    RaiseFramePtr^.ExceptObject := nil;
                  end;
                  ReleaseRaiseFrame(RaiseFramePtr);
                end;
                Break;
              end;
              PrevRaiseFramePtr := RaiseFramePtr;
              RaiseFramePtr := RaiseFramePtr^.NextRaise;
            end; // while RaiseFramePtr...
          end; // try-finally...
        end // ExcScope^.TableOffset < 2
        else
        begin // ExcScope^.TableOffset >= 2
          // If current handler is target(termination) catch handler,
          // Invoke NotifyAnyExcept or NotifyOnExcept, and exit loop.
          if ((ExceptionRecord.ExceptionFlags and EXCEPTION_TARGET_UNWIND) <> 0) then
          begin
            TargetIp := 0;
            if ExcScope^.TableOffset = 2 then // catch_any
              TargetIp := ImageBase + NativeUInt(ExcScope^.TargetOffset)
            else // catch_table
            begin
              if ExceptionRecord.ExceptionCode = cDelphiException then
                ExceptionClass := PPointer(ExceptionRecord.ExceptObject)^
              else
              begin
                if Assigned(ExceptClsProc) then
                  ExceptionClass := nil
                else
                  ExceptionClass := TExceptClsProc(ExceptClsProc)(ExceptionRecord);
              end;
              if ExceptionClass <> nil then
              begin
                ExcDesc := PExcDesc(ImageBase + NativeUInt(ExcScope^.TableOffset));
                ExcDescEntry := FindOnExceptionDescEntry(
                                  PDispatcherContext(DispatcherContext),
                                  ExceptionClass, ExcDesc);
                if ExcDescEntry <> nil then
                  TargetIp := ImageBase + UIntPtr(ExcDescEntry^.Handler);
              end;
            end; // if ExcScope^.TableOffset
            if (TargetIp <> 0) and
               (TargetIp = PDispatcherContext(DispatcherContext).TargetIp) then
            begin
              if ExcScope^.TableOffset = 2 then // catch_any
                NotifyAnyExcept(TargetIp)
              else
                NotifyOnExcept(TargetIp);
              Break;
            end;
          end; // if ExceptionRecord.ExceptionFlags
        end; // if ExcScope^.TableOffset
      end; // if ControlPcRVA...
    end; // for ScopeIndex...
  end;
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF STACK_BASED_EXCEPTIONS}
procedure _HandleFinallyInternal; forward;
{$ENDIF STACK_BASED_EXCEPTIONS}

{
 When an exception is to be handled unconditionally by some bit of user code,
 there is still some book-keeping that needs to be done.  There is special
 handling that needs to be done if the exception is from C++, either a Delphi
 exception, or a pure C++ exception.  We have to restore some internal state
 as well, and we have to notify the debugger.  Once we've done those things,
 we return to the exception handling fragment in the user code that called us.
}
                                                                       
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure _HandleAnyException;
asm //StackAlignSafe
{$IFDEF PC_MAPPED_EXCEPTIONS}
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        // C++ exceptions aren't wanted here.  Retoss them as is
        // We won't return from this.
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX

{$IFDEF ALIGN_STACK}
        // We'll just increase our alignment adjustment from above
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        OR      [EAX].TRaisedException.Flags, excIsBeingHandled
        CALL    LinkException
        MOV     ESI, EBX
        MOV     EDX, [ESP] // EDX = return address
        CALL    NotifyExcept
        MOV     EBX, ESI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
        {
         When _HandleAnyException was called, the stack was adjusted to align
         it per the Mach ABI.  The unwinder will have reset the stack pointer
         to the prologue result, and the generated code in the caller will
         have further adjusted for dynamic arrays.  Thus the alignment adjustment
         made by the compiler will always have been 12 bytes.  The code generator
         could dispose of that in the user code, on return, but it saves a little
         space, and it's easy to do from here.
        }
//        RET   12
{$ENDIF ALIGN_STACK}
        // End of routine - we return to user code now.
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFNDEF  PC_MAPPED_EXCEPTIONS}
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        JE      @@DelphiException
        CLD
        CALL    _FpuInit
        MOV     EDX,ExceptObjProc
        TEST    EDX,EDX
        JE      @@exit
        CALL    EDX
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[ESP+12]
        MOV     ECX,[ESP+4]
        CMP     [ECX].TExceptionRecord.ExceptionCode,cCppException
        JE      @@CppException
        CALL    NotifyNonDelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@CppException
        CMP     BYTE PTR DebugHook,0
        JA      @@CppException                     // Do not JIT if debugging
        LEA     ECX,[ESP+4]
        PUSH    EAX
        PUSH    ECX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     EAX
        JE      @@exit
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress
        JMP     @@GoUnwind

@@CppException:
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress

@@DelphiException:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0                { Do not JIT if debugging }
        JA      @@GoUnwind
        PUSH    EAX
        LEA     EAX,[ESP+8]
        PUSH    EDX
        PUSH    ECX
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit

@@GoUnwind:
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    EBX
        XOR     EBX,EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,FS:[EBX]
        PUSH    EBX                     { Save pointer to topmost frame }
        PUSH    EAX                     { Save OS exception pointer     }
        PUSH    EDX                     { Save exception object         }
        PUSH    ECX                     { Save exception address        }

        MOV     EDX,[ESP+8+8*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc
@@returnAddress:

        MOV     EDI,[ESP+8+8*4]

        {       Make the RaiseList entry on the stack }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     EBX,[EDI].TExcFrame.desc
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally

        ADD     EBX,TExcDesc.instructions
        CALL    NotifyAnyExcept
        JMP     EBX

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an exception handler has thrown yet another exception }
        {       we need to destroy the exception object and pop the raise list. }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free

@@exit:
        MOV     EAX,1
{$ENDIF !PC_MAPPED_EXCEPTIONS}  { not PC_MAPPED_EXCEPTIONS }
end;
{$ENDIF !TABLE_BASED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  Common code between the Win32 and PC mapped exception handling
  scheme.  This function takes a pointer to an object, and an exception
  'on' descriptor table and finds the matching handler descriptor.

  For support of Linux, we assume that EBX has been loaded with the GOT
  that pertains to the code which is handling the exception currently.
  If this function is being called from code which is not PIC, then
  EBX should be zero on entry.

  N.B. For the Mac, it is critical that this code never calls out of
  the System unit, as we do not align the stack around calls to it.
}
procedure FindOnExceptionDescEntry;
asm
{ ->    EAX raised object: Pointer                }
{       EDX descriptor table: ^TExcDesc           }
{       EBX GOT of user code, or 0 if not an SO   }
{ <-    EAX matching descriptor: ^TExcDescEntry   }
        PUSH    EBP
        MOV     EBP, ESP
        SUB     ESP, 8                          { Room for vtable temp, and adjustor }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV [EBP - 8], EBX      { Store the potential GOT }
        MOV EAX, [EAX]          { load vtable of exception object }
        MOV     EBX,[EDX].TExcDesc.cnt
        LEA     ESI,[EDX].TExcDesc.excTab       { point ECX to exc descriptor table }
        MOV     [EBP - 4], EAX                  { temp for vtable of exception object }

@@innerLoop:
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EAX,EAX                         { catch all clause?                     }
        JE      @@found                         { yes: This is the handler              }
        ADD     EAX, [EBP - 8]                  { add in the adjustor (could be 0) }
        MOV     EDI,[EBP - 4]                   { load vtable of exception object       }
        JMP     @@haveVMT

@@vtLoop:
        MOV     EDI,[EDI]
@@haveVMT:
        MOV     EAX,[EAX]
        CMP     EAX,EDI
        JE      @@found

        MOV     ECX,[EAX].vmtInstanceSize
        CMP     ECX,[EDI].vmtInstanceSize
        JNE     @@parent

        MOV     EAX,[EAX].vmtClassName
        MOV     EDX,[EDI].vmtClassName

        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@parent

        INC     EAX
        INC     EDX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _AStrCmp
{$IFDEF ALIGN_STACK}
        ADD      ESP, 4
{$ENDIF ALIGN_STACK}
        JE      @@found

@@parent:
        MOV     EDI,[EDI].vmtParent             { load vtable of parent         }
        MOV     EAX,[ESI].TExcDescEntry.vTable
        ADD     EAX, [EBP - 8]                  { add in the adjustor (could be 0) }
        TEST    EDI,EDI
        JNE     @@vtLoop

        ADD     ESI,8
        DEC     EBX
        JNZ     @@innerLoop

        { Didn't find a handler. }
        XOR     ESI, ESI

@@found:
        MOV     EAX, ESI
@@done:
        POP     EDI
        POP     ESI
        POP     EBX
        MOV     ESP, EBP
        POP     EBP
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure       _HandleOnExceptionPIC;
asm
        { ->    EAX obj : Exception object }
        {       [RA]  desc: ^TExcDesc }
        { <-    Doesn't return }

        // Mark the exception as being handled
        OR      [EAX].TRaisedException.Flags, excIsBeingHandled

        MOV     ESI, EBX                      // Save the GOT
        MOV     EDX, [ESP]                    // Get the addr of the TExcDesc
        PUSH    EAX                           // Save the object
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        CALL    FindOnExceptionDescEntry
        OR      EAX, EAX
        JE      @@NotForMe

        MOV     EBX, ESI                      // Set back to user's GOT
        MOV     EDX, EAX
        POP     EAX                           // Get the object back
        POP     ECX                           // Ditch the return addr

        CALL    LinkException

        // Get the Pascal object itself.
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        MOV     EDX, [EDX].TExcDescEntry.handler
        ADD     EDX, EBX                      // adjust for GOT
        CALL    NotifyOnExcept

        MOV     EBX, ESI                      // Make sure of user's GOT
{$IFDEF ALIGN_STACK}
        {
         When _HandleOnExceptionPIC was called, the stack was adjusted to align
         it per the Mach ABI.  The unwinder will have reset the stack pointer
         to the prologue result, and the generated code in the caller will
         have further adjusted for dynamic arrays.  Thus the alignment adjustment
         made by the compiler will always have been 12 bytes.  We have to
         discard that alignment here.
        }
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     EDX                           // Back to the user code
        // never returns
@@NotForMe:
        POP     EAX                           // Get the exception object

        // Mark that we're reraising this exception, so that the
        // compiler generated exception handler for the 'except on' clause
        // will not get confused
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised

        JMP     SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{
 N.B.  On the Mac, the stack alignment code in _HandleOnException is fine tuned.
 On first read, it may seem incorrect.
}
                                                                      
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure       _HandleOnException;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm //StackAlignSafe
        { ->    EAX obj : Exception object }
        {       [RA]  desc: ^TExcDesc }
        { <-    Doesn't return }

        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        // C++ exceptions aren't wanted here.  Retoss them as is.
        // We won't return from this.
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        // Mark the exception as being handled
        OR      [EAX].TRaisedException.Flags, excIsBeingHandled

{$IFDEF ALIGN_STACK}
        MOV     EDX, [ESP + 12]               // Get the addr of the TExcDesc
{$ELSE !ALIGN_STACK}
        MOV     EDX, [ESP]                    // Get the addr of the TExcDesc
{$ENDIF !ALIGN_STACK}
        // STACK: 16 - no alignment required by FindOnExceptionDescEntry
        // N.B.  We have to make sure that FindOnExceptionDescEntry
        // never calls out of System.pas.
        PUSH    EAX                           // Save the object
        PUSH    EBX                           // Save EBX
        XOR     EBX, EBX                      // No GOT
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        CALL    FindOnExceptionDescEntry
        POP     EBX                           // Restore EBX
        OR      EAX, EAX                      // Is the exception for me?
        JE      @@NotForMe

        MOV     EDX, EAX
        POP     EAX                           // Get the object back
{$IFDEF ALIGN_STACK}
        ADD     ESP, 16                       // Ditch the alignment _and_ return addr
        // STACK ALIGNMENT: 16, since we ditched the return addr as well
{$ELSE !ALIGN_STACK}
        POP     ECX                           // Ditch the return addr
{$ENDIF !ALIGN_STACK}

        CALL    LinkException

        // Get the Pascal object itself.
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        MOV     EDX, [EDX].TExcDescEntry.handler
        CALL    NotifyOnExcept                // Tell the debugger about it

{$IFDEF ALIGN_STACK}
        {
         When _HandleOnException was called, the stack was adjusted to align
         it per the Mach ABI.  The unwinder will have reset the stack pointer
         to the prologue result, and the generated code in the caller will
         have further adjusted for dynamic arrays.  Thus the alignment adjustment
         made by the compiler will always have been 12 bytes.  We have to
         discard that alignment here.
        }
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     EDX                           // Back to the user code
        // never returns
@@NotForMe:
        POP     EAX                           // Get the exception object
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}

        // Mark that we're reraising this exception, so that the
        // compiler generated exception handler for the 'except on' clause
        // will not get confused
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised
        JMP     SysRaiseException             // Should be using resume here
end;
{$ELSE !PC_MAPPED_EXCEPTIONS}
                                                                      
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@DelphiException
        CLD
        CALL    _FpuInit
        MOV     EDX,ExceptClsProc
        TEST    EDX,EDX
        JE      @@exit
        CALL    EDX
        TEST    EAX,EAX
        JNE     @@common
        JMP     @@exit

@@DelphiException:
        MOV     EAX,[EAX].TExceptionRecord.ExceptObject
        MOV     EAX,[EAX]                       { load vtable of exception object       }

@@common:
        MOV     EDX,[ESP+8]

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     ECX,[EDX].TExcFrame.desc
        MOV     EBX,[ECX].TExcDesc.cnt
        LEA     ESI,[ECX].TExcDesc.excTab       { point ECX to exc descriptor table }
        MOV     EBP,EAX                         { load vtable of exception object }

@@innerLoop:
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EAX,EAX                         { catch all clause?                     }
        JE      @@doHandler                     { yes: go execute handler               }
        MOV     EDI,EBP                         { load vtable of exception object       }
        JMP     @@haveVMT

@@vtLoop:
        MOV     EDI,[EDI]
@@haveVMT:
        MOV     EAX,[EAX]
        CMP     EAX,EDI
        JE      @@doHandler

        MOV     ECX,[EAX].vmtInstanceSize
        CMP     ECX,[EDI].vmtInstanceSize
        JNE     @@parent

        MOV     EAX,[EAX].vmtClassName
        MOV     EDX,[EDI].vmtClassName

        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@parent

        INC     EAX
        INC     EDX
        CALL    _AStrCmp
        JE      @@doHandler

@@parent:
        MOV     EDI,[EDI].vmtParent             { load vtable of parent         }
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EDI,EDI
        JNE     @@vtLoop

        ADD     ESI,8
        DEC     EBX
        JNZ     @@innerLoop

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@doHandler:
        MOV     EAX,[ESP+4+4*4]
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        JE      @@haveObject
        CALL    ExceptObjProc
        MOV     EDX,[ESP+12+4*4]
        CALL    NotifyNonDelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@NoJIT
        CMP     BYTE PTR DebugHook,0
        JA      @@noJIT                 { Do not JIT if debugging }
        LEA     ECX,[ESP+4+4*4]
        PUSH    EAX
        PUSH    ECX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     EAX
        JE      @@exit

@@noJIT:
        MOV     EDX,EAX
        MOV     EAX,[ESP+4+4*4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress
        JMP     @@GoUnwind

@@haveObject:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0
        JA      @@GoUnwind
        PUSH    EAX
        LEA     EAX,[ESP+4+5*4]
        PUSH    EDX
        PUSH    ECX
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit

@@GoUnwind:
        XOR     EBX,EBX
        MOV     EBX,FS:[EBX]
        PUSH    EBX                     { Save topmost frame     }
        PUSH    EAX                     { Save exception record  }
        PUSH    EDX                     { Save exception object  }
        PUSH    ECX                     { Save exception address }

        MOV     EDX,[ESP+8+8*4]
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    ESI                     { Save handler entry     }

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc
@@returnAddress:

        POP     EBX                     { Restore handler entry  }

        MOV     EDI,[ESP+8+8*4]

        {       Make the RaiseList entry on the stack }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally
        MOV     EAX,[ESP].TRaiseFrame.ExceptObject
        CALL    NotifyOnExcept
        JMP     [EBX].TExcDescEntry.handler

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an exception handler has thrown yet another exception }
        {       we need to destroy the exception object and pop the raise list. }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free
@@exit:
        MOV     EAX,1
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS}
{$ENDIF !TABLE_BASED_EXCEPTIONS}

                                                                  
{$IFNDEF TABLE_BASED_EXCEPTIONS}
procedure _HandleFinally;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm //StackAlignSafe
{$IFDEF PIC}
        MOV     ESI, EBX
{$ENDIF PIC}
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
        // unwinding a C++ exception.  We handle that specially.
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        MOV     EDX, [ESP+12]
        CALL    EDX
        POP     ECX
        POP     EDX
        POP     EAX
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4 { RA, XX, and 2 PUSHes to come = 16}
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8 { RA, prev XX, XX XX = 16 }
{$ENDIF ALIGN_STACK}

        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

{$IFDEF ALIGN_STACK}
        MOV     EDX, [ESP + 12] { get the return address; stack still aligned }
{$ELSE !ALIGN_STACK}
        MOV     EDX, [ESP]
{$ENDIF !ALIGN_STACK}
        CALL    NotifyExceptFinally
        PUSH    EAX
{$IFDEF PIC}
        MOV     EBX, ESI
{$ENDIF PIC}
        {
          Mark the current exception with the EBP of the handler.  If
          an exception is raised from the finally block, then this
          exception will be orphaned.  We will catch this later, when
          we clean up the next except block to complete execution.
          See DoneExcept.
        }
        MOV [EAX].TRaisedException.HandlerEBP, EBP
        { Note: This call appears misalligned when ALIGN_STACK is
          defined, but the finally handler fixes up the stack upon
          entry.  This is due to the fact that the code generated for
          the finally block deals with both the normal flow, and the
          exception case.  In the normal flow, we push the address of
          a label to simulate a return address, prior to entering the
          finally block.  This push mis-aligns the stack, and the
          finally block compensates.  We'll be skipping that push,
          so the return address that we pushed above mirrors the effect. }
        CALL    EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        { We have to make it look like we've arrived here and setup
          a basic EBP frame, in order for the unwind that we will now
          cause to succeed properly.  We popped the saved EAX, now
          we have to get rid of stuff up to the original return address. }
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        {
          We executed the finally handler without adverse reactions.
          It's safe to clear the marker now.
        }
        MOV [EAX].TRaisedException.HandlerEBP, $FFFFFFFF
        PUSH    EBP
        MOV     EBP, ESP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JE      @@exit

        PUSH    EBX
        XOR     EBX,EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        {       Make exception frame    }

        PUSH    EBP
        PUSH    offset @@exceptFinally
        PUSH    dword ptr FS:[EBX]
        MOV     FS:[EBX],ESP

        MOV     EBX,FS:[EBX]
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        PUSH    EBX                     { Save pointer to topmost frame }
        PUSH    EAX                     { Save OS exception pointer     }
        PUSH    EDX                     { Save exception object         }
        PUSH    ECX                     { Save exception address        }

        MOV     EDI,[ESP+8+11*4]        { Load errPtr:PExcFrame         }

        {       Make the RaiseList entry on the stack   }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     ECX,[EDI].TExcFrame.desc
        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally
        ADD     ECX,TExcDesc.instructions
        CALL    NotifyExceptFinally
        CALL    ECX

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX
        ADD     ESP,5*4                 { Remove local RaiseList        }

        {       Remove exception frame  }

        XOR     EAX,EAX
        POP     EDX
        POP     ECX
        POP     ECX
        MOV     FS:[EAX],EDX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an finalization handler has thrown yet  }
        {       another exception we need to destroy the exception      }
        {       object and pop the raise list.                          }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free

@@exit:
        MOV     EAX,1
end;
{$ENDIF MSWINDOWS}
{$ENDIF !TABLE_BASED_EXCEPTIONS}

                                                                          
{$IFDEF STACK_BASED_EXCEPTIONS}
procedure       _HandleFinallyInternal;
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        MOV     EDX,[ESP+8]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JE      @@exit
        MOV     ECX,[EDX].TExcFrame.desc
        MOV     [EDX].TExcFrame.desc,offset @@exit

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBP,[EDX].TExcFrame.hEBP
        ADD     ECX,TExcDesc.instructions
        CALL    NotifyExceptFinally
        CALL    ECX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX

@@exit:
        MOV     EAX,1
end;
{$ENDIF STACK_BASED_EXCEPTIONS}

                                                                        
{$IFDEF STACK_BASED_EXCEPTIONS}
procedure _HandleAutoException;
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        CLD
        CALL    _FpuInit
        JE      @@DelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@DelphiException
        CMP     BYTE PTR DebugHook,0
        JA      @@DelphiException

@@DoUnhandled:
        LEA     EAX,[ESP+4]
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        JE      @@exit
        MOV     EAX,[ESP+4]
        JMP     @@GoUnwind

@@DelphiException:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0
        JA      @@GoUnwind
        JMP     @@DoUnhandled

@@GoUnwind:
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EDX,[ESP+8+3*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc

@@returnAddress:
        POP     EBP
        POP     EDI
        POP     ESI
        MOV     EAX,[ESP+4]
        MOV     EBX,8000FFFFH
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JNE     @@done

        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        MOV     EAX,[ESP+8]
        MOV     EAX,[EAX].TExcFrame.SelfOfMethod
        TEST    EAX,EAX
        JZ      @@freeException
        MOV     EBX,[EAX]
        CALL    DWORD PTR [EBX] + VMTOFFSET TObject.SafeCallException
        MOV     EBX,EAX
@@freeException:
        MOV     EAX,[ESP+4]
        MOV     EAX,[EAX].TExceptionRecord.ExceptObject
        CALL    TObject.Free
@@done:
        XOR     EAX,EAX
        MOV     ESP,[ESP+8]
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EDX
        POP     EBP
        LEA     EDX,[EDX].TExcDesc.instructions
        POP     ECX
        JMP     EDX
@@exit:
        MOV     EAX,1
end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _HandleAutoException;
asm
        // EAX = TObject reference, or nil
        // [ESP] = ret addr

{$IFDEF ALIGN_STACK}
         SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions

        CALL    CurrentException
        MOV     EDX, [ESP]
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException
        CALL    LinkException
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD      ESP, 4
{$ENDIF ALIGN_STACK}

//  The compiler wants the stack to look like this:
//  ESP+4->  HRESULT
//  ESP+0->  ret addr
//
//  Make it so.
//
        POP     EDX
        PUSH    8000FFFFH
        PUSH    EDX

        PUSH    ESI
        PUSH    EAX
        OR      EAX, EAX    // Was this a method call?
        JE      @@Done

        CALL    CurrentException
        MOV     EDX, [EAX].TRaisedException.ExceptObject
        MOV     ECX, [EAX].TRaisedException.ExceptionAddr;
        MOV     EAX, [ESP]
        MOV     ESI, [EAX]
        CALL    DWORD PTR [ESI] + VMTOFFSET TObject.SafeCallException;
        MOV     [ESP+12], EAX
@@Done:
        CALL    _DoneExcept
        POP     EAX
        POP     ESI
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
                                                        
                                                                 
procedure       _RaiseAtExcept;
asm //StackAlignSafe
        { ->    EAX     Pointer to exception object     }
        { ->    EDX     Purported addr of exception     }
        { Be careful: EBX is not set up in PIC mode. }
        { Outward bound calls must go through an exported fn, like SysRaiseException }
        OR      EAX, EAX
        JNE     @@GoAhead
        MOV     EAX, 216
        JMP     _RunError

@@GoAhead:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    BlockOSExceptions
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP, ESP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    NotifyReRaise
        CALL    AllocateException
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
        MOV     EDX, [EBX].RaiseExceptObjProc
        MOV     EDX, [EDX]
{$ELSE !PIC}
        MOV     EDX,RaiseExceptObjProc
{$ENDIF !PIC}
        TEST    EDX,EDX
        JZ      @@DoRaise
{$IFDEF ALIGN_STACK}
        MOV     [ESP],EAX
        CALL    EDX
        MOV     EAX,[ESP]
{$ELSE}
        PUSH    EAX
        CALL    EDX
        POP     EAX
{$ENDIF ALIGN_STACK}
@@DoRaise:
        CALL    SysRaiseException
        {
          This can only return if there was a terrible error.  In this event,
          we have to bail out.
        }
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
        POP     EBP
{$ENDIF ALIGN_STACK}
        JMP     _Run0Error
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

                                                 
{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure _RaiseAtExcept(Obj: TObject; Address: Pointer);
var
  Params: Array[0..6] of NativeUInt;
  ExceptionRecord: TExceptionRecord;
  //CurRaiseFrame: PRaiseFrame;
begin
  if Obj = nil then
    _RunError(216); // reAccessViolation

  Params[0] := UIntPtr(Address);
  Params[1] := UIntPtr(Pointer(Obj));
  Params[2] := 0 {EBX};
  Params[3] := 0 {ESI};
  Params[4] := 0 {EDI};
  Params[5] := 0 {EBP};
  Params[6] := 0 {ESP};

  if RaiseExceptObjProc <> nil then
  begin
    //CurRaiseFrame := RaiseListPtr;
    ExceptionRecord.ExceptionCode := cDelphiException;
    ExceptionRecord.ExceptionFlags := cNonContinuable;
    ExceptionRecord.ExceptionRecord := nil;
//  if CurRaiseFrame <> nil then
//    ExceptionRecord.ExceptionRecord := CurRaiseFrame^.ExceptionRecord;
    ExceptionRecord.ExceptionAddress := Address;
    ExceptionRecord.NumberParameters := 7;
    ExceptionRecord.ExceptionInformation[0] := Params[0];
    ExceptionRecord.ExceptionInformation[1] := Params[1];
    ExceptionRecord.ExceptionInformation[2] := Params[2];
    ExceptionRecord.ExceptionInformation[3] := Params[3];
    ExceptionRecord.ExceptionInformation[4] := Params[4];
    ExceptionRecord.ExceptionInformation[5] := Params[5];
    ExceptionRecord.ExceptionInformation[6] := Params[6];
    TRaiseExceptObjProc(RaiseExceptObjProc)(@ExceptionRecord);
  end;
  RaiseExceptionProc(cDelphiException, cNonContinuable, 7, @Params);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

                                                                
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure       _RaiseExcept;
asm
        { ->    EAX     Pointer to exception object     }
        MOV     EDX, [ESP]
        JMP     _RaiseAtExcept
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF STACK_BASED_EXCEPTIONS}
procedure       _RaiseExcept;
asm
  { When making changes to the way Delphi Exceptions are raised, }
  { please realize that the C++ Exception handling code reraises }
  { some exceptions as Delphi Exceptions.  Of course we want to  }
  { keep exception raising compatible between Delphi and C++, so }
  { when you make changes here, consult with the relevant C++    }
  { exception handling engineer. The C++ code is in xx.cpp, in   }
  { the RTL sources, in function tossAnException.                }

  { ->    EAX     Pointer to exception object     }
  {       [ESP]   Error address           }

        OR      EAX, EAX
        JNE     @@GoAhead
        MOV     EAX, 216
        CALL    _RunError
@@GoAhead:
        POP     EDX

        PUSH    ESP
        PUSH    EBP
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        PUSH    EAX                             { pass class argument           }
        PUSH    EDX                             { pass address argument         }

        MOV     EAX,ESP                         { Need these values later }
        PUSH    ESP                             { pass pointer to arguments             }
        PUSH    7                               { there are seven arguments               }
        PUSH    cNonContinuable                 { we can't continue execution   }
        PUSH    cDelphiException                { our magic exception code              }
        PUSH    EDX                             { pass the user's return address        }
        MOV     EDX,RaiseExceptObjProc          { has this been hooked? }
        TEST    EDX,EDX
        JZ      @@2

        PUSH    [EAX + 6 * 4]
        PUSH    [EAX + 5 * 4]
        PUSH    [EAX + 4 * 4]
        PUSH    [EAX + 3 * 4]
        PUSH    [EAX + 2 * 4]
        PUSH    [EAX + 1 * 4]                   { object }
        PUSH    [EAX + 0 * 4]                   { address }
        PUSH    7                               { how many of the above }
        PUSH    [EAX + 0 * 4]                   { the address goes here again }
        PUSH    EAX
        PUSH    EDX
        CALL    RaiseList
        MOV     ECX,EAX
        POP     EDX
        POP     EAX
        TEST    ECX,ECX
        JZ      @@1
        MOV     ECX,[ECX].TRaiseFrame.ExceptionRecord
@@1:    PUSH    ECX
        PUSH    cNonContinuable
        PUSH    cDelphiException
        MOV     EAX,ESP
        CALL    EDX
        ADD     ESP,12 * 4                      { Cleanup 12 DWORDS from the stack }
@@2:
        JMP     RaiseExceptionProc
end;
{$ENDIF STACK_BASED_EXCEPTIONS}

{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure _RaiseExcept(Obj: TObject);
begin
  _RaiseAtExcept(Obj, ReturnAddress);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  Used in the PC mapping exception implementation to handle exceptions in constructors.
}
                                                                          
procedure       _ClassHandleException;
asm
  {
  EAX = Ptr to TRaisedException
  EDX = self
  ECX = top flag
  }
        PUSH     ECX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        CALL     LinkException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 8
{$ENDIF ALIGN_STACK}
        MOV      EAX,EDX
        POP      EDX
        TEST     DL, DL
        JE       _RaiseAgain
        MOV      ECX,[EAX]
        MOV      DL,$81
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH     EAX
        CALL     DWORD PTR [ECX] + VMTOFFSET TObject.Destroy
        POP      EAX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        CALL     _ClassDestroy
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
        JMP      _RaiseAgain
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

                                                               
                                                  
procedure       _RaiseAgain;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm
{$IFDEF ALIGN_STACK}
        SUB      ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
// The following notifies the debugger of a reraise of exceptions.  This will
// be supported in a later release, but is disabled for now.
//        PUSH    EAX
//        MOV     EDX, [EAX].TRaisedException.ExceptionAddr
//        MOV     EAX, [EAX].TRaisedException.ExceptObject
//        CALL    NotifyReRaise                   { Tell the debugger }
//        POP     EAX
        TEST    [EAX].TRaisedException.Flags, excIsBeingHandled
        JZ      @@DoIt
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised
@@DoIt:
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    UnlinkException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EAX
        MOV     EDX, [ESP]                      { Get the user's addr }
        JMP     SysRaiseException
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF STACK_BASED_EXCEPTIONS}
asm
        { ->    [ESP        ] return address to user program }
        {       [ESP+ 4     ] raise list entry (4 dwords)    }
        {       [ESP+ 4+ 4*4] saved topmost frame            }
        {       [ESP+ 4+ 5*4] saved registers (4 dwords)     }
        {       [ESP+ 4+ 9*4] return address to OS           }
        { ->    [ESP+ 4+10*4] excPtr: PExceptionRecord       }
        {       [ESP+ 8+10*4] errPtr: PExcFrame              }

        { Point the error handler of the exception frame to something harmless }

        MOV     EAX,[ESP+8+10*4]
        MOV     [EAX].TExcFrame.desc,offset @@exit

        { Pop the RaiseList }

        CALL    SysInit.@GetTLS
        MOV     EDX,[EAX].RaiseListPtr
        MOV     ECX,[EDX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,ECX

        { Destroy any objects created for non-delphi exceptions }

        MOV     EAX,[EDX].TRaiseFrame.ExceptionRecord
        AND     [EAX].TExceptionRecord.ExceptionFlags,NOT cUnwinding
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@delphiException
        MOV     EAX,[EDX].TRaiseFrame.ExceptObject
        CALL    TObject.Free
        CALL    NotifyReRaise

@@delphiException:

        XOR     EAX,EAX
        ADD     ESP,5*4
        MOV     EDX,FS:[EAX]
        POP     ECX
        MOV     EDX,[EDX].TExcFrame.next
        MOV     [ECX].TExcFrame.next,EDX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
@@exit:
        MOV     EAX,1
end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
var
  CurRaiseFrame: PRaiseFrame;
  ExceptObject: Pointer;
  ExceptAddr: Pointer;
begin
  CurRaiseFrame := RaiseListPtr;
  ExceptObject := CurRaiseFrame^.ExceptObject;
  ExceptAddr := CurRaiseFrame^.ExceptAddr;
  CurRaiseFrame^.ExceptObject := nil;
  NotifyReRaise(ExceptObject, ExceptAddr);
  _RaiseAtExcept(ExceptObject, ExceptAddr);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  This is implemented slow and dumb.  The theory is that it is rare
  to throw an exception past an except handler, and that the penalty
  can be particularly high here.  Partly it's done the dumb way for
  the sake of maintainability.  It could be inlined.
}
procedure       _DestroyException;
var
  Exc: PRaisedException;
  RefCount: Integer;
  ExcObj: Pointer;
  ExcAddr: Pointer;
begin
  asm
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP
        JNE     @@notCPP
{$IFDEF ALIGN_STACK}
        SUB      ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysRaiseCPPException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
@@notCPP:
    MOV     Exc, EAX
  end;

  if (Exc^.Flags and excIsBeingReRaised) = 0 then
  begin
    RefCount := Exc^.RefCount;
    ExcObj := Exc^.ExceptObject;
    ExcAddr := Exc^.ExceptionAddr;
    Exc^.RefCount := 1;
    FreeException;
    _DoneExcept;
    Exc := AllocateException(ExcObj, ExcAddr);
    Exc^.RefCount := RefCount;
  end;

  Exc^.Flags := Exc^.Flags and not (excIsBeingReRaised or excIsBeingHandled);

  SysRaiseException(Exc);
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure _DestroyException(ExceptionPointers: PExceptionPointers; EstablisherFrame: NativeUInt);
var
  CurRaiseFrame: PRaiseFrame;
  ExceptObject: Pointer;
  ExceptAddr: Pointer;
  RaiseFramePtr: PRaiseFrame;
begin
  // RaiseFramePtr^.ExceptObject is
  //     nil -> re-raised exception will be catched in another
  //            domain (DLL / EXE). Pop and destroy old frame on the top.
  // not nil -> re-raised exception will be catched in the same
  //            domain. Pop and Destroy old frame on the 2nd.
  // Pop and save top of exception object list
  CurRaiseFrame := PopRaiseFrame;
  ExceptObject := CurRaiseFrame^.ExceptObject;
  ExceptAddr := CurRaiseFrame^.ExceptAddr;
  CurRaiseFrame^.ExceptObject := nil;
  ReleaseRaiseFrame(CurRaiseFrame);

  if Assigned(ExceptObject) then
  begin
    // Pop and release top of exception object list
    CurRaiseFrame := PopRaiseFrame;
    // Destroy exception object
    if CurRaiseFrame^.ExceptObject <> nil then
       TObject(CurRaiseFrame^.ExceptObject).Free;
    ReleaseRaiseFrame(CurRaiseFrame);

    // Restore saved exception object list
    RaiseFramePtr := AllocateRaiseFrame;
    RaiseFramePtr.ExceptAddr := ExceptAddr;
    RaiseFramePtr.ExceptObject := ExceptObject;
    LinkRaiseFrame(RaiseFramePtr);
  end;
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF PC_MAPPED_EXCEPTIONS}
                                                               
{ cleanup of old PC_MAPPED_EXCEPTION code, This routine doesn't appear to be
 used any more, and should be considered for removal}
procedure CleanupException;
asm
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done
        CALL    TObject.Free
@@Done:
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

                                                               
procedure       _DoneExcept;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        {
          We do one alignment call for the entire function as there are no
          other stack adjustments made in the function body.
        }
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done
        CALL    TObject.Free
@@Done:
        CALL    UnlinkException
        {
          Take a peek at the next exception object on the stack.
          If its EBP marker is at an address lower than our current
          EBP, then we know that it was orphaned when an exception was
          thrown from within the execution of a finally block.  We clean
          it up now, so that we won't leak exception records/objects.
        }
        CALL    CurrentException
        OR      EAX, EAX
        JE      @@Done2
        CMP     [EAX].TRaisedException.HandlerEBP, EBP
        JA      @@Done2
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done2
        CALL    TObject.Free
@@Done2:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF STACK_BASED_EXCEPTIONS}
asm
        { ->    [ESP+ 4+10*4] excPtr: PExceptionRecord       }
        {       [ESP+ 8+10*4] errPtr: PExcFrame              }

        { Pop the RaiseList }

        CALL    SysInit.@GetTLS
        MOV     EDX,[EAX].RaiseListPtr
        MOV     ECX,[EDX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,ECX

        { Destroy exception object }

        MOV     EAX,[EDX].TRaiseFrame.ExceptObject
        CALL    TObject.Free

        POP     EDX
        MOV     ESP,[ESP+8+9*4]
        XOR     EAX,EAX
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EAX
        POP     EBP
        CALL    NotifyTerminate
        JMP     EDX
end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
var
  CurRaiseFrame: PRaiseFrame;
  TargetIp: NativeUInt;
begin
  CurRaiseFrame := PopRaiseFrame;
  // Destroy exception object
  if CurRaiseFrame^.ExceptObject <> nil then
     TObject(CurRaiseFrame^.ExceptObject).Free;
  ReleaseRaiseFrame(CurRaiseFrame);
  TargetIp := UIntPtr(ReturnAddress);
  NotifyTerminate(TargetIp);
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

                                                                   
{$IFDEF STACK_BASED_EXCEPTIONS}
procedure _TryFinallyExit;
asm
        XOR     EDX,EDX
        MOV     ECX,[ESP+4].TExcFrame.desc
        MOV     EAX,[ESP+4].TExcFrame.next
        ADD     ECX,TExcDesc.instructions
        MOV     FS:[EDX],EAX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    ECX
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
@@1:    RET     12
end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
procedure _TryFinallyExit(EstablisherFrame: NativeUInt; TargetAddr: NativeUInt);
var
  Context: TContext;
begin
  RtlUnwindEx(EstablisherFrame, TargetAddr,
              nil, 0,         // PExceptionRecord, ReturnValue
              @Context, nil); // PContext, PUnwindHistoryTable
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF STACK_BASED_EXCEPTIONS}
procedure       MapToRunError(P: PExceptionRecord); stdcall;
const
  STATUS_ACCESS_VIOLATION         = $C0000005;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO     = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT     = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW           = $C0000091;
  STATUS_FLOAT_STACK_CHECK        = $C0000092;
  STATUS_FLOAT_UNDERFLOW          = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW         = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW           = $C00000FD;
  STATUS_CONTROL_C_EXIT           = $C000013A;
var
  ErrCode: Byte;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:  ErrCode := 200; { reDivByZero }
    STATUS_ARRAY_BOUNDS_EXCEEDED:   ErrCode := 201; { reRangeError }
    STATUS_FLOAT_OVERFLOW:          ErrCode := 205; { reOverflow }
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:       ErrCode := 207; { reInvalidOp }
    STATUS_FLOAT_DIVIDE_BY_ZERO:    ErrCode := 200; { reZeroDivide }
    STATUS_INTEGER_OVERFLOW:        ErrCode := 215; { reIntOverflow}
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:  ErrCode := 206; { reUnderflow }
    STATUS_ACCESS_VIOLATION:        ErrCode := 216; { reAccessViolation }
    STATUS_PRIVILEGED_INSTRUCTION:  ErrCode := 218; { rePrivInstruction }
    STATUS_CONTROL_C_EXIT:          ErrCode := 217; { reControlBreak }
    STATUS_STACK_OVERFLOW:          ErrCode := 202; { reStackOverflow }
  else                              ErrCode := 255;
  end;
  RunErrorAt(ErrCode, P.ExceptionAddress);
end;
{$ENDIF STACK_BASED_EXCEPTIONS}

                                                                     
                                                        
{$IFDEF STACK_BASED_EXCEPTIONS}
procedure       _ExceptionHandler;
asm
        MOV     EAX,[ESP+4]

        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR DebugHook,0
        JA      @@ExecuteHandler
        LEA     EAX,[ESP+4]
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        //JNE     @@ExecuteHandler
        //JMP     @@exit
        JE      @@exit
{$ENDIF MSWINDOWS}

@@ExecuteHandler:
        MOV     EAX,[ESP+4]
        CLD
        CALL    _FpuInit
        MOV     EDX,[ESP+8]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc

@@returnAddress:
        MOV     EBX,[ESP+4]
        CMP     [EBX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EBX].TExceptionRecord.ExceptAddr
        MOV     EAX,[EBX].TExceptionRecord.ExceptObject
        JE      @@DelphiException2

        MOV     EDX,ExceptObjProc
        TEST    EDX,EDX
        JE      MapToRunError
        MOV     EAX,EBX
        CALL    EDX
        TEST    EAX,EAX
        JE      MapToRunError
        MOV     EDX,[EBX].TExceptionRecord.ExceptionAddress

@@DelphiException2:

        CALL    NotifyUnhandled
        MOV     ECX,ExceptProc
        TEST    ECX,ECX
        JE      @@noExceptProc
        CALL    ECX             { call ExceptProc(ExceptObject, ExceptAddr) }

@@noExceptProc:
        MOV     ECX,[ESP+4]
        MOV     EAX,217
        MOV     EDX,[ECX].TExceptionRecord.ExceptAddr
        MOV     [ESP],EDX
        JMP     _RunError

@@exit:
        XOR     EAX,EAX
end;
{$ENDIF STACK_BASED_EXCEPTIONS}

                                                                                              
                                                          
{$IFDEF STACK_BASED_EXCEPTIONS}
procedure  SetExceptionHandler(Context: PInitContext);
asm
        { ->    EAX   PInitContext
        { ->    [EBP-type(TExcFrame)] TExcFrame local (returned in EAX) }

        PUSH    EAX               { Save off Context pointer }
        XOR     EDX,EDX           { using [EDX] saves some space over [0] }
        LEA     EAX,[EBP-type(TExcFrame)]
        MOV     ECX,FS:[EDX]      { ECX := head of chain                  }
        MOV     FS:[EDX],EAX      { head of chain := @exRegRec            }

        MOV     [EAX].TExcFrame.next,ECX
{$IFDEF PIC}
        LEA     EDX,[EBX]._ExceptionHandler
        MOV     [EAX].TExcFrame.desc,EDX
{$ELSE}
        MOV     [EAX].TExcFrame.desc,offset _ExceptionHandler
{$ENDIF}
        MOV     [EAX].TExcFrame.hEBP,EBP
        POP     ECX               { Restore Context pointer }
        MOV     [ECX].TInitContext.ExcFrame,EAX
end;
{$ENDIF STACK_BASED_EXCEPTIONS}

                                                                                                
{$IFDEF STACK_BASED_EXCEPTIONS}
procedure       UnsetExceptionHandler(Context: PInitContext);
asm
        { ->    EAX   PInitContext }

        MOV     EAX,[EAX].TInitContext.ExcFrame
        XOR     EDX,EDX
        TEST    EAX,EAX
        JZ      @@exit

        MOV     ECX,FS:[EDX]    { ECX := head of chain          }
        CMP     EAX,ECX         { simple case: our record is first      }
        JNE     @@search
        MOV     EAX,[EAX]       { head of chain := exRegRec.next        }
        MOV     FS:[EDX],EAX
        JMP     @@exit

@@loop:
        MOV     ECX,[ECX]
@@search:
        CMP     ECX,-1          { at end of list?                       }
        JE      @@exit          { yes - didn't find it          }
        CMP     [ECX],EAX       { is it the next one on the list?       }
        JNE     @@loop          { no - look at next one on list }
@@unlink:                       { yes - unlink our record               }
        MOV     EAX,[EAX]       { get next record on list               }
        MOV     [ECX],EAX       { unlink our record                     }
@@exit:
end;
{$ENDIF STACK_BASED_EXCEPTIONS}


var
  InitContext: TInitContext;
{$IFDEF MSWINDOWS}
  DLLThreadContext: TInitContext;
{$ENDIF}

type
  TProc = procedure;

{$IFDEF POSIX}
procedure CallProc(Proc: Pointer; GOT: Cardinal);
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        MOV     EBX,EDX
{$IFDEF LINUX}
        ADD     EAX,EBX
{$ENDIF LINUX}
        CALL    EAX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF POSIX}

procedure FinalizeUnits;
var
  Count: Integer;
  Table: PUnitEntryTable;
  P: Pointer;
begin
  if InitContext.InitTable = nil then
    exit;
  Count := InitContext.InitCount;
  Table := InitContext.InitTable^.UnitInfo;
{$IFDEF LINUX}
  Inc(PByte(Table), InitContext.Module^.GOT);
{$ENDIF}
  try
    while Count > 0 do
    begin
      Dec(Count);
      InitContext.InitCount := Count;
      P := Table^[Count].FInit;
      if Assigned(P) and Assigned(Pointer(P^)) then
      begin
{$IFDEF POSIX}
        CallProc(P, InitContext.Module^.GOT);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
        TProc(P)();
{$ENDIF}
      end;
    end;
  except
    FinalizeUnits;  { try to finalize the others }
    raise;
  end;
end;

const
  errCaption: array[0..5] of AnsiChar = ('E', 'r', 'r', 'o', 'r', #0);

{***********************************************************}
{$IFDEF TRIAL_EDITION}
{
    This code is used as part of the timeout test for
    applications built with trial editions of the product.  It provides
    the current local time in a format native to the platform in question.

    The linker will generate a checksum of _InitUnitPrep that it will
    place into linked units.  The code generated for _InitUnitPrep must
    not contain fixups actually in the image, as this could alter the
    code at load time, invalidating the checksum.  Take great care to
    make sure that this code is entirely position independent on all
    platforms and circumstances to avoid a serious problem!
}
{$IFDEF MSWINDOWS}

function _InitUnitPrep: Int64;
var
  SystemTime: TSystemTime;
  FileTime: TFileTime;
  Days: Int64;
begin
  GetLocalTime(SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);

    // used to hack the result to force a failure for testing:
  Days := 1000000000 div 100;
  Days := Days * 3600;
  Days := Days * 24;
  Days := Days * 31;
  Days := 0;

  Result := Int64(FileTime) + Days;
//  Dec(InitContext.InitTable^.UnitCount);
end;
{$ENDIF}
{$IFDEF LINUX}

function _InitUnitPrep: Integer;
var
  Days: Integer;
begin
  Days := 0;    // used to hack the result to force a failure for testing
    Result := _time(nil) + Days;
end;
{$ENDIF}

resourcestring
{$IFDEF LINUX}
  SExpiredMsg =
  'This module was compiled with a trial version of Kylix.'+#10+
  'The trial period has expired.'+#10;
{$ENDIF}
{$IFDEF MACOS}
  SExpiredMsg =
  'This module was compiled with a trial version of Delphi.'+#10+
  'The trial period has expired.'+#10;
{$ENDIF}
{$IFDEF MSWINDOWS}
  SExpiredMsg =
  'This module was compiled with a trial version of Delphi.'+#13+#10+
  'The trial period has expired.'+#13+#10;
{$ENDIF}
var
  ExpiredMsg: AnsiString;

function LoadResStringA(ResStringRec: PResStringRec): AnsiString; forward;

procedure _Expired;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  ExpiredMsg := LoadResStringA(@SExpiredMsg);
  if IsConsole then
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), PAnsiChar(ExpiredMsg), Length(ExpiredMsg), Dummy, nil)
  else
    MessageBoxA(0, PAnsiChar(ExpiredMsg), errCaption, 0);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  ExpiredMsg := LoadResStringA(@SExpiredMsg);
  __write(2, PAnsiChar(ExpiredMsg), Length(ExpiredMsg));
{$ENDIF POSIX}
  ExpiredMsg := '';
  Halt(232);
end;

{$ENDIF} // TRIAL_EDITION

procedure InitUnits;
var
  Count, I: Integer;
  Table: PUnitEntryTable;
  P: Pointer;
begin
  if InitContext.InitTable = nil then
    exit;
  Count := InitContext.InitTable^.UnitCount;
  I := 0;
  Table := InitContext.InitTable^.UnitInfo;
{$IFDEF LINUX}
  Inc(PByte(Table), InitContext.Module^.GOT);
{$ENDIF}
  try
    while I < Count do
    begin
      P := Table^[I].Init;
      Inc(I);
      InitContext.InitCount := I;
      if Assigned(P) and Assigned(Pointer(P^)) then
      begin
{$IFDEF POSIX}
        CallProc(P, InitContext.Module^.GOT);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
        TProc(P)();
{$ENDIF}
      end;
    end;
  except
    FinalizeUnits;
    raise;
  end;
end;

procedure _PackageLoad(const Table : PackageInfo; Module: PLibModule);
var
  SavedContext: TInitContext;
begin
  SavedContext := InitContext;
  InitContext.DLLInitState := 0;
  InitContext.InitTable := Table;
  InitContext.InitCount := 0;
  InitContext.Module := Module;
  InitContext.OuterContext := @SavedContext;
  try
    InitUnits;
  finally
    InitContext := SavedContext;
  end;
end;


procedure _PackageUnload(const Table : PackageInfo; Module: PLibModule);
var
  SavedContext: TInitContext;
begin
  SavedContext := InitContext;
  InitContext.DLLInitState := 0;
  InitContext.InitTable := Table;
  InitContext.InitCount := Table^.UnitCount;
  InitContext.Module := Module;
  InitContext.OuterContext := @SavedContext;
  try
    FinalizeUnits;
  finally
    InitContext := SavedContext;
  end;
end;

{$IF defined(LINUX) or defined(MACOS)}
procedure       _StartExe(InitTable: PackageInfo; Module: PLibModule; Argc: Integer; Argv: Pointer);
begin
  ArgCount := Argc;
  ArgValues := Argv;
{$IFEND LINUX or MACOS}
{$IFDEF MSWINDOWS}
procedure       _StartExe(InitTable: PackageInfo; Module: PLibModule);
begin
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$ENDIF MSWINDOWS}
  InitContext.InitTable := InitTable;
  InitContext.InitCount := 0;
  InitContext.Module := Module;
  MainInstance := Module.Instance;
{$IFDEF STACK_BASED_EXCEPTIONS}
  SetExceptionHandler(@InitContext);
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
  InitContext.ExcFrame := Pointer(1);
{$ENDIF TABLE_BASED_EXCEPTIONS}
  IsLibrary := False;
  InitUnits;
end;

{$IFDEF MSWINDOWS}
{$IFDEF CPUX86}
                                                             
procedure       _StartLib;
asm
        { ->    EAX InitTable   }
        {       EDX Module      }
        {       ECX InitTLS     }
        {       [ESP+4] DllProc }
        {       [EBP+8] HInst   }
        {       [EBP+12] Reason }
        {       [EBP-(type TExcFrame)] TExcFrame local }
        {       [EBP-(type TExcFrame)-(type TInitContext)] TInitContext local }

        { Push some desperately needed registers }

        PUSH    ECX
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        { Setup EBX to point to InitContext or DLLThreadContext based on Reason }

        MOV     EBX,offset InitContext
        CMP     DWORD PTR [EBP+12],2    // DLL_THEAD_ATTACH
        JL      @@notDLLThread
        MOV     EBX,offset DLLThreadContext

        { Save the current init context into the stackframe of our caller }

@@notDLLThread:
        MOV     ESI,EBX
        LEA     EDI,[EBP - (type TExcFrame) - (type TInitContext)]
        MOV     ECX,(type TInitContext)/4
        REP     MOVSD

        { Setup the current InitContext }

        POP     [EBX].TInitContext.DLLSaveEDI
        POP     [EBX].TInitContext.DLLSaveESI
        POP     [EBX].TInitContext.DLLSaveEBX
        MOV     [EBX].TInitContext.DLLSaveEBP,EBP
        MOV     [EBX].TInitContext.InitTable,EAX
        MOV     [EBX].TInitContext.Module,EDX
        LEA     ECX,[EBP - (type TExcFrame) - (type TInitContext)]
        MOV     [EBX].TInitContext.OuterContext,ECX

        { Get and save the current thread ID }

        CALL    GetCurrentThreadID
        MOV     [EBX].TInitContext.ThreadID,EAX
        MOV     EAX,[EBX].TInitContext.InitTable

        { Setup InitCount for FinalizeUnits call }

        XOR     ECX,ECX
        CMP     DWORD PTR [EBP+12],0    // Reason = DLL_PROCESS_DETACH?
        JNE     @@notShutDown
        MOV     ECX,[EAX].PackageInfoTable.UnitCount
@@notShutDown:
        MOV     [EBX].TInitContext.InitCount,ECX

        { Setup exception handler }

        MOV     EAX, offset RaiseException
        MOV     RaiseExceptionProc, EAX
        MOV     EAX, offset RTLUnwind
        MOV     RTLUnwindProc, EAX

        MOV     EAX,EBX                 // Pass address of current context
        CALL    SetExceptionHandler

        MOV     EAX,[EBP+12]
        INC     EAX
        MOV     [EBX].TInitContext.DLLInitState,AL
        DEC     EAX

        { Init any needed TLS }

        POP     ECX
        MOV     EDX,[ECX]
        MOV     [EBX].TInitContext.ExitProcessTLS,EDX
        JE      @@skipTLSproc
        CMP     AL,3                    // DLL_THREAD_DETACH
        JGE     @@skipTLSproc           // call ExitThreadTLS proc after DLLProc
        CALL    dword ptr [ECX+EAX*4]   // Call TlsProc[Reason]

@@skipTLSproc:

        { Call any DllProc }

        PUSH    ECX                     // TlsProc
        MOV     ECX,[ESP+8]             // DLLProc
        TEST    ECX,ECX
        JE      @@noDllProc
        MOV     EAX,[EBP+12]            // Reason
        MOV     EDX,[EBP+16]            // Reserved
        CALL    ECX

@@noDllProc:

        POP     ECX
        MOV     EAX, [EBP+12]
        CMP     AL,3                    // DLL_THREAD_DETACH
        JL      @@afterDLLproc          // don't free TLS on process shutdown
        CALL    dword ptr [ECX+EAX*4]   // Call TlsProc[Reason]

@@afterDLLProc:

        { Set IsLibrary if there was no exe yet }

        CMP     MainInstance,0
        JNE     @@haveExe
        MOV     IsLibrary,1
        FNSTCW  Default8087CW           // save host exe's FPU preferences

@@haveExe:

        MOV     EAX,[EBP+12]
        DEC     EAX
        JNE     _Halt0
        CALL    InitUnits
        RET     4
end;
{$ENDIF CPUX86}

{$IFDEF CPUX64}
                                                     
procedure _StartLib(ContextBuf: PInitContext; InitTable: PackageInfo; Module: PLibModule; TlsProc: Pointer; DllProc: TDllProcEx; AHInst: HINST; Reason: LongWord; Reserved: Pointer);
type
  TlsProcType = array[0..3] of procedure;
  PTlsProcType = ^TlsProcType;
const
  DLL_PROCESS_DETACH = 0;
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH  = 2;
  DLL_THREAD_DETACH  = 3;
var
  Context: PInitContext;
begin
  Context := @InitContext;
  if Reason >= DLL_THREAD_ATTACH then
    Context := @DllThreadContext;
  ContextBuf^ := Context^;

  // Setup the current InitContext
  Context.InitTable := InitTable;
  Context.Module := Module;
  Context.OuterContext := ContextBuf;

  // Get and save the current thread ID
  Context.ThreadID := GetCurrentThreadID;

  // Setup InitCount for FinalizeUnits call
  if Reason = DLL_PROCESS_DETACH then
    Context.InitCount := Context.InitTable.UnitCount
  else
    Context.InitCount := 0;

  // Setup exception handler
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$IFDEF STACK_BASED_EXCEPTIONS}
  SetExceptionHandler(@Context);
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
  Context^.ExcFrame := Pointer(1);
{$ENDIF TABLE_BASED_EXCEPTIONS}
  Context.DLLInitState := Reason + 1;

  // Init any needed TLS
  Context.ExitProcessTLS := PTlsProcType(TlsProc)^[DLL_PROCESS_DETACH];

  if (Reason = DLL_PROCESS_ATTACH) or (Reason = DLL_THREAD_ATTACH) then
    PTlsProcType(TlsProc)^[Reason]();

  // Call any DllProc
  if Assigned(DllProc) then
    DllProc(Reason, Reserved);

  // don't free TLS on process shutdown
  if Reason = DLL_THREAD_DETACH then
    PTlsProcType(TlsProc)^[Reason]();

  // Set IsLibrary if there was no exe yet
  if MainInstance = 0 then
  begin
    IsLibrary := True;
    DefaultMXCSR := GetMXCSR;
  end;
  if Reason = DLL_PROCESS_ATTACH then
    InitUnits
  else
    _Halt0;
end;
{$ENDIF CPUX64}
{$ENDIF  MSWINDOWS}

{$IFDEF POSIX}
procedure       _StartLib(Context: PInitContext; Module: PLibModule; DLLProc: TDLLProcEx);
var
  TempSwap: TInitContext;
begin
  // Context's register save fields are already initialized.
  // Save the current InitContext and activate the new Context by swapping them
  TempSwap := InitContext;
  InitContext := PInitContext(Context)^;
  PInitContext(Context)^ := TempSwap;

  InitContext.Module := Module;
  InitContext.OuterContext := Context;

  // DLLInitState is initialized by SysInit to 0 for shutdown, 1 for startup
  // Inc DLLInitState to distinguish from package init:
  // 0 for package, 1 for DLL shutdown, 2 for DLL startup

  Inc(InitContext.DLLInitState);

  if InitContext.DLLInitState = 1 then
  begin
    InitContext.InitTable := Module.InitTable;
    if Assigned(InitContext.InitTable) then
      InitContext.InitCount := InitContext.InitTable.UnitCount  // shutdown
  end
  else
  begin
    Module.InitTable := InitContext.InitTable;  // save for shutdown
    InitContext.InitCount := 0;  // startup
  end;

  if Assigned(DLLProc) then
    DLLProc(InitContext.DLLInitState-1,nil);

  if MainInstance = 0 then        { Set IsLibrary if there was no exe yet }
  begin
    IsLibrary := True;
    Default8087CW := Get8087CW;
  end;

  if InitContext.DLLInitState = 1 then
    _Halt0
  else
    InitUnits;
end;
{$ENDIF POSIX}

function LoadResStringA(ResStringRec: PResStringRec): AnsiString;
begin
  Result := AnsiString(LoadResString(ResStringRec));
end;

function LoadResStringW(ResStringRec: PResStringRec): WideString;
begin
  Result := WideString(LoadResString(ResStringRec));
end;

function LoadResStringU(ResStringRec: PResStringRec): UnicodeString;
begin
  Result := UnicodeString(LoadResString(ResStringRec));
end;

{$IFDEF PUREPASCAL}
type
  _PResStringInitTableElem = ^_TResStringInitTableElem;
  _TResStringInitTableElem = record
    const // stringKind
      LString = 0;
      WString = 1;
      UString = 2;
    var
      variableAddress: Pointer;
      resStringAddress: PResStringRec;
      stringKind: NativeInt;
  end;
  _PResStringInitTable = ^_TResStringInitTable;
  _TResStringInitTable = record
    Count: NativeInt;
    Table: array[1..(MaxInt div SizeOf(_TResStringInitTableElem) - 2)] of _TresStringInitTableElem;
  end;
  {$NODEFINE _PResStringInitTableElem}
  {$NODEFINE _TResStringInitTableElem}
  {$NODEFINE _PResStringInitTable}
  {$NODEFINE _TResStringInitTable}

  _PResStringImportInitTableElem = ^_TResStringImportInitTableElem;
  _TResStringImportInitTableElem = record
    const // stringKind
      LString = 0;
      WString = 1;
      UString = 2;
    var
      variableAddress: Pointer;
      resStringIndirAddress: ^PResStringRec;
      stringKind: NativeInt;
  end;
  _PResStringImportInitTable = ^_TResStringImportInitTable;
  _TResStringImportInitTable = record
    Count: NativeInt;
    Table: array[1..(MaxInt div SizeOf(_TResStringImportInitTableElem) - 2)] of _TResStringImportInitTableElem;
  end;
  {$NODEFINE _PResStringImportInitTableElem}
  {$NODEFINE _TResStringImportInitTableElem}
  {$NODEFINE _PResStringImportInitTable}
  {$NODEFINE _TResStringImportInitTable}

  _PImportInitTableElem = ^_TImportInitTableElem;
  _TImportInitTableElem = record
    variableAddress: Pointer;
    sourceIndirAddress: PPointer;
    soruceOffset: NativeInt;
  end;
  _PImportInitTable = ^_TImportInitTable;
  _TImportInitTable = record
    Count: NativeInt;
    Table: array[1..(MaxInt div SizeOf(_TImportInitTableElem) - 2)] of _TImportInitTableElem;
  end;
  {$NODEFINE _PImportInitTableElem}
  {$NODEFINE _TImportInitTableElem}
  {$NODEFINE _PImportInitTable}
  {$NODEFINE _TImportInitTable}

  _PWideStringInitTableElem = ^_TWideStringInitTableElem;
  _TWideStringInitTableElem = record
    variableAddress: Pointer;
    stringAddress: Pointer;
  end;
  _PWideStringInitTable = ^_TWideStringInitTable;
  _TWideStringInitTable = record
    Count: NativeInt;
    Table: array[1..(MaxInt div SizeOf(_TWideStringInitTableElem) - 2)] of _TWideStringInitTableElem;
  end;
  {$NODEFINE _PWideStringInitTableElem}
  {$NODEFINE _TWideStringInitTableElem}
  {$NODEFINE _PWideStringInitTable}
  {$NODEFINE _TWideStringInitTable}
{$ENDIF}

                                                                   
                                                   
//procedure _InitResStrings(InitTable: _PResStringInitTable);
procedure _InitResStrings(InitTable: Pointer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: _PResStringInitTableElem;
begin
  for I := 1 to _PResStringInitTable(InitTable)^.Count do
  begin
    P := @_PResStringInitTable(InitTable)^.Table[I];
    case P^.stringKind of
      _TResStringInitTableElem.LString:
        PAnsiString(P^.variableAddress)^ := LoadResStringA(P^.resStringAddress);
      _TResStringInitTableElem.WString:
        PWideString(P^.variableAddress)^ := LoadResStringW(P^.resStringAddress);
      _TResStringInitTableElem.UString:
        PUnicodeString(P^.variableAddress)^ := LoadResStringU(P^.resStringAddress);
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      resStringAddress: Pointer;   }
        {                      stringKind: (LString, WString, UString) as Int32; }
        {                   end;                            }
        {                 end;                              }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]     // EDI := initTable.cnt
        LEA     ESI,[EBX+EAX+4]   // ESI := @initTable.tab
@@loop:
        MOV     EAX,[ESI+4]       // EAX := initTable.tab[i].resStringAddress
        ADD     EAX,EBX
        MOV     EDX,[ESI]         // EDX := initTable.tab[i].variableAddress
        ADD     EDX,EBX
        MOV     ECX,[ESI+8]       // ECX := initTable.tab[i].stringKind

        // Handle appropriate string kind.
        TEST    ECX,ECX
        JZ      @@lstring
        DEC     ECX
        JZ      @@wstring
        DEC     ECX
        JZ      @@ustring
        INT     3

@@lstring:
        CALL    LoadResStringA
        JMP     @@doneLoad

@@wstring:
        CALL    LoadResStringW
        JMP     @@doneLoad

@@ustring:
        CALL    LoadResStringU

@@doneLoad:
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

        POP     ESI
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

                                                                         
                                                         
//procedure _InitResStringImports(InitTable: _PResStringImportInitTable);
procedure _InitResStringImports(InitTable: Pointer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: _PResStringImportInitTableElem;
begin
  for I := 1 to _PResStringImportInitTable(InitTable)^.Count do
  begin
    P := @_PResStringImportInitTable(InitTable)^.Table[I];
    case P^.stringKind of
      _TResStringImportInitTableElem.LString:
        PAnsiString(P^.variableAddress)^ := LoadResStringA(P^.resStringIndirAddress^);
      _TResStringImportInitTableElem.WString:
        PWideString(P^.variableAddress)^ := LoadResStringW(P^.resStringIndirAddress^);
      _TResStringImportInitTableElem.UString:
        PUnicodeString(P^.variableAddress)^ := LoadResStringU(P^.resStringIndirAddress^);
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      resStringAddress: ^Pointer; *** note indirection  }
        {                      stringKind: (LString, WString, UString) as Int32; }
        {                   end;                            }
        {                 end;                              }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]     // EDI := initTable.cnt
        LEA     ESI,[EBX+EAX+4]   // ESI := @initTable.tab
@@loop:
        MOV     EAX,[ESI+4]       // EAX := initTable.tab[i].resStringAddress
        MOV     EAX,[EBX+EAX]     // EAX := EAX^ (to do indirection)
        MOV     EDX,[ESI]         // EDX := initTable.tab[i].variableAddress
{$IFNDEF MACOS}
        ADD     EDX,EBX
{$ENDIF MACOS}
        MOV     ECX,[ESI+8]       // ECX := initTable.tab[i].stringKind

        // Handle appropriate string kind.
        TEST    ECX,ECX
        JZ      @@lstring
        DEC     ECX
        JZ      @@wstring
        DEC     ECX
        JZ      @@ustring
        INT     3

@@lstring:
        CALL    LoadResStringA
        JMP     @@doneLoad

@@wstring:
        CALL    LoadResStringW
        JMP     @@doneLoad

@@ustring:
        CALL    LoadResStringU

@@doneLoad:
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

        POP     ESI
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

                                                                
                                                
//procedure _InitImports(InitTable: _PImportInitTable);
procedure _InitImports(InitTable: Pointer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: _PImportInitTableElem;
begin
  for I := 1 to _PImportInitTable(InitTable)^.Count do
  begin
    P := @_PImportInitTable(InitTable)^.Table[I];
    PPointer(P^.variableAddress)^ := Pointer(PByte(P^.sourceIndirAddress^) + P^.soruceOffset);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      sourceAddress: ^Pointer;     }
        {                      sourceOffset: Longint;       }
        {                   end;                            }
        {                 end;                              }
        { ->    EDX     Linux only, this points to          }
        {               SysInit.ModuleIsCpp                 }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }
{$IFDEF MACOS}
                                                                
        RET
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF MSWINDOWS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]
        LEA     ESI,[EBX+EAX+4]
{$IFDEF LINUX}
        {
            The C++ linker may have already fixed these things up to valid
            addresses.  In this case, we don't want to do this pass.  If this
            module's init tab was linked with ilink, then SysInit.ModuleIsCpp
            will be set, and we'll bail out.
        }
        CMP     BYTE PTR[EDX+EBX], 0  { SysInit.ModuleIsCpp }
        JNE     @@exit
{$ENDIF LINUX}
@@loop:
        MOV     EAX,[ESI+4]     { load address of import    }
        MOV     EDX,[ESI]       { load address of variable  }
        MOV     EAX,[EBX+EAX]   { load contents of import   }
        ADD     EAX,[ESI+8]     { calc address of variable  }
        MOV     [EBX+EDX],EAX   { store result              }
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

@@exit:

        POP     ESI
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF MSWINDOWS}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

                                                                    
{$IFDEF MSWINDOWS}
//procedure _InitWideStrings(InitTable: _PWideStringInitTable);
procedure _InitWideStrings(InitTable: Pointer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: _PWideStringInitTableElem;
begin
  for I := 1 to _PWideStringInitTable(InitTable)^.Count do
  begin
    P := @_PWideStringInitTable(InitTable)^.Table[I];
    _WStrAsg(PWideString(P^.variableAddress)^, WideString(P^.stringAddress));
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
     { ->    EAX     Pointer to init table               }
     {                 record                            }
     {                   cnt: Integer;                   }
     {                   tab: array [1..cnt] record      }
     {                      variableAddress: Pointer;    }
     {                      stringAddress: ^Pointer;     }
     {                   end;                            }
     {                 end;                              }

    PUSH    EBX
    PUSH    ESI
    MOV     EBX,[EAX]
    LEA     ESI,[EAX+4]
@@loop:
    MOV     EDX,[ESI+4]     { load address of string    }
    MOV     EAX,[ESI]       { load address of variable  }
    CALL    _WStrAsg
    ADD     ESI,8
    DEC     EBX
    JNZ     @@loop

    POP     ESI
    POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

{$IFDEF CPUX64}
var
  runErrMsg: array[0..37] of AnsiChar = (
    'R', 'u', 'n', 't', 'i', 'm', 'e', ' ', // 0..7
    'e', 'r', 'r', 'o', 'r', ' ', ' ', ' ', // 8..15
    ' ', ' ', 'a', 't', ' ', '0', '0', '0', // 16..23
    '0', '0', '0', '0', '0', '0', '0', '0', // 24..31
    '0', '0', '0', '0', '0', #0);           // 32..37
{$ELSE !CPUX64}
var
  runErrMsg: array[0..29] of AnsiChar = (
    'R', 'u', 'n', 't', 'i', 'm', 'e', ' ', // 0..7
    'e', 'r', 'r', 'o', 'r', ' ', ' ', ' ', // 8..15
    ' ', ' ', 'a', 't', ' ', '0', '0', '0', // 16..23
    '0', '0', '0', '0', '0', #0);           // 24..29
{$ENDIF !CPUX64}

const
  hexDigits: array[0..15] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

procedure MakeErrorMessage;
var
  digit: Byte;
  Temp: Integer;
  Addr: NativeUInt;
begin
  digit := 16;
  Temp := ExitCode;
  repeat
    runErrMsg[digit] := AnsiChar(Ord('0') + (Temp mod 10));
    Temp := Temp div 10;
    Dec(digit);
  until Temp = 0;
{$IFDEF CPUX64}
  digit := 36;
{$ELSE !CPUX64}
  digit := 28;
{$ENDIF !CPUX64}
  Addr := UIntPtr(ErrorAddr);
  repeat
    runErrMsg[digit] := hexDigits[Addr and $F];
    Addr := Addr div 16;
    Dec(digit);
  until Addr = 0;
end;


{$IFDEF TABLE_BASED_EXCEPTIONS}
function OSGetMem(Size: NativeInt): Pointer;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := HeapAlloc(GetProcessHeap, 0, Size);
{$ELSEIF DEFINED(POSIX)}
  Result := __malloc(Size);
{$ELSE}
  Error(rePlatformNotImplemented);
{$IFEND}
end;

function OSFreeMem(P: Pointer): Integer;
begin
{$IF DEFINED(MSWINDOWS)}
  HeapFree(GetProcessHeap, 0, P);
  Result := 0;
{$ELSEIF DEFINED(POSIX)}
  __free(P);
  Result := 0;
{$ELSE}
  Error(rePlatformNotImplemented);
{$IFEND}
end;

constructor _TExitDllException.Create(ExitCode: Integer);
begin
  inherited Create;
  FExitCode := ExitCode;
end;
class function _TExitDllException.NewInstance: TObject;
begin
  Pointer(Result) := InitInstance(OSGetMem(InstanceSize));
end;

procedure _TExitDllException.FreeInstance;
begin
  CleanupInstance;
  OSFreeMem(Pointer(Self));
end;

{$ENDIF TABLE_BASED_EXCEPTIONS}

                                                                                  
                                                                             
                                         
procedure ExitDll(Context: PInitContext);
{$IFDEF TABLE_BASED_EXCEPTIONS}
var
  ResultExitCode: Integer;
begin
  Context^ := Context.OuterContext^;
  ResultExitCode := ExitCode;
  ExitCode := 0;
  //raise _TExitDllException.Create(ResultExitCode);
  _RaiseExcept(_TExitDllException.Create(ResultExitCode));
end;
{$ELSE !TABLE_BASED_EXCEPTIONS}
{$IFDEF CPUX86}
asm
        { ->    EAX  PInitContext }

        { Restore the InitContext }
        MOV     EDI,EAX
        MOV     EBX,[EDI].TInitContext.DLLSaveEBX
        MOV     EBP,[EDI].TInitContext.DLLSaveEBP
        PUSH    [EDI].TInitContext.DLLSaveESI
        PUSH    [EDI].TInitContext.DLLSaveEDI

        MOV     ESI,[EDI].TInitContext.OuterContext
        MOV     ECX,(type TInitContext)/4
        REP     MOVSD
        POP     EDI
        POP     ESI

{$IFDEF MSWINDOWS}
        // Linux: See notes in legacy versions of this file.
        { Return False if ExitCode <> 0, and set ExitCode to 0 }
        XOR     EAX,EAX
        XCHG    EAX, ExitCode
        NEG     EAX
        SBB     EAX,EAX
        INC     EAX
{$ENDIF MSWINDOWS}

        LEAVE
{$IFDEF MSWINDOWS}
        RET     12
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
        RET
{$ENDIF LINUX}
end;
{$ENDIF CPUX86}
{$ENDIF !TABLE_BASED_EXCEPTIONS}

procedure WriteErrorMessage;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  if IsConsole then
  begin
    with TTextRec(Output) do
    begin
      if (Mode = fmOutput) and (BufPos > 0) then
        TTextIOFunc(InOutFunc)(TTextRec(Output));  // flush out text buffer
    end;
    // Leave #0 off end of runErrMsg
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), @runErrMsg, Sizeof(runErrMsg) - 1, Dummy, nil);
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), @sLineBreak[1], 2, Dummy, nil);
  end
  else if not NoErrMsg then
    MessageBoxA(0, runErrMsg, errCaption, 0);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  c: AnsiChar;
begin
  with TTextRec(Output) do
  begin
    if (Mode = fmOutput) and (BufPos > 0) then
      TTextIOFunc(InOutFunc)(TTextRec(Output));  // flush out text buffer
  end;
   __write(STDERR_FILENO, @runErrMsg, Sizeof(runErrMsg)-1);
   c := sLineBreak;
   __write(STDERR_FILENO, @c, 1);
{$ENDIF POSIX}
end;

var
  RTLInitFailed: Boolean = False;

procedure _Halt0;
var
  P: procedure;
                                                                                           
{$IFNDEF PC_MAPPED_EXCEPTIONS}
  ExceptObject: TObject;
{$ENDIF PC_MAPPED_EXCEPTIONS}
begin
{$IF defined(LINUX) or defined(MACOS)}
  if (ExitCode <> 0) and CoreDumpEnabled then
    __raise(SIGABRT);

  if (InitContext.DLLInitState = 2) and (ExitCode <> 0) then
    RTLInitFailed := True;

  if (InitContext.DLLInitState = 1) and RTLInitFailed then
    // RTL failed to initialized in library startup.  Units have already been
    // finalized, don't finalize them again.
    ExitDll(@InitContext);
{$IFEND LINUX or MACOS}

  { If there was some kind of runtime error, alert the user }

  if ErrorAddr <> nil then
  begin
    MakeErrorMessage;
    WriteErrorMessage;
    ErrorAddr := nil;
  end;

  { For DLL_THREAD_ATTACH or DLL_THREAD_DETACH, just cleanup and exit }
                                                             
{$IFDEF MSWINDOWS}
  if Assigned(DLLThreadContext.ExcFrame) and
    (GetCurrentThreadId = DLLThreadContext.ThreadID) then
  begin
{$IFDEF STACK_BASED_EXCEPTIONS}
    UnsetExceptionHandler(@DLLThreadContext);
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
    DLLThreadContext.ExcFrame := nil;
{$ENDIF TABLE_BASED_EXCEPTIONS}
    ExitDll(@DLLThreadContext);
  end;
{$ENDIF MSWINDOWS}

  if InitContext.DLLInitState = 0 then
    while ExitProc <> nil do
    begin
      @P := ExitProc;
      ExitProc := nil;
      P;
    end;

  { This loop exists because we might be nested in PackageLoad calls when }
  { Halt got called. We need to unwind these contexts.                    }

  while True do
  begin

    { If we are a library, and we are starting up fine, there are no units to finalize }

    if (InitContext.DLLInitState = 2) and (ExitCode = 0) then
      InitContext.InitCount := 0;

    { Clear the exception stack to prevent handled exceptions from being shown to the user }
{$IFNDEF PC_MAPPED_EXCEPTIONS}
                                                                                                        
                                                                                             
                                                                                       
                                                                                      
                                                                                           
                                                             

                                                                                 
    ExceptObject := TObject(AcquireExceptionObject);
    while ExceptObject <> nil do
    begin
      ExceptObject.Free;
      ExceptObject := TObject(AcquireExceptionObject);
    end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

    { Undo any unit initializations accomplished so far }

    FinalizeUnits;

    if (InitContext.DLLInitState <= 1) or (ExitCode <> 0) then
    begin
      if InitContext.Module <> nil then
        with InitContext do
        begin
          UnregisterModule(Module);
{$IFDEF PC_MAPPED_EXCEPTIONS}
          SysUnregisterIPLookup(Module.CodeSegStart);
{$ENDIF PC_MAPPED_EXCEPTIONS}
          if (Module.ResInstance <> Module.Instance) and (Module.ResInstance <> 0) then
{$IFDEF MSWINDOWS}
            FreeLibrary(Module.ResInstance);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
            dlclose(Module.ResInstance);
{$ENDIF POSIX}
        end;
    end;

{$IFDEF STACK_BASED_EXCEPTIONS}
    UnsetExceptionHandler(@InitContext);
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF TABLE_BASED_EXCEPTIONS}
    InitContext.ExcFrame := nil;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF MSWINDOWS}
    if InitContext.DllInitState = 1 then
      InitContext.ExitProcessTLS;
{$ENDIF MSWINDOWS}

    if InitContext.DllInitState <> 0 then
      ExitDll(@InitContext);

    if InitContext.OuterContext = nil then
    begin
      {
        If an ExitProcessProc is set, we call it.  Note that at this
        point the RTL is completely shutdown.  The only thing this is used
        for right now is the proper semantic handling of signals under Linux.
      }
      if Assigned(ExitProcessProc) then
        ExitProcessProc;
{$IFDEF MSWINDOWS}
      ExitProcess(ExitCode);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
      __exit(ExitCode);
{$ENDIF POSIX}
    end;

    InitContext := InitContext.OuterContext^
  end;
end;

procedure _Halt;
begin
  ExitCode := Code;
  _Halt0;
end;

                                                              
procedure _Run0Error;
begin
  ErrorAddr := ReturnAddress;
  Halt(0);
end;

                                                                              
procedure _RunError(errorCode: Byte);
begin
  ErrorAddr := ReturnAddress;
  Halt(errorCode);
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _UnhandledException;
type
  TExceptProc = procedure (Obj: TObject; Addr: Pointer);
begin
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptObject, ExceptAddr)
  else
    RunErrorAt(230, ExceptAddr);
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF TABLE_BASED_EXCEPTIONS}
                                                                                                
procedure _UnhandledException;
type
  TExceptProc = procedure (Obj: TObject; Addr: Pointer);
var
  ExceptionObject: TObject;
  ExceptionAddress: Pointer;
begin
  ExceptionObject := ExceptObject;
  ExceptionAddress := ExceptAddr;
  NotifyUnhandled(ExceptionObject, ExceptionAddress);
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptionObject, ExceptionAddress);
  RunErrorAt(217, ExceptionAddress); // reControlBreak
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}

{$IFDEF TABLE_BASED_EXCEPTIONS}
function _HandleExitDllException: Integer;
var
  ExceptionObject: TObject;
begin
  Result := -1;
  ExceptionObject := ExceptObject;
  if ExceptionObject is _TExitDllException then
    Result := _TExitDllException(ExceptionObject).ExitCode
  else
    _UnhandledException;
  _DoneExcept;
end;
{$ENDIF TABLE_BASED_EXCEPTIONS}


procedure _Assert(const Message, Filename: string; LineNumber: Integer);
begin
  if Assigned(AssertErrorProc) then
    AssertErrorProc(Message, Filename, LineNumber, ReturnAddress)
  else
    ErrorAt(Byte(reAssertionFailed), ReturnAddress);
end;

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    {
      WARNING: Don't change these fields without also changing them in
      the C++ RTL : winrtl/source/vcl/crtlvcl.cpp
    }
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

                                                                                             
                                                      
{$IFDEF MSWINDOWS}
function ThreadWrapper(Parameter: Pointer): Integer; stdcall;
{$ELSE}
function ThreadWrapper(Parameter: Pointer): NativeInt; cdecl;
{$ENDIF}
{$IFDEF PUREPASCAL}
var
  ThreadRec: TThreadRec;
begin
  Result := 0; // supress warning
  try
    ThreadRec := PThreadRec(Parameter)^;
    FreeMem(PThreadRec(Parameter));
    Result := ThreadRec.Func(ThreadRec.Parameter);
  except
    _UnhandledException;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Mark the top of the stack with a signature }
        PUSH    UNWINDFI_TOPOFSTACK
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _FpuInit
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EBP
{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     ECX,ECX
        PUSH    offset _ExceptionHandler
        MOV     EDX,FS:[ECX]
        PUSH    EDX
        MOV     FS:[ECX],ESP
{$ENDIF !PC_MAPPED_EXCEPTIONS}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    // The signal handling code in SysUtils depends on being able to
    // discriminate between Delphi threads and foreign threads in order
    // to choose the disposition of certain signals.  It does this by
    // testing a TLS index.  However, we allocate TLS in a lazy fashion,
    // so this test can fail unless we've already allocated the TLS segment.
    // So we force the allocation of the TLS index value by touching a TLS
    // value here.  So don't remove this silly call to AreOSExceptionsBlocked.
        CALL    AreOSExceptionsBlocked
{$ENDIF PC_MAPPED_EXCEPTIONS}
        MOV     EAX,Parameter

        MOV     ECX,[EAX].TThreadRec.Parameter
        MOV     EDX,[EAX].TThreadRec.Func
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ECX
        PUSH    EDX
        CALL    _FreeMem
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    EDX

{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     EDX,EDX
        POP     ECX
        MOV     FS:[EDX],ECX
        POP     ECX
{$ENDIF !PC_MAPPED_EXCEPTIONS}
        POP     EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Ditch our TOS marker }
        ADD     ESP, 4
{$ENDIF PC_MAPPED_EXCEPTIONS}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


{$IFDEF MSWINDOWS}
function BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: TThreadID): Integer;
var
  P: PThreadRec;
begin
  if Assigned(SystemThreadFuncProc) then
    P := PThreadRec(SystemThreadFuncProc(ThreadFunc, Parameter))
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
  end;

  IsMultiThread := TRUE;

  Result := CreateThread(SecurityAttributes, StackSize, @ThreadWrapper, P,
    CreationFlags, ThreadID);

  { P variable is supposed to be freed by the ThreadWrapper routine.
    If the call to CreateThread fails, then ThreadWrapper will not be called
    and P will not get freed. Check for failure now and free P if required.
  }
  if Result = 0 then
    Dispose(P);
end;


procedure EndThread(ExitCode: Integer);
begin
  if Assigned(SystemThreadEndProc) then
    SystemThreadEndProc(ExitCode);
  ExitThread(ExitCode);
end;
{$ENDIF}

{$IFDEF POSIX}
function BeginThread(Attribute: PThreadAttr;
                     ThreadFunc: TThreadFunc;
                     Parameter: Pointer;
                     var ThreadId: TThreadID): Integer;
var
  P: PThreadRec;
begin
  if Assigned(BeginThreadProc) then
    Result := BeginThreadProc(Attribute, ThreadFunc, Parameter, ThreadId)
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
    IsMultiThread := True;

    Result := pthread_create(pthread_t(ThreadID), Ppthread_attr_t(Attribute), @ThreadWrapper, P);

    { P variable is supposed to be freed by the ThreadWrapper routine.
      If the call to CreateThread fails, then ThreadWrapper will not be called
      and P will not get freed. Check for failure now and free P if required.
    }
    if Result <> 0 then
      Dispose(P);
  end;
end;

procedure EndThread(ExitCode: Integer);
begin
  if Assigned(EndThreadProc) then
    EndThreadProc(ExitCode);
  // No "else" required since EndThreadProc does not (!!should not!!) return.
  pthread_detach(pthread_t(GetCurrentThreadID));
  pthread_exit(ExitCode);
end;
{$ENDIF POSIX}


{ STRING SUPPORT }

{ ----------------------------------------------------- }
{       internal functions & procedures for strings     }
{ ----------------------------------------------------- }

// S must be non-nil.
function __StringLength(const S: UnicodeString): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 4)^;                // StrRec.length
end;

// S must be non-nil.
// Returns number of characters.
// Note: On Windows, length field contains number of bytes and not number
//       of characters.
function __StringLength(const S: WideString): LongInt; overload; inline;
begin
{$IFDEF MSWINDOWS}
  Result := PLongInt(PByte(S) - 4)^ div 2;          // size field of BSTR
{$ELSE}
  Result := PLongInt(PByte(S) - 4)^;                // StrRec.length
{$ENDIF}
end;

// S must be non-nil
function __StringLength(const S: RawByteString): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 4)^;                // StrRec.length
end;

// S must be non-nil
function __StringLength(const S: Pointer): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 4)^;                // StrRec.length
end;

// S must be non-nil
function __StringRefCnt(const S: UnicodeString): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 8)^;                // StrRec.refCnt
end;

{$IFNDEF MSWINDOWS}
// S must be non-nil
// Note: On Windows, WideString doesn't contain refCount field.
function __StringRefCnt(const S: WideString): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 8)^;                // StrRec.refCnt
end;
{$ENDIF}

// S must be non-nil
function __StringRefCnt(const S: RawByteString): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 8)^;                // StrRec.refCnt
end;

// S must be non-nil. Don't use for Windows WideString.
function __StringRefCnt(const S: Pointer): LongInt; overload; inline;
begin
  Result := PLongInt(PByte(S) - 8)^;                // StrRec.refCnt
end;

// S must be non-nil
function __StringCodePage(const S: UnicodeString): Word; overload; inline;
begin
  Result := PWord(PByte(S) - 12)^;                  // StrRec.codePage
end;

{$IFNDEF MSWINDOWS}
// S must be non-nil
// Note: On Windows, WideString doesn't contain codePage field.
function __StringCodePage(const S: WideString): Word; overload; inline;
begin
  Result := PWord(PByte(S) - 12)^;                  // StrRec.codePage
end;
{$ENDIF}

// S must be non-nil
function __StringCodePage(const S: RawByteString): Word; overload; inline;
begin
  Result := PWord(PByte(S) - 12)^;                  // StrRec.codePage
end;

// S must be non-nil. Don't use for Windows WideString.
function __StringCodePage(const S: Pointer): Word; overload; inline;
begin
  Result := PWord(PByte(S) - 12)^;                  // StrRec.codePage
end;


{ ------------------------------------------------------------- }
{       Compiler helper for string allocation and release       }
{ ------------------------------------------------------------- }

function _NewUnicodeString(CharLength: LongInt): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := nil;
  if CharLength > 0 then
  begin
    // Allocate a memory with record and extra wide-null terminator.
    if CharLength >= (MaxInt - SizeOf(StrRec)) div SizeOf(WideChar) then _IntOver;
    GetMem(P, SizeOf(StrRec) + (CharLength + 1) * SizeOf(WideChar));
    Result := Pointer(PByte(P) + SizeOf(StrRec));
    P.length := CharLength;
    P.refCnt := 1;
    P.elemSize := SizeOf(WideChar);
    P.codePage := Word(DefaultUnicodeCodePage);
    PWideChar(Result)[CharLength] := #0;
  end;
end;
{$ELSE}
asm
        { ->    EAX     length                  }
        { <-    EAX     pointer to new string   }
        TEST    EAX,EAX
        JLE     @@lengthLEZero  // length <= 0?
        PUSH    EAX             // save length
        ADD     EAX,EAX         // convert to bytes
        JO      @@overflow
        ADD     EAX,rOff+2      // + record + terminator
        JO      @@overflow
        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        CALL    _GetMem
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
        ADD     EAX,rOff
        POP     EDX                              // requested string length
        MOV     [EAX-skew].StrRec.refCnt,1
        MOV     [EAX-skew].StrRec.length,EDX
        MOV     word ptr [EAX+EDX*2],0           // wide null terminator
        MOV     word ptr [EAX-skew].StrRec.elemSize,2
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        MOV     EDX, [EAX].OFFSET DefaultUnicodeCodePage
        MOV     EDX, [EDX]
        POP     ECX
        POP     EAX
        POP     EBX
{$ELSE !PIC}
        MOV     EDX, DefaultUnicodeCodePage
{$ENDIF}
        MOV     word ptr [EAX-skew].StrRec.codePage,DX
        RET
@@overflow:
        {$IFDEF ALIGN_STACK}
        POP     EAX
        {$ENDIF ALIGN_STACK}
        JMP     _IntOver
@@lengthLEZero:
        XOR     EAX,EAX
end;
{$ENDIF !PUREPASCAL}


function _NewAnsiString(CharLength: LongInt; CodePage: Word): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := nil;
  if CharLength > 0 then
  begin
    // Alloc an extra null for strings with even length.  This has no actual
    // cost since the allocator will round up the request to an even size
    // anyway. All widestring allocations have even length, and need a double
    // null terminator.
    if CharLength >= MaxInt - SizeOf(StrRec) then _IntOver;
    GetMem(P, CharLength + SizeOf(StrRec) + 1 + ((CharLength + 1) and 1));
    Result := Pointer(PByte(P) + SizeOf(StrRec));
    P.length := CharLength;
    P.refcnt := 1;
    if CodePage = 0 then
      CodePage := Word(DefaultSystemCodePage);
    P.codePage := CodePage;
    P.elemSize := 1;
    PWideChar(Result)[CharLength div 2] := #0;  // length guaranteed >= 2
  end;
end;
{$ELSE}
asm
        { ->    EAX     length                  }
        { <-    EAX pointer to new string       }

        TEST    EAX,EAX
        JLE     @@lengthLEZero
        PUSH    EAX
        ADD     EAX,rOff+2                      // one or two nulls (Ansi/Wide)
        JO      @@overflow
        AND     EAX, not 1                      // round up to even length
        PUSH    EDX
        PUSH    EAX
        CALL    _GetMem
        POP     EDX                             // actual allocated length (>= 2)
        POP     ECX
        MOV     word ptr [EAX+EDX-2],0          // double null terminator
        ADD     EAX,rOff
        POP     EDX                             // requested string length
        MOV     [EAX-skew].StrRec.length,EDX
        MOV     [EAX-skew].StrRec.refCnt,1
        TEST    ECX,ECX
        JNE     @@NotDefault
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     ECX,[EAX].OFFSET DefaultSystemCodePage
        MOV     ECX, [ECX]
        POP     EAX
        POP     EBX
{$ELSE !PIC}
        MOV     ECX,DefaultSystemCodePage
{$ENDIF !PIC}
@@NotDefault:
        MOV     EDX,ECX
        MOV     word ptr [EAX-skew].StrRec.codePage,DX
        MOV     word ptr [EAX-skew].StrRec.elemSize,1
        RET
@@overflow:
        {$IFDEF ALIGN_STACK}
        POP     EAX
        {$ENDIF ALIGN_STACK}
        JMP     _IntOver
@@lengthLEZero:
        XOR     EAX,EAX
end;
{$ENDIF !PUREPASCAL}

{$IFDEF MSWINDOWS}
procedure WStrError;
{$IFDEF PUREPASCAL}
begin
  ErrorAt(byte(reOutOfMemory), ReturnAddress);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        MOV     AL,reOutOfMemory
        JMP     Error
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

function _NewWideString(CharLength: LongInt): Pointer;
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
   Result := _NewUnicodeString(CharLength);
end;
{$ELSE}
asm
        JMP     _NewUnicodeString
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
begin
  Result := nil;
  if CharLength <> 0 then
  begin
    Result := SysAllocStringLen(nil, CharLength);
    if Result = nil then
      WStrError;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     length                  }
        { <-    EAX     pointer to new string   }

        TEST    EAX,EAX
        JE      @@1
        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    0
        CALL    SysAllocStringLen
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF}

{$IFDEF CPUX64}
function _UStrClr(var S): Pointer;
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(PByte(S) - SizeOf(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
    begin
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
    end;
  end;
  Result := @S;
end;
{$ELSE !CPUX64}
procedure _UStrClr(var S);
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to str  }
        { <-    EAX     pointer to str  }

        MOV     EDX,[EAX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@done
        MOV     dword ptr [EAX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@done
        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        LEA     EAX,[EDX-skew]                  { if refCnt now zero, deallocate}
        CALL    _FreeMem
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF CPUX86}
{$ENDIF !CPUX64}

{$IFDEF CPUX64}
function _LStrClr(var S): Pointer;
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(PByte(S) - SizeOf(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
    begin
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
    end;
  end;
  Result := @S;
end;
{$ELSE !CPUX64}
{$IFDEF CPUX86}
procedure _LStrClr(var S);
asm
        { ->    EAX     pointer to str  }
        { <-    EAX     pointer to str  }

        MOV     EDX,[EAX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@done
        MOV     dword ptr [EAX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@done
        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        LEA     EAX,[EDX-skew]                  { if refCnt now zero, deallocate}
        CALL    _FreeMem
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF CPUX86}
{$ENDIF !CPUX64}

{$IFDEF POSIX}
{$IFDEF CPUX64}
function _WStrClr(var S): Pointer;
begin
  Result := _UStrClr(S);
end;
{$ELSE}
procedure _WStrClr(var S);
asm
        JMP     _UStrClr;
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF CPUX64}
function _WStrClr(var S): Pointer;
var
  P: Pointer;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(S);
    Pointer(S) := nil;
    SysFreeString(WideString(P));
  end;
  Result := @S;
end;
{$ELSE !CPUX64}
{$IFDEF CPUX86}
procedure _WStrClr(var S);
asm
        { ->    EAX     pointer to str  }
        { <-    EAX     pointer to Str  }

        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        MOV     DWORD PTR [EAX],0
        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    SysFreeString
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
@@1:
end;
{$ENDIF CPUX86}
{$ENDIF !CPUX64}
{$ENDIF MSWINDOWS}


procedure _UStrArrayClr(var StrArray; Count: Integer);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := @StrArray;
  while Count > 0 do
  begin
    _UStrClr(P^);
    Dec(Count);
    Inc(PByte(P), SizeOf(Pointer));
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to str      }
        {       EDX Count               }

        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX

@@loop:
        MOV     EDX,[EBX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@doneEntry
        MOV     dword ptr [EBX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@doneEntry
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@doneEntry
        LEA     EAX,[EDX-skew]                  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@doneEntry:
        ADD     EBX,4
        DEC     ESI
        JNE     @@loop

        POP     ESI
        POP     EBX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


procedure _LStrArrayClr(var StrArray; Count: Integer);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := @StrArray;
  while Count > 0 do
  begin
    _LStrClr(P^);
    Dec(Count);
    Inc(PByte(P), SizeOf(Pointer));
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to str      }
        {       EDX Count               }

        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX

@@loop:
        MOV     EDX,[EBX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@doneEntry
        MOV     dword ptr [EBX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@doneEntry
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@doneEntry
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@doneEntry:
        ADD     EBX,4
        DEC     ESI
        JNE     @@loop

        POP     ESI
        POP     EBX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


procedure _WStrArrayClr(var StrArray; Count: Integer);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrArrayClr(StrArray, Count);
end;
{$ELSE}
asm
        JMP     _UStrArrayClr
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  S: PPointer;
  P: Pointer;
begin
  S := PPointer(@StrArray);
  while Count > 0 do
  begin
    P := S^;
    if P <> nil then
    begin
      S^ := nil;
      SysFreeString(WideString(P));
    end;
    Inc(S);
    Dec(Count);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to str      }
        {       EDX cnt                 }

        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX
@@1:    MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@2
        MOV     DWORD PTR [EBX],0
        PUSH    EAX
        CALL    SysFreeString
@@2:    ADD     EBX,4
        DEC     ESI
        JNE     @@1
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}


function _UStrAddRef(Str: Pointer): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := Str;
  if Str <> nil then
  begin
    P := Pointer(PByte(Str) - SizeOf(StrRec));
    if P.refcnt >= 0 then
      InterlockedIncrement(P.refcnt);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     str     }
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[EAX-skew].StrRec.refCnt
        INC     EDX
        JLE     @@exit
   LOCK INC     [EAX-skew].StrRec.refCnt
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function _LStrAddRef(Str: Pointer): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := Str;
  if Str <> nil then
  begin
    P := Pointer(PByte(Str) - SizeOf(StrRec));
    if P.refcnt >= 0 then
      InterlockedIncrement(P.refcnt);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     str     }
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[EAX-skew].StrRec.refCnt
        INC     EDX
        JLE     @@exit
   LOCK INC     [EAX-skew].StrRec.refCnt
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


// Note: Windows version of WideString is single reference.
//       Only _WStrAddRef for Windows of *StrAddRef versions has
//       'var' parameter.
{$IFDEF POSIX}
function _WStrAddRef(Str: Pointer): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := _UStrAddRef(Str);
end;
{$ELSE}
asm
        JMP     _UStrAddRef
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
function _WStrAddRef(var Str: WideString): Pointer;
{$IFDEF PUREPASCAL}
var
  Len: LongInt;
begin
  Result := Pointer(Str);
  if Pointer(Str) <> nil then
  begin
    Len := PLongInt(PByte(Pointer(Str)) - Sizeof(LongInt))^ div 2;
    Result := Pointer(SysAllocStringLen(PWideChar(Pointer(Str)), Len));
    if Result = nil then
      WStrError;
    Pointer(Str) := Result;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to WideString   }
        { <-    EAX     str                     }
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        PUSH    EAX
        MOV     ECX,[EDX-4]
        SHR     ECX,1
        PUSH    ECX
        PUSH    EDX
        CALL    SysAllocStringLen
        POP     EDX
        TEST    EAX,EAX
        JE      WStrError
        MOV     [EDX],EAX
@@1:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}


{ ----------------------------------------------------- }
{       internal string conversion                      }
{ ----------------------------------------------------- }


function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer; CodePage: Integer): Integer;
begin
  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;
  Result := LocaleCharsFromUnicode(CodePage, 0, WCharSource, SrcChars, CharDest,
    DestBytes, nil, nil);
end;

function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer;
begin
  Result := CharFromWChar(CharDest, DestBytes, WCharSource, SrcChars, DefaultSystemCodePage);
end;

function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer; CodePage: Integer): Integer;
begin
  Result := UnicodeFromLocaleChars(CodePage, 0, CharSource, SrcBytes, WCharDest,
    DestChars);
end;


{ ----------------------------------------------------- }
{       basic string constructors                       }
{ ----------------------------------------------------- }

procedure _UStrFromPWCharLen(var Dest: UnicodeString; Source: PWideChar; CharLength: Integer);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := Pointer(Dest);
  if CharLength > 0 then
  begin
    Pointer(Dest) := _NewUnicodeString(CharLength);
    if Source <> nil then
      Move(Source^, Pointer(Dest)^, CharLength * SizeOf(WideChar));
  end
  else
    Pointer(Dest) := nil;
  _UStrClr(Temp);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest         }
        {       EDX     source                  }
        {       ECX     length in characters    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX // EBX := addr of Dest (result) in EBX
        MOV     ESI,EDX // ESI := source
        MOV     EDI,ECX // EDI := length

        { allocate new string }

        MOV     EAX,EDI // EAX := length

        CALL    _NewUnicodeString // EAX := new string (result)
        MOV     ECX,EDI // ECX := length
        MOV     EDI,EAX // EDI := result

        TEST    ESI,ESI // nil source?
        JE      @@noMove

        MOV     EDX,EAX // EDX := result (dest for Move)
        MOV     EAX,ESI // EAX := source (source for Move)
        SHL     ECX,1   // ECX := ECX * 2 (turn length into characters)
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromPWCharLen(var Dest: WideString; Source: PWideChar; CharLength: Integer);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrFromPWCharLen(UnicodeString(Pointer(Dest)), Source, CharLength);
end;
{$ELSE}
asm
        JMP     _UStrFromPWCharLen
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  if CharLength <= 0 then
    _WStrClr(Dest)
  else
  begin
    Temp := SysAllocStringLen(Source, CharLength);
    if Temp = nil then
      WStrError;
  //  Temp := InterlockedExchangePointer(Pointer(Dest), Temp);
  //  if Temp <> nil then
  //    SysFreeString(WideString(Temp));
    if Pointer(Dest) <> nil then
      SysFreeString(Dest);
    Pointer(Dest) := Temp;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to WideString (dest)      }
        {       EDX     Pointer to characters (source)    }
        {       ECX     number of characters  (not bytes) }
        TEST    ECX,ECX
        JE      _WStrClr

        PUSH    EAX

        PUSH    ECX
        PUSH    EDX
        CALL    SysAllocStringLen
        TEST    EAX,EAX
        POP     EDX
        JE      WStrError

        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        PUSH    [EDX].PWideChar
        MOV     [EDX],EAX

        CALL    SysFreeString
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

                                                                                                                                             
procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
{$IFDEF PUREPASCAL}
var
  P: PAnsiChar;
begin
  P := _NewAnsiString(Length, CodePage);
  if Source <> nil then
    Move(Source^, P^, Length);
  _LStrClr(Dest);
  Pointer(Dest) := P;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        { ->    EAX     pointer to dest }
        {       EDX     source          }
        {       ECX     length          }
        {       [ESP+0] caller EBP      }
        {       [ESP+4] return address  }
        {       [ESP+8] CodePage        }
{$IFDEF ALIGN_STACK}
        // EBP is already pushed on the stack
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        { allocate new string }

        MOV     EAX,EDI
        MOVZX   EDX,CodePage

        CALL    _NewAnsiString
        MOV     ECX,EDI
        MOV     EDI,EAX

        TEST    ESI,ESI
        JE      @@noMove

        MOV     EDX,EAX
        MOV     EAX,ESI
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure InternalUStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
begin
  if Length <= 0 then
  begin
    _UStrClr(Dest);
    Exit;
  end;
  if Length+1 < High(Buffer) then
  begin
    DestLen := WCharFromChar(Buffer, High(Buffer), Source, Length, CodePage);
    if DestLen > 0 then
    begin
      _UStrFromPWCharLen(Dest, @Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1);
  _UStrSetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := WCharFromChar(Pointer(Dest), DestLen, Source, Length, CodePage);
  if DestLen < 0 then
    DestLen := 0;
  _UStrSetLength(Dest, DestLen);
                                                                   
end;

procedure _UStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
begin
  InternalUStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;

procedure InternalWStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
begin
  if Length <= 0 then
  begin
    _WStrClr(Dest);
    Exit;
  end;
  if Length+1 < High(Buffer) then
  begin
    DestLen := WCharFromChar(Buffer, High(Buffer), Source, Length, CodePage);
    if DestLen > 0 then
    begin
      _WStrFromPWCharLen(Dest, @Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1);
  _WStrSetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := WCharFromChar(PWideChar(Pointer(Dest)), DestLen, Source, Length, CodePage);
  if DestLen < 0 then DestLen := 0;
  _WStrSetLength(Dest, DestLen);
                                                                   
end;

procedure _WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
begin
  InternalWStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;

procedure _LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
var
  DestLen: Integer;
begin
  if Length <= 0 then
  begin
    _LStrClr(Dest);
    Exit;
  end;

  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;

  DestLen := CharFromWChar(nil, 0, Source, Length, CodePage);
  SetLength(Dest, DestLen);
                                                              
  if DestLen > 0 then
  begin
    CharFromWChar(Pointer(Dest), DestLen, Source, Length, CodePage);
    PStrRec(PByte(Dest) - SizeOf(StrRec)).codePage := CodePage;
  end
  else
    _LStrClr(Dest);
end;


{ ----------------------------------------------------- }
{       Compiler helper for string assignment           }
{ ----------------------------------------------------- }

procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString); // globals (need copy)
{$IFDEF PUREPASCAL}
var
  S, D: Pointer;
  P: PStrRec;
  Len: LongInt;
begin
  S := Pointer(Source);
  if S <> nil then
  begin
    if __StringRefCnt(Source) < 0 then   // make copy of string literal
    begin
      Len := __StringLength(Source);
      S := _NewUnicodeString(Len);
      Move(Pointer(Source)^, S^, Len * SizeOf(WideChar));
    end else
    begin
      P := PStrRec(PByte(S) - SizeOf(StrRec));
      InterlockedIncrement(P.refCnt);
    end;
  end;
  D := Pointer(Dest);
  Pointer(Dest) := S;
  _UStrClr(D);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to dest   str       }
        { ->    EDX pointer to source str       }

        TEST    EDX,EDX                         { have a source? }
        JE      @@2                             { no -> jump     }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JG      @@1                             { literal string -> jump not taken }

        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX-skew].StrRec.length
        CALL    _NewUnicodeString
        MOV     EDX,EAX
        POP     EAX
        PUSH    EDX
        MOV     ECX,[EAX-skew].StrRec.length
        SHL     ECX,1                           { length to bytes for move }
        CALL    Move
        POP     EDX
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
        JMP     @@2

@@1:
   LOCK INC     [EDX-skew].StrRec.refCnt

@@2:    XCHG    EDX,[EAX]
        TEST    EDX,EDX
        JE      @@3
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@3
   LOCK DEC     [EDX-skew].StrRec.refCnt
        JNE     @@3
        LEA     EAX,[EDX-skew].StrRec.codePage
        {$IFDEF ALIGN_STACK}
        SUB     ESP,12
        {$ENDIF ALIGN_STACK}
        CALL    _FreeMem
        {$IFDEF ALIGN_STACK}
        ADD     ESP,12
        {$ENDIF ALIGN_STACK}
@@3:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrLAsg(var Dest: UnicodeString; const Source: UnicodeString); // locals
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  if Pointer(Source) <> nil then
    _UStrAddRef(Pointer(Source));
  P := Pointer(Dest);
  Pointer(Dest) := Pointer(Source);
  _UStrClr(P);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX
        JE      @@sourceDone

        { bump up the ref count of the source }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JLE     @@sourceDone                    { literal assignment -> jump taken }
   LOCK INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

        { we need to release whatever the dest is pointing to   }

        XCHG    EDX,[EAX]                       { fetch str                    }
        TEST    EDX,EDX                         { if nil, nothing to do        }
        JE      @@done
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
        DEC     ECX                             { if < 0: literal str          }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
        JNE     @@done
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        {$IFDEF ALIGN_STACK}
        SUB     ESP,12
        {$ENDIF ALIGN_STACK}
        CALL    _FreeMem
        {$IFDEF ALIGN_STACK}
        ADD     ESP,12
        {$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


procedure _WStrAsg(var Dest: WideString; const Source: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrAsg(UnicodeString(Pointer(Dest)), UnicodeString(Pointer(Source)));
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString }
        {       EDX     Pointer to data       }

        JMP     _UStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  if Pointer(Dest) <> Pointer(Source) then
  begin
    if Pointer(Source) = nil then
      _WStrClr(Dest)
    else
    begin
      Len := __StringLength(Source);
      if Len = 0 then
        _WStrClr(Dest)
      else
      begin
        if not SysReAllocStringLen(Dest, PWideChar(Pointer(Source)), Len) then
          WStrError;
      end;
    end;
  end;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString }
        {       EDX     Pointer to data       }
        CMP     [EAX],EDX
        JE      @@1
        TEST    EDX,EDX
        JE      _WStrClr
        MOV     ECX,[EDX-4]
        SHR     ECX,1
        JE      _WStrClr
        PUSH    ECX
        PUSH    EDX
        PUSH    EAX
        CALL    SysReAllocStringLen
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

procedure _WStrLAsg(var Dest: WideString; const Source: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrLAsg(UnicodeString(Pointer(Dest)), UnicodeString(Pointer(Source)));
end;
{$ELSE}
asm
        JMP     _UStrLAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
begin
  _WStrAsg(Dest, Source);
end;
{$ELSE}
asm
        JMP   _WStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}


{ 99.03.11
  This function is used when assigning to global variables.

  Literals are copied to prevent a situation where a dynamically
  allocated DLL or package assigns a literal to a variable and then
  is unloaded -- thereby causing the string memory (in the code
  segment of the DLL) to be removed -- and therefore leaving the
  global variable pointing to invalid memory.
}
procedure _LStrAsg(var Dest: AnsiString; const Source: AnsiString);
{$IFDEF PUREPASCAL}
var
  S, D: Pointer;
  P: PStrRec;
  Len: LongInt;
begin
  S := Pointer(Source);
  if S <> nil then
  begin
    if __StringRefCnt(S) < 0 then   // make copy of string literal
    begin
      Len := __StringLength(S);
      S := _NewAnsiString(Len, __StringCodePage(S));
      Move(PAnsiChar(Source)^, S^, Len);
    end else
    begin
      P := PStrRec(PByte(S) - SizeOf(StrRec));
      InterlockedIncrement(P.refCnt);
    end;
  end;
  D := Pointer(Dest);
  Pointer(Dest) := S;
  _LStrClr(D);
end;
{$ELSE}
asm
        { ->    EAX pointer to dest   str       }
        { ->    EDX pointer to source str       }

        TEST    EDX,EDX                         { have a source? }
        JE      @@2                             { no -> jump     }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JG      @@1                             { literal string -> jump not taken }

{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX-skew].StrRec.length
        MOVZX   EDX,[EDX-skew].StrRec.codePage
        CALL    _NewAnsiString
        MOV     EDX,EAX
        POP     EAX
        PUSH    EDX
        MOV     ECX,[EAX-skew].StrRec.length
        CALL    Move
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP,4
{$ENDIF ALIGN_STACK}
        JMP     @@2

@@1:
   LOCK INC     [EDX-skew].StrRec.refCnt

@@2:    XCHG    EDX,[EAX]
        TEST    EDX,EDX
        JE      @@3
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@3
   LOCK DEC     [EDX-skew].StrRec.refCnt
        JNE     @@3
        LEA     EAX,[EDX-skew].StrRec.codePage // Beginning of StrRec
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
@@3:
end;
{$ENDIF !PUREPASCAL}

procedure _LStrLAsg(var Dest: AnsiString; const Source: AnsiString);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := Pointer(Source);
  if P <> nil then
    _LStrAddRef(P);
  P := Pointer(Dest);
  Pointer(Dest) := Pointer(Source);
  _LStrClr(P);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX
        JE      @@sourceDone

        { bump up the ref count of the source }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JLE     @@sourceDone                    { literal assignment -> jump taken }
   LOCK INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

        { we need to release whatever the dest is pointing to   }

        XCHG    EDX,[EAX]                       { fetch str                    }
        TEST    EDX,EDX                         { if nil, nothing to do        }
        JE      @@done
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
        DEC     ECX                             { if < 0: literal str          }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
        JNE     @@done
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       string info utilities                           }
{ ----------------------------------------------------- }

function StringElementSize(const S: UnicodeString): Word; overload;
begin
  if S <> '' then
    Result := PWord(PByte(S) - 10)^                          // StrRec.elemSize
  else
    Result := SizeOf(WideChar);
end;

function StringElementSize(const S: RawByteString): Word; overload;
begin
  if S <> '' then
    Result := PWord(PByte(S) - 10)^                          // StrRec.elemSize
  else
    Result := SizeOf(AnsiChar);
end;

{$IFNDEF MSWINDOWS}
function StringElementSize(const S: WideString): Word; overload;
begin
  if S <> '' then
    Result := PWord(PByte(S) - 10)^                          // StrRec.elemSize
  else
    Result := SizeOf(WideChar);
end;
{$ENDIF !MSWINDOWS}

function StringCodePage(const S: UnicodeString): Word; overload;
begin
  if S <> '' then
    Result := PWord(PByte(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultUnicodeCodePage);
end;

function StringCodePage(const S: RawByteString): Word; overload;
begin
  if S <> '' then
    Result := PWord(PByte(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultSystemCodePage);
end;

{$IFNDEF MSWINDOWS}
function StringCodePage(const S: WideString): Word; overload;
begin
  if S <> '' then
    Result := PWord(PByte(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultUnicodeCodePage);
end;
{$ENDIF !MSWINDOWS}

function StringRefCount(const S: UnicodeString): Longint;
begin
  if Pointer(S) <> nil then           // PStrRec should be used here, but
    Result := PLongInt(PByte(S) - 8)^ // a private symbol can't be inlined
  else
    Result := 0;
end;

function StringRefCount(const S: RawByteString): Longint;
begin
  if Pointer(S) <> nil then           // PStrRec should be used here, but
    Result := PLongInt(PByte(S) - 8)^ // a private symbol can't be inlined
  else
    Result := 0;
end;

{$IFNDEF MSWINDOWS}
function StringRefCount(const S: WideString): Integer;
begin
  if Pointer(S) <> nil then           // PStrRec should be used here, but
    Result := PLongInt(PByte(S) - 8)^ // a private symbol can't be inlined
  else
    Result := 0;
end;
{$ENDIF !MSWINDOWS}


{ ----------------------------------------------------- }
{       Compiler helper for string length               }
{ ----------------------------------------------------- }

function _UStrLen(const S: UnicodeString): Integer;
{$IFDEF CPUX64}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(PByte(S) - 4)^; // a private symbol can't be inlined
end;
{$ELSE !CPUX64}
begin
  Result := IntPtr(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(PByte(Result - 4))^; // a private symbol can't be inlined
end;
{$ENDIF !CPUX64}

function _WStrLen(const S: WideString): Longint; inline;
{$IFDEF CPUX64}
begin
  Result := 0;
  if Pointer(S) <> nil then
    {$IFDEF MSWINDOWS}
    Result := PLongInt(PByte(S) - 4)^ shr 1;
    {$ELSE}
    Result := PLongInt(PByte(S) - 4)^;
    {$ENDIF}
end;
{$ELSE !CPUX64}
begin
  Result := IntPtr(S);
  if Result <> 0 then
    {$IFDEF MSWINDOWS}
    Result := PLongInt(PByte(Result - 4))^ shr 1;
    {$ELSE}
    Result := PLongInt(PByte(Result - 4))^;
    {$ENDIF}
end;
{$ENDIF !CPUX64}

function _LStrLen(const S: AnsiString): Longint;
{$IFDEF CPUX64}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(PByte(S) - 4)^; // a private symbol can't be inlined
end;
{$ELSE !CPUX64}
begin
  Result := IntPtr(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(PByte(Result - 4))^; // a private symbol can't be inlined
end;
{$ENDIF !CPUX64}

{$IFDEF PUREPASCAL}
function _PStrLen(const str: ShortString): Integer; inline;
begin
  Result := Byte(str[0]);
end;
{$ENDIF PUREPASCAL}

function _PCharLen(P: PAnsiChar): Longint;
{$IFNDEF LEGACY_PCHARLEN}
begin
  Result := 0;
  if P <> nil then
    while P[Result] <> #0 do
      Inc(Result);
end;
{$ELSE !LEGACY_PCHARLEN}
{$IFDEF CPUX86}
asm
        TEST    EAX,EAX
        JE      @@5
        PUSH    EAX
        XOR     ECX,ECX
@@0:    CMP     CL,[EAX+0]
        JE      @@4
        CMP     CL,[EAX+1]
        JE      @@3
        CMP     CL,[EAX+2]
        JE      @@2
        CMP     CL,[EAX+3]
        JE      @@1
        ADD     EAX,4
        JMP     @@0
@@1:    INC     EAX
@@2:    INC     EAX
@@3:    INC     EAX
@@4:    POP     ECX
        SUB     EAX,ECX
@@5:
end;
{$ENDIF CPUX86}
{$ENDIF !LEGACY_PCHARLEN}

function _PWCharLen(P: PWideChar): Longint;
{$IFNDEF LEGACY_PWCHARLEN}
begin
  Result := 0;
  if P <> nil then
    while P[Result] <> #0 do
      Inc(Result);
end;
{$ELSE !LEGACY_PWCHARLEN}
{$IFDEF CPUX86}
asm
        TEST    EAX,EAX
        JE      @@5
        PUSH    EAX
        XOR     ECX,ECX
@@0:    CMP     CX,[EAX+0]
        JE      @@4
        CMP     CX,[EAX+2]
        JE      @@3
        CMP     CX,[EAX+4]
        JE      @@2
        CMP     CX,[EAX+6]
        JE      @@1
        ADD     EAX,8
        JMP     @@0
@@1:    ADD     EAX,2
@@2:    ADD     EAX,2
@@3:    ADD     EAX,2
@@4:    POP     ECX
        SUB     EAX,ECX
        SHR     EAX,1
@@5:
end;
{$ENDIF CPUX86}
{$ENDIF !LEGACY_PWCHARLEN}


{ ----------------------------------------------------- }
{       internal UniqueString* support functions        }
{ ----------------------------------------------------- }

function InternalUniqueStringU(var Str: UnicodeString): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := Pointer(Str);
  if Result <> nil then
  begin
    Result := Pointer(Str);
    P := Pointer(PByte(Str) - SizeOf(StrRec));
    if P.refCnt <> 1 then
    begin
      Result := _NewUnicodeString(P.length);
      Move(PWideChar(Str)^, PWideChar(Result)^, P.length * SizeOf(WideChar));
      _UStrClr(Str);
      Pointer(Str) := Result;
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to unique copy      }
        MOV     EDX,[EAX]       // EDX := str
        TEST    EDX,EDX         // nil?
        JE      @@exit
        MOV     ECX,[EDX-skew].StrRec.refCnt // ECX := str.refCnt
        DEC     ECX             // refCnt = 1?
        JE      @@exit

        PUSH    EBX
        {$IFDEF ALIGN_STACK}
        SUB     ESP, 8
        {$ENDIF ALIGN_STACK}
        MOV     EBX,EAX         // EBX := @str
        MOV     EAX,[EDX-skew].StrRec.length
        CALL    _NewUnicodeString
        MOV     EDX,EAX         // EDX := newStr
        XCHG    EAX,[EBX]       // EAX := str ; @str^ := newStr
        {$IFDEF ALIGN_STACK}
        MOV     [ESP],EAX       // save str
        {$ELSE !ALIGN_STACK}
        PUSH    EAX             // save str
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.length
        SHL     ECX,1           // ECX := Length(str) * 2
        CALL    Move            // Move(str, newStr, Length(str) * 2)
        {$IFDEF ALIGN_STACK}
        MOV    EAX,[ESP]        // EAX := str
        {$ELSE !ALIGN_STACK}
        POP     EAX             // EAX := str
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.refCnt // ECX := str.refCnt
        DEC     ECX
        JL      @@skip          // Was already zero?
   LOCK DEC     [EAX-skew].StrRec.refCnt
        JNZ     @@skip
        LEA     EAX,[EAX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@skip:
        MOV     EDX,[EBX]       // EDX := @str^ (= newStr)
        {$IFDEF ALIGN_STACK}
        ADD     ESP, 8
        {$ENDIF ALIGN_STACK}
        POP     EBX
@@exit:
        MOV     EAX,EDX         // EAX := newStr
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function InternalUniqueStringA(var Str: AnsiString): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := Pointer(Str);
  if Result <> nil then
  begin
    Result := Pointer(Str);
    P := Pointer(PByte(Str) - sizeof(StrRec));
    if P.refCnt <> 1 then
    begin
      Result := _NewAnsiString(P.length, P.codePage);
      Move(PAnsiChar(Str)^, PAnsiChar(Result)^, P.length);
      _LStrClr(Str);
      Pointer(Str) := Result;
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to unique copy      }
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@exit
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JE      @@exit

        PUSH    EBX
        {$IFDEF ALIGN_STACK}
        SUB     ESP, 8
        {$ENDIF ALIGN_STACK}
        MOV     EBX,EAX
        MOV     EAX,[EDX-skew].StrRec.length
        MOVZX   EDX,[EDX-skew].StrRec.codePage
        CALL    _NewAnsiString
        MOV     EDX,EAX
        XCHG    EAX,[EBX]       // EAX := str ; @str^ := newStr
        {$IFDEF ALIGN_STACK}
        MOV     [ESP],EAX       // save str
        {$ELSE !ALIGN_STACK}
        PUSH    EAX
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.length
        CALL    Move
        {$IFDEF ALIGN_STACK}
        MOV     EAX,[ESP]       // EAX := str
        {$ELSE !ALIGN_STACK}
        POP     EAX
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@skip
   LOCK DEC     [EAX-skew].StrRec.refCnt
        JNZ     @@skip
        LEA     EAX,[EAX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@skip:
        MOV     EDX,[EBX]
        {$IFDEF ALIGN_STACK}
        ADD     ESP, 8
        {$ENDIF ALIGN_STACK}
        POP     EBX
@@exit:
        MOV     EAX,EDX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       Compiler helper for _UniqueString* functions    }
{ ----------------------------------------------------- }

function _UniqueStringU(var Str: UnicodeString): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := InternalUniqueStringU(Str);
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF !PUREPASCAL}

{$IFNDEF MSWINDOWS}
function _UniqueStringW(var Str: WideString): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := InternalUniqueStringU(UnicodeString(Pointer(Str)));
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

function _UniqueStringA(var Str: AnsiString): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := InternalUniqueStringA(Str);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        JMP     InternalUniqueStringA
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       UniqueString* functions                         }
{ ----------------------------------------------------- }

procedure UniqueString(var str: UnicodeString); overload;
{$IFDEF PUREPASCAL}
begin
  InternalUniqueStringU(str);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure UniqueString(var str: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  InternalUniqueStringU(UnicodeString(Pointer(str)));
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
begin
  // nothing to do - Windows WideStrings are always single reference
end;
{$ENDIF}

procedure UniqueString(var str: AnsiString);
{$IFDEF PUREPASCAL}
begin
  InternalUniqueStringA(str);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        JMP     InternalUniqueStringA
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


{ ------------------------------------------------------------- }
{       Compiler helper for comparing array of characters       }
{ ------------------------------------------------------------- }

{$IFNDEF CPUX86}
function _PStrCmp(const Left, Right: ShortString): Integer;
var
  Len, LLen, RLen: Cardinal;
  PLeft, PRight: PByte;
begin
  PLeft := PByte(@Left[0]);
  PRight := PByte(@Right[0]);
  LLen := PLeft^;
  RLen := PRight^;
  Inc(PLeft);
  Inc(PRight);
  if LLen > RLen then
    Len := RLen
  else
    Len := LLen;
  while Len >= SizeOf(LongWord) do
  begin
    if PLongWord(PLeft)^ <> PLongWord(PRight)^ then
      Break;
    if (Len < SizeOf(LongWord) * 2) or
       (PLongWord(PByte(PLeft) + SizeOf(LongWord))^ <>
       PLongWord(PByte(PRight) + SizeOf(LongWord))^) then
    begin
      Inc(PLeft, SizeOf(LongWord));
      Inc(PRight, SizeOf(LongWord));
      Dec(Len, SizeOf(LongWord));
      Break;
    end;
    Inc(PLeft, SizeOf(LongWord) * 2);
    Inc(PRight, SizeOf(LongWord) * 2);
    Dec(Len, SizeOf(LongWord) * 2);
  end;
  if Len = 0 then
    Exit(LLen - RLen);
  Result := PByte(PLeft)^ - PByte(PRight)^;
  if Result <> 0 then
    Exit;
  if Len = 1 then
    Exit(LLen - RLen);
  Result := PByte(PByte(PLeft) + 1)^ - PByte(PByte(PRight) + 1)^;
  if Result <> 0 then
    Exit;
  if Len = 2 then
    Exit(LLen - RLen);
  Result := PByte(PByte(PLeft) + 2)^ - PByte(PByte(PRight) + 2)^;
  if Result <> 0 then
    Exit;
  if Len = 3 then
    Exit(LLen - RLen);
  Result := PByte(PByte(PLeft) + 3)^ - PByte(PByte(PRight) + 3)^;
  if Result <> 0 then
    Exit;
  Exit(LLen - RLen);
end;
{$ELSE !CPUX86}
procedure       _PStrCmp;
asm
        {     ->EAX = Pointer to left string    }
        {       EDX = Pointer to right string   }
        {     <-ZF,CF = Result                  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[EDI]
        INC     ESI
        INC     EDI

        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }

@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,3
        JE      @@equal

        MOV     CL,[ESI]
        CMP     CL,[EDI]
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        MOV     CL,[ESI+1]
        CMP     CL,[EDI+1]
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        MOV     CL,[ESI+2]
        CMP     CL,[EDI+2]
        JNE     @@exit

@@equal:
        ADD     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     EDX
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _AStrCmp(const Left, Right: PAnsiChar; Len: NativeInt): Integer;
var
  P1, P2: PByte;
begin
  P1 := PByte(Left);
  P2 := PByte(Right);
  Result := 0;
  while Len >= SizeOf(LongWord) do begin
    if PLongWord(P1)^ <> PLongWord(P2)^ then break;
    if (Len < SizeOf(LongWord) * 2) or
       (PLongWord(PByte(P1) + SizeOf(LongWord))^ <>
        PLongWord(PByte(P2) + SizeOf(LongWord))^) then
    begin
      Inc(P1, SizeOf(LongWord));
      Inc(P2, SizeOf(LongWord));
      Dec(Len, SizeOf(LongWord));
      break;
    end;
    Inc(P1, SizeOf(LongWord) * 2);
    Inc(P2, SizeOf(LongWord) * 2);
    Dec(Len, SizeOf(LongWord) * 2);
  end;
  if Len = 0 then Exit;
  Result := PByte(P1)^ - PByte(P2)^;
  if Result <> 0 then Exit;
  if Len = 1 then Exit;
  Result := PByte(PByte(P1) + 1)^ - PByte(PByte(P2) + 1)^;
  if Result <> 0 then Exit;
  if Len = 2 then Exit;
  Result := PByte(PByte(P1) + 2)^ - PByte(PByte(P2) + 2)^;
  if Result <> 0 then Exit;
  if Len = 3 then Exit;
  Result := PByte(PByte(P1) + 3)^ - PByte(PByte(P2) + 3)^;
end;
{$ELSE CPUX86}
procedure       _AStrCmp;
asm
        {     ->EAX = Pointer to left string            }
        {       EDX = Pointer to right string           }
        {       ECX = Number of chars to compare        }
        {     <-ZF,CF = Result                          }

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX
        MOV     ESI,ECX
        SHR     ESI,2
        JE      @@cmpRest

@@longLoop:
        MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     ESI
        JE      @@cmpRestP4
        MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     EAX,8
        ADD     EDX,8
        DEC     ESI
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestp4:
        ADD     EAX,4
        ADD     EDX,4
@@cmpRest:
        POP     ESI
        AND     ESI,3
        JE      @@exit

        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@exit
        DEC     ESI
        JE      @@equal
        MOV     CL,[EAX+1]
        CMP     CL,[EDX+1]
        JNE     @@exit
        DEC     ESI
        JE      @@equal
        MOV     CL,[EAX+2]
        CMP     CL,[EDX+2]
        JNE     @@exit

@@equal:
        XOR     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     ESI
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _WStrLCmp(const Left, Right: PWideChar; Len: NativeInt): Integer;
var
  P1, P2: PWord;
begin
  P1 := PWord(Left);
  P2 := PWord(Right);
  Result := 0;
  while Len >= SizeOf(LongWord) div SizeOf(Word) do begin
    if PLongWord(P1)^ <> PLongWord(P2)^ then break;
    if (Len < SizeOf(LongWord) * 2 div SizeOf(Word)) or
       (PLongWord(PByte(P1) + SizeOf(LongWord))^ <>
        PLongWord(PByte(P2) + SizeOf(LongWord))^)
    then
    begin
      P1 := PWord(PByte(P1) + SizeOf(LongWord));
      P2 := PWord(PByte(P2) + SizeOf(LongWord));
      Dec(Len, SizeOf(LongWord) div SizeOf(Word));
      break;
    end;
    P1 := PWord(PByte(P1) + SizeOf(LongWord) * 2);
    P2 := PWord(PByte(P2) + SizeOf(LongWord) * 2);
    Dec(Len, SizeOf(LongWord) * 2 div SizeOf(Word));
  end;
  if Len = 0 then Exit;
  Result := PWord(P1)^ - PWord(P2)^;
  if Result <> 0 then Exit;
  if Len = 1 then Exit;
  Result := PWord(PByte(P1) + 2)^ - PWord(PByte(P2) + 2)^;
end;
{$ELSE CPUX64}
procedure       _WStrLCmp;
asm
        {     ->EAX = Pointer to left wide string       }
        {       EDX = Pointer to right wide string      }
        {       ECX = Number of chars to compare        }
        {     <-ZF,CF = Result                          }

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX
        MOV     ESI,ECX
        SHR     ESI,1
        JE      @@cmpRest

@@longLoop:
        MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     ESI
        JE      @@cmpRestP4
        MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     EAX,8
        ADD     EDX,8
        DEC     ESI
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestp4:
        ADD     EAX,4
        ADD     EDX,4
@@cmpRest:
        POP     ESI
        AND     ESI,1
        JE      @@exit

        MOV     CX,[EAX]
        CMP     CX,[EDX]
        JNE     @@exit

@@equal:
        XOR     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     ESI
        CMP     CX,BX
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CX,BX

@@exit:
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{ ----------------------------------------------------- }
{       Compiler helper for ShortString support         }
{ ----------------------------------------------------- }

procedure       _PStrCpy(Dest: PShortString; Source: PShortString);
begin
  Move(Source^, Dest^, Byte(Source^[0])+1);
end;

procedure       _PStrNCpy(Dest: PShortString; Source: PShortString; MaxLen: Byte);
begin
  if MaxLen > Byte(Source^[0]) then
    MaxLen := Byte(Source^[0]);
  Byte(Dest^[0]) := MaxLen;
  Move(Source^[1], Dest^[1], MaxLen);
end;

procedure _PStrCat(Dest: PShortString; const Src: ShortString);
{$IFDEF PUREPASCAL}
var
  DestLen, SrcLen, I: Integer;
begin
  DestLen := _PStrLen(Dest^);
  SrcLen := _PStrLen(Src);
  if DestLen + SrcLen > 255 then
    SrcLen := 255 - DestLen;
  Byte(Dest^[0]) := DestLen + SrcLen;
  for I := 1 to SrcLen do
    Dest^[DestLen + I] := Src[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{     ->EAX = Pointer to destination string     }
{       EDX = Pointer to source string  }

        PUSH    ESI
        PUSH    EDI

{       load dest len into EAX  }

        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     AL,[EDI]

{       load source address in ESI, source len in ECX   }

        MOV     ESI,EDX
        XOR     ECX,ECX
        MOV     CL,[ESI]
        INC     ESI

{       calculate final length in DL and store it in the destination    }

        MOV     DL,AL
        ADD     DL,CL
        JC      @@trunc

@@cont:
        MOV     [EDI],DL

{       calculate final dest address    }

        INC     EDI
        ADD     EDI,EAX

{       do the copy     }

        REP     MOVSB

                 

        POP     EDI
        POP     ESI
        RET

@@trunc:
        INC     DL      {       DL = #chars to truncate                 }
        SUB     CL,DL   {       CL = source len - #chars to truncate    }
        MOV     DL,255  {       DL = maximum length                     }
        JMP     @@cont
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _PStrNCat(Dest: PShortString; const Src: ShortString; Size:Integer);
{$IFDEF PUREPASCAL}
var
  DestLen, SrcLen, I: Integer;
begin
  DestLen := _PStrLen(Dest^);
  SrcLen := _PStrLen(Src);
  if DestLen + SrcLen > Size then
    SrcLen := Size - DestLen;
  Byte(Dest^[0]) := DestLen + SrcLen;
  for I := 1 to SrcLen do
    Dest^[DestLen + I] := Src[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{     ->EAX = Pointer to destination string                     }
{       EDX = Pointer to source string                          }
{       CL  = max length of result (allocated size of dest - 1) }

        PUSH    ESI
        PUSH    EDI

{       load dest len into EAX  }

        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     AL,[EDI]

{       load source address in ESI, source len in EDX   }

        MOV     ESI,EDX
        XOR     EDX,EDX
        MOV     DL,[ESI]
        INC     ESI

{       calculate final length in AL and store it in the destination    }

        ADD     AL,DL
        JC      @@trunc
        CMP     AL,CL
        JA      @@trunc

@@cont:
        MOV     ECX,EDX
        MOV     DL,[EDI]
        MOV     [EDI],AL

{       calculate final dest address    }

        INC     EDI
        ADD     EDI,EDX

{       do the copy     }

        REP     MOVSB

@@done:
        POP     EDI
        POP     ESI
        RET

@@trunc:
{       CL = maxlen     }

        MOV     AL,CL           { AL = final length = maxlen                    }
        SUB     CL,[EDI]        { CL = length to copy = maxlen - destlen        }
        JBE     @@done
        MOV     DL,CL
        JMP     @@cont
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function _Copy(const S: ShortString; Index, Count: Integer): ShortString;
{$IFDEF PUREPASCAL}
var
  Len, I: Integer;
begin
  Len := Byte(S[0]);
  if Len = 0 then
    Byte(Result[0]) := 0
  else
  begin
    if Index <= 0 then Index := 1
    else if Index > Len then Count := 0;
    Len := Len - Index + 1;
    if Count < 0 then Count := 0
    else if Count > Len then Count := Len;
    Byte(Result[0]) := Count;
    for I := 1 to Count do
      Result[I] := S[Index + I - 1];
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{     ->EAX     Source string                   }
{       EDX     index                           }
{       ECX     count                           }
{       [ESP+4] Pointer to result string        }
{       PUSH    EBP              }
{       MOV     EBP, ESP         }
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,[Result]

        XOR     EAX,EAX
        OR      AL,[ESI]
        JZ      @@srcEmpty

{       limit index to satisfy 1 <= index <= Length(src) }

        TEST    EDX,EDX
        JLE     @@smallInx
        CMP     EDX,EAX
        JG      @@bigInx
@@cont1:

{       limit count to satisfy 0 <= count <= Length(src) - index + 1    }

        SUB     EAX,EDX { calculate Length(src) - index + 1     }
        INC     EAX
        TEST    ECX,ECX
        JL      @@smallCount
        CMP     ECX,EAX
        JG      @@bigCount
@@cont2:

        ADD     ESI,EDX

        MOV     [EDI],CL
        INC     EDI
        REP     MOVSB
        JMP     @@exit

@@smallInx:
        MOV     EDX,1
        JMP     @@cont1
@@bigInx:
{       MOV     EDX,EAX
        JMP     @@cont1 }
@@smallCount:
        XOR     ECX,ECX
        JMP     @@cont2
@@bigCount:
        MOV     ECX,EAX
        JMP     @@cont2
@@srcEmpty:
        MOV     [EDI],AL
@@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFDEF CPUX64}
procedure _Delete(var s: ShortString; Index, Count: Integer);
var
  Len, TailLen: Integer;
begin
  Len := Byte(S[0]);
  if (Index >= 1) and (Index <= Len) then
  begin
    if Count > 0 then
    begin
      TailLen := Len - Index + 1;
      if Count > TailLen then Count := TailLen;
      Byte(S[0]) := Len - Count;
      Move(S[Index+Count], S[Index], TailLen - Count);
    end;
  end;
end;
{$ELSE CPUX86}
procedure _Delete(S: PShortString; Index, Count: Integer);
asm
{     ->EAX     Pointer to s    }
{       EDX     index           }
{       ECX     count           }

        PUSH    ESI
        PUSH    EDI

        MOV     EDI,EAX

        XOR     EAX,EAX
        MOV     AL,[EDI]

{       if index not in [1 .. Length(s)] do nothing     }

        TEST    EDX,EDX
        JLE     @@exit
        CMP     EDX,EAX
        JG      @@exit

{       limit count to [0 .. Length(s) - index + 1]     }

        TEST    ECX,ECX
        JLE     @@exit
        SUB     EAX,EDX         { calculate Length(s) - index + 1       }
        INC     EAX
        CMP     ECX,EAX
        JLE     @@1
        MOV     ECX,EAX
@@1:
        SUB     [EDI],CL        { reduce Length(s) by count                     }
        ADD     EDI,EDX         { point EDI to first char to be deleted }
        LEA     ESI,[EDI+ECX]   { point ESI to first char to be preserved       }
        SUB     EAX,ECX         { #chars = Length(s) - index + 1 - count        }
        MOV     ECX,EAX

        REP     MOVSB

@@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF CPUX86}

procedure _Insert(const Source: ShortString; var S: OpenString; Index: Integer);
{$IFDEF PUREPASCAL}
var
  Len: Integer;
  I: Integer;
  Len1, Len2, Len3: Integer;
begin
  Len := Byte(S[0]);
  if Index <= 0 then Index := 1
  else if Index > Len + 1 then Index := Len + 1;

  Len1 := Index - 1;
  Len2 := Byte(Source[0]);
  Len3 := Len - Len1;

  if Len1 + Len2 + Len3 > High(S) then
  begin
    if Len1 + Len2 > High(S) then
    begin
      Len3 := 0;
      Len2 := High(S) - Len1;
    end
    else
      Len3 := High(S) - Len1 - Len2;
  end;

  Byte(S[0]) := Len1 + Len2 + Len3;

  if Len2 > 0 then
  begin
    for I := Len3 downto 1 do
      S[Len1 + Len2 + I] := S[Len1 + I];
    for I := 1 to Len2 do
      S[Len1 + I] := Source[I];
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        {     ->EAX     Pointer to source string        }
        {       EDX     Pointer to destination string   }
        {       ECX     Length of destination string    }
        {       [ESP+4] Index                           }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    ECX
        MOV     ECX,Index
        SUB     ESP,512         { VAR buf: ARRAY [0..511] of Char       }

        MOV     EBX,EDX         { save pointer to s for later   }
        MOV     ESI,EDX

        XOR     EDX,EDX
        MOV     DL,[ESI]
        INC     ESI

{       limit index to [1 .. Length(s)+1]       }

        INC     EDX
        TEST    ECX,ECX
        JLE     @@smallInx
        CMP     ECX,EDX
        JG      @@bigInx
@@cont1:
        DEC     EDX             { EDX = Length(s)               }
                                { EAX = Pointer to src          }
                                { ESI = EBX = Pointer to s      }
                                { ECX = Index                   }

{       copy index-1 chars from s to buf        }

        MOV     EDI,ESP
        DEC     ECX
        SUB     EDX,ECX         { EDX = remaining length of s   }
        REP     MOVSB

{       copy Length(src) chars from src to buf  }

        XCHG    EAX,ESI         { save pointer into s, point ESI to src         }
        MOV     CL,[ESI]        { ECX = Length(src) (ECX was zero after rep)    }
        INC     ESI
        REP     MOVSB

{       copy remaining chars of s to buf        }

        MOV     ESI,EAX         { restore pointer into s                }
        MOV     ECX,EDX         { copy remaining bytes of s             }
        REP     MOVSB

{       calculate total chars in buf    }

        SUB     EDI,ESP         { length = bufPtr - buf         }
        MOV     ECX,[ESP+512]   { ECX = Min(length, destLength) }
{       MOV     ECX,[EBP-16]   }{ ECX = Min(length, destLength) }
        CMP     ECX,EDI
        JB      @@1
        MOV     ECX,EDI
@@1:
        MOV     EDI,EBX         { Point EDI to s                }
        MOV     ESI,ESP         { Point ESI to buf              }
        MOV     [EDI],CL        { Store length in s             }
        INC     EDI
        REP     MOVSB           { Copy length chars to s        }
        JMP     @@exit

@@smallInx:
        MOV     ECX,1
        JMP     @@cont1
@@bigInx:
        MOV     ECX,EDX
        JMP     @@cont1

@@exit:
        ADD     ESP,512+4
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

// Don't use var param here - var ShortString is an open string param, which
// passes the ptr in EAX and the string's declared buffer length in EDX.
// Compiler codegen expects only two params for this call - ptr and newlength
procedure       _SetLength(s: PShortString; newLength: Byte);
begin
  Byte(s^[0]) := newLength;   // should also fill new space
end;

procedure       _SetString(s: PShortString; buffer: PAnsiChar; len: Byte);
begin
  Byte(s^[0]) := len;
  if buffer <> nil then
    Move(buffer^, s^[1], len);
end;


{ ----------------------------------------------------- }
{       Compiler helper for AnsiString support          }
{ ----------------------------------------------------- }

procedure _LStrFromChar(var Dest: AnsiString; Source: AnsiChar; CodePage: Word);
{$IFDEF PUREPASCAL}
begin
  _LStrFromPCharLen(Dest, @Source, 1, CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest         }
        {       DL      source ANSI character   }
        {       ECX     CodePage                }
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EDX,ESP
        PUSH    ECX
        MOV     ECX,1
        CALL    _LStrFromPCharLen
{$IFDEF ALIGN_STACK}
        ADD     ESP,8
{$ELSE !ALIGN_STACK}
        POP     EDX
{$ENDIF !ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromWChar(var Dest: AnsiString; Source: WideChar; CodePage: Word);
{$IFDEF PUREPASCAL}
begin
  _LStrFromPWCharLen(Dest, @Source, 1, CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest         }
        {       DX      source wide character   }
        {       ECX     CodePage                }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EDX,ESP
        PUSH    ECX
        MOV     ECX,1
        CALL    _LStrFromPWCharLen
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromPChar(var Dest: AnsiString; Source: PAnsiChar; CodePage: Word);
{$IFDEF PUREPASCAL}
var
  Len: Integer;
  P: PAnsiChar;
begin
  Len := 0;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do Inc(P);
    Len := P - Source;
  end;
  _LStrFromPCharLen(Dest, Source, Len, CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest                 }
        {       EDX     pointer to ANSI characters      }
        {       ECX     CodePage                        }
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPCharLen
        ADD     ESP,8
{$ELSE ALIGN_STACK}
        JMP     _LStrFromPCharLen
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromPWChar(var Dest: AnsiString; Source: PWideChar; CodePage: Word);
{$IFDEF PUREPASCAL}
var
  Len: Integer;
  P: PWideChar;
begin
  Len := 0;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do Inc(P);
    Len := P - Source;
  end;
  _LStrFromPWCharLen(Dest, Source, Len, CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest                 }
        {       EDX     pointer to wide characters      }
        {       ECX     CodePage                        }
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPWCharLen
        ADD     ESP,8
{$ELSE ALIGN_STACK}
        JMP     _LStrFromPWCharLen
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromString(var Dest: AnsiString; const Source: ShortString; CodePage: Word);
{$IFDEF PUREPASCAL}
begin
  _LStrFromPCharLen(Dest, @Source[1], Byte(Source[0]), CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest         }
        {       EDX     pointer to ShortString  }
        {       ECX     CodePage                }
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPCharLen
        ADD     ESP,8
{$ELSE ALIGN_STACK}
        JMP     _LStrFromPCharLen
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromArray(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
{$IFDEF PUREPASCAL}
var
  P: PAnsiChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _LStrFromPCharLen(Dest, Source, Length, CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest                     }
        {       EDX     pointer to source ANSI characters   }
        {       ECX     number of characters of src         }
        {       [ESP+0] caller EBP                          }
        {       [ESP+4] return address                      }
        {       [ESP+8] CodePage                            }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        POP     EBP
        JMP     _LStrFromPCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromWArray(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
{$IFDEF PUREPASCAL}
var
  P: PWideChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _LStrFromPWCharLen(Dest, Source, Length, CodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest                     }
        {       EDX     pointer to source wide characters   }
        {       ECX     number of characters of src         }
        {       [ESP+0] caller EBP                          }
        {       [ESP+4] return address                      }
        {       [ESP+8] CodePage                            }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        POP     EBP
        JMP     _LStrFromPWCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrFromWStr(var Dest: AnsiString; const Source: WideString; CodePage: Word);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _LStrFromUStr(Dest, UnicodeString(Pointer(Source)), CodePage);
end;
{$ELSE}
asm
        JMP     _LStrFromUStr
end;
{$ENDIF !PUREPASCAL}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  Len := 0;
  if Pointer(Source) <> nil then
    Len := __StringLength(Source);
  _LStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), Len, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest                 }
        {       EDX     pointer to WideString data      }
        {       ECX     CodePage                        }
        PUSH    [ESP]
        MOV     [ESP+4],ECX
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-4]
        SHR     ECX,1
@@1:
        JMP     _LStrFromPWCharLen
end;
{$ENDIF !PUREPASCAL}
{$ENDIF}

procedure _LStrToString(Dest: PShortString; const Source: AnsiString; MaxLen: Integer);
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  if (Pointer(Source) = nil) or (__StringLength(Source) = 0) then
    Byte(Dest^[0]) := 0
  else
  begin
    Len := __StringLength(Source);
    if Len > MaxLen then Len := MaxLen;
    Byte(Dest^[0]) := Len;
    Move(Source[1], Dest^[1], Len);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to result   }
        {       EDX AnsiString s        }
        {       ECX length of result    }

        PUSH    EBX
        TEST    EDX,EDX
        JE      @@empty
        MOV     EBX,[EDX-skew].StrRec.length
        TEST    EBX,EBX
        JE      @@empty

        CMP     ECX,EBX
        JL      @@truncate
        MOV     ECX,EBX
@@truncate:
        MOV     [EAX],CL
        INC     EAX

        XCHG    EAX,EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
{$ENDIF ALIGN_STACK}
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP,8
{$ENDIF ALIGN_STACK}

        JMP     @@exit

@@empty:
        MOV     byte ptr [EAX],0

@@exit:
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

                                                         
procedure _LStrCat(var Dest: AnsiString; const Source: AnsiString);
{$IFDEF PUREPASCAL}
var
  L1, L2, Len: Cardinal;
  Temp: PAnsiChar;
begin
  if Pointer(Source) <> nil then
  begin
    if Pointer(Dest) = nil then
      _LStrAsg(Dest, Source)
    else
    begin
      L1 := __StringLength(Dest);
      L2 := __StringLength(Source);
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      Temp := @Dest[1];
      _LStrSetLength(Dest, Len, __StringCodePage(Dest));
      if Temp = @Source[1] then
        Temp := @Dest[1]
      else
        Temp := @Source[1];
      Move(Temp^, Dest[L1+1], L2);
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX
        JE      @@exit

        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      _LStrAsg

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX
        CMP     ESI,ECX
        MOV     EDI,[ECX-skew].StrRec.length

        MOV     EDX,[ESI-skew].StrRec.length
        ADD     EDX,EDI
        JO      @@lengthOverflow
        CMP     ESI,ECX
        JE      @@appendSelf

        MOVZX   ECX,[ECX-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EAX,ESI
        MOV     ECX,[ESI-skew].StrRec.length

@@appendStr:
        MOV     EDX,[EBX]
        ADD     EDX,EDI
        CALL    Move
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@appendSelf:
        MOVZX   ECX,[ECX-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EAX,[EBX]
        MOV     ECX,EDI
        JMP     @@appendStr

@@lengthOverflow:
{$IFDEF ALIGN_STACK}
        POP     EDI
        POP     ESI
        POP     EBX
{$ENDIF ALIGN_STACK}
        JMP     _IntOver

@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrCat3(var Dest:AnsiString; const Source1, Source2: AnsiString);
{$IFDEF PUREPASCAL}
var
  CodePage: Word;
  Temp: Pointer;
  L1, L2, Len: Cardinal;
begin
  if Pointer(Source1) = nil then
    _LStrAsg(Dest, Source2)
  else if Pointer(Source2) = nil then
    _LStrAsg(Dest, Source1)
  else
  begin
    if Pointer(Dest) = Pointer(Source1) then
      _LStrCat(Dest, Source2)
    else if Pointer(Dest) = Pointer(Source2) then
    begin
      L1 := __StringLength(Source1);
      L2 := __StringLength(Source2);
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      CodePage := __StringCodePage(Source2);
      Temp := _NewAnsiString(Len, CodePage);
      Move(PAnsiChar(Source1)^, PAnsiChar(Temp)[0], L1);
      Move(PAnsiChar(Source2)^, PAnsiChar(Temp)[L1], L2);
      _LStrClr(Dest);
      Pointer(Dest) := Temp;
    end
    else
    begin
      _LStrAsg(Dest, Source1);
      _LStrCat(Dest, Source2);
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        {     ->EAX = Pointer to dest   }
        {       EDX = source1           }
        {       ECX = source2           }

        TEST    EDX,EDX
        JE      @@assignSource2

        TEST    ECX,ECX
        JE      _LStrAsg

        CMP     EDX,[EAX]
        JE      @@appendToDest

        CMP     ECX,[EAX]
        JE      @@theHardWay

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    ECX
        CALL    _LStrAsg

        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     _LStrCat

@@theHardWay: // s(*EAX,ECX) := source1(EDX) + s(ECX)

        PUSH    EDI

        MOV     EDI,[EDX-skew].StrRec.length  // EDI := Length(source1) + Length(source2)
        ADD     EDI,[ECX-skew].StrRec.length
        JO      @@overflow

        PUSH    EBX
        PUSH    ESI
        PUSH    EAX
        MOV     EBX,EDX   // EBX : source1
        MOV     ESI,ECX   // ESI : source2

{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        MOV     EAX,EDI   // EAX := Final length
        MOVZX   EDX,[ESI-skew].StrRec.codePage // use source2's codepage
        CALL    _NewAnsiString
        MOV     EDI,EAX

        MOV     EDX,EDI  //Move(source1, temp[0], len(source1))
        MOV     EAX,EBX
        MOV     ECX,[EBX-skew].StrRec.length
        CALL    Move

        MOV     EDX,EDI  //Move(source2, temp[len(source1)], len(source2))
        MOV     EAX,ESI
        MOV     ECX,[ESI-skew].StrRec.length
        ADD     EDX,[EBX-skew].StrRec.length
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}

        POP     EAX
        MOV     EDX,EDI
        TEST    EDI,EDI
        JE      @@skip
        DEC     [EDI-skew].StrRec.refCnt    // EDI = local temp str - pass this reference to the caller
@@skip:
        CALL    _LStrAsg

        POP     ESI
        POP     EBX
        POP     EDI

        JMP     @@exit

@@assignSource2:
        MOV     EDX,ECX
        JMP     _LStrAsg

@@appendToDest:
        MOV     EDX,ECX
        JMP     _LStrCat

@@overflow:
        POP     EDI
        JMP     _IntOver

@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFDEF PUREPASCAL}
procedure InternalLStrCatN(var Dest: AnsiString; ArgCnt: Integer; Strs: PAnsiString);
var
  CodePage: Word;
  I, Start: Integer;
  Len, L: Integer;
  P: Pointer;
  NewDest: Pointer;
  Appending: Boolean;
begin
  CodePage := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      CodePage := __StringCodePage(P);
      if CodePage <> 0 then
        Break;
    end;
  end;
  Appending := False;
  Len := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      if P = Pointer(Dest) then
        Appending := (I = 0);
      Inc(Len, __StringLength(P));
      if Len < 0 then _IntOver;
    end;
  end;
  if Appending then
  begin
    // Dest is non-nil
    L := __StringLength(Dest);
    _LStrSetLength(Dest, Len, CodePage);
    NewDest := Pointer(Dest);
    Start := 1;
  end
  else
  begin
    NewDest := _NewAnsiString(Len, CodePage);
    Start := 0;
    L := 0;
  end;
  for I := Start to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      Move(P^, PAnsiChar(NewDest)[L], __StringLength(P));
      Inc(L, __StringLength(P));
    end;
  end;
  if not Appending then
  begin
    if Pointer(Dest) <> nil then
      _LStrClr(Dest);
    Pointer(Dest) := NewDest;
  end;
end;
{$ENDIF PUREPASCAL}

{$IFNDEF CPUX86}
procedure _LStrCatN(var Dest: AnsiString; ArgCnt: Integer; const Strs: AnsiString); varargs;
begin
  InternalLStrCatN(Dest, ArgCnt, @Strs);
end;
{$ELSE CPUX86}
procedure       _LStrCatN{var dest:AnsiString; argCnt: Integer; ...};
asm //StackAligned
        {     ->EAX = Pointer to dest           }
        {       EDX = number of args (>= 3)     }
        {       [EBP+8], [EBP+12], ... crgCnt AnsiString arguments, reverse order }

        PUSH    0                   // Stack - xxxxxxx8 - Save CodePage
        PUSH    EBX                 // Stack - xxxxxxx4
        PUSH    ESI                 // Stack - xxxxxxx0
        PUSH    EDI                 // Stack - xxxxxxxc
        PUSH    EDX                 // Stack - xxxxxxx8
        PUSH    EAX                 // Stack - xxxxxxx4
        PUSH    0                   // Stack - xxxxxxx0 - Local Temp
        MOV     EBX,EDX

        XOR     EDI,EDI
        MOV     ECX,[ESP+EDX*4+7*4] // first arg is furthest out
        TEST    ECX,ECX
        JZ      @@0
        MOVZX   ESI,[ECX-skew].StrRec.codePage
        MOV     [ESP+6*4],ESI      // Save the first arg's code page in case we need to copy
        CMP     [EAX],ECX          // is dest = first arg?
        JNE     @@0
        MOV     EDI,ECX            // EDI nonzero -> potential appendstr case
        MOV     EAX,[ECX-skew].StrRec.length
        DEC     EDX
        JMP     @@loop1
@@0:
        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+7*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-skew].StrRec.length
        JO      @@overflow

        CMP     [ESP+6*4],0        // Have we already found a valid codepage?
        JNZ     @@hascodepage
        MOVZX   ESI,[ECX-skew].StrRec.codePage // Save the first non-blank arg we find's codepage
        MOV     [ESP+6*4],ESI
@@hascodepage:

        CMP     EDI,ECX          // is dest an arg besides arg1?
        JNE     @@1
        XOR     EDI,EDI          // can't appendstr - dest is multiple args
@@1:
        DEC     EDX
        JNE     @@loop1

@@append:
        TEST    EDI,EDI          // dest is 1st and only 1st arg?
        JZ      @@copy
        MOV     EDX,EAX          // length into EDX
        MOV     EAX,[ESP + 4]    // ptr to str into EAX
        MOV     ESI,[EDI-skew].StrRec.Length  // save old size before realloc
        MOVZX   ECX,[EDI-Skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EDI,[ESP + 4]        // append other strs to dest
        MOV     EAX,[EDI]        // Stack - xxxxxxx0
        MOV     [ESP],EAX
        ADD     ESI,[EDI]        // ESI = end of old string
        DEC     EBX
        JMP     @@loop2

@@copy:
        MOV     EDX,[ESP+6*4]
        CALL    _NewAnsiString
        MOV     [ESP],EAX       // Stack - xxxxxxx0
        MOV     ESI,EAX

@@loop2:
        MOV     EAX,[ESP+EBX*4+7*4]
        MOV     EDX,ESI
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-skew].StrRec.length
        ADD     ESI,ECX
        CALL    Move
@@2:
        DEC     EBX
        JNE     @@loop2

        MOV     EDX,[ESP]
        MOV     EAX,[ESP+4]         // Stack - xxxxxxx0
        TEST    EDI,EDI
        JNZ     @@exit

        TEST    EDX,EDX
        JE      @@skip
        DEC     [EDX-skew].StrRec.refCnt   // EDX = local temp str
@@skip:
        CALL    _LStrAsg

@@exit:
        ADD     ESP,8           // Stack - xxxxxxx8 - Clean Local Temp & Saved EAX
        POP     EDX             // Stack - xxxxxxxc
        POP     EDI             // Stack - xxxxxxx0
        POP     ESI             // Stack - xxxxxxx4
        POP     EBX             // Stack - xxxxxxx8
        POP     EAX             // Stack - xxxxxxxc - Codepage Temp
        POP     EAX             // Stack - xxxxxxx0 - Return Address
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX // Unbalanced CALL/RET means clobbered branch prediction.
                    // Should fix codegen and have caller pop arguments, like cdecl.

@@overflow:
        JMP     _IntOver
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
// Returns 0 : Left = Right
//     minus : Left < Right
//      plus : Left > Right
function _LStrCmp(const Left, Right: AnsiString): Integer;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PAnsiChar;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - __StringLength(Right)
  else if Pointer(Right) = nil then
    Result := __StringLength(Left)
  else
  begin
    LLen := __StringLength(Left);
    RLen := __StringLength(Right);
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PAnsiChar(Left);
    RPtr := PAnsiChar(Right);
    while Len > 0 do
    begin
      Result := Ord(LPtr^) - Ord(RPtr^);
      if Result <> 0 then
        Exit;
      if Len = 1 then break;
      Result := Ord(LPtr[1]) - Ord(RPtr[1]);
      if Result <> 0 then
        Exit;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len, 2);
    end;
    Result := LLen - RLen;
  end;
end;
{$ELSE CPUX86}
{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _LStrCmp{left: AnsiString; right: AnsiString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       CF = 1 if Left < Right, CF = 0 otherwise
       ZF = 1 if Left = Right, ZF = 0 otherwise}

  CMP   EAX, EDX   // Do S1 and S2 point to the same string data?
  JE    @DoneNoPop

  TEST  EAX, EDX   // Is one of the two string pointers perhaps nil?
  JZ    @PossibleNilString
@BothStringsNonNil:
  {Compare the first character. (There has to be a trailing #0, so this
   comparison is safe). In "random" string compares this can save significant
   CPU time.}
  MOVZX ECX, BYTE PTR [EAX]
  SUB   CL, [EDX]
  JNE   @DoneNoPop

  PUSH  EBX             // Save ebx
  MOV   EBX, [EAX - 4]  // Set ebx = length(S1)
  SUB   EBX, [EDX - 4]  // Set ebx = length(S1) - length(S2)
  PUSH  EBX             // Save the length difference on the stack

  ADC   ECX, -1 // Set ecx = 0 if length(S1) <= length(S2), $ffffffff otherwise
  AND   ECX, EBX        // Set ecx = - min(length(S1), length(S2))

  SUB   ECX, [EAX - 4]
  {Adjust the pointers to be negative offset based}
  SUB   EAX, ECX
  SUB   EDX, ECX
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  MOV   EBX, [EAX + ECX]
  XOR   EBX, [EDX + ECX]
  JNZ   @Mismatch

  ADD   ECX, 4   // Next four bytes
  JS    @CompareLoop

@MatchUpToLength: // All characters match up to the compare length
  POP   EAX       // Restore the string length difference to eax
  ADD   EAX, EAX  // Set the flags according to the length difference
  POP   EBX       // Restore ebx and return
@DoneNoPop:
  RET

@Mismatch:
  BSF   EBX, EBX // Find the byte index that mismatched
  SHR   EBX, 3

  ADD   ECX, EBX //   Is the mismatch beyond the compare length?
  JNS   @MatchUpToLength

  MOV   AL, [EAX + ECX] // Compare the mismatched byte, setting the flags
  CMP   AL, [EDX + ECX]

  POP   EBX      // Pop the length difference, restore ebx and return
  POP   EBX
  RET
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  TEST  EAX, EAX
  JZ    @FirstStringNil
  TEST  EDX, EDX
  JNZ   @BothStringsNonNil

  CMP   [EAX - 4], EDX // S2 is nil - compare lengths of the strings
  RET
@FirstStringNil:
  CMP   EAX, [EDX - 4] // S1 is nil - compare lengths of the strings
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _LStrEqual(const Left, Right: AnsiString): Integer;
begin
  Result := _LStrCmp(Left, Right);
end;
{$ELSE CPUX86}
{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _LStrEqual{const Left, Right: AnsiString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       ZF = 1 if Left = Right, ZF = 0 otherwise}

        CMP   EAX, EDX  //Do Left and Right point to the same string data?
        JE    @CompareDoneNoPop

        TEST  EAX, EDX  //Is one of the two string pointers perhaps nil?
        JZ    @PossibleNilString
@BothStringsNonNil:
        MOV   ECX, [EAX - 4] //Compare lengths
        CMP   ECX, [EDX - 4]
        JNE   @CompareDoneNoPop

        PUSH  EBX       // Save ebx
        {Get pointers to the 4th last bytes in the strings}
        LEA   EDX, [EDX + ECX - 4]
        LEA   EBX, [EAX + ECX - 4]
        NEG   ECX       // Negate the loop counter
        {Compare the last four bytes. If the string length is less
         than four bytes then part of the length field is compared
         again - no harm done.}
        MOV   EAX, [EBX]
        CMP   EAX, [EDX]
        JNE   @CompareDonePop
@CompareLoop:
        ADD   ECX, 4 // Next four bytes
        JNS   @Match
        {Compare four bytes per iteration}
        MOV   EAX, [EBX + ECX]
        CMP   EAX, [EDX + ECX]
        JE    @CompareLoop
@CompareDonePop:
        POP   EBX
@CompareDoneNoPop:
        RET
@Match:
        XOR   EAX, EAX // Strings match - set the zero flag
        POP   EBX
        RET
@PossibleNilString:
        {There is a good probability that one of the strings are nil
         (but not both)}
        TEST  EAX, EAX
        JZ    @FirstStringNil
        TEST  EDX, EDX
        JNZ   @BothStringsNonNil
        {Right is nil - compare lengths of the strings}
        CMP   [EAX - 4], EDX
        RET
@FirstStringNil:
        {Left is nil - compare lengths of the strings}
        CMP   EAX, [EDX - 4]
        RET
end;
{$ENDIF CPUX86}

type
  PEmptyString = ^TEmptyString;
  TEmptyString = packed record
    Rec: StrRec;
    Nul: Word;
  end;

const
  // Using an initialized AnsiString to be sure of alignement
  // and so that it is read only.
  // Note: This const assumes a little endian machine.
  EmptyStringA: AnsiString =
{$IFDEF CPUX64}
  #$00#$00#$00#$00 +  // Padding, data is 16 byte aligned
{$ENDIF}
  #$FF#$FF +          // codePage := FFFF;
  #$01#$00 +          // elemSize := 1;
  #$FF#$FF#$FF#$FF +  // refCnt := -1;
  #$00#$00#$00#$00 +  // length := 0;
  #$00#$00;           // Data   := nil;

  EmptyStringW: AnsiString =
{$IFDEF CPUX64}
  #$00#$00#$00#$00 +  // Padding, data is 16 byte aligned
{$ENDIF}
  #$FF#$FF +          // codePage := FFFF;
  #$02#$00 +          // elemSize := 2;
  #$FF#$FF#$FF#$FF +  // refCnt := -1;
  #$00#$00#$00#$00 +  // length := 0;
  #$00#$00;           // Data   := nil;

function _LStrToPChar(const S: AnsiString): PAnsiChar;
begin
  if Pointer(s) = nil then
    Result := @(PEmptyString(@EmptyStringA[1])^.Nul)
  else
    Result := Pointer(s);
end;

function _LStrCopy(const S: AnsiString; Index, Count: Integer): AnsiString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then
    Index := 0
  else
  begin
    Dec(Index);
    if Index > L then
      Index := L;
  end;
  if Count < 0 then
    N := 0
  else
  begin
    N := L - Index;
    if N > Count then
      N := Count;
  end;
  if Pointer(S) = nil then
    _LStrClr(Result)
  else
    _LStrFromPCharLen(Result, PAnsiChar(Pointer(S)) + Index, N, __StringCodePage(S))
end;

procedure _LStrDelete(var S: AnsiString; Index, Count: Integer);
{$IFDEF PUREPASCAL}
var
  Len, TailLen: Integer;
begin
  InternalUniqueStringA(S);
  if Pointer(S) <> nil then
  begin
    Len := __StringLength(S);
    if (Index >= 1) and (Index <= Len) then
    begin
      if Count > 0 then
      begin
        TailLen := Len - Index + 1;
        if Count > TailLen then Count := TailLen;
        Move(S[Index + Count], S[Index], TailLen - Count);
        _LStrSetLength(S, Len - Count, __StringCodePage(S));
      end;
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        {     ->EAX     Pointer to s    }
        {       EDX     index           }
        {       ECX     count           }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        CALL    InternalUniqueStringA

        MOV     EDX,[EBX]
        TEST    EDX,EDX         { source already empty: nothing to do   }
        JE      @@exit

        MOV     ECX,[EDX-skew].StrRec.length

{       make index 0-based, if not in [0 .. Length(s)-1] do nothing     }

        DEC     ESI
        JL      @@exit
        CMP     ESI,ECX
        JGE     @@exit

{       limit count to [0 .. Length(s) - index] }

        TEST    EDI,EDI
        JLE     @@exit
        SUB     ECX,ESI         { ECX = Length(s) - index       }
        CMP     EDI,ECX
        JLE     @@1
        MOV     EDI,ECX
@@1:

{       move length - index - count characters from s+index+count to s+index }

        SUB     ECX,EDI         { ECX = Length(s) - index - count       }
        ADD     EDX,ESI         { EDX = s+index                 }
        LEA     EAX,[EDX+EDI]   { EAX = s+index+count           }
        CALL    Move

{       set length(s) to length(s) - count      }

        MOV     EDX,[EBX]
        MOV     EAX,EBX
        MOVZX   ECX,[EDX-skew].StrRec.codePage
        MOV     EDX,[EDX-skew].StrRec.length
        SUB     EDX,EDI
        CALL    _LStrSetLength

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrInsert(const Source: AnsiString; var S: AnsiString; Index: Integer);
{$IFDEF PUREPASCAL}
var
  CodePage: Word;
  Len: Integer;
  Len1, Len2, Len3: Integer;
begin
  if Pointer(Source) <> nil then
  begin
    CodePage := 0;
    Len := 0;
    if Pointer(S) <> nil then
    begin
      Len := __StringLength(S);
      CodePage := __StringCodePage(S);
    end;
    if Index <= 0 then Index := 1
    else if Index > Len + 1 then Index := Len + 1;
    Len1 := Index - 1;
    Len2 := __StringLength(Source);
    Len3 := Len - Len1;
    _LStrSetLength(S, Len + Len2, CodePage);
    if Len2 > 0 then
    begin
      Move(S[Index], S[Index + Len2], Len3);
      Move(Source[1], S[Index], Len2);
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     source string                   }
        {       EDX     pointer to destination string   }
        {       ECX     index                           }

        TEST    EAX,EAX
        JE      @@nothingToDo

{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

{       make index 0-based and limit to 0 <= index <= Length(s) }

        MOV     EDX,[EDX]
        PUSH    EDX
        TEST    EDX,EDX
        JE      @@sIsNull
        MOV     EDX,[EDX-skew].StrRec.length
@@sIsNull:
        DEC     EDI
        JGE     @@indexNotLow
        XOR     EDI,EDI
@@indexNotLow:
        CMP     EDI,EDX
        JLE     @@indexNotHigh
        MOV     EDI,EDX
@@indexNotHigh:

        MOV     EBP,[EBX-skew].StrRec.length

{       set length of result to length(source) + length(s)      }

        MOV     EAX,[ESI]
        TEST    EAX,EAX
        JNE     @@DestNotNull
        MOV     EAX,EBX
@@DestNotNull:
        MOVZX   ECX,[EAX-skew].StrRec.codePage
        MOV     EAX,ESI
        ADD     EDX,EBP
        JO      @@overflow
        CALL    _LStrSetLength
        POP     EAX

        CMP     EAX,EBX
        JNE     @@notInsertSelf
        MOV     EBX,[ESI]

@@notInsertSelf:

{       move length(s) - length(source) - index chars from s+index to s+index+length(source) }

        MOV     EAX,[ESI]                       { EAX = s       }
        LEA     EDX,[EDI+EBP]                   { EDX = index + length(source)  }
        MOV     ECX,[EAX-skew].StrRec.length
        SUB     ECX,EDX                         { ECX = length(s) - length(source) - index }
        ADD     EDX,EAX                         { EDX = s + index + length(source)      }
        ADD     EAX,EDI                         { EAX = s + index       }
        CALL    Move

{       copy length(source) chars from source to s+index        }

        MOV     EAX,EBX
        MOV     EDX,[ESI]
        MOV     ECX,EBP
        ADD     EDX,EDI
        CALL    Move

@@exit:
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF}
@@nothingToDo:
        RET

@@overflow:
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF}
        JMP     _IntOver
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _LStrSetLength(var Str: AnsiString; NewLength: Integer; CodePage: Word);
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
  Temp: Pointer;
  CopyCount: Integer;
begin
  if newLength <= 0 then
  begin
    _LStrClr(Str);
    Exit;
  end
  else
  begin
    if Pointer(Str) <> nil then
    begin
      if __StringRefCnt(Str) = 1 then
      begin
        P := Pointer(PByte(Str) - Sizeof(StrRec));
        _ReallocMem(Pointer(P), NewLength + 1 + SizeOf(StrRec));
        P.length := NewLength;
        Pointer(Str) := Pointer(PByte(P) + SizeOf(StrRec));
        PAnsiChar(Str)[NewLength] := #0;
        Exit;
      end;
    end;
    Temp := _NewAnsiString(NewLength, CodePage);
    if Pointer(Str) = nil then
    begin
      Pointer(Str) := Temp;
      Exit;
    end;
    CopyCount := __StringLength(Str);
    if CopyCount > NewLength then
      CopyCount := NewLength;
    Move(PAnsiChar(str)^, PAnsiChar(Temp)^, CopyCount);
    _LStrClr(Str);
    Pointer(Str) := Temp;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to str  }
        {       EDX     new length      }
        {       ECX     codePage        }

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8                       // Stack - xxxxxxx4
{$ENDIF ALIGN_STACK}
        PUSH    EBX                          // Stack - xxxxxxx0
        PUSH    ESI                          // Stack - xxxxxxxc
        PUSH    EDI                          // Stack - xxxxxxx8
        PUSH    EBP                          // Stack - xxxxxxx4
        PUSH    0                            // Stack - xxxxxxx0 - Local Temp
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EBP,ECX
        XOR     EDI,EDI

        TEST    EDX,EDX
        JLE     @@setString

        MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@copyString

        CMP     [EAX-skew].StrRec.refCnt,1
        JNE     @@copyString

        SUB     EAX,rOff
        ADD     EDX,rOff+1
        JO      @@overflow
        MOV     [ESP],EAX                    // Stack - xxxxxxx0
        MOV     EAX,ESP
        CALL    _ReallocMem
        MOV     EAX,[ESP]                    // Stack - xxxxxxx0
        ADD     EAX,rOff
        MOV     [EBX],EAX
        MOV     [EAX-skew].StrRec.length,ESI
        MOV     BYTE PTR [EAX+ESI],0
        JMP     @@exit

@@overflow:
        JMP     _IntOver

@@copyString:
        MOV     EAX,EDX
        MOV     EDX,EBP
        CALL    _NewAnsiString
        MOV     EDI,EAX

        MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@setString

        MOV     EDX,EDI
        MOV     ECX,[EAX-skew].StrRec.length
        CMP     ECX,ESI
        JL      @@moveString
        MOV     ECX,ESI

@@moveString:
        CALL    Move

@@setString:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

@@exit:
        POP     EDX                          // Stack - xxxxxxx4 - Local Temp
        POP     EBP                          // Stack - xxxxxxx8
        POP     EDI                          // Stack - xxxxxxxc
        POP     ESI                          // Stack - xxxxxxx0
        POP     EBX                          // Stack - xxxxxxx4
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8                       // Stack - xxxxxxxc
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


procedure _LStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word);
{$IFDEF PUREPASCAL}
begin
  if Pointer(Source) = nil then
    _LStrClr(Dest)
  else
    _LStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), __StringLength(Source), CodePage)
end;
{$ELSE}
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to UnicodeString data   }
        {       ECX destination codepage            }

{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE !ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF !ALIGN_STACK}
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-Skew].StrRec.length     // length in UnicodeString is widechar count
@@1:
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPWCharLen
        ADD     ESP,8
        RET
{$ELSE !ALIGN_STACK}
        JMP     _LStrFromPWCharLen
{$ENDIF !ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       Compiler helper for WideString support          }
{ ----------------------------------------------------- }

procedure UStrSet(var S: UnicodeString; P: PWideChar); forward;
procedure WStrSet(var S: WideString; P: PWideChar);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := InterlockedExchangePointer(Pointer(S), Pointer(P));
  if Temp <> nil then
    _WStrClr(Temp);
end;
{$ELSE}
{$IFDEF POSIX}
asm
        JMP     UStrSet
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
asm
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
        PUSH    EDX
        CALL    SysFreeString
@@1:
end;
{$ENDIF MSWINDOWS}
{$ENDIF !PUREPASCAL}

procedure _WStrFromChar(var Dest: WideString; Source: AnsiChar);
{$IFDEF PUREPASCAL}
begin
  InternalWStrFromPCharLen(Dest, @Source, 1, DefaultSystemCodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        PUSH    DefaultSystemCodePage
        CALL    InternalWStrFromPCharLen
        POP     EDX
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromChar
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromWChar(var Dest: WideString; Source: WideChar);
{$IFDEF PUREPASCAL}
begin
  _WStrFromPWCharLen(Dest, @Source, 1);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     character             (source) }
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _WStrFromPWCharLen
        POP     EDX
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP _UStrFromWChar
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromPChar(var Dest: WideString; Source: PAnsiChar);
{$IFDEF PUREPASCAL}
begin
  InternalWStrFromPCharLen(Dest, Source, _PCharLen(Source), DefaultSystemCodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)    }
        {       EDX     Pointer to character  (source)  }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _WStrFromPCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromPChar
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromPWChar(var Dest: WideString; Source: PWideChar);
{$IFDEF PUREPASCAL}
begin
  if Pointer(Source) = nil then
    _WStrClr(Dest)
  else
    _WStrFromPWCharLen(Dest, Source, _PWCharLen(Source));
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _WStrFromPWCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromPWChar
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromString(var Dest: WideString; const Source: ShortString);
{$IFDEF PUREPASCAL}
begin
  InternalWStrFromPCharLen(Dest, @Source[1], Byte(Source[0]), DefaultSystemCodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _WStrFromPCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP    _UStrFromString
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromArray(var Dest: WideString; Source: PAnsiChar; Length: Integer);
{$IFDEF PUREPASCAL}
var
  P: PAnsiChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  InternalWStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        {       ECX     Length of source characters    }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _WStrFromPCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromArray
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromWArray(var Dest: WideString; Source: PWideChar; Length: Integer);
{$IFDEF PUREPASCAL}
var
  P: PWideChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _WStrFromPWCharLen(Dest, Source, Length);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        {       ECX     Length of source characters    }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _WStrFromPWCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromWArray
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _WStrFromLStr(var Dest: WideString; const Source: AnsiString);
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  if Pointer(Source) = nil then
    _WStrClr(Dest)
  else
  begin
    Len := __StringLength(Source);
    InternalWStrFromPCharLen(Dest, PAnsiChar(Pointer(Source)), Len, __StringCodePage(Source))
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
{$IFDEF MSWINDOWS}
asm
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
// Inject the CodePage parameter onto the stack ahead of the return address
        MOVZX   ECX,[EDX-Skew].StrRec.codePage
        PUSH    [ESP]
        MOV     [ESP+4],ECX

        MOV     ECX,[EDX-Skew].StrRec.length
        JMP     InternalWStrFromPCharLen
@@1:    JMP     _WStrClr
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromLStr
end;
{$ENDIF}
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


procedure _WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else
  begin
    DestLen := CharFromWChar(Buffer, High(Buffer), PWideChar(Pointer(Source)), SourceLen);
    if DestLen < 0 then
      DestLen := 0
    else if DestLen > MaxLen then
      DestLen := MaxLen;
  end;
  Byte(Dest^[0]) := DestLen;
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;

function _WStrToPWChar(const S: WideString): PWideChar;
const
  EmptyString = '';
begin
  if Pointer(S) = nil then
    Result := EmptyString
  else
    Result := Pointer(S);
end;

procedure _WStrCat(var Dest: WideString; const Source: WideString);
var
  DestLen, SourceLen: Integer;
  NewStr: PWideChar;
begin
  SourceLen := Length(Source);
  if SourceLen <> 0 then
  begin
    DestLen := Length(Dest);
    NewStr := _NewWideString(DestLen + SourceLen);
    if DestLen > 0 then
      Move(Pointer(Dest)^, NewStr^, DestLen * sizeof(WideChar));
    Move(Pointer(Source)^, NewStr[DestLen], SourceLen * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end;
end;

procedure _WStrCat3(var Dest: WideString; const Source1, Source2: WideString);
var
  Source1Len, Source2Len: Integer;
  NewStr: PWideChar;
begin
  Source1Len := Length(Source1);
  Source2Len := Length(Source2);
  if (Source1Len <> 0) or (Source2Len <> 0) then
  begin
    NewStr := _NewWideString(Source1Len + Source2Len);
    Move(Pointer(Source1)^, Pointer(NewStr)^, Source1Len * sizeof(WideChar));
    Move(Pointer(Source2)^, NewStr[Source1Len], Source2Len * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end
  else
    _WStrClr(Dest);
end;

{$IFNDEF CPUX86}
{$IFDEF MSWINDOWS}
procedure InternalWStrCatN(var Dest: WideString; ArgCnt: Integer; Strs: PWideString);
var
  I: Integer;
  Len, L: Integer;
  P: Pointer;
  NewDest: Pointer;
begin
  Len := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      Inc(Len, __StringLength(WideString(P)));
      if Len < 0 then _IntOver;
    end;
  end;
  NewDest := _NewWideString(Len);
  L := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      Move(P^, PWideChar(NewDest)[L], __StringLength(WideString(P)) * SizeOf(WideChar));
      Inc(L, __StringLength(WideString(P)));
    end;
  end;
  WStrSet(Dest, NewDest);
end;
{$ENDIF MSWINDOWS}

procedure InternalUStrCatN(var Dest: UnicodeString; ArgCnt: Integer; Strs: PUnicodeString);
var
  I, Start: Integer;
  Len, L: Integer;
  P: Pointer;
  NewDest: Pointer;
  Appending: Boolean;
begin
  Appending := False;
  Len := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      if P = Pointer(Dest) then
        Appending := (I = 0);
      Inc(Len, __StringLength(P));
      if Len < 0 then _IntOver;
    end;
  end;
  if Appending then
  begin
    // Dest is non-nil
    L := __StringLength(Dest);
    _UStrSetLength(Dest, Len);
    NewDest := Pointer(Dest);
    Start := 1;
  end
  else
  begin
    NewDest := _NewUnicodeString(Len);
    Start := 0;
    L := 0;
  end;
  for I := Start to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      Move(P^, PWideChar(NewDest)[L], __StringLength(P) * SizeOf(WideChar));
      Inc(L, __StringLength(P));
    end;
  end;
  if not Appending then
  begin
    if Pointer(Dest) <> nil then
      _UStrClr(Dest);
    Pointer(Dest) := NewDest;
  end;
end;

procedure _WStrCatN(var Dest: WideString; ArgCnt: Integer; const Strs: WideString); varargs;
begin
  {$IFDEF MSWINDOWS}
  InternalWStrCatN(Dest, ArgCnt, @Strs);
  {$ELSE !MSWINDOWS}
  InternalUStrCatN(UnicodeString(Pointer(Dest)), ArgCnt, @UnicodeString(Pointer(Strs)));
  {$ENDIF !MSWINDOWS}
end;
{$ELSE CPUX86}
procedure _WStrCatN{var Dest: WideString; ArgCnt: Integer; ...};
{$IFDEF MSWINDOWS}
asm
        {     ->EAX = Pointer to dest }
        {       EDX = number of args (>= 3) }
        {       [ESP+4], [ESP+8], ... crgCnt WideString arguments }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDX
        PUSH    EAX
        MOV     EBX,EDX

        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+4*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-4]
@@1:
        DEC     EDX
        JNE     @@loop1

        SHR     EAX,1
        CALL    _NewWideString
        PUSH    EAX
        MOV     ESI,EAX

@@loop2:
        MOV     EAX,[ESP+EBX*4+5*4]
        MOV     EDX,ESI
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        ADD     ESI,ECX
        CALL    Move
@@2:
        DEC     EBX
        JNE     @@loop2

        POP     EDX
        POP     EAX
        CALL    WStrSet

        POP     EDX
        POP     ESI
        POP     EBX
        POP     EAX
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
asm
        {     ->EAX = Pointer to dest }
        {       EDX = number of args (>= 3) }
        {       [ESP+4], [ESP+8], ... crgCnt WideString arguments }

        JMP    _UStrCatN
end;
{$ENDIF POSIX}
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
// Returns 0 : Left = Right
//     minus : Left < Right
//      plus : Left > Right
function _WStrCmp(const Left, Right: WideString): Integer;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PWideChar;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - __StringLength(Right)
  else if Pointer(Right) = nil then
    Result := __StringLength(Left)
  else
  begin
    LLen := __StringLength(Left);
    RLen := __StringLength(Right);
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PWideChar(Left);
    RPtr := PWideChar(Right);
    while Len > 0 do
    begin
      Result := Ord(LPtr^) - Ord(RPtr^);
      if Result <> 0 then
        Exit;
      if Len = 1 then break;
      Result := Ord(LPtr[1]) - Ord(RPtr[1]);
      if Result <> 0 then
        Exit;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len, 2);
    end;
    Result := LLen - RLen;
  end;
end;
{$ELSE CPUX86}
procedure _WStrCmp{Left, Right: WideString};
{$IFDEF MSWINDOWS}
asm //StackAlignSafe
        {     ->EAX = Pointer to left string    }
        {       EDX = Pointer to right string   }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        CMP     EAX,EDX
        JE      @@exit

        TEST    ESI,ESI
        JE      @@str1null

        TEST    EDI,EDI
        JE      @@str2null

        MOV     EAX,[ESI-4]
        MOV     EDX,[EDI-4]

        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1 { len1 > len2 (unsigned)? }
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }
                        // edx := Min(len1, len2)
@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,2
        JE      @@equal

        MOV     CX,[ESI]
        MOV     BX,[EDI]
        CMP     CX,BX
        JNE     @@exit

@@equal:
        ADD     EAX,EAX
        JMP     @@exit

@@str1null:
        MOV     EDX,[EDI-4]
        SUB     EAX,EDX
        JMP     @@exit

@@str2null:
        MOV     EAX,[ESI-4]
        SUB     EAX,EDX
        JMP     @@exit

@@misMatch:
        POP     EDX
        CMP     CX,BX
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CX,BX

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
asm
        {     ->EAX = Pointer to left string    }
        {       EDX = Pointer to right string   }

        JMP     _UStrCmp
end;
{$ENDIF POSIX}
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _WStrEqual(const Left, Right: WideString): Integer;
begin
  Result := _WStrCmp(Left, Right);
end;
{$ELSE CPUX86}
procedure _WStrEqual{const Left, Right: WideString};
{$IFDEF MSWINDOWS}
asm
        JMP     _WStrCmp
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
asm
        JMP     _UStrCmp
end;
{$ENDIF POSIX}
{$ENDIF CPUX86}

function _WStrCopy(const S: WideString; Index, Count: Integer): WideString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then
    Index := 0
  else
  begin
    Dec(Index);
    if Index > L then
      Index := L;
  end;
  if Count < 0 then
    N := 0
  else
  begin
    N := L - Index;
    if N > Count then
      N := Count;
  end;
  _WStrFromPWCharLen(Result, PWideChar(Pointer(S)) + Index, N);
end;

procedure _WStrDelete(var S: WideString; Index, Count: Integer);
var
  L, N: Integer;
  NewStr: PWideChar;
begin
  L := Length(S);
  if (L > 0) and (Index >= 1) and (Index <= L) and (Count > 0) then
  begin
    Dec(Index);
    N := L - Index - Count;
    if N < 0 then N := 0;
    if (Index = 0) and (N = 0) then NewStr := nil else
    begin
      NewStr := _NewWideString(Index + N);
      if Index > 0 then
        Move(Pointer(S)^, NewStr^, Index * 2);
      if N > 0 then
        Move(PWideChar(Pointer(S))[L - N], NewStr[Index], N * 2);
    end;
    WStrSet(S, NewStr);
  end;
end;

procedure _WStrInsert(const Source: WideString; var Dest: WideString; Index: Integer);
var
  SourceLen, DestLen: Integer;
  NewStr: PWideChar;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then
  begin
    DestLen := Length(Dest);
    if Index < 1 then Index := 0 else
    begin
      Dec(Index);
      if Index > DestLen then Index := DestLen;
    end;
    NewStr := _NewWideString(DestLen + SourceLen);
    if Index > 0 then
      Move(Pointer(Dest)^, NewStr^, Index * 2);
    Move(Pointer(Source)^, NewStr[Index], SourceLen * 2);
    if Index < DestLen then
      Move(PWideChar(Pointer(Dest))[Index], NewStr[Index + SourceLen],
        (DestLen - Index) * 2);
    WStrSet(Dest, NewStr);
  end;
end;

procedure _WStrSetLength(var S: WideString; NewLength: Integer);
var
  NewStr: PWideChar;
  Count: Integer;
begin
  NewStr := nil;
  if NewLength > 0 then
  begin
    NewStr := _NewWideString(NewLength);
    Count := Length(S);
    if Count > 0 then
    begin
      if Count > NewLength then Count := NewLength;
      Move(Pointer(S)^, NewStr^, Count * SizeOf(WideChar));
    end;
  end;
  WStrSet(S, NewStr);
end;

procedure _WCharToString(Dest: PShortString; const Source: WideChar; MaxLen: Integer);
var
  DestLen: Integer;
  Buffer: array[0..255] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  DestLen := CharFromWChar(Buffer, High(Buffer), @Source, 1);
  if DestLen < 0 then
    DestLen := 0
  else if DestLen > MaxLen then
    DestLen := MaxLen;
  Byte(Dest^[0]) := DestLen;
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;


{ ----------------------------------------------------- }
{       Compiler helper for UnicodeString support       }
{ ----------------------------------------------------- }

{ UnicodeString helper functions }


function _UStrToPWChar(const S: UnicodeString): PWideChar;
begin
  if Pointer(S) = nil then
    Result := @(PEmptyString(@EmptyStringW[1])^.Nul)
  else
    Result := Pointer(S);
end;

procedure _UStrFromChar(var Dest: UnicodeString; Source: AnsiChar);
{$IFDEF PUREPASCAL}
begin
  _UStrFromPCharLen(Dest, @Source, 1);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX      // char on stack
        MOV     EDX,ESP  // addr of char on stack in EDX
        MOV     ECX,1
        CALL    _UStrFromPCharLen
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromWChar(var Dest: UnicodeString; Source: WideChar);
{$IFDEF PUREPASCAL}
begin
  _UStrFromPWCharLen(Dest, @Source, 1);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _UStrFromPWCharLen
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromPChar(var Dest: UnicodeString; Source: PAnsiChar);
{$IFDEF PUREPASCAL}
begin
  _UStrFromPCharLen(Dest, Source, _PCharLen(Source));
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _UStrFromPCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromPWChar(var Dest: UnicodeString; Source: PWideChar);
{$IFDEF PUREPASCAL}
begin
  _UStrFromPWCharLen(Dest, Source, _PWCharLen(Source));
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _UStrFromPWCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromArray(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
{$IFDEF PUREPASCAL}
var
  P: PAnsiChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _UStrFromPCharLen(Dest, Source, Length);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB      // find #0
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _UStrFromPCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromWArray(var Dest: UnicodeString; Source: PWideChar; Length: Integer);
{$IFDEF PUREPASCAL}
var
  P: PWideChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _UStrFromPWCharLen(Dest, Source, Length);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW     // find #$0000
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _UStrFromPWCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromLStr(var Dest: UnicodeString; const Source: AnsiString);
{$IFDEF PUREPASCAL}
begin
  if Pointer(Source) = nil then
    _UStrClr(Dest)
  else
    InternalUStrFromPCharLen(Dest, PAnsiChar(Pointer(Source)), __StringLength(Source), __StringCodePage(Source));
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOVZX   ECX,WORD PTR [EDX-Skew].StrRec.codePage
{$IFDEF ALIGN_STACK}
        PUSH    EBP         // Need a stack frame setup for the
        MOV     EBP, ESP    // unwinder in case there's an Exception
        SUB     ESP, 4
        PUSH    ECX
        MOV     ECX,[EDX-Skew].StrRec.length
        CALL    InternalUStrFromPCharLen
        ADD     ESP, 4
        POP     EBP
        RET
{$ELSE}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
        MOV     ECX,[EDX-Skew].StrRec.length
        JMP     InternalUStrFromPCharLen
{$ENDIF ALIGN_STACK}
@@1:    JMP     _UStrFromPCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrAsg(Dest, UnicodeString(Pointer(Source)));
end;
{$ELSE}
asm
        JMP     _UStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  Len := 0;
  if Pointer(Source) <> nil then
    Len := __StringLength(Source);
  _UStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), Len);
end;
{$ELSE}
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to WideString data      }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JZ      @@1            // nil source => zero length
        MOV     ECX,[EDX-4]
        SHR     ECX,1          // length in WideString is byte count
@@1:    JMP     _UStrFromPWCharLen
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _WStrAsg(Dest, WideString(Pointer(Source)));
end;
{$ELSE}
asm
        JMP     _WStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
begin
  if Pointer(Source) = nil then
    _WStrClr(Dest)
  else
    _WStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), __StringLength(Source))
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to UnicodeString data   }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JZ      @@1            // nil source => zero length
        MOV     ECX,[EDX-Skew].StrRec.length    // length in UnicodeString is widechar count
@@1:    JMP     _WStrFromPWCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}

procedure _UStrToString(Dest: PShortString; const Source: UnicodeString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else
  begin
    DestLen := CharFromWChar(Buffer, High(Buffer), PWideChar(Pointer(Source)), SourceLen);
    if DestLen < 0 then
      DestLen := 0
    else if DestLen > MaxLen then
      DestLen := MaxLen;
  end;
  Byte(Dest^[0]) := DestLen;
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;

procedure _UStrFromString(var Dest: UnicodeString; const Source: ShortString);
{$IFDEF PUREPASCAL}
begin
  InternalUStrFromPCharLen(Dest, @Source[1], Byte(Source[0]), DefaultSystemCodePage);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _UStrFromPCharLen
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrSetLength(var Str: UnicodeString; NewLength: Integer);
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
  Temp: Pointer;
  CopyCount: Integer;
begin
  if NewLength <= 0 then
    _UStrClr(Str)
  else
  begin
    if Pointer(Str) <> nil then
    begin
      if __StringRefCnt(Str) = 1 then
      begin
        P := Pointer(PByte(Str) - Sizeof(StrRec));
        if Cardinal(NewLength) >= Cardinal(- SizeOf(StrRec) - SizeOf(WideChar)) div 2 then
          _IntOver;
        _ReallocMem(Pointer(P), (NewLength + 1) * SizeOf(WideChar) + SizeOf(StrRec));
        P.length := NewLength;
        Pointer(Str) := Pointer(PByte(P) + SizeOf(StrRec));
        PWideChar(Str)[NewLength] := #0;
        Exit;
      end;
    end;
    Temp := _NewUnicodeString(NewLength);
    if Pointer(Str) <> nil then
    begin
      CopyCount := __StringLength(Str);
      if CopyCount > NewLength then
        CopyCount := NewLength;
      Move(PWideChar(Str)^, PWideChar(Temp)^, CopyCount * SizeOf(WideChar));
      _UStrClr(Str);
    end;
    Pointer(Str) := Temp;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        { ->    EAX     Pointer to S  }
        {       EDX     new length    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX       // EBX saves @S
        MOV     ESI,EDX       // ESI saves NewLength (chars)
        XOR     EDI,EDI       // EDI := 0; EDI is Temp (result)

        TEST    EDX,EDX       // NewLength <= 0?
        JLE     @@setString   // Assign S := Temp

        MOV     EAX,[EBX]     // EAX := S
        TEST    EAX,EAX       // nil?
        JE      @@copyString  // cannot reallocate (it's nil), so copy

        CMP     [EAX-skew].StrRec.refCnt,1 // !!! MT safety
        JNE     @@copyString  // not unique, so copy

        SUB     EAX,rOff      // Offset EAX "S" to start of memory block
        ADD     EDX,EDX       // Double length to get size
        JO      @@overflow
        ADD     EDX,rOff+2    // Add string rec size
        JO      @@overflow
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EAX           // Put S on stack
        MOV     EAX,ESP       // to pass by reference
        CALL    _ReallocMem
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        ADD     EAX,rOff      // Readjust
        MOV     [EBX],EAX     // Store
        MOV     [EAX-skew].StrRec.length,ESI
        MOV     WORD PTR [EAX+ESI*2],0 // Null terminate
        TEST    EDI,EDI       // Was a temp created?
        JZ      @@exit
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        MOV     EAX,ESP
        CALL    _LStrClr
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@overflow:
        JMP     _IntOver

@@copyString:
        MOV     EAX,EDX       // EAX := NewLength
        CALL    _NewUnicodeString
        MOV     EDI,EAX       // EDI "Temp" := new string

        MOV     EAX,[EBX]     // EAX := S, also Source of Move
        TEST    EAX,EAX       // nil?
        JE      @@setString   // Assign straight away

        MOV     EDX,EDI       // EDX := EDI "Temp", also Dest of Move
        MOV     ECX,[EAX-skew].StrRec.length  // ECX := Length(S), also Count of Move
        CMP     ECX,ESI       // ECX "Length(S)" <> NewLength
        JL      @@moveString  // ECX smaller => jump
        MOV     ECX,ESI       // ECX := ESI

@@moveString:
        SHL     ECX,1         // Length widechars to bytes translation
        CALL    Move          // Move ECX chars from EAX to EDX

@@setString:
        MOV     EAX,EBX       // EAX := @S
        CALL    _LStrClr      // clear S
        MOV     [EBX],EDI     // S := Temp

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrCat(var Dest: UnicodeString; const Source: UnicodeString);
{$IFDEF PUREPASCAL}
var
  L1, L2, Len: Cardinal;
  Temp: PWideChar;
begin
  if Pointer(Source) <> nil then
  begin
    if Pointer(Dest) = nil then
      _UStrAsg(Dest, Source)
    else
    begin
      L1 := __StringLength(Dest);
      L2 := __StringLength(Source);
      Len := L1 + L2;
      if (Len and $C0000000) <> 0 then _IntOver;
      Temp := @Dest[1];
      _UStrSetLength(Dest, Len);
      if Temp = @Source[1] then
        Temp := @Dest[1]
      else
        Temp := @Source[1];
      Move(Temp^, Dest[L1+1], L2 * SizeOf(WideChar));
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX       // Source empty, nop.
        JE      @@exit

        MOV     ECX,[EAX]     // ECX := Dest
        TEST    ECX,ECX       // Nil source => assignment
        JE      _UStrAsg

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX         // EBX := @Dest
        MOV     ESI,EDX         // ESI := Source
        MOV     EDI,[ECX-skew].StrRec.length  // EDI := Length(Dest)

        MOV     EDX,[ESI-skew].StrRec.length  // EDX := Length(Source)
        ADD     EDX,EDI         // EDX := (Length(Source) + Length(Dest)) * 2
        TEST    EDX,$C0000000
        JNZ     @@lengthOverflow
        CMP     ESI,ECX
        JE      @@appendSelf

        CALL    _UStrSetLength  // Set length of Dest
        MOV     EAX,ESI         // EAX := Source
        MOV     ECX,[ESI-skew].StrRec.length // ECX := Length(Source)

@@appendStr:
        MOV     EDX,[EBX]       // EDX := Dest
        SHL     EDI,1           // EDI to bytes (Length(Dest) * 2)
        ADD     EDX,EDI         // Offset EDX for destination of move
        SHL     ECX,1           // convert Length(Source) to bytes
        CALL    Move            // Move(Source, Dest + Length(Dest)*2, Length(Source)*2)
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@appendSelf:
        CALL    _UStrSetLength
        MOV     EAX,[EBX]
        MOV     ECX,EDI
        JMP     @@appendStr

@@lengthOverflow:
        JMP     _IntOver

@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _UStrCat3(var Dest: UnicodeString; const Source1, Source2: UnicodeString);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
  L1, L2, Len: Cardinal;
begin
  if Pointer(Source1) = nil then
    _UStrAsg(Dest, Source2)
  else if Pointer(Source2) = nil then
    _UStrAsg(Dest, Source1)
  else
  begin
    if Pointer(Dest) = Pointer(Source1) then
      _UStrCat(Dest, Source2)
    else if Pointer(Dest) = Pointer(Source2) then
    begin
      L1 := __StringLength(Source1);
      L2 := __StringLength(Source2);
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      Temp := _NewUnicodeString(Len);
      Move(PWideChar(Source1)^, PWideChar(Temp)[0], L1 * SizeOf(WideChar));
      Move(PWideChar(Source2)^, PWideChar(Temp)[L1], L2 * SizeOf(WideChar));
      _UStrClr(Dest);
      Pointer(Dest) := Temp;
    end
    else
    begin
      _UStrAsg(Dest, Source1);
      _UStrCat(Dest, Source2);
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        {     ->EAX = Pointer to dest   }
        {       EDX = source1           }
        {       ECX = source2           }

        TEST    EDX,EDX
        JE      @@assignSource2

        TEST    ECX,ECX
        JE      _UStrAsg

        CMP     EDX,[EAX]
        JE      @@appendToDest

        CMP     ECX,[EAX]
        JE      @@theHardWay

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    ECX
        CALL    _UStrAsg

        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     _UStrCat

@@theHardWay: // s(*EAX,ECX) := source1(EDX) + s(ECX)

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        MOV     EBX,EDX         // EBX := source1
        MOV     ESI,ECX         // ESI := source2
        PUSH    EAX             // Push(@s)

        MOV     EAX,[EBX-skew].StrRec.length
        ADD     EAX,[ESI-skew].StrRec.length

        TEST    EAX,$C0000000   // either of top two bits set => overflow for size
        JNZ     @@overflow
        CALL    _NewUnicodeString   // EAX := new string ("result")

        MOV     EDI,EAX         // EDI := result

        MOV     EDX,EDI         // EDX := result
        MOV     EAX,EBX         // EAX := source1
        MOV     ECX,[EBX-skew].StrRec.length // ECX := Length(source1)
        SHL     ECX,1           // double ECX for bytes
        CALL    Move            // Move(source1, result, Length(source1)*2)

        MOV     EAX,ESI         // EAX := source2
        MOV     ECX,[ESI-skew].StrRec.length // ECX := Length(source2)
        SHL     ECX,1           // ECX => to bytes
        MOV     EDX,[EBX-skew].StrRec.length // EDX := Length(source1)
        SHL     EDX,1           // EDX => to bytes
        ADD     EDX,EDI         // EDX := result + (num bytes in source1)
        CALL    Move            // Move(source2, result+offset, Length(source2)*2)

        POP     EAX             // EAX := Pop() // @s
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
        MOV     EDX,EDI         // EDX := result
        TEST    EDI,EDI
        JE      @@skip          // result is nil? => don't decrement
        DEC     [EDI-skew].StrRec.refCnt    // EDI = local temp str; _UStrAsg will addref, so ensure final refCnt = 1
@@skip:
        CALL    _UStrAsg

        POP     EDI
        POP     ESI
        POP     EBX

        JMP     @@exit

@@assignSource2:
        MOV     EDX,ECX
        JMP     _UStrAsg

@@appendToDest:
        MOV     EDX,ECX
        JMP     _UStrCat

@@overflow:
        JMP     _IntOver

@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFNDEF CPUX86}
procedure _UStrCatN(var Dest: UnicodeString; ArgCnt: Integer; const Strs: UnicodeString); varargs;
begin
  InternalUStrCatN(Dest, ArgCnt, @Strs);
end;
{$ELSE CPUX86}
procedure _UStrCatN{var dest:UnicodeString; argCnt: Integer; ...};
asm //StackAlignSafe
        {     ->EAX = Pointer to dest           }
        {       EDX = number of args (>= 3)     }
        {       [ESP+4], [ESP+8], ... argCnt UnicodeString arguments, reverse order }

        SUB     ESP,4                       // Stack - xxxxxxx8
        PUSH    EBX                         // Stack - xxxxxxx4
        PUSH    ESI                         // Stack - xxxxxxx0
        PUSH    EDI                         // Stack - xxxxxxxc
        PUSH    EDX                         // Stack - xxxxxxx8
        PUSH    EAX                         // Stack - xxxxxxx4
        PUSH    0                           // Stack - xxxxxxx0 - Local Temp
        MOV     EBX,EDX

        XOR     EDI,EDI
        MOV     ECX,[ESP+EDX*4+7*4] // first arg is furthest out
        TEST    ECX,ECX
        JZ      @@0
        CMP     [EAX],ECX          // is dest = first arg?
        JNE     @@0
        MOV     EDI,ECX            // EDI nonzero -> potential appendstr case
        MOV     EAX,[ECX-skew].StrRec.length  // EAX accumulates final length during @@loop1
        DEC     EDX
        JMP     @@loop1
@@0:
        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+7*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-skew].StrRec.length
        TEST    EAX,$C0000000
        JNZ     @@overflow
        CMP     EDI,ECX          // is dest an arg besides arg1?
        JNE     @@1
        XOR     EDI,EDI          // can't appendstr - dest is multiple args
@@1:
        DEC     EDX
        JNE     @@loop1

@@append:
        TEST    EDI,EDI          // dest is 1st and only 1st arg?
        JZ      @@copy
        MOV     EDX,EAX          // length into EDX
        MOV     EAX,[ESP + 4]    // ptr to str into EAX
        MOV     ESI,[EDI-skew].StrRec.Length  // save old size before realloc
        CALL    _UStrSetLength
        MOV     EDI,[ESP + 4]    // append other strs to dest
        MOV     EAX,[EDI]
        MOV     [ESP],EAX
        SHL     ESI,1            // Length to bytes for offset into string
        ADD     ESI,[EDI]        // ESI = end of old string
        DEC     EBX
        JMP     @@loop2

@@copy:
        CALL    _NewUnicodeString
        MOV     [ESP],EAX
        MOV     ESI,EAX

@@loop2:
        // Loop invariants:
        // - ESI is target of move, going through final dest
        // - EBX is arg index in stack to get arguments;
        //   last argument pushed last => lowest address => addresses decrease from first to last
        MOV     EAX,[ESP+EBX*4+7*4]     // EAX := argN
        MOV     EDX,ESI                 // EDX := dest
        TEST    EAX,EAX                 // argN nil?
        JE      @@2                     // => skip
        MOV     ECX,[EAX-skew].StrRec.length    // ECX := Length(argN)
        SHL     ECX,1                   // ECX to bytes
        ADD     ESI,ECX                 // ESI (running target of move) += ECX
        CALL    Move                    // Move(argN, dest, Length(argN) * 2)
@@2:
        DEC     EBX
        JNE     @@loop2

        MOV     EDX,[ESP]
        MOV     EAX,[ESP + 4]
        TEST    EDI,EDI
        JNZ     @@exit

        TEST    EDX,EDX
        JE      @@skip
        DEC     [EDX-skew].StrRec.refCnt   // EDX = local temp str
@@skip:
        CALL    _UStrAsg

@@exit:
        ADD     ESP,8                     // Stack - xxxxxxx8 - Clean Local Temp & Saved EAX
        POP     EDX                       // Stack - xxxxxxxc
        POP     EDI                       // Stack - xxxxxxx0
        POP     ESI                       // Stack - xxxxxxx4
        POP     EBX                       // Stack - xxxxxxx8
        POP     EAX                       // Stack - xxxxxxxc
        POP     EAX // ret address from CALL Stack - xxxxxxx0
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX // Unbalanced CALL/RET means clobbered branch prediction.
                    // Should fix codegen and have caller pop arguments, like cdecl.

@@overflow:
        JMP     _IntOver
end;
{$ENDIF CPUX86}

{$IF defined(MACOS) and defined(PUREPASCAL)}
//  StrRec = packed record
//    codePage: Word;
//    elemSize: Word;
//    refCnt: Longint;
//    length: Longint;
//  end;
function strEltSize(const s: UnicodeString): Integer; inline;
begin
   Result := PStrRec(Pointer(PByte(s) - SizeOf(StrRec)))^.elemSize;
end;

function strRawLength(const s: UnicodeString): Longint; inline;
begin
//   Result := PStrRec(Pointer(PByte(s) - SizeOf(StrRec)))^.length;
   Result := PLongint(Pointer(PByte(s) - SizeOf(Longint)))^;
end;

function StrRecIsNil(const p: PStrRec): Boolean; inline;
begin
   Result := IntPtr(p) = NativeInt(-SizeOf(StrRec));
end;

function _UStrCmpAsymmetric(const left, right: UnicodeString): Integer;
begin
   // this case should only arise in the face of C++ code
   Error(rePlatformNotImplemented);
   Result := 0;
end;

function _UStrCmpInternal(const left, right: UnicodeString): Integer;
var
   LengthDelta: Integer;
   leftR: PStrRec;
   rightR: PStrRec;
   compareLength: Integer;
begin
   if Pointer(left) = Pointer(right) then
      Exit(0);

   leftR := PStrRec(PByte(Pointer(left)) - SizeOf(StrRec));
   rightR := PStrRec(PByte(Pointer(right)) - SizeOf(StrRec));
   if StrRecIsNil(leftR) then
   begin
//      if StrRecIsNil(rightR) then
//         Exit(0)
//      else
//         Exit(leftR^.length);
      Exit(1);
   end
   else if StrRecIsNil(rightR) then
//      Exit(-leftR^.length)
      Exit(-1)
   else
   begin
      // both strings are present
      LengthDelta := leftR^.length - rightR^.length;
      if LengthDelta <> 0 then
         Exit(LengthDelta)
      else
      begin
         // both strings same length - now the awful payload escape
          // plain old unicode strings
          compareLength := leftR^.length;
          if compareLength > rightR^.length then
             compareLength := rightR^.length;
          Result := memcmp(Pointer(PByte(leftR) + SizeOf(StrRec))^,
                           Pointer(PByte(rightR) + SizeOf(StrRec))^,
                           compareLength);
      end;
    end;
end;
{$IFEND MACOS and PUREPASCAL}

{$IFNDEF CPUX86}
function _UStrCmp(const Left, Right: UnicodeString): Integer;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PWideChar;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - __StringLength(Right)
  else if Pointer(Right) = nil then
    Result := __StringLength(Left)
  else
  begin
    LLen := __StringLength(Left);
    RLen := __StringLength(Right);
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PWideChar(Left);
    RPtr := PWideChar(Right);
    while Len > 0 do
    begin
      Result := Ord(LPtr^) - Ord(RPtr^);
      if Result <> 0 then
        Exit;
      if Len = 1 then break;
      Result := Ord(LPtr[1]) - Ord(RPtr[1]);
      if Result <> 0 then
        Exit;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len, 2);
    end;
    Result := LLen - RLen;
  end;
end;
{$ELSE CPUX86}
{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _UStrCmp{const Left, Right: UnicodeString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       CF = 1 if Left < Right, CF = 0 otherwise
       ZF = 1 if Left = Right, ZF = 0 otherwise}

        CMP     EAX, EDX // Do Left and Right point to the same string data?
        JE      @DoneNoPop

        TEST    EAX, EDX // Is one of the two string pointers perhaps nil?
        JZ      @PossibleNilString
@BothStringsNonNil:
  {Compare the first two characters. (There has to be a trailing #0, and non-nil
   UnicodeStrings must contain at least one other character so this comparison
   is safe). In "random" string compares this can save significant CPU time.}
        MOV     ECX, [EAX]
        CMP     ECX, [EDX]
        JNE @InitialMismatch

        PUSH    EBX             // Save ebx
        MOV     EBX, [EAX - 4]  // set ebx = Length(Left)
        XOR     ECX, ECX
        SUB     EBX, [EDX - 4]  // set ebx = Length(Left) - Length(Right)
        PUSH    EBX             // Save the length difference on the stack

        ADC     ECX, -1  // set ecx = 0 if Length(Left) < Length(Right), $ffffffff otherwise
        AND     ECX, EBX // set ecx = - min(length(Left), Length(Right))
        SUB     ECX, [EAX - 4]

        ADD     ECX, ECX        // Two bytes per character

        SUB     EAX, ECX        // Adjust the pointers to be
        SUB     EDX, ECX        // negative offset based

@CompareLoop:
        ADD     ECX, 4          // Next four bytes
        JNS     @MatchUpToLength
        {Compare four bytes (two characters) per cycle. This compare may include the
         trailing #0 for uneven string lengths, in which case no harm is done.}
        MOV     EBX, [EAX + ECX]
        CMP     EBX, [EDX + ECX]
        JE      @CompareLoop
        {Found a mismatch: Swap the two characters into the correct order}
        MOV     EDX, [EDX + ECX]
        ROR     EBX, 16
        ROR     EDX, 16

        CMP     EBX, EDX  // Compare the characters again, setting the flags
        POP     EBX       // Pop the length difference, restore ebx and return
        POP     EBX
        RET
        {All characters match up to the compare length}
@MatchUpToLength:
        POP     EAX       // Restore the string length difference to eax

        ADD     EAX, EAX  // Set the flags according to the length difference
        POP     EBX       // Restore ebx and return
@DoneNoPop:
        RET
@InitialMismatch:
        MOV     EDX, [EDX]  // Swap the two characters into the correct order
        ROR     ECX, 16
        ROR     EDX, 16
        CMP     ECX, EDX    // Compare them again so the flags are set correctly
        RET
@PossibleNilString:
        {There is a good probability that one of the strings are nil (but not both)}
        TEST    EAX, EAX
        JZ      @StringNil
        TEST    EDX, EDX
        JNZ     @BothStringsNonNil
@StringNil:
        CMP     EAX, EDX  // One of the strings are nil, so compare pointers
        RET
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
function _UStrEqual(const Left, Right: UnicodeString): Integer;
begin
  Result := _UStrCmp(Left, Right);
end;
{$ELSE !CPUX86}
{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _UStrEqual{const Left, Right: UnicodeString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1],
   On exit:
     ZF = 1 if Left = Right
     ZF = 0 if Left <> Right}

        CMP     EAX, EDX  // Same string?
        JE      @DoneNoPop

        TEST    EAX, EDX  // Any of the two possibly nil?
        JZ      @PossibleNilString
@BothStringsNonNil:

        MOV     ECX, [EAX - skew].StrRec.length // Get the string length
        CMP     ECX, [EDX - skew].StrRec.length // Are the string lengths the same?
        JNE     @DoneNoPop
        ADD     ECX, ECX    // Two bytes per character
        ADD     EAX, ECX    // Point eax and edx to just past the last character
        ADD     EDX, ECX
        NEG     ECX         // Make the counter negative based

        PUSH    EBX         // Save ebx
@CompareLoop:
        MOV     EBX, [EAX + ECX] // Compare four bytes per iteration
        CMP     EBX, [EDX + ECX]
        JNE     @Mismatch

        ADD     ECX, 4      // Next two characters
        JS      @CompareLoop
        XOR     EAX, EAX    // Match: Set the ZF
@Mismatch:
        POP     EBX         // Restore ebx

        RET
@PossibleNilString:
        {There is a good probability that one of the strings are nil (but not both)}
        TEST    EAX, EAX
        JZ      @StringNil
        TEST    EDX, EDX
        JNZ     @BothStringsNonNil
@StringNil:
        {One of the strings are nil. Clear the ZF.}
        CMP     EAX, EDX
@DoneNoPop:
end;
{$ENDIF CPUX86}

function _UStrCopy(const S: UnicodeString; Index, Count: Integer): UnicodeString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then
    Index := 0
  else
  begin
    Dec(Index);
    if Index > L then
      Index := L;
  end;
  if Count < 0 then
    N := 0
  else
  begin
    N := L - Index;
    if N > Count then
      N := Count;
  end;
    _UStrFromPWCharLen(Result, PWideChar(Pointer(S)) + Index, N)
end;

                                                 
procedure UStrSet(var S: UnicodeString; P: PWideChar);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := InterlockedExchangePointer(Pointer(S), Pointer(P));
  if Temp <> nil then
    _UStrClr(Temp);
end;
{$ELSE}
asm
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EAX,ESP
        CALL    _UStrClr
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
@@1:
end;
{$ENDIF !PUREPASCAL}

procedure _UStrDelete(var S: UnicodeString; Index, Count: Integer);
var
  L, N: Integer;
begin
  InternalUniqueStringU(S);
  L := Length(S);
  if (Index >= 1) and (Index <= L) and (Count > 0) then
  begin
    Dec(Index);
    N := L - Index - Count;
    if N < 0 then
      N := 0;
    Move(PWideChar(Pointer(S))[L - N], PWideChar(Pointer(S))[Index], N * 2);
    SetLength(S, Index + N);
  end;
end;

procedure _UStrInsert(const Source: UnicodeString; var Dest: UnicodeString; Index: Integer);
var
  SourceLen, DestLen, NewLen: Integer;
  SelfInsert: Boolean;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then
  begin
    DestLen := Length(Dest);
    if Index < 1 then Index := 0 else
    begin
      Dec(Index);
      if Index > DestLen then Index := DestLen;
    end;
    SelfInsert := (Pointer(Source) = Pointer(Dest));
    NewLen := DestLen + SourceLen;
    if NewLen < 0 then   // overflow check
      _IntOver;
    SetLength(Dest, NewLen);
    if Index < DestLen then
      Move(PWideChar(Pointer(Dest))[Index], PWideChar(Pointer(Dest))[Index + SourceLen],
        (DestLen - Index) * 2);
    if SelfInsert then
      Move(Pointer(Dest)^, PWideChar(Pointer(Dest))[Index], SourceLen * 2)
    else
      Move(Pointer(Source)^, PWideChar(Pointer(Dest))[Index], SourceLen * 2);
  end;
end;

{ ----------------------------------------------------- }
{       string utilities                                }
{ ----------------------------------------------------- }

function Pos(const SubStr, Str: ShortString): Integer;
{$IFDEF PUREPASCAL}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: AnsiChar;
begin
  SrcLen := Byte(Str[0]);
  SubLen := Byte(SubStr[0]);
  Result := 0;
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in Str[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := SubStr[1];
  for I := 1 to Len do
  begin
    if Str[I] = C1 then
    begin
      Result := I;
      for J := 1 to SubLen-1 do
      begin
        if Str[I+J] <> SubStr[1+J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then Exit;
    end;
  end;
  // not found
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        {     ->EAX     Pointer to substr               }
        {       EDX     Pointer to string               }
        {     <-EAX     Position of substr in s or 0    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX         { Point ESI to substr                   }
        MOV     EDI,EDX         { Point EDI to s                        }

        XOR     ECX,ECX         { ECX = Length(s)                       }
        MOV     CL,[EDI]
        INC     EDI             { Point EDI to first char of s          }

        PUSH    EDI             { remember s position to calculate index}

        XOR     EDX,EDX         { EDX = Length(substr)                  }
        MOV     DL,[ESI]
        INC     ESI             { Point ESI to first char of substr     }

        DEC     EDX             { EDX = Length(substr) - 1              }
        JS      @@fail          { < 0 ? return 0                        }
        MOV     AL,[ESI]        { AL = first char of substr             }
        INC     ESI             { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX         { #positions in s to look at            }
                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASB
        JNE     @@fail
        MOV     EBX,ECX         { save outer loop counter               }
        PUSH    ESI             { save outer loop substr pointer        }
        PUSH    EDI             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSB
        POP     EDI             { restore outer loop s pointer          }
        POP     ESI             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX         { restore outer loop counter            }
        JMP     @@loop

@@fail:
        POP     EDX             { get rid of saved s pointer            }
        XOR     EAX,EAX
        JMP     @@exit

@@found:
        POP     EDX             { restore pointer to first char of s    }
        MOV     EAX,EDI { EDI points of char after match        }
        SUB     EAX,EDX { the difference is the correct index   }
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function Pos(const SubStr, Str: UnicodeString): Integer; overload;
{$IFDEF PUREPASCAL}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: WideChar;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := __StringLength(Str);
  SubLen := __StringLength(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in S[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PWideChar(SubStr)[0];
  for I := 0 to Len - 1 do
  begin
    if PWideChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if PWideChar(Str)[I+J] <> PWideChar(SubStr)[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        {     ->EAX     Pointer to substr               }
        {       EDX     Pointer to string               }
        {     <-EAX     Position of substr in str or 0  }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-4]                     { ECX = Length(s)               }

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-4]                     { EDX = Length(substr)          }

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AX,[ESI]                        { AX = first char of substr             }
        ADD     ESI,2                           { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASW
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSW
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
        SHR     EAX,1
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function Pos(const SubStr, Str: WideString): Integer; overload;
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: WideChar;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := __StringLength(Str);
  SubLen := __StringLength(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in S[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PWideChar(SubStr)[0];
  for I := 0 to Len - 1 do
  begin
    if PWideChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if PWideChar(Str)[I+J] <> PWideChar(SubStr)[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
        {     ->EAX     Pointer to substr               }
        {       EDX     Pointer to string               }
        {     <-EAX     Position of substr in str or 0  }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-4]                     { ECX = Length(s)               }
        SHR     ECX,1

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-4]                     { EDX = Length(substr)          }
        SHR     EDX,1

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AX,[ESI]                        { AX = first char of substr             }
        ADD     ESI,2                           { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASW
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSW
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
        SHR     EAX,1
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := Pos(UnicodeString(Pointer(SubStr)), UnicodeString(Pointer(Str)));
end;
{$ENDIF}

function Pos(const SubStr, Str: RawByteString): Integer; overload;
{$IFDEF PUREPASCAL}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: AnsiChar;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := __StringLength(Str);
  SubLen := __StringLength(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in Str[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PAnsiChar(SubStr)[0];
  for I := 0 to Len - 1 do
  begin
    if PAnsiChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if PAnsiChar(Str)[I+J] <> PAnsiChar(SubStr)[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function Pos is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)
asm //StackAlignSafe
       PUSH  EBX
       PUSH  ESI
       ADD   ESP, -16
       TEST  EDX, EDX
       JZ    @NotFound
       TEST  EAX, EAX
       JZ    @NotFound
       MOV   ESI, [EDX-4] //Length(Str)
       MOV   EBX, [EAX-4] //Length(Substr)
       CMP   ESI, EBX
       JL    @NotFound
       TEST  EBX, EBX
       JLE   @NotFound
       DEC   EBX
       ADD   ESI, EDX
       ADD   EDX, EBX
       MOV   [ESP+8], ESI
       ADD   EAX, EBX
       MOV   [ESP+4], EDX
       NEG   EBX
       MOVZX ECX, BYTE PTR [EAX]
       MOV   [ESP], EBX
       JNZ   @FindString

       SUB   ESI, 2
       MOV   [ESP+12], ESI

@FindChar2:
       CMP   CL, [EDX]
       JZ    @Matched0ch
       CMP   CL, [EDX+1]
       JZ    @Matched1ch
       ADD   EDX, 2
       CMP   EDX, [ESP+12]
       JB    @FindChar4
       CMP   EDX, [ESP+8]
       JB    @FindChar2
@NotFound:
       XOR   EAX, EAX
       JMP   @Exit0ch

@FindChar4:
       CMP   CL, [EDX]
       JZ    @Matched0ch
       CMP   CL, [EDX+1]
       JZ    @Matched1ch
       CMP   CL, [EDX+2]
       JZ    @Matched2ch
       CMP   CL, [EDX+3]
       JZ    @Matched3ch
       ADD   EDX, 4
       CMP   EDX, [ESP+12]
       JB    @FindChar4
       CMP   EDX, [ESP+8]
       JB    @FindChar2
       XOR   EAX, EAX
       JMP   @Exit0ch

@Matched2ch:
       ADD   EDX, 2
@Matched0ch:
       INC   EDX
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
@Exit0ch:
       ADD   ESP, 16
       POP   ESI
       POP   EBX
       RET

@Matched3ch:
       ADD   EDX, 2
@Matched1ch:
       ADD   EDX, 2
       XOR   EAX, EAX
       CMP   EDX, [ESP+8]
       JA    @Exit1ch
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
@Exit1ch:
       ADD   ESP, 16
       POP   ESI
       POP   EBX
       RET

@FindString4:
       CMP   CL, [EDX]
       JZ    @Test0
       CMP   CL, [EDX+1]
       JZ    @Test1
       CMP   CL, [EDX+2]
       JZ    @Test2
       CMP   CL, [EDX+3]
       JZ    @Test3
       ADD   EDX, 4
       CMP   EDX, [ESP+12]
       JB    @FindString4
       CMP   EDX, [ESP+8]
       JB    @FindString2
       XOR   EAX, EAX
       JMP   @Exit1

@FindString:
       SUB   ESI, 2
       MOV   [ESP+12], ESI
@FindString2:
       CMP   CL, [EDX]
       JZ    @Test0
@AfterTest0:
       CMP   CL, [EDX+1]
       JZ    @Test1
@AfterTest1:
       ADD   EDX, 2
       CMP   EDX, [ESP+12]
       JB    @FindString4
       CMP   EDX, [ESP+8]
       JB    @FindString2
       XOR   EAX, EAX
       JMP   @Exit1

@Test3:
       ADD   EDX, 2
@Test1:
       MOV   ESI, [ESP]
@Loop1:
       MOVZX EBX, WORD PTR [ESI+EAX]
       CMP   BX, WORD PTR [ESI+EDX+1]
       JNZ   @AfterTest1
       ADD   ESI, 2
       JL    @Loop1
       ADD   EDX, 2
       XOR   EAX, EAX
       CMP   EDX, [ESP+8]
       JA    @Exit1
@RetCode1:
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
@Exit1:
       ADD   ESP, 16
       POP   ESI
       POP   EBX
       RET

@Test2:
       ADD   EDX,2
@Test0:
       MOV   ESI, [ESP]
@Loop0:
       MOVZX EBX, WORD PTR [ESI+EAX]
       CMP   BX, WORD PTR [ESI+EDX]
       JNZ   @AfterTest0
       ADD   ESI, 2
       JL    @Loop0
       INC   EDX
@RetCode0:
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
       ADD   ESP, 16
       POP   ESI
       POP   EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
var
  P: PWideChar;
begin
  _UStrFromPWCharLen(Result, nil, Count);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Dec(Count);
    P[Count] := Ch;
  end;
end;

function StringOfChar(Ch: AnsiChar; Count: Integer): AnsiString; overload;
{$IFDEF PUREPASCAL}
begin
  _LStrClr(Result);
  if Count > 0 then
  begin
    Pointer(Result) := _NewAnsiString(Count, DefaultSystemCodePage);
    _FillChar(Pointer(Result)^, Count, Ch);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAligned
        { ->    AL      c               }
        {       EDX     count           }
        {       ECX     result          }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        MOV     EAX,ECX
        CALL    _LStrClr

        TEST    ESI,ESI
        JLE     @@exit

        MOV     EAX,ESI
{$IFDEF PIC}
        PUSH    EAX
        PUSH    EBX
        PUSH    ECX
        CALL    GetGOT
        MOV     EDX, [EAX].OFFSET DefaultSystemCodePage
        MOV     EDX, [EDX]
        POP     ECX
        POP     EBX
        POP     EAX
{$ELSE}
        MOV     EDX,DefaultSystemCodePage
{$ENDIF}
        CALL    _NewAnsiString

        MOV     [EDI],EAX

        MOV     EDX,ESI
        MOV     CL,BL

        CALL    _FillChar

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX

end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure SetAnsiString(Dest: PAnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
begin
  _LStrFromPWCharLen(Dest^, Source, Length, CodePage);
end;

procedure SetCodePage(var S: RawByteString; CodePage: Word; Convert: Boolean);
var
  W: UnicodeString;
  NewLen: Integer;
begin
  if (StringCodePage(S) = CodePage) or (Length(S) = 0) then
    Exit;
  if Convert then
  begin
    if StringElementSize(S) = 1 then
      W := UnicodeString(S)  // This up-converts to Unicode utf-16 using the existing codepage in the payload
    else
      W := UnicodeString(Pointer(S));  // Payload is already utf-16 so just reference it
    // now find out how large the resulting string will be
    NewLen := CharFromWChar(nil, 0, PWideChar(W), Length(W), CodePage);
    SetLength(S, NewLen);
    // finally actually convert the payload based on the new CodePage
    if NewLen > 0 then
      CharFromWChar(PAnsiChar(S), Length(S), PWideChar(W), Length(W), CodePage);
  end
  else
    InternalUniqueStringA(AnsiString(S));
  if Length(S) > 0 then
    PWord(PByte(S) - 12)^ := CodePage;
end;

function UnicodeStringToUCS4String(const S: UnicodeString): UCS4String;
var
  I: Integer;
  CharCount: Integer;
begin
  CharCount := 0;
  SetLength(Result, Length(S) + 1);
  I := 0;
  while I < Length(S) do
  begin

    if ((S[I + 1] >= #$D800) and (S[I + 1] <= #$DFFF)) and (I + 1 < Length(S)) then
    begin
      Result[CharCount] := UCS4Char((Cardinal(S[I + 1]) and $000003FF) shl 10 or (Cardinal(S[I + 2]) and $000003FF) + $00010000);
      Inc(I);
    end
    else
      Result[CharCount] := UCS4Char(S[I + 1]);

    Inc(CharCount);
    Inc(I);
  end;
  Result[CharCount] := 0;
  SetLength(Result, CharCount + 1);
end;

function UCS4StringToUnicodeString(const S: UCS4String): UnicodeString;
var
  I: Integer;
  CharCount: Integer;
begin
  SetLength(Result, Length(S) * 2 - 1); //Maximum possible number of characters
  CharCount := 0;

  I := 0;
  while I < Length(S) - 1 do
  begin
    if S[I] >= $10000 then
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar((((S[I] - $00010000) shr 10) and $000003FF) or $D800);
      Inc(CharCount);
      Result[CharCount] := WideChar(((S[I] - $00010000) and $000003FF)or $DC00);
    end
    else
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar(S[I]);
    end;

    Inc(I);
  end;

  SetLength(Result, CharCount);
end;

function WideCharToUCS4String(S: PWideChar; Len: Integer = MaxInt): UCS4String;
var
  Buffer: array[0..255] of UCS4Char;
  Index: Integer;

  procedure FlushBuffer(var Result: UCS4String; AddNull: Integer);
  begin
    SetLength(Result, Length(Result) + Index + AddNull);
    Move(Buffer, Result[Length(Result) - Index - AddNull], Index * SizeOf(UCS4Char));
    if AddNull > 0 then
      Result[Length(Result) - 1] := 0;
    Index := 0;
  end;

begin
  Index := 0;
  while (S[0] <> #0) and (Len > 0) do
  begin
    if ((S[0] >= #$D800) and (S[0] <= #$DFFF)) and (Len > 0) and (S[1] <> #0) then
    begin
      Buffer[Index] := UCS4Char((Cardinal(S[0]) and $000003FF) shl 10 or (Cardinal(S[1]) and $000003FF) + $00010000);
      Inc(S);
    end
    else
      Buffer[Index] := UCS4Char(S[0]);
    Inc(Index);
    Inc(S);
    Dec(Len);
    if Index >= Length(Buffer) then
      FlushBuffer(Result, 0);
  end;
  FlushBuffer(Result, 1);
end;

                                                                                                           
//function UTF8Encode(const WS: UnicodeString): UTF8String;
//function UTF8Decode(const S: UTF8String): UnicodeString;


{ ------------------------------------------------------------- }
{       Compiler helper for initializing/finalizing variable    }
{ ------------------------------------------------------------- }

type
  PPTypeInfo = ^PTypeInfo;
  PTypeInfo = ^TTypeInfo;
  TTypeInfo = packed record
    Kind: Byte;
    Name: ShortString;
   {TypeData: TTypeData}
  end;

  // 32 bit = 8 bytes, 64bit = 16 bytes
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    case Integer of
    0: ( Offset: Cardinal );
    1: ( _Dummy: NativeUInt );
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

{ ===========================================================================
  InitializeRecord, InitializeArray, and Initialize are PIC safe even though
  they alter EBX because they only call each other.  They never call out to
  other functions and they don't access global data.

  FinalizeRecord, Finalize, and FinalizeArray are PIC safe because they call
  Pascal routines which will have EBX fixup prologs.
  ===========================================================================}
procedure _InitializeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(PByte(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
  if FT.Count > 0 then
  begin
    for I := FT.Count - 1 downto 0 do
      _InitializeArray(Pointer(PByte(P) + UIntPtr(FT.Fields[I].Offset)), FT.Fields[I].TypeInfo^, 1);
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to record to be initialized }
        {       EDX pointer to type info                }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]                  { type name length }

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX                     // PIC safe. See comment above
        LEA     ESI,[EDX+ECX+2+8]           { address of destructable fields }
        MOV     EDI,[EDX+ECX+2+4]           { number of destructable fields }
        TEST    EDI,EDI
        JZ      @@exit

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX,1
        CALL    _InitializeArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF !PUREPASCAL}


const
  tkLString   = 10;
  tkWString   = 11;
  tkVariant   = 12;
  tkArray     = 13;
  tkRecord    = 14;
  tkInterface = 15;
  tkDynArray  = 17;
  tkUString   = 18;
  tkMRecord   = 19;

procedure InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: NativeUInt);
{$IFDEF PUREPASCAL}
begin
  _InitializeArray(p, typeInfo, elemCount);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
  JMP _InitializeArray
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: NativeUInt);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
  I: Cardinal;
begin
  if elemCount = 0 then Exit;
  case PTypeInfo(typeInfo).Kind of
    tkLString, tkWString, tkInterface, tkDynArray, tkUString:
      while elemCount > 0 do
      begin
        PPointer(P)^ := nil;
        Inc(PByte(P), SizeOf(Pointer));
        Dec(elemCount);
      end;
    tkVariant:
      while elemCount > 0 do
      begin
        with PVarData(P)^ do
          for I := Low(RawData) to High(RawData) do RawData[I] := 0;
        Inc(PByte(P), SizeOf(TVarData));
        Dec(elemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(PByte(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _InitializeArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(PByte(P), FT.Size);
          Dec(elemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(PByte(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _InitializeRecord(P, typeInfo);
          Inc(PByte(P), FT.Size);
          Dec(elemCount);
        end;
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE}
asm
        { ->    EAX     pointer to data to be initialized       }
        {       EDX     pointer to type info describing data    }
        {       ECX     number of elements of that type         }

        TEST    ECX, ECX
        JZ      @@zerolength

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX             // PIC safe.  See comment above
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]
        XOR     ECX,ECX

        CMP     AL,tkLString
        JE      @@LString
        CMP     AL,tkWString
        JE      @@WString
        CMP     AL,tkVariant
        JE      @@Variant
        CMP     AL,tkArray
        JE      @@Array
        CMP     AL,tkRecord
        JE      @@Record
        CMP     AL,tkInterface
        JE      @@Interface
        CMP     AL,tkDynArray
        JE      @@DynArray
        CMP     AL,tkUString
        JE      @@UString
        MOV     AL,reInvalidPtr
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
@@WString:
@@Interface:
@@DynArray:
@@UString:
        MOV     [EBX],ECX
        ADD     EBX,4
        DEC     EDI
        JG      @@LString
        JMP     @@exit

@@Variant:
        MOV     [EBX   ],ECX
        MOV     [EBX+ 4],ECX
        MOV     [EBX+ 8],ECX
        MOV     [EBX+12],ECX
        ADD     EBX,16
        DEC     EDI
        JG      @@Variant
        JMP     @@exit

@@Array:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]    // address of destructable fields typeinfo
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]      // size in bytes of the array data
        MOV     ECX,[ESI+EBP+2+4]    // number of destructable fields
        MOV     EDX,[EDX]
        CALL    _InitializeArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@Record:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _InitializeRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}

@@exit:

        POP     EDI
        POP     ESI
        POP     EBX
@@zerolength:
end;
{$ENDIF !PUREPASCAL}

procedure _Initialize(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _InitializeArray(p, typeInfo, 1);
end;
{$ELSE}
asm
        MOV     ECX,1
        JMP     _InitializeArray
end;
{$ENDIF !PUREPASCAL}

{$IFDEF CPUX64}
function _FinalizeRecord(P: Pointer; TypeInfo: Pointer): Pointer;
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
  if FT.Count > 0 then
  begin
    for I := 0 to FT.Count - 1 do
      _FinalizeArray(Pointer(PByte(P) + IntPtr(FT.Fields[I].Offset)), FT.Fields[I].TypeInfo^, 1);
  end;
  Result := P;
end;
{$ELSE !CPUX64}
procedure _FinalizeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to record to be finalized   }
        {       EDX pointer to type info                }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        LEA     ESI,[EDX+ECX+2+8]
        MOV     EDI,[EDX+ECX+2+4]
        TEST    EDI,EDI
        JZ      @@exit

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX,1
        CALL    _FinalizeArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

@@exit:
        MOV     EAX,EBX

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !CPUX64}

procedure _VarClr(var v: TVarData);
begin
  if Assigned(VarClearProc) then
    VarClearProc(v)
  else
    Error(reVarInvalidOp);
end;

{$IFDEF CPUX64}
function _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: NativeUInt): Pointer;
var
  FT: PFieldTable;
begin
  Result := P;
  if ElemCount = 0 then Exit;
  case PTypeInfo(TypeInfo).Kind of
    tkLString: _LStrArrayClr(P^, ElemCount);
    tkWString: _WStrArrayClr(P^, ElemCount);
    tkUString: _UStrArrayClr(P^, ElemCount);
    tkVariant:
      while ElemCount > 0 do
      begin
        _VarClr(PVarData(P)^);
        Inc(PByte(P), SizeOf(TVarData));
        Dec(ElemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(PByte(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _FinalizeArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(PByte(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _FinalizeRecord(P, TypeInfo);
          Inc(PByte(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkInterface:
      while ElemCount > 0 do
      begin
        _IntfClear(IInterface(P^));
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkDynArray:
      while ElemCount > 0 do
      begin
        { The cast and dereference of P here is to fake out the call to
          _DynArrayClear.  That function expects a var parameter.  Our
          declaration says we got a non-var parameter, but because of
          the data type that got passed to us (tkDynArray), this isn't
          strictly true.  The compiler will have passed us a reference. }
        _DynArrayClear(PPointer(P)^, typeInfo);
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE !CPUX64}
procedure _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: NativeUInt);
{$IFDEF CPUX86}
asm
        { ->    EAX     pointer to data to be finalized         }
        {       EDX     pointer to type info describing data    }
        {       ECX     number of elements of that type         }

        { This code appears to be PIC safe.  The functions called from
          here either don't make external calls or call Pascal
          routines that will fix up EBX in their prolog code
          (FreeMem, VarClr, IntfClr).  }

        CMP     ECX, 0                        { no array -> nop }
        JE      @@zerolength

        PUSH    EAX
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]

        CMP     AL,tkLString
        JE      @@LString

        CMP     AL,tkUString
        JE      @@UString

        CMP     AL,tkWString
        JE      @@WString

        CMP     AL,tkVariant
        JE      @@Variant

        CMP     AL,tkArray
        JE      @@Array

        CMP     AL,tkRecord
        JE      @@Record

        CMP     AL,tkInterface
        JE      @@Interface

        CMP     AL,tkDynArray
        JE      @@DynArray

        JMP     @@error

@@LString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@LStringArray
        CALL    _LStrClr
        JMP     @@exit
@@LStringArray:
        MOV     EDX,ECX
        CALL    _LStrArrayClr
        JMP     @@exit

@@WString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@WStringArray
        CALL    _WStrClr
        JMP     @@exit
@@WStringArray:
        MOV     EDX,ECX
        CALL    _WStrArrayClr
        JMP     @@exit

@@UString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@UStringArray
        CALL    _UStrClr
        JMP     @@exit
@@UStringArray:
        MOV     EDX,ECX
        CALL    _UStrArrayClr
        JMP     @@exit

@@Variant:
        MOV     EAX,EBX
        ADD     EBX,16
        CALL    _VarClr
        DEC     EDI
        JG      @@Variant
        JMP     @@exit
@@Array:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     ECX,[ESI+EBP+2+4]
        MOV     EDX,[EDX]
        CALL    _FinalizeArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@Record:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        { inv: EDI = number of array elements to finalize }

        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _FinalizeRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@Interface:
        MOV     EAX,EBX
        ADD     EBX,4
        CALL    _IntfClear
        DEC     EDI
        JG      @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,EBX
        MOV     EDX,ESI
        ADD     EBX,4
        CALL    _DynArrayClear
        DEC     EDI
        JG      @@DynArray
        JMP     @@exit

@@error:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX
        MOV     AL,reInvalidPtr
        JMP     Error

@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX
@@zerolength:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFDEF CPUX64}
function _Finalize(p: Pointer; typeInfo: Pointer): Pointer;
begin
  Result := _FinalizeArray(p, typeInfo, 1);
end;
{$ELSE !CPUX64}
procedure _Finalize(p: Pointer; typeInfo: Pointer);
{$IFDEF CPUX86}
asm
        MOV     ECX,1
        JMP     _FinalizeArray
end;
{$ENDIF CPUX86}
{$ENDIF}

procedure _AddRefRecord(P: Pointer; TypeInfo: Pointer);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
  if FT.Count > 0 then
  begin
    for I := 0 to FT.Count - 1 do
      _AddRefArray(Pointer(PByte(P) + UIntPtr(FT.Fields[I].Offset)), FT.Fields[I].TypeInfo^, 1);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        { ->    EAX pointer to record to be referenced  }
        {       EDX pointer to type info        }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        LEA     ESI,[EDX+ECX+2+8]
        MOV     EDI,[EDX+ECX+2+4]
        TEST    EDI,EDI
        JZ      @@exit
@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX, 1
        CALL    _AddRefArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _VarAddRef(var v: TVarData);
begin
  if Assigned(VarAddRefProc) then
    VarAddRefProc(v)
  else
    Error(reVarInvalidOp);
end;

procedure _AddRefArray(P: Pointer; TypeInfo: Pointer; ElemCount: NativeUInt);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
begin
  if ElemCount = 0 then Exit;
  case PTypeInfo(TypeInfo).Kind of
    tkLString:
      while ElemCount > 0 do
      begin
        _LStrAddRef(PPointer(P)^);
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkWString:
      while ElemCount > 0 do
      begin
        {$IFDEF MSWINDOWS}
        _WStrAddRef(PWideString(P)^);
        {$ELSE}
        _WStrAddRef(PPointer(P)^);
        {$ENDIF}
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkUString:
      while ElemCount > 0 do
      begin
        _UStrAddRef(PPointer(P)^);
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkVariant:
      while ElemCount > 0 do
      begin
        _VarAddRef(PVarData(P)^);
        Inc(PByte(P), SizeOf(TVarData));
        Dec(ElemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _AddRefArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(PByte(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _AddRefRecord(P, TypeInfo);
          Inc(PByte(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkInterface:
      while ElemCount > 0 do
      begin
        _IntfAddRef(IInterface(P^));
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkDynArray:
      while ElemCount > 0 do
      begin
        _DynArrayAddRef(PPointer(P)^);
        Inc(PByte(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAligned
        { ->    EAX     pointer to data to be referenced        }
        {       EDX     pointer to type info describing data    }
        {       ECX     number of elements of that type         }

        { This code appears to be PIC safe.  The functions called from
          here either don't make external calls (LStrAddRef, WStrAddRef) or
          are Pascal routines that will fix up EBX in their prolog code
          (VarAddRef, IntfAddRef).  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        TEST  ECX,ECX
        JZ    @@exit

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]

        CMP     AL,tkLString
        JE      @@LString
        CMP     AL,tkWString
        JE      @@WString
        CMP     AL,tkUString
        JE      @@UString
        CMP     AL,tkVariant
        JE      @@Variant
        CMP     AL,tkArray
        JE      @@Array
        CMP     AL,tkRecord
        JE      @@Record
        CMP     AL,tkInterface
        JE      @@Interface
        CMP     AL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
@@UString:
{$IFDEF POSIX}
@@WString:
{$ENDIF POSIX}
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _LStrAddRef
        DEC     EDI
        JG      @@LString
        JMP     @@exit

{$IFDEF MSWINDOWS}
@@WString:
        MOV     EAX,EBX
        ADD     EBX,4
        CALL    _WStrAddRef
        DEC     EDI
        JG      @@WString
        JMP     @@exit
{$ENDIF MSWINDOWS}
@@Variant:
        MOV     EAX,EBX
        ADD     EBX,16
        CALL    _VarAddRef
        DEC     EDI
        JG      @@Variant
        JMP     @@exit

@@Array:
        PUSH    EBP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     ECX,[ESI+EBP+2+4]
        MOV     EDX,[EDX]
        CALL    _AddRefArray
        DEC     EDI
        JG      @@ArrayLoop
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        JMP     @@exit

@@Record:
        PUSH    EBP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EBP,EDX
@@RecordLoop:
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _AddRefRecord
        DEC     EDI
        JG      @@RecordLoop
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        JMP     @@exit

@@Interface:
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _IntfAddRef
        DEC     EDI
        JG      @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _DynArrayAddRef
        DEC     EDI
        JG      @@DynArray
@@exit:

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _AddRef(P: Pointer; TypeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _AddRefArray(P, TypeInfo, 1);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        MOV     ECX,1
        JMP     _AddRefArray
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _VarCopy(var Dest: TVarData; const Src: TVarData);
begin
  if Assigned(VarCopyProc) then
    VarCopyProc(Dest, Src)
  else
    Error(reVarInvalidOp);
end;

{$IFNDEF CPUX86}
procedure _CopyRecord(Dest, Source, TypeInfo: Pointer);
var
  FT, EFT: PFieldTable;
  I: Cardinal;
  Offset: UIntPtr;
  FTypeInfo: PTypeInfo;
  DestOff, SrcOff: Pointer;
begin
  FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
  Offset := 0;
  if FT.Count > 0 then
  begin
    for I := 0 to FT.Count - 1 do
    begin
      if FT.Fields[I].Offset > Offset then
        Move(Pointer(PByte(Source) + Offset)^,
             Pointer(PByte(Dest) + Offset)^,
             FT.Fields[I].Offset - Offset);
      Offset := FT.Fields[I].Offset;
      FTypeInfo := FT.Fields[I].TypeInfo^;
      DestOff := Pointer(PByte(Dest) + Offset);
      SrcOff := Pointer(PByte(Source) + Offset);
      case FTypeInfo.Kind of
        tkLString:
          begin
            _LStrAsg(PAnsiString(DestOff)^, PAnsiString(SrcOff)^);
            Inc(Offset, SizeOf(Pointer));
          end;
        tkWString:
          begin
            _WStrAsg(PWideString(DestOff)^, PWideString(SrcOff)^);
            Inc(Offset, SizeOf(Pointer));
          end;
        tkUString:
          begin
            _UStrAsg(PUnicodeString(DestOff)^, PUnicodeString(SrcOff)^);
            Inc(Offset, SizeOf(Pointer));
          end;
        tkVariant:
          begin
            _VarCopy(PVarData(DestOff)^, PVarData(SrcOff)^);
            Inc(Offset, SizeOf(TVarData));
          end;
        tkArray:
          begin
            EFT := PFieldTable(PByte(FTypeInfo) + Byte(PTypeInfo(FTypeInfo).Name[0]));
            _CopyArray(DestOff, SrcOff, EFT.Fields[0].TypeInfo^, EFT.Count);
            Inc(Offset, EFT.Size);
          end;
        tkRecord:
          begin
            EFT := PFieldTable(PByte(FTypeInfo) + Byte(PTypeInfo(FTypeInfo).Name[0]));
            _CopyRecord(DestOff, SrcOff, FTypeInfo);
  
            Inc(Offset, EFT.Size);
          end;
        tkInterface:
          begin
            _IntfCopy(IInterface(PPointer(DestOff)^), IInterface(PPointer(SrcOff)^));
            Inc(Offset, SizeOf(Pointer));
          end;
        tkDynArray:
          begin
            _DynArrayAsg(PPointer(DestOff)^, PPointer(SrcOff)^, FTypeInfo);
            Inc(Offset, SizeOf(Pointer));
          end;
      else
        Error(reInvalidPtr);
      end;
    end;
  end;
  if FT.Size > Offset then
    Move(Pointer(PByte(Source) + Offset)^,
         Pointer(PByte(Dest) + Offset)^,
         FT.Size - Offset);
end;
{$ELSE CPUX86}
procedure _CopyRecord{ dest, source, typeInfo: Pointer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX

        XOR     EAX,EAX
        MOV     AL,[ECX+1]

        LEA     EDI,[ECX+EAX+2+8]
        MOV     EBP,[EDI-4]
        XOR     EAX,EAX
        MOV     ECX,[EDI-8]
        TEST    EBP,EBP
        JZ      @@moveWhole
        PUSH    ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
@@loop:
        MOV     ECX,[EDI+4]
        SUB     ECX,EAX
        JLE     @@nomove1
        MOV     EDX,EAX
        ADD     EAX,ESI
        ADD     EDX,EBX
        CALL    Move
@@noMove1:
        MOV     EAX,[EDI+4]

        MOV     EDX,[EDI]
        MOV     EDX,[EDX]
        MOV     CL,[EDX]

        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkInterface
        JE      @@Interface
        CMP     CL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     ECX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _LStrAsg
        MOV     EAX,4
        JMP     @@common

@@UString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _UStrAsg
        MOV     EAX,4
        JMP     @@common

@@WString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _WStrAsg
        MOV     EAX,4
        JMP     @@common

@@Variant:
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _VarCopy
        MOV     EAX,16
        JMP     @@common

@@Array:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8  // Negitive adjustment
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        PUSH    dword ptr [EDX+ECX+2]
        PUSH    dword ptr [EDX+ECX+2+4]
        MOV     ECX,[EDX+ECX+2+8]
        MOV     ECX,[ECX]
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyArray
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        JMP     @@common

@@Record:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4  //Negitive adjustment
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        MOV     ECX,[EDX+ECX+2]
        PUSH    ECX
        MOV     ECX,EDX
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyRecord
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     @@common

@@Interface:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _IntfCopy
        MOV     EAX,4
        JMP     @@common

@@DynArray:
        MOV     ECX,EDX
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _DynArrayAsg
        MOV     EAX,4

@@common:
        ADD     EAX,[EDI+4]
        ADD     EDI,8
        DEC     EBP
        JNZ     @@loop
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     ECX
@@moveWhole:
        SUB     ECX,EAX
        JLE     @@noMove2
        LEA     EDX,[EBX+EAX]
        ADD     EAX,ESI
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
@@noMove2:

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _CopyObject(Dest, Source: Pointer; vmtPtrOffs: LongInt; TypeInfo: Pointer);
var
  SavedVmtPtr: Pointer;
begin
  SavedVmtPtr := PPointer(PByte(Dest) + vmtPtrOffs)^;
  _CopyRecord(Dest, Source, TypeInfo);
  PPointer(PByte(Dest) + vmtPtrOffs)^ := SavedVmtPtr;
end;
{$ELSE CPUX86}
procedure       _CopyObject{ dest, source: Pointer; vmtPtrOffs: Longint; typeInfo: Pointer };
asm
        { ->    EAX     pointer to dest         }
        {       EDX     pointer to source       }
        {       ECX     offset of vmt in object }
        {       [ESP+4] pointer to typeInfo     }

        ADD     ECX,EAX                         { pointer to dest vmt }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    dword ptr [ECX]                 { save dest vmt }
        PUSH    ECX
        MOV     ECX,[ESP+4+4+4]
        CALL    _CopyRecord
        POP     ECX
        POP     dword ptr [ECX]                 { restore dest vmt }
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        RET     4

end;
{$ENDIF CPUX86}

{$IFNDEF CPUX86}
procedure _CopyArray(Dest, Source, TypeInfo: Pointer; Count: NativeUInt);
var
  FT: PFieldTable;
begin
  if Count = 0 then Exit;
  case PTypeInfo(TypeInfo).Kind of
    tkLString:
      while Count > 0 do
      begin
        _LStrAsg(PAnsiString(Dest)^, PAnsiString(Source)^);
        Inc(PByte(Dest), SizeOf(Pointer));
        Inc(PByte(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkWString:
      while Count > 0 do
      begin
        _WStrAsg(PWideString(Dest)^, PWideString(Source)^);
        Inc(PByte(Dest), SizeOf(Pointer));
        Inc(PByte(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkUString:
      while Count > 0 do
      begin
        _UStrAsg(PUnicodeString(Dest)^, PUnicodeString(Source)^);
        Inc(PByte(Dest), SizeOf(Pointer));
        Inc(PByte(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkVariant:
      while Count > 0 do
      begin
        _VarCopy(PVarData(Dest)^, PVarData(Source)^);
        Inc(PByte(Dest), SizeOf(TVarData));
        Inc(PByte(Source), SizeOf(TVarData));
        Dec(Count);
      end;
    tkArray:
      begin
        FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while Count > 0 do
        begin
          _CopyArray(Pointer(Dest), Pointer(Source), FT.Fields[0].TypeInfo^, FT.Count);
          Inc(PByte(Dest), FT.Size);
          Inc(PByte(Source), FT.Size);
          Dec(Count);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while Count > 0 do
        begin
          _CopyRecord(Dest, Source, TypeInfo);
          Inc(PByte(Dest), FT.Size);
          Inc(PByte(Source), FT.Size);
          Dec(Count);
        end;
      end;
    tkInterface:
      while Count > 0 do
      begin
        _IntfCopy(IInterface(PPointer(Dest)^), IInterface(PPointer(Source)^));
        Inc(PByte(Dest), SizeOf(Pointer));
        Inc(PByte(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkDynArray:
      while Count > 0 do
      begin
        _DynArrayAsg(PPointer(Dest)^, PPointer(Source)^, TypeInfo);
        Inc(PByte(Dest), SizeOf(Pointer));
        Inc(PByte(Source), SizeOf(Pointer));
        Dec(Count);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE CPUX86}
procedure _CopyArray{ dest, source, typeInfo: Pointer; cnt: Integer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }
        {       [ESP+4] count                   }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX
        MOV     EBP,[ESP+4+4*4]

        MOV     CL,[EDI]

{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkInterface
        JE      @@Interface
        CMP     CL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _LStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@LString
        JMP     @@exit

@@WString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _WStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@WString
        JMP     @@exit

@@UString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _UStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@UString
        JMP     @@exit

@@Variant:
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    _VarCopy
        ADD     EBX,16
        ADD     ESI,16
        DEC     EBP
        JNE     @@Variant
        JMP     @@exit

@@Array:
        XOR     ECX,ECX
        MOV     CL,[EDI+1]
        LEA     EDI,[EDI+ECX+2]
@@ArrayLoop:
        MOV     EAX,EBX
        MOV     EDX,ESI
        MOV     ECX,[EDI+8]
        MOV     ECX,[ECX]
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4 // Negitive Adjustment
{$ENDIF ALIGN_STACK}
        PUSH    dword ptr [EDI+4]
        CALL    _CopyArray
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        ADD     EBX,[EDI]
        ADD     ESI,[EDI]
        DEC     EBP
        JNE     @@ArrayLoop
        JMP     @@exit

@@Record:
        MOV     EAX,EBX
        MOV     EDX,ESI
        MOV     ECX,EDI
        CALL    _CopyRecord
        XOR     EAX,EAX
        MOV     AL,[EDI+1]
        ADD     EBX,[EDI+EAX+2]
        ADD     ESI,[EDI+EAX+2]
        DEC     EBP
        JNE     @@Record
        JMP     @@exit

@@Interface:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _IntfCopy
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        MOV     ECX,EDI
        CALL    _DynArrayAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@DynArray

@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET     4
end;
{$ENDIF CPUX86}

function _New(Size: NativeInt; TypeInfo: Pointer): Pointer;
{$IFDEF PUREPASCAL}
begin
  GetMem(Result, Size);
  if Result <> nil then
    _Initialize(Result, TypeInfo);
end;
{$ELSE}
asm
        { ->    EAX size of object to allocate  }
        {       EDX pointer to typeInfo         }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        CALL    _GetMem
        POP     EDX
        TEST    EAX,EAX
        JE      @@exit
        PUSH    EAX
        CALL    _Initialize
        POP     EAX
@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}

procedure _Dispose(P: Pointer; TypeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _Finalize(P, TypeInfo);
  FreeMem(P);
end;
{$ELSE}
asm
        { ->    EAX     Pointer to object to be disposed        }
        {       EDX     Pointer to type info                    }

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        CALL    _Finalize
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}


procedure CopyArray(Dest, Source, TypeInfo: Pointer; Count: NativeInt);
{$IFNDEF CPUX86}
begin
  _CopyArray(Dest, Source, TypeInfo, Count);
end;
{$ELSE CPUX86}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    dword ptr [EBP+8]
        CALL    _CopyArray
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}

procedure FinalizeArray(P, TypeInfo: Pointer; Count: NativeUInt);
{$IFDEF PUREPASCAL}
begin
  _FinalizeArray(P, TypeInfo, Count);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        JMP     _FinalizeArray
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       Wide character support                          }
{ ----------------------------------------------------- }

function WideCharToString(Source: PWideChar): UnicodeString;
begin
  WideCharToStrVar(Source, Result);
end;

function WideCharLenToString(Source: PWideChar; SourceLen: Integer): UnicodeString;
begin
  WideCharLenToStrVar(Source, SourceLen, Result);
end;

procedure WideCharToStrVar(Source: PWideChar; var Dest: UnicodeString);
begin
  _UStrFromPWChar(Dest, Source);
end;

procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: UnicodeString);
begin
  _UStrFromPWCharLen(Dest, Source, SourceLen);
end;

procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: AnsiString);
begin
  _LStrFromPWCharLen(Dest, Source, SourceLen, DefaultSystemCodePage);
end;

function StringToWideChar(const Source: UnicodeString; Dest: PWideChar;
  DestSize: Integer): PWideChar;
begin
  //Check to see if enough storage is allocated
  if Length(Source) + 1 > DestSize then
  begin
    if DestSize > 0 then
    begin
      Dest[0] := #0;
      Result := Dest;
    end
    else
    begin
      Result := '';
    end;
    Exit;
  end;

  Move(Source[1], Dest[0], Length(Source) * SizeOf(WideChar));
  Dest[Length(Source)] := #0;
  Result := Dest;
end;

{ ----------------------------------------------------- }
{       OLE string support                              }
{ ----------------------------------------------------- }

function OleStrToString(Source: PWideChar): UnicodeString;
begin
  OleStrToStrVar(Source, Result);
end;

procedure OleStrToStrVar(Source: PWideChar; var Dest: AnsiString);
begin
  WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
end;

procedure OleStrToStrVar(Source: PWideChar; var Dest: UnicodeString);
begin
  WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
end;

function StringToOleStr(const Source: AnsiString): PWideChar;
begin
  Result := nil;
  _WStrFromPCharLen(WideString(Pointer(Result)), PAnsiChar(Pointer(Source)), Length(Source));
end;

function StringToOleStr(const Source: UnicodeString): PWideChar; overload;
begin
  Result := nil;
  _WStrFromPWCharLen(WideString(Pointer(Result)), PWideChar(Pointer(Source)), Length(Source));
end;

{ ----------------------------------------------------- }
{       Variant manager support   (obsolete)            }
{ ----------------------------------------------------- }

procedure GetVariantManager(var VarMgr: TVariantManager);
begin
  FillChar(VarMgr, sizeof(VarMgr), 0);
end;

procedure SetVariantManager(const VarMgr: TVariantManager);
begin
end;

function IsVariantManagerSet: Boolean;
begin
  Result := False;
end;

                                                                 
{$IFNDEF CPUX86}
procedure _IntfDispCall(Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer); cdecl;
type
  TDispCallByIDProc = procedure(Result: Pointer; const Dispatch: IDispatch;
    DispDesc: PDispDesc; Params: Pointer); cdecl;
begin
  TDispCallByIDProc(DispCallByIDProc)(Result, Dispatch, DispDesc, @Params);
end;
{$ELSE CPUX86}
procedure _IntfDispCall;
asm
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
        LEA     EAX,[EAX].OFFSET DispCallByIDProc
        MOV     EAX,[EAX]
        XCHG    EAX,[ESP]
        RET
{$ELSE}
        JMP     DispCallByIDProc
{$ENDIF}
end;
{$ENDIF CPUX86}

                                                                      
{$IFDEF PUREPASCAL}
procedure _DispCallByIDError(Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer); cdecl;
begin
  ErrorAt(byte(reVarDispatch), ReturnAddress);
end;
{$ELSE !PUREPASCAL}
procedure _DispCallByIDError;
{$IFDEF CPUX86}
asm
        MOV     AL,reVarDispatch
        JMP     Error
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFNDEF CPUX86}
procedure _IntfVarCall(Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer);
{$ELSE !PUREPASCAL}
procedure _IntfVarCall;
{$ENDIF !PUREPASCAL}
begin
end;

{$IF defined(CPU386)}
// 64 bit integer helper routines
//
// These functions always return the 64-bit result in EAX:EDX

// ------------------------------------------------------------------------------
//  64-bit signed multiply
// ------------------------------------------------------------------------------
//
//  Param 1(EAX:EDX), Param 2([ESP+8]:[ESP+4])  ; before reg pushing
//
procedure __llmul;
asm //StackAlignSafe
        PUSH  EDX
        PUSH  EAX

  // Param2 : [ESP+16]:[ESP+12]  (hi:lo)
  // Param1 : [ESP+4]:[ESP]      (hi:lo)

        MOV   EAX, [ESP+16]
        MUL   DWORD PTR [ESP]
        MOV   ECX, EAX

        MOV   EAX, [ESP+4]
        MUL   DWORD PTR [ESP+12]
        ADD   ECX, EAX

        MOV   EAX, [ESP]
        MUL   DWORD PTR [ESP+12]
        ADD   EDX, ECX

        POP   ECX
        POP   ECX

        RET   8
end;

// ------------------------------------------------------------------------------
//  64-bit signed multiply, with overflow check (98.05.15: overflow not supported yet)
// ------------------------------------------------------------------------------
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  Param 1(EAX:EDX), Param 2([ESP+8]:[ESP+4])  ; before reg pushing
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
procedure __llmulo;
asm //StackAlignSafe
        PUSH   EDX
        PUSH   EAX

        // Param2 : [ESP+16]:[ESP+12]  (hi:lo)
        // Param1 : [ESP+4]:[ESP]      (hi:lo)

        MOV    EAX, [ESP+16]
        MUL    DWORD PTR [ESP]
        MOV    ECX, EAX

        MOV    EAX, [ESP+4]
        MUL    DWORD PTR [ESP+12]
        ADD    ECX, EAX

        MOV    EAX, [ESP]
        MUL    DWORD PTR [ESP+12]
        ADD    EDX, ECX

        POP    ECX
        POP    ECX

        RET    8
end;

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function __lldiv is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): AMD, John O'Harrow and Dennis Christensen
 *
 * ***** END LICENSE BLOCK ***** *)

// ------------------------------------------------------------------------------
//  64-bit signed division
// ------------------------------------------------------------------------------

//
//  Dividend = Numerator, Divisor = Denominator
//
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])  ; before reg pushing
//
//
procedure __lldiv; //JOH Version
asm //StackAlignSafe
{$IFDEF PC_MAPPED_EXCEPTIONS}
        PUSH    EBP
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBX, [ESP+20]
        MOV     ECX, [ESP+24]
{$ELSE !PC_MAPPED_EXCEPTIONS}
        MOV     EBX, [ESP+16]
        MOV     ECX, [ESP+20]
{$ENDIF !PC_MAPPED_EXCEPTIONS}
        MOV     ESI, EDX
        MOV     EDI, ECX
        SAR     ESI, 31
        XOR     EAX, ESI
        XOR     EDX, ESI
        SUB     EAX, ESI
        SBB     EDX, ESI          // EDX:EAX := abs(Dividend)
        SAR     EDI, 31
        XOR     ESI, EDI          // 0 if X and Y have same sign
        XOR     EBX, EDI
        XOR     ECX, EDI
        SUB     EBX, EDI
        SBB     ECX, EDI          // ECX:EBX := abs(Divisor)
        JNZ     @@BigDivisor      // divisor > 32^32-1
        CMP     EDX, EBX          // only one division needed ? (ecx = 0)
        JB      @@OneDiv          // yes, one division sufficient
        MOV     ECX, EAX          // save dividend-lo in ecx
        MOV     EAX, EDX          // get dividend-hi
        XOR     EDX, EDX          // zero extend it into edx:eax
        DIV     EBX               // quotient-hi in eax
        XCHG    EAX, ECX          // ecx = quotient-hi, eax =dividend-lo
@@OneDiv:
        DIV     EBX               // eax = quotient-lo
        MOV     EDX, ECX          // edx = quotient-hi(quotient in edx:eax)
        JMP     @SetSign
@@BigDivisor:
        SUB     ESP, 12           // Create three local variables.
        MOV     [ESP  ], EAX      // dividend_lo
        MOV     [ESP+4], EBX      // divisor_lo
        MOV     [ESP+8], EDX      // dividend_hi
        MOV     EDI, ECX          //  edi:ebx and ecx:esi
        SHR     EDX, 1            // shift both
        RCR     EAX, 1            //  divisor and
        ROR     EDI, 1            //   and dividend
        RCR     EBX, 1            //    right by 1 bit
        BSR     ECX, ECX          // ecx = number of remaining shifts
        SHRD    EBX, EDI, CL      // scale down divisor and
        SHRD    EAX, EDX, CL      //   dividend such that divisor
        SHR     EDX, CL           //    less than 2^32 (i.e. fits in ebx)
        ROL     EDI, 1            // restore original divisor (edi:esi)
        DIV     EBX               // compute quotient
        MOV     EBX, [ESP]        // dividend_lo
        MOV     ECX, EAX          // save quotient
        IMUL    EDI, EAX          // quotient * divisor hi-word (low only)
        MUL     DWORD PTR [ESP+4] // quotient * divisor low word
        ADD     EDX, EDI          // edx:eax = quotient * divisor
        SUB     EBX, EAX          // dividend-lo - (quot.*divisor)-lo
        MOV     EAX, ECX          // get quotient
        MOV     ECX, [ESP+8]      // dividend_hi
        SBB     ECX, EDX          // subtract divisor * quot. from dividend
        SBB     EAX, 0            // Adjust quotient if remainder is negative.
        XOR     EDX, EDX          // clear hi-word of quot (eax<=FFFFFFFFh)
        ADD     ESP, 12           // Remove local variables.
@SetSign:
        XOR     EAX, ESI          // If (quotient < 0),
        XOR     EDX, ESI          //   compute 1's complement of result.
        SUB     EAX, ESI          // If (quotient < 0),
        SBB     EDX, ESI          //   compute 2's complement of result.
@Done:
        POP     EDI
        POP     ESI
        POP     EBX
{$IFDEF PC_MAPPED_EXCEPTIONS}
        POP     EBP
{$ENDIF}
        RET     8
end;

// ------------------------------------------------------------------------------
//  64-bit signed division with overflow check (98.05.15: not implementated yet)
// ------------------------------------------------------------------------------

//
//  Dividend = Numerator, Divisor = Denominator
//
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])
//  Param 1 (EAX:EDX), Param 2([ESP+8]:[ESP+4])
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
//
procedure __lldivo;
asm //StackAligned
    //Don't need to stack align only calls local __lldiv which doesn't not call anything else
  // check for overflow condition: min(int64) DIV -1
        push  esi
        mov esi, [esp+12]   // Vh
        and esi, [esp+8]    // Vl
        cmp esi, 0ffffffffh   // V = -1?
        jne @@divok

        mov esi, eax
        or  esi, edx
        cmp esi, 80000000H    // U = min(int64)?
        jne @@divok

@@divOvl:
        mov eax, esi
        pop esi
        dec eax                     // turn on O-flag
        ret 8

@@divok:
        pop esi
        push  dword ptr [esp+8]   // Vh
        push  dword ptr [esp+8]   // Vl (offset is changed from push)

        call  __lldiv
        and eax, eax    // turn off O-flag
        ret 8
end;

// ------------------------------------------------------------------------------
//  64-bit unsigned division
// ------------------------------------------------------------------------------

//  Dividend(EAX(hi):EDX(lo)), Divisor([ESP+8](hi):[ESP+4](lo))  // before reg pushing
procedure __lludiv;
asm //StackAlignSafe
        PUSH    EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//

//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        MOV     EBX,20[ESP]             // get the first low word
        MOV     ECX,24[ESP]             // get the first high word

        OR      ECX,ECX
        JNZ     @__lludiv@slow_ldiv     // both high words are zero

        OR      EDX,EDX
        JZ      @__lludiv@quick_ldiv

        or      ebx,ebx
        JZ      @__lludiv@quick_ldiv    // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work

@__lludiv@slow_ldiv:
        MOV     EBP,ECX
        MOV     ECX,64                  // shift counter
        XOR     EDI,EDI                 // fake a 64 bit dividend
        XOR     ESI,ESI

@__lludiv@xloop:
        SHL     EAX,1                   // shift dividend left one bit
        RCL     EDX,1
        RCL     ESI,1
        RCL     EDI,1
        CMP     EDI,EBP                 // dividend larger?
        JB      @__lludiv@nosub
        JA      @__lludiv@subtract
        CMP     ESI,EBX                 // maybe
        JB      @__lludiv@nosub

@__lludiv@subtract:
        SUB     ESI,EBX
        SBB     EDI,EBP                 // subtract the divisor
        INC     EAX                     // build quotient

@__lludiv@nosub:
        loop    @__lludiv@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//

@__lludiv@finish:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EBP
        RET     8

@__lludiv@quick_ldiv:
        DIV     EBX                     // unsigned divide
        XOR     EDX,EDX
        JMP     @__lludiv@finish
end;

// ------------------------------------------------------------------------------
//  64-bit modulo
// ------------------------------------------------------------------------------

//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])  // before reg pushing
procedure __llmod;
asm //StackAlignSafe
        PUSH    EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     EDI,EDI
//
//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        MOV     EBX,20[ESP]             // get the first low word
        MOV     ECX,24[ESP]             // get the first high word
        OR      ECX,ECX
        JNZ     @__llmod@slow_ldiv      // both high words are zero

        OR      EDX,EDX
        JZ      @__llmod@quick_ldiv

        OR      EBX,EBX
        JZ      @__llmod@quick_ldiv     // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work
@__llmod@slow_ldiv:
//
//               Signed division should be done.  Convert negative
//               values to positive and do an unsigned division.
//               Store the sign value in the next higher bit of
//               di (test mask of 4).  Thus when we are done, testing
//               that bit will determine the sign of the result.
//
        OR      EDX,EDX                 // test sign of dividend
        JNS     @__llmod@onepos
        NEG     EDX
        NEG     EAX
        SBB     EDX,0                   // negate dividend
        OR      EDI,1

@__llmod@onepos:
        OR      ECX,ECX                 // test sign of divisor
        JNS     @__llmod@positive
        NEG     ECX
        NEG     EBX
        SBB     ECX,0                   // negate divisor

@__llmod@positive:
        MOV     EBP,ECX
        MOV     ECX,64                  // shift counter
        PUSH    EDI                     // save the flags
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//
        XOR     EDI,EDI                 // fake a 64 bit dividend
        XOR     ESI,ESI

@__llmod@xloop:
        SHL     EAX,1                   // shift dividend left one bit
        RCL     EDX,1
        RCL     ESI,1
        RCL     EDI,1
        CMP     EDI,EBP                 // dividend larger?
        JB      @__llmod@nosub
        JA      @__llmod@subtract
        CMP     ESI,EBX                 // maybe
        JB      @__llmod@nosub

@__llmod@subtract:
        SUB     ESI,EBX
        SBB     EDI,EBP                 // subtract the divisor
        INC     EAX                     // build quotient

@__llmod@nosub:
        LOOP    @__llmod@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//
        MOV     EAX,ESI
        mov     edx,edi                 // use remainder

        POP     EBX                     // get control bits
        TEST    EBX,1                   // needs negative
        JZ      @__llmod@finish
        NEG     EDX
        NEG     EAX
        SBB     EDX,0                    // negate

@__llmod@finish:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EBP
        RET     8

@__llmod@quick_ldiv:
        DIV     EBX                     // unsigned divide
        XCHG    EAX,EDX
        XOR     EDX,EDX
        JMP     @__llmod@finish
end;

// ------------------------------------------------------------------------------
//  64-bit signed modulo with overflow (98.05.15: overflow not yet supported)
// ------------------------------------------------------------------------------

//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])
//  Param 1 (EAX:EDX), Param 2([ESP+8]:[ESP+4])
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
//
procedure __llmodo;
asm //StackAlignSafe
    //Don't need to stack align only calls local __llmod which doesn't not call anything else
  // check for overflow condition: min(int64) MOD -1
        PUSH  ESI
        MOV   ESI, [ESP+12]     // Vh
        AND   ESI, [ESP+8]      // Vl
        CMP   ESI, 0FFFFFFFFH   // V = -1?
        JNE   @@modok

        MOV   ESI, EAX
        OR    ESI, EDX
        CMP   ESI, 80000000H    // U = min(int64)?
        JNE   @@modok

@@modOvl:
        MOV   EAX, ESI
        POP   ESI
        DEC   EAX               // turn on O-flag
        RET   8

@@modok:
        POP   ESI
        PUSH  DWORD PTR [ESP+8] // Vh
        PUSH  DWORD PTR [ESP+8] // Vl (offset is changed from push)

        CALL  __llmod
        AND   EAX, EAX    // turn off O-flag
        RET   8
end;

// ------------------------------------------------------------------------------
//  64-bit unsigned modulo
// ------------------------------------------------------------------------------
//  Dividend(EAX(hi):EDX(lo)), Divisor([ESP+8](hi):[ESP+4](lo))  // before reg pushing
procedure __llumod;
asm //StackAlignSafe
        PUSH    EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//

//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        MOV     EBX,20[ESP]             // get the first low word
        MOV     ECX,24[ESP]             // get the first high word
        OR      ECX,ECX
        JNZ     @__llumod@slow_ldiv     // both high words are zero

        OR      EDX,EDX
        JZ      @__llumod@quick_ldiv

        OR      EBX,EBX
        JZ      @__llumod@quick_ldiv    // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work
@__llumod@slow_ldiv:
        MOV     EBP,ECX
        MOV     ECX,64                  // shift counter
        XOR     EDI,EDI                 // fake a 64 bit dividend
        XOR     ESI,ESI                 //

@__llumod@xloop:
        SHL     EAX,1                   // shift dividend left one bit
        RCL     EDX,1
        RCL     ESI,1
        RCL     EDI,1
        CMP     EDI,EBP                 // dividend larger?
        JB      @__llumod@nosub
        JA      @__llumod@subtract
        CMP     ESI,EBX                 // maybe
        JB      @__llumod@nosub

@__llumod@subtract:
        SUB     ESI,EBX
        SBB     EDI,EBP                 // subtract the divisor
        INC     EAX                     // build quotient

@__llumod@nosub:
        LOOP    @__llumod@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//
        MOV     EAX,ESI
        MOV     EDX,EDI                 // use remainder

@__llumod@finish:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EBP
        RET     8

@__llumod@quick_ldiv:
        DIV     EBX                     // unsigned divide
        XCHG    EAX,EDX
        XOR     EDX,EDX
        jmp     @__llumod@finish
end;

// ------------------------------------------------------------------------------
//  64-bit shift left
// ------------------------------------------------------------------------------

// target (EAX:EDX) count (ECX)
//
procedure __llshl;
asm //StackAlignSafe
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshl@below32
        MOV   EDX, EAX
        SHL   EDX, CL
        XOR   EAX, EAX
        RET

@__llshl@below32:
        SHLD  EDX, EAX, CL
        SHL   EAX, CL
        RET
end;

// ------------------------------------------------------------------------------
//  64-bit signed shift right
// ------------------------------------------------------------------------------
// target (EAX:EDX) count (ECX)
procedure __llshr;
asm //StackAlignSafe
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshr@below32
        MOV   EAX, EDX
        CDQ
        SAR   EAX,CL
        RET

@__llshr@below32:
        SHRD  EAX, EDX, CL
        SAR   EDX, CL
        RET
end;

// ------------------------------------------------------------------------------
//  64-bit unsigned shift right
// ------------------------------------------------------------------------------

// target (EAX:EDX) count (ECX)
procedure __llushr;
asm //StackAlignSafe
        and cl, $3F
        cmp cl, 32
        jl  @__llushr@below32
        mov eax, edx
        xor edx, edx
        shr eax, cl
        ret

@__llushr@below32:
        shrd  eax, edx, cl
        shr edx, cl
        ret
end;
{$IFEND}

function _StrUInt64Digits(val: UInt64; width: Integer; sign: Boolean): ShortString;
var
  d: array[0..31] of Char;  { need 19 digits and a sign }
  i, k: Integer;
  spaces: Integer;
begin
  { Produce an ASCII representation of the number in reverse order }
  i := 0;
  repeat
    d[i] := Chr( (val mod 10) + Ord('0') );
    Inc(i);
    val := val div 10;
  until val = 0;
  if sign then
  begin
    d[i] := '-';
    Inc(i);
  end;

  { Fill the Result with the appropriate number of blanks }
  if width > 255 then
    width := 255;
  k := 1;
  spaces := width - i;
  while k <= spaces do
  begin
    Result[k] := AnsiChar(' ');
    Inc(k);
  end;

  { Fill the Result with the number }
  while i > 0 do
  begin
    Dec(i);
    Result[k] := AnsiChar(d[i]);
    Inc(k);
  end;

  { Result is k-1 characters long }
  SetLength(Result, k-1);
end;

function _StrInt64(val: Int64; width: Integer): ShortString;
begin
  Result := _StrUInt64Digits(Abs(val), width, val < 0);
end;

function _Str0Int64(val: Int64): ShortString;
begin
  Result := _StrInt64(val, 0);
end;

function _StrUInt64(val: UInt64; width: Integer): ShortString;
begin
  Result := _StrUInt64Digits(val, width, False);
end;

function _Str0UInt64(val: Int64): ShortString;
begin
  Result := _StrUInt64(val, 0);
end;

function _WriteInt64(var t: TTextRec; val: Int64; width: LongInt): Pointer;
var
  s: ShortString;
begin
  s := _StrInt64(val, 0);
  Result := _WriteString(t, s, width);
end;

function _Write0Int64(var t: TTextRec; val: Int64): Pointer;
begin
  Result := _WriteInt64(t, val, 0);
end;

function _WriteUInt64(var t: TTextRec; val: UInt64; width: LongInt): Pointer;
var
  s: ShortString;
begin
  s := _StrUInt64Digits(val, 0, False);
  Result := _WriteString(t, s, width);
end;

function _Write0UInt64(var t: TTextRec; val: UInt64): Pointer;
begin
  Result := _WriteUInt64(t, val, 0);
end;

function _ValInt64L(const s: AnsiString; var code: Integer): Int64;
begin
  Result := _ValInt64(string(s), code);
end;

function _ReadInt64(var t: TTextRec): Int64;
var
  p: PWord;
  count: Integer;
  c: Byte;
  eof: Boolean;
  code: Integer;
  u: UnicodeString;
begin
  if _SeekEof(t) then
    Result := 0
  else
  begin
    SetLength(U, 32);
    p := PWord(U);
    for count := 1 to 32 do
    begin
      c := _GetAnsiChar(t, eof, DefaultSystemCodePage);
      if c <= $20 then break;
      p^ := c;
      _SkipAnsiChar(t);
      Inc(p);
    end;
    SetLength(U, (PByte(p) - PByte(U)) div sizeof(word));
    Result := _ValInt64(u, code);
    if code <> 0 then
      SetInOutRes(106);
  end;
end;

function _ValInt64(const s: string; var code: Integer): Int64;
var
  i: Integer;
  dig: Integer;
  sign: Boolean;
  empty: Boolean;
begin
  i := 1;
  {$IFNDEF CPUX64} // avoid E1036: Variable 'dig' might not have been initialized
  dig := 0;
  {$ENDIF}
  Result := 0;
  if s = '' then
  begin
    code := i;
    exit;
  end;
  while s[i] = Char(' ') do
    Inc(i);
  sign := False;
  if s[i] =  Char('-') then
  begin
    sign := True;
    Inc(i);
  end
  else if s[i] =  Char('+') then
    Inc(i);
  empty := True;
  if (s[i] =  Char('$')) or (Upcase(s[i]) =  Char('X'))
    or ((s[i] =  Char('0')) and (I < Length(S)) and (Upcase(s[i+1]) =  Char('X'))) then
  begin
    if s[i] =  Char('0') then
      Inc(i);
    Inc(i);
    while True do
    begin
      case   Char(s[i]) of
       Char('0').. Char('9'): dig := Ord(s[i]) -  Ord('0');
       Char('A').. Char('F'): dig := Ord(s[i]) - (Ord('A') - 10);
       Char('a').. Char('f'): dig := Ord(s[i]) - (Ord('a') - 10);
      else
        break;
      end;
      if (Result < 0) or (Result > (High(Int64) shr 3)) then
        Break;
      Result := Result shl 4 + dig;
      Inc(i);
      empty := False;
    end;
    if sign then
      Result := - Result;
  end
  else
  begin
    while True do
    begin
      case  Char(s[i]) of
        Char('0').. Char('9'): dig := Ord(s[i]) - Ord('0');
      else
        break;
      end;
      if (Result < 0) or (Result > (High(Int64) div 10)) then
        break;
      Result := Result*10 + dig;
      Inc(i);
      empty := False;
    end;
    if sign then
      Result := - Result;
    if (Result <> 0) and (sign <> (Result < 0)) then
      Dec(i);
  end;
  if (s[i] <> Char(#0)) or empty then
    code := i
  else
    code := 0;
end;


{ ----------------------------------------------------- }
{       Compiler helper for Dynamic array support       }
{ ----------------------------------------------------- }

function _DynArrayLength(const A: Pointer): NativeInt;
{$IFDEF PUREPASCAL}
begin
  Result := 0;
  if A <> nil then
    Result := PDynArrayRec(PByte(A) - SizeOf(TDynArrayRec))^.Length;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{       FUNCTION _DynArrayLength(const a: array of ...): Longint; }
{     ->EAX     Pointer to array or nil                           }
{     <-EAX     High bound of array + 1 or 0                      }
        TEST    EAX,EAX
        JZ      @@skip
        MOV     EAX,[EAX-4]
@@skip:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function _DynArrayHigh(const A: Pointer): NativeInt;
{$IFDEF PUREPASCAL}
begin
  Result := _DynArrayLength(A) - 1;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{       FUNCTION _DynArrayHigh(const a: array of ...): Longint; }
{     ->EAX     Pointer to array or nil                         }
{     <-EAX     High bound of array or -1                       }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    _DynArrayLength
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        DEC     EAX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure DynArrayClear(var A: Pointer; TypeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _DynArrayClear(a, typeInfo);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    _DynArrayClear
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: NativeInt; lengthVec: PNativeint);
var
  i: NativeInt;
  newLength, oldLength, minLength: NativeInt;
  elSize: NativeInt;
  neededSize: NativeInt;
  p, pp: Pointer;
begin
  p := a;

  // Fetch the new length of the array in this dimension, and the old length
  newLength := lengthVec^;
  if newLength <= 0 then
  begin
    if newLength < 0 then
      Error(reRangeError);
    DynArrayClear(a, typeInfo);
    exit;
  end;

  oldLength := 0;
  if p <> nil then
  begin
    Dec(PByte(p), SizeOf(TDynArrayRec));
    oldLength := PDynArrayRec(p).Length;
  end;

  // Calculate the needed size of the heap object
  Inc(PAnsiChar(typeInfo), Length(PDynArrayTypeInfo(typeInfo).name));
  elSize := PDynArrayTypeInfo(typeInfo).elSize;
  if PDynArrayTypeInfo(typeInfo).elType <> nil then
    typeInfo := PDynArrayTypeInfo(typeInfo).elType^
  else
    typeInfo := nil;
  neededSize := newLength*elSize;
  if neededSize div newLength <> elSize then
    Error(reRangeError);
  Inc(neededSize, SizeOf(TDynArrayRec));
  if neededSize < 0 then
    Error(reRangeError);

  // If the heap object isn't shared (ref count = 1), just resize it. Otherwise, we make a copy
  if (p = nil) or (PDynArrayRec(p).RefCnt = 1) then
  begin
    pp := p;
    if (newLength < oldLength) and (typeInfo <> nil) then
      FinalizeArray(PAnsiChar(p) + SizeOf(TDynArrayRec) + newLength*elSize, typeInfo, oldLength - newLength);
    ReallocMem(pp, neededSize);
    p := pp;
  end
  else
  begin
    Dec(PDynArrayRec(p).RefCnt);
    GetMem(p, neededSize);
    minLength := oldLength;
    if minLength > newLength then
      minLength := newLength;
    if typeInfo <> nil then
    begin
      FillChar((PAnsiChar(p) + SizeOf(TDynArrayRec))^, minLength*elSize, 0);
      CopyArray(PAnsiChar(p) + SizeOf(TDynArrayRec), a, typeInfo, minLength)
    end
    else
      Move(PAnsiChar(a)^, (PAnsiChar(p) + SizeOf(TDynArrayRec))^, minLength*elSize);
  end;

  // The heap object will now have a ref count of 1 and the new length
  PDynArrayRec(p).RefCnt := 1;
  PDynArrayRec(p).Length := newLength;
  Inc(PByte(p), SizeOf(TDynArrayRec));

  // Set the new memory to all zero bits
  if newLength > oldLength then
    FillChar((PAnsiChar(p) + elSize * oldLength)^, elSize * (newLength - oldLength), 0);

  // Take care of the inner dimensions, if any
  if dimCnt > 1 then
  begin
    Inc(lengthVec);
    Dec(dimCnt);
    for i := 0 to newLength-1 do
      DynArraySetLength(PPointerArray(p)[i], typeInfo, dimCnt, lengthVec);
  end;
  a := p;
end;

{$IFDEF CPUX64}
procedure _DynArraySetLength(var A: Pointer; TypeInfo: Pointer; DimCnt: NativeInt; LengthVec: NativeInt); varargs;
begin
  DynArraySetLength(A, TypeInfo, DimCnt, @LengthVec);
end;
{$ELSE !CPUX64}
procedure _DynArraySetLength;
{$IFDEF CPUX86}
asm
{       PROCEDURE _DynArraySetLength(var a: dynarray; typeInfo: PDynArrayTypeInfo; dimCnt: Longint; lengthVec: ^Longint) }
{     ->EAX     Pointer to dynamic array (= pointer to pointer to heap object) }
{       EDX     Pointer to type info for the dynamic array                     }
{       ECX     number of dimensions                                           }
{       [ESP+4] dimensions                                                     }
{$IFDEF ALIGN_STACK}
        PUSH    EBP        // Setup stack frame in case of exception
        MOV     EBP, ESP   // to prevent unwinder from freaking out
        SUB     ESP, 4
        PUSH    ESP
        ADD     dword ptr [ESP],12
{$ELSE !ALIGN_STACK}
        PUSH    ESP
        ADD     dword ptr [ESP],4
{$ENDIF !ALIGN_STACK}
        CALL    DynArraySetLength
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
        POP     EBP
{$ENDIF ALIGN_STACK}
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

{$IFDEF CPUX86}
procedure _DynArrayCopy(A: Pointer; TypeInfo: Pointer; var Result: Pointer);
begin
  if A <> nil then
    _DynArrayCopyRange(A, TypeInfo, 0, PDynArrayRec(PByte(A) - SizeOf(TDynArrayRec)).Length, Result)
  else
    _DynArrayClear(Result, TypeInfo);
end;
{$ELSE !CPUX86}
procedure _DynArrayCopy(var Result: Pointer; A: Pointer; TypeInfo: Pointer);
begin
  if A <> nil then
    _DynArrayCopyRange(Result, A, TypeInfo, 0, PDynArrayRec(PByte(A) - SizeOf(TDynArrayRec)).Length)
  else
    _DynArrayClear(Result, TypeInfo);
end;
{$ENDIF !CPUX86}

{$IFDEF CPUX86}
procedure _DynArrayCopyRange(A: Pointer; TypeInfo: Pointer; Index, Count : Integer; var Result: Pointer);
{$ELSE !CPUX86}
procedure _DynArrayCopyRange(var Result: Pointer; A: Pointer; TypeInfo: Pointer; Index, Count: NativeInt);
{$ENDIF !CPUX86}
var
  arrayLength: NativeInt;
  elSize: Integer;
  typeInf: PDynArrayTypeInfo;
  p: Pointer;
begin
  p := nil;
  if A <> nil then
  begin
    typeInf := TypeInfo;

    // Limit index and count to values within the array
    if Index < 0 then
    begin
      Inc(Count, Index);
      Index := 0;
    end;
    arrayLength := PDynArrayRec(PByte(A) - SizeOf(TDynArrayRec)).Length;
    if Index > arrayLength then
      Index := arrayLength;
    if Count > arrayLength - Index then
      Count := arrayLength - Index;
    if Count < 0 then
      Count := 0;

    if Count > 0 then
    begin
      // Figure out the size and type descriptor of the element type
      Inc(PByte(typeInf), Byte(typeInf.name[0]));
      elSize := typeInf.elSize;
      if typeInf.elType <> nil then
        typeInf := typeInf.elType^
      else
        typeInf := nil;

      // Allocate the amount of memory needed
      GetMem(p, Count * elSize + SizeOf(TDynArrayRec));

      // The reference count of the new array is 1, the length is count
      PDynArrayRec(p).RefCnt := 1;
      PDynArrayRec(p).Length := Count;
      Inc(PByte(p), SizeOf(TDynArrayRec));
      Inc(PByte(A), Index * elSize);

      // If the element type needs destruction, we must copy each element,
      // otherwise we can just copy the bits
      if Count > 0 then
      begin
        if typeInf <> nil then
        begin
          FillChar(p^, Count * elSize, 0);
          CopyArray(p, A, typeInf, Count)
        end
        else
          Move(A^, p^, Count * elSize);
      end;
    end;
  end;
  DynArrayClear(Result, TypeInfo);
  Result := p;
end;

{$IFDEF CPUX64}
function _DynArrayClear(var A: Pointer; TypeInfo: Pointer): Pointer;
{$ELSE !CPUX64}
procedure _DynArrayClear(var A: Pointer; TypeInfo: Pointer);
{$ENDIF CPUX64}
{$IFDEF PUREPASCAL}
var
  P: Pointer;
  Len: NativeInt;
begin
  // Nothing to do if Pointer to heap object is nil
  P := A;
  if P <> nil then
  begin
    // Set the variable to be finalized to nil
    A := nil;
    // Decrement ref count. Nothing to do if not zero now.
    if InterlockedDecrement(PDynArrayRec(PByte(P) - SizeOf(TDynArrayRec))^.RefCnt) = 0 then
    begin
      // Fetch the type descriptor of the elements
      Inc(PByte(TypeInfo), Byte(PDynArrayTypeInfo(TypeInfo)^.name[0]));
      if PDynArrayTypeInfo(TypeInfo)^.elType <> nil then
      begin
        Len := PDynArrayRec(PByte(P) - SizeOf(TDynArrayRec))^.Length;
        if Len <> 0 then
        begin
          TypeInfo := PDynArrayTypeInfo(TypeInfo)^.elType^;
          _FinalizeArray(P, TypeInfo, Len);
        end;
      end;
      // Now deallocate the array
      Dec(PByte(P), SizeOf(TDynArrayRec));
      _FreeMem(P);
    end;
  end;
{$IFDEF CPUX64}
  Result := @A;
{$ENDIF CPUX64}
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm //StackAlignSafe
{     ->EAX     Pointer to dynamic array (Pointer to pointer to heap object)}
{       EDX     Pointer to type info                                        }

        {       Nothing to do if Pointer to heap object is nil }
        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      @@exit

        {       Set the variable to be finalized to nil }
        MOV     dword ptr [EAX],0

        {       Decrement ref count. Nothing to do if not zero now. }
   LOCK DEC     dword ptr [ECX-8]
        JNE     @@exit

        {       Save the source - we're supposed to return it }
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EAX,ECX

        {       Fetch the type descriptor of the elements }
        XOR     ECX,ECX
        MOV     CL,[EDX].TDynArrayTypeInfo.name;
        MOV     EDX,[EDX+ECX].TDynArrayTypeInfo.elType;

        {       If it's non-nil, finalize the elements }
        TEST    EDX,EDX
        JE      @@noFinalize
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@noFinalize
        MOV     EDX,[EDX]
        {       Save/restore the array around the call to _FinalizeArray }
{$IFDEF ALIGN_STACK}
        MOV     [ESP], EAX
{$ELSE !ALIGN_STACK}
        PUSH    EAX
{$ENDIF !ALIGN_STACK}
        CALL    _FinalizeArray
{$IFDEF ALIGN_STACK}
        MOV     EAX, [ESP]
{$ELSE !ALIGN_STACK}
        POP     EAX
{$ENDIF !ALIGN_STACK}
@@noFinalize:
        {       Now deallocate the array }
        SUB     EAX,8
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EAX
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _DynArrayAsg(var Dest: Pointer; Src: Pointer; TypeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _DynArrayAddRef(Src);
  _DynArrayClear(Dest, TypeInfo);
  Dest := Src;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{     ->EAX     Pointer to destination (pointer to pointer to heap object) }
{       EDX     source (pointer to heap object) }
{       ECX     Pointer to rtti describing dynamic array }

        PUSH    EBX
        MOV     EBX,[EAX]

        {       Increment ref count of source if non-nil }

        TEST    EDX,EDX
        JE      @@skipInc
   LOCK INC     dword ptr [EDX-8]
@@skipInc:
        {       Dec ref count of destination - if it becomes 0, clear dest }
        TEST    EBX,EBX
        JE      @@skipClear
   LOCK DEC     dword ptr[EBX-8]
        JNZ     @@skipClear
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,ECX
        INC     dword ptr[EBX-8]
        {       Stack is aligned at this point }
        CALL    _DynArrayClear
        POP     EDX
        POP     EAX
@@skipClear:
        {       Finally store source into destination }
        MOV     [EAX],EDX

        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure _DynArrayAddRef(P: Pointer);
{$IFDEF PUREPASCAL}
begin
  if P <> nil then
    InterlockedIncrement(PDynArrayRec(PByte(P) - SizeOf(TDynArrayRec))^.RefCnt);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
{     ->EAX     Pointer to heap object }
        TEST    EAX,EAX
        JE      @@exit
   LOCK INC     dword ptr [EAX-8]
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function DynArrayIndex(P: Pointer; const Indices: array of NativeInt; TypInfo: Pointer): Pointer; overload;
var
  I, L, H: Integer;
begin
  L := Low(Indices);
  H := High(Indices);

  if H >= L then
  begin
    { Start at the beggining of the array }
    for I := L to H do
    begin
      { Skip to the relevant part }
      Inc(PByte(TypInfo), Length(PDynArrayTypeInfo(TypInfo)^.name));

      { Jump into the array at the given position using the indices and element size }
      Inc(PByte(P), Indices[I] * PDynArrayTypeInfo(TypInfo)^.elSize);

      { Go one step deeper into the child arrays only if it's not the last dimension }
      if I < H then
      begin
        if PDynArrayTypeInfo(TypInfo)^.elType <> nil then
          TypInfo := PDynArrayTypeInfo(TypInfo)^.elType^;

        P := PPointer(P)^;
      end;
    end;
  end;

  { Result is the input pointer modified }
  Result := P;
end;

function DynArrayIndex(P: Pointer; const Indices: array of Integer; TypInfo: Pointer): Pointer; overload;
{$IFDEF PUREPASCAL}
var
  I, L, H: Integer;
begin
  L := Low(Indices);
  H := High(Indices);

  if H >= L then
  begin
    { Start at the beggining of the array }
    for I := L to H do
    begin
      { Skip to the relevant part }
      Inc(PByte(TypInfo), Length(PDynArrayTypeInfo(TypInfo)^.name));

      { Jump into the array at the given position using the indices and element size }
      Inc(PByte(P), Indices[I] * PDynArrayTypeInfo(TypInfo)^.elSize);

      { Go one step deeper into the child arrays only if it's not the last dimension }
      if I < H then
      begin
        if PDynArrayTypeInfo(TypInfo)^.elType <> nil then
          TypInfo := PDynArrayTypeInfo(TypInfo)^.elType^;
        if P = nil then
          Exit(nil);
        P := PPointer(P)^;
      end;
    end;
  end;

  { Result is the input pointer modified }
  Result := P;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        {     ->EAX     P                       }
        {       EDX     Pointer to Indices      }
        {       ECX     High bound of Indices   }
        {       [EBP+8] TypInfo                 }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     ESI,EDX
        MOV     EDI,[EBP+8]
        MOV     EBP,EAX

        XOR     EBX,EBX                 {  for i := 0 to High(Indices) do       }
        TEST    ECX,ECX
        JGE     @@start
@@loop:
        TEST    EBP, EBP
        JE      @@loopEnd
        MOV     EBP,[EBP]
@@start:
        XOR     EAX,EAX
        MOV     AL,[EDI].TDynArrayTypeInfo.name
        ADD     EDI,EAX
        MOV     EAX,[ESI+EBX*4]         {    P := P + Indices[i]*TypInfo.elSize }
        MUL     [EDI].TDynArrayTypeInfo.elSize
        MOV     EDI,[EDI].TDynArrayTypeInfo.elType
        TEST    EDI,EDI
        JE      @@skip
        MOV     EDI,[EDI]
@@skip:
        ADD     EBP,EAX
        INC     EBX
        CMP     EBX,ECX
        JLE     @@loop

@@loopEnd:

        MOV     EAX,EBP

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}


{ Returns the DynArrayTypeInfo of the Element Type of the specified DynArrayTypeInfo }
function DynArrayElTypeInfo(typeInfo: PDynArrayTypeInfo): PDynArrayTypeInfo;
begin
  Result := nil;
  if typeInfo <> nil then
  begin
    Inc(PByte(typeInfo), Byte(typeInfo.name[0]));
    if typeInfo.elType <> nil then
      Result := typeInfo.elType^;
  end;
end;

{ Returns # of dimemsions of the DynArray described by the specified DynArrayTypeInfo}
function DynArrayDim(typeInfo: Pointer): Integer;
begin
  Result := 0;
  while (typeInfo <> nil) and (PDynArrayTypeInfo(typeInfo)^.kind = tkDynArray) do
  begin
    Inc(Result);
    typeInfo := DynArrayElTypeInfo(typeInfo);
  end;
end;

{ Returns size of the Dynamic Array}
function DynArraySize(A: Pointer): NativeInt;
{$IFDEF PUREPASCAL}
begin
  Result := 0;
  if A <> nil then
    Result := PDynArrayRec(PByte(A) - SizeOf(TDynArrayRec)).Length;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        TEST EAX, EAX
        JZ   @@exit
        MOV  EAX, [EAX-4]
@@exit:
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

// Returns whether array is rectangular
function IsDynArrayRectangular(const DynArray: Pointer; typeInfo: Pointer): Boolean;
var
  Dim, I, J, Size, SubSize: Integer;
  P: Pointer;
begin
  // Assume we have a rectangular array
  Result := True;

  P := DynArray;
  Dim := DynArrayDim(typeInfo);

  {NOTE: Start at 1. Don't need to test the first dimension - it's rectangular by definition}
  for I := 1 to dim-1 do
  begin
    if P <> nil then
    begin
      { Get size of this dimension }
      Size := DynArraySize(P);

      { Get Size of first sub. dimension }
      SubSize := DynArraySize(PPointerArray(P)[0]);

      { Walk through every dimension making sure they all have the same size}
      for J := 1  to Size-1 do
        if DynArraySize(PPointerArray(P)[J]) <> SubSize then
        begin
          Result := False;
          Exit;
        end;

      { Point to next dimension}
      P := PPointerArray(P)[0];
    end;
  end;
end;

// Returns Bounds of Dynamic array as an array of integer containing the 'high' of each dimension
function DynArrayBounds(const DynArray: Pointer; typeInfo: Pointer): TBoundArray;
var
  Dim, I: Integer;
  P: Pointer;
begin
  P := DynArray;

  Dim := DynArrayDim(typeInfo);
  SetLength(Result, Dim);

  for I := 0 to dim-1 do
    if P <> nil then
    begin
      Result[I] := DynArraySize(P)-1;
      P := PPointerArray(P)[0]; // Assume rectangular arrays
    end;
end;

{ Decrements to next lower index - Returns True if successful }
{ Indices: Indices to be decremented }
{ Bounds : High bounds of each dimension }
function DecIndices(var Indices: TBoundArray; const Bounds: TBoundArray): Boolean;
var
  I, J: Integer;
begin
  { Find out if we're done: all at zeroes }
  Result := False;
  for I := Low(Indices)  to High(Indices) do
    if Indices[I] <> 0  then
    begin
      Result := True;
      break;
    end;
  if not Result then
    Exit;

  { Two arrays must be of same length }
  Assert(Length(Indices) = Length(Bounds));

  { Find index of item to tweak }
  for I := High(Indices) downto Low(Bounds) do
  begin
    // If not reach zero, dec and bail out
    if Indices[I] <> 0 then
    begin
      Dec(Indices[I]);
      Exit;
    end
    else
    begin
      J := I;
      while Indices[J] = 0 do
      begin
        // Restore high bound when we've reached zero on a particular dimension
        Indices[J] := Bounds[J];
        // Move to higher dimension
        Dec(J);
        Assert(J >= 0);
      end;
      Dec(Indices[J]);
      Exit;
    end;
  end;
end;

{ Package/Module registration/unregistration }

{$IFDEF MSWINDOWS}
const
  LCID_SUPPORTED          = $00000002;  { supported locale ids }
  LOCALE_SABBREVLANGNAME  = $00000003;  { abbreviated language name }
  LOCALE_SISO639LANGNAME  = $00000059;  { ISO abbreviated language name }
  LOCALE_SISO3166CTRYNAME = $0000005A;  { ISO abbreviated country name }
  LOCALE_SNAME            = $0000005c;  { locale name (ie: en-us) }
  LOCALE_SPARENT          = $0000006d;  { Fallback name for resources }
  LOCALE_NAME_MAX_LENGTH  = 85;
  MUI_LANGUAGE_ID         = $4;  { Use traditional language ID convention }
  MUI_LANGUAGE_NAME       = $8;  { Use ISO language (culture) name convention }
  MUI_UI_FALLBACK         = $30; { Retrieve a complete thread preferred UI languages list }
  LOAD_LIBRARY_AS_DATAFILE = 2;
  HKEY_CURRENT_USER = HKEY(NativeUInt($80000001));
  HKEY_LOCAL_MACHINE = HKEY(NativeUInt($80000002));
  KEY_ALL_ACCESS = $000F003F;
  KEY_READ = $000F0019;

  OlderLocaleOverrideKey = 'Software\Borland\Delphi\Locales'; // do not localize
  OldLocaleOverrideKey = 'Software\Borland\Locales'; // do not localize
  NewLocaleOverrideKey = 'Software\CodeGear\Locales'; // do not localize
  NewerLocaleOverrideKey = 'Software\Embarcadero\Locales'; // do not localize
{$ENDIF}

function FindModule(Instance: HINST): PLibModule;
begin
  Result := LibModuleList;
  while Result <> nil do
  begin
    if (Instance = Result.Instance) or
       (Instance = Result.CodeInstance) or
       (Instance = Result.DataInstance) or
       (Instance = Result.ResInstance) then
      Exit;
    Result := Result.Next;
  end;
end;

function FindHInstance(Address: Pointer): HINST;
{$IFDEF MSWINDOWS}
var
  MemInfo: TMemoryBasicInformation;
begin
  VirtualQuery(Address, MemInfo, SizeOf(MemInfo));
  if MemInfo.State = $1000{MEM_COMMIT} then
    Result := UIntPtr(MemInfo.AllocationBase)
  else
    Result := 0;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  Info: dl_info;
begin
  if (dladdr(UIntPtr(Address), Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
    Info.dli_fname := nil;   // if it's not in a library, assume the exe
  Result := NativeUInt(dlopen(PAnsiChar(UTF8Encode(Info.dli_fname)), RTLD_LAZY));
  if Result <> 0 then
    dlclose(Result);
end;
{$ENDIF}

function FindClassHInstance(ClassType: TClass): HINST;
begin
  Result := FindHInstance(Pointer(ClassType));
end;

{$IFDEF POSIX}
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
var
  Addr: Pointer;
  Info: dl_info;
{$IFDEF LINUX}
  Temp: Integer;
  ProcBuff: array [0..MAX_PATH] of AnsiChar;
  FoundInModule: HMODULE;
{$ENDIF}
{$IFDEF MACOS}
  LoadedModule: HMODULE;
  Name: PAnsiChar;
  Index: Integer;
{$ENDIF MACOS}
begin
  Result := 0;
  if BufLen <= 0 then Exit;
  if (Module = MainInstance) or (Module = 0) then
  begin
    // First, try the dlsym approach.
    // dladdr fails to return the name of the main executable
    // in glibc prior to 2.1.91

{   Look for a dynamic symbol exported from this program.
    _DYNAMIC is not required in a main program file.
    If the main program is compiled with Delphi, it will always
    have a resource section, named @Sysinit@ResSym.
    If the main program is not compiled with Delphi, dlsym
    will search the global name space, potentially returning
    the address of a symbol in some other shared object library
    loaded by the program.  To guard against that, we check
    that the address of the symbol found is within the
    main program address range.  }

    dlerror;   // clear error state;  dlsym doesn't
//    Addr := dlsym(Module, '@Sysinit@ResSym');
    if Module = 0 then Module := RTLD_DEFAULT;
    Addr := dlsym(Module, 'SysinitResSym');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(UIntPtr(Addr), Info) <> 0)
      and (Info.dli_fname <> nil)
      and (Info.dli_fbase = ExeBaseAddress) then
    begin
      Result := strlen(Info.dli_fname);
      if Result >= BufLen then Result := BufLen-1;

      // dlinfo may not give a full path.  Compare to /proc/self/exe,
      // take longest result.
{$IFDEF LINUX}
//      Temp := readlink('/proc/self/exe', Buffer, BufLen);
      Temp := readlink('/proc/self/exe', ProcBuff, MAX_PATH);
      if Temp >= BufLen then Temp := BufLen-1;
      if Temp > Result then
      begin
        Utf8ToUnicode(Buffer, BufLen, ProcBuff, Temp);
        Result := Temp;
      end
      else
        Move(Info.dli_fname^, Buffer^, Result);
{$ENDIF LINUX}
{$IFDEF MACOS}
      Utf8ToUnicode(Buffer, BufLen, Info.dli_fname, Result);
{$ENDIF MACOS}
      Buffer[Result] := #0;
      Exit;
    end;

{$IFDEF LINUX}
    // Try inspecting the /proc/ virtual file system
    // to find the program filename in the process info
    Result := readlink(AnsiString('/proc/self/exe'), ProcBuff, MAX_PATH);
    if Result <> -1 then
    begin
      if Result >= BufLen then Result := BufLen-1;
      ProcBuff[Result] := #0;
      Utf8ToUnicode(Buffer, BufLen, ProcBuff, Result);
    end;
{$ENDIF LINUX}
{$IFDEF AllowParamStrModuleName}
{   Using ParamStr(0) to obtain a module name presents a potential
    security hole.  Resource modules are loaded based upon the filename
    of a given module.  We use dlopen() to load resource modules, which
    means the .init code of the resource module will be executed.
    Normally, resource modules contain no code at all - they're just
    carriers of resource data.
    An unpriviledged user program could launch our trusted,
    priviledged program with a bogus parameter list, tricking us
    into loading a module that contains malicious code in its
    .init section.
    Without this ParamStr(0) section, GetModuleFilename cannot be
    misdirected by unpriviledged code (unless the system program loader
    or the /proc file system or system root directory has been compromised).
    Resource modules are always loaded from the same directory as the
    given module.  Trusted code (programs, packages, and libraries)
    should reside in directories that unpriviledged code cannot alter.

    If you need GetModuleFilename to have a chance of working on systems
    where glibc < 2.1.91 and /proc is not available, and your
    program will not run as a priviledged user (or you don't care),
    you can define AllowParamStrModuleNames and rebuild the System unit
    and baseCLX package.  Note that even with ParamStr(0) support
    enabled, GetModuleFilename can still fail to find the name of
    a module.  C'est la Unix.  }

    if Result = -1 then // couldn't access the /proc filesystem
    begin               // return less accurate ParamStr(0)

{     ParamStr(0) returns the name of the link used
      to launch the app, not the name of the app itself.
      Also, if this app was launched by some other program,
      there is no guarantee that the launching program has set
      up our environment at all.  (example: Apache CGI) }

      if (ArgValues = nil) or (ArgValues^ = nil) or
        (PCharArray(ArgValues^)[0] = nil) then
      begin
        Result := 0;
        Exit;
      end;
      Result := strlen(PCharArray(ArgValues^)[0]);
      if Result >= BufLen then Result := BufLen-1;
      Move(PCharArray(ArgValues^)[0]^, Buffer^, Result);
      Buffer[Result] := #0;
    end;
{$ENDIF AllowParamStrModuleName}
  end
  else
  begin
{$IFDEF LINUX}
{   For shared object libraries, we can rely on the dlsym technique.
    Look for a dynamic symbol in the requested module.
    Don't assume the module was compiled with Delphi.
    We look for a dynamic symbol with the name _DYNAMIC.  This
    exists in all ELF shared object libraries that export
    or import symbols;  If someone has a shared object library that
    contains no imports or exports of any kind, this will probably fail.
    If dlsym can't find the requested symbol in the given module, it
    will search the global namespace and could return the address
    of a symbol from some other module that happens to be loaded
    into this process.  That would be bad, so we double check
    that the module handle of the symbol found matches the
    module handle we asked about.}

    dlerror;   // clear error state;  dlsym doesn't
    Addr := dlsym(Module, '_DYNAMIC');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(UIntPtr(Addr), Info) <> 0) then
    begin
      if Info.dli_fbase = ExeBaseAddress then
        Info.dli_fname := nil;
      FoundInModule := HMODULE(dlopen(PAnsiChar(UTF8Encode(Info.dli_fname)), RTLD_LAZY));
      if FoundInModule <> 0 then
        dlclose(FoundInModule);
      if Module = FoundInModule then
      begin
        if Assigned(Info.dli_fname) then
        begin
          Result := strlen(Info.dli_fname);
          if Result >= BufLen then Result := BufLen-1;
          Move(Info.dli_fname^, Buffer^, Result);
        end
        else
          Result := 0;
        Buffer[Result] := #0;
      end;
    end;
{$ENDIF LINUX}
{$IFDEF MACOS}
{   Iterate through the loaded modules using _dyld_get_image_name,
    comparing those module handles to the handle provided.
    Note that this function is not thread safe as indicies into the
    loaded object list may change if other threads are loading or
    unloading modules. Most of the time this will not be the case;
    failure mode is to just return an empty string. }

    Index := 0;
    Name := _dyld_get_image_name(Index);
    while Name <> nil do
    begin
      LoadedModule := dlopen(Name, RTLD_LAZY);
      dlclose(LoadedModule);

      if LoadedModule = Module then
      begin
        Result := StrLen(Name);
        if Result >= BufLen then Result := BufLen - 1;
        Result := UTF8ToUnicode(Buffer, BufLen, Name, Result);
        Exit(Result);
      end;

      Inc(Index);
      Name := _dyld_get_image_name(Index);
    end;
    Result := 0;
{$ENDIF MACOS}
  end;

  if Result < 0 then Result := 0;
end;
{$ENDIF POSIX}

function DelayLoadResourceModule(Module: PLibModule): HINST;
var
  FileName: array[0..MAX_PATH] of Char;
begin
  if Module.ResInstance = 0 then
  begin
    GetModuleFileName(Module.Instance, FileName, SizeOf(FileName));
    Module.ResInstance := LoadResourceModule(FileName);
    if Module.ResInstance = 0 then
      Module.ResInstance := Module.Instance;
  end;
  Result := Module.ResInstance;
end;

function FindResourceHInstance(Instance: HINST): HINST;
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if (Instance = CurModule.Instance) or
       (Instance = CurModule.CodeInstance) or
       (Instance = CurModule.DataInstance) then
    begin
      Result := DelayLoadResourceModule(CurModule);
      Exit;
    end;
    CurModule := CurModule.Next;
  end;
  Result := Instance;
end;

{$IFDEF LINUX}
function GetUILanguages(const LANGID: WORD): string;
var
  Lang: AnsiString;
  Ind: integer;
  languagePart: string;
begin
  // language[_territory][.codeset][@modifiers]
  // language and territory shall consist of LETTERS only.
  Lang := AnsiString(getenv('LANG'));
  Result := '';
  if Lang = '' then exit;

  languagePart := '';

  for ind := 1 to length(Lang) do
  begin
    if not(Lang[Ind] in ['a'..'z', 'A'..'Z', '_']) then
      break;
    if Lang[Ind] = '_' then languagePart := Result;
    Result := Result + WideChar(Lang[Ind]);
  end;

  if languagePart <> '' then
    Result :=  Result + ',' + languagePart;
end;

function InternalGetLocaleOverride(AppName: string): string;
begin
  Result := ''; // no override mechanism
end;
{$ENDIF LINUX}
{$IFDEF MACOS}
function StringRefToString(StringRef: CFStringRef): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := CFStringGetLength(StringRef);
  if Range.length > 0 then
  begin
    SetLength(Result, Range.length);
    CFStringGetCharacters(StringRef, Range, @Result[1]);
  end
  else
    Result := '';
end;

function GetUILanguages(const LANGID: WORD): string;
var
  PL : CFArrayRef;
  I: integer;
begin
  Result := '';
  PL := CFLocaleCopyPreferredLanguages;
  try
    if CFArrayGetCount(PL) > 0 then
    begin
      Result := StringRefToString(CFArrayGetValueAtIndex(PL, 0));
      for I := 1 to CFArrayGetCount(PL)-1 do
        Result := Result + ',' + StringRefToString(CFArrayGetValueAtIndex(PL, I));
    end;
  finally
    CFRelease(PL);
  end;
end;

function InternalGetLocaleOverride(AppName: string): string;
begin
  Result := ''; // no override mechanism
end;
{$ENDIF MACOS}

{$IFDEF MSWINDOWS}
type
  TLanguageEntry = record
    ID: WORD;
    List: PAnsiChar;
  end;

{$I LocaleData.INC }

var
  GetThreadPreferredUILanguages : function(dwFlags: LONGWORD; pulNumLanguages: Pointer;
    pwszLanguagesBuffer: PWideChar; pcchLanguagesBuffer: Pointer): Boolean; stdcall;
  SetThreadPreferredUILanguages : function(dwFlags: LONGWORD; pwszLanguagesBuffer: Pointer;
    pulNumLanguages: Pointer): Boolean; stdcall;
  GetThreadUILanguage : function : WORD; stdcall;
  UseThreadUILanguageAPI: Boolean;
  CrSec: TRTLCriticalSection;
  CachedLangID: Word;
  CachedLanguageNames: array[0.. LOCALE_NAME_MAX_LENGTH-1] of Char;

procedure InitializeLocaleData;
begin
  InitializeCriticalSection(CrSec);
  CachedLangID := $7f; //  LANG_INVARIANT
  UseThreadUILanguageAPI := (GetVersion and $000000FF) >= 6;
  if UseThreadUILanguageAPI then
  begin
    @GetThreadPreferredUILanguages := GetProcAddress(GetModuleHandle(kernel), 'GetThreadPreferredUILanguages');
    @SetThreadPreferredUILanguages := GetProcAddress(GetModuleHandle(kernel), 'SetThreadPreferredUILanguages');
    @GetThreadUILanguage:= GetProcAddress(GetModuleHandle(kernel), 'GetThreadUILanguage');
  end;
end;

procedure FinalizeLocaleDate;
begin
  DeleteCriticalSection(CrSec);
end;

function GetUILanguages(const LANGID: WORD): string;

  function LastHyphenPos(S : String) : integer;
  var
    I: integer;
  begin
    for I := Length(S) downto 1 do
      if S[I] = '-' then exit (I-1);
    Result := 0;
  end;

  function ConvertResToUILanguages(ResBuffer: PAnsiChar): String;
  var
    I: Integer;
    Separator,
    ALanguage: String;
  begin
    Result := String(PAnsiChar(ResBuffer));
    for I := 1 to Length(Result) do
      if Result[I] = ',' then exit;
    ALanguage := Result;
    Result := '';
    while ALanguage <> '' do
    begin
      Result := Result + Separator + ALanguage;
      Separator := ',';
      ALanguage := Copy(ALanguage, 1, LastHyphenPos(ALanguage));
    end;
  end;

  function GetPreferredLangForOldOS(LANGID: Word): string;
  var
    Language, Region : array[0.. LOCALE_NAME_MAX_LENGTH-1] of Char;
    H, L, I: Cardinal;
  begin
    Result := '';
    // Lookup exceptional languages table.
    if (NumberOfLocaleData > 0) and (LocaleTable[0].ID <= LANGID) and (LANGID <= LocaleTable[NumberOfLocaleData-1].ID) then
    begin
      H := NumberOfLocaleData-1;
      L := 0;
      while H >= L do
      begin
        I := (H + L) div 2;
        if LocaleTable[I].ID > LANGID then H := I - 1
        else if LocaleTable[I].ID < LANGID then L :=  I + 1
        else
        begin
          Result := ConvertResToUILanguages(LocaleTable[I].List);
          Break;
        end;
      end;
    end;
    if (Result = '') and IsValidLocale(LANGID, LCID_SUPPORTED) then
    begin
      // Generate language names: <language>-<country> and <language>
      GetLocaleInfo(LANGID, LOCALE_SISO639LANGNAME, Language, LOCALE_NAME_MAX_LENGTH);
      GetLocaleInfo(LANGID, LOCALE_SISO3166CTRYNAME, Region, LOCALE_NAME_MAX_LENGTH);
      Result := String(Language) + '-' + String(Region) + ',' + String(Language);
    end;
  end;

  function CheckDifferentLanguageList(src1, src2: PWideChar; len: integer): boolean;
  begin
    Result := True;
    while len > 0 do
    begin
      if (src1^ <> src2^) then exit;
      inc(src1);
      inc(src2);
      dec(len);
    end;
    Result := False;
  end;

  function ThreadUILanguages(var bufsize: Integer): PWideChar;
  var
    I: Integer;
  begin
    Result := nil;
    bufsize := 0;
    if GetThreadPreferredUILanguages(MUI_LANGUAGE_NAME or MUI_UI_FALLBACK, @I, nil, @bufsize) then
    begin
      GetMem(Result, bufsize * sizeof(Char));
      GetThreadPreferredUILanguages(MUI_LANGUAGE_NAME or MUI_UI_FALLBACK, @I, Result, @bufsize);
    end;
  end;

  function GetPreferredLangForNewOS(const LANGID: WORD): string;
  var
    SavedBufSize, BufSize: Integer;
    SavedUILanguages, UILanguages: PChar;
    I: integer;
    W: WORD;
    IDBuf: array[0..5] of WideChar; // four digits + two #0
  begin
    SavedUILanguages := nil;
    if GetThreadUILanguage <> LANGID then
    begin
      SavedUILanguages := ThreadUILanguages(SavedBufSize);
      W := LANGID;
      for I := 3 downto 0 do
      begin
        IDBuf[I] := WideChar(Ord(hexDigits[W and $0F]));
        W := W div 16;
      end;
      IDBuf[4] := #0; // Double null-terminator.
      IDBuf[5] := #0;
      SetThreadPreferredUILanguages(MUI_LANGUAGE_ID, @IDBuf, @I);
    end;

    UILanguages := ThreadUILanguages(BufSize);
    if UILanguages <> nil then
    begin
      for I := 0 to BufSize - 2 do
        if UILanguages[I] = #0 then UILanguages[I] := ',';
      Result := UILanguages;
      FreeMem(UILanguages);
    end;

    if SavedUILanguages <> nil then
    begin
      SetThreadPreferredUILanguages(0, nil, @I);
      UILanguages := ThreadUILanguages(BufSize);
      if (SavedBufSize <> BufSize) or CheckDifferentLanguageList(SavedUILanguages, UILanguages, BufSize) then
        SetThreadPreferredUILanguages(MUI_LANGUAGE_NAME, SavedUILanguages, @I);
      FreeMem(UILanguages);
      FreeMem(SavedUILanguages);
    end;
  end;

begin
  EnterCriticalSection(CrSec);
  if CachedLangID = LANGID then
  begin
    Result := CachedLanguageNames;
    LeaveCriticalSection(CrSec);
    exit;
  end;
  LeaveCriticalSection(CrSec);

  Result := '';
  if IsValidLocale(LANGID, LCID_SUPPORTED) then
  begin
    if UseThreadUILanguageAPI then
      Result := GetPreferredLangForNewOS(LANGID)
    else
    begin
      Result := GetPreferredLangForOldOS(LANGID);
      if LangID <> GetSystemDefaultUILanguage then
      begin
        if Result <> '' then Result := Result + ',';
        Result := Result + GetPreferredLangForOldOS(GetSystemDefaultUILanguage);
      end;
    end;
  end;

  EnterCriticalSection(CrSec);
  CachedLangID := LANGID;
  lstrcpyn(CachedLanguageNames, PChar(Result), SizeOf(CachedLanguageNames));
  LeaveCriticalSection(CrSec);
end;

function InternalGetLocaleOverride(AppName: string): string;

  function FindBS(Current: PChar): PChar;
  begin
    Result := Current;
    while (Result^ <> #0) and (Result^ <> '\') do
      Result := CharNext(Result);
  end;

  function ToLongPath(AFileName: PChar; BufLen: Integer): PChar;
  var
    CurrBS, NextBS: PChar;
    {$IFDEF CPU386}
    Module: Integer;
    {$ELSE}
    Module: HMODULE;
    {$ENDIF}
    Handle: THandle;
    L: Integer;
    FindData: TWin32FindData;
    Buffer: array[0..MAX_PATH] of Char;
    GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
      cchBuffer: Integer): Integer stdcall;
  const
    longPathName = 'GetLongPathNameW';
  begin
    Result := AFileName;
    Module := GetModuleHandle(kernel);
    if Module <> 0 then
    begin
      @GetLongPathName := GetProcAddress(Module, longPathName);
      if Assigned(GetLongPathName) and
        (GetLongPathName(AFileName, Buffer, Length(Buffer)) <> 0) then
      begin
        lstrcpyn(AFileName, Buffer, BufLen);
        Exit;
      end;
    end;

    if AFileName[0] = '\' then
    begin
      if AFileName[1] <> '\' then Exit;
      CurrBS := FindBS(AFileName + 2);  // skip server name
      if CurrBS^ = #0 then Exit;
      CurrBS := FindBS(CurrBS + 1);     // skip share name
      if CurrBS^ = #0 then Exit;
    end else
      CurrBS := AFileName + 2;          // skip drive name

    L := CurrBS - AFileName;
    lstrcpyn(Buffer, AFileName, L + 1);
    while CurrBS^ <> #0 do
    begin
      NextBS := FindBS(CurrBS + 1);
      if L + (NextBS - CurrBS) + 1 > Length(Buffer) then Exit;
      lstrcpyn(Buffer + L, CurrBS, (NextBS - CurrBS) + 1);

      Handle := FindFirstFile(Buffer, FindData);
      if Handle = THandle(-1) then Exit;
      FindClose(Handle);

      if L + 1 + _strlen(FindData.cFileName) + 1 > Length(Buffer) then Exit;
      Buffer[L] := '\';
      lstrcpyn(Buffer + L + 1, FindData.cFileName, Length(Buffer) - L - 1);
      Inc(L, _strlen(FindData.cFileName) + 1);
      CurrBS := NextBS;
    end;
    lstrcpyn(AFileName, Buffer, BufLen);
  end;

var
  HostAppName: array [0..MAX_PATH] of Char;
  LocaleOverride: PChar;
  Key: HKEY;
  LocSize: Integer;
begin
  if AppName = '' then
    GetModuleFileName(0, HostAppName, Length(HostAppName)) // Get host application name
  else
    lstrcpyn(HostAppName, PChar(AppName), Length(HostAppName));
  if HostAppName[0] = #$0 then exit;
  LocaleOverride := nil;

  if (RegOpenKeyEx(HKEY_CURRENT_USER, NewerLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewerLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OldLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OlderLocaleOverrideKey, 0, KEY_READ, Key) = 0) then
  try
    ToLongPath(HostAppName, Length(HostAppName));
    if RegQueryValueEx(Key, HostAppName, nil, nil, nil, @LocSize) = 0 then
    begin
      GetMem(LocaleOverride, LocSize);
      RegQueryValueEx(Key, HostAppName, nil, nil, PByte(LocaleOverride), @LocSize);
      Result := LocaleOverride;
    end
    else if RegQueryValueEx(Key, '', nil, nil, nil, @LocSize) = 0 then
    begin
      GetMem(LocaleOverride, LocSize);
      RegQueryValueEx(Key, '', nil, nil, PByte(LocaleOverride), @LocSize);
      Result := LocaleOverride;
    end;
  finally
    if LocaleOverride <> nil then
      FreeMem(LocaleOverride);
    RegCloseKey(Key);
  end;
end;
{$ENDIF MSWINDOWS}

var
  PreferredLanguagesOverride: PChar = nil;

function GetLocaleOverride(const AppName: string): string;
begin
  if PreferredLanguagesOverride = nil then
    SetLocaleOverride(InternalGetLocaleOverride(AppName));
  Result := PreferredLanguagesOverride;
end;

procedure SetLocaleOverride(const NewPreferredLanguages: string);
var
  L: Integer;
begin
  if PreferredLanguagesOverride <> nil then
    FreeMem(PreferredLanguagesOverride);
  L := Length(NewPreferredLanguages);
  if L > 0 then
  begin
    Inc(L);
    GetMem(PreferredLanguagesOverride, L * SizeOf(Char));
    MoveChars(NewPreferredLanguages[1], PreferredLanguagesOverride^, L);
  end;
end;

function GetResourceModuleName(HostAppName, ModuleName: string): string;

  function ResouceDLLExists(S: string): Boolean;
{$IFDEF POSIX}
  var
    st1: _stat;
  begin
    Result := stat(PAnsiChar(UTF8Encode(S)), st1) <> -1;
  end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  var
    Handle: THandle;
    FindData: TWin32FindData;
  begin
    Handle := FindFirstFile(PWideChar(S), FindData);
    Result := Handle <> THandle(-1);
    if Result then
      FindClose(Handle);
  end;
{$ENDIF}

  function LoadLanguageList(FileNameBody, List: String): string;
  Var
    s, ind : integer;
  begin
    Result := '';
    ind := 1;
    while (ind <= length(List)) do
    begin
      s := ind;
      while (ind <= Length(List)) and (List[ind] <> ',') do inc(ind);
      if s <> ind then
      begin
        Result := FileNameBody + Copy(List, s, ind-s);
        if ResouceDLLExists(Result) then exit;
{$IFDEF MACOS}
        // workaround for 282039.
        if Copy(List, s, ind-s) = 'en' then exit('');
{$ENDIF MACOS}
      end;
      inc(ind);
    end;
    Result := '';
  end;

{$IFDEF MACOS}
  function LoadLocalizedBundle(ModuleName: string): string;

    function CFSTR(const s: string): CFStringRef;
    begin
      Result := CFStringCreateWithCharacters(nil, PChar(s), Length(s));
    end;
    
  var
    ind: integer;
    resourceName, resourceType : CFStringRef;
    resourceDirectoryURL: CFURLRef;
    resourceDirectoryPath: CFStringRef;
  begin
    Result := '';
    for ind := length(ModuleName) downto 1 do 
      if ModuleName[ind] = '/' then
      begin
        ModuleName := copy(ModuleName, ind+1, MAXINT);
        break;
      end;

    resourceName := CFSTR(ModuleName);
    resourceType := CFSTR('resources');
    try
      resourceDirectoryURL := CFBundleCopyResourceURL(CFBundleGetMainBundle, resourceName, resourceType, nil);
      if resourceDirectoryURL <> nil then
      try
        resourceDirectoryPath := CFURLCopyFileSystemPath(resourceDirectoryURL, 0); // kCFURLPOSIXPathStyle
        try
          Result := StringRefToString(resourceDirectoryPath);
          if ResouceDLLExists(Result) then exit;
        finally
          CFRelease(resourceDirectoryPath);
        end;
      finally
        CFRelease(resourceDirectoryURL);
      end;
    finally
      CFRelease(resourceType);
      CFRelease(resourceName);
    end;
    Result := '';
  end;
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
  function Load3LettersModule(FileNameBody: string): string;
  var
    ExtPart : array[0..3] of char;
  begin
    GetLocaleInfo(GetUserDefaultUILanguage, LOCALE_SABBREVLANGNAME, ExtPart, SizeOf(ExtPart) div SizeOf(ExtPart[0]));
    Result := FileNameBody + string(ExtPart);
    if ResouceDLLExists(Result) then Exit;
    ExtPart[2] := #$0;
    Result := FileNameBody + string(ExtPart);
    if ResouceDLLExists(Result) then Exit;
    Result := '';
  end;
{$ENDIF MSWINDOWS}

var
  LocaleOverrideKey: string;
  FileNameBody : string;
  Ind: integer;
begin
  Result := '';
{$IFDEF MSWINDOWS}
  FileNameBody := ModuleName;
  for ind := Length(ModuleName) downto 1 do
  begin
    if ModuleName[ind] = '.' then
    begin
      FileNameBody := Copy(ModuleName, 1, ind);
      break;
    end;
  end;
{$ENDIF}
{$IFDEF POSIX}
  FileNameBody := ModuleName;
  for ind := Length(ModuleName) downto 1 do
  begin
    if ModuleName[ind] = '.' then
    begin
      if Copy(ModuleName, ind, MaxInt) = '.dylib' then
        FileNameBody := Copy(ModuleName, 1, ind);
      break;
    end;
  end;
  if FileNameBody[Length(FileNameBody)] <> '.' then
    FileNameBody := FileNameBody + '.';
{$ENDIF}
  LocaleOverrideKey := GetLocaleOverride(HostAppName);
  if LocaleOverrideKey <> '' then
    Result := LoadLanguageList(FileNameBody, LocaleOverrideKey)
  else
  begin
{$IFDEF POSIX}
    Result := LoadLanguageList(FileNameBody, GetUILanguages(0));
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    Result := LoadLanguageList(FileNameBody, GetUILanguages(GetUserDefaultUILanguage));
    if (Result = '') and (not UseThreadUILanguageAPI) then
      Result := LoadLanguageList(FileNameBody, GetUILanguages(GetSystemDefaultUILanguage));
    if Result = '' then
      Result := Load3LettersModule(FileNameBody);
{$ENDIF}
  end;
{$IFDEF MACOS}
  if Result = '' then 
    Result := LoadLocalizedBundle(ModuleName);
{$ENDIF}

end;

{$IFDEF POSIX}
function LoadModule(ModuleName, ResModuleName: string; CheckOwner: Boolean): LongWord;
var
  st1, st2: _stat;
  ModuleFileName, ResModuleFileName: UTF8String;
begin
{ Security check:  make sure the user id (owner) and group id of
  the base module matches the user id and group id of the resource
  module we're considering loading.  This is to prevent loading
  of malicious code dropped into the base module's directory by
  a hostile user.  The app and all its resource modules must
  have the same owner and group.  To disable this security check,
  call this function with CheckOwner set to False. }

  Result := 0;

  ModuleFileName := UTF8Encode(ModuleName);
  if CheckOwner and (stat(PAnsiChar(ModuleFileName), st1) = -1) then
    Exit;
    
  ResModuleFileName := UTF8Encode(ResModuleName);

  if (not CheckOwner) or
    ((stat(PAnsiChar(ResModuleFileName), st2) <> -1)
     and (st1.st_uid = st2.st_uid)
     and (st1.st_gid = st2.st_gid)) then
    Result := dlopen(PAnsiChar(ResModuleFileName), RTLD_LAZY);
end;
{$ENDIF}

function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean): LongWord;
var
  HostAppName: array [0..MAX_PATH] of Char;
  ResModuleName : string;
begin
  Result := 0;
  GetModuleFileName(0, HostAppName, Length(HostAppName));
  ResModuleName := GetResourceModuleName(HostAppName, ModuleName);
  if ResModuleName <> '' then
{$IFDEF MSWINDOWS}
    Result := LoadLibraryEx(PChar(ResModuleName), 0, LOAD_LIBRARY_AS_DATAFILE)
{$ENDIF}
{$IFDEF POSIX}
    Result := LoadModule(ModuleName, ResModuleName, CheckOwner);
{$ENDIF}
end;

procedure EnumModules(Func: TEnumModuleFunc; Data: Pointer);
begin
  EnumModules(TEnumModuleFuncLW(Func), Data);
end;

procedure EnumResourceModules(Func: TEnumModuleFunc; Data: Pointer);
begin
  EnumResourceModules(TEnumModuleFuncLW(Func), Data);
end;

procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer);
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if not Func(CurModule.Instance, Data) then Exit;
    CurModule := CurModule.Next;
  end;
end;

procedure EnumResourceModules(Func: TEnumModuleFuncLW; Data: Pointer);
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if not Func(DelayLoadResourceModule(CurModule), Data) then Exit;
    CurModule := CurModule.Next;
  end;
end;

procedure AddModuleUnloadProc(Proc: TModuleUnloadProc);
begin
  AddModuleUnloadProc(TModuleUnloadProcLW(Proc));
end;

procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProc);
begin
  RemoveModuleUnloadProc(TModuleUnloadProcLW(Proc));
end;

procedure AddModuleUnloadProc(Proc: TModuleUnloadProcLW);
var
  P: PModuleUnloadRec;
begin
  New(P);
  P.Next := ModuleUnloadList;
  @P.Proc := @Proc;
  ModuleUnloadList := P;
end;

procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProcLW);
var
  P, C: PModuleUnloadRec;
begin
  P := ModuleUnloadList;
  if (P <> nil) and (@P.Proc = @Proc) then
  begin
    ModuleUnloadList := ModuleUnloadList.Next;
    Dispose(P);
  end else
  begin
    C := P;
    while C <> nil do
    begin
      if (C.Next <> nil) and (@C.Next.Proc = @Proc) then
      begin
        P := C.Next;
        C.Next := C.Next.Next;
        Dispose(P);
        Break;
      end;
      C := C.Next;
    end;
  end;
end;

procedure NotifyModuleUnload(HInstance: LongWord);
var
  P: PModuleUnloadRec;
begin
  P := ModuleUnloadList;
  while P <> nil do
  begin
    try
      P.Proc(HInstance);
    except
      // Make sure it doesn't stop notifications
    end;
    P := P.Next;
  end;
{$IFDEF LINUX}
  InvalidateModuleCache;
{$ENDIF}
end;

procedure RegisterModule(LibModule: PLibModule);
begin
  LibModule.Next := LibModuleList;
  LibModuleList := LibModule;
end;

procedure UnregisterModule(LibModule: PLibModule);
var
  CurModule: PLibModule;
begin
  try
    NotifyModuleUnload(LibModule.Instance);
  finally
    if LibModule = LibModuleList then
      LibModuleList := LibModule.Next
    else
    begin
      CurModule := LibModuleList;
      while CurModule <> nil do
      begin
        if CurModule.Next = LibModule then
        begin
          CurModule.Next := LibModule.Next;
          Break;
        end;
        CurModule := CurModule.Next;
      end;
    end;
  end;
end;

function _IntfClear(var Dest: IInterface): Pointer;
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  Result := @Dest;
  if Dest <> nil then
  begin
    P := Pointer(Dest);
    Pointer(Dest) := nil;
    IInterface(P)._Release;
  end;
end;
{$ELSE}
asm
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        MOV     DWORD PTR [EAX],0
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
@@1:
end;
{$ENDIF !PUREPASCAL}

procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := Pointer(Dest);
  if Source <> nil then
    Source._AddRef;
  Pointer(Dest) := Pointer(Source);
  if P <> nil then
    IInterface(P)._Release;
end;
{$ELSE}
asm
{
  The most common case is the single assignment of a non-nil interface
  to a nil interface.  So we streamline that case here.  After this,
  we give essentially equal weight to other outcomes.

    The semantics are:  The source intf must be addrefed *before* it
    is assigned to the destination.  The old intf must be released
    after the new intf is addrefed to support self assignment (I := I).
    Either intf can be nil.  The first requirement is really to make an
    error case function a little better, and to improve the behaviour
    of multithreaded applications - if the addref throws an exception,
    you don't want the interface to have been assigned here, and if the
    assignment is made to a global and another thread references it,
    again you don't want the intf to be available until the reference
    count is bumped.
}
        TEST    EDX,EDX         // is source nil?
        JE      @@NilSource
        PUSH    EDX             // save source
        PUSH    EAX             // save dest
        MOV     EAX,[EDX]       // get source vmt
        PUSH    EDX             // source as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._AddRef
        POP     EAX             // retrieve dest
        MOV     ECX, [EAX]      // get current value
        POP     [EAX]           // set dest in place
        TEST    ECX, ECX        // is current value nil?
        JNE     @@ReleaseDest   // no, release it
        RET                     // most common case, we return here
@@ReleaseDest:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EAX,[ECX]       // get current value vmt
        PUSH    ECX             // current value as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        RET

{   Now we're into the less common cases.  }
@@NilSource:
        MOV     ECX, [EAX]      // get current value
        TEST    ECX, ECX        // is it nil?
        MOV     [EAX], EDX      // store in dest (which is nil)
        JE      @@Done
        MOV     EAX, [ECX]      // get current vmt
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ECX             // current value as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
@@Done:
end;
{$ENDIF !PUREPASCAL}

procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
{$IF defined(PUREPASCAL) or defined(PIC)}
// PIC:  EBX must be correct before calling QueryInterface
var
  Temp: Pointer;
begin
  if Source = nil then
    Dest := nil
  else
  begin
    Temp := nil;
    if Source.QueryInterface(IID, IInterface(Temp)) <> 0 then
      ErrorAt(byte(reIntfCastError), ReturnAddress)
    else
    begin
      if Assigned(Dest) then
          Dest._Release;
      Pointer(Dest) := Temp;
    end;
  end;
end;
{$ELSE}
asm
        TEST    EDX,EDX
        JE      _IntfClear
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        MOV     EDI, EAX   // ptr to dest
        PUSH    0
        PUSH    ESP        // ptr to temp
        PUSH    ECX        // ptr to GUID
        PUSH    EDX        // ptr to source
@@1:    MOV     EAX,[EDX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface.QueryInterface
        TEST    EAX,EAX
        JE      @@2
{$IFDEF ALIGN_STACK}
        ADD     ESP, 16
{$ENDIF ALIGN_STACK}
        MOV     AL,reIntfCastError
        JMP     Error
@@2:    MOV     EAX, [EDI]
        TEST    EAX, EAX
        JE      @@3
        PUSH    EAX
        MOV     EAX,[EAX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
@@3:    POP     EAX          // value of temp
        MOV     [EDI], EAX
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$IFEND !PUREPASCAL or !PIC}

procedure _IntfAddRef(const Dest: IInterface);
begin
  if Dest <> nil then Dest._AddRef;
end;

procedure TInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TInterfacedObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    Error(reInvalidPtr);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedObject(Result).FRefCount := 1;
end;

function TInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TAggregatedObject }

constructor TAggregatedObject.Create(const Controller: IInterface);
begin
  // weak reference to controller - don't keep it alive
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedObject._Release: Integer; stdcall;
begin
  Result := IInterface(FController)._Release;
end;

{ TContainedObject }

function TContainedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TClassHelperBase }

constructor TClassHelperBase._Create(Instance: TObject);
begin
  inherited Create;
  FInstance := Instance;
end;

function _CheckAutoResult(ResultCode: HResult): HResult;
begin
  if ResultCode < 0 then
  begin
    if Assigned(SafeCallErrorProc) then
      SafeCallErrorProc(ResultCode, ReturnAddress);
    ErrorAt(Byte(reSafeCallError), ReturnAddress);
  end;
  Result := ResultCode;
end;

function  CompToDouble(Value: Comp): Double; cdecl;
begin
  Result := Value;
end;

procedure  DoubleToComp(Value: Double; var Result: Comp); cdecl;
begin
  Result := Value;
end;

function  CompToCurrency(Value: Comp): Currency; cdecl;
begin
  Result := Value;
end;

procedure  CurrencyToComp(Value: Currency; var Result: Comp); cdecl;
begin
  Result := Value;
end;

function GetMemory(Size: NativeInt): Pointer; cdecl;
begin
  Result := MemoryManager.GetMem(Size);
end;

function FreeMemory(P: Pointer): Integer; cdecl;
begin
  if P = nil then
    Result := 0
  else
    Result := MemoryManager.FreeMem(P);
end;

function ReallocMemory(P: Pointer; Size: NativeInt): Pointer; cdecl;
begin
  if P = nil then
    Result := GetMemory(Size)
  else
  Result := MemoryManager.ReallocMem(P, Size);
end;

// UnicodeToUTF8(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer;
begin
  Result := UnicodeToUtf8(Dest, MaxBytes, Source, Cardinal(-1));
end;

// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  if Dest <> nil then
  begin
    Result := Cardinal(LocaleCharsFromUnicode(CP_UTF8, 0, Source, Integer(SourceChars), Dest, Integer(MaxDestBytes), nil, nil));
    if (Result > 0) and (Result <= MaxDestBytes) then
    begin
      if (SourceChars = Cardinal(-1)) and (Dest[Result -1] = #0) then Exit;

      if Result = MaxDestBytes then
      begin
        while (Result > 1) and (Byte(Dest[Result - 1]) > $7F) and (Byte(Dest[Result - 1]) and $80 <> 0) and (Byte(Dest[Result - 1]) and $C0 <> $C0) do
          Dec(Result);
      end else
        Inc(Result);
      Dest[Result - 1] := #0;
    end;
  end else
    Result := Cardinal(LocaleCharsFromUnicode(CP_UTF8, 0, Source, Integer(SourceChars), nil, 0, nil, nil));
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer;
begin
  Result := Utf8ToUnicode(Dest, MaxChars, Source, Cardinal(-1));
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  if (Dest <> nil) and (MaxDestChars > 0) then
  begin
    Result := Cardinal(UnicodeFromLocaleChars(CP_UTF8, 0, Source, Integer(SourceBytes), Dest, Integer(MaxDestChars)));
    if (Result > 0) and (Result <= MaxDestChars) then
    begin
      if (SourceBytes = Cardinal(-1)) and (Dest[Result - 1] = #0) then Exit;

      if Result = MaxDestChars then
      begin
        if (Result > 1) and (Word(Dest[Result - 1]) >= $DC00) and (Word(Dest[Result - 1]) <= $DFFF) then
          Dec(Result);
      end else
        Inc(Result);
      Dest[Result - 1] := #0;
    end;
  end else
    Result := Cardinal(UnicodeFromLocaleChars(CP_UTF8, 0, Source, Integer(SourceBytes), nil, 0));
end;

function Utf8Encode(const WS: WideString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  L := Length(WS);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(WS), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    PStrRec(PByte(Result) - SizeOf(StrRec)).codePage := CP_UTF8;
end;

function Utf8Encode(const US: UnicodeString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if US = '' then Exit;
  L := Length(US);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(US), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    PStrRec(PByte(Result) - SizeOf(StrRec)).codePage := CP_UTF8;
end;

function UTF8Encode(const A: RawByteString): RawByteString;
begin
  if StringCodePage(A) = CP_UTF8 then
    Result := A
  else
    Result := UTF8Encode(UnicodeString(A));
end;

function Max(I1, I2: Integer): Integer; inline;
begin
  if I1 > I2 then
    Result := I1
  else
    Result := I2;
end;

function UTF8EncodeToShortString(const WS: WideString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(WS), Length(WS)) - 1));
end;

function UTF8EncodeToShortString(const US: UnicodeString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(US), Length(US)) - 1));
end;

function UTF8EncodeToShortString(const A: RawByteString): ShortString;
begin
  if StringCodePage(A) = CP_UTF8 then
    Result := A
  else
    Result := UTF8EncodeToShortString(UnicodeString(A));
end;

function Utf8Decode(const S: RawByteString): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToWideString(const S: RawByteString): WideString; inline;
begin
  Result := UTF8Decode(S);
end;

function UTF8ToUnicodeString(const S: RawByteString): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, S, L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, @S[1], L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToString(const S: RawByteString): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function UTF8ToString(const S: ShortString): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function UTF8ToString(const S: PAnsiChar): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function UTF8ToString(const S: array of AnsiChar): string;
begin
  Result := UTF8ToUnicodeString(@S[0]);
end;

function AnsiToUtf8(const S: string): RawByteString;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: RawByteString): string;
begin
  Result := string(Utf8ToUnicodeString(S));
end;

{$IFDEF LINUX}

function GetCPUType: Integer;
asm
      PUSH      EBX
    // this code assumes ESP is 4 byte aligned
    // test for 80386:  see if bit #18 of EFLAGS (Alignment fault) can be toggled
      PUSHFD
      POP       EAX
      MOV       ECX, EAX
      XOR       EAX, $40000   // flip AC bit in EFLAGS
      PUSH      EAX
      POPFD
      PUSHFD
      POP       EAX
      XOR       EAX, ECX      // zero = 80386 CPU (can't toggle AC bit)
      MOV       EAX, CPUi386
      JZ        @@Exit
      PUSH      ECX
      POPFD                    // restore original flags before next test

      // test for 80486:  see if bit #21 of EFLAGS (CPUID supported) can be toggled
      MOV       EAX, ECX        // get original EFLAGS
      XOR       EAX, $200000    // flip CPUID bit in EFLAGS
      PUSH      EAX
      POPFD
      PUSHFD
      POP       EAX
      XOR       EAX, ECX    // zero = 80486 (can't toggle EFLAGS bit #21)
      MOV       EAX, CPUi486
      JZ        @@Exit

      // Use CPUID instruction to get CPU family
      XOR       EAX, EAX
      CPUID
      CMP       EAX, 1
      JL        @@Exit          // unknown processor response: report as 486
      XOR       EAX, EAX
      INC       EAX       // we only care about info level 1
      CPUID
      AND       EAX, $F00
      SHR       EAX, 8
      // Test8086 values are one less than the CPU model number, for historical reasons
      DEC       EAX

@@Exit:
      POP       EBX
end;

{$ENDIF LINUX}

{$IFDEF LINUX_OLD_RESOURCES }
const
  sResSymExport = '@Sysinit@ResSym';
  sResStrExport = '@Sysinit@ResStr';
  sResHashExport = '@Sysinit@ResHash';

type
  TElf32Sym = record
    Name: Cardinal;
    Value: Pointer;
    Size: Cardinal;
    Info: Byte;
    Other: Byte;
    Section: Word;
  end;
  PElf32Sym = ^TElf32Sym;

  TElfSymTab = array [0..0] of TElf32Sym;
  PElfSymTab = ^TElfSymTab;

  TElfWordTab = array [0..2] of Cardinal;
  PElfWordTab = ^TElfWordTab;


{ If Name encodes a numeric identifier, return it, else return -1.  }
function NameToId(Name: PChar): Longint;
var digit: Longint;
begin
  if LongInt(IntPtr(Name)) and $ffff0000 = 0 then
  begin
    Result := LongInt(IntPtr(Name)) and $ffff;
  end
  else if Name^ = '#' then
  begin
    Result := 0;
    inc (Name);
    while (Ord(Name^) <> 0) do
    begin
      digit := Ord(Name^) - Ord('0');
      if (LongWord(digit) > 9) then
      begin
        Result := -1;
        exit;
      end;
      Result := Result * 10 + digit;
      inc (Name);
    end;
  end
  else
    Result := -1;
end;


// Return ELF hash value for NAME converted to lower case.
function ElfHashLowercase(Name: PChar): Cardinal;
var
  g: Cardinal;
  c: Char;
begin
  Result := 0;
  while name^ <> #0 do
  begin
    c := name^;
    c := tolower(c);
    Result := (Result shl 4) + Ord(c);
    g := Result and $f0000000;
    Result := (Result xor (g shr 24)) and not g;
    Inc(name);
  end;
end;

type
  PFindResourceCache = ^TFindResourceCache;
  TFindResourceCache = record
    ModuleHandle: HMODULE;
    Version: Cardinal;
    SymbolTable: PElfSymTab;
    StringTable: PChar;
    HashTable: PElfWordTab;
    BaseAddress: Pointer;
  end;

threadvar
  FindResourceCache: TFindResourceCache;

function GetResourceCache(ModuleHandle: HMODULE): PFindResourceCache;
var
  Info: dl_info;
begin
  Result := @FindResourceCache;
  if (ModuleHandle <> Result^.ModuleHandle) or (ModuleCacheVersion <> Result^.Version) then
  begin
    Result^.SymbolTable := dlsym(ModuleHandle, sResSymExport);
    Result^.StringTable := dlsym(ModuleHandle, sResStrExport);
    Result^.HashTable := dlsym(ModuleHandle, sResHashExport);
    Result^.ModuleHandle := ModuleHandle;
    if (dladdr(Result^.HashTable, Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
      Result^.BaseAddress := nil   // if it's not in a library, assume the exe
    else
      Result^.BaseAddress := Info.dli_fbase;
    Result^.Version := ModuleCacheVersion;
  end;
end;

function FindResource(ModuleHandle: HMODULE; ResourceName: PChar; ResourceType: PChar): TResourceHandle;
var
  P: PFindResourceCache;
  nid, tid: Longint;
  ucs2_key: array [0..2] of WideChar;
  key: array [0..127] of Char;
  len: Integer;
  pc: PChar;
  ch: Char;
  nbucket: Cardinal;
  bucket, chain: PElfWordTab;
  syndx: Cardinal;
begin
  Result := 0;
  if ResourceName = nil then Exit;
  P := GetResourceCache(ModuleHandle);

  tid := NameToId (ResourceType);
  if tid = -1 then Exit;  { not supported (yet?) }

  { This code must match util-common/elfres.c }
  nid := NameToId (ResourceName);
  if nid = -1 then
  begin
    ucs2_key[0] := WideChar(2*tid+2);
    ucs2_key[1] := WideChar(0);
    len := UnicodeToUtf8 (key, ucs2_key, SizeOf (key)) - 1;
    pc := key+len;
    while Ord(ResourceName^) <> 0 do
    begin
      ch := ResourceName^;
      if Ord(ch) > 127 then exit; { insist on 7bit ASCII for now }
      if ('A' <= ch) and (ch <= 'Z') then Inc(ch, Ord('a') - Ord('A'));
      pc^ := ch;
      inc (pc);
      if pc = key + SizeOf(key) then exit;
      inc (ResourceName);
    end;
    pc^ := Char(0);
  end
  else
  begin
    ucs2_key[0] := WideChar(2*tid+1);
    ucs2_key[1] := WideChar(nid);
    ucs2_key[2] := WideChar(0);
    UnicodeToUtf8 (key, ucs2_key, SizeOf (key));
  end;

  with P^ do
  begin
    nbucket := HashTable[0];
  //  nsym := HashTable[1];
    bucket := @HashTable[2];
    chain := @HashTable[2+nbucket];

    syndx := bucket[ElfHashLowercase(key) mod nbucket];
    while (syndx <> 0)
      and (strcasecmp(key, @StringTable[SymbolTable[syndx].Name]) <> 0) do
      syndx := chain[syndx];

    if syndx = 0 then
      Result := 0
    else
      Result := TResourceHandle(@SymbolTable[syndx]);
  end;
end;

function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
var
  P: PFindResourceCache;
begin
  if ResHandle <> 0 then
  begin
    P := GetResourceCache(ModuleHandle);
    Result := HGLOBAL(PElf32Sym(ResHandle)^.Value);
    Inc(NativeUInt(Result), UIntPtr(P^.BaseAddress));
  end
  else
    Result := 0;
end;

function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  if ResHandle <> 0 then
    Result := PElf32Sym(ResHandle)^.Size
  else
    Result := 0;
end;

function LockResource(ResData: HGLOBAL): Pointer;
begin
  Result := Pointer(ResData);
end;

function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  Result := False;
end;

function FreeResource(ResData: HGLOBAL): LongBool;
begin
  Result := True;
end;
{$ENDIF LINUX_OLD_RESOURCES}

{$IFDEF POSIX}
const
  sResSymExport = 'SysinitResSym';

type
  TResourceHeader = record
    SymtabOffset: Cardinal;
    SymbolCount: Integer;
    HashtableOffset: Cardinal;
  end;
  PResourceHeader = ^TResourceHeader;
  TResourceSym = record
    Name:  Cardinal;
    Data:  Cardinal;
    Size:  Cardinal;
    Chain: Cardinal;            // link to next hash entry, or -1 for end
  end;
  PResourceSym = ^TResourceSym;

  Hashtable = record
    Size: Cardinal;
    Entries: array [0..0] of Cardinal;
  end;
  PHashtable = ^Hashtable;

  TResourceSymTab = array [0..0] of TResourceSym;
  PResourceSymTab = ^TResourceSymTab;

{ If Name encodes a numeric identifier, return it, else return -1.  }
function NameToId(Name: PChar): Longint;
var digit: Longint;
begin
  if LongInt(IntPtr(Name)) and $ffff0000 = 0 then
  begin
    Result := LongInt(IntPtr(Name)) and $ffff;
  end
  else if Name^ = '#' then
  begin
    Result := 0;
    inc (Name);
    while (Ord(Name^) <> 0) do
    begin
      digit := Ord(Name^) - Ord('0');
      if (LongWord(digit) > 9) then
      begin
        Result := -1;
        exit;
      end;
      Result := Result * 10 + digit;
      inc (Name);
    end;
  end
  else
    Result := -1;
end;


// Return ELF hash value for NAME converted to lower case.
function ElfHashLowercase(Name: PAnsiChar): Cardinal;
var
  g: Cardinal;
  c: AnsiChar;
begin
  Result := 0;
  while name^ <> #0 do
  begin
    c := name^;
    c := AnsiChar(towlower(UCS4Char(c)));
    Result := (Result shl 4) + Ord(c);
    g := Result and $f0000000;
    Result := (Result xor (g shr 24)) and not g;
    Inc(name);
  end;
end;

type
  PFindResourceCache = ^TFindResourceCache;
  TFindResourceCache = record
    ModuleHandle: HMODULE;
    Version: Cardinal;
    ResourceHeader: PResourceHeader;
    SymbolTable: PResourceSymTab;
    StringTable: PAnsiChar;
    BaseAddress: Pointer;
    Hashtable: PHashtable;
  end;

threadvar
  FindResourceCache: TFindResourceCache;

function GetResourceCache(ModuleHandle: HMODULE): PFindResourceCache;
var
  info: dl_info;
begin
  Result := @FindResourceCache;
  if (ModuleHandle <> Result^.ModuleHandle) or (ModuleCacheVersion <> Result^.Version) then
  begin
    Result^.ResourceHeader := dlsym(ModuleHandle, sResSymExport);
    Result^.SymbolTable := PResourceSymTab((PByte(Result^.ResourceHeader) +
                                               SizeOf(TResourceHeader)));
    Result^.StringTable := PAnsiChar((PByte(Result^.ResourceHeader) +
                                         SizeOf(TResourceHeader) +
                                         SizeOf(TResourceSym) * Result^.ResourceHeader^.SymbolCount));
    Result^.ModuleHandle := ModuleHandle;
    if (dladdr(UIntPtr(Result^.ResourceHeader), Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
      Result^.BaseAddress := nil   // if it's not in a library, assume the exe
    else
      Result^.BaseAddress := Info.dli_fbase;
    Result^.Version := ModuleCacheVersion;
    Result^.Hashtable := PHashtable( PByte(Result^.ResourceHeader)
                                        +  Result^.ResourceHeader^.HashtableOffset);
  end;
end;

// UCS4cToUTF8c matches code in util-common, and must be kept in sync.
function UCS4cToUTF8c(Dest: PAnsiChar; Src: UInt32): Integer;
begin
  if ((Src and not $7F) = 0) then begin
    Dest^ := AnsiChar(Src);
    Result := 1;
  end
  else if ((Src and not $7FF) = 0) then begin
    Dest^ := AnsiChar($C0 or (Src shr 6));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 2;
  end
  else if ((Src and not $FFFF) = 0) then begin
    Dest^ := AnsiChar($E0 or (Src shr 12));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 6) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 3;
  end
  else if ((Src and not $1FFFFF) = 0) then begin
    Dest^ := AnsiChar($F0 or (Src shr 18));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 12) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 6) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 4;
  end
  else if ((Src and not $3FFFFFF) = 0) then begin
    Dest^ := AnsiChar($F8 or (Src shr 24));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 18) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 12) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 16) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 5;
  end
  else if ((Src and not $7FFFFFFFF) = 0) then begin
    Dest^ := AnsiChar($F0 or (Src shr 30));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 24) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 18) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 12) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 16) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 6;
  end
  else
     Result := -1;
end;

function FindResource(ModuleHandle: HMODULE; ResourceName: PChar; ResourceType: PChar): TResourceHandle;
var
  ResCacheEntry: PFindResourceCache;
  nid, tid: Longint;
  ucs2_key: array [0..2] of WideChar;
  key: array [0..127] of AnsiChar;
  len: Integer;
  pc: PAnsiChar;
  ch: AnsiChar;
  symName: PAnsiChar;
  utf8ResName: PAnsiChar;
  I: Integer;
  syndx: Cardinal;
begin
  Result := 0;
  if ResourceName = nil then Exit;
  ResCacheEntry := GetResourceCache(ModuleHandle);

  tid := NameToId (ResourceType);
  if tid = -1 then Exit;  { not supported (yet?) }

  { This code must match util-common/elfres.c }
  nid := NameToId (ResourceName);
  if nid = -1 then
  begin
    ucs2_key[0] := WideChar(2*tid+2);
    ucs2_key[1] := WideChar(0);
    len := UCS4cToUTF8c(key, 2*tid+2);
    pc := key+len;
    utf8ResName := PAnsiChar(AnsiString(ResourceName));
    while Ord(utf8ResName^) <> 0 do
    begin
      ch := utf8ResName^;
      if Ord(ch) > 127 then exit; { insist on 7bit ASCII for now }
      if ('A' <= ch) and (ch <= 'Z') then Inc(ch, Ord('a') - Ord('A'));
      pc^ := ch;
      inc (pc);
      if pc = key + SizeOf(key) then exit;
      inc (utf8ResName);
    end;
    pc^ := Char(0);
  end
  else
  begin
    len := UCS4cToUTF8c(key, 2*tid+1);
    len := len + UCS4cToUTF8c(key + len, nid);
    key[len] := #0;
  end;

  with ResCacheEntry^ do
  begin
    syndx := ElfHashLowercase(key) mod Hashtable^.Size;
    I := Hashtable^.Entries[syndx];
    while I <> -1 do
    begin
      symName := StringTable + SymbolTable[I].Name;
      if (strcasecmp(key, symName) = 0) then
      begin
        Result := TResourceHandle(@SymbolTable[I]);
        Exit;
      end
      else
      begin
        I := SymbolTable[I].Chain;
      end;
    end;
  end;

end;

function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
var
  ResCacheEntry: PFindResourceCache;
begin
  if ResHandle <> 0 then
  begin
    ResCacheEntry := GetResourceCache(ModuleHandle);
    Result := HGLOBAL(PResourceSym(ResHandle)^.Data + HGLOBAL(ResCacheEntry^.ResourceHeader));
  end
  else
    Result := 0;
end;

function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  if ResHandle <> 0 then
    Result := PResourceSym(ResHandle)^.Size
  else
    Result := 0;
end;

function LockResource(ResData: HGLOBAL): Pointer;
begin
  Result := Pointer(ResData);
end;

function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  Result := False;
end;

function FreeResource(ResData: HGLOBAL): LongBool;
begin
  Result := True;
end;
{$ENDIF POSIX}

{ ResString support function }

{$IFDEF MSWINDOWS}
function LoadResString(ResStringRec: PResStringRec): string;
var
  Buffer: array [0..4095] of Char;
begin
  if ResStringRec = nil then Exit;
  if ResStringRec.Identifier < 64*1024 then
    SetString(Result, Buffer,
      LoadString(FindResourceHInstance(ResStringRec.Module^),
        ResStringRec.Identifier, Buffer, Length(Buffer)))
  else
    Result := PChar(ResStringRec.Identifier);
end;
{$ENDIF}

{$IFDEF POSIX}

const
  ResStringTableLen = 16;

type
  ResStringTable = array [0..ResStringTableLen-1] of LongWord;

function LoadResString(ResStringRec: PResStringRec): string;
var
  Handle: TResourceHandle;
  Tab: ^ResStringTable;
  ResMod: HMODULE;
  blk: Integer;
begin
  if ResStringRec = nil then Exit;
  ResMod := FindResourceHInstance(ResStringRec^.Module^);
  //P := GetResourceCache(ModuleHandle);
  blk := ResStringRec^.Identifier div ResStringTableLen;
  if blk = 0 then    // can't pass 'nil' to FindResource. use alternate scheme for 0 instead
    Handle := FindResource(ResMod, '#0', PChar(6)) // RT_STRING
  else
    Handle := FindResource(ResMod,
//         PChar(ResStringRec^.Identifier),
       PChar(PByte(blk)),
//       PChar(((blk + 1) shl 16) or $FFFF),
       PChar(6));   // RT_STRING
  Tab := Pointer(LoadResource(ResMod, Handle));
  if Tab = nil then
    Result := ''
  else
  begin
    Result := PWideChar(PAnsiChar(Tab) + Tab[ResStringRec^.Identifier mod ResStringTableLen]);
  end;
end;
{$ENDIF POSIX}

{$IFDEF LINUX}
{ The Win32 program loader sets up the first 64k of process address space
  with no read or write access, to help detect use of invalid pointers
  (whose integer value is 0..64k).  Linux doesn't do this.  Mac OS/X
  does allow this, and the linker will ensure that this is the case by
  reserving 64k at the start of the image.

  Parts of the Delphi RTL and IDE design environment
  rely on the notion that pointer values in the [0..64k] range are
  invalid pointers.  To accomodate this in Linux, we reserve the range
  at startup.  If the range is already allocated, we keep going anyway. }

var
  ZeroPageReserved: Boolean = False;

procedure ReserveZeroPage;
const
  PROT_NONE = 0;
  MAP_PRIVATE   = $02;
  MAP_FIXED     = $10;
  MAP_ANONYMOUS = $20;
var
  P: Pointer;
begin
  if IsLibrary then Exit;  // page reserve is app's job, not .so's

  if not ZeroPageReserved then
  begin
    P := mmap(nil, High(Word), PROT_NONE,
      MAP_ANONYMOUS or MAP_PRIVATE or MAP_FIXED, 0, 0);
    ZeroPageReserved := P = nil;
    if (IntPtr(P) <> -1) and (P <> nil) then  // we didn't get it
      munmap(P, High(Word));
  end;
end;

procedure ReleaseZeroPage;
begin
  if ZeroPageReserved then
  begin
    munmap(nil, High(Word) - 4096);
    ZeroPageReserved := False;
  end;
end;
{$ENDIF}

var
  xxNull: UCS4Char = 0;
  xxPNull: PUCS4Char = @xxNull;

function PUCS4Chars(const S: UCS4String): PUCS4Char;
begin
  if Length(S) > 0 then
    Result := @S[0]
  else
    Result := xxPNull;
end;

function WideStringToUCS4String(const S: WideString): UCS4String;
var
  I: Integer;
  CharCount: Integer;
begin
  CharCount := 0;
  SetLength(Result, Length(S) + 1);
  I := 0;

  while I < Length(S) do
  begin

    if ((S[I + 1] >= #$D800) and (S[I + 1] <= #$DFFF)) and (I + 1 < Length(S)) then
    begin
      Result[CharCount] := UCS4Char((Cardinal(S[I + 1]) and $000003FF) shl 10 or (Cardinal(S[I + 2]) and $000003FF) + $00010000);
      Inc(I);
    end
    else
      Result[CharCount] := UCS4Char(S[I + 1]);

    Inc(CharCount);
    Inc(I);
  end;
  Result[CharCount] := 0;
  SetLength(Result, CharCount + 1);
end;

function UCS4StringToWideString(const S: UCS4String): WideString;
var
  I: Integer;
  CharCount: Integer;
begin
  SetLength(Result, Length(S) * 2 - 1); //Maximum possible number of characters
  CharCount := 0;

  I := 0;
  while I < Length(S) - 1 do
  begin
    if S[I] >= $10000 then
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar((((S[I] - $00010000) shr 10) and $000003FF) or $D800);
      Inc(CharCount);
      Result[CharCount] := WideChar(((S[I] - $00010000) and $000003FF)or $DC00);
    end
    else
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar(S[I]);
    end;

    Inc(I);
  end;

  SetLength(Result, CharCount);
end;

{$IFDEF POSIX}
type
  TCodePageMapEntry = record
    LocaleName: string;
    CodePage: Cardinal;
  end;

const
  // Predefined set of Name <=> CP mappings for POSIX
  CodePageMapA: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'ar'; CodePage: 1256),
    (LocaleName: 'az-cyrl'; CodePage: 1251),
    (LocaleName: 'az-latn'; CodePage: 1254));

  CodePageMapBC: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'be'; CodePage: 1251),
    (LocaleName: 'bg'; CodePage: 1251),
    (LocaleName: 'cs'; CodePage: 1250));

  CodePageMapEF: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'el'; CodePage: 1253),
    (LocaleName: 'et'; CodePage: 1257),
    (LocaleName: 'fa'; CodePage: 1256));

  CodePageMapH: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'he'; CodePage: 1255),
    (LocaleName: 'hr'; CodePage: 1250),
    (LocaleName: 'hu'; CodePage: 1250));

  CodePageMapJK: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'ja'; CodePage: 932),
    (LocaleName: 'kk'; CodePage: 1251),
    (LocaleName: 'ko'; CodePage: 949));

  CodePageMapLM: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'lt'; CodePage: 1257),
    (LocaleName: 'lv'; CodePage: 1257),
    (LocaleName: 'mk'; CodePage: 1251));

  CodePageMapP: array[0..1] of TCodePageMapEntry = (
    (LocaleName: 'pa-arab'; CodePage: 1256),
    (LocaleName: 'pl'; CodePage: 1250));

  CodePageMapR: array[0..1] of TCodePageMapEntry = (
    (LocaleName: 'ro'; CodePage: 1250),
    (LocaleName: 'ru'; CodePage: 1251));

  CodePageMapS: array[0..4] of TCodePageMapEntry = (
    (LocaleName: 'sk'; CodePage: 1250),
    (LocaleName: 'sl'; CodePage: 1250),
    (LocaleName: 'sq'; CodePage: 1250),
    (LocaleName: 'sr-cyrl'; CodePage: 1251),
    (LocaleName: 'sr-latn'; CodePage: 1250));

  CodePageMapT: array[0..1] of TCodePageMapEntry = (
    (LocaleName: 'th'; CodePage: 874),
    (LocaleName: 'tr'; CodePage: 1254));

  CodePageMapUV: array[0..5] of TCodePageMapEntry = (
    (LocaleName: 'uk'; CodePage: 1251),
    (LocaleName: 'ur'; CodePage: 1256),
    (LocaleName: 'uz-arab'; CodePage: 1256),
    (LocaleName: 'uz-cyrl'; CodePage: 1251),
    (LocaleName: 'uz-latn'; CodePage: 1254),
    (LocaleName: 'vi'; CodePage: 1258));

  // Special case - needs full LANG_CNTRY to determine proper codepage
  CodePageMapZH: array[0..6] of TCodePageMapEntry = (
    (LocaleName: 'zh_cn'; CodePage: 936),
    (LocaleName: 'zh_hk'; CodePage: 950),
    (LocaleName: 'zh-hans_hk'; CodePage: 936),
    (LocaleName: 'zh_mo'; CodePage: 950),
    (LocaleName: 'zh-hans_mo'; CodePage: 936),
    (LocaleName: 'zh_sg'; CodePage: 936),
    (LocaleName: 'zh_tw'; CodePage: 950));

function GetPosixLocaleName: string;
{$IFDEF MACOS}

  function StringRefToString(StringRef: CFStringRef): string;
  var
    Range: CFRange;
  begin
    Range.location := 0;
    Range.length := CFStringGetLength(StringRef);
    if Range.length > 0 then
    begin
      SetLength(Result, Range.length);
      CFStringGetCharacters(StringRef, Range, @Result[1]);
    end
    else
      Result := '';
  end;

var
  Locale: CFLocaleRef;
begin
  Locale := CFLocaleCopyCurrent;
  try
    Result := StringRefToString(CFLocaleGetIdentifier(Locale));
  finally
    CFRelease(Locale);
  end;
end;
{$ELSE}
var
  P: Integer;
begin
  Result := string(getenv(PAnsiChar('LANG'))); // do not localize
  P := Pos('.', Result);
  if P <> 0 then
    SetLength(Result, P - 1);
end;
{$ENDIF MACOS}

function GetACP: Cardinal;

  function FindCodePage(const Name: string; const Map: array of TCodePageMapEntry;
    var CodePage: Cardinal): Boolean;
  var
    I: Integer;
  begin
    for I := Low(Map) to High(Map) do
      if Map[I].LocaleName = Name then
      begin
        CodePage := Map[I].CodePage;
        Exit(True);
      end;
    Result := False;
  end;

var
  I: Integer;
  LName: string;
  LCodePage: Cardinal;
begin
  LName := GetPosixLocaleName;
  I := 1;
  while I <= Length(LName) do
  begin
    if AnsiChar(LName[I]) in ['A'..'Z'] then         // do not localize
      Inc(LName[I], Ord('a') - Ord('A'))   // do not localize
    else if LName[I] = '_' then            // do not localize
    begin
      SetLength(LName, I - 1);
      Break;
    end;
    Inc(I);
  end;

  Result := 1252; // Default codepage
  if Length(LName) > 0 then
    case LName[1] of
      'a':
        if FindCodePage(LName, CodePageMapA, LCodePage) then
          Result := LCodePage;
      'b','c':
        if FindCodePage(LName, CodePageMapBC, LCodePage) then
          Result := LCodePage;
      'e','f':
        if FindCodePage(LName, CodePageMapEF, LCodePage) then
          Result := LCodePage;
      'h':
        if FindCodePage(LName, CodePageMapH, LCodePage) then
          Result := LCodePage;
      'j','k':
        if FindCodePage(LName, CodePageMapJK, LCodePage) then
          Result := LCodePage;
      'l','m':
        if FindCodePage(LName, CodePageMapLM, LCodePage) then
          Result := LCodePage;
      'p':
        if FindCodePage(LName, CodePageMapP, LCodePage) then
          Result := LCodePage;
      'r':
        if FindCodePage(LName, CodePageMapR, LCodePage) then
          Result := LCodePage;
      's':
        if FindCodePage(LName, CodePageMapS, LCodePage) then
          Result := LCodePage;
      't':
        if FindCodePage(LName, CodePageMapT, LCodePage) then
          Result := LCodePage;
      'u','v':
        if FindCodePage(LName, CodePageMapUV, LCodePage) then
          Result := LCodePage;
      'z':
        begin
          LName := GetPosixLocaleName;
          I := 1;
          while I <= Length(LName) do
          begin
            if AnsiChar(LName[I]) in ['A'..'Z'] then         // do not localize
              Inc(LName[I], Ord('a') - Ord('A'))   // do not localize
            else if LName[I] = '@' then            // do not localize
            // Non Gregorian calendars include "@calendar=<calendar>" on MACOS
            begin
              SetLength(LName, I - 1);
              Break;
            end;
            Inc(I);
          end;
          if FindCodePage(LName, CodePageMapZH, LCodePage) then
            Result := LCodePage
          else if (Length(LName) >= 2) and (LName[2] = 'h') then
            // Fallback for Chinese in countries other than cn, hk, mo, tw, sg
            Result := 936;
        end;
    end;
end;
{$ENDIF POSIX}

{$IFDEF POSIX}
function LocaleNameFromCodePage(CodePage: Integer): AnsiString;
begin
  if CodePage = CP_ACP then
    CodePage := GetACP;
  case CodePage of
    // Special cases
    10000:   Result := 'MACROMAN';         // do not localize
    10004:   Result := 'MACARABIC';        // do not localize
    10005:   Result := 'MACHEBREW';        // do not localize
    10006:   Result := 'MACGREEK';         // do not localize
    10007:   Result := 'MACCYRILLIC';      // do not localize
    10010:   Result := 'MACROMANIA';       // do not localize
    10017:   Result := 'MACUKRAINE';       // do not localize
    10021:   Result := 'MACTHAI';          // do not localize
    10029:   Result := 'MACCENTRALEUROPE'; // do not localize
    10079:   Result := 'MACICELAND';       // do not localize
    10081:   Result := 'MACTURKISH';       // do not localize
    10082:   Result := 'MACCROATIAN';      // do not localize
    12000:   Result := 'UTF-32LE';         // do not localize
    12001:   Result := 'UTF-32BE';         // do not localize
    20127:   Result := 'ASCII';            // do not localize
    20866:   Result := 'KOI8-R';           // do not localize
    20932:   Result := 'EUC-JP';           // do not localize
    20936:   Result := 'GB2312';           // do not localize
    21866:   Result := 'KOI8-U';           // do not localize
    28591:   Result := 'ISO-8859-1';       // do not localize
    28592:   Result := 'ISO-8859-2';       // do not localize
    28593:   Result := 'ISO-8859-3';       // do not localize
    28594:   Result := 'ISO-8859-4';       // do not localize
    28595:   Result := 'ISO-8859-5';       // do not localize
    28596:   Result := 'ISO-8859-6';       // do not localize
    28597:   Result := 'ISO-8859-7';       // do not localize
    28598:   Result := 'ISO-8859-8';       // do not localize
    28599:   Result := 'ISO-8859-9';       // do not localize
    28600:   Result := 'ISO-8859-10';      // do not localize
    28601:   Result := 'ISO-8859-11';      // do not localize
    28603:   Result := 'ISO-8859-13';      // do not localize
    28604:   Result := 'ISO-8859-14';      // do not localize
    28605:   Result := 'ISO-8859-15';      // do not localize
    28606:   Result := 'ISO-8859-16';      // do not localize
    50221:   Result := 'ISO-2022-JP';      // do not localize
    50225:   Result := 'ISO-2022-KR';      // do not localize
    50227:   Result := 'ISO-2022-CN';      // do not localize
    51932:   Result := 'EUC-JP';           // do not localize
    51936:   Result := 'GB2312';           // do not localize
    51949:   Result := 'EUC-KR';           // do not localize
    51950:   Result := 'EUC-TW';           // do not localize
    52936:   Result := 'HZ-GB-2312';       // do not localize
    54936:   Result := 'GB18030';          // do not localize
    CP_UTF7: Result := 'UTF-7';            // do not localize
    CP_UTF8: Result := 'UTF-8';            // do not localize
  else
    Str(CodePage, Result);
    Result := 'cp' + Result;  // do not localize
  end;
end;
{$ENDIF POSIX}

function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
{$IFDEF MSWINDOWS}
begin
  Result := WideCharToMultiByte(CodePage, Flags, UnicodeStr, UnicodeStrLen, LocaleStr,
    LocaleStrLen, DefaultChar, PBOOL(UsedDefaultChar));
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := LocaleCharsFromUnicode(LocaleNameFromCodePage(CodePage), Flags,
    UnicodeStr, UnicodeStrLen, LocaleStr, LocaleStrLen, DefaultChar, UsedDefaultChar);
end;
{$ENDIF POSIX}

function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer; overload;
{$IFDEF MSWINDOWS}
begin
  Result := MultiByteToWideChar(CodePage, Flags, LocaleStr, LocaleStrLen,
    UnicodeStr, UnicodeStrLen);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := UnicodeFromLocaleChars(LocaleNameFromCodePage(CodePage), Flags,
    LocaleStr, LocaleStrLen, UnicodeStr, UnicodeStrLen);
end;
{$ENDIF POSIX}

{$IFDEF POSIX}
const
  MB_ERR_INVALID_CHARS = 8;

function LocaleCharsFromUnicode(const LocaleName: AnsiString; Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
var
  LContext: iconv_t;
  LSourcePtr, LDestPtr: Pointer;
  LCalcSizeBuf: array of AnsiChar;
  LDestLen, LSourceLen, LSize: Integer;
begin
  Result := 0;
  // DefaultChar and UsedDefaultChar are not valid with UTF-7 or UTF-8
  if ((DefaultChar <> nil) or (UsedDefaultChar <> nil)) and
     ((LocaleName = 'UTF-8') or (LocaleName = 'UTF-7')) then // do not localize
    Exit(0);

  // When UnicodeStrLen is -1 treat UnicodeStr as a null terminated string
  if UnicodeStrLen = -1 then
    UnicodeStrLen := Length(UnicodeStr) + 1; // include terminating null

  LContext := iconv_open(PAnsiChar(LocaleName), 'UTF-16LE'); // do not localize
  if LContext <> iconv_t(-1) then
  try
  // LocaleStrLen = 0 is a request to calculate the required buffer size,
  // use a local destination buffer for the conversion.
  if LocaleStrLen = 0 then
  begin
    SetLength(LCalcSizeBuf, 1024);
    LDestLen := Length(LCalcSizeBuf);
    LDestPtr := Pointer(@LCalcSizeBuf[0]);
  end
  else
  begin
    LDestLen := LocaleStrLen;
    LDestPtr := Pointer(LocaleStr);
  end;

  LSourceLen := UnicodeStrLen * SizeOf(WideChar);
  LSourcePtr := Pointer(UnicodeStr);

  LSize := 0;
  if UsedDefaultChar <> nil then
    UsedDefaultChar^ := False;

  while True do
  begin
      Result := iconv(LContext, @LSourcePtr, @LSourceLen, @LDestPtr, @LDestLen);
    if Result <> -1 then
      Break
    else
    begin
      case GetLastError of
        E2BIG: // Insufficient destination buffer
          if LocaleStrLen = 0 then
          begin
            // Save converted buffer size and reset to beginning of local buffer
            Inc(LSize, Length(LCalcSizeBuf) - LDestLen);
            LDestPtr := Pointer(@LCalcSizeBuf[0]);
            LDestLen := Length(LCalcSizeBuf);
          end
          else
          begin
            LDestLen := LocaleStrLen; // Return a length of 0
            Break;
          end;
        EILSEQ: // Invalid character for destination character set
          begin
            // Increment pointers and insert '?' (or the DefaultChar
            // if specified) into the destination string.
            Inc(PByte(LSourcePtr), SizeOf(WideChar));
            Dec(LSourceLen, SizeOf(WideChar));
            if LocaleStrLen <> 0 then
            begin
              if DefaultChar = nil then
                PAnsiChar(LDestPtr)^ := AnsiChar('?') // do not localize
              else
                PAnsiChar(LDestPtr)^ := DefaultChar^;
              if UsedDefaultChar <> nil then
                UsedDefaultChar^ := True;
            end;
            Inc(PByte(LDestPtr));
            Dec(LDestLen);
          end;
        else
          Exit(0); // Return a length of 0
      end;
    end;
  end;

  if LocaleStrLen = 0 then
    Result := LSize + Length(LCalcSizeBuf) - LDestLen
  else
    Result := LocaleStrLen - LDestLen
  finally
    iconv_close(LContext);
  end;
end;

function UnicodeFromLocaleChars(const LocaleName: AnsiString; Flags: Cardinal;
  LocaleStr: PAnsiChar; LocaleStrLen: Integer; UnicodeStr: PWideChar;
  UnicodeStrLen: Integer): Integer; overload;
var
  LContext: iconv_t;
  InvalidCharFound: Boolean;
  LCalcSizeBuf: array of Byte;
  LDestLen, LSourceLen, LSize, LZero: Integer;
  LSourcePtr, LDestPtr, LNil, LastInvalidChar: Pointer;
begin
  Result := 0;
  LContext := iconv_open('UTF-16LE', PAnsiChar(LocaleName)); // do not localize
  if LContext <> iconv_t(-1) then
  try
  // When LocaleStrLen is -1 treat LocaleStr as a null terminated string
  if LocaleStrLen = -1 then
    LocaleStrLen := Length(LocaleStr) + 1; // include terminating null

  // UnicodeStrLen = 0 is a request to calculate the required buffer size,
  // use a local destination buffer for the conversion.
  if UnicodeStrLen = 0 then
  begin
    SetLength(LCalcSizeBuf, 1024);
    LDestLen := Length(LCalcSizeBuf);
    LDestPtr := Pointer(@LCalcSizeBuf[0]);
  end
  else
  begin
    LDestLen := UnicodeStrLen * SizeOf(WideChar);
    LDestPtr := Pointer(UnicodeStr);
  end;

  LSourceLen := LocaleStrLen;
  LSourcePtr := Pointer(LocaleStr);

  LSize := 0;
  LastInvalidChar := nil;
  InvalidCharFound := False;

  while True do
  begin
      Result := iconv(LContext, @LSourcePtr, @LSourceLen, @LDestPtr, @LDestLen);
    if Result <> -1 then
      Break
    else
    begin
      case GetLastError of
        E2BIG: // Insufficient destination buffer
          if UnicodeStrLen = 0 then
          begin
            // Save converted buffer size and reset to beginning of local buffer
            Inc(LSize, Length(LCalcSizeBuf));
            LDestPtr := Pointer(@LCalcSizeBuf[0]);
            LDestLen := Length(LCalcSizeBuf);
          end
          else
          begin
            // Return a length of 0
            LDestLen := UnicodeStrLen * SizeOf(WideChar);
            Break;
          end;
        EILSEQ: // Invalid character sequence in source string
          if LocaleName = 'UTF-7' then // do not localize
          begin
            // Special case for emulating MultiByteToWideChar with UTF-7.
            // This does not produce an exact match due to differences in the
            // decoders, but it preserves similar behaviour.
            if (Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS) then
              Exit(0);
            Inc(PByte(LSourcePtr), SizeOf(AnsiChar));
            Dec(LSourceLen, SizeOf(AnsiChar));
            // Reset state of context
            LNil := nil;
            LZero := 0;
              iconv(LContext, @LNil, @LZero, @LNil, @LZero);
          end
          else
          begin
            // Increment pointers and insert #$FFFD into the destination
            // string if the source is a UTF-8 string. Only insert #$FFFD
            // once per invalid UTF-8 character.
            if LSourcePtr <> LastInvalidChar then
            begin
              if (UnicodeStrLen <> 0) and (LocaleName = 'UTF-8') then // do not localize
                PWideChar(LDestPtr)^ := #$FFFD; // Invalid UTF-8 char
              Inc(PByte(LDestPtr), SizeOf(WideChar));
              Dec(LDestLen, SizeOf(WideChar));
            end;
           Inc(PByte(LSourcePtr), SizeOf(AnsiChar));
            Dec(LSourceLen, SizeOf(AnsiChar));
            LastInvalidChar := LSourcePtr;
            InvalidCharFound := True;
          end;
        else
          Exit(0); // Return a length of 0
      end;
    end;
  end;

  if InvalidCharFound and (Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS) then
    Exit(0); // Return 0 if an invalid character was encountered

  if UnicodeStrLen = 0 then
    Result := (LSize + Length(LCalcSizeBuf) - LDestLen) div SizeOf(WideChar)
  else
    Result := UnicodeStrLen - (LDestLen div 2);
  finally
    iconv_close(LContext);
  end;
end;
{$ENDIF POSIX}

procedure SetMultiByteConversionCodePage(CodePage: Integer);
begin
  DefaultSystemCodePage := CodePage;
end;

function GetCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SetUtf8CompareLocale;
var
  OSVersion: Cardinal;
  MajorVersion: Cardinal;
  MinorVersion: Cardinal;
begin
  OSVersion := GetVersion;
  MajorVersion := OSVersion and $000000FF;
  MinorVersion := (OSVersion and $0000FF00) shr 8;

  if ((MajorVersion = 5) and (MinorVersion >= 1)) or
     (MajorVersion > 5) then
    UTF8CompareLocale := LOCALE_INVARIANT
  else
    UTF8CompareLocale := $0409;
end;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
var
  InternalUTF8CompareLocale: Pointer = nil;

function UTF8CompareLocale: Pointer;
begin
  Result := InternalUTF8CompareLocale;
end;

function SetUTF8CompareLocale(const LocaleName: string): Boolean;
var
  LNewLocale, OldLocale: Pointer;
begin
  LNewLocale := newlocale(LC_ALL_MASK, PAnsiChar(AnsiString(LocaleName)), nil);
  Result := LNewLocale <> nil;
  if Result then
  begin
    OldLocale := InterlockedExchangePointer(InternalUTF8CompareLocale, LNewLocale);
    if OldLocale <> nil then
      freelocale(OldLocale);
  end;
end;
{$ENDIF POSIX}

class operator TGUID.Equal(const Left, Right: TGUID): Boolean;
{$IFDEF CPUX64}
var
  a, b: PInt64Array;
begin
  a := PInt64Array(@Left);
  b := PInt64Array(@Right);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]);
end;
{$ELSE !CPUX64}
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@Left);
  b := PIntegerArray(@Right);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3] = b^[3]);
end;
{$ENDIF}

class operator TGUID.NotEqual(const Left, Right: TGUID): Boolean;
begin
  Result := not (Left = Right);
end;

class function TGUID.Empty: TGUID;
begin
  FillChar(Result, Sizeof(Result), 0)
end;

initialization
{$IFDEF MSWINDOWS}
  SetThreadLocale(LOCALE_USER_DEFAULT);
  InitializeMemoryManager;
  InitializeLocaleData;
{$ENDIF}
{$IFDEF POSIX}
  setlocale(LC_ALL, '');
{$ENDIF POSIX}

  FileMode := 2;

{$IFDEF MSWINDOWS}
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
  Test8086 := 2;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  FileAccessRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Test8086 := GetCPUType;
  IsConsole := True;
  FindResourceCache.ModuleHandle := LongWord(-1);
  ReserveZeroPage;
{$ENDIF LINUX}
{$IFDEF MACOS}
  FileAccessRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Test8086 := 2;
  IsConsole := True;
//  FindResourceCache.ModuleHandle := LongWord(-1);
{$ENDIF MACOS}
  CPUCount := GetCPUCount;
  DispCallByIDProc := @_DispCallByIDError;

  _InitializeControlWord;
  _FpuInit();

  TTextRec(Input).Mode := fmClosed;
  TTextRec(Output).Mode := fmClosed;
  TTextRec(ErrOutput).Mode := fmClosed;

{$IFDEF MSWINDOWS}
  CmdLine := GetCommandLine;
  CmdShow := GetCmdShow;
{$ENDIF MSWINDOWS}
  DefaultSystemCodePage := GetACP;
  DefaultUnicodeCodePage := CP_UTF16; // UTF16 - Do not pass to MultiByteToWideChar or WideCharToMultiByte
  MainThreadID := GetCurrentThreadID;

{$IFDEF MSWINDOWS}
  SetUtf8CompareLocale;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  SetUTF8CompareLocale('en_US.UTF-8');  // do not localize
{$ENDIF POSIX}

finalization
  Close(Input);
  Close(Output);
  Close(ErrOutput);
{$IFDEF LINUX}
  ReleaseZeroPage;
{$ENDIF LINUX}
{$IFDEF POSIX}
  if InternalUTF8CompareLocale <> nil then
    freelocale(InternalUTF8CompareLocale);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  FinalizeLocaleDate;
  if PreferredLanguagesOverride <> nil then
    FreeMem(PreferredLanguagesOverride);
  {Uninitialize the default memory manager, and free all memory allocated by
   this memory manager.}
  FinalizeMemoryManager;
{$ENDIF MSWINDOWS}
end.
