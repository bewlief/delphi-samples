{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

type
  dl_info = record
     dli_fname: MarshaledAString;
     dli_fbase: Pointer;
     dli_sname: MarshaledAString;
     dli_saddr: Pointer;
  end;
  {$EXTERNALSYM dl_info}
  Pdl_info = ^dl_info;

const
  RTLD_LAZY = 1;               { Lazy function call binding.  }
  {$EXTERNALSYM RTLD_LAZY}

{$IFNDEF CPU64BITS}
{ LP32 is broken for historical reasons. }
  RTLD_NOW  = 0;               { Immediate function call binding.  }
  RTLD_GLOBAL = 2;
{$ELSE   CPU64BITS}
  RTLD_NOW  = 2;               { Immediate function call binding.  }
  RTLD_GLOBAL = $100;
{$ENDIF  CPU64BITS}
  {$EXTERNALSYM RTLD_NOW}
  {$EXTERNALSYM RTLD_GLOBAL}
  RTLD_LOCAL = 0;
  {$EXTERNALSYM RTLD_LOCAL}
  RTLD_NOLOAD =   $00004;      { This one is not POSIX-compliant }
  {$EXTERNALSYM RTLD_NOLOAD}
  RTLD_NODELETE = $01000;      { This one is not POSIX-compliant }
  {$EXTERNALSYM RTLD_NODELETE}

{$IFDEF CPU64BITS}
  RTLD_DEFAULT : NativeUInt = 0;
  RTLD_NEXT : NativeUInt = NativeUInt(-1);
{$ELSE !CPU64BITS}
  RTLD_DEFAULT : NativeUInt = $ffffffff;           { This one is not POSIX-compliant }
  RTLD_NEXT : NativeUInt = $fffffffe;
{$ENDIF CPU64BITS}
  {$EXTERNALSYM RTLD_DEFAULT}
  {$EXTERNALSYM RTLD_NEXT}
