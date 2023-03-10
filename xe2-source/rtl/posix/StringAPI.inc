{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

function memccpy(var s1; const s2; c: Integer; n: size_t): Pointer; cdecl;
  external libc name _PU + 'memccpy';
{$EXTERNALSYM memccpy}

function memchr(const s; c: Integer; n: size_t): Pointer; cdecl;
  external libc name _PU + 'memchr';
{$EXTERNALSYM memchr}

function memcmp(const s1; const s2; n: size_t): Integer; cdecl;
  external libc name _PU + 'memcmp';
{$EXTERNALSYM memcmp}

function memcpy(var s1; const s2; n: size_t): Pointer; cdecl;
  external libc name _PU + 'memcpy';
{$EXTERNALSYM memcpy}

function memmove(var s1; const s2; n: size_t): Pointer; cdecl;
  external libc name _PU + 'memmove';
{$EXTERNALSYM memmove}

function memset(var b; c: Integer; len: size_t): Pointer; cdecl;
  external libc name _PU + 'memset';
{$EXTERNALSYM memset}

function stpcpy(s1: PAnsiChar; s2: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'stpcpy';
{$EXTERNALSYM stpcpy}

{$IFDEF LINUX}
function stpncpy(s1: PAnsiChar; s2: PAnsiChar; n: size_t): PAnsiChar; cdecl;
  external libc name _PU + 'stpncpy';
{$EXTERNALSYM stpncpy}
{$ENDIF LINUX}

function strcat(s1: PAnsiChar; s2: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strcat';
{$EXTERNALSYM strcat}

function strchr(s: PAnsiChar; c: Integer): PAnsiChar; cdecl;
  external libc name _PU + 'strchr';
{$EXTERNALSYM strchr}

function strcmp(s1: PAnsiChar; s2: PAnsiChar): Integer; cdecl;
  external libc name _PU + 'strcmp';
{$EXTERNALSYM strcmp}

{ Compare the collated forms of S1 and S2.  }
function strcoll(S1: PAnsiChar; S2: PAnsiChar): Integer; cdecl;
  external libc name _PU + 'strcoll';
{$EXTERNALSYM strcoll}

function strcoll_l(s1: PAnsiChar; s2: PAnsiChar; loc: locale_t): Integer; cdecl;
  external libc name _PU + 'strcoll_l';
{$EXTERNALSYM strcoll_l}

function strcpy(s1: PAnsiChar; s2: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strcpy';
{$EXTERNALSYM strcpy}

function strcspn(s1: PAnsiChar; s2: PAnsiChar): size_t; cdecl;
  external libc name _PU + 'strcspn';
{$EXTERNALSYM strcspn}

function strdup(s1: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strdup';
{$EXTERNALSYM strdup}

function strerror(errnum: Integer): PAnsiChar; cdecl;
  external libc name _PU + 'strerror';
{$EXTERNALSYM strerror}

{
 strerror_l is in POSIX, but is only implemented in glibc in version 2.6,
 using versioning support on linux.  Ilink needs additional work before it can make the
 linkage work.
}
//function strerror_l(errnum: Integer; loc: locale_t): PAnsiChar; cdecl;
//{$IFDEF MACOS}
//  external libc name _PU + 'strerror_l';
//{$ENDIF MACOS}
//{$IFDEF LINUX}
//  external libc name _PU + 'strerror_l@@GLIBC_2.6';
//{$ENDIF}
//{$EXTERNALSYM strerror_l}

{ Reentrant version of `strerror'.  If a temporary buffer is required, at
   most BUFLEN bytes of BUF will be used.  }
function strerror_r(ErrorNum: Integer; Buf: PAnsiChar; Buflen: size_t): Integer; cdecl;
  external libc name _PU + 'strerror_r';
{$EXTERNALSYM strerror_r}

function strlen(S1: PAnsiChar): size_t; cdecl;
  external libc name _PU + 'strlen';
{$EXTERNALSYM strlen}

function strncat(s1: PAnsiChar; s2: PAnsiChar; n: size_t): PAnsiChar; cdecl;
  external libc name _PU + 'strncat';
{$EXTERNALSYM strncat}
{ Compare N characters of S1 and S2.  }
function strncmp(S1: PAnsiChar; S2: PAnsiChar; N: size_t): Integer; cdecl;
  external libc name _PU + 'strncmp';
{$EXTERNALSYM strncmp}

function strncpy(s1: PAnsiChar; s2: PAnsiChar; n: size_t): PAnsiChar; cdecl;
  external libc name _PU + 'strncpy';
{$EXTERNALSYM strncpy}

{$IFDEF LINUX}
function strndup(s1: PAnsiChar; n: size_t): PAnsiChar; cdecl;
  external libc name _PU + 'strndup';
{$EXTERNALSYM strndup}
function strnlen(s1: PAnsiChar; n: size_t): size_t; cdecl;
  external libc name _PU + 'strnlen';
{$EXTERNALSYM strnlen}
{$ENDIF LINUX}

function strpbrk(s1: PAnsiChar; s2: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strpbrk';
{$EXTERNALSYM strpbrk}

function strrchr(s: PAnsiChar; c: Integer): PAnsiChar; cdecl;
  external libc name _PU + 'strrchr';
{$EXTERNALSYM strrchr}

function strsignal(sig: Integer): PAnsiChar; cdecl;
  external libc name _PU + 'strsignal';
{$EXTERNALSYM strsignal}

function strspn(s1: PAnsiChar; s2: PAnsiChar): size_t; cdecl;
  external libc name _PU + 'strspn';
{$EXTERNALSYM strspn}

function strstr(s1: PAnsiChar; s2: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strstr';
{$EXTERNALSYM strstr}

function strtok(str: PAnsiChar; sep: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strtok';
{$EXTERNALSYM strtok}

function strtok_r(str: PAnsiChar; sep: PAnsiChar; var lasts: PAnsiChar): PAnsiChar; cdecl;
  external libc name _PU + 'strtok_r';
{$EXTERNALSYM strtok_r}

function strxfrm(s1: PAnsiChar; s2: PAnsiChar; n: size_t): size_t; cdecl;
  external libc name _PU + 'strxfrm';
{$EXTERNALSYM strxfrm}

function strxfrm_l(s1: PAnsiChar; s2: PAnsiChar; n: size_t; loc: locale_t): size_t; cdecl;
  external libc name _PU + 'strxfrm_l';
{$EXTERNALSYM strxfrm_l}

{TODO: This belongs in strings.h}
function strcasecmp(S1: PAnsiChar; S2: PAnsiChar): Integer; cdecl;
  external libc name _PU + 'strcasecmp';
{$EXTERNALSYM strcasecmp}
