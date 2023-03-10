{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

function iconv(cd: iconv_t; InBuf: PPAnsiChar; InBytesLeft: Psize_t;
	 OutBuf: PPAnsiChar; OutBytesLeft: Psize_t): size_t; cdecl;
  external libiconv name _PU + 'iconv';
{$EXTERNALSYM iconv}

function iconv_open(ToCode: PAnsiChar; FromCode: PAnsiChar): iconv_t; cdecl;
  external libiconv name _PU + 'iconv_open';
{$EXTERNALSYM iconv_open}

function iconv_close(cd: iconv_t): Integer; cdecl;
  external libiconv name _PU + 'iconv_close';
{$EXTERNALSYM iconv_close}
