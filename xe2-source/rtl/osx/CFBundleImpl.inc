{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}


var
  _kCFBundleInfoDictionaryVersionKey: Pointer = nil;
  _kCFBundleExecutableKey: Pointer = nil;
  _kCFBundleIdentifierKey: Pointer = nil;
  _kCFBundleVersionKey: Pointer = nil;
  _kCFBundleDevelopmentRegionKey: Pointer = nil;
  _kCFBundleNameKey: Pointer = nil;
  _kCFBundleLocalizationsKey: Pointer = nil;

function kCFBundleInfoDictionaryVersionKey: CFStringRef;
begin
  if _kCFBundleInfoDictionaryVersionKey = nil then
    _kCFBundleInfoDictionaryVersionKey := dlsym(CFModule, PAnsiChar('kCFBundleInfoDictionaryVersionKey'));
  Result := CFStringRef(_kCFBundleInfoDictionaryVersionKey^);
end;

function kCFBundleExecutableKey: CFStringRef;
begin
  if _kCFBundleExecutableKey = nil then
    _kCFBundleExecutableKey := dlsym(CFModule, PAnsiChar('kCFBundleExecutableKey'));
  Result := CFStringRef(_kCFBundleExecutableKey^);
end;

function kCFBundleIdentifierKey: CFStringRef;
begin
  if _kCFBundleIdentifierKey = nil then
    _kCFBundleIdentifierKey := dlsym(CFModule, PAnsiChar('kCFBundleIdentifierKey'));
  Result := CFStringRef(_kCFBundleIdentifierKey^);
end;

function kCFBundleVersionKey: CFStringRef;
begin
  if _kCFBundleVersionKey = nil then
    _kCFBundleVersionKey := dlsym(CFModule, PAnsiChar('kCFBundleVersionKey'));
  Result := CFStringRef(_kCFBundleVersionKey^);
end;

function kCFBundleDevelopmentRegionKey: CFStringRef;
begin
  if _kCFBundleDevelopmentRegionKey = nil then
    _kCFBundleDevelopmentRegionKey := dlsym(CFModule, PAnsiChar('kCFBundleDevelopmentRegionKey'));
  Result := CFStringRef(_kCFBundleDevelopmentRegionKey^);
end;

function kCFBundleNameKey: CFStringRef;
begin
  if _kCFBundleNameKey = nil then
    _kCFBundleNameKey := dlsym(CFModule, PAnsiChar('kCFBundleNameKey'));
  Result := CFStringRef(_kCFBundleNameKey^);
end;

function kCFBundleLocalizationsKey: CFStringRef;
begin
  if _kCFBundleLocalizationsKey = nil then
    _kCFBundleLocalizationsKey := dlsym(CFModule, PAnsiChar('kCFBundleLocalizationsKey'));
  Result := CFStringRef(_kCFBundleLocalizationsKey^);
end;

function CFCopyLocalizedString(key: CFStringRef; comment: PAnsiChar): CFStringRef;
begin
  Result := CFBundleCopyLocalizedString(CFBundleGetMainBundle, key, key, nil);
end;

function CFCopyLocalizedStringFromTable(key: CFStringRef; tbl: CFStringRef;
  comment: PAnsiChar): CFStringRef;
begin
  Result := CFBundleCopyLocalizedString(CFBundleGetMainBundle, key, key, tbl);
end;

function CFCopyLocalizedStringFromTableInBundle(key: CFStringRef; tbl: CFStringRef;
  bundle: CFBundleRef; comment: PAnsiChar): CFStringRef;
begin
  Result := CFBundleCopyLocalizedString(bundle, key, key, tbl);
end;

function CFCopyLocalizedStringWithDefaultValue(key: CFStringRef; tbl: CFStringRef;
  bundle: CFBundleRef; value: CFStringRef; comment: PAnsiChar): CFStringRef;
begin
  Result := CFBundleCopyLocalizedString(bundle, key, value, tbl);
end;

