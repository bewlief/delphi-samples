{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}


var
  _kCFURLFileExists: Pointer = nil;
  _kCFURLFileDirectoryContents: Pointer = nil;
  _kCFURLFileLength: Pointer = nil;
  _kCFURLFileLastModificationTime: Pointer = nil;
  _kCFURLFilePOSIXMode: Pointer = nil;
  _kCFURLFileOwnerID: Pointer = nil;
  _kCFURLHTTPStatusCode: Pointer = nil;
  _kCFURLHTTPStatusLine: Pointer = nil;

function kCFURLFileExists: CFStringRef;
begin
  if _kCFURLFileExists = nil then
    _kCFURLFileExists := dlsym(CFModule, PAnsiChar('kCFURLFileExists'));
  Result := CFStringRef(_kCFURLFileExists^);
end;

function kCFURLFileDirectoryContents: CFStringRef;
begin
  if _kCFURLFileDirectoryContents = nil then
    _kCFURLFileDirectoryContents := dlsym(CFModule, PAnsiChar('kCFURLFileDirectoryContents'));
  Result := CFStringRef(_kCFURLFileDirectoryContents^);
end;

function kCFURLFileLength: CFStringRef;
begin
  if _kCFURLFileLength = nil then
    _kCFURLFileLength := dlsym(CFModule, PAnsiChar('kCFURLFileLength'));
  Result := CFStringRef(_kCFURLFileLength^);
end;

function kCFURLFileLastModificationTime: CFStringRef;
begin
  if _kCFURLFileLastModificationTime = nil then
    _kCFURLFileLastModificationTime := dlsym(CFModule, PAnsiChar('kCFURLFileLastModificationTime'));
  Result := CFStringRef(_kCFURLFileLastModificationTime^);
end;

function kCFURLFilePOSIXMode: CFStringRef;
begin
  if _kCFURLFilePOSIXMode = nil then
    _kCFURLFilePOSIXMode := dlsym(CFModule, PAnsiChar('kCFURLFilePOSIXMode'));
  Result := CFStringRef(_kCFURLFilePOSIXMode^);
end;

function kCFURLFileOwnerID: CFStringRef;
begin
  if _kCFURLFileOwnerID = nil then
    _kCFURLFileOwnerID := dlsym(CFModule, PAnsiChar('kCFURLFileOwnerID'));
  Result := CFStringRef(_kCFURLFileOwnerID^);
end;

function kCFURLHTTPStatusCode: CFStringRef;
begin
  if _kCFURLHTTPStatusCode = nil then
    _kCFURLHTTPStatusCode := dlsym(CFModule, PAnsiChar('kCFURLHTTPStatusCode'));
  Result := CFStringRef(_kCFURLHTTPStatusCode^);
end;

function kCFURLHTTPStatusLine: CFStringRef;
begin
  if _kCFURLHTTPStatusLine = nil then
    _kCFURLHTTPStatusLine := dlsym(CFModule, PAnsiChar('kCFURLHTTPStatusLine'));
  Result := CFStringRef(_kCFURLHTTPStatusLine^);
end;

