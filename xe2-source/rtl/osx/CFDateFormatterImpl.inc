{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}


var
  _kCFDateFormatterIsLenient: Pointer = nil;
  _kCFDateFormatterTimeZone: Pointer = nil;
  _kCFDateFormatterCalendarName: Pointer = nil;
  _kCFDateFormatterDefaultFormat: Pointer = nil;
  _kCFDateFormatterTwoDigitStartDate: Pointer = nil;
  _kCFDateFormatterDefaultDate: Pointer = nil;
  _kCFDateFormatterCalendar: Pointer = nil;
  _kCFDateFormatterEraSymbols: Pointer = nil;
  _kCFDateFormatterMonthSymbols: Pointer = nil;
  _kCFDateFormatterShortMonthSymbols: Pointer = nil;
  _kCFDateFormatterWeekdaySymbols: Pointer = nil;
  _kCFDateFormatterShortWeekdaySymbols: Pointer = nil;
  _kCFDateFormatterAMSymbol: Pointer = nil;
  _kCFDateFormatterPMSymbol: Pointer = nil;
  _kCFDateFormatterLongEraSymbols: Pointer = nil;
  _kCFDateFormatterVeryShortMonthSymbols: Pointer = nil;
  _kCFDateFormatterStandaloneMonthSymbols: Pointer = nil;
  _kCFDateFormatterShortStandaloneMonthSymbols: Pointer = nil;
  _kCFDateFormatterVeryShortStandaloneMonthSymbols: Pointer = nil;
  _kCFDateFormatterVeryShortWeekdaySymbols: Pointer = nil;
  _kCFDateFormatterStandaloneWeekdaySymbols: Pointer = nil;
  _kCFDateFormatterShortStandaloneWeekdaySymbols: Pointer = nil;
  _kCFDateFormatterVeryShortStandaloneWeekdaySymbols: Pointer = nil;
  _kCFDateFormatterQuarterSymbols: Pointer = nil;
  _kCFDateFormatterShortQuarterSymbols: Pointer = nil;
  _kCFDateFormatterStandaloneQuarterSymbols: Pointer = nil;
  _kCFDateFormatterShortStandaloneQuarterSymbols: Pointer = nil;
  _kCFDateFormatterGregorianStartDate: Pointer = nil;
  _kCFDateFormatterDoesRelativeDateFormattingKey: Pointer = nil;

function kCFDateFormatterIsLenient: CFStringRef;
begin
  if _kCFDateFormatterIsLenient = nil then
    _kCFDateFormatterIsLenient := dlsym(CFModule, PAnsiChar('kCFDateFormatterIsLenient'));
  Result := CFStringRef(_kCFDateFormatterIsLenient^);
end;

function kCFDateFormatterTimeZone: CFStringRef;
begin
  if _kCFDateFormatterTimeZone = nil then
    _kCFDateFormatterTimeZone := dlsym(CFModule, PAnsiChar('kCFDateFormatterTimeZone'));
  Result := CFStringRef(_kCFDateFormatterTimeZone^);
end;

function kCFDateFormatterCalendarName: CFStringRef;
begin
  if _kCFDateFormatterCalendarName = nil then
    _kCFDateFormatterCalendarName := dlsym(CFModule, PAnsiChar('kCFDateFormatterCalendarName'));
  Result := CFStringRef(_kCFDateFormatterCalendarName^);
end;

function kCFDateFormatterDefaultFormat: CFStringRef;
begin
  if _kCFDateFormatterDefaultFormat = nil then
    _kCFDateFormatterDefaultFormat := dlsym(CFModule, PAnsiChar('kCFDateFormatterDefaultFormat'));
  Result := CFStringRef(_kCFDateFormatterDefaultFormat^);
end;

function kCFDateFormatterTwoDigitStartDate: CFStringRef;
begin
  if _kCFDateFormatterTwoDigitStartDate = nil then
    _kCFDateFormatterTwoDigitStartDate := dlsym(CFModule, PAnsiChar('kCFDateFormatterTwoDigitStartDate'));
  Result := CFStringRef(_kCFDateFormatterTwoDigitStartDate^);
end;

function kCFDateFormatterDefaultDate: CFStringRef;
begin
  if _kCFDateFormatterDefaultDate = nil then
    _kCFDateFormatterDefaultDate := dlsym(CFModule, PAnsiChar('kCFDateFormatterDefaultDate'));
  Result := CFStringRef(_kCFDateFormatterDefaultDate^);
end;

function kCFDateFormatterCalendar: CFStringRef;
begin
  if _kCFDateFormatterCalendar = nil then
    _kCFDateFormatterCalendar := dlsym(CFModule, PAnsiChar('kCFDateFormatterCalendar'));
  Result := CFStringRef(_kCFDateFormatterCalendar^);
end;

function kCFDateFormatterEraSymbols: CFStringRef;
begin
  if _kCFDateFormatterEraSymbols = nil then
    _kCFDateFormatterEraSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterEraSymbols'));
  Result := CFStringRef(_kCFDateFormatterEraSymbols^);
end;

function kCFDateFormatterMonthSymbols: CFStringRef;
begin
  if _kCFDateFormatterMonthSymbols = nil then
    _kCFDateFormatterMonthSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterMonthSymbols'));
  Result := CFStringRef(_kCFDateFormatterMonthSymbols^);
end;

function kCFDateFormatterShortMonthSymbols: CFStringRef;
begin
  if _kCFDateFormatterShortMonthSymbols = nil then
    _kCFDateFormatterShortMonthSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterShortMonthSymbols'));
  Result := CFStringRef(_kCFDateFormatterShortMonthSymbols^);
end;

function kCFDateFormatterWeekdaySymbols: CFStringRef;
begin
  if _kCFDateFormatterWeekdaySymbols = nil then
    _kCFDateFormatterWeekdaySymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterWeekdaySymbols'));
  Result := CFStringRef(_kCFDateFormatterWeekdaySymbols^);
end;

function kCFDateFormatterShortWeekdaySymbols: CFStringRef;
begin
  if _kCFDateFormatterShortWeekdaySymbols = nil then
    _kCFDateFormatterShortWeekdaySymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterShortWeekdaySymbols'));
  Result := CFStringRef(_kCFDateFormatterShortWeekdaySymbols^);
end;

function kCFDateFormatterAMSymbol: CFStringRef;
begin
  if _kCFDateFormatterAMSymbol = nil then
    _kCFDateFormatterAMSymbol := dlsym(CFModule, PAnsiChar('kCFDateFormatterAMSymbol'));
  Result := CFStringRef(_kCFDateFormatterAMSymbol^);
end;

function kCFDateFormatterPMSymbol: CFStringRef;
begin
  if _kCFDateFormatterPMSymbol = nil then
    _kCFDateFormatterPMSymbol := dlsym(CFModule, PAnsiChar('kCFDateFormatterPMSymbol'));
  Result := CFStringRef(_kCFDateFormatterPMSymbol^);
end;

function kCFDateFormatterLongEraSymbols: CFStringRef;
begin
  if _kCFDateFormatterLongEraSymbols = nil then
    _kCFDateFormatterLongEraSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterLongEraSymbols'));
  Result := CFStringRef(_kCFDateFormatterLongEraSymbols^);
end;

function kCFDateFormatterVeryShortMonthSymbols: CFStringRef;
begin
  if _kCFDateFormatterVeryShortMonthSymbols = nil then
    _kCFDateFormatterVeryShortMonthSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterVeryShortMonthSymbols'));
  Result := CFStringRef(_kCFDateFormatterVeryShortMonthSymbols^);
end;

function kCFDateFormatterStandaloneMonthSymbols: CFStringRef;
begin
  if _kCFDateFormatterStandaloneMonthSymbols = nil then
    _kCFDateFormatterStandaloneMonthSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterStandaloneMonthSymbols'));
  Result := CFStringRef(_kCFDateFormatterStandaloneMonthSymbols^);
end;

function kCFDateFormatterShortStandaloneMonthSymbols: CFStringRef;
begin
  if _kCFDateFormatterShortStandaloneMonthSymbols = nil then
    _kCFDateFormatterShortStandaloneMonthSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterShortStandaloneMonthSymbols'));
  Result := CFStringRef(_kCFDateFormatterShortStandaloneMonthSymbols^);
end;

function kCFDateFormatterVeryShortStandaloneMonthSymbols: CFStringRef;
begin
  if _kCFDateFormatterVeryShortStandaloneMonthSymbols = nil then
    _kCFDateFormatterVeryShortStandaloneMonthSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterVeryShortStandaloneMonthSymbols'));
  Result := CFStringRef(_kCFDateFormatterVeryShortStandaloneMonthSymbols^);
end;

function kCFDateFormatterVeryShortWeekdaySymbols: CFStringRef;
begin
  if _kCFDateFormatterVeryShortWeekdaySymbols = nil then
    _kCFDateFormatterVeryShortWeekdaySymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterVeryShortWeekdaySymbols'));
  Result := CFStringRef(_kCFDateFormatterVeryShortWeekdaySymbols^);
end;

function kCFDateFormatterStandaloneWeekdaySymbols: CFStringRef;
begin
  if _kCFDateFormatterStandaloneWeekdaySymbols = nil then
    _kCFDateFormatterStandaloneWeekdaySymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterStandaloneWeekdaySymbols'));
  Result := CFStringRef(_kCFDateFormatterStandaloneWeekdaySymbols^);
end;

function kCFDateFormatterShortStandaloneWeekdaySymbols: CFStringRef;
begin
  if _kCFDateFormatterShortStandaloneWeekdaySymbols = nil then
    _kCFDateFormatterShortStandaloneWeekdaySymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterShortStandaloneWeekdaySymbols'));
  Result := CFStringRef(_kCFDateFormatterShortStandaloneWeekdaySymbols^);
end;

function kCFDateFormatterVeryShortStandaloneWeekdaySymbols: CFStringRef;
begin
  if _kCFDateFormatterVeryShortStandaloneWeekdaySymbols = nil then
    _kCFDateFormatterVeryShortStandaloneWeekdaySymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterVeryShortStandaloneWeekdaySymbols'));
  Result := CFStringRef(_kCFDateFormatterVeryShortStandaloneWeekdaySymbols^);
end;

function kCFDateFormatterQuarterSymbols: CFStringRef;
begin
  if _kCFDateFormatterQuarterSymbols = nil then
    _kCFDateFormatterQuarterSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterQuarterSymbols'));
  Result := CFStringRef(_kCFDateFormatterQuarterSymbols^);
end;

function kCFDateFormatterShortQuarterSymbols: CFStringRef;
begin
  if _kCFDateFormatterShortQuarterSymbols = nil then
    _kCFDateFormatterShortQuarterSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterShortQuarterSymbols'));
  Result := CFStringRef(_kCFDateFormatterShortQuarterSymbols^);
end;

function kCFDateFormatterStandaloneQuarterSymbols: CFStringRef;
begin
  if _kCFDateFormatterStandaloneQuarterSymbols = nil then
    _kCFDateFormatterStandaloneQuarterSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterStandaloneQuarterSymbols'));
  Result := CFStringRef(_kCFDateFormatterStandaloneQuarterSymbols^);
end;

function kCFDateFormatterShortStandaloneQuarterSymbols: CFStringRef;
begin
  if _kCFDateFormatterShortStandaloneQuarterSymbols = nil then
    _kCFDateFormatterShortStandaloneQuarterSymbols := dlsym(CFModule, PAnsiChar('kCFDateFormatterShortStandaloneQuarterSymbols'));
  Result := CFStringRef(_kCFDateFormatterShortStandaloneQuarterSymbols^);
end;

function kCFDateFormatterGregorianStartDate: CFStringRef;
begin
  if _kCFDateFormatterGregorianStartDate = nil then
    _kCFDateFormatterGregorianStartDate := dlsym(CFModule, PAnsiChar('kCFDateFormatterGregorianStartDate'));
  Result := CFStringRef(_kCFDateFormatterGregorianStartDate^);
end;

function kCFDateFormatterDoesRelativeDateFormattingKey: CFStringRef;
begin
  if _kCFDateFormatterDoesRelativeDateFormattingKey = nil then
    _kCFDateFormatterDoesRelativeDateFormattingKey := dlsym(CFModule, PAnsiChar('kCFDateFormatterDoesRelativeDateFormattingKey'));
  Result := CFStringRef(_kCFDateFormatterDoesRelativeDateFormattingKey^);
end;

