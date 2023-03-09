{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Log;

interface

(*$HPPEMIT '#include <android/log.h>' *)

{$I Androidapi.inc}

type
  /// <summary>Android log priority values, in increasing order of priority.</summary>
  android_LogPriority = (
    /// <summary>For internal use only.</summary>
    ANDROID_LOG_UNKNOWN,
    /// <summary>The default priority, for internal use only.</summary>
    ANDROID_LOG_DEFAULT,
    /// <summary>Verbose logging. Should typically be disabled for a release
    /// apk.</summary>
    ANDROID_LOG_VERBOSE,
    /// <summary>Debug logging. Should typically be disabled for a release apk.
    /// </summary>
    ANDROID_LOG_DEBUG,
    /// <summary>Informational logging. Should typically be disabled for a
    /// release apk.</summary>
    ANDROID_LOG_INFO,
    /// <summary>Warning logging. For use with recoverable failures.</summary>
    ANDROID_LOG_WARN,
    /// <summary>Error logging. For use with unrecoverable failures.</summary>
    ANDROID_LOG_ERROR,
    /// <summary>Fatal logging. For use when aborting.</summary>
    ANDROID_LOG_FATAL,
    /// <summary>For internal use only.</summary>
    ANDROID_LOG_SILENT
 );
 {$EXTERNALSYM android_LogPriority}

/// <summary>Writes the constant string `text` to the log, with priority
/// `Priority` and tag `Tag`.</summary>
function __android_log_write(Priority: android_LogPriority; const Tag, Text: MarshaledAString): Integer; cdecl;
  external AndroidLogLib name '__android_log_write';
 {$EXTERNALSYM android_LogPriority}

{ Helper functions }
function LOGI(Text: MarshaledAString): Integer;
function LOGW(Text: MarshaledAString): Integer;
function LOGE(Text: MarshaledAString): Integer;
function LOGF(Text: MarshaledAString): Integer;

implementation

function LOGI(Text: MarshaledAString): Integer;
begin
  Result := __android_log_write(android_LogPriority.ANDROID_LOG_INFO, 'info', Text);
end;

function LOGW(Text: MarshaledAString): Integer;
begin
  Result := __android_log_write(android_LogPriority.ANDROID_LOG_WARN, 'warning', Text);
end;

function LOGE(Text: MarshaledAString): Integer;
begin
  Result := __android_log_write(android_LogPriority.ANDROID_LOG_ERROR, 'error', Text);
end;

function LOGF(Text: MarshaledAString): Integer;
begin
  Result := __android_log_write(android_LogPriority.ANDROID_LOG_FATAL, 'fatal', Text);
end;

end.
