{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{        FireDAC SQLite static API wrapping classes     }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  {$HPPEMIT LINKUNIT}
{$ELSE}
  {$IFDEF WIN32}
    {$HPPEMIT '#pragma link "FireDAC.Phys.SQLiteWrapper.Stat.obj"'}
  {$ELSE}
    {$HPPEMIT '#pragma link "FireDAC.Phys.SQLiteWrapper.Stat.o"'}
  {$ENDIF}
{$ENDIF}

unit FireDAC.Phys.SQLiteWrapper.Stat;

interface

{$IFDEF FireDAC_SQLITE_STATIC}
uses
  FireDAC.Stan.Intf, FireDAC.Phys.SQLiteCli, FireDAC.Phys.SQLiteWrapper;

const
{$IFDEF FireDAC_SQLITE_EXTERNAL}
  {$IFDEF UNDERSCOREIMPORTNAME}
    _SLU = '_';
  {$ELSE}
    _SLU = '';
  {$ENDIF}
  {$IF DEFINED(MACOS) and not DEFINED(IOS)}
    C_FD_SQLiteLib = 'libcgsqlite3.dylib';
  {$ELSE}
    C_FD_SQLiteLib = 'libsqlite.a';
  {$ENDIF}
{$ELSE}
  {$IFDEF FireDAC_32}
    C_FD_SQLiteLib = 'sqlite3_x86.obj';
    {$L sqlite3_x86.obj}
  {$ENDIF}
  {$IFDEF FireDAC_64}
    C_FD_SQLiteLib = 'sqlite3_x64.obj';
    {$L sqlite3_x64.obj}
  {$ENDIF}
{$ENDIF}

type
  TSQLiteLibStat = class(TSQLiteLib)
  protected
    function GetDefaultSharedCacheMode: Integer; override;
    procedure LoadEntries; override;
  public
    procedure Load(const AVendorHome, AVendorLib: String); override;
    procedure Unload; override;
  end;

  function sqlite3_libversion(): PFDAnsiString; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_libversion' {$ENDIF};
  function sqlite3_libversion_number(): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_libversion_number' {$ENDIF};
  function sqlite3_compileoption_used(zOptName: PFDAnsiString): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_compileoption_used' {$ENDIF};
  function sqlite3_compileoption_get(N: Integer): PFDAnsiString; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_compileoption_get' {$ENDIF};
  function sqlite3_enable_shared_cache(onoff: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_enable_shared_cache' {$ENDIF};
  function sqlite3_release_memory(amount: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_release_memory' {$ENDIF};
  procedure sqlite3_soft_heap_limit(amount: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_soft_heap_limit' {$ENDIF};
  function sqlite3_status(op: Integer; var pCurrent: Integer; var pHighwater: Integer; resetFlag: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_status' {$ENDIF};
  function sqlite3_initialize(): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_initialize' {$ENDIF};
  function sqlite3_shutdown(): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_shutdown' {$ENDIF};
  function sqlite3_malloc(n: Integer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_malloc' {$ENDIF};
  function sqlite3_memory_used(): sqlite3_int64; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_memory_used' {$ENDIF};
  function sqlite3_memory_highwater(resetFlag: Integer): sqlite3_int64; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_memory_highwater' {$ENDIF};
  procedure sqlite3_free(APtr: Pointer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_free' {$ENDIF};
  function sqlite3_config(option: Integer): Integer; cdecl varargs; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_config' {$ENDIF};

  function sqlite3_open(filename: PByte; var ppDb: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_open' {$ENDIF};
  function sqlite3_open16(filename: PByte; var ppDb: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_open16' {$ENDIF};
  function sqlite3_open_v2(filename: PUtf8; var ppDb: psqlite3; flags: Integer; zVfs: PFDAnsiString): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_open_v2' {$ENDIF};
  function sqlite3_close(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_close' {$ENDIF};
  function sqlite3_busy_timeout(db: psqlite3; ms: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_busy_timeout' {$ENDIF};
  function sqlite3_busy_handler(db: psqlite3; callback: Tsqlite3_busy_callback; userdata: Pointer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_busy_handler' {$ENDIF};
  function sqlite3_trace(db: psqlite3; xTrace: Tsqlite3_trace_callback; userdata: Pointer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_trace' {$ENDIF};
  function sqlite3_profile(db: psqlite3; xProfile: Tsqlite3_profile_callback; userdata: Pointer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_profile' {$ENDIF};
  function sqlite3_set_authorizer(db: psqlite3; xAuth: Tsqlite3_auth_callback; userdata: Pointer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_set_authorizer' {$ENDIF};
  function sqlite3_get_autocommit(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_get_autocommit' {$ENDIF};
  function sqlite3_commit_hook(db: psqlite3; callback: Tsqlite3_commit_callback; userdata: Pointer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_commit_hook' {$ENDIF};
  function sqlite3_rollback_hook(db: psqlite3; callback: Tsqlite3_rollback_callback; userdata: Pointer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_rollback_hook' {$ENDIF};
  function sqlite3_table_column_metadata(db: psqlite3; zDbName: PUtf8; zTableName: PUtf8; zColumnName: PUtf8; var pzDataType: PUtf8; var pzCollSeq: PUtf8; var pNotNull: Integer; var pPrimaryKey: Integer; var pAutoinc: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_table_column_metadata' {$ENDIF};
  function sqlite3_update_hook(db: psqlite3; callback: Tsqlite3_update_callback; userdata: Pointer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_update_hook' {$ENDIF};
  function sqlite3_limit(db: psqlite3; id: Integer; newVal: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_limit' {$ENDIF};
  function sqlite3_collation_needed(db: psqlite3; userdata: Pointer; callback: Tsqlite3_collation_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_collation_needed' {$ENDIF};
  function sqlite3_create_collation(db: psqlite3; zName: PByte; eTextRep: Integer; userdata: Pointer; callback: Tsqlite3_compare_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_create_collation' {$ENDIF};
  function sqlite3_collation_needed16(db: psqlite3; userdata: Pointer; callback: Tsqlite3_collation_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_collation_needed16' {$ENDIF};
  function sqlite3_create_collation16(db: psqlite3; zName: PByte; eTextRep: Integer; userdata: Pointer; callback: Tsqlite3_compare_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_create_collation16' {$ENDIF};
  procedure sqlite3_progress_handler(db: psqlite3; nOpers: Integer; callback: Tsqlite3_progress_callback; userdata: Pointer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_progress_handler' {$ENDIF};

  function sqlite3_errcode(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_errcode' {$ENDIF};
  function sqlite3_errmsg(db: psqlite3): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_errmsg' {$ENDIF};
  function sqlite3_errmsg16(db: psqlite3): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_errmsg16' {$ENDIF};
  function sqlite3_extended_result_codes(db: psqlite3; onoff: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_extended_result_codes' {$ENDIF};
  function sqlite3_errstr(code: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_errstr' {$ENDIF};

  function sqlite3_changes(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_changes' {$ENDIF};
  function sqlite3_total_changes(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_total_changes' {$ENDIF};
  procedure sqlite3_interrupt(db: psqlite3); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_interrupt' {$ENDIF};
  function sqlite3_last_insert_rowid(db: psqlite3): sqlite3_int64; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_last_insert_rowid' {$ENDIF};
  function sqlite3_db_filename(db: psqlite3; zDbName: PUtf8): PUtf8; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_db_filename' {$ENDIF};
  function sqlite3_db_readonly(db: psqlite3; zDbName: PUtf8): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_db_readonly' {$ENDIF};
  function sqlite3_db_status(db: psqlite3; op: Integer; var pCurrent: Integer; var pHighwater: Integer; resetFlag: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_db_status' {$ENDIF};
  function sqlite3_exec(db: psqlite3; zSql: PByte; callback: Pointer; data: Pointer; errmsg: PPByte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_exec' {$ENDIF};

  function sqlite3_prepare(db: psqlite3; zSql: PByte; nByte: Integer; var ppStmt: psqlite3_stmt; var pzTail: PByte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_prepare' {$ENDIF};
  function sqlite3_prepare16(db: psqlite3; zSql: PByte; nByte: Integer; var ppStmt: psqlite3_stmt; var pzTail: PByte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_prepare16' {$ENDIF};
  function sqlite3_prepare_v2(db: psqlite3; zSql: PByte; nByte: Integer; var ppStmt: psqlite3_stmt; var pzTail: PByte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_prepare_v2' {$ENDIF};
  function sqlite3_prepare16_v2(db: psqlite3; zSql: PByte; nByte: Integer; var ppStmt: psqlite3_stmt; var pzTail: PByte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_prepare16_v2' {$ENDIF};
  function sqlite3_step(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_step' {$ENDIF};
  function sqlite3_reset(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_reset' {$ENDIF};
  function sqlite3_finalize(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_finalize' {$ENDIF};
  function sqlite3_stmt_readonly(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_stmt_readonly' {$ENDIF};
  function sqlite3_stmt_busy(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_stmt_busy' {$ENDIF};
  function sqlite3_stmt_status(stmt: psqlite3_stmt; op: Integer; resetFlg: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_stmt_status' {$ENDIF};

  function sqlite3_clear_bindings(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_clear_bindings' {$ENDIF};
  function sqlite3_bind_parameter_count(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_parameter_count' {$ENDIF};
  function sqlite3_bind_parameter_index(stmt: psqlite3_stmt; zName: PUtf8): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_parameter_index' {$ENDIF};
  function sqlite3_bind_parameter_name(stmt: psqlite3_stmt; index: Integer): PUtf8; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_parameter_name' {$ENDIF};
  function sqlite3_bind_blob(stmt: psqlite3_stmt; index: Integer; value: Pointer; nBytes: Integer; destr: Tsqlite3_destroy_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_blob' {$ENDIF};
  function sqlite3_bind_blob64(stmt: psqlite3_stmt; index: Integer; value: Pointer; nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_blob64' {$ENDIF};
  function sqlite3_bind_double(stmt: psqlite3_stmt; index: Integer; value: double): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_double' {$ENDIF};
  function sqlite3_bind_int(stmt: psqlite3_stmt; index: Integer; value: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_int' {$ENDIF};
  function sqlite3_bind_int64(stmt: psqlite3_stmt; index: Integer; value: sqlite3_int64): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_int64' {$ENDIF};
  function sqlite3_bind_null(stmt: psqlite3_stmt; index: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_null' {$ENDIF};
  function sqlite3_bind_text(stmt: psqlite3_stmt; index: Integer; value: PByte; nBytes: Integer; destr: Tsqlite3_destroy_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_text' {$ENDIF};
  function sqlite3_bind_text16(stmt: psqlite3_stmt; index: Integer; value: PByte; nBytes: Integer; destr: Tsqlite3_destroy_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_text16' {$ENDIF};
  function sqlite3_bind_text64(stmt: psqlite3_stmt; index: Integer; value: PByte; nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback; encoding: Byte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_text64' {$ENDIF};
  function sqlite3_bind_value(stmt: psqlite3_stmt; index: Integer; value: psqlite3_value): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_value' {$ENDIF};
  function sqlite3_bind_zeroblob(stmt: psqlite3_stmt; index: Integer; nBytes: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_bind_zeroblob' {$ENDIF};

  function sqlite3_column_count(stmt: psqlite3_stmt): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_count' {$ENDIF};
  function sqlite3_column_type(stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_type' {$ENDIF};
  function sqlite3_column_name(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_name' {$ENDIF};
  function sqlite3_column_name16(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_name16' {$ENDIF};
  function sqlite3_column_database_name(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_database_name' {$ENDIF};
  function sqlite3_column_database_name16(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_database_name16' {$ENDIF};
  function sqlite3_column_table_name(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_table_name' {$ENDIF};
  function sqlite3_column_table_name16(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_table_name16' {$ENDIF};
  function sqlite3_column_origin_name(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_origin_name' {$ENDIF};
  function sqlite3_column_origin_name16(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_origin_name16' {$ENDIF};
  function sqlite3_column_decltype(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_decltype' {$ENDIF};
  function sqlite3_column_decltype16(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_decltype16' {$ENDIF};
  function sqlite3_column_blob(stmt: psqlite3_stmt; iCol: Integer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_blob' {$ENDIF};
  function sqlite3_column_bytes(stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_bytes' {$ENDIF};
  function sqlite3_column_bytes16(stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_bytes16' {$ENDIF};
  function sqlite3_column_double(stmt: psqlite3_stmt; iCol: Integer): Double; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_double' {$ENDIF};
  function sqlite3_column_int(stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_int' {$ENDIF};
  function sqlite3_column_int64(stmt: psqlite3_stmt; iCol: Integer): sqlite3_int64; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_int64' {$ENDIF};
  function sqlite3_column_text(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_text' {$ENDIF};
  function sqlite3_column_text16(stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_text16' {$ENDIF};
  function sqlite3_column_value(stmt: psqlite3_stmt; iCol: Integer): psqlite3_value; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_column_value' {$ENDIF};

  function sqlite3_blob_open(db: psqlite3; zDb: PUtf8; zTable: PUtf8; zColumn: PUtf8; iRow: sqlite3_int64; flags: Integer; var ppBlob: psqlite3_blob): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_blob_open' {$ENDIF};
  function sqlite3_blob_close(blob: psqlite3_blob): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_blob_close' {$ENDIF};
  function sqlite3_blob_bytes(blob: psqlite3_blob): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_blob_bytes' {$ENDIF};
  function sqlite3_blob_read(blob: psqlite3_blob; Z: Pointer; N: Integer; iOffset: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_blob_read' {$ENDIF};
  function sqlite3_blob_write(blob: psqlite3_blob; Z: Pointer; N: Integer; iOffset: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_blob_write' {$ENDIF};

  function sqlite3_create_function(db: psqlite3; zFunctionName: PByte; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: Tsqlite3_func_callback; xStep: Tsqlite3_step_callback; xFinal: Tsqlite3_final_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_create_function' {$ENDIF};
  function sqlite3_create_function16(db: psqlite3; zFunctionName: PByte; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: Tsqlite3_func_callback; xStep: Tsqlite3_step_callback; xFinal: Tsqlite3_final_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_create_function16' {$ENDIF};
  function sqlite3_user_data(context: psqlite3_context): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_user_data' {$ENDIF};
  function sqlite3_aggregate_context(context: psqlite3_context; nBytes: Integer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_aggregate_context' {$ENDIF};
  function sqlite3_context_db_handle(context: psqlite3_context): psqlite3; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_context_db_handle' {$ENDIF};
  function sqlite3_get_auxdata(context: psqlite3_context; N: Integer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_get_auxdata' {$ENDIF};
  procedure sqlite3_set_auxdata(context: psqlite3_context; N: Integer; data: Pointer; destr: Tsqlite3_destroy_callback); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_set_auxdata' {$ENDIF};

  function sqlite3_auto_extension(xEntryPoint: Pointer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_auto_extension' {$ENDIF};
  procedure sqlite3_reset_auto_extension(); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_reset_auto_extension' {$ENDIF};
  function sqlite3_enable_load_extension(db: psqlite3; onoff: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_enable_load_extension' {$ENDIF};
  function sqlite3_load_extension(db: psqlite3; zFile, zProc: PByte; var pzErrMsg: PByte): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_load_extension' {$ENDIF};

  function sqlite3_create_module(db: psqlite3; zName: PUtf8; module: psqlite3_module; userdata: Pointer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_create_module' {$ENDIF};
  function sqlite3_create_module_v2(db: psqlite3; zName: PUtf8; module: psqlite3_module; userdata: Pointer; destr: Tsqlite3_destroy_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_create_module_v2' {$ENDIF};
  function sqlite3_declare_vtab(db: psqlite3; zCreateTable: PUtf8): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_declare_vtab' {$ENDIF};
  function sqlite3_overload_function(db: psqlite3; zFuncName: PUtf8; nArg: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_overload_function' {$ENDIF};

  function sqlite3_value_blob(value: psqlite3_value): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_blob' {$ENDIF};
  function sqlite3_value_bytes(value: psqlite3_value): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_bytes' {$ENDIF};
  function sqlite3_value_bytes16(value: psqlite3_value): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_bytes16' {$ENDIF};
  function sqlite3_value_double(value: psqlite3_value): Double; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_double' {$ENDIF};
  function sqlite3_value_int(value: psqlite3_value): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_int' {$ENDIF};
  function sqlite3_value_int64(value: psqlite3_value): sqlite3_int64; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_int64' {$ENDIF};
  function sqlite3_value_text(value: psqlite3_value): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_text' {$ENDIF};
  function sqlite3_value_text16(value: psqlite3_value): PByte; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_text16' {$ENDIF};
  function sqlite3_value_type(value: psqlite3_value): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_type' {$ENDIF};
  function sqlite3_value_numeric_type(value: psqlite3_value): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_value_numeric_type' {$ENDIF};

  procedure sqlite3_result_blob(context: psqlite3_context; value: Pointer; nBytes: Integer; destr: Tsqlite3_destroy_callback); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_blob' {$ENDIF};
  procedure sqlite3_result_blob64(context: psqlite3_context; value: Pointer; nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_blob64' {$ENDIF};
  procedure sqlite3_result_double(context: psqlite3_context; value: Double); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_double' {$ENDIF};
  procedure sqlite3_result_int(context: psqlite3_context; value: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_int' {$ENDIF};
  procedure sqlite3_result_int64(context: psqlite3_context; value: sqlite3_int64); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_int64' {$ENDIF};
  procedure sqlite3_result_null(context: psqlite3_context); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_null' {$ENDIF};
  procedure sqlite3_result_text(context: psqlite3_context; value: PByte; nBytes: Integer; destr: Tsqlite3_destroy_callback); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_text' {$ENDIF};
  procedure sqlite3_result_text16(context: psqlite3_context; value: PByte; nBytes: Integer; destr: Tsqlite3_destroy_callback); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_text16' {$ENDIF};
  procedure sqlite3_result_text64(context: psqlite3_context; value: PByte; nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback; encoding: Byte); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_text64' {$ENDIF};
  procedure sqlite3_result_value(context: psqlite3_context; value: psqlite3_value); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_value' {$ENDIF};
  procedure sqlite3_result_zeroblob(context: psqlite3_context; n: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_zeroblob' {$ENDIF};
  procedure sqlite3_result_error(context: psqlite3_context; msg: PByte; nBytes: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_error' {$ENDIF};
  procedure sqlite3_result_error16(context: psqlite3_context; msg: PByte; nBytes: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_error16' {$ENDIF};
  procedure sqlite3_result_error_toobig(context: psqlite3_context); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_error_toobig' {$ENDIF};
  procedure sqlite3_result_error_nomem(context: psqlite3_context); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_error_nomem' {$ENDIF};
  procedure sqlite3_result_error_code(context: psqlite3_context; code: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_result_error_code' {$ENDIF};

  function sqlite3_vfs_find(zVfsName: PFDAnsiString): psqlite3_vfs; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_vfs_find' {$ENDIF};
  function sqlite3_vfs_register(pVfs: psqlite3_vfs; makeDflt: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_vfs_register' {$ENDIF};
  function sqlite3_vfs_unregister(pVfs: psqlite3_vfs): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_vfs_unregister' {$ENDIF};

  function sqlite3_key(db: psqlite3; zKey: PByte; nKey: Integer): Integer; cdecl;
  function sqlite3_rekey(db: psqlite3; zKey: PByte; nKey: Integer): Integer; cdecl;
  function sqlite3_key_v2(db: psqlite3; zDbName: PByte; zKey: PByte; nKey: Integer): Integer; cdecl;
  function sqlite3_rekey_v2(db: psqlite3; zDbName: PByte; zKey: PByte; nKey: Integer): Integer; cdecl;
  procedure sqlite3_activate_see(see: PFDAnsiString); cdecl;
  procedure sqlite3CodecGetKey(db: psqlite3; nDb: Integer; zKey: PPointer; nKey: PInteger); cdecl;
  function sqlite3CodecAttach(db: psqlite3; nDb: Integer; zKey: Pointer; nKey: Integer): Integer; cdecl;

  function ad_sqlite3GetCacheSize(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3GetCacheSize' {$ENDIF};
  function ad_sqlite3GetEncoding(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3GetEncoding' {$ENDIF};
  function ad_sqlite3GetEncryptionMode(db: psqlite3; var name: PFDAnsiString; var len: Integer): Integer;
  function ad_sqlite3GetEncryptionError(db: psqlite3; var error: PFDAnsiString; var len: Integer; var error_code: Integer): Integer;
  procedure ad_sqlite3Error(db: psqlite3; err_code: Integer; zMessage: PByte); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3Error' {$ENDIF};

  function sqlite3_backup_init(pDest: psqlite3; zDestName: PByte; pSource: psqlite3; zSourceName: PByte): psqlite3_backup; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_backup_init' {$ENDIF};
  function sqlite3_backup_step(p: psqlite3_backup; nPage: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_backup_step' {$ENDIF};
  function sqlite3_backup_finish(p: psqlite3_backup): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_backup_finish' {$ENDIF};
  function sqlite3_backup_remaining(p: psqlite3_backup): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_backup_remaining' {$ENDIF};
  function sqlite3_backup_pagecount(p: psqlite3_backup): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_backup_pagecount' {$ENDIF};

  function sqlite3_wal_hook(db: psqlite3; callback: Tsqlite3_wal_callback; userdata: Pointer): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_wal_hook' {$ENDIF};
  function sqlite3_wal_autocheckpoint(db: psqlite3; N: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_wal_autocheckpoint' {$ENDIF};
  function sqlite3_wal_checkpoint(db: psqlite3; zDb: PFDAnsiString): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_wal_checkpoint' {$ENDIF};

  function sqlite3_rtree_geometry_callback(db: psqlite3; zGeom: PByte; xGeom: Tsqlite3_rtree_xGeom_callback; pContext: Pointer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_rtree_geometry_callback' {$ENDIF};
  function sqlite3_rtree_query_callback(db: psqlite3; zQueryFunc: PByte; xQueryFunc: Tsqlite3_rtree_xQuery_callback; pContext: Pointer; xDestructor: Tsqlite3_rtree_xDelUser_callback): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_rtree_query_callback' {$ENDIF};

  function sqlite3_vtab_config(db: psqlite3; op: Integer): Integer; cdecl varargs; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_vtab_config' {$ENDIF};
  function sqlite3_vtab_on_conflict(db: psqlite3): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_vtab_on_conflict' {$ENDIF};
{$ENDIF}

implementation

{$IFDEF FireDAC_SQLITE_STATIC}
uses
  System.SysUtils, System.Classes, System.SyncObjs,
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.Crtl,
{$ENDIF}
  FireDAC.Stan.Util, FireDAC.Stan.Consts, FireDAC.Stan.Cipher;

{-------------------------------------------------------------------------------}
{ TSQLiteLibStat                                                                }
{-------------------------------------------------------------------------------}
function TSQLiteLibStat.GetDefaultSharedCacheMode: Integer;
begin
  Result := 0;
end;

{-------------------------------------------------------------------------------}
procedure TSQLiteLibStat.Load(const AVendorHome, AVendorLib: String);
begin
  FDLLName := '<' + C_FD_SQLiteLib + ' statically linked>';
  LoadEntries;
  InternalAfterLoad(True);
end;

{-------------------------------------------------------------------------------}
procedure TSQLiteLibStat.Unload;
begin
  InternalBeforeUnload;
end;

{-------------------------------------------------------------------------------}
procedure TSQLiteLibStat.LoadEntries;
begin
  @Fsqlite3_libversion := @sqlite3_libversion;
  FVersionStr := TFDEncoder.Deco(Fsqlite3_libversion(), -1, ecANSI);
  @Fsqlite3_libversion_number := @sqlite3_libversion_number;
  FVersion := Fsqlite3_libversion_number();
  FVersion :=
    (FVersion div 1000000) * 100000000 +
    ((FVersion mod 1000000) div 1000) * 1000000 +
    (FVersion mod 1000) * 10000;
  @Fsqlite3_compileoption_used := @sqlite3_compileoption_used;
  @Fsqlite3_compileoption_get := @sqlite3_compileoption_get;

  @Fsqlite3_initialize := @sqlite3_initialize;
  @Fsqlite3_shutdown := @sqlite3_shutdown;
  @Fsqlite3_config := @sqlite3_config;
  @Fsqlite3_close := @sqlite3_close;
  @Fsqlite3_errcode := @sqlite3_errcode;
  @Fsqlite3_errmsg := @sqlite3_errmsg16;
  @Fsqlite3_errstr := @sqlite3_errstr;
  @Fsqlite3_extended_result_codes := @sqlite3_extended_result_codes;
  @Fsqlite3_open := @sqlite3_open16;
  @Fsqlite3_open_v2 := @sqlite3_open_v2;
  @Fsqlite3_activate_see := @sqlite3_activate_see;
  @Fsqlite3_key := @sqlite3_key;
  @Fsqlite3_rekey := @sqlite3_rekey;
  @Fsqlite3_trace := @sqlite3_trace;
  @Fsqlite3_profile := @sqlite3_profile;
  @Fsqlite3_busy_timeout := @sqlite3_busy_timeout;
  @Fsqlite3_get_autocommit := @sqlite3_get_autocommit;
  @Fsqlite3_set_authorizer := @sqlite3_set_authorizer;
  @Fsqlite3_update_hook := @sqlite3_update_hook;
  @Fsqlite3_limit := @sqlite3_limit;
  @Fsqlite3_changes := @sqlite3_changes;
  @Fsqlite3_total_changes := @sqlite3_total_changes;
  @Fsqlite3_interrupt := @sqlite3_interrupt;
  @Fsqlite3_last_insert_rowid := @sqlite3_last_insert_rowid;
  @Fsqlite3_db_status := @sqlite3_db_status;
  @Fsqlite3_exec := @sqlite3_exec;
  @Fsqlite3_enable_shared_cache := @sqlite3_enable_shared_cache;
  @Fsqlite3_release_memory := @sqlite3_release_memory;
  @Fsqlite3_soft_heap_limit := @sqlite3_soft_heap_limit;
  @Fsqlite3_malloc := @sqlite3_malloc;
  @Fsqlite3_memory_used := @sqlite3_memory_used;
  @Fsqlite3_memory_highwater := @sqlite3_memory_highwater;
  @Fsqlite3_status := @sqlite3_status;
  @Fsqlite3_prepare := @sqlite3_prepare16_v2;
  @Fsqlite3_finalize := @sqlite3_finalize;
  @Fsqlite3_step := @sqlite3_step;
  @Fsqlite3_reset := @sqlite3_reset;
  @Fsqlite3_stmt_status := @sqlite3_stmt_status;
  @Fsqlite3_column_count := @sqlite3_column_count;
  @Fsqlite3_column_type := @sqlite3_column_type;
  @Fsqlite3_column_name := @sqlite3_column_name16;
  @Fsqlite3_column_database_name := @sqlite3_column_database_name16;
  @Fsqlite3_column_table_name := @sqlite3_column_table_name16;
  @Fsqlite3_column_origin_name := @sqlite3_column_origin_name16;
  @Fsqlite3_table_column_metadata := @sqlite3_table_column_metadata;
  @Fsqlite3_column_decltype := @sqlite3_column_decltype16;
  @Fsqlite3_column_blob := @sqlite3_column_blob;
  @Fsqlite3_column_double := @sqlite3_column_double;
  @Fsqlite3_column_int64 := @sqlite3_column_int64;
  @Fsqlite3_column_text := @sqlite3_column_text16;
  @Fsqlite3_column_bytes_row := @sqlite3_column_bytes;
  @Fsqlite3_column_bytes := @sqlite3_column_bytes16;
  @Fsqlite3_clear_bindings := @sqlite3_clear_bindings;
  @Fsqlite3_bind_parameter_count := @sqlite3_bind_parameter_count;
  @Fsqlite3_bind_parameter_index := @sqlite3_bind_parameter_index;
  @Fsqlite3_bind_parameter_name := @sqlite3_bind_parameter_name;
  @Fsqlite3_bind_blob := @sqlite3_bind_blob;
  @Fsqlite3_bind_blob64 := @sqlite3_bind_blob64;
  @Fsqlite3_bind_double := @sqlite3_bind_double;
  @Fsqlite3_bind_int64 := @sqlite3_bind_int64;
  @Fsqlite3_bind_null := @sqlite3_bind_null;
  @Fsqlite3_bind_text := @sqlite3_bind_text16;
  @Fsqlite3_bind_text64 := @sqlite3_bind_text64;
  @Fsqlite3_bind_value := @sqlite3_bind_value;
  @Fsqlite3_bind_zeroblob := @sqlite3_bind_zeroblob;
  @Fsqlite3_value_type := @sqlite3_value_type;
  @Fsqlite3_value_blob := @sqlite3_value_blob;
  @Fsqlite3_value_bytes := @sqlite3_value_bytes16;
  @Fsqlite3_value_double := @sqlite3_value_double;
  @Fsqlite3_value_int64 := @sqlite3_value_int64;
  @Fsqlite3_value_text := @sqlite3_value_text16;
  @Fsqlite3_result_blob := @sqlite3_result_blob;
  @Fsqlite3_result_blob64 := @sqlite3_result_blob64;
  @Fsqlite3_result_double := @sqlite3_result_double;
  @Fsqlite3_result_error := @sqlite3_result_error16;
  @Fsqlite3_result_error_code := @sqlite3_result_error_code;
  @Fsqlite3_result_zeroblob := @sqlite3_result_zeroblob;
  @Fsqlite3_result_int64 := @sqlite3_result_int64;
  @Fsqlite3_result_null := @sqlite3_result_null;
  @Fsqlite3_result_text := @sqlite3_result_text16;
  @Fsqlite3_result_text64 := @sqlite3_result_text64;
  @Fsqlite3_create_collation := @sqlite3_create_collation16;
  @Fsqlite3_create_function := @sqlite3_create_function16;
  @Fsqlite3_user_data := @sqlite3_user_data;
  @Gsqlite3_user_data := @Fsqlite3_user_data;
  @Fsqlite3_enable_load_extension := @sqlite3_enable_load_extension;
  @Fsqlite3_load_extension := @sqlite3_load_extension;
  @Fsqlite3_free := @sqlite3_free;
  @Fsqlite3_progress_handler := @sqlite3_progress_handler;
  @Fsqlite3_declare_vtab := @sqlite3_declare_vtab;
  @Fsqlite3_create_module := @sqlite3_create_module;
  @Fsqlite3_create_module_v2 := @sqlite3_create_module_v2;
  @Fsqlite3_vfs_find := @sqlite3_vfs_find;
  @Fsqlite3_vfs_register := @sqlite3_vfs_register;
  @Fsqlite3_vfs_unregister := @sqlite3_vfs_unregister;
  @Fsqlite3_backup_init := @sqlite3_backup_init;
  @Fsqlite3_backup_step := @sqlite3_backup_step;
  @Fsqlite3_backup_finish := @sqlite3_backup_finish;
  @Fsqlite3_backup_remaining := @sqlite3_backup_remaining;
  @Fsqlite3_backup_pagecount := @sqlite3_backup_pagecount;
  @Fsqlite3_wal_hook := @sqlite3_wal_hook;
  @Fsqlite3_wal_autocheckpoint := @sqlite3_wal_autocheckpoint;
  @Fsqlite3_wal_checkpoint := @sqlite3_wal_checkpoint;
  @Fsqlite3_rtree_geometry_callback := @sqlite3_rtree_geometry_callback;
  @Fsqlite3_rtree_query_callback := @sqlite3_rtree_query_callback;
  @Fsqlite3_blob_open := @sqlite3_blob_open;
  @Fsqlite3_blob_close := @sqlite3_blob_close;
  @Fsqlite3_blob_bytes := @sqlite3_blob_bytes;
  @Fsqlite3_blob_read := @sqlite3_blob_read;
  @Fsqlite3_blob_write := @sqlite3_blob_write;
  @Fsqlite3_vtab_config := @sqlite3_vtab_config;
  @Fsqlite3_vtab_on_conflict := @sqlite3_vtab_on_conflict;
  @Fad_sqlite3GetCacheSize := @ad_sqlite3GetCacheSize;
  @Fad_sqlite3GetEncoding := @ad_sqlite3GetEncoding;
  @Fad_sqlite3GetEncryptionMode := @ad_sqlite3GetEncryptionMode;
  @Fad_sqlite3GetEncryptionError := @ad_sqlite3GetEncryptionError;
  @Fad_sqlite3Error := @ad_sqlite3Error;
end;

{-------------------------------------------------------------------------------}
// FireDAC SQLite driver encryption feature is derived from the following work:
(*
** SQLCipher
** crypto.c developed by Stephen Lombardo (Zetetic LLC)
** sjlombardo at zetetic dot net
** http://zetetic.net
**
** Copyright (c) 2009, ZETETIC LLC
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in the
**       documentation and/or other materials provided with the distribution.
**     * Neither the name of the ZETETIC LLC nor the
**       names of its contributors may be used to endorse or promote products
**       derived from this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY ZETETIC LLC ''AS IS'' AND ANY
** EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL ZETETIC LLC BE LIABLE FOR ANY
** DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
** ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
*)
type
  // Internal structures
  _pPgHdr = Pointer;
  _pPager = Pointer;
  _pBtree = Pointer;
  _pDb = ^_Db;
  _Pgno = Cardinal;

  _Db = record
    zName: PFDAnsiString;
    pBt: _pBtree;
    // .................
  end;

  {-----------------------------------------------------------------------------}
  // Codec private structures
  PSQLiteCipherCtx = ^TSQLiteCipherCtx;
  TSQLiteCipherCtx = record
    derive_key: Boolean;
    evp_cipher_class: TFDCipherClass;
    evp_cipher: TFDCipher;
    key_sz: Integer;
    pass_sz: Integer;
    iv_sz: Integer;
    name_sz: Integer;
    pass: PFDAnsiString;
  end;

  PSQLiteCodecCtx = ^TSQLiteCodecCtx;
  TSQLiteCodecCtx = record
    page_size: Integer;
    kdf_salt_sz: Integer;
    buffer_sz: Integer;
    kdf_salt: PByte;
    buffer: PByte;
    read_ctx: PSQLiteCipherCtx;
    write_ctx: PSQLiteCipherCtx;
    mode_rekey: Boolean;
    error: PFDAnsiString;
    error_sz: Integer;
    error_code: Integer;
  end;

  TSQLiteCodecProc = function (iCtx: Pointer; data: Pointer; pgn: _Pgno; mode: Integer): Pointer; cdecl;
  TSQLiteCodecSizeChngProc = procedure (iCtx: Pointer; pageSize: Integer; nReserve: Integer); cdecl;
  TSQLiteCodecFreeProc = procedure (iCtx: Pointer); cdecl;

const
  // Adjust SQLite file header size to make the payload size equal to factor
  // of C_FD_AESBlockSize. SQLite is using only <= 70 bytes in the header.
  C_SQLiteFileHeaderSize = 100 - 100 mod C_FD_AESBlockSize;
  C_SQLiteWellKnownSize = 16;
  C_SQLiteMaxKeyLength = 32;
  C_SQLiteDefaultKeyLength = 32;
  C_SQLiteDefaultCipherClass: TFDCipherClass = TFDCipherAES;
  C_SQLiteReservedSpace = 32;

var
  GCodecAttachLock: TCriticalSection;

// sqlite3.obj must export these entries
procedure sqlite3_randomness(N: Integer; P: Pointer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'sqlite3_randomness' {$ENDIF};
procedure ad_sqlite3PagerSetCodec(pPager: _pPager; xCodec: TSQLiteCodecProc; xCodecSizeChng: TSQLiteCodecSizeChngProc; xCodecFree: TSQLiteCodecFreeProc; pCodec: Pointer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerSetCodec' {$ENDIF};
function ad_sqlite3BtreeGetPageSize(p: _pBtree): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3BtreeGetPageSize' {$ENDIF};
function ad_sqlite3BtreeSetPageSize(p: _pBtree; pageSize: Integer; nReserve: Integer; iFix: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3BtreeSetPageSize' {$ENDIF};
function ad_sqlite3BtreeGetPager(p: _pBtree): _pPager; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3BtreeGetPager' {$ENDIF};
function ad_sqlite3BtreeBeginTrans(p: _pBtree; wrflag: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3BtreeBeginTrans' {$ENDIF};
function ad_sqlite3BtreeCommit(p: _pBtree): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3BtreeCommit' {$ENDIF};
function ad_sqlite3BtreeRollback(p: _pBtree): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3BtreeRollback' {$ENDIF};
function ad_sqlite3PagerGetFd(pPager: _pPager): psqlite3_file; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerGetFd' {$ENDIF};
function ad_sqlite3PagerGetCodec(pPager: _pPager): Pointer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerGetCodec' {$ENDIF};
procedure ad_sqlite3PagerPagecount(pPager: _pPager; var pnPage: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerPagecount' {$ENDIF};
function ad_sqlite3PagerIsMjPgno(pPager: _pPager; pgn: _Pgno): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerIsMjPgno' {$ENDIF};
function ad_sqlite3PagerGet(pPager: _pPager; pgn: _Pgno; var ppPage: _pPgHdr): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerGet' {$ENDIF};
function ad_sqlite3PagerWrite(pPage: _pPgHdr): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerWrite' {$ENDIF};
procedure ad_sqlite3PagerUnref(pPage: _pPgHdr); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3PagerUnref' {$ENDIF};
function ad_sqlite3GetBackend(db: psqlite3; nDb: Integer): _pDb; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3GetBackend' {$ENDIF};
procedure ad_sqlite3SetNextPagesize(db: psqlite3; size: Integer); cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3SetNextPagesize' {$ENDIF};
function ad_sqlite3GetFileHeader(): PFDAnsiString; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3GetFileHeader' {$ENDIF};
function ad_sqlite3RunVacuum(db: psqlite3; pDb: _pDb; nReserved: Integer): Integer; cdecl; external {$IFDEF FireDAC_SQLITE_EXTERNAL} C_FD_SQLiteLib name _SLU + 'ad_sqlite3RunVacuum' {$ENDIF};
{$IFDEF FireDAC_SQLITE_EXTERNAL}
// sqlite3_fd_glue.obj must export this entry
procedure ad_sqlite3SetEncryptionCallbacks(cb_activate_see: Tsqlite3_activate_see; cb_key_v2: Tsqlite3_key_v2; cb_rekey_v2: Tsqlite3_rekey_v2; cb_CodecGetKey: Tsqlite3CodecGetKey; cb_CodecAttach: Tsqlite3CodecAttach); cdecl; external C_FD_SQLiteLib name _SLU + 'ad_sqlite3SetEncryptionCallbacks';
{$ENDIF}

{-------------------------------------------------------------------------------}
procedure SetCodecError(ctx: PSQLiteCodecCtx; const AMsg: String; AErrorCode: Integer);
var
  sMsg: TFDByteString;
begin
  if ctx^.error <> nil then begin
    FreeMem(ctx^.error, ctx^.error_sz);
    ctx^.error := nil;
    ctx^.error_sz := 0;
    ctx^.error_code := 0;
  end;
  if AMsg <> '' then begin
    sMsg := TFDEncoder.Enco('Cipher: ' + AMsg, ecANSI);
    ctx^.error_sz := TFDEncoder.EncoLength(sMsg, ecANSI);
    GetMem(ctx^.error, ctx^.error_sz);
    Move(PByte(sMsg)^, ctx^.error^, ctx^.error_sz);
    ctx^.error_code := AErrorCode;
  end;
end;

{-------------------------------------------------------------------------------}
function sqlite3Codec(iCtx: Pointer; data: Pointer; pgn: _Pgno; mode: Integer): Pointer; cdecl;
var
  pCtx: PSQLiteCodecCtx;
  iPageSize: Integer;
  iOffset: Integer;
  pData: PByte;

  function KeyDerive(ctx: PSQLiteCodecCtx; c_ctx: PSQLiteCipherCtx): Integer;
  begin
    // if pass is not null
    if (c_ctx^.pass <> nil) and (c_ctx^.pass_sz <> 0) then begin
      if c_ctx^.evp_cipher = nil then
        c_ctx^.evp_cipher := c_ctx^.evp_cipher_class.Create;
      c_ctx^.evp_cipher.Initialize(PByte(c_ctx^.pass), c_ctx^.pass_sz,
        PAESBlock(ctx^.kdf_salt)^, ctx^.kdf_salt_sz, c_ctx^.key_sz * 8);
      Result := SQLITE_OK;
    end
    else begin
      SetCodecError(ctx, 'Password must be not empty', er_FD_SQLitePwdInvalid);
      Result := SQLITE_ERROR;
    end;
  end;

  function Cipher(ctx: PSQLiteCodecCtx; c_ctx: PSQLiteCipherCtx; AEncrypt: Boolean;
    ASize: Integer; ASrc, ADest: Pointer; APageNum: Cardinal): Integer;
  begin
    Result := SQLITE_OK;
    if c_ctx^.key_sz = 0 then
      Move(ASrc^, ADest^, ASize)
    else if not Assigned(c_ctx^.evp_cipher) then begin
      SetCodecError(ctx, 'Algorythm is not assigned', er_FD_SQLiteAlgFailure);
      Result := SQLITE_ERROR;
    end
    else if (ASize mod C_FD_AESBlockSize) <> 0 then begin
      SetCodecError(ctx, 'Invalid block size', er_FD_SQLiteAlgFailure);
      Result := SQLITE_ERROR;
    end
    else
      case c_ctx^.evp_cipher.Process(PAESBlock(ASrc), PAESBlock(ADest),
        ASize div C_FD_AESBlockSize, APageNum, AEncrypt) of
      1:
        begin
          if (pgn = 1) and (mode in [0, 2, 3]) and
             CompareMem(ad_sqlite3GetFileHeader(), data, C_SQLiteWellKnownSize) then
            SetCodecError(ctx, 'DB is not encrypted', er_FD_SQLiteDBUnencrypted)
          else
            SetCodecError(ctx, 'Invalid password is specified or DB is corrupted', er_FD_SQLitePwdInvalid);
          Result := SQLITE_ERROR;
        end;
      2:
        begin
          SetCodecError(ctx, 'Failed to initialize algorythm', er_FD_SQLiteAlgFailure);
          Result := SQLITE_ERROR;
        end;
      3:
        if (pgn = 1) and (mode in [0, 2, 3]) and
           CompareMem(ad_sqlite3GetFileHeader(), data, C_SQLiteWellKnownSize) then begin
          SetCodecError(ctx, 'DB is not encrypted', er_FD_SQLiteDBUnencrypted);
          Result := SQLITE_ERROR;
        end;
      end;
  end;

begin
  pCtx := PSQLiteCodecCtx(iCtx);
  try
    iPageSize := pCtx^.page_size;
    pData := PByte(data);

    // derive key on first use if necessary
    if pCtx^.read_ctx^.derive_key then begin
      KeyDerive(pCtx, pCtx^.read_ctx);
      pCtx^.read_ctx^.derive_key := False;
    end;

    if pCtx^.write_ctx^.derive_key then begin
      KeyDerive(pCtx, pCtx^.write_ctx);
      pCtx^.write_ctx^.derive_key := False;
    end;

    // adjust buffer size is a page size is changed
    if iPageSize > pCtx^.buffer_sz then begin
      pCtx^.buffer_sz := iPageSize;
      ReallocMem(pCtx^.buffer, iPageSize);
    end;

    // adjust starting pointers in data page for header iOffset on first page
    if pgn = 1 then
      iOffset := C_SQLiteFileHeaderSize
    else
      iOffset := 0;

    case mode of
    // decrypt
    0, 2, 3:
      begin
        if Cipher(pCtx, pCtx^.read_ctx, False, iPageSize - iOffset,
                  PFDAnsiString(pData) + iOffset, PFDAnsiString(pCtx^.buffer) + iOffset, pgn) = SQLITE_OK then begin
          // copy file header to the first 16 bytes of the page
          if pgn = 1 then begin
            Move(ad_sqlite3GetFileHeader()^, pCtx^.buffer^, C_SQLiteWellKnownSize);
            Move((PFDAnsiString(pData) + C_SQLiteWellKnownSize)^,
                 (PFDAnsiString(pCtx^.buffer) + C_SQLiteWellKnownSize)^,
                 C_SQLiteFileHeaderSize - C_SQLiteWellKnownSize);
          end;
          // copy buffer data back to pData and return
          Move(pCtx^.buffer^, pData^, iPageSize);
          Result := pData;
        end
        else
          Result := nil;
      end;
    // encrypt
    6, 7:
      begin
        if Cipher(pCtx, pCtx^.write_ctx, True, iPageSize - iOffset,
                  PFDAnsiString(pData) + iOffset, PFDAnsiString(pCtx^.buffer) + iOffset, pgn) = SQLITE_OK then begin
          // copy salt to output buffer
          if pgn = 1 then begin
            if pCtx^.write_ctx^.key_sz = 0 then
              Move(pData^, pCtx^.buffer^, iOffset)
            else begin
              Move(pCtx^.kdf_salt^, pCtx^.buffer^, C_SQLiteWellKnownSize);
              Move((PFDAnsiString(pData) + C_SQLiteWellKnownSize)^,
                   (PFDAnsiString(pCtx^.buffer) + C_SQLiteWellKnownSize)^,
                   C_SQLiteFileHeaderSize - C_SQLiteWellKnownSize);
            end;
          end;
          // return persistent buffer data, pData remains intact
          Result := pCtx^.buffer;
        end
        else
          Result := nil;
      end;
    else
      Result := pData;
    end;
    if Result <> nil then
      SetCodecError(pCtx, '', 0);
  except
    on E: Exception do begin
      SetCodecError(pCtx, E.Message, er_FD_SQLiteAlgFailure);
      Result := nil;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure sqlite3CodecFree(iCtx: Pointer); cdecl;

  procedure FreeCiperCtx(var ApCtx: PSQLiteCipherCtx);
  begin
    if ApCtx = nil then
      Exit;
    if ApCtx^.pass <> nil then
      FreeMem(ApCtx^.pass);
    FDFreeAndNil(ApCtx^.evp_cipher);
    FreeMem(ApCtx);
    ApCtx := nil;
  end;

var
  pCtx: PSQLiteCodecCtx;
begin
  pCtx := PSQLiteCodecCtx(iCtx);
  if pCtx = nil then
    Exit;
  FreeMem(pCtx^.kdf_salt, 0);
  FreeMem(pCtx^.buffer, 0);
  FreeCiperCtx(pCtx^.read_ctx);
  FreeCiperCtx(pCtx^.write_ctx);
  SetCodecError(pCtx, '', 0);
  FreeMem(pCtx);
end;

{-------------------------------------------------------------------------------}
procedure sqlite3CodecSizeChng(iCtx: Pointer; pageSize, nReserve: Integer); cdecl;
var
  pCtx: PSQLiteCodecCtx;
begin
  pCtx := PSQLiteCodecCtx(iCtx);
  if pCtx = nil then
    Exit;
  pCtx^.page_size := pageSize;
end;

{-------------------------------------------------------------------------------}
procedure SetPasswordCtx(ApCtx: PSQLiteCipherCtx; zKey: Pointer; nKey: Integer);
begin
  ApCtx^.pass_sz := nKey;
  ApCtx^.key_sz := 0;
  ApCtx^.iv_sz := 0;
  if ApCtx^.pass <> nil then begin
    FreeMem(ApCtx^.pass, ApCtx^.pass_sz);
    ApCtx^.pass := nil;
  end;
  if nKey <> 0 then begin
    GetMem(ApCtx^.pass, nKey);
    Move(zKey^, ApCtx^.pass^, nKey);
    if not FDCipherParsePassword(zKey, nKey, ApCtx^.evp_cipher_class,
                                 ApCtx^.key_sz, ApCtx^.name_sz) then begin
      ApCtx^.evp_cipher_class := C_SQLiteDefaultCipherClass;
      ApCtx^.key_sz := C_SQLiteDefaultKeyLength;
    end;
    ApCtx^.iv_sz := ApCtx^.evp_cipher_class.ReserveLength;
  end;
  ApCtx^.derive_key := True;
end;

{-------------------------------------------------------------------------------}
function ComparePasswordCtx(ApCtx: PSQLiteCipherCtx; zKey: Pointer; nKey: Integer): Integer;
begin
  Result := FDCompareByteStr(PByte(ApCtx^.pass), PByte(zKey),
    ApCtx^.pass_sz, nKey);
end;

{-------------------------------------------------------------------------------}
function sqlite3CodecAttach(db: psqlite3; nDb: Integer; zKey: Pointer;
  nKey: Integer): Integer; cdecl;

  procedure NewCipherCtx(var ApCtx: PSQLiteCipherCtx);
  begin
    GetMem(ApCtx, SizeOf(TSQLiteCipherCtx));
    FillChar(ApCtx^, SizeOf(TSQLiteCipherCtx), 0);
  end;

var
  pDb: _pDb;
  pCtx: PSQLiteCodecCtx;
  pPager: _pPager;
  fd: psqlite3_file;
  iPageSize: Integer;
begin
  GCodecAttachLock.Acquire;
  try
    try
      pDb := ad_sqlite3GetBackend(db, nDb);
      if pDb^.pBt <> nil then begin
        pPager := ad_sqlite3BtreeGetPager(pDb^.pBt);
        iPageSize := ad_sqlite3BtreeGetPageSize(pDb^.pBt);
        pCtx := ad_sqlite3PagerGetCodec(pPager);

        if pCtx <> nil then
          pCtx^.page_size := iPageSize;
        if (pCtx <> nil) and (pCtx^.read_ctx <> nil) and (pCtx.read_ctx^.key_sz <> 0) then begin
          if ComparePasswordCtx(pCtx.read_ctx, zKey, nKey) <> 0 then begin
            SetCodecError(pCtx, 'Invalid password is specified', er_FD_SQLitePwdInvalid);
            Result := SQLITE_ERROR;
          end
          else
            Result := SQLITE_OK;
          Exit;
        end;
        if (nKey = 0) or (zKey = nil) then begin
          Result := SQLITE_OK;
          Exit;
        end;

        GetMem(pCtx, SizeOf(TSQLiteCodecCtx));
        FillChar(pCtx^, SizeOf(TSQLiteCodecCtx), 0);
        pCtx^.page_size := iPageSize;
        NewCipherCtx(pCtx^.read_ctx);
        NewCipherCtx(pCtx^.write_ctx);

        // pre-allocate a page buffer of PageSize bytes. This will
        // be used as a persistent buffer for encryption and decryption
        // operations to avoid overhead of multiple memory allocations
        pCtx^.buffer_sz := iPageSize;
        GetMem(pCtx^.buffer, iPageSize);

        // allocate space for salt data. Then read the first 16 bytes
        // directly off the database file. This is the salt for the
        // key derivation function. If we get a short read allocate
        // a new random salt value
        pCtx^.kdf_salt_sz := C_SQLiteWellKnownSize;
        GetMem(pCtx^.kdf_salt, pCtx^.kdf_salt_sz);

        // if unable to read the bytes, generate random salt
        fd := ad_sqlite3PagerGetFd(pPager);
        if (fd = nil) or
           (fd^.pMethods^.xRead(fd, pCtx^.kdf_salt, C_SQLiteWellKnownSize, 0) <> SQLITE_OK) then
          sqlite3_randomness(C_SQLiteWellKnownSize, PFDAnsiString(pCtx^.kdf_salt));

        ad_sqlite3PagerSetCodec(pPager, sqlite3Codec, sqlite3CodecSizeChng, sqlite3CodecFree, pCtx);
        SetPasswordCtx(pCtx^.read_ctx, zKey, nKey);
        SetPasswordCtx(pCtx^.write_ctx, zKey, nKey);
        // Do not check result - it will be SQLITE_READONLY for not empty DB
        ad_sqlite3BtreeSetPageSize(pDb^.pBt, iPageSize, pCtx^.read_ctx^.iv_sz, 0);
      end;
      Result := SQLITE_OK;
    except
      on E: Exception do begin
        ad_sqlite3Error(db, SQLITE_ERROR,
          PByte(TFDEncoder.Enco('Cipher: ' + E.Message, ecANSI)));
        Result := SQLITE_ERROR;
      end;
    end;
  finally
    GCodecAttachLock.Release;
  end;
end;

{-------------------------------------------------------------------------------}
function sqlite3_key(db: psqlite3; zKey: PByte; nKey: Integer): Integer; cdecl;
begin
  // attach key if db and zKey are not null and nKey is > 0
  if (db <> nil) and (zKey <> nil) and (nKey > 0) then begin
    // operate only on the main db
    sqlite3CodecAttach(db, 0, zKey, nKey);
    Result := SQLITE_OK;
  end
  else begin
    if db <> nil then
      ad_sqlite3Error(db, SQLITE_ERROR,
        PByte(TFDEncoder.Enco('Cipher: Password must be not empty', ecANSI)));
    Result := SQLITE_ERROR;
  end;
end;

{-------------------------------------------------------------------------------}
function sqlite3_key_v2(db: psqlite3; zDbName: PByte; zKey: PByte; nKey: Integer): Integer; cdecl;
begin
  ad_sqlite3Error(db, SQLITE_ERROR,
    PByte(TFDEncoder.Enco('Cipher: sqlite3_key_v2 is not supported', ecANSI)));
  Result := SQLITE_ERROR;
end;

{-------------------------------------------------------------------------------}
function sqlite3_rekey(db: psqlite3; zKey: PByte; nKey: Integer): Integer; cdecl;
var
  pDb: _pDb;
  pCtx: PSQLiteCodecCtx;
  rc: Integer;
  page_count, pgn: _Pgno;
  pPage: _pPgHdr;
  pPager: _pPager;
begin
  try
    if db <> nil then begin
      pDb := ad_sqlite3GetBackend(db, 0);
      rc := SQLITE_OK;
      if pDb^.pBt <> nil then begin
        pPager := ad_sqlite3BtreeGetPager(pDb^.pBt);
        pCtx := ad_sqlite3PagerGetCodec(pPager);

        // no codec and no encryption requested
        if (pCtx = nil) and ((nKey = 0) or (zKey = nil)) then begin
          Result := SQLITE_OK;
          Exit;
        end;

        if pCtx = nil then begin
          // there was no codec attached to this database, so attach one now with a null password
          sqlite3CodecAttach(db, 0, zKey, nKey);
          pCtx := ad_sqlite3PagerGetCodec(pPager);

          // prepare this setup as if it had already been initialized
          sqlite3_randomness(pCtx^.kdf_salt_sz, pCtx^.kdf_salt);
          pCtx^.read_ctx^.key_sz := 0;
          pCtx^.read_ctx^.pass_sz := 0;
          pCtx^.read_ctx^.iv_sz := 0;
        end;

        SetPasswordCtx(pCtx^.write_ctx, zKey, nKey);
        pCtx^.mode_rekey := True;

        if pCtx^.read_ctx^.iv_sz <> pCtx^.write_ctx^.iv_sz then begin
          rc := ad_sqlite3RunVacuum(db, pDb, pCtx^.write_ctx^.iv_sz);
          if rc <> SQLITE_OK then begin
            ad_sqlite3Error(db, SQLITE_ERROR,
              PByte(TFDEncoder.Enco('Cipher: failed to reserve an envelope space', ecANSI)));
            Result := rc;
            Exit;
          end;
        end;

        // do stuff here to rewrite the database
        // 1. Create a transaction on the database
        // 2. Iterate through each page, reading it and then writing it.
        // 3. If that goes ok then commit and put zKey into read_ctx

        // begin write transaction
        rc := ad_sqlite3BtreeBeginTrans(pDb^.pBt, 1);
        if rc = SQLITE_OK then begin

          ad_sqlite3PagerPagecount(pPager, Integer(page_count));
          // pgno's start at 1 see pager.c:pagerAcquire
          pgn := 1;
          while (rc = SQLITE_OK) and (pgn <= page_count) do begin
            // skip this page (see pager.c:pagerAcquire for reasoning)
            if ad_sqlite3PagerIsMjPgno(pPager, pgn) = 0 then begin
              rc := ad_sqlite3PagerGet(pPager, pgn, pPage);
              // write page see pager_incr_changecounter for example
              if rc = SQLITE_OK then begin
                rc := ad_sqlite3PagerWrite(pPage);
                if rc = SQLITE_OK then
                  ad_sqlite3PagerUnref(pPage);
              end;
            end;
            Inc(pgn);
          end;

          // if commit was successful commit and copy the rekey data to
          // current key, else rollback to release locks
          if rc = SQLITE_OK then begin
            ad_sqlite3SetNextPagesize(db, ad_sqlite3BtreeGetPageSize(pDb^.pBt));
            rc := ad_sqlite3BtreeCommit(pDb^.pBt);
            if rc = SQLITE_OK then
              SetPasswordCtx(pCtx^.read_ctx, zKey, nKey);
          end
          else
            rc := ad_sqlite3BtreeRollback(pDb^.pBt);
        end;

        if rc <> SQLITE_OK then
          ad_sqlite3Error(db, SQLITE_ERROR,
            PByte(TFDEncoder.Enco('Cipher: failed to change the DB password', ecANSI)));

        pCtx^.mode_rekey := False;
      end;
      Result := rc;
    end
    else
      Result := SQLITE_ERROR;
  except
    on E: Exception do begin
      ad_sqlite3Error(db, SQLITE_ERROR,
        PByte(TFDEncoder.Enco('Cipher: ' + E.Message, ecANSI)));
      Result := SQLITE_ERROR;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function sqlite3_rekey_v2(db: psqlite3; zDbName: PByte; zKey: PByte; nKey: Integer): Integer; cdecl;
begin
  ad_sqlite3Error(db, SQLITE_ERROR,
    PByte(TFDEncoder.Enco('Cipher: sqlite3_rekey_v2 is not supported', ecANSI)));
  Result := SQLITE_ERROR;
end;

{-------------------------------------------------------------------------------}
procedure sqlite3_activate_see(see: PFDAnsiString); cdecl;
begin
  // do nothing, security enhancements are always active
end;

{-------------------------------------------------------------------------------}
procedure sqlite3CodecGetKey(db: psqlite3; nDb: Integer; zKey: PPointer;
  nKey: PInteger); cdecl;
var
  pDb: _pDb;
  pPager: _pPager;
  pCtx: PSQLiteCodecCtx;
begin
  pDb := ad_sqlite3GetBackend(db, nDb);
  if pDb^.pBt <> nil then begin
    pPager := ad_sqlite3BtreeGetPager(pDb^.pBt);
    pCtx := ad_sqlite3PagerGetCodec(pPager);
    // if the codec has an attached codec_context user the raw key data
    if pCtx <> nil then begin
      zKey^ := pCtx^.read_ctx^.pass;
      nKey^ := pCtx^.read_ctx^.pass_sz;
    end
    else begin
      zKey^ := nil;
      nKey^ := 0;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function ad_sqlite3GetEncryptionMode(db: psqlite3; var name: PFDAnsiString;
  var len: Integer): Integer;
var
  pDb: _pDb;
  pCtx: PSQLiteCodecCtx;
  pPager: _pPager;
begin
  name := nil;
  len := 0;
  Result := SQLITE_ERROR;
  pDb := ad_sqlite3GetBackend(db, 0);
  if pDb^.pBt <> nil then begin
    pPager := ad_sqlite3BtreeGetPager(pDb^.pBt);
    pCtx := ad_sqlite3PagerGetCodec(pPager);
    if (pCtx <> nil) and (pCtx^.write_ctx <> nil) then begin
      name := FDCipherGetName(pCtx^.write_ctx^.evp_cipher_class, pCtx^.write_ctx^.key_sz);
      if name <> nil then begin
        len := FDAnsiStrLen(name);
        Result := SQLITE_OK;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function ad_sqlite3GetEncryptionError(db: psqlite3; var error: PFDAnsiString;
  var len: Integer; var error_code: Integer): Integer;
var
  pDb: _pDb;
  pCtx: PSQLiteCodecCtx;
  pPager: _pPager;
begin
  error := nil;
  len := 0;
  error_code := 0;
  Result := SQLITE_OK;
  pDb := ad_sqlite3GetBackend(db, 0);
  if pDb^.pBt <> nil then begin
    pPager := ad_sqlite3BtreeGetPager(pDb^.pBt);
    pCtx := ad_sqlite3PagerGetCodec(pPager);
    if (pCtx <> nil) and (pCtx^.error <> nil) then begin
      error := pCtx^.error;
      len := pCtx^.error_sz;
      error_code := pCtx^.error_code;
      Result := SQLITE_ERROR;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
initialization
  TSQLiteLib.GLibClasses[slDefault] := TSQLiteLibStat;
  TSQLiteLib.GLibClasses[slStatic] := TSQLiteLibStat;
  GCodecAttachLock := TCriticalSection.Create;

{$IFDEF FireDAC_SQLITE_EXTERNAL}
  ad_sqlite3SetEncryptionCallbacks(@sqlite3_activate_see, @sqlite3_key_v2,
    @sqlite3_rekey_v2, @sqlite3CodecGetKey, @sqlite3CodecAttach);
{$ENDIF}

finalization
  FDFreeAndNil(GCodecAttachLock);
{$ENDIF}

end.
