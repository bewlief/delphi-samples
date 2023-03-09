{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{             FireDAC SQLite Call Interface             }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}

unit FireDAC.Phys.SQLiteCli;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  FireDAC.Stan.Intf, FireDAC.Stan.Consts;

const
{$IFDEF MSWINDOWS}
  C_SQLiteDll: String = 'sqlite3' + C_FD_DLLExt;
  C_BDBDll: String = 'libdb_sql51' + C_FD_DLLExt;
{$ENDIF}
{$IFDEF ANDROID}
  C_SQLiteDll: String = 'libsqlite' + C_FD_DLLExt;
  C_BDBDll: String = 'libdb_sql' + C_FD_DLLExt;
{$ELSE}
  {$IFDEF POSIX}
  C_CGSQLiteDll: String = 'libcgsqlite3' + C_FD_DLLExt;
  C_SQLiteDll: String = 'libsqlite3' + C_FD_DLLExt;
  C_BDBDll: String = 'libdb_sql' + C_FD_DLLExt;
  {$ENDIF}
{$ENDIF}

type
  PUtf8 = PFDAnsiString;
  PPUtf8 = ^PUtf8;
  sqlite3_int64 = Int64;
  sqlite3_uint64 = UInt64;
  sqlite3_rtree_dbl = Double;
  psqlite3_rtree_dbl = ^sqlite3_rtree_dbl;

  // -------------------------------------------------------------------------
  // Handles

  psqlite3 = Pointer;
  psqlite3_blob = Pointer;
  psqlite3_context = Pointer;
  psqlite3_stmt = Pointer;
  psqlite3_value = Pointer;
  ppsqlite3_value = ^psqlite3_value;
  psqlite3_backup = Pointer;

  psqlite3_index_info = ^sqlite3_index_info;
  psqlite3_index_constraint = ^sqlite3_index_constraint;
  psqlite3_index_orderby = ^sqlite3_index_orderby;
  psqlite3_index_constraint_usage = ^sqlite3_index_constraint_usage;
  psqlite3_module = ^sqlite3_module;
  psqlite3_vtab = ^sqlite3_vtab;
  psqlite3_vtab_cursor = ^sqlite3_vtab_cursor;

  psqlite3_mem_methods = ^sqlite3_mem_methods;
  psqlite3_file = ^sqlite3_file;
  psqlite3_io_methods = ^sqlite3_io_methods;
  psqlite3_vfs = ^sqlite3_vfs;

  psqlite3_rtree_geometry = ^sqlite3_rtree_geometry;
  psqlite3_rtree_query_info = ^sqlite3_rtree_query_info;

  // -------------------------------------------------------------------------
  // Callbacks

  Tsqlite3_busy_callback = function (userdata: Pointer; times: Integer): Integer; cdecl;
  Tsqlite3_func_callback = procedure (context: psqlite3_context; nargs: Integer;
    args: ppsqlite3_value); cdecl;
  Tsqlite3_step_callback = procedure (context: psqlite3_context; nargs: Integer;
    args: ppsqlite3_value); cdecl;
  Tsqlite3_final_callback = procedure (context: psqlite3_context); cdecl;
  Tsqlite3_destroy_callback = procedure (value: Pointer); cdecl;
  Tsqlite3_commit_callback = function (userdata: Pointer): Integer; cdecl;
  Tsqlite3_rollback_callback = procedure (userdata: Pointer); cdecl;
  Tsqlite3_trace_callback = procedure (userdata: Pointer; zSql: PUtf8); cdecl;
  Tsqlite3_profile_callback = procedure (userdata: Pointer; zSql: PUtf8;
    time: sqlite3_uint64); cdecl;
  Tsqlite3_auth_callback = function (userdata: Pointer; code: Integer;
    zArg1, zArg2, zArg3, zArg4: PUtf8): Integer; cdecl;
  Tsqlite3_update_callback = procedure (userdata: Pointer; oper: Integer;
    zDb, zTable: PUtf8; rowid: sqlite3_int64); cdecl;
  Tsqlite3_collation_callback = procedure (userdata: Pointer; db: psqlite3;
    eTextRep: Integer; name: Pointer {utf8/utf16}); cdecl;
  Tsqlite3_compare_callback = function (userdata: Pointer; len1: Integer;
    str1: Pointer {utf8/utf16}; len2: Integer; str2: Pointer {utf8/utf16}): Integer; cdecl;
  Tsqlite3_progress_callback = function (userdata: Pointer): Integer; cdecl;
  Tsqlite3_wal_callback = function (userdata: Pointer; db: psqlite3;
    name: PByte; nPages: Integer): Integer; cdecl;
  Tsqlite3_rtree_xGeom_callback = function (geom: psqlite3_rtree_geometry; nCoord: Integer;
    aCoord: psqlite3_rtree_dbl; var pRes: Integer): Integer; cdecl;
  Tsqlite3_rtree_xQuery_callback = function (info: psqlite3_rtree_query_info): Integer; cdecl;
  Tsqlite3_rtree_xDelUser_callback = procedure (userdata: Pointer); cdecl;

  // -------------------------------------------------------------------------
  // Virtual Table Object

  // Virtual Table Indexing Information
  sqlite3_index_constraint = record
    iColumn: Integer;     // Column on left-hand side of constraint
    op: Byte;             // Constraint operator
    usable: Byte;         // True if this constraint is usable
    iTermOffset: Integer; // Used internally - xBestIndex should ignore
  end;

  sqlite3_index_orderby = record
    iColumn: Integer;     // Column number
    desc: Byte;           // True for DESC.  False for ASC.
  end;

  sqlite3_index_constraint_usage = record
    argvIndex: Integer;   // if >0, constraint is part of argv to xFilter
    omit: Byte;           // Do not code a test for this constraint
  end;

  sqlite3_index_info = record
    // Inputs
    nConstraint: Integer;                   // Number of entries in aConstraint
    aConstraint: psqlite3_index_constraint; // Table of WHERE clause constraints
    nOrderBy: Integer;                      // Number of terms in the ORDER BY clause
    aOrderBy: psqlite3_index_orderby;       // The ORDER BY clause
    // Outputs
    aConstraintUsage: psqlite3_index_constraint_usage;
    idxNum: Integer;                        // Number used to identify the index
    idxStr: PUtf8;                          // String, possibly obtained from sqlite3_malloc
    needToFreeIdxStr: Integer;              // Free idxStr using sqlite3_free() if true
    orderByConsumed: Integer;               // True if output is already ordered
    estimatedCost: Double;                  // Estimated cost of using this index
    // Fields below are only available in SQLite 3.8.2 and later
    estimatedRows: sqlite3_int64;           // Estimated number of rows returned
    // Fields below are only available in SQLite 3.9.0 and later
    idxFlags: Integer;                      // Mask of SQLITE_INDEX_SCAN_* flags
    // Fields below are only available in SQLite 3.10.0 and later
    colUsed: sqlite3_uint64;                // Input: Mask of columns used by statement
  end;

  sqlite3_module = record
    iVersion: Integer;
    xCreate: function(db: psqlite3; pAux: Pointer; argc: Integer; argv: PPUtf8;
      var ppVTab: psqlite3_vtab; var pzErr: PUtf8): Integer; cdecl;
    xConnect: function (db: psqlite3; pAux: Pointer; argc: Integer; argv: PPUtf8;
      var ppVTab: psqlite3_vtab; var pzErr: PUtf8): Integer; cdecl;
    xBestIndex: function (pVTab: psqlite3_vtab; info: psqlite3_index_info): Integer; cdecl;
    xDisconnect: function (pVTab: psqlite3_vtab): Integer; cdecl;
    xDestroy: function (pVTab: psqlite3_vtab): Integer; cdecl;
    xOpen: function (pVTab: psqlite3_vtab; var ppCursor: psqlite3_vtab_cursor): Integer; cdecl;
    xClose: function (cursor: psqlite3_vtab_cursor): Integer; cdecl;
    xFilter: function (cursor: psqlite3_vtab_cursor; idxNum: Integer; idxStr: PUtf8;
      argc: Integer; argv: ppsqlite3_value): Integer; cdecl;
    xNext: function (cursor: psqlite3_vtab_cursor): Integer; cdecl;
    xEof: function (cursor: psqlite3_vtab_cursor): Integer; cdecl;
    xColumn: function (cursor: psqlite3_vtab_cursor; context: psqlite3_context;
      index: Integer): Integer; cdecl;
    xRowid: function (cursor: psqlite3_vtab_cursor; var pRowid: sqlite3_int64): Integer; cdecl;
    xUpdate: function (pVTab: psqlite3_vtab; nArg: Integer; apArgs: ppsqlite3_value;
      var rowid: sqlite3_int64): Integer; cdecl;
    xBegin: function (pVTab: psqlite3_vtab): Integer; cdecl;
    xSync: function (pVTab: psqlite3_vtab): Integer; cdecl;
    xCommit: function (pVTab: psqlite3_vtab): Integer; cdecl;
    xRollback: function (pVTab: psqlite3_vtab): Integer; cdecl;
    xFindFunction: function (pVTab: psqlite3_vtab; nArg: Integer; zName: PUtf8;
      var pxFunc: Tsqlite3_func_callback; var ppArg: Pointer): Integer; cdecl;
    xRename: function (pVTab: psqlite3_vtab; zNew: PUtf8): Integer; cdecl;
    // The methods above are in version 1 of the sqlite_module object. Those
    // below are for version 2 and greater.
    // SQLite 3.7.7
    xSavepoint: function (pVTab: psqlite3_vtab; p: Integer): Integer; cdecl;
    xRelease: function (pVTab: psqlite3_vtab; p: Integer): Integer; cdecl;
    xRollbackTo: function (pVTab: psqlite3_vtab; p: Integer): Integer; cdecl;
  end;

  // Virtual Table Instance Object
  sqlite3_vtab = record
    pModule: psqlite3_module; // The module for this virtual table
    nRef: Integer;            // Used internally
    zErrMsg: PUtf8;           // Error message from sqlite3_mprintf()
    // Virtual table implementations will typically add additional fields
  end;

  // Virtual Table Cursor Object
  sqlite3_vtab_cursor = record
    pVtab: psqlite3_vtab;     // Virtual table of this cursor
    // Virtual table implementations will typically add additional fields
  end;

  // -------------------------------------------------------------------------
  // OS abstraction layer

  // Memory Allocation Routines
  sqlite3_mem_methods = record
    xMalloc: function (size: Integer): Pointer; cdecl;  // Memory allocation function
    xFree: procedure (ptr: Pointer); cdecl;             // Free a prior allocation
    xRealloc: function (ptr: Pointer; size: Integer): Pointer; cdecl; // Resize an allocation
    xSize: function (ptr: Pointer): Integer; cdecl;     // Return the size of an allocation
    xRoundup: function (size: Integer): Integer; cdecl; // Round up request size to allocation size
    xInit: function (data: Pointer): Integer; cdecl;    // Initialize the memory allocator
    xShutdown: procedure (data: Pointer); cdecl;        // Deinitialize the memory allocator
    pAppData: Pointer;                                  // Argument to xInit() and xShutdown()
  end;

  // OS Interface Open File Handle
  sqlite3_file = record
    pMethods: psqlite3_io_methods; // Methods for an open file
  end;

  // OS Interface File Virtual Methods Object
  sqlite3_io_methods = record
    iVersion: Integer;
    xClose: function (pFile: psqlite3_file): Integer; cdecl;
    xRead: function (pFile: psqlite3_file; buf: Pointer; iAmt: Integer; iOfst: sqlite3_int64): Integer; cdecl;
    xWrite: function (pFile: psqlite3_file; bug: Pointer; iAmt: Integer; iOfst: sqlite3_int64): Integer; cdecl;
    xTruncate: function (pFile: psqlite3_file; size: sqlite3_int64): Integer; cdecl;
    xSync: function (pFile: psqlite3_file; flags: Integer): Integer; cdecl;
    xFileSize: function (pFile: psqlite3_file; var pSize: sqlite3_int64): Integer; cdecl;
    xLock: function (pFile: psqlite3_file; mode: Integer): Integer; cdecl;
    xUnlock: function (pFile: psqlite3_file; mode: Integer): Integer; cdecl;
    xCheckReservedLock: function (pFile: psqlite3_file; var pResOut: Integer): Integer; cdecl;
    xFileControl: function (pFile: psqlite3_file; op: Integer; pArg: Pointer): Integer; cdecl;
    xSectorSize: function (pFile: psqlite3_file): Integer; cdecl;
    xDeviceCharacteristics: function (pFile: psqlite3_file): Integer; cdecl;
    // Methods above are valid for version 1
    xShmMap: function (pFile: psqlite3_file; iPg: Integer; pgsz: Integer; i: Integer; p: PPointer): Integer; cdecl;
    xShmLock: function (pFile: psqlite3_file; offset: Integer; n: Integer; flags: Integer): Integer; cdecl;
    xShmBarrier: procedure (pFile: psqlite3_file); cdecl;
    xShmUnmap: function (pFile: psqlite3_file; deleteFlag: Integer): Integer; cdecl;
    // Methods above are valid for version 2
    xFetch: function (pFile: sqlite3_file; iOfst: sqlite3_int64; iAmt: Integer; pp: PPointer): Integer; cdecl;
    xUnfetch: function (pFile: sqlite3_file; iOfst: sqlite3_int64; p: Pointer): Integer; cdecl;
    // Methods above are valid for version 3
    // Additional methods may be added in future releases
  end;

  // OS Interface Object
  Tsqlite3_vfs_xOpen = function (pVfs: psqlite3_vfs; zName: PFDAnsiString; pFile: psqlite3_file;
      flags: Integer; var pOutFlags: Integer): Integer; cdecl;
  Tsqlite3_syscall_ptr = procedure (); cdecl;

  sqlite3_vfs = record
    iVersion: Integer;     // Structure version number
    szOsFile: Integer;     // Size of subclassed sqlite3_file
    mxPathname: Integer;   // Maximum file pathname length
    pNext: psqlite3_vfs;   // Next registered VFS
    zName: PFDAnsiString;      // Name of this virtual file system
    pAppData: Pointer;     // Pointer to application-specific data
    xOpen: Tsqlite3_vfs_xOpen;
    xDelete: function (pVfs: psqlite3_vfs; zName: PFDAnsiString; syncDir: Integer): Integer; cdecl;
    xAccess: function (pVfs: psqlite3_vfs; zName: PFDAnsiString; flags: Integer; var pResOut: Integer): Integer; cdecl;
    xFullPathname: function (pVfs: psqlite3_vfs; zName: PFDAnsiString; nOut: Integer; zOut: PFDAnsiString): Integer; cdecl;
    xDlOpen: function (pVfs: psqlite3_vfs; zFilename: PFDAnsiString): Pointer; cdecl;
    xDlError: procedure (pVfs: psqlite3_vfs; nByte: Integer; zErrMsg: PFDAnsiString); cdecl;
    xDlSym: function (pVfs: psqlite3_vfs; ptr: Pointer; zSymbol: PFDAnsiString): Pointer; cdecl;
    xDlClose: procedure (pVfs: psqlite3_vfs; ptr: Pointer); cdecl;
    xRandomness: function (pVfs: psqlite3_vfs; nByte: Integer; zOut: PFDAnsiString): Integer; cdecl;
    xSleep: function (pVfs: psqlite3_vfs; microseconds: Integer): Integer; cdecl;
    xCurrentTime: function (pVfs: psqlite3_vfs; var tm: double): Integer; cdecl;
    xGetLastError: function (pVfs: psqlite3_vfs; l: Integer; v: PFDAnsiString): Integer; cdecl;
    //
    // The methods above are in version 1 of the sqlite_vfs object
    // definition.  Those that follow are added in version 2 or later
    //
    xCurrentTimeInt64: function (pVfs: psqlite3_vfs; var tm: sqlite3_int64): Integer; cdecl;
    //
    // The methods above are in versions 1 and 2 of the sqlite_vfs object.
    // New fields may be appended in figure versions.  The iVersion
    // value will increment whenever this happens.
    xSetSystemCall: function (pVfs: psqlite3_vfs; zName: PFDAnsiString; p: Tsqlite3_syscall_ptr): Integer; cdecl;
    xGetSystemCall: function (pVfs: psqlite3_vfs; zName: PFDAnsiString): Tsqlite3_syscall_ptr; cdecl;
    xNextSystemCall: function (pVfs: psqlite3_vfs; zName: PFDAnsiString): PFDAnsiString; cdecl;
    //
    // The methods above are in versions 1 through 3 of the sqlite_vfs object.
    // New fields may be appended in figure versions.  The iVersion
    // value will increment whenever this happens.
    //
  end;

  // -------------------------------------------------------------------------
  // RTree support

  sqlite3_rtree_geometry = record
    pContext: Pointer;                  // Copy of pContext passed to s_r_g_c()
    nParam: Integer;                    // Size of array aParam[]
    aParam: psqlite3_rtree_dbl;         // Parameters passed to SQL geom function
    pUser: Pointer;                     // Callback implementation user data
    xDelUser: Tsqlite3_rtree_xDelUser_callback; // Called by SQLite to clean up pUser
  end;

  sqlite3_rtree_query_info = record
    pContext: Pointer;                  // pContext from when function registered
    nParam: Integer;                    // Number of function parameters
    aParam: psqlite3_rtree_dbl;         // value of function parameters
    pUser: Pointer;                     // callback can use this, if desired
    xDelUser: Tsqlite3_rtree_xDelUser_callback; // function to free pUser
    aCoord: psqlite3_rtree_dbl;         // Coordinates of node or entry to check
    anQueue: PCardinal;                 // Number of pending entries in the queue
    nCoord: Integer;                    // Number of coordinates
    iLevel: Integer;                    // Level of current node or entry
    mxLevel: Integer;                   // The largest iLevel value in the tree
    iRowid: sqlite3_int64;              // Rowid for current entry
    rParentScore: sqlite3_rtree_dbl;    // Score of parent node
    eParentWithin: Integer;             // Visibility of parent node
    eWithin: Integer;                   // OUT: Visiblity
    rScore: sqlite3_rtree_dbl;          // OUT: Write the score here
  end;

const
  // Allowed values for sqlite3_rtree_query.eWithin and .eParentWithin.
  NOT_WITHIN      = 0;   // Object completely outside of query region
  PARTLY_WITHIN   = 1;   // Object partially overlaps query region
  FULLY_WITHIN    = 2;   // Object fully contained within query region

const
  // -------------------------------------------------------------------------
  // Flags For File Open Operations
  SQLITE_OPEN_READONLY         = $00000001;  // Ok for sqlite3_open_v2()
  SQLITE_OPEN_READWRITE        = $00000002;  // Ok for sqlite3_open_v2()
  SQLITE_OPEN_CREATE           = $00000004;  // Ok for sqlite3_open_v2()
  SQLITE_OPEN_DELETEONCLOSE    = $00000008;  // VFS only
  SQLITE_OPEN_EXCLUSIVE        = $00000010;  // VFS only
  SQLITE_OPEN_MAIN_DB          = $00000100;  // VFS only
  SQLITE_OPEN_TEMP_DB          = $00000200;  // VFS only
  SQLITE_OPEN_TRANSIENT_DB     = $00000400;  // VFS only
  SQLITE_OPEN_MAIN_JOURNAL     = $00000800;  // VFS only
  SQLITE_OPEN_TEMP_JOURNAL     = $00001000;  // VFS only
  SQLITE_OPEN_SUBJOURNAL       = $00002000;  // VFS only
  SQLITE_OPEN_MASTER_JOURNAL   = $00004000;  // VFS only
  SQLITE_OPEN_NOMUTEX          = $00008000;  // Ok for sqlite3_open_v2()
  SQLITE_OPEN_FULLMUTEX        = $00010000;  // Ok for sqlite3_open_v2()
  // SQLite 3.7
  SQLITE_OPEN_SHAREDCACHE      = $00020000;  // Ok for sqlite3_open_v2()
  SQLITE_OPEN_PRIVATECACHE     = $00040000;  // Ok for sqlite3_open_v2()
  SQLITE_OPEN_WAL              = $00080000;  // VFS only
  SQLITE_OPEN_NOFOLLOW         = $01000000;  // Ok for sqlite3_open_v2()

  // Authorizer Return Codes
  SQLITE_DENY   = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

  // Status Parameters
  SQLITE_STATUS_MEMORY_USED          = 0;
  SQLITE_STATUS_PAGECACHE_USED       = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW   = 2;
  SQLITE_STATUS_SCRATCH_USED         = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW     = 4;
  SQLITE_STATUS_MALLOC_SIZE          = 5;
  SQLITE_STATUS_PARSER_STACK         = 6;
  SQLITE_STATUS_PAGECACHE_SIZE       = 7;
  SQLITE_STATUS_SCRATCH_SIZE         = 8;
  SQLITE_STATUS_MALLOC_COUNT         = 9;

  // Status Parameters for database connections
  SQLITE_DBSTATUS_LOOKASIDE_USED      = 0;
  SQLITE_DBSTATUS_CACHE_USED          = 1;
  SQLITE_DBSTATUS_SCHEMA_USED         = 2;
  SQLITE_DBSTATUS_STMT_USED           = 3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT       = 4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6;
  SQLITE_DBSTATUS_CACHE_HIT           = 7;
  SQLITE_DBSTATUS_CACHE_MISS          = 8;
  SQLITE_DBSTATUS_CACHE_WRITE         = 9;
  SQLITE_DBSTATUS_DEFERRED_FKS        = 10;
  SQLITE_DBSTATUS_MAX                 = 10;   // Largest defined DBSTATUS

  // Status Parameters for prepared statements
  SQLITE_STMTSTATUS_FULLSCAN_STEP     = 1;
  SQLITE_STMTSTATUS_SORT              = 2;
  SQLITE_STMTSTATUS_AUTOINDEX         = 3;
  SQLITE_STMTSTATUS_VM_STEP           = 4;
  SQLITE_STMTSTATUS_REPREPARE         = 5;
  SQLITE_STMTSTATUS_RUN               = 6;
  SQLITE_STMTSTATUS_MEMUSED           = 99;

  // Result Codes
  SQLITE_OK           = 0;   // Successful result
  // beginning-of-error-codes
  SQLITE_ERROR        = 1;   // SQL error or missing database
  SQLITE_INTERNAL     = 2;   // Internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND    = 12;   // Unknown opcode in sqlite3_file_control()
  SQLITE_FULL        = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14;   // Unable to open the database file
  SQLITE_PROTOCOL    = 15;   // Database lock protocol error
  SQLITE_EMPTY       = 16;   // Database is empty
  SQLITE_SCHEMA      = 17;   // The database schema changed
  SQLITE_TOOBIG      = 18;   // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT  = 19;   // Abort due to constraint violation
  SQLITE_MISMATCH    = 20;   // Data type mismatch
  SQLITE_MISUSE      = 21;   // Library used incorrectly
  SQLITE_NOLFS       = 22;   // Uses OS features not supported on host
  SQLITE_AUTH        = 23;   // Authorization denied
  SQLITE_FORMAT      = 24;   // Auxiliary database format error
  SQLITE_RANGE       = 25;   // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26;   // File opened that is not a database file
  SQLITE_NOTICE      = 27;   // Notifications from sqlite3_log()
  SQLITE_WARNING     = 28;   // Warnings from sqlite3_log()
  SQLITE_ROW         = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE        = 101;  // sqlite3_step() has finished executing

  // Extended Result Codes
  SQLITE_IOERR_READ              = (SQLITE_IOERR or (1 shl 8));
  SQLITE_IOERR_SHORT_READ        = (SQLITE_IOERR or (2 shl 8));
  SQLITE_IOERR_WRITE             = (SQLITE_IOERR or (3 shl 8));
  SQLITE_IOERR_FSYNC             = (SQLITE_IOERR or (4 shl 8));
  SQLITE_IOERR_DIR_FSYNC         = (SQLITE_IOERR or (5 shl 8));
  SQLITE_IOERR_TRUNCATE          = (SQLITE_IOERR or (6 shl 8));
  SQLITE_IOERR_FSTAT             = (SQLITE_IOERR or (7 shl 8));
  SQLITE_IOERR_UNLOCK            = (SQLITE_IOERR or (8 shl 8));
  SQLITE_IOERR_RDLOCK            = (SQLITE_IOERR or (9 shl 8));
  SQLITE_IOERR_DELETE            = (SQLITE_IOERR or (10 shl 8));
  SQLITE_IOERR_BLOCKED           = (SQLITE_IOERR or (11 shl 8));
  SQLITE_IOERR_NOMEM             = (SQLITE_IOERR or (12 shl 8));
  SQLITE_IOERR_ACCESS            = (SQLITE_IOERR or (13 shl 8));
  SQLITE_IOERR_CHECKRESERVEDLOCK = (SQLITE_IOERR or (14 shl 8));
  SQLITE_IOERR_LOCK              = (SQLITE_IOERR or (15 shl 8));
  SQLITE_IOERR_CLOSE             = (SQLITE_IOERR or (16 shl 8));
  SQLITE_IOERR_DIR_CLOSE         = (SQLITE_IOERR or (17 shl 8));
  SQLITE_IOERR_SHMOPEN           = (SQLITE_IOERR or (18 shl 8));
  SQLITE_IOERR_SHMSIZE           = (SQLITE_IOERR or (19 shl 8));
  SQLITE_IOERR_SHMLOCK           = (SQLITE_IOERR or (20 shl 8));
  SQLITE_IOERR_SHMMAP            = (SQLITE_IOERR or (21 shl 8));
  SQLITE_IOERR_SEEK              = (SQLITE_IOERR or (22 shl 8));
  SQLITE_IOERR_DELETE_NOENT      = (SQLITE_IOERR or (23 shl 8));
  SQLITE_IOERR_MMAP              = (SQLITE_IOERR or (24 shl 8));
  SQLITE_IOERR_GETTEMPPATH       = (SQLITE_IOERR or (25 shl 8));
  SQLITE_IOERR_CONVPATH          = (SQLITE_IOERR or (26 shl 8));
  SQLITE_IOERR_VNODE             = (SQLITE_IOERR or (27 shl 8));
  SQLITE_LOCKED_SHAREDCACHE      = (SQLITE_LOCKED or (1 shl 8));
  SQLITE_BUSY_RECOVERY           = (SQLITE_BUSY or (1 shl 8));
  SQLITE_BUSY_SNAPSHOT           = (SQLITE_BUSY or (2 shl 8));
  SQLITE_CANTOPEN_NOTEMPDIR      = (SQLITE_CANTOPEN or (1 shl 8));
  SQLITE_CANTOPEN_ISDIR          = (SQLITE_CANTOPEN or (2 shl 8));
  SQLITE_CANTOPEN_FULLPATH       = (SQLITE_CANTOPEN or (3 shl 8));
  SQLITE_CANTOPEN_CONVPATH       = (SQLITE_CANTOPEN or (4 shl 8));
  SQLITE_CORRUPT_VTAB            = (SQLITE_CORRUPT or (1 shl 8));
  SQLITE_READONLY_RECOVERY       = (SQLITE_READONLY or (1 shl 8));
  SQLITE_READONLY_CANTLOCK       = (SQLITE_READONLY or (2 shl 8));
  SQLITE_READONLY_ROLLBACK       = (SQLITE_READONLY or (3 shl 8));
  SQLITE_READONLY_DBMOVED        = (SQLITE_READONLY or (4 shl 8));
  SQLITE_ABORT_ROLLBACK          = (SQLITE_ABORT or (2 shl 8));
  SQLITE_CONSTRAINT_CHECK        = (SQLITE_CONSTRAINT or (1 shl 8));
  SQLITE_CONSTRAINT_COMMITHOOK   = (SQLITE_CONSTRAINT or (2 shl 8));
  SQLITE_CONSTRAINT_FOREIGNKEY   = (SQLITE_CONSTRAINT or (3 shl 8));
  SQLITE_CONSTRAINT_FUNCTION     = (SQLITE_CONSTRAINT or (4 shl 8));
  SQLITE_CONSTRAINT_NOTNULL      = (SQLITE_CONSTRAINT or (5 shl 8));
  SQLITE_CONSTRAINT_PRIMARYKEY   = (SQLITE_CONSTRAINT or (6 shl 8));
  SQLITE_CONSTRAINT_TRIGGER      = (SQLITE_CONSTRAINT or (7 shl 8));
  SQLITE_CONSTRAINT_UNIQUE       = (SQLITE_CONSTRAINT or (8 shl 8));
  SQLITE_CONSTRAINT_VTAB         = (SQLITE_CONSTRAINT or (9 shl 8));
  SQLITE_CONSTRAINT_ROWID        = (SQLITE_CONSTRAINT or(10 shl 8));
  SQLITE_NOTICE_RECOVER_WAL      = (SQLITE_NOTICE or (1 shl 8));
  SQLITE_NOTICE_RECOVER_ROLLBACK = (SQLITE_NOTICE or (2 shl 8));
  SQLITE_WARNING_AUTOINDEX       = (SQLITE_WARNING or (1 shl 8));
  SQLITE_AUTH_USER               = (SQLITE_AUTH or (1 shl 8));

  // Boolean results
  SQLITE_FALSE = 0;
  SQLITE_TRUE  = 1;

  // Fundamental Datatypes
  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_TEXT     = 3;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;

  // Authorizer Action Codes
  // *********************************** 3rd *********** 4th ***********
  SQLITE_CREATE_INDEX          = 1;   // Index Name      Table Name
  SQLITE_CREATE_TABLE          = 2;   // Table Name      NULL
  SQLITE_CREATE_TEMP_INDEX     = 3;   // Index Name      Table Name
  SQLITE_CREATE_TEMP_TABLE     = 4;   // Table Name      NULL
  SQLITE_CREATE_TEMP_TRIGGER   = 5;   // Trigger Name    Table Name
  SQLITE_CREATE_TEMP_VIEW      = 6;   // View Name       NULL
  SQLITE_CREATE_TRIGGER        = 7;   // Trigger Name    Table Name
  SQLITE_CREATE_VIEW           = 8;   // View Name       NULL
  SQLITE_DELETE                = 9;   // Table Name      NULL
  SQLITE_DROP_INDEX           = 10;   // Index Name      Table Name
  SQLITE_DROP_TABLE           = 11;   // Table Name      NULL
  SQLITE_DROP_TEMP_INDEX      = 12;   // Index Name      Table Name
  SQLITE_DROP_TEMP_TABLE      = 13;   // Table Name      NULL
  SQLITE_DROP_TEMP_TRIGGER    = 14;   // Trigger Name    Table Name
  SQLITE_DROP_TEMP_VIEW       = 15;   // View Name       NULL
  SQLITE_DROP_TRIGGER         = 16;   // Trigger Name    Table Name
  SQLITE_DROP_VIEW            = 17;   // View Name       NULL
  SQLITE_INSERT               = 18;   // Table Name      NULL
  SQLITE_PRAGMA               = 19;   // Pragma Name     1st arg or NULL
  SQLITE_READ                 = 20;   // Table Name      Column Name
  SQLITE_SELECT               = 21;   // NULL            NULL
  SQLITE_TRANSACTION          = 22;   // NULL            NULL
  SQLITE_UPDATE               = 23;   // Table Name      Column Name
  SQLITE_ATTACH               = 24;   // Filename        NULL
  SQLITE_DETACH               = 25;   // Database Name   NULL
  SQLITE_ALTER_TABLE          = 26;   // Database Name   Table Name
  SQLITE_REINDEX              = 27;   // Index Name      NULL
  SQLITE_ANALYZE              = 28;   // Table Name      NULL
  SQLITE_CREATE_VTABLE        = 29;   // Table Name      Module Name
  SQLITE_DROP_VTABLE          = 30;   // Table Name      Module Name
  SQLITE_FUNCTION             = 31;   // Function Name   NULL
  SQLITE_SAVEPOINT            = 32;   // Operation       Savepoint Name
  SQLITE_COPY                 = 0;    // No longer used
  SQLITE_RECURSIVE            = 33;   // NULL            NULL

  // Configuration Options
  SQLITE_CONFIG_SINGLETHREAD  = 1;  // nil
  SQLITE_CONFIG_MULTITHREAD   = 2;  // nil
  SQLITE_CONFIG_SERIALIZED    = 3;  // nil
  SQLITE_CONFIG_MALLOC        = 4;  // sqlite3_mem_methods*
  SQLITE_CONFIG_GETMALLOC     = 5;  // sqlite3_mem_methods*
  SQLITE_CONFIG_SCRATCH       = 6;  // void*, int sz, int N
  SQLITE_CONFIG_PAGECACHE     = 7;  // void*, int sz, int N
  SQLITE_CONFIG_HEAP          = 8;  // void*, int nByte, int min
  SQLITE_CONFIG_MEMSTATUS     = 9;  // boolean
  SQLITE_CONFIG_MUTEX        = 10;  // sqlite3_mutex_methods*
  SQLITE_CONFIG_GETMUTEX     = 11;  // sqlite3_mutex_methods*
  SQLITE_CONFIG_CHUNKALLOC   = 12;  // int threshold
  // previously SQLITE_CONFIG_LOOKASIDE = 13 which is now unused.
  SQLITE_CONFIG_PCACHE       = 14;  // sqlite3_pcache_methods*
  SQLITE_CONFIG_GETPCACHE    = 15;  // sqlite3_pcache_methods*
  SQLITE_CONFIG_LOG          = 16;  // xFunc, void*
  SQLITE_CONFIG_URI          = 17;  // int
  SQLITE_CONFIG_PCACHE2      = 18;  // sqlite3_pcache_methods2*
  SQLITE_CONFIG_GETPCACHE2   = 19;  // sqlite3_pcache_methods2*
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20; // int
  SQLITE_CONFIG_SQLLOG       = 21;  // xSqllog, void*
  SQLITE_CONFIG_MMAP_SIZE    = 22;  // sqlite3_int64, sqlite3_int64
  SQLITE_CONFIG_WIN32_HEAPSIZE = 23;  // int nByte
  SQLITE_CONFIG_PCACHE_HDRSZ   = 24;  // int *psz
  SQLITE_CONFIG_PMASZ          = 25;  // unsigned int szPma

  // Configuration Options
  SQLITE_DBCONFIG_MAINDBNAME     = 1000;  // const char*
  SQLITE_DBCONFIG_LOOKASIDE      = 1001;  // void* int int
  SQLITE_DBCONFIG_ENABLE_FKEY    = 1002;  // int int*
  SQLITE_DBCONFIG_ENABLE_TRIGGER = 1003;  // int int*
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004;  // int int*
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005;  // int int*
  SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE      = 1006;  // int int*
  SQLITE_DBCONFIG_ENABLE_QPSG           = 1007;  // int int*
  SQLITE_DBCONFIG_TRIGGER_EQP           = 1008;  // int int*
  SQLITE_DBCONFIG_RESET_DATABASE        = 1009;  // int int*
  SQLITE_DBCONFIG_DEFENSIVE             = 1010;  // int int*
  SQLITE_DBCONFIG_WRITABLE_SCHEMA       = 1011;  // int int*
  SQLITE_DBCONFIG_LEGACY_ALTER_TABLE    = 1012;  // int int*
  SQLITE_DBCONFIG_DQS_DML               = 1013;  // int int*
  SQLITE_DBCONFIG_DQS_DDL               = 1014;  // int int*
  SQLITE_DBCONFIG_ENABLE_VIEW           = 1015;  // int int*
  SQLITE_DBCONFIG_LEGACY_FILE_FORMAT    = 1016;  // int int*
  SQLITE_DBCONFIG_TRUSTED_SCHEMA        = 1017;  // int int*
  SQLITE_DBCONFIG_MAX                   = 1017;  // Largest DBCONFIG

  // Virtual Table Indexing Information
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;
  // 3.10.0
  SQLITE_INDEX_CONSTRAINT_LIKE      = 65;
  SQLITE_INDEX_CONSTRAINT_GLOB      = 66;
  SQLITE_INDEX_CONSTRAINT_REGEXP    = 67;
  // 3.21.0
  SQLITE_INDEX_CONSTRAINT_NE        = 68;
  SQLITE_INDEX_CONSTRAINT_ISNOT     = 69;
  SQLITE_INDEX_CONSTRAINT_ISNOTNULL = 70;
  SQLITE_INDEX_CONSTRAINT_ISNULL    = 71;
  SQLITE_INDEX_CONSTRAINT_IS        = 72;

  // Virtual Table Scan Flags
  SQLITE_INDEX_SCAN_UNIQUE      = 1; // Scan visits at most 1 row

  // Virtual Table Safecall result
  E_SQLITE_VTAB_RES = HRESULT($8000);

  // Run-Time Limit Categories
  SQLITE_LIMIT_LENGTH                    = 0;
  SQLITE_LIMIT_SQL_LENGTH                = 1;
  SQLITE_LIMIT_COLUMN                    = 2;
  SQLITE_LIMIT_EXPR_DEPTH                = 3;
  SQLITE_LIMIT_COMPOUND_SELECT           = 4;
  SQLITE_LIMIT_VDBE_OP                   = 5;
  SQLITE_LIMIT_FUNCTION_ARG              = 6;
  SQLITE_LIMIT_ATTACHED                  = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH       = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER           = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH             = 10;
  SQLITE_LIMIT_WORKER_THREADS            = 11;

  // Constants Defining Special Destructor Behavior
var
  SQLITE_STATIC: Tsqlite3_destroy_callback;
  SQLITE_TRANSIENT: Tsqlite3_destroy_callback;

const
  // Text Encodings
  SQLITE_UTF16NATIVE    = 0;
  SQLITE_UTF8           = 1;
  SQLITE_UTF16LE        = 2;
  SQLITE_UTF16BE        = 3;
  SQLITE_UTF16          = 4;    // Use native byte order
  SQLITE_ANY            = 5;    // sqlite3_create_function only
  SQLITE_UTF16_ALIGNED  = 8;    // sqlite3_create_collation only

  // Function flags
  SQLITE_DETERMINISTIC  = $000000800;
  SQLITE_DIRECTONLY     = $000080000;
  SQLITE_SUBTYPE        = $000100000;
  SQLITE_INNOCUOUS      = $000200000;

  // VTab index modes
  SQLITE_VTAB_DS_ROWID_IDX = 1;
  SQLITE_VTAB_DS_KEY_IDX   = 2;
  SQLITE_VTAB_DS_ORD_IDX   = 4;

  // VTab configuration options
  SQLITE_VTAB_CONSTRAINT_SUPPORT = 1;
  SQLITE_VTAB_INNOCUOUS          = 2;
  SQLITE_VTAB_DIRECTONLY         = 3;

  // VTab conflict resolution modes
  SQLITE_ROLLBACK = 1;
  // SQLITE_IGNORE = 2; // Also used by sqlite3_authorizer() callback
  SQLITE_FAIL = 3;
  // SQLITE_ABORT = 4;  // Also an error code
  SQLITE_REPLACE = 5;

type
  Tsqlite3_libversion = function (): PFDAnsiString; cdecl;
  Tsqlite3_libversion_number = function (): Integer; cdecl;
  Tsqlite3_compileoption_used = function (zOptName: PFDAnsiString): Integer; cdecl;
  Tsqlite3_compileoption_get = function (N: Integer): PFDAnsiString; cdecl;
  Tsqlite3_enable_shared_cache = function (onoff: Integer): Integer; cdecl;
  Tsqlite3_release_memory = function (amount: Integer): Integer; cdecl;
  Tsqlite3_soft_heap_limit = procedure (amount: Integer); cdecl;
  Tsqlite3_status = function (op: Integer; var pCurrent: Integer;
    var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;
  Tsqlite3_initialize = function (): Integer; cdecl;
  Tsqlite3_shutdown = function (): Integer; cdecl;
  Tsqlite3_malloc = function (n: Integer): Pointer; cdecl;
  Tsqlite3_memory_used = function (): sqlite3_int64; cdecl;
  Tsqlite3_memory_highwater = function (resetFlag: Integer): sqlite3_int64; cdecl;
  Tsqlite3_free = procedure (APtr: Pointer); cdecl;
  Tsqlite3_config = function (option: Integer): Integer; cdecl varargs;

  Tsqlite3_open {.|16} = function (filename: PByte; var ppDb: psqlite3): Integer; cdecl;
  Tsqlite3_open_v2 = function (filename: PUtf8; var ppDb: psqlite3;
    flags: Integer; zVfs: PFDAnsiString): Integer; cdecl;
  Tsqlite3_activate_see = procedure(see: PFDAnsiString); cdecl;
  Tsqlite3_key = function (db: psqlite3; key: PByte; nKey: Integer): Integer; cdecl;
  Tsqlite3_rekey = function (db: psqlite3; key: PByte; nKey: Integer): Integer; cdecl;
  Tsqlite3_key_v2 = function (db: psqlite3; zDbName: PByte; zKey: PByte; nKey: Integer): Integer; cdecl;
  Tsqlite3_rekey_v2 = function (db: psqlite3; zDbName: PByte; zKey: PByte; nKey: Integer): Integer; cdecl;
  Tsqlite3_close = function (db: psqlite3): Integer; cdecl;
  Tsqlite3_busy_timeout = function (db: psqlite3; ms: Integer): Integer; cdecl;
  Tsqlite3_busy_handler = function (db: psqlite3;
    callback: Tsqlite3_busy_callback; userdata: Pointer): Integer; cdecl;
  Tsqlite3_trace = function (db: psqlite3; xTrace: Tsqlite3_trace_callback;
    userdata: Pointer): Pointer; cdecl;
  Tsqlite3_profile = function (db: psqlite3; xProfile: Tsqlite3_profile_callback;
    userdata: Pointer): Pointer; cdecl;
  Tsqlite3_set_authorizer = function (db: psqlite3; xAuth: Tsqlite3_auth_callback;
    userdata: Pointer): Integer; cdecl;
  Tsqlite3_get_autocommit = function (db: psqlite3): Integer; cdecl;
  Tsqlite3_commit_hook = function (db: psqlite3; callback: Tsqlite3_commit_callback;
    userdata: Pointer): Pointer; cdecl;
  Tsqlite3_rollback_hook = function (db: psqlite3; callback: Tsqlite3_rollback_callback;
    userdata: Pointer): Pointer; cdecl;
  Tsqlite3_table_column_metadata = function (db: psqlite3; zDbName: PUtf8;
    zTableName: PUtf8; zColumnName: PUtf8; var pzDataType: PUtf8;
    var pzCollSeq: PUtf8; var pNotNull: Integer; var pPrimaryKey: Integer;
    var pAutoinc: Integer): Integer; cdecl;
  Tsqlite3_update_hook = function (db: psqlite3; callback: Tsqlite3_update_callback;
    userdata: Pointer): Pointer; cdecl;
  Tsqlite3_limit = function (db: psqlite3; id: Integer; newVal: Integer): Integer; cdecl;
  Tsqlite3_collation_needed {.|16} = function (db: psqlite3; userdata: Pointer;
    callback: Tsqlite3_collation_callback): Integer; cdecl;
  Tsqlite3_create_collation {.|16} = function (db: psqlite3; zName: PByte;
    eTextRep: Integer; userdata: Pointer; callback: Tsqlite3_compare_callback): Integer; cdecl;
  Tsqlite3_progress_handler = procedure (db: psqlite3; nOpers: Integer;
    callback: Tsqlite3_progress_callback; userdata: Pointer); cdecl;

  Tsqlite3_errcode = function (db: psqlite3): Integer; cdecl;
  Tsqlite3_errmsg {.|16} = function (db: psqlite3): PByte; cdecl;
  Tsqlite3_extended_result_codes = function (db: psqlite3; onoff: Integer): Integer; cdecl;
  Tsqlite3_errstr = function (code: Integer): PByte; cdecl;

  Tsqlite3_changes = function (db: psqlite3): Integer; cdecl;
  Tsqlite3_total_changes = function (db: psqlite3): Integer; cdecl;
  Tsqlite3_interrupt = procedure (db: psqlite3); cdecl;
  Tsqlite3_last_insert_rowid = function (db: psqlite3): sqlite3_int64; cdecl;
  Tsqlite3_db_filename = function (db: psqlite3; zDbName: PUtf8): PUtf8; cdecl;
  Tsqlite3_db_readonly = function (db: psqlite3; zDbName: PUtf8): Integer; cdecl;
  Tsqlite3_db_status = function (db: psqlite3; op: Integer; var pCurrent: Integer;
    var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;
  Tsqlite3_exec = function (db: psqlite3; zSql: PByte; callback: Pointer;
    data: Pointer; errmsg: PPByte): Integer; cdecl;

  Tsqlite3_prepare {.|16} {.|_v2} = function (db: psqlite3; zSql: PByte;
    nByte: Integer; var ppStmt: psqlite3_stmt; var pzTail: PByte): Integer; cdecl;
  Tsqlite3_step = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_reset = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_finalize = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_stmt_readonly = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_stmt_busy = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_stmt_status = function (stmt: psqlite3_stmt; op: Integer;
    resetFlg: Integer): Integer; cdecl;

  Tsqlite3_clear_bindings = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_bind_parameter_count = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_bind_parameter_index = function (stmt: psqlite3_stmt; zName: PUtf8): Integer; cdecl;
  Tsqlite3_bind_parameter_name = function (stmt: psqlite3_stmt; index: Integer): PUtf8; cdecl;
  Tsqlite3_bind_blob = function (stmt: psqlite3_stmt; index: Integer; value: Pointer;
    nBytes: Integer; destr: Tsqlite3_destroy_callback): Integer; cdecl;
  Tsqlite3_bind_blob64 = function (stmt: psqlite3_stmt; index: Integer; value: Pointer;
    nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback): Integer; cdecl;
  Tsqlite3_bind_double = function (stmt: psqlite3_stmt; index: Integer;
    value: double): Integer; cdecl;
  Tsqlite3_bind_int = function (stmt: psqlite3_stmt; index: Integer;
    value: Integer): Integer; cdecl;
  Tsqlite3_bind_int64 = function (stmt: psqlite3_stmt; index: Integer;
    value: sqlite3_int64): Integer; cdecl;
  Tsqlite3_bind_null = function (stmt: psqlite3_stmt; index: Integer): Integer; cdecl;
  Tsqlite3_bind_text {.|16} = function (stmt: psqlite3_stmt; index: Integer;
    value: PByte; nBytes: Integer; destr: Tsqlite3_destroy_callback): Integer; cdecl;
  Tsqlite3_bind_text64 = function (stmt: psqlite3_stmt; index: Integer;
    value: PByte; nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback; encoding: Byte): Integer; cdecl;
  Tsqlite3_bind_value = function (stmt: psqlite3_stmt; index: Integer;
    value: psqlite3_value): Integer; cdecl;
  Tsqlite3_bind_zeroblob = function (stmt: psqlite3_stmt; index: Integer;
    nBytes: Integer): Integer; cdecl;

  Tsqlite3_column_count = function (stmt: psqlite3_stmt): Integer; cdecl;
  Tsqlite3_column_type = function (stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl;
  Tsqlite3_column_name {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl;
  Tsqlite3_column_database_name {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl;
  Tsqlite3_column_table_name {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl;
  Tsqlite3_column_origin_name {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl;
  Tsqlite3_column_decltype {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl;
  Tsqlite3_column_blob = function (stmt: psqlite3_stmt; iCol: Integer): Pointer; cdecl;
  Tsqlite3_column_bytes {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl;
  Tsqlite3_column_double = function (stmt: psqlite3_stmt; iCol: Integer): Double; cdecl;
  Tsqlite3_column_int = function (stmt: psqlite3_stmt; iCol: Integer): Integer; cdecl;
  Tsqlite3_column_int64 = function (stmt: psqlite3_stmt; iCol: Integer): sqlite3_int64; cdecl;
  Tsqlite3_column_text {.|16} = function (stmt: psqlite3_stmt; iCol: Integer): PByte; cdecl;
  Tsqlite3_column_value = function (stmt: psqlite3_stmt; iCol: Integer): psqlite3_value; cdecl;

  Tsqlite3_blob_open = function (db: psqlite3; zDb: PUtf8; zTable: PUtf8;
    zColumn: PUtf8; iRow: sqlite3_int64; flags: Integer;
    var ppBlob: psqlite3_blob): Integer; cdecl;
  Tsqlite3_blob_close = function (blob: psqlite3_blob): Integer; cdecl;
  Tsqlite3_blob_bytes = function (blob: psqlite3_blob): Integer; cdecl;
  Tsqlite3_blob_read = function (blob: psqlite3_blob; Z: Pointer; N: Integer;
    iOffset: Integer): Integer; cdecl;
  Tsqlite3_blob_write = function (blob: psqlite3_blob; Z: Pointer; N: Integer;
    iOffset: Integer): Integer; cdecl;

  Tsqlite3_create_function {.|16} = function (db: psqlite3; zFunctionName: PByte;
    nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: Tsqlite3_func_callback;
    xStep: Tsqlite3_step_callback; xFinal: Tsqlite3_final_callback): Integer; cdecl;
  Tsqlite3_user_data = function (context: psqlite3_context): Pointer; cdecl;
  Tsqlite3_aggregate_context = function (context: psqlite3_context;
    nBytes: Integer): Pointer; cdecl;
  Tsqlite3_context_db_handle = function (context: psqlite3_context): psqlite3; cdecl;
  Tsqlite3_get_auxdata = function (context: psqlite3_context; N: Integer): Pointer; cdecl;
  Tsqlite3_set_auxdata = procedure (context: psqlite3_context; N: Integer; data: Pointer;
    destr: Tsqlite3_destroy_callback); cdecl;

  Tsqlite3_auto_extension = function (xEntryPoint: Pointer): Integer; cdecl;
  Tsqlite3_reset_auto_extension = procedure (); cdecl;
  Tsqlite3_enable_load_extension = function (db: psqlite3; onoff: Integer): Integer; cdecl;
  Tsqlite3_load_extension = function (db: psqlite3; zFile, zProc: PByte;
    var pzErrMsg: PByte): Integer; cdecl;

  Tsqlite3_create_module = function (db: psqlite3; zName: PUtf8; module: psqlite3_module;
    userdata: Pointer): Integer; cdecl;
  Tsqlite3_create_module_v2 = function (db: psqlite3; zName: PUtf8; module: psqlite3_module;
    userdata: Pointer; destr: Tsqlite3_destroy_callback): Integer; cdecl;
  Tsqlite3_declare_vtab = function (db: psqlite3; zCreateTable: PUtf8): Integer; cdecl;
  Tsqlite3_overload_function = function (db: psqlite3; zFuncName: PUtf8;
    nArg: Integer): Integer; cdecl;

  Tsqlite3_value_blob = function (value: psqlite3_value): Pointer; cdecl;
  Tsqlite3_value_bytes {.|16} = function (value: psqlite3_value): Integer; cdecl;
  Tsqlite3_value_double = function (value: psqlite3_value): Double; cdecl;
  Tsqlite3_value_int = function (value: psqlite3_value): Integer; cdecl;
  Tsqlite3_value_int64 = function (value: psqlite3_value): sqlite3_int64; cdecl;
  Tsqlite3_value_text {.|16} = function (value: psqlite3_value): PByte; cdecl;
  Tsqlite3_value_type = function (value: psqlite3_value): Integer; cdecl;
  Tsqlite3_value_numeric_type = function (value: psqlite3_value): Integer; cdecl;

  Tsqlite3_result_blob = procedure (context: psqlite3_context; value: Pointer;
    nBytes: Integer; destr: Tsqlite3_destroy_callback); cdecl;
  Tsqlite3_result_blob64 = procedure (context: psqlite3_context; value: Pointer;
    nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback); cdecl;
  Tsqlite3_result_double = procedure (context: psqlite3_context; value: Double); cdecl;
  Tsqlite3_result_int = procedure (context: psqlite3_context; value: Integer); cdecl;
  Tsqlite3_result_int64 = procedure (context: psqlite3_context; value: sqlite3_int64); cdecl;
  Tsqlite3_result_null = procedure (context: psqlite3_context); cdecl;
  Tsqlite3_result_text {.|16} = procedure (context: psqlite3_context; value: PByte;
    nBytes: Integer; destr: Tsqlite3_destroy_callback); cdecl;
  Tsqlite3_result_text64 = procedure (context: psqlite3_context; value: PByte;
    nBytes: sqlite3_uint64; destr: Tsqlite3_destroy_callback; encoding: Byte); cdecl;
  Tsqlite3_result_value = procedure (context: psqlite3_context; value: psqlite3_value); cdecl;
  Tsqlite3_result_zeroblob = procedure (context: psqlite3_context; n: Integer); cdecl;
  Tsqlite3_result_error {.|16} = procedure (context: psqlite3_context; msg: PByte;
    nBytes: Integer); cdecl;
  Tsqlite3_result_error_toobig = procedure (context: psqlite3_context); cdecl;
  Tsqlite3_result_error_nomem = procedure (context: psqlite3_context); cdecl;
  Tsqlite3_result_error_code = procedure (context: psqlite3_context; code: Integer); cdecl;

  Tsqlite3_vfs_find = function(zVfsName: PFDAnsiString): psqlite3_vfs; cdecl;
  Tsqlite3_vfs_register = function(pVfs: psqlite3_vfs; makeDflt: Integer): Integer; cdecl;
  Tsqlite3_vfs_unregister = function(pVfs: psqlite3_vfs): Integer; cdecl;

  Tsqlite3_backup_init = function (pDest: psqlite3; zDestName: PByte; pSource: psqlite3;
    zSourceName: PByte): psqlite3_backup; cdecl;
  Tsqlite3_backup_step = function (p: psqlite3_backup; nPage: Integer): Integer; cdecl;
  Tsqlite3_backup_finish = function (p: psqlite3_backup): Integer; cdecl;
  Tsqlite3_backup_remaining = function (p: psqlite3_backup): Integer; cdecl;
  Tsqlite3_backup_pagecount = function (p: psqlite3_backup): Integer; cdecl;

  Tsqlite3_wal_hook = function (db: psqlite3; callback: Tsqlite3_wal_callback; userdata: Pointer): Pointer; cdecl;
  Tsqlite3_wal_autocheckpoint = function (db: psqlite3; N: Integer): Integer; cdecl;
  Tsqlite3_wal_checkpoint = function (db: psqlite3; zDb: PFDAnsiString): Integer; cdecl;

  Tsqlite3_rtree_geometry_callback = function (db: psqlite3; zGeom: PByte;
    xGeom: Tsqlite3_rtree_xGeom_callback; pContext: Pointer): Integer; cdecl;
  Tsqlite3_rtree_query_callback = function (db: psqlite3; zQueryFunc: PByte;
    xQueryFunc: Tsqlite3_rtree_xQuery_callback; pContext: Pointer;
    xDestructor: Tsqlite3_rtree_xDelUser_callback): Integer; cdecl;

  Tsqlite3_vtab_config = function(db: psqlite3; op: Integer): Integer; cdecl varargs;
  Tsqlite3_vtab_on_conflict = function(db: psqlite3): Integer; cdecl;
  Tsqlite3CodecGetKey = procedure(db: psqlite3; nDb: Integer; zKey: PPointer; nKey: PInteger); cdecl;
  Tsqlite3CodecAttach = function(db: psqlite3; nDb: Integer; zKey: Pointer; nKey: Integer): Integer; cdecl;

  Tad_sqlite3GetCacheSize = function (db: psqlite3): Integer; cdecl;
  Tad_sqlite3GetEncoding = function (db: psqlite3): Integer; cdecl;
  Tad_sqlite3GetEncryptionMode = function (db: psqlite3; var name: PFDAnsiString; var len: Integer): Integer;
  Tad_sqlite3GetEncryptionError = function (db: psqlite3; var error: PFDAnsiString; var len: Integer; var error_code: Integer): Integer;
  Tad_sqlite3Error = procedure (db: psqlite3; err_code: Integer; zMessage: PByte); cdecl;

                 
                                
                       
                   
                     
                     
                
                     
                   

                   
  

                                    
                                           

                       
                
                 
                 
                
                    
                                          
                                                        
                                     
                                
                                               
                                           
                          
                                                     
                                                      
                                                      
                                                    
                                                 
                                 
                                      
                                      
                            
                                   
                                 
                                     
                                     
                                        
 

implementation

{-------------------------------------------------------------------------------}
initialization
  SQLITE_STATIC := nil;
  SQLITE_TRANSIENT := Tsqlite3_destroy_callback(Pointer({$IFDEF FireDAC_32} $FFFFFFFF {$ENDIF}
                                                        {$IFDEF FireDAC_64} $FFFFFFFFFFFFFFFF {$ENDIF}));

end.
