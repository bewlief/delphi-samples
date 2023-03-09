@echo off
REM ----------------------------------------------------------------
REM  Batch file to compile sqlite3.c and produce sqlite3_see_x86.obj
REM  and sqlite3_see_x64.obj. Requires to adjust BDS and SQLITE_SRC.
REM ----------------------------------------------------------------
setlocal

SET BDS=%BDS%
SET SQLITE_SRC=sqlite3-see.c

if "%BDS%" == "" (
  echo Set BDS variable to the path to RAD Studio base folder
  goto :EOF
)
if "%SQLITE_SRC%" == "" (
  echo Set SQLITE_SRC variable to the path to sqlite3.c
  goto :EOF
)

SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_COLUMN_METADATA=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_STAT4=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_FTS3=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_FTS3_PARENTHESIS=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_FTS4=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_RTREE=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_MEMSTATUS=0
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_RECURSIVE_TRIGGERS=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_PAGE_SIZE=4096
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_CACHE_SIZE=8000
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_TEMP_CACHE_SIZE=2000
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_FILE_FORMAT=4
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_DEFAULT_JOURNAL_SIZE_LIMIT=1048576
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_OMIT_AUTOINIT=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_HAS_CODEC=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_TEMP_STORE=2
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_THREADSAFE=2
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_USE_URI=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_EXPLAIN_COMMENTS=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_WITHOUT_MSIZE=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_DBSTAT_VTAB=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_RBU=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_FTS5=1
SET SQLITE_DEF=%SQLITE_DEF% -DSQLITE_ENABLE_JSON1=1

"%BDS%\bin\bcc32.exe" -6 -O2 -c -d -u- -pc -k- -w-par -w-pia -w-rvl^
 "-I%BDS%\include\windows\crtl"^
 "-I%BDS%\include\windows\sdk"^
 %SQLITE_DEF% -D__MT__ -D_MT^
 -osqlite3_see_x86.obj "%SQLITE_SRC%"

"%BDS%\bin\bcc64.exe" -O2 -c^
 "-isystem %BDS%\include\windows\crtl"^
 "-isystem %BDS%\include\windows\sdk"^
 %SQLITE_DEF% -D__MT__ -D_MT -D_WIN64^
 -osqlite3_see_x64.obj "%SQLITE_SRC%"
