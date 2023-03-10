{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{*******************************************************}
{                 BDE Interface Unit                    }
{*******************************************************}

unit BDE;

{$Z+}

interface

uses Winapi.Windows, Data.DBCommonTypes;

{ Translated from IDAPI.H,  Revision 4.203 }

const
  DBIINTFVER         = 500;             { Version of the interface }
  DBIENGVERSION      = 500;             { Version of the engine }

{-----------------------------------------------------------------------}
{     DBI types                                                         }
{-----------------------------------------------------------------------}


{ sdk debug layer defines }

const
  DEBUGON            = $1;
  OUTPUTTOFILE       = $2;
  FLUSHEVERYOP       = $8;
  APPENDTOLOG        = $10;

{ Constants }

  DBIMAXNAMELEN      = 31;              { Name limit (table, field etc) }
  DBIMAXSPNAMELEN    = 64;              { Max stored procedure name length }
  DBIMAXFUNCNAMELEN  = 255;             { Max function name len }
  DBIMAXFLDSINKEY    = 16;              { Max fields in a key }
  DBIMAXKEYEXPLEN    = 220;             { Max Key expression length }
  DBIMAXEXTLEN       = 3;               { Max file extension len, not incl. dot (excluding zero termination) }
  DBIMAXTBLNAMELEN   = 260;             { Max table name length }
  DBIMAXDRIVELEN     = 127;             { Max drive length }
  DBIMAXPATHLEN      = 260;             { Max path+file name len (excluding zero termination) }
  DBIMAXMSGLEN       = 127;             { Max message len }
  DBIMAXVCHKLEN      = 255;             { Max val check len }
  DBIMAXPICTLEN      = 175;             { Max picture len }
  DBIMAXFLDSINSEC    = 256;             { Max fields in security spec }

  DBIMAXSCFIELDS     = 40;              { max nbr of fields in a config section }
  DBIMAXSCFLDLEN     = 128;             { max field length }
  DBIMAXSCRECSIZE    = DBIMAXSCFIELDS*DBIMAXSCFLDLEN; { max rec size }

  DBIMAXUSERNAMELEN  = 14;              { Max user name (general) }
  DBIMAXXBUSERNAMELEN = 16;             { Max user name length (xBASE) }
  DBIMAXBOOKMARKLEN  = 4104;            { Max Bookmark length }

  DBIMAXTRACELEN     = 8192;            { Max trace msg len }

  DBIMAXTYPEDESC     = 127;             { Max Type Description size }
  DBIMAXDRSQLSTR     = 8192;            { Max Size of SQL Constraint }

  DBIMAXOLEDBNAMELEN = 64;              { Max OLE DB name length }

{============================================================================}
{                             G e n e r a l                                  }
{============================================================================}

type
  DBIDATE            = Longint;
  TIME               = Longint;
  TIMESTAMP          = Double;
  DBIResult          = Word;         { Function result }
  TypedEnum          = Integer;
  PLocale            = Pointer;

{ Handle Types }

  _hDBIObj           = record end;      { Dummy structure to create "typed" handles }
  hDBIObj            = ^_hDBIObj;       { Generic object handle }
  hDBIDb             = ^_hDBIObj;       { Database handle }
  hDBIQry            = ^_hDBIObj;       { Query handle }
  hDBIStmt           = ^_hDBIObj;       { Statement handle ("new query") }
  hDBICur            = ^_hDBIObj;       { Cursor handle }
  hDBISes            = ^_hDBIObj;       { Session handle }
  hDBIXlt            = ^_hDBIObj;       { Translation handle }
  hDBICfg            = ^_hDBIObj;       { Configuration handle }
  hDBIXact           = ^_hDBIObj;       { Transaction handle }
  hDBIFilter         = ^_hDBIObj;       { Filter handle }
  hDBIDR             = ^_hDBIObj;       { Handle to a Client Data Repository (CDR) Object }


{ Handle Pointers }

  phDBIObj           = ^hDBIObj;        { Pointer to Generic object handle }
  phDBIDb            = ^hDBIDb;         { Pointer to Database handle }
  phDBIQry           = ^hDBIQry;        { Pointer to Query handle }
  phDBIStmt          = ^hDBIStmt;       { Pointer to Statement handle }
  phDBICur           = ^hDBICur;        { Pointer to Cursor handle }
  phDBISes           = ^hDBISes;        { Pointer to Session handle }
  phDBIXlt           = ^hDBIXlt;        { Pointer to Translation handle }
  phDBICfg           = ^hDBICfg;        { Pointer to Configuration handle }
  phDBIXact          = ^hDBIXact;       { Pointer to Transaction handle }
  phDBIFilter        = ^hDBIFilter;     { Pointer to Filter handle }
  phDBIDR            = ^hDBIDR;         { Pointer to CDR Handle }

{ typedefs for buffers of various common sizes: }

  DBIPATH            = packed array [0..DBIMAXPATHLEN] of AnsiChar; { holds a DOS path }
  DBINAME            = packed array [0..DBIMAXNAMELEN] of AnsiChar; { holds a name }
  DBIEXT             = packed array [0..DBIMAXEXTLEN] of AnsiChar; { holds an extension EXT }
  DBIDOTEXT          = packed array [0..DBIMAXEXTLEN+1] of AnsiChar; { holds an extension EXT including '.' }
  DBIDRIVE           = packed array [0..DBIMAXDRIVELEN] of AnsiChar; { holds a drive name }
  DBITBLNAME         = packed array [0..DBIMAXTBLNAMELEN] of AnsiChar; { holds a table name }
  DBISPNAME          = packed array [0..DBIMAXSPNAMELEN] of AnsiChar; { holds a stored procedure name }
  DBIUSERNAME        = packed array [0..DBIMAXUSERNAMELEN] of AnsiChar; { holds a user name }
  DBIKEY             = packed array [0..DBIMAXFLDSINKEY-1] of Word; { holds list of fields in a key }
  DBIKEYEXP          = packed array [0..DBIMAXKEYEXPLEN] of AnsiChar; { holds a key expression }
  DBIVCHK            = packed array [0..DBIMAXVCHKLEN] of Byte; { holds a validity check }
  DBIPICT            = packed array [0..DBIMAXPICTLEN] of AnsiChar; { holds a picture (Pdox) }
  DBIMSG             = packed array [0..DBIMAXMSGLEN] of AnsiChar; { holds an error message }
  DBIDRTYPEDESC      = packed array [0..DBIMAXTYPEDESC] of AnsiChar; { holds a Type Description string }
  DBISQLSTR          = packed array [0..DBIMAXDRSQLSTR] of AnsiChar; { a SQL string }


{============================================================================}
{                             Environmental                                  }
{============================================================================}

type
  pDBIEnv = ^DBIEnv;
  DBIEnv = packed record                { Struct for defining the environ. }
    szWorkDir       : DBIPATH;          { Working directory }
    szIniFile       : DBIPATH;          { Configuration file }
    bForceLocalInit : WordBool;         { Force local initialization }
    szLang          : DBINAME;          { System language driver }
    szClientName    : DBINAME;          { Client name (documentary) }
  end;

{============================================================================}
{                   System Info                                              }
{============================================================================}

type
  pSYSVersion = ^SYSVersion;
  SYSVersion = packed record            { System Version Info }
    iVersion        : Word;             { Engine version }
    iIntfLevel      : Word;             { Client Interface level }
    dateVer         : DBIDATE;          { Version date (Compile/Release) }
    timeVer         : Time;             { Version time (Compile/Release) }
    szVersion       : array [0..19] of AnsiChar; { Version name (xxx.xxx.xxx.xxx) }
  end;

  pSYSConfig = ^SYSConfig;
  SYSConfig = packed record             { System configuration (basic) }
    bLocalShare     : WordBool;         { If Local files will be shared }
    iNetProtocol    : Word;             { Net Protocol (35, 40 etc.) }
    bNetShare       : WordBool;         { If Net files will be shared }
    szNetType       : DBINAME;          { Network type }
    szUserName      : DBIUSERNAME;      { Network user name }
    szIniFile       : DBIPATH;          { Configuration file }
    szLangDriver    : DBINAME;          { System language driver }
  end;

  pSYSInfo = ^SYSInfo;
  SYSInfo = packed record               { System Status/Info }
    iBufferSpace    : Word;             { in K }
    iHeapSpace      : Word;             { in K }
    iDrivers        : Word;             { Active/Loaded drivers }
    iClients        : Word;             { Active clients }
    iSessions       : Word;             { Number of sessions (For all clients) }
    iDatabases      : Word;             { Open databases }
    iCursors        : Word;             { Number of cursors }
  end;

  pCLIENTInfo = ^CLIENTInfo;
  CLIENTInfo = packed record
    szName          : DBINAME;          { Documentary name }
    iSessions       : Word;             { Number of sessions }
    szWorkDir       : DBIPATH;          { Working directory }
    szLang          : DBINAME;          { System language driver (Client supplied) }
  end;

  pSESInfo = ^SESInfo;
  SESInfo = packed record
    iSession        : Word;             { Session id (1..n) }
    szName          : DBINAME;          { Documentary name }
    iDatabases      : Word;             { Open databases }
    iCursors        : Word;             { Open cursors }
    iLockWait       : SmallInt;         { Lock wait time (in seconds) }
    szNetDir        : DBIPATH;          { Net directory location }
    szPrivDir       : DBIPATH;          { Current Private directory }
  end;

{============================================================================}
{                             Table / Field Types                            }
{============================================================================}

{ Driver Types }

const
  szPARADOX          = 'PARADOX';
  szDBASE            = 'DBASE';
  szFOXPRO           = 'FOXPRO';
  szASCII            = 'ASCIIDRV';
  szOLEDB            = 'OLEDBDRV';
  szMSACCESS         = 'MSACCESS'; 


{ Field Types (Logical) }

  fldUNKNOWN         = 0;
  fldZSTRING         = 1;               { Null terminated string }
  fldDATE            = 2;               { Date     (32 bit) }
  fldBLOB            = 3;               { Blob }
  fldBOOL            = 4;               { Boolean  (16 bit) }
  fldINT16           = 5;               { 16 bit signed number }
  fldINT32           = 6;               { 32 bit signed number }
  fldFLOAT           = 7;               { 64 bit floating point }
  fldBCD             = 8;               { BCD }
  fldBYTES           = 9;               { Fixed number of bytes }
  fldTIME            = 10;              { Time        (32 bit) }
  fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  fldUINT16          = 12;              { Unsigned 16 bit integer }
  fldUINT32          = 13;              { Unsigned 32 bit integer }
  fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  fldVARBYTES        = 15;              { Length prefixed var bytes }
  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  fldCURSOR          = 17;              { For Oracle Cursor type }
  fldINT64           = 18;              { 64 bit signed number }
  fldUINT64          = 19;              { Unsigned 64 bit integer }
  fldADT             = 20;              { Abstract datatype (structure) }
  fldARRAY           = 21;              { Array field type }
  fldREF             = 22;              { Reference to ADT }
  fldTABLE           = 23;              { Nested table (reference) }

  fldDATETIME        = 24;              { DateTime structure field }

  MAXLOGFLDTYPES     = 25;              { Number of logical fieldtypes }

{ Sub Types (Logical) }

{ fldFLOAT subtype }

  fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }

  fldstMEMO          = 22;              { Text Memo }
  fldstBINARY        = 23;              { Binary data }
  fldstFMTMEMO       = 24;              { Formatted Text }
  fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  fldstGRAPHIC       = 26;              { Graphics object }
  fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  fldstTYPEDBINARY   = 28;              { Typed Binary data }
  fldstACCOLEOBJ     = 30;              { Access OLE object }
  fldstHMEMO         = 33;              { CLOB }
  fldstHBINARY       = 34;              { BLOB }
  fldstBFILE         = 36;              { BFILE }

{ fldZSTRING subtype }

  fldstPASSWORD      = 1;               { Password }
  fldstFIXED         = 31;              { AnsiChar type }
  fldstUNICODE       = 32;              { Unicode }

{ fldINT32 subtype }

  fldstAUTOINC       = 29;

{ fldADT subtype }

  fldstADTNestedTable = 35;             { ADT for nested table (has no name) }

{ fldDATE subtype }
  fldstADTDATE       = 37;              { DATE (OCIDate ) with in an ADT }

{ fldZSTRING subtype }
  fldstGUID          = 38;              { Subtype of string. Ado-guid field }

{ fldBYTES subtype }
  fldstINT64         = 39;              { Interbase 6 INT64 }

{ Paradox types (Physical) }

  fldPDXCHAR         = $101;            { Alpha    (string) }
  fldPDXNUM          = $102;            { Numeric }
  fldPDXMONEY        = $103;            { Money }
  fldPDXDATE         = $104;            { Date }
  fldPDXSHORT        = $105;            { Short }
  fldPDXMEMO         = $106;            { Text Memo       (blob) }
  fldPDXBINARYBLOB   = $107;            { Binary data     (blob) }
  fldPDXFMTMEMO      = $108;            { Formatted text  (blob) }
  fldPDXOLEBLOB      = $109;            { OLE object      (blob) }
  fldPDXGRAPHIC      = $10A;            { Graphics object (blob) }
  fldPDXBLOB         = fldPDXMEMO;
  fldPDXLONG         = $10B;            { Long }
  fldPDXTIME         = $10C;            { Time }
  fldPDXDATETIME     = $10D;            { Time Stamp }
  fldPDXBOOL         = $10E;            { Logical }
  fldPDXAUTOINC      = $10F;            { Auto increment (long) }
  fldPDXBYTES        = $110;            { Fixed number of bytes }
  fldPDXBCD          = $111;            { BCD (32 digits) }
  fldPDXUNICHAR      = $112;            { not supported yet }

{ xBASE types (Physical) }

  fldDBCHAR          = $201;            { AnsiChar string }
  fldDBNUM           = $202;            { Number }
  fldDBMEMO          = $203;            { Memo          (blob) }
  fldDBBOOL          = $204;            { Logical }
  fldDBDATE          = $205;            { Date }
  fldDBFLOAT         = $206;            { Float }
  fldDBLOCK          = $207;            { Logical type is LOCKINFO }
  fldDBOLEBLOB       = $208;            { OLE object    (blob) }
  fldDBBINARY        = $209;            { Binary data   (blob) }
  fldDBBYTES         = $20A;            { Only for TEMPORARY tables }
  fldDBLONG          = $20B;            { Long (Integer) }
  fldDBDATETIME      = $20C;            { DateTime }
  fldDBDOUBLE        = $20D;            { Double }
  fldDBAUTOINC       = $20E;            { Auto increment (long) }

{ xBASE key types (Cannot be used as field types) }

  fldDBKEYNUM        = $210;
  fldDBKEYBCD        = $211;


{ Ascii types (Physical) }

  fldASCCHAR         = $301;            { AnsiChar string }
  fldASCNUM          = $302;            { Number }
  fldASCBOOL         = $303;            { Logical }
  fldASCDATE         = $304;            { Date }
  fldASCFLOAT        = $305;            { Float }
  fldASCLOCK         = $306;            { Not used }
  fldASCTIMESTAMP    = $307;            { TimeStamp }
  fldASCTIME         = $308;            { Time }
  fldASCLONG         = $309;            { Long }
  fldASCMONEY        = $30A;            { Money }

{ MS Access types (Physical) }

  fldACCCHAR         = $701;            { Fixed length Character }
  fldACCVARCHAR      = $702;            { Variable length Character }
  fldACCDATETIME     = $703;            { Date/Time }
  fldACCBIT          = $704;            { Boolean }
  fldACCMONEY        = $705;            { Currency }
  fldACCSHORT        = $706;            { Short }
  fldACCLONG         = $707;            { Long }
  fldACCFLOAT        = $708;            { Float }
  fldACCLONGTEXT     = $709;            { Memo }
  fldACCLONGBINARY   = $70A;            { OLE Object }
  fldACCAUTOINC      = $70B;            { Auto Number }
  fldACCBYTE         = $70C;            { Byte }
  fldACCDOUBLE       = $70D;            { Double }
  fldACCBINARY       = $70E;            { Binary }
  fldACCGUID         = $70F;            { Replication ID }


{============================================================================}
{                    Field descriptor                                        }
{============================================================================}

type
  pFLDDesc = ^FLDDesc;
  FLDDesc = packed record               { Field Descriptor }
    iFldNum         : Word;             { Field number (1..n) }
    szName          : DBINAME;          { Field name }
    iFldType        : Word;             { Field type }
    iSubType        : Word;             { Field subtype (if applicable) }
    iUnits1         : SmallInt;         { Number of Chars, digits etc }
    iUnits2         : SmallInt;         { Decimal places etc. }
    iOffset         : Word;             { Offset in the record (computed) }
    iLen            : Word;             { Length in bytes (computed) }
    iNullOffset     : Word;             { For Null bits (computed) }
    efldvVchk       : FLDVchk;          { Field Has vcheck (computed) }
    efldrRights     : FLDRights;        { Field Rights (computed) }
    bCalcField      : WordBool;         { Is Calculated field (computed) }
    iUnUsed         : packed array [0..1] of Word;
  end;
  BDEFLDDesc = FLDDesc; 

{ ============================================================================ }
{                    Blob parameter descriptor                                 }
{ ============================================================================ }

type
  pBLOBParamDesc = ^BLOBParamDesc;
  BLOBParamDesc = packed record
    pBlobBuffer     : Pointer;          { Blob buffer (client) }
    ulBlobLen       : Integer;          { Length of the blob }
    iUnUsed         : packed array[0..3] of Word;
  end;

{============================================================================}
{                    Index descriptor                                        }
{============================================================================}

const
  mdxDISTINCT      = $0020;         { OR with bUnique for dBASE distinct tags }

type
  pIDXDesc = ^IDXDesc;
  IDXDesc = packed record               { Index description }
    szName          : DBITBLNAME;       { Index name }
    iIndexId        : Word;             { Index number }
    szTagName       : DBINAME;          { Tag name (for dBASE) }
    szFormat        : DBINAME;          { Optional format (BTREE, HASH etc) }
    bPrimary        : WordBool;         { True, if primary index }
    bUnique         : WordBool;         { True, if unique keys (TRI-STATE for dBASE) }
    bDescending     : WordBool;         { True, for descending index }
    bMaintained     : WordBool;         { True, if maintained index }
    bSubset         : WordBool;         { True, if subset index }
    bExpIdx         : WordBool;         { True, if expression index }
    iCost           : Word;             { Not used }
    iFldsInKey      : Word;             { Fields in the key (1 for Exp) }
    iKeyLen         : Word;             { Phy Key length in bytes (Key only) }
    bOutofDate      : WordBool;         { True, if index out of date }
    iKeyExpType     : Word;             { Key type of Expression }
    aiKeyFld        : DBIKEY;           { Array of field numbers in key }
    szKeyExp        : DBIKEYEXP;        { Key expression }
    szKeyCond       : DBIKEYEXP;        { Subset condition }
    bCaseInsensitive : WordBool;        { True, if case insensitive index }
    iBlockSize      : Word;             { Block size in bytes }
    iRestrNum       : Word;             { Restructure number }
    abDescending    : packed array [0..DBIMAXFLDSINKEY-1] of WordBool; { TRUE }
    iUnUsed         : packed array [0..15] of Word;
  end;

{============================================================================}
{             Validity check, Referential integrity descriptors              }
{============================================================================}



{ Subtypes for Lookup }

  LKUPType = (                          { Paradox Lookup type }
    lkupNONE,                           { Has no lookup }
    lkupPRIVATE,                        { Just Current Field + Private }
    lkupALLCORRESP,                     { All Corresponding + No Help }
    lkupHELP,                           { Just Current Fld + Help and Fill }
    lkupALLCORRESPHELP                  { All Corresponging + Help }
  );

const
  TODAYVAL           = 2;               { for Min/Max/Def val flags }
  NOWVAL             = 3;               { for Min/Max/Def val flags }

   { In VCHKDesc below, if any of bHasMinVal/bHasMaxVal/bHasDefVal }
   { = TODAYVAL, then TODAY is assumed , = NOWVAL, then current time/today is assumed }

type
  pVCHKDesc = ^VCHKDesc;
  VCHKDesc = packed record              { Val Check structure }
    iFldNum         : Word;             { Field number }
    bRequired       : WordBool;         { If True, value is required }
    bHasMinVal      : WordBool;         { If True, has min value }
    bHasMaxVal      : WordBool;         { If True, has max value }
    bHasDefVal      : WordBool;         { If True, has default value }
    aMinVal         : DBIVCHK;          { Min Value }
    aMaxVal         : DBIVCHK;          { Max Value }
    aDefVal         : DBIVCHK;          { Default value }
    szPict          : DBIPICT;          { Picture string }
    elkupType       : LKUPType;         { Lookup/Fill type }
    szLkupTblName   : DBIPATH;          { Lookup Table name }
  end;

  RINTType = (                          { Ref integrity type }
    rintMASTER,                         { This table is Master }
    rintDEPENDENT                       { This table is Dependent }
  );

  RINTQual = (                          { Ref integrity action/qualifier }
    rintRESTRICT,                       { Prohibit operation }
    rintCASCADE                         { Cascade operation }
  );

  pRINTDesc = ^RINTDesc;
  RINTDesc = packed record              { Ref Integrity Desc }
    iRintNum        : Word;             { Ref integrity number }
    szRintName      : DBINAME;          { A name to tag this integegrity constraint }
    eType           : RINTType;         { Whether master/dependent }
    szTblName       : DBIPATH;          { Other table name }
    eModOp          : RINTQual;         { Modify qualifier }
    eDelOp          : RINTQual;         { Delete qualifier }
    iFldCount       : Word;             { Fields in foreign key }
    aiThisTabFld    : DBIKEY;           { Fields in this table }
    aiOthTabFld     : DBIKEY;           { Fields in other table }
  end;


{============================================================================}
{                    Security descriptor                                     }
{============================================================================}
{ Family rights }

const
  NOFAMRIGHTS        = $00;             { No Family rights }
  FORMRIGHTS         = $01;             { Can change form }
  RPTRIGHTS          = $02;             { Can change reports }
  VALRIGHTS          = $04;             { Can change valchecks }
  SETRIGHTS          = $08;             { Can change settings }
  ALLFAMRIGHTS       = (FORMRIGHTS or RPTRIGHTS or VALRIGHTS or SETRIGHTS);
                                             { All family rights }

type
  PRVType = TypedEnum;
const
    prvNONE      = 0;                   { No privilege }
    prvREADONLY  = 1;                   { Read only Table or Field }
    prvMODIFY    = 3;                   { Read and Modify fields (non-key) }
    prvINSERT    = 7;                   { Insert + All of above }
    prvINSDEL    = $0F;                 { Delete + All of above }
    prvFULL      = $1F;                 { Full Writes }
    prvUNKNOWN   = $FF;                 { Unknown }

type
  pSECDesc = ^SECDesc;
  SECDesc = packed record               { Security description }
    iSecNum         : Word;             { Nbr to identify desc }
    eprvTable       : PrvType;          { Table privileges }
    iFamRights      : Word;             { Family rights }
    szPassword      : DBINAME;          { Null terminated string }
    aprvFld         : packed array [0..DBIMAXFLDSINSEC-1] of PrvType;
                     { Field level privileges (prvNONE/prvREADONLY/prvFULL) }
  end;

{============================================================================}
{                            Miscellaneous                                   }
{============================================================================}

{ Index Id used to open table without a default index (i.e. no order) }

const
  NODEFAULTINDEX     = $FFFF;


{============================================================================}
{                    Object types                                            }
{============================================================================}

type
  DBIOBJType = (
    objFILLER,                          { Filler to make next start at 1 }
    objSYSTEM,                          { System object }
    objSESSION,                         { Session object }
    objDRIVER,                          { Driver object }
    objDATABASE,                        { Database object }
    objCURSOR,                          { Cursor object }
    objSTATEMENT,                       { Statement object }
    objCLIENT,                          { Client object }
    objDBSEC,                           { DbSystem object (dBASE only) }
    objREPOSITORY                       { Data Repository object }
  );

  pObjAttrDesc = ^ObjAttrDesc;
  ObjAttrDesc = packed record
    iFldNum    : Word;                  { Field id }
    pszAttributeName : PAnsiChar;           { Object attribute name }
  end;
  BDEObjAttrDesc = ObjAttrDesc;

  pObjTypeDesc = ^ObjTypeDesc;
  ObjTypeDesc = packed record
    iFldNum    : Word;                  { Field id }
    szTypeName : DBINAME;               { Object type name }
  end;
  BDEObjTypeDesc = ObjTypeDesc;

  pObjParentDesc = ^ObjParentDesc;
  ObjParentDesc = packed record
    iFldNum    : Word;                  { Field id }
    iParentFldNum : Word;               { Parent Field id }
  end;
  
{============================================================================}
{                    Cursor properties                                       }
{============================================================================}

type
  DBIShareMode = (                      { Database/Table Share type }
    dbiOPENSHARED,                      { Open shared  (Default) }
    dbiOPENEXCL                         { Open exclusive }
  );

  DBIOpenMode = (                       { Database/Table Access type }
    dbiREADWRITE,                       { Read + Write   (Default) }
    dbiREADONLY                         { Read only }
  );

  DBILockType = (                       { Lock types (Table level) }
    dbiNOLOCK,                          { No lock   (Default) }
    dbiWRITELOCK,                       { Write lock }
    dbiREADLOCK                         { Read lock }
  );

  XLTMode = (                           { Field translate mode }
    xltNONE,                            { No translation  (Physical Types) }
    xltRECORD,                          { Record level translation (not supported) }
    xltFIELD                            { Field level translation (Logical types) }
  );

  UPDLockMode = (                       { Update lock mode (SQL only) }
    updWHEREALL,                        { All fields in WHERE clause }
    updWHEREKEYCHG,                     { Keyed and changed fields in WHERE clause }
    updWHEREKEY                         { Keyed fields in WHERE clause }
  );

  TEXACTRECCNTExpensive = (             { Whether DbiGetExactRecordCount is expensive }
    exactRecCntINEXPENSIVE,             { inexpensive }
    exactRecCntEXPENSIVE,               { expensive }
    exactRecCntUNKNOWN                  { record count is unknown }
  );

  pServerColDesc = ^ServerColDesc;
  ServerColDesc = packed record         { Auto increment and Defaults property }
   iFldNum     : Word;                  { Field id }
   bServerCol  : WordBool;              { Auto Increment and Default }
  end;



{ Table levels }

const
  TBLLEVEL3          = 3;               { Paradox level 3 and dBASE level 3+ }
  TBLLEVEL4          = 4;               { Paradox level 4 and dBASE level 4 }
  TBLLEVEL5          = 5;               { Paradox level 5 and dBASE/Win }
  TBLLEVEL7          = 7;               { Paradox level 7 , Win32, dBASE/Win 7 }
  FOXLEVEL25         = 25;              { Fox Table (Ver 2.5) }

type
  pCURProps = ^CURProps;
  CURProps = packed record              { Virtual Table properties }
    szName          : DBITBLNAME;       { table name (no extension, if it can be derived) }
    iFNameSize      : Word;             { Full file name size }
    szTableType     : DBINAME;          { Driver type }
    iFields         : Word;             { No of fields in Table }
    iRecSize        : Word;             { Record size (logical record) }
    iRecBufSize     : Word;             { Record size (physical record) }
    iKeySize        : Word;             { Key size }
    iIndexes        : Word;             { Number of indexes }
    iValChecks      : Word;             { Number of val checks }
    iRefIntChecks   : Word;             { Number of Ref Integrity constraints }
    iBookMarkSize   : Word;             { Bookmark size }
    bBookMarkStable : WordBool;         { Stable book marks }
    eOpenMode       : DBIOpenMode;      { ReadOnly / RW }
    eShareMode      : DBIShareMode;     { Excl / Share }
    bIndexed        : WordBool;         { Index is in use }
    iSeqNums        : SmallInt;         { 1: Has Seqnums; 0: Has Record# }
    bSoftDeletes    : WordBool;         { Supports soft deletes }
    bDeletedOn      : WordBool;         { If above, deleted recs seen }
    iRefRange       : Word;             { Not used }
    exltMode        : XLTMode;          { Translate Mode }
    iRestrVersion   : Word;             { Restructure version number }
    bUniDirectional : WordBool;         { Cursor is uni-directional }
    eprvRights      : Word;             { Table  rights }
    Dummy4          : Word;
    iFmlRights      : Word;             { Family rights }
    iPasswords      : Word;             { Number of Aux passwords }
    iCodePage       : Word;             { Codepage (0 if unknown) }
    bProtected      : WordBool;         { Table is protected by password }
    iTblLevel       : Word;             { Driver dependent table level }
    szLangDriver    : DBINAME;          { Language driver name }
    bFieldMap       : WordBool;         { Field map active }
    iBlockSize      : Word;             { Physical file blocksize in K }
    bStrictRefInt   : WordBool;         { Strict referential integrity }
    iFilters        : Word;             { Number of filters }
    bTempTable      : WordBool;         { Table is a temporary table }
    iUnUsed         : packed array [0..15] of Word;
  end;

{ Delayed Update Types and Constants }

type
  DBIDelayedUpdCmd = (                  { Op types for Delayed Update cursor }
    dbiDelayedUpdCommit,                { Commit the updates }
    dbiDelayedUpdCancel,                { Rollback the updates }
    dbiDelayedUpdCancelCurrent,         { Cancel the Current Rec Change }
    dbiDelayedUpdPrepare                { Phase1 of 2 phase commit }
  );

const
  DBIDELAYUPD_SHOWMODIFYBIT    = $1;    { Show only modified records. }
  DBIDELAYUPD_SHOWINSERTBIT    = $2;    { Show only inserted records. }
  DBIDELAYUPD_SHOWDELETEBIT    = $4;    { Show only deleted records. }
  DBIDELAYUPD_SHOWNONMODIFYBIT = $8;    { Show only unmodified recs. }

{============================================================================}
{                   Record Properties                                        }
{============================================================================}

type
  pRECProps = ^RECProps;
  RECProps = packed record              { Record properties }
    iSeqNum         : Longint;          { When Seq# supported only }
    iPhyRecNum      : Longint;          { When Phy Rec#s supported only }
    iRecStatus      : Word;             { Delayed Updates Record Status }
    bSeqNumChanged  : WordBool;         { Not used }
    bDeleteFlag     : WordBool;         { When soft delete supported only }
  end;


{============================================================================}
{                   General properties  DbiGetProp/DbiSetProp                }
{============================================================================}


{ Cursor properties }
{ General           }

const
  curMAXPROPS        = $00050000;       { ro UINT16   , Number of defined properties }
  curTABLENAME       = $00050001;       { ro pTBLNAME , Table name }
  curTABLETYPE       = $00050002;       { ro pDBINAME , Table type }
  curTABLELEVEL      = $00050003;       { ro UINT16   , Table level 1..n }
  curFILENAME        = $00050004;       { ro pPATH    , Full filename }
  curXLTMODE         = $00050005;       { rw XLTMode  , Translate mode }
  curSEQREADON       = $00050006;       { rw BOOL     , Sequential read mode hint ON }
  curONEPASSON       = $00050007;       { rw BOOL     , One pass mode hint ON }
  curUPDATETS        = $00050008;       { ro TIMESTAMP, Update timestamp }
  curSOFTDELETEON    = $00050009;       { rw BOOL     , Softdelete ON }
  curLANGDRVNAME     = $0005000A;       { ro PAnsiChar    , Symbolic language drv. name }
  curCURSORNAME      = $0005000B;       { ro PAnsiChar    , name of the cursor }
  curPESSIMISTICLOCKS= $0005000C;       { rw BOOL     , Pess Locks = TRUE, Optimistic = FALSE }
  curEXACTRECCNTEXPENSIVE= $0005000D;   { ro EXPENSIVE, Whether DbiGetRecordCount is expensive }
  curCOMPRESSARRAYFLDDESC= $0005000E;   { rw BOOL, Compress ARRAY field desc }
  curMAXFIELDID      = $0005000F;       { ro UINT16, Max # of field desc }
  curFIELDFULLNAME   = $00050010;       { ro pObjAttrDesc, Object attribute name }
  curFIELDTYPENAME   = $00050011;       { ro pObjTypeDesc, Object Type name }
  curFIELDIDFORNAME  = $00050012;       { ro Field id returned for a given fieldname }
  curFIELDIDFORPARENT= $00050013;       { ro Field id of the parent returned }
  curMAKECRACK       = $00050014;       { Create a crack at the current cursor position }
  curFIELDISAUTOINCR = $00050015;       { wo BOOL, Auto increment field }
  curFIELDISDEFAULT  = $00050016;       { wo BOOL, Default field }
  curAUTOREFETCH     = $00050017;       { rw BOOL, Refetch inserted record }

  maxcurPROPS        = 23;              { keep in sync when adding cursor properties }

{ Paradox specific }
  curPDXMAXPROPS     = $01050000;       { ro UINT16   , Number of defined properties }
  maxcurPDXPROPS     = 0;

{ DBase specific }
  curDBMAXPROPS      = $02050000;       { ro UINT16   , Number of defined properties }
  curINEXACTON       = $02050001;       { rw BOOL     , InExact match ON }
  curSHAREMODE       = $02050002;       { rw DBIShareMode, Share mode }
  maxcurDBPROPS      = 2;

{ Text Driver specific }
  curDELIMITALLFLDS  = $03050000;       { rw BOOL , Delimit all fields. }
  curUSESCHEMAFILE   = $03050001;       { rw BOOL , read schema from a file }

{ SQL Driver specific }
  curUPDLOCKMODE     = $04050000;       { rw UPDLockMode, Update lock mode }
  curNATIVEHNDL      = $04050001;       { ro pBYTE, Native cursor handle }
  curMAXROWS         = $04050002;       { rw INT32, Max rows to fetch from server }
  curGETEXTENDEDINFO = $04050003;       { rw BOOL, Get RINT info etc. }
  curGETHIDDENCOLUMNS= $04050004;       { rw BOOL , Get all selected columns from server. }
  curAUTOFLUSHREF    = $04050005;       { rw BOOL, automatic flush ref }
  curREFINSERTTABLENAME= $04050006;     { rw pTBLNAME , REF Table name }
  curGETREF          = $04050007;       { ro REF of inserted record }
  curCONSTSTATE      = $0405009B;       { rw BOOL, enable/disable constraints }

{ Delayed Updates Specific. }
  curDELAYUPDRECSTATE     = $05050001;  { ro DELYUPDCbDesc }
  curDELAYUPDABORTRESTORE = $05050002;  { rw BOOL, restore state }
                                        {  when commit is aborted. }
  curDELAYUPDDISPLAYOPT   = $05050003;  { rw UINT16, view records }
                                        {  with specific update status }
  curDELAYUPDGETOLDRECORD = $05050004;  { rw BOOL, get un-modified }
                                        {  rec buf for modified recs }
  curDELAYUPDNUMUPDATES   = $05050005;  { ro INT32, num of updates }
  curDELAYUPDUSECALLBACK  = $05050006;  { rw BOOL, callback usr. }

{ Driver properties }
{ General           }
  drvMAXPROPS        = $00030000;       { ro UINT16   , Number of defined properties }
  drvDRIVERTYPE      = $00030002;       { ro pDBINAME , Driver type }
  drvDRIVERVERSION   = $00030003;       { ro UINT16   , Driver version }
  maxdrvPROPS        = 2;               { keep in sync when adding driver properties }

  drvPSEUDOINDEX     = $ff030001;       { ro BOOL, Pseudo Index Support. }
  
{ SQL Driver specific }
  drvNATIVESQLCA     = $00030004;       { ro pBYTE    , Native SQLCA structure (Informix) }

{ Database properties }
{ General             }
  dbMAXPROPS         = $00040000;       { ro UINT16   , Number of defined properties }
  dbDATABASENAME     = $00040001;       { ro pDBINAME , Database name/alias }
  dbDATABASETYPE     = $00040002;       { ro pDBINAME , Database type }
  dbDEFAULTDRIVER    = $00040003;       { rw pDBINAME , Default Driver name }
  dbPARAMFMTQMARK    = $00040004;       { rw BOOL     , Stmt param marker fmt = ? }
  dbUSESCHEMAFILE    = $00040005;       { rw BOOL , for text driver only. }
  maxdbPROPS         = 27;              { keep in sync when adding ANY db properties }

{ SQL Driver specific }
  dbASYNCSUPPORT     = $04040000;       { ro BOOL     , Async. qry exec support }
  dbPROCEDURES       = $04040001;       { ro BOOL     , Stored procedure support }
  dbDEFAULTTXNISO    = $04040002;       { ro eXILType , Default transaction iso level }
  dbNATIVEHNDL       = $04040003;       { ro pBYTE    , Native connection handle }
  dbNATIVEPASSTHRUHNDL = $04040004;     { ro pBYTE    , Native passthru connection handle }
  dbSERVERVERSION    = $04040005;       { ro UINT16   , Major server version number }
  dbBATCHCOUNT       = $04040006;       { rw UINT16   , Batch modification count before auto-commit }
  dbTRACEMODE        = $04040007;       { rw UINT16   , Trace mode }
  dbCURRENTUSERNAME  = $04040008;       { rw UINT16   , Current User Name }
  dbOWNERQUALIFIED   = $04040009;       { ro BOOL     , For SQL Link Drivers - does this driver support }
  dbQUOTECHAR        = $0404000A;       { ro AnsiChar     , Quote character for quoting Object Names }
  dbONLINE           = $0404000B;       { ro BOOL     , Informix ONLINE }
  dbTRANALLOWED      = $0404000C;       { ro BOOL     , Informix Transactions allowed. }
  dbANSI             = $0404000D;       { ro BOOL     , Informix Ansi complaint database. }
  dbDBMSNAME         = $0404000E;       { ro pDBINAME , DB2 specific Database (OS/2 or MVS or AS/400). }
  dbBLOBCOUNT        = $0404000F;       { ro UINT16, No.of dead BLOB to cache. }
  dbBLOBSIZE         = $04040010;       { ro UINT16, Dead BLOB fetch buffer size. }
  dbCOMPRESSARRAYFLDDESC  = $04040011;  { rw BOOL, VARRAY in compressed format, ORACLE 8 specific. }
  dbWAITONLOCK       = $04040012;      { rw BOOL, Lock resolution on transactions , Interbase specific. }
  dbCOMMITRETAIN     = $04040013;      { rw BOOL, Commit mode for explicit transactions , Interbase specific. }

  dbSCHEMACACHEENABLED = $04040099;     { w BOOL     , TRUE - enabled/FALSE - disabled }
  dbBCDENABLED       = $0404009A;       { rw BOOL , enable BCD field types. }

{ Session properties }
{ General            }
  sesMAXPROPS        = $00020000;       { ro UINT16   , Number of defined properties }
  sesSESSIONNAME     = $00020001;       { ro pDBINAME , Name of seesion }
  sesNETFILE         = $00020002;       { rw PAnsiChar    , Full filename of netfile (MAXPATHLEN) }
  sesCFGMODE         = $00020003;       { rw CFGMode, Mode of configuration file. }
  sesDEFAULTDRIVER   = $00020004;       { rw pDBINAME, default driver name }
  sesGROUPNAME       = $00020005;       { rw PAnsiChar, Security - user group name (dBASE) }
  sesUSERNAME        = $00020006;       { rw PAnsiChar, User Name }
  sesUSERPASSWORD    = $00020007;       { rw PAnsiChar, User password }
  sesSECFILENAME     = $00020008;       { rw PAnsiChar, Location of dBASE Security file }
  sesDRNAME          = $00020009;       { rw PAnsiChar, Repository Database Name }
  sesCFGUPDATE       = $0002000A;       { rw CFGUpdate, Mode for updating session configuration }
  sesCFGNAME         = $0002000B;       { ro CFGName, get session configuration name }
  sesCFGMODE2        = $0002000C;       { rw new CFGMode (persist, session, virtual) }
  sesCFGRefresh      = $0002000F;       { rw, set session to refresh virtual config }
  maxsesPROPS        = 13;              { keep in sync when adding session properties }

{ System properties }
{ General           }
  sysMAXPROPS        = $00010000;       { ro UINT16  , Number of defined properties }
  sysLOWMEMUSAGE     = $00010001;       { ro UINT16  , Low memory usage in (k) }
  maxsysPROPS        = 1;               { keep in sync when adding system properties }

{ Statement properties }
{ General              }
  stmtMAXPROPS       = $00060001;       { ro UINT16      Number of defined properties }
  stmtPARAMETERCOUNT = $00060002;       { ro UINT16      Count of parameters }
  stmtPARAMETERDESCS = $00060003;       { ro aFLDDesc    Array of parameters }
  stmtLANGDRVNAME    = $00060004;       { ro PAnsiChar       Symbolic language drv. name }
  stmtUNIDIRECTIONAL = $00060010;       { rw BOOL        Cursor Unidirectional }
  stmtCURSORNAME     = $00060011;       { rw PAnsiChar       Cursor name }
  stmtNEWCONNECT     = $00060012;       { rw BOOL        Stmt on new connection }
  stmtNATIVEHNDL     = $00060013;       { ro pBYTE       Native statement handle }
  stmtROWCOUNT       = $00060014;       { ro UINT32      Rows effected by a stmt }
  stmtEXECASCURSOR   = $00060015;       { rw BOOL        Open a server cursor (ct-lib) }
  maxstmtPROPS       = 32;              { keep in sync when adding ANY stmt properties }

{ specific to QBE or local SQL }
  stmtANSTYPE        = $00060020;       { rw pBYTE       Table Type of Answer set }
  stmtLIVENESS       = $00060021;       { rw LIVENESS    Preference for canned/live answers }
  stmtQRYMODE        = $00060022;       { rw QryEvalMode Execute on server or local or either }
  stmtBLANKS         = $00060023;       { rw BOOL        True if blanks to be regarded as zeros. }
  stmtDATEFORMAT     = $00060024;       { rw FMTDate     Date format }
  stmtNUMBERFORMAT   = $00060025;       { rw FMTNumber   Number format }
  stmtAUXTBLS        = $00060026;       { rw BOOL        True if QBE to create CHANGED, etc. }
  stmtTBLVECTOR      = $00060027;       { ro UINT16      Vector of tables generated by query. }
  stmtALLPROPS       = $00060028;       { rw QueryLowProps }
  stmtALLPROPSSIZE   = $00060029;       { rw INT16       size of QueryLowProps }
  stmtANSNAME        = $00060030;       { rw pBYTE       Answer Table Name. }
  stmtCONSTRAINED    = $00060031;       { rw BOOL        Constrain input }
  stmtFIELDDESCS     = $00060032;       { rw pFLDDESC    Answer FieldDescs }
  stmtCURPROPS       = $00060033;       { rw pCURProps    Answer Curprops }
  stmtDEFDBNAME      = $00060034;       { rw pCURProps    Answer Curprops }
  stmtXLTMODE        = $00060035;       { rw XLTMode      Xltmode }
  stmtINSTBLNAME     = $00060036;       { ro DBITBLNAME  INSERT table's name }
  stmtINSERRTBLNAME  = $00060037;       { ro DBITBLNAME  ERRINS table's name }
  stmtUPDTBLNAME     = $00060038;       { ro DBITBLNAME  UPDATE table's name }
  stmtUPDERRTBLNAME  = $00060039;       { ro DBITBLNAME  ERRUPD table's name }
  stmtDELTBLNAME     = $00060040;       { ro DBITBLNAME  DELETED table's name }
  stmtDELERRTBLNAME  = $00060041;       { ro DBITBLNAME  ERRDEL table's name }
  stmtCANNEDREADONLY = $00060042;       { rw BOOL canned answers are readonly }


{============================================================================}
{                    Transactions                                            }
{============================================================================}

{ Transaction support }

type
  eXILType = (                          { Transaction isolation levels }
    xilDIRTYREAD,                       { Uncommitted changes read }
    xilREADCOMMITTED,                   { Committed changes, no phantoms }
    xilREPEATABLEREAD                   { Full read repeatability }
  );

  eXEnd = (                             { Transaction end control }
    xendCOMMIT,                         { Commit transaction }
    xendCOMMITKEEP,                     { Commit transaction, keep cursors }
    xendABORT                           { Rollback transaction }
  );

  eXState = (                           { Transaction end control }
    xsINACTIVE,                         { Transaction inactive }
    xsACTIVE                            { Transaction active }
  );

type
  pXInfo = ^XInfo;
  XInfo = packed record
    exState         : eXState;          { xsActive, xsInactive }
    eXIL            : eXILType;         { Xact isolation level }
    uNests          : Word;             { Xact children }
  end;


{============================================================================}
{                         BookMark compares                                  }
{============================================================================}

type
  PCMPBkMkRslt = ^CMPBkMkRslt;
  CMPBkMkRslt = TypedEnum;
const
    CMPLess           = -1;             { Bkm1 < Bkm2 }
    CMPEql            = 0;              { BookMarks are exactly the same }
    CMPGtr            = 1;              { Bkm1 > Bkm2 }
    CMPKeyEql         = 2;              { Only Bkm1.key_val = Bkm2.key_val }

{============================================================================}
{                             Key searches                                   }
{============================================================================}

type
  DBISearchCond = (                     { Search condition for keys }
    keySEARCHEQ,                        { = }
    keySEARCHGT,                        { > }
    keySEARCHGEQ                        { >= }
  );

{============================================================================}
{                      Create/Restructure descriptor                         }
{============================================================================}

type
  pCROpType          = ^CROpType;
  CROpType = (                          { Create/Restruct Operation type }
    crNOOP,
    crADD,                              { Add a new element. }
    crCOPY,                             { Copy an existing element. }
    crMODIFY,                           { Modify an element. }
    crDROP,                             { Removes an element. }
    crREDO,                             { Reconstruct an element. }
    crTABLE,                            { Not used }
    crGROUP,                            { Not used }
    crFAMILY,                           { Not used }
    crDONE,                             { Used internally }
    crDROPADD                           { Used internally }
  );

type
  pCRTblDesc         = ^CRTblDesc;
  CRTblDesc = packed record             { Create/Restruct Table descr }
    szTblName       : DBITBLNAME;       { TableName incl. optional path & ext }
    szTblType       : DBINAME;          { Driver type (optional) }
    szErrTblName    : DBIPATH;          { Error Table name (optional) }
    szUserName      : DBINAME;          { User name (if applicable) }
    szPassword      : DBINAME;          { Password (optional) }
    bProtected      : WordBool;         { Master password supplied in szPassword }
    bPack           : WordBool;         { Pack table (restructure only) }
    iFldCount       : Word;             { Number of field defs supplied }
    pecrFldOp       : pCROpType;        { Array of field ops }
    pfldDesc        : pFLDDesc;         { Array of field descriptors }
    iIdxCount       : Word;             { Number of index defs supplied }
    pecrIdxOp       : pCROpType;        { Array of index ops }
    pidxDesc        : PIDXDesc;         { Array of index descriptors }
    iSecRecCount    : Word;             { Number of security defs supplied }
    pecrSecOp       : pCROpType;        { Array of security ops }
    psecDesc        : pSECDesc;         { Array of security descriptors }
    iValChkCount    : Word;             { Number of val checks }
    pecrValChkOp    : pCROpType;        { Array of val check ops }
    pvchkDesc       : pVCHKDesc;        { Array of val check descs }
    iRintCount      : Word;             { Number of ref int specs }
    pecrRintOp      : pCROpType;        { Array of ref int ops }
    printDesc       : pRINTDesc;        { Array of ref int specs }
    iOptParams      : Word;             { Number of optional parameters }
    pfldOptParams   : pFLDDesc;         { Array of field descriptors }
    pOptData        : Pointer;          { Optional parameters }
  end;

{============================================================================}
{                      Batch                                                 }
{============================================================================}

type
  pBATTblDesc        = ^BATTblDesc;
  BATTblDesc = packed record            { Batch Table definition struct }
    hDb             : hDBIDb;           { Database }
    szTblName       : DBIPATH;          { Table name }
    szTblType       : DBINAME;          { Optional Driver type }
    szUserName      : DBINAME;          { Optional User name }
    szPassword      : DBINAME;          { Optional Pass word }
  end;

  eBATMode = (                          { Batch mode for DBIBatchMove }
    batchAPPEND,
    batchUPDATE,
    batchAPPENDUPDATE,
    batchSUBTRACT,
    batchCOPY
  );

type
  pSORTOrder         = ^SORTOrder;
  SORTOrder = (                         { Sort Order }
    sortASCEND,                         { ascending (default) }
    sortDESCEND                         { descending }
  );

  ppfSORTCompFn = ^pfSORTCompFn;
  pfSORTCompFn = function (             { pntr to client compare fn }
var   LdObj         : Pointer;          { Language driver, if needed }
var   Value1        : Pointer;          { first value }
var   Value2        : Pointer;          { second value }
      iLen          : SmallInt              { Length, if needed }
   ): SmallInt;

      { Returns: -1 if (Value 1 < Value 2),  }
      {           0 if (Value 1 == Value 2), }
      {          +1 if (Value 1 > Value 2)   }

{============================================================================}
{                       Types/Structs for Capabilities                       }
{============================================================================}

type
  DRVCat = (                            { Driver Category }
    drvFILLER,                          { Filler to make next start at 1 }
    drvFILE,                            { File based (Paradox, xBASE) }
    drvOTHERSERVER,                     { Other kind of server (IMS ?) }
    drvSQLBASEDSERVER,                  { SQL Based Server }
    drvOLEDBPROVIDER                    { OLE DB Provider }
  );

  OPType = (                            { Config info & Optional Parameters }
    opFILLER0,                          { Filler to make first one 3 }
    opFILLER1,                          { Filler to make first one 3 }
    opFILLER2,                          { Filler to make first one 3 }
    opDBCREATE,
    opDBOPEN,
    opTBLCREATE,
    opTBLOPEN
  );

type
  pDRVType = ^DRVType;
  DRVType = packed record               { Driver Description/Capabilities }
    szType          : DBINAME;          { Symbolic name to identify }
    szText          : DBINAME;          { Descriptive text }
    edrvCat         : DRVCat;           { Driver category }
    bTrueDb         : WordBool;         { Supports True Db concept }
    szDbType        : DBINAME;          { Db Type to be used }
    bMultiUser      : WordBool;         { Supports Multi-user access }
    bReadWrite      : WordBool;         { Read Write or Read-only }
    bTrans          : WordBool;         { Supports Transactions }
    bPassThruSQL    : WordBool;         { Supports Pass-thru SQL }
    bLogIn          : WordBool;         { Requires explicit login }
    bCreateDb       : WordBool;         { Can reate a Database }
    bDeleteDb       : WordBool;         { Can drop database }
    bCreateTable    : WordBool;         { Can create a Table }
    bDeleteTable    : WordBool;         { Can delete a Table }
    bMultiplePWs    : WordBool;         { Multiple passwords }
    iDriverVersion  : Word;             { Driver version 1..n }
    bSQLRowid       : WordBool;         { Supports SQL rowid }
    iUnUsed         : packed array [0..14] of Word;
  end;


  pTBLType = ^TBLType;
  TBLType = packed record               { Table Capabilities }
    iId             : Word;             { Id of Table Type }
    szName          : DBINAME;          { Symbolic name; eg "dBASE" }
    szText          : DBINAME;          { Descriptive text }
    szFormat        : DBINAME;          { Format; eg "HEAP" }
    bReadWrite      : WordBool;         { User can Read/Write }
    bCreate         : WordBool;         { Can create new tables }
    bRestructure    : WordBool;         { Can restructure this table }
    bValChecks      : WordBool;         { Val Checks can be specified }
    bSecurity       : WordBool;         { Can be protected }
    bRefIntegrity   : WordBool;         { Can participate in ref integrity }
    bPrimaryKey     : WordBool;         { Supports primary key concept }
    bIndexing       : WordBool;         { Can have other indexes }
    iFldTypes       : Word;             { Number of Phy Field types supported }
    iMaxRecSize     : Word;             { Max record size }
    iMaxFldsInTable : Word;             { Max fields in a table }
    iMaxFldNameLen  : Word;             { Maximum field name length }
    iTblLevel       : Word;             { Driver dependent table level (version) }
    iUnUsed         : packed array [0..15] of Word;
  end;


  pIDXType = ^IDXType;
  IDXType = packed record
    iId             : Word;             { Id of Index Type }
    szName          : DBINAME;          { Symbolic name }
    szText          : DBINAME;          { Descriptive text }
    szFormat        : DBINAME;          { Optional format(BTREE, HASH etc) }
    bComposite      : WordBool;         { Supports composite keys }
    bPrimary        : WordBool;         { True, if primary index }
    bUnique         : WordBool;         { True, No duplicates supported }
    bKeyDescending  : WordBool;         { If whole key can be descending }
    bFldDescending  : WordBool;         { Field level descending }
    bMaintained     : WordBool;         { Supports maintained option }
    bSubset         : WordBool;         { Supports Subset expression }
    bKeyExpr        : WordBool;         { If Key can be expres }
    bCaseInsensitive : WordBool;        { Supports Caseinsensitive indexes }
    iUnUsed         : packed array [0..15] of Word;
  end;

  pFLDType = ^FLDType;
  FLDType = packed record
    iId             : Word;             { Id of Field Type }
    szName          : DBINAME;          { Symbolic name; eg "ALPHA" }
    szText          : DBINAME;          { Descriptive text }
    iPhyType        : Word;             { Physical/Native type }
    iXltType        : Word;             { Default xlated type }
    iXltSubType     : Word;             { Default xlated sub type }
    iMaxUnits1      : Word;             { Max units allowed (1) }
    iMaxUnits2      : Word;             { Max units allowed (2) }
    iPhySize        : Word;             { Physical size in bytes (per unit) }
    bRequired       : WordBool;         { Supports 'required' option }
    bDefaultVal     : WordBool;         { Supports user specified 'default' }
    bMinVal         : WordBool;         { Supports MIN Val constraint }
    bMaxVal         : WordBool;         { Supports MAX Val constraint }
    bRefIntegrity   : WordBool;         { Can participate in ref integrity }
    bOtherChecks    : WordBool;         { Supports other kinds of checks }
    bKeyed          : WordBool;         { The field type can be keyed }
    bMultiplePerTable : WordBool;       { Table can have more than 1 of this type }
    iMinUnits1      : Word;             { Min units required (1) }
    iMinUnits2      : Word;             { Min units required (2) }
    bCreateable     : WordBool;         { Type can be created }
    szNativeName    : DBINAME;          { Native (SQL) name used in DDL }
    iUnUsed         : packed array [0..15] of Word;
  end;

  PROPValid = (                         { PROP Validity }
    epropINVALID,                       { Property is INVALID (not supported) }
    epropCANGET,                        { You can GET value of the property }
    epropCANSET                         { You can GET and SET value of the property }
  );

{============================================================================}
{                                Call Backs                                  }
{============================================================================}

const
  cbYIELDCLIENT = cbCANCELQRY;

type
  ppfDBICallBack = ^pfDBICallBack;
  pfDBICallBack  = function (           { Call-back funtion pntr type }
      ecbType       : CBType;           { Callback type }
      iClientData   : Longint;          { Client callback data }
      CbInfo        : Pointer           { Call back info/Client Input }
   ): CBRType stdcall;

{ Progress callback structure }

  pCBPROGRESSDesc = ^CBPROGRESSDesc;
  CBPROGRESSDesc  = packed record
    iPercentDone    : SmallInt;         { Percentage done. }
    szMsg           : DBIMSG;           { Message to display }
  end;

  DelayUpdErrOpType = (                 { type of delayed update object (delayed updates callback) }
    delayupdNONE,
    delayupdMODIFY,
    delayupdINSERT,
    delayupdDELETE
  );

  PDELAYUPDCbDesc = ^DELAYUPDCbDesc;
  DELAYUPDCbDesc = packed record        { delayed updates callback info }
    iErrCode        : DBIResult;
    eDelayUpdOpType : DelayUpdErrOpType;
    iRecBufSize     : Word;             { Record size (physical record) }
    pNewRecBuf      : Pointer;
    pOldRecBuf      : Pointer;
  end;

  RESTErrObjType = (                    { type of restructure object (restructure callback) }
    restrNONE,
    restrNEWFLD,
    restrOLDFLD,
    restrNEWINDEX,
    restrOLDINDEX,
    restrNEWVCHK,
    restrOLDVCHK,
    restrNEWRINT,
    restrOLDRINT,
    restrNEWSEC,
    restrOLDSEC,
    restrNEWTABLE
  );

  pCBDBLogin = ^TCBDBLogin;
  TCBDBLogin = packed record            { Database login }
    szDbName        : DBINAME;          { Database name }
    eOpenMode       : DBIOpenMode;      { Open type desired }
    eShareMode      : DBIShareMode;     { Share type desired }
    szPassword      : DBINAME;          { Password }
    bCallbackToClose : Bool;            { Returned flag }
    hDb             : hDBIDb;           { db handle }
  end;

type
  pTRACECat = ^TRACECat;                { trace categories }
  TRACECat = TypedEnum;
const
    traceUNKNOWN   = $0000;
    traceQPREPARE  = $0001;             { prepared query statements }
    traceQEXECUTE  = $0002;             { executed query statements }
    traceERROR     = $0004;             { vendor errors }
    traceSTMT      = $0008;             { statement ops (i.e. allocate, free) }
    traceCONNECT   = $0010;             { connect / disconnect }
    traceTRANSACT  = $0020;             { transaction }
    traceBLOB      = $0040;             { blob i/o }
    traceMISC      = $0080;             { misc. }
    traceVENDOR    = $0100;             { vendor calls }
    traceDATAIN    = $0200;             { parameter bound data }
    traceDATAOUT   = $0400;             { trace fetched data }

type
  pTRACEDesc = ^TRACEDesc;
  TRACEDesc = packed record             { trace callback info }
    eTraceCat       : TRACECat;
    uTotalMsgLen    : Word;
    pszTrace        : array [0..0] of AnsiChar;
  end;

type
  TuObjDesc = packed record
    case Integer of
      1: (fldDesc:  FLDDesc);
      2: (idxDesc:  IDXDesc);
      3: (vchkDesc: VCHKDesc);
      4: (rintDesc: RINTDesc);
      5: (secDesc:  SECDesc);
  end;

  RESTCbDesc = packed record            { restructure callback info }
    iErrCode        : DBIResult;
    iTblNum         : Word;
    iObjNum         : Word;
    eRestrObjType   : RESTErrObjType;
    uObjDesc        : TuObjDesc;
  end;

  CBEntry = packed record
    szKeyWord       : DBINAME;          { Keyword to display }
    szHelp          : DBIMSG;           { Help String }
  end;

const
  MAXCBENTRIES       = 4;

type
  CBInputId = (
    cbiFILLER,                          { Force next to start at 1 }
    cbiMDXMISSING,                      { MDX file missing request }
    cbiPDXLOOKUP,                       { Lookup table missing }
    cbiPDXBLOB,                         { Blob file missing }
    cbiDBTMISSING,                      { DBT file missing request }
    cbiDRINPUT,                         { DR Manager needs user input }
    cbiRINTMISSING                      { RefInt link missing/corrupt }
  );

  PCBInputDesc = ^CBInputDesc;
  CBInputDesc = packed record
    eCbInputId      : CBInputId;        { Id for this input request }
    iCount          : SmallInt;         { Number of entries }
    iSelection      : SmallInt;         { Selection 1..n  (In/Out) }
    bSave           : WordBool;         { Save this option  (In/Out) }
    szMsg           : DBIMSG;           { Message to display }
    acbEntry        : packed array [0..MAXCBENTRIES-1] of CBEntry; { Entries }
  end;

  pCBLoginDesc = ^CBLoginDesc;
  CBLoginDesc  = packed record          { dBASE login callback structure }
    szUserName      : DBINAME;          { Login name of user }
    szGroupName     : DBINAME;          { Group to log in to }
    szUserPassword  : DBINAME;          { User password }
  end;

{ cbSERVERCALL info }

  PCBSCType = ^CBSCType;
  CBSCType = (                             { Server call type }
    cbscRETURN,                            { Return from a call }
    cbscSQL                                { SQL server call }
  );

{ cbYIELDCLIENT }

  PCBYieldStep = ^CBYieldStep;
  CBYieldStep = (
    cbYieldFirst,
    cbYieldContinue,
    cbYieldLast
  );

{============================================================================}
{                         Basic Query Types                                  }
{============================================================================}

type
  DBIQryLang = (
    qrylangUNKNOWN,                     { UNKNOWN (Error) }
    qrylangQBE,                         { QBE }
    qrylangSQL                          { SQL }
  );

{============================================================================}
{                         Statement parameter information                    }
{============================================================================}

type
  eINDValues = TypedEnum;
const
    indTRUNC     = -2;                   { Value has been truncated }
    indNULL      = -1;                   { Value is NULL }
    indVALUE     = 1;

const
  INDLEN             = sizeof(Word);

type
  STMTParamType = (
    paramUNKNOWN,                       { UNKNOWN (Error) }
    paramIN,                            { Input parameter }
    paramOUT,                           { Output parameter }
    paramINOUT,                         { Input/Output parameter }
    paramRET                            { Procedure (or function) return }
  );

  STMTParamFormat = (
    paramfmtUNKNOWN,                    { UNKNOWN (Error) }
    paramfmtNUMBER,                     { Numbered parameter markers of the form ? or :n }
    paramfmtNAME                        { Named parameters markers of the form :name }
  );

{============================================================================}
{                         Properties For DbiQPrepareExt                      }
{============================================================================}

const
  qprepNONE          = 0;               { like DbiQPrepare }
  qprepFORUPDATE     = $1;              { do extra work, anticipating UPDATE WHERE CURRENT }


{============================================================================}
{                      Date, Time, Number Formats                            }
{============================================================================}

type
  pFMTNumber = ^FMTNumber;
  FMTNumber = packed record             { Date Format }
    cDecimalSeparator : AnsiChar;           { Default "." }
    cThousandSeparator : AnsiChar;          { Default "," }
    iDecimalDigits  : Byte;             { Default 2 }
    bLeadingZero    : Boolean;          { Default TRUE. }
  end;

  pFMTDate = ^FMTDate;
  FMTDate = packed record
    szDateSeparator : packed array [0..3] of AnsiChar;    { Default "/" }
    iDateMode       : Byte;            { 0 = MDY (Def), 1 = DMY, 2 = YMD }
    bFourDigitYear  : Boolean;         { Write Year as 4 digits (FALSE) }
    bYearBiased     : Boolean;         { On input add 1900 to year (TRUE) }
    bMonthLeadingZero : Boolean;       { Month with Leading Zero (TRUE) }
    bDayLeadingZero : Boolean;         { Day with Leading Zero (TRUE) }
  end;

  pFMTTime = ^FMTTime;
  FMTTime = packed record
    cTimeSeparator  : AnsiChar;             { Default ":" }
    bTwelveHour     : Boolean;          { Represent as 12 Hour time (FALSE) }
    szAmString      : packed array [0..5] of AnsiChar;    { Default Null (Only for 12 Hr) }
    szPmString      : packed array [0..5] of AnsiChar;    { Default Null (Only for 12 Hr) }
    bSeconds        : Boolean;          { Show Seconds (TRUE) }
    bMilSeconds     : Boolean;          { Show Milli Seconds (FALSE) }
  end;

  pFMTBcd = ^FMTBcd;
  FMTBcd  = packed record
    iPrecision      : Byte;             { 1..64 considered valid }
    iSignSpecialPlaces : Byte;          { sign:1, special:1, places:6 }
    iFraction       : packed array [0..31] of Byte;    { bcd nibbles, 00..99 per byte, high nibble 1st }
  end;


{============================================================================}
{                              Error Info                                    }
{============================================================================}

type
  pDBIErrInfo        = ^DBIErrInfo;
  DBIErrInfo = packed record            { Error info }
    iError          : DBIResult;        { Last error code returned }
    szErrCode       : DBIMSG;           { Error Code }
    szContext       : packed array[1..4] of DBIMSG;    { Context info }
  end;

{ Error contexts (To be used with DbiGetErrorContext) }

const
  ecTOKEN            = 1;               { Token (For QBE) }
  ecTABLENAME        = 3;               { Table name }
  ecFIELDNAME        = 4;               { Field Name }
  ecIMAGEROW         = 5;               { Image Row (For QBE) }
  ecUSERNAME         = 6;               { eg, In lock conflicts, user involved }
  ecFILENAME         = 7;               { File Name }
  ecINDEXNAME        = 8;               { Index Name }
  ecDIRNAME          = 9;               { Directory Name }
  ecKEYNAME          = 10;              { Key Name }
  ecALIAS            = 11;              { Alias }
  ecDRIVENAME        = 12;              { Drive name ('c:') }
  ecNATIVECODE       = 13;              { Native error code }
  ecNATIVEMSG        = 14;              { Native error message }
  ecLINENUMBER       = 15;              { Line Number }
  ecCAPABILITY       = 16;              { Capability }
  ecCDRNAME          = 17;              { Client Data Repository Name }
  ecUSERERRMSG       = 18;              { User Defined error msg }
  ecDROBJNAME        = 19;              { Data Repository Object Name }
  ecINTERNALLIMIT    = 20;              { Internal limit }
  ecEXPRESSION       = 21;              { SQL Expression }

{============================================================================}
{                  Schema info structures                                    }
{============================================================================}
{                    Database descriptor                                     }
{============================================================================}


{ DbiOpenDatabaseList }
type
  pDBDesc = ^DBDesc;
  DBDesc = packed record                { A given Database Description }
    szName          : DBINAME;          { Logical name (Or alias) }
    szText          : DBINAME;          { Descriptive text }
    szPhyName       : DBIPATH;          { Physical name/path }
    szDbType        : DBINAME;          { Database type }
  end;


{============================================================================}
{                    User info descriptor                                    }
{============================================================================}


{ DbiOpenUserList }
type
  pUSERDesc = ^USERDesc;
  USERDesc = packed record              { User description }
    szUserName      : DBIUSERNAME;
    iNetSession     : Word;             { Net level session number }
    iProductClass   : Word;             { Product class of user }
    szSerialNum     : packed array [0..21] of AnsiChar; { Serial number }
  end;

{============================================================================}
{                    Table descriptor                                        }
{============================================================================}


{ DbiOpenTableList }
type
  pTBLBaseDesc = ^TBLBaseDesc;
  TBLBaseDesc = packed record           { Table description (Base) }
    szName          : DBITBLNAME;       { Table name(No extension or Dir) }
    szFileName      : DBITBLNAME;       { File name }
    szExt           : DBIEXT;           { File extension }
    szType          : DBINAME;          { Driver type }
    dtDate          : DBIDATE;          { Date on the table }
    tmTime          : Time;             { Time on the table }
    iSize           : Longint;          { Size in bytes }
    bView           : WordBool;         { If this a view }
    bSynonym        : WordBool;         { If this is a synonym }
  end;

  pTBLExtDesc = ^TBLExtDesc;
  TBLExtDesc = packed record            { Table description (Extended part) }
    szStruct        : DBINAME;          { Physical structure }
    iRestrVersion   : Word;             { Version # }
    iRecSize        : Word;             { Physical record size }
    iFields         : Word;             { Number of fields }
    iIndexes        : Word;             { Number Indexes }
    iValChecks      : Word;             { Number of field validity checks }
    iRintChecks     : Word;             { Number of ref. integrity checks }
    iRecords        : Longint;          { Number of records in table }
    bProtected      : WordBool;         { If the table is prot }
    bValidInfo      : WordBool;         { Info available for this table }
  end;

  pTBLFullDesc = ^TBLFullDesc;
  TBLFullDesc = packed record           { Table description (Base + Ext) }
    tblBase         : TBLBaseDesc;      { Base      description }
    tblExt          : TBLExtDesc;       { Extended  description }
  end;

{============================================================================}
{                    File descriptor                                         }
{============================================================================}


{ DbiOpenFileList }
type
  pFILEDesc = ^FILEDesc;
  FILEDesc = packed record              { File description }
    szFileName      : DBIPATH;          { File name (No Dir or ext) }
    szExt           : DBIEXT;           { Extension }
    bDir            : WordBool;         { True, if directory }
    iSize           : Longint;          { File size in bytes }
    dtDate          : DBIDATE;          { Date on the file }
    tmTime          : Time;             { Time on the file }
  end;

{======================================================================}
{            Stored Procedure and Stored Procedure Param descriptor    }
{======================================================================}

type
  pSPDesc = ^SPDesc;
  SPDesc = packed record
    szName          : DBISPNAME;
    dtDate          : DBIDATE;
    tmTime          : Time;
  end;

  pSPParamDesc = ^SPParamDesc;
  SPParamDesc = packed record
    uParamNum       : Word;
    szName          : DBINAME;
    eParamType      : STMTParamType;
    uFldType        : Word;
    uSubType        : Word;
    iUnits1         : SmallInt;
    iUnits2         : SmallInt;
    uOffset         : Word;
    uLen            : Word;
    uNullOffset     : Word;
  end;
  BDESPParamDesc = SPParamDesc;

{======================================================================}
{            Function and Function Argument Descriptors                }
{======================================================================}

type
  DBISTDFuncs = (
    fnAVG,
    fnCOUNT,
    fnMIN,
    fnMAX,
    fnSUM,
    fnSTDDEV,
    fnVARIANCE,
    fnABS,
    fnCEIL,
    fnCOS,
    fnCOSH,
    fnEXP,
    fnFLOOR,
    fnLN,
    fnLOG,
    fnMOD,
    fnPOWER,
    fnROUND,
    fnSIGN,
    fnSIN,
    fnSINH,
    fnSQRT,
    fnTAN,
    fnTANH,
    fnTRUNC,
    fnCHR,
    fnCONCAT,
    fnINITCAP,
    fnLOWER,
    fnLPAD,
    fnLTRIM,
    fnNLS_INITCAP,
    fnNLS_LOWER,
    fnNLS_UPPER,
    fnREPLACE,
    fnRPAD,
    fnRTRIM,
    fnSOUNDEX,
    fnSUBSTR,
    fnSUBSTRB,
    fnTRANSLATE,
    fnUPPER,
    fnASCII,
    fnINSTR,
    fnINSTRB,
    fnLENGTH,
    fnLENGTHB,
    fnNLSSORT,
    fnADD_MONTHS,
    fnLAST_DAY,
    fnMONTHS_BETWEEN,
    fnNEW_TIME,
    fnNEXT_DAY,
    fnSYSDATE,
    fnCONVERT,
    fnTO_CHAR,
    fnTO_DATE,
    fnTO_MULTI_BYTE,
    fnTO_NUMBER,
    fnTO_SINGLE_BYTE,
    fnUID,
    fnUSER,
    fnORACLEMISC,
    fnACOS,
    fnASIN,
    fnATAN,
    fnATN2,
    fnCOT,
    fnDEGREES,
    fnLOG10,
    fnPI,
    fnRADIANS,
    fnRAND,
    fnTEXTPTR,
    fnTEXTVALID,
    fnCHARINDEX,
    fnDIFFERENCE,
    fnPATINDEX,
    fnREPLICATE,
    fnREVERSE,
    fnRIGHT,
    fnSPACE,
    fnSTR,
    fnSTUFF,
    fnCOL_NAME,
    fnCOL_LENGTH,
    fnDATALENGTH,
    fnDB_ID,
    fnDB_NAME,
    fnHOST_ID,
    fnHOST_NAME,
    fnINDEX_COL,
    fnOBJECT_ID,
    fnOBJECT_NAME,
    fnUSER_ID,
    fnUSER_NAME,
    fnLEFT,
    fnLOCATE,
    fnTRUNCATE,
    fnCURTIME,
    fnDAYNAME,
    fnDAYOFMONTH,
    fnDAYOFWEEK,
    fnDAYOFYEAR,
    fnHOUR,
    fnMINUTE,
    fnMONTH,
    fnMONTHNAME,
    fnNOW,
    fnQUARTER,
    fnSECOND,
    fnWEEK,
    fnYEAR,
    fnDAY,
    fnWEEKDAY,
    fnTODAY,
    fnDATE,
    fnINFMISC
  );

type
  DBIFuncFlags = TypedEnum;

const
  fnSCALARS_ALLOW_CONSTANTS           = $0001;    { fn args may contain refeences to constants | }
  fnSCALARS_ALLOW_COLUMNS             = $0002;    { fn args may contain refeences to columns }
  fnSCALARS_ALLOW_PARAMETERS          = $0004;    { fn args may contain refeences to parameters }
  fnSCALARS_ALLOW_FUNCTIONS           = $0008;    { fn args may contain refeences to functions }
  fnSCALARS_ALLOW_USER_DEFINED_FUNCS  = $0010;    { fn args may contain refeences to user defined functions }
  fnSCALARS_ALLOW_SUBQUERIES          = $0020;    { fn args can contain subqueries }
  fnSCALARS_ALLOW_CORRELATION         = $0040;    { fn subqueries can be correlated }

type
  DBIFUNCOpts = (
    fnDummy,
    fnListINCL_USER_DEF                 { include user-defined functions }
  );

  pDBIFUNCDesc = ^DBIFUNCDesc;
  DBIFUNCDesc  = packed record
    szName          : DBINAME;          { Function name }
    szDesc          : packed array [0..254] of AnsiChar; { Short description }
    uOverload       : Word;             { Number of function overloads }
    eStdFn          : DBISTDFuncs;      { Corresponds to DBI standard function }
  end;


  pDBIFUNCArgDesc = ^DBIFUNCArgDesc;
  DBIFUNCArgDesc  = packed record
    uArgNum         : Word;             { Argument position num; 0 for fn return }
    uFldType        : Word;             { Field type }
    uSubType        : Word;             { Field subtype (if applicable) }
    ufuncFlags      : Word;             { Function flags }
  end;

{============================================================================}
{                   Configuration Info Descriptor                            }
{============================================================================}

  CFGMode = (
    cfgPersistent,                      { Persistent only }
    cfgSession,                         { Session relative only }
    cfgAll                              { All (system and single session) }
  );

type
  CFGMode2 = type Integer;
const
    cfgmNone        = $00;
    cfgmVirtual     = $01;
    cfgmPersistent  = $02;
    cfgmSession     = $04;
    cfgmAll         = cfgmVirtual or cfgmPersistent or cfgmSession;

type
  CFGUpdate = (
    cfgUpdateOn,                        { Accept updates from other sessions }
    cfgUpdateOff                        { Do not updates from other sessions }
  );

{ DbiOpenCfgInfoList }
type
  pCFGDesc = ^CFGDesc;
  CFGDesc = packed record               { Config description }
    szNodeName      : DBINAME;          { Node name }
    szDescription   : packed array [0..DBIMAXSCFLDLEN-1] of AnsiChar; { Node description }
    iDataType       : Word;             { Value type }
    szValue         : packed array [0..DBIMAXSCFLDLEN-1] of AnsiChar; { Value }
    bHasSubnodes    : WordBool;         { True, if not leaf node }
  end;


{============================================================================}
{                    Family descriptor                                       }
{============================================================================}


type
  pFMLType = ^FMLType;
  FMLType  = (                          { Family member types }
    fmlUNKNOWN,
    fmlTABLE,
    fmlINDEX,
    fmlFORM,
    fmlREPORT,
    fmlVALCHECK,
    fmlSECINDEX,
    fmlSECINDEX2,
    fmlBLOBFILE
  );

{ DbiOpenFamilyList }
type
  pFMLDesc = ^FMLDesc;
  FMLDesc = packed record               { Family record structure }
    szName          : DBINAME;          { Member name (documentary) }
    iId             : Word;             { Id (if applicable) }
    eType           : FMLType;          { Member type }
    szFileName      : DBIPATH;          { File name of member }
  end;


{============================================================================}
{                    Language driver descriptor                              }
{============================================================================}


const
  DBIOEM_CP          = 1;               { (dos) }
  DBIANSI_CP         = 2;               { (win) }
  DBIOS2_CP          = 3;               { (OS2) }
(* UNIX etc. *)
  DBISUNOS_CP        = 4;
  DBIVMS_CP          = 5;
  DBIHPUX_CP         = 6;
  DBIULTRIX_CP       = 7;
  DBIAIX_CP          = 8;
  DBIAUX_CP          = 9;
  DBIXENIX_CP        = 10;
  DBIMAC_CP          = 11;
  DBINEXT_CP         = 12;
  DBIUNICODE_CP      = 13;
  DBIROMEN8_CP       = 14;
  DBIISO_CP          = 15;

{ DbiOpenLdList }
type
  pLDDesc = ^LDDesc;
  LDDesc = packed record                { Lang Driver description }
    szName          : DBINAME;          { Driver symbolic name }
    szDesc          : DBINAME;          { Description }
    iCodePage       : Word;
    PrimaryCpPlatform : Word;
    AlternateCpPlatform : Word;
  end;

{============================================================================}
{                    Lock descriptor                                         }
{============================================================================}

{ Lock types in LOCKDesc: }

const
  lckRECLOCK         = 0;               { Normal Record lock (Write) }
  lckRRECLOCK        = 1;               { Special Pdox Record lock (Read) }
  lckGROUPLOCK       = 2;               { Pdox Group lock }
  lckIMGAREA         = 3;               { Pdox Image area }
  lckTABLEREG        = 4;               { Table registration/Open (No lock) }
  lckTABLEREAD       = 5;               { Table Read lock }
  lckTABLEWRITE      = 6;               { Table Write lock }
  lckTABLEEXCL       = 7;               { Table Exclusive lock }
  lckUNKNOWN         = 9;               { Unknown lock }

{ DbiOpenLockList }
type
  pLOCKDesc = ^LOCKDesc;
  LOCKDesc = packed record              { Lock Description }
    iType           : Word;             { Lock type (0 for rec lock) }
    szUserName      : DBIUSERNAME;      { Lock owner }
    iNetSession     : Word;             { Net level Session number }
    iSession        : Word;             { Idapi session#, if our lock }
    iRecNum         : Longint;          { If a record lock }
    iInfo           : Word;             { Info for table locks }
  end;

{============================================================================}
{                    Filter description                                      }
{============================================================================}

type
  pCANOp = ^CANOp;
  CANOp  = (
    canNOTDEFINED,                      {                                  (*) }
    canISBLANK,                         { CANUnary;  is operand blank.     (*) }
    canNOTBLANK,                        { CANUnary;  is operand not blank. (*) }
    canEQ,                              { CANBinary, CANCompare; equal.    (*) }
    canNE,                              { CANBinary; NOT equal.            (*) }
    canGT,                              { CANBinary; greater than.         (*) }
    canLT,                              { CANBinary; less than.            (*) }
    canGE,                              { CANBinary; greater or equal.     (*) }
    canLE,                              { CANBinary; less or equal.        (*) }
    canNOT,                             { CANUnary; NOT                    (*) }
    canAND,                             { CANBinary; AND                   (*) }
    canOR,                              { CANBinary; OR                    (*) }
    canTUPLE2,                          { CANUnary; Entire record is operand. }
    canFIELD2,                          { CANUnary; operand is field       (*) }
    canCONST2,                          { CANUnary; operand is constant    (*) }
    canMINUS,                           { CANUnary;  minus. }
    canADD,                             { CANBinary; addition. }
    canSUB,                             { CANBinary; subtraction. }
    canMUL,                             { CANBinary; multiplication. }
    canDIV,                             { CANBinary; division. }
    canMOD,                             { CANBinary; modulo division. }
    canREM,                             { CANBinary; remainder of division. }
    canSUM,                             { CANBinary, accumulate sum of. }
    canCOUNT,                           { CANBinary, accumulate count of. }
    canMIN,                             { CANBinary, find minimum of. }
    canMAX,                             { CANBinary, find maximum of. }
    canAVG,                             { CANBinary, find average of. }
    canCONT,                            { CANBinary; provides a link between two }
    canUDF2,                            { CANBinary; invokes a User defined fn }
    canCONTINUE2,                       { CANUnary; Stops evaluating records }
    canLIKE,                            { CANCompare, extended binary compare       (*) }
    canIN,                              { CANBinary field in list of values }
    canLIST2,                           { List of constant values of same type }
    canUPPER,                           { CANUnary: upper case }
    canLOWER,                           { CANUnary: lower case }
    canFUNC2,                           { CANFunc: Function }
    canLISTELEM2,                       { CANListElem: List Element }
    canASSIGN                           { CANBinary: Field assignment }
  );

  NODEClass = (                         { Node Class }
    nodeNULL,                           { Null node                  (*) }
    nodeUNARY,                          { Node is a unary            (*) }
    nodeBINARY,                         { Node is a binary           (*) }
    nodeCOMPARE,                        { Node is a compare          (*) }
    nodeFIELD,                          { Node is a field            (*) }
    nodeCONST,                          { Node is a constant         (*) }
    nodeTUPLE,                          { Node is a record }
    nodeCONTINUE,                       { Node is a continue node    (*) }
    nodeUDF,                            { Node is a UDF node }
    nodeLIST,                           { Node is a LIST node }
    nodeFUNC,                           { Node is a Function node }
    nodeLISTELEM                        { Node is a List Element node }
  );

{ NODE definitions including misc data structures }
{-------------------------------------------------}

type
  pCANHdr = ^CANHdr;
  CANHdr = packed record                { Header part common to all     (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
  end;

  pCANUnary = ^CANUnary;
  CANUnary = packed record              { Unary Node                    (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOperand1       : Word;             { Byte offset of Operand node }
  end;

  pCANBinary = ^CANBinary;
  CANBinary = packed record             { Binary Node                   (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANField = ^CANField;
  CANField = packed record              { Field }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iFieldNum       : Word;
    iNameOffset     : Word;             { Name offset in Literal pool }
  end;

  pCANConst = ^CANConst;
  CANConst = packed record              { Constant }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iType           : Word;             { Constant type. }
    iSize           : Word;             { Constant size. (in bytes) }
    iOffset         : Word;             { Offset in the literal pool. }
  end;

  pCANTuple = ^CANTuple;
  CANTuple = packed record              { Tuple (record) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iSize           : Word;             { Record size. (in bytes) }
  end;

  pCANContinue = ^CANContinue;
  CANContinue = packed record           { Break Node                    (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iContOperand    : Word;             { Continue if operand is true. }
  end;

  pCANCompare = ^CANCompare;
  CANCompare = packed record            { Extended compare Node (text fields) (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;            { canLIKE, canEQ }
    bCaseInsensitive : WordBool;        { 3 val: UNKNOWN = "fastest", "native" }
    iPartialLen     : Word;             { Partial fieldlength (0 is full length) }
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANFunc = ^CANFunc;
  CANFunc = packed record               { Function }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iNameOffset     : Word;             { Name offset in Literal pool }
    iElemOffset     : Word;             { Offset of first List Element in Node pool }
  end;

  pCANListElem = ^CANListElem;
  CANListElem = packed record           { List Element }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOffset         : Word;             { Arg offset in Node pool }
    iNextOffset     : Word;             { Offset in Node pool of next ListElem or 0 if end of list }
  end;

{This is the node to be used to pass User defined functions }
const
  iLangSQL           = 0;               { Common SQL dialect }
  iDbaseExpr         = 2;               { This is also the driver ID for dBASE }

type
  pCANUdf = ^CANUdf;
  CANUdf = packed record                { A user defined function }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOffSzFuncName  : Word;             { Offset in literal pool to Function Name string(0 terminated) }
    iOperands       : Word;             { Byte offset of Operands (concatenated using canCONT) }
    iDrvDialect     : Word;             { Driver Dialect ID for UDF string supplied }
    iOffSzUDF       : Word;             { Offset in literal pool to UDF string (0 terminated) }
  end;

  pCANList = ^CANList;
  CANList = packed record           { List of Constants }
    nodeClass       : NODEClass; 
    canOp           : CANOp;
    iType           : Word;            { Constant type. }
    iTotalSize      : Word;            { Total list size; }
    iElemSize       : Word;            { Size of each elem for fix-width types }
    iElems          : Word;            { Number of elements in list }
    iOffset         : Word;            { Offset in the literal pool to first elem. }
  end;

  pCANNode = ^CANNode;
  CANNode = packed record
    case Integer of
      0: (canHdr      : CANHdr);
      1: (canUnary    : CANUnary);
      2: (canBinary   : CANBinary);
      3: (canField    : CANField);
      4: (canConst    : CANConst);
      5: (canTuple    : CANTuple);
      6: (canContinue : CANContinue);
      7: (canCompare  : CANCompare);
      8: (canList     : CANList);
      9: (canFunc     : CANFunc);
     10: (canListElem : CANListElem);
  end;

{ Linear exression tree}
{----------------------}

const
  CANEXPRVERSION     = 2;

type
  ppCANExpr = ^pCANExpr;
  pCANExpr  = ^CANExpr;
  CANExpr   = packed record             { Expression Tree }
    iVer            : Word;             { Version tag of expression. }
    iTotalSize      : Word;             { Size of this structure }
    iNodes          : Word;             { Number of nodes }
    iNodeStart      : Word;             { Starting offet of Nodes in this }
    iLiteralStart   : Word;             { Starting offset of Literals in this }
  end;

{pfGENFilter returns TRUE, FALSE or ABORT }
const
  ABORT              = -2;

type
  pfGENFilter = function (
      ulClientData  : Longint;
      pRecBuf       : Pointer;
      iPhyRecNum    : Longint
   ): SmallInt stdcall;

  pFILTERInfo = ^FILTERInfo;
  FILTERInfo = packed record
    iFilterId       : Word;             { Id for filter }
    hFilter         : hDBIFilter;       { Filter handle }
    iClientData     : Longint;          { Client supplied data }
    iPriority       : Word;             { 1..N with 1 being highest }
    bCanAbort       : WordBool;         { TRUE : pfFilter can return ABORT }
    pfFilter        : pfGENFilter;      { Client filter function }
    pCanExpr        : Pointer;          { Supplied expression }
    bActive         : WordBool;         { TRUE : filter is active }
  end;

{----------------------------------------------------------------------------}
{   DBI Query related types                                                  }
{----------------------------------------------------------------------------}

const
  MAXQBEEXPRSIZE     = 300;             { size of one QBE expr }

type
  pDBIQryProp = ^DBIQryProp;
  DBIQryProp = packed record
    szQryName       : DBINAME;          { Name of query }
    eLang           : DBIQryLang;       { Language }
    iQryPrice       : SmallInt;         { Query price 1..100 (1 = cheap, 100 = expensive) }
    iNumTables      : SmallInt;         { Number of tables in join.  0 = unknown. }
    bHasAnswer      : WordBool;
    bInsert         : WordBool;
    bDelete         : WordBool;
    bChange         : WordBool;
  end;

const
  DBIQBE_ANSWERBIT   = ($1);            { Answer table bit flag }
  DBIQBE_INSERTEDBIT = ($2);            { Inserted table bit flag }
  DBIQBE_DELETEDBIT  = ($4);            { Deleted table bit flag }
  DBIQBE_CHANGEDBIT  = ($8);            { Changed table bit flag }
  DBIQBE_ERRORINSBIT = ($10);           { Error inserted table bit flag }
  DBIQBE_ERRORDELBIT = ($20);           { Error deleted table bit flag }
  DBIQBE_ERRORCHGBIT = ($40);           { Error changed table bit flag }


{ answer cursor properties: }

  bAnsHasLiveFields  = $1;
  bAnsHasFilter      = $2;
  bAnsHasFieldMap    = $4;
  bAnsHasCalcField   = $8;
  bAnsHasLiveBlob    = $10;

{ answer field properties: }

  bIsAnsFieldLive    = $1;

type
  DBIQryType = (
    dbiqryDEFAULT,
    dbiqryDIRTY,
    dbiqryCLEAN,
    dbiqryRESTART
  );

  pDBIQryProgress = ^DBIQryProgress;
  DBIQryProgress = packed record
    stepsInQry      : Word;             { Total steps in query. }
    stepsCompleted  : Word;             { Steps completed out of total (steps may be skipped). }
    totElemInStep   : Longint;          { Total elements in current step. }
    elemCompleted   : Longint;          { Elements completed in current step. }
  end;

  QryEvalMode = (
    qryModeNONE,                        { Reserved }
    qryModeLOCAL,
    qryModeSERVER,
    qryModeEITHER,
    qryModeNOWLOCAL                     { used only in call back, when failed on server }
  );

{ values for client indicating live/canned preference about query execution }

  LIVENESS = (
    wantDEFAULT,                        { Default , same as wantCANNED }
    wantLIVE,                           { Want live data even if extra effort (no guarantee) }
    wantCANNED,                         { Want canned data even if extra effort (guaranteed) }
    wantSPEED                           { Let query manager decide, find out afterwards }
  );

  pQueryLowProps = ^QueryLowProps;
  QueryLowProps = packed record
    length          : SmallInt;         { Length in bytes of this structure }
    blankzeroes     : WordBool;         { TRUE if blanks to be regarded as zeros }
    dateFormat      : FMTDate;          { Date format }
    numberFormat    : FMTNumber;        { Number format }
    bNeedAuxTbls    : WordBool;         { If FALSE, don't bother with DELETED/ERRDEL, etc. }
    qryMode         : QryEvalMode;      { qryModeSERVER, qryModeLOCAL or qryModeEITHER. }
    perQrySqlMode   : WordBool;
    livenessDesired : LIVENESS;
  end;

{============================================================================}
{                      DBI symbols                                           }
{============================================================================}

const
  DBIMOD_BEGIN       = ($3F00);

  DBIMOD_QBE         = (DBIMOD_BEGIN + 1);
  DBIMOD_SQLG        = (DBIMOD_BEGIN + 2);
  DBIMOD_LEGO        = (DBIMOD_BEGIN + 3);
  DBIMOD_LOCKMNGR    = (DBIMOD_BEGIN + 4);
  DBIMOD_SQLDRIVER   = (DBIMOD_BEGIN + 5);
  DBIMOD_OS          = (DBIMOD_BEGIN + 6);
  DBIMOD_DBASEDRV    = (DBIMOD_BEGIN + 7);
  DBIMOD_CDR         = (DBIMOD_BEGIN + 8);

  DBIMOD_END         = (DBIMOD_BEGIN + 9);

{----------------------------------------------------------------------------}

  DBISYM_BEGIN       = (DBIMOD_END + 1);

  DBISYM_TOKEN       = (DBISYM_BEGIN + ecTOKEN);
  DBISYM_TABLENAME   = (DBISYM_BEGIN + ecTABLENAME);
  DBISYM_FIELDNAME   = (DBISYM_BEGIN + ecFIELDNAME);
  DBISYM_IMAGEROW    = (DBISYM_BEGIN + ecIMAGEROW);
  DBISYM_USERNAME    = (DBISYM_BEGIN + ecUSERNAME);
  DBISYM_FILENAME    = (DBISYM_BEGIN + ecFILENAME);
  DBISYM_INDEXNAME   = (DBISYM_BEGIN + ecINDEXNAME);
  DBISYM_DIRNAME     = (DBISYM_BEGIN + ecDIRNAME);
  DBISYM_KEYNAME     = (DBISYM_BEGIN + ecKEYNAME);
  DBISYM_ALIAS       = (DBISYM_BEGIN + ecALIAS);
  DBISYM_DRIVENAME   = (DBISYM_BEGIN + ecDRIVENAME);
  DBISYM_NATIVECODE  = (DBISYM_BEGIN + ecNATIVECODE);
  DBISYM_NATIVEMSG   = (DBISYM_BEGIN + ecNATIVEMSG);
  DBISYM_LINENUMBER  = (DBISYM_BEGIN + ecLINENUMBER);
  DBISYM_CAPABILITY  = (DBISYM_BEGIN + ecCAPABILITY);
  DBISYM_CDRNAME     = (DBISYM_BEGIN + ecCDRNAME);
  DBISYM_USERERRMSG  = (DBISYM_BEGIN + ecUSERERRMSG);
  DBISYM_DROBJNAME   = (DBISYM_BEGIN + ecDROBJNAME);
  DBISYM_INTERNALLIMIT = (DBISYM_BEGIN + ecINTERNALLIMIT);
  DBISYM_EXPRESSION  = (DBISYM_BEGIN + ecEXPRESSION);

  DBISYM_BASEEND     = (DBISYM_BEGIN + 100);

{----------------------------------------------------------------------------}

  DBISYM_MISC        = (DBISYM_BASEEND + 1);

  DBISYM_WORK        = (DBISYM_MISC + 1);
  DBISYM_PRIV        = (DBISYM_MISC + 2);
  DBISYM_COPY        = (DBISYM_MISC + 3);
  DBISYM_APPEND      = (DBISYM_MISC + 4);
  DBISYM_TXTPROBFLD1 = (DBISYM_MISC + 5);
  DBISYM_TXTPROBFLD2 = (DBISYM_MISC + 6);
  DBISYM_TXTPROBFLD3 = (DBISYM_MISC + 7);


  DBISYM_END         = (DBIMOD_BEGIN + $3FFF);

{============================================================================}
{                    SQL parsing                                             }
{============================================================================}

type
  SQLType = type TypedEnum;             { Object type. }
const
    RELTYPE   = 1;                      { Relation type. }
    COLTYPE   = 2;                      { Column type. }
    CONSTYPE  = 3;                      { Constant type. }
    EXPRTYPE  = 4;                      { Expression type. }
    AGGRTYPE  = 5;                      { Aggregate type. }
    CELLTYPE  = 6;                      { Cell type. }

type
  REQType = TypedEnum;
const
  REQSELECT      = 1;                    { Select. }
  REQSELECTALL   = 2;                    { Select All. }
  REQSELECTDIS   = 3;                    { Select Distinct. }
  REQSELECTDEC   = 4;                    { Select Decending. }
  REQSELECTCOUNT = 5;                    { Select count(*) }
  REQDDL         = 6;                    { DDL, create, drop etc. }
  REQINSERT      = 7;                    { Insert. }
  REQUPDATE      = 8;                    { Update. }
  REQDELETE      = 9;                    { Delete. }

  NAMELENGTH = DBIMAXPATHLEN;

type
  pSQLObject = ^SQLObject;

  pLIST = ^LIST;
  LIST = packed record
    car: pSQLObject;                    { SQL Object }
    cdr: pLIST;                         { Next }
  end;

  pSQLRelation = ^SQLRelation;
  SQLRelation = packed record        { Relation Name structure. }
    esType: SQLType;                    { value 1 for Relation type. }
    erType: REQType;                    { Request type, used for t.*. }
    szRelName: packed array[0..NAMELENGTH] of AnsiChar;{ Relation Name. }
    bRelQuote: Bool;                    { TRUE = Relation Name quoted. }
    szRelType: packed array[0..NAMELENGTH] of AnsiChar;{ Relation Type <Paradox,dBASE,etc> }
    szRelAlias: packed array[0..NAMELENGTH] of AnsiChar;{ Relation Alias. }
    szRelOwner: packed array[0..NAMELENGTH] of AnsiChar;{ Relation Owner. }
    plCol: pLIST;                       { List of Columns in table. }
    iRowId: Smallint;                   { Used for Self Joins. }
    iRowNo: Smallint;                   { Number of rows, 0 based. }
  end;

  CHECKType = (                       { Request type. }
    NOCHECK,                            { No Check. }
    chkCHECK,                           { Check. }
    chkCHECKPLUS,                       { Check plus. }
    chkCHECKDESC                        { Check descending. }
  );

  pSQLColumn = ^SQLColumn;
  SQLColumn = packed record          { Column Name structure. }
    esType: SQLType;                     { value 2 for Column type. }
    szColName: packed array[0..NAMELENGTH] of AnsiChar;{ Column Name. }
    bColQuote: Bool;                      { TRUE = Column Name quoted. }
    szColAlias: packed array[0..NAMELENGTH] of AnsiChar;{ Column Alias, if exists. }
    bColAlQuote: Bool;                    { TRUE = Alias Name quoted. }
    szColRelation: packed array[0..NAMELENGTH] of AnsiChar;{ Relation associated by alias. }
    szColOwner: packed array[0..NAMELENGTH] of AnsiChar;{ Owner of table. }
    eCheck: CHECKType;                   { Check Mark. }
    paiRowCheck: PWord;                  { Array of rows which have checks. }
    iColAggr: Smallint;                  { Column Projected Aggregate. }
    iColExmpl: Integer;                  { Column Example Element. }
    plCell: pLIST;                       { List of Cells for each column. }
  end;

  pSQLConstant = ^SQLConstant;
  SQLConstant = packed record        { Constant structure. }
    esType: SQLType;                    { value 3 for Constant type. }
    szConstValue: packed array[0..NAMELENGTH] of AnsiChar;{ Constant Value. }
  end;

  SQLOp = (opUNKNOWNOP, opEQUAL, opNOTEQ, opLESSEQUAL, opGREQUAL, opLESS,
    opGREATER, opLIKE, opNOT, opPLUSX, opMINUSX, opTIMESX, opDIVX, opAND, opOR,
    opBLANK, opAS, opOUTERJOIN, opNOTLIKE, opCHANGETO, opUPPER, opLOWER, opTRIM,
    opSUBSTRING, opPAND, opPOR, opPNOT, opEXTRACT, opFULLOTRJOIN);

  pSQLExpr = ^SQLExpr;
  SQLExpr = packed record            { Expression structure. }
    esType: SQLType;                    { value 4 for Expression type. }
    eOp: SQLOp;                         { Operator for the Objects. }
    plOperands: pLIST;                  { Operands, Objects. }
  end;

  pSQLAggr = ^SQLAggr;
  SQLAggr = packed record            { Aggregate structure. }
    esType: SQLType;                    { value 5 for Aggr. }
    pcolAggr: pSQLColumn;               { Column where aggr occurs. }
    iAggr: Smallint;                    { Type of Aggr. }
    bProjAggr: Bool;                    { Is a Projection Aggr.? }
    pAggrName: pSQLConstant;            { New Aggr's field name. }
  end;

  pSQLCell = ^SQLCell;
  SQLCell = packed record            { Cell structure. }
    esType: SQLType;                    { Value 6 for Cell type. }
    szCellValue: packed array[0..NAMELENGTH] of AnsiChar;{ Cell Value. }
    iRow: Integer;                      { Row number of cell value, 0 based. }
  end;

  SQLObject = packed record
    case Integer of
     0: (esType: SQLType);
     1: (pRel: SQLRelation);   {  RELTYPE   (1)  Relation Information. }
     2: (pCol: SQLColumn);     {  COLTYPE   (2)  Column Information. }
     3: (pConst: SQLConstant); {  CONSTYPE  (3)  Constant Information. }
     4: (pExpr: SQLExpr);      {  EXPRTYPE  (4)  Expression Information. }
     5: (pAggr: SQLAggr);      {  AGGRTYPE  (5)  Aggregate Information. }
     6: (pCell: SQLCell);      {  CELLTYPE  (6)  Cell Information. }
  end;

const
  REQVERSION         = 3;                { Version of request structure. }

type
  PSQLRequest = ^TSQLRequest;
  TSQLRequest = packed record           { SQL request. }
    iVersion        : Word;             { Version of request. }
    iMemMark        : Integer;          { Memory Mark to free request. }
    hQry            : hDBIQry;          { Query handle for memory mark. }
    erType          : REQType;          { Request type. }
    hDb             : hDBIDb;           { Database handle. }
    hStmt           : hDBIStmt;         { Statement handle used for parameter binding. }
    iParams         : Integer;          { # of parameters found in SQL statement. }
    iOptions        : Integer;          { Used for determining which options to set. }
    iDistinct       : Integer;          { Has "DISTINCT" been specified. }
    plRelation      : pLIST;            { List of SQLRelation. }
    plProject       : pLIST;            { List of SQLColumn. }
    plWhere         : pLIST;            { List of SQLExpr. }
    plOrder         : pLIST;            { List of SQLColumn. }
    plGroupBy       : pLIST;            { List of SQLColumn. }
    plHaving        : pLIST;            { List of SQLObjects. }
    plAlias         : pLIST;            { List of SQLConstant to represent aliases used. }
   end;

{============================================================================}
{                      DBI Config symbols                                    }
{============================================================================}

{ Categories }

const
  szCFGSYSTEM        = 'SYSTEM';
  szCFGDRIVER        = 'DRIVERS';
  szCFGDATABASE      = 'DATABASES';
  szCFGREPOSITORY    = 'REPOSITORIES';

{----------------------------------------------------------------------------}
{ System Fields                                                              }
{----------------------------------------------------------------------------}

  szCFGSYSVERSION    = 'VERSION';
  szCFGSYSNETTYPE    = 'NET TYPE';
  szCFGSYSNETDIR     = 'NET DIR';
  szCFGSYSLOCALSHARE = 'LOCAL SHARE';
  szCFGSYSLANGDRV    = 'LANGDRIVER';
  szCFGSYSLANGDRVDIR = 'LANGDRVDIR';
  szCFGSYSMINBUF     = 'MINBUFSIZE';
  szCFGSYSMAXBUF     = 'MAXBUFSIZE';
  szCFGSYSLOCKRETRY  = 'LOCKRETRY';
  szCFGSYSFLAGS      = 'SYSFLAGS';
  szCFGMAXFILEHANDLES = 'MAXFILEHANDLES';
  szCFGSQLQRYMODE    = 'SQLQRYMODE';
  szCFGLOWMEMLIMIT   = 'LOW MEMORY USAGE LIMIT'; { Use this instead of NOLOWMEMBUF }
  szCFGSYSODBCCFGIMPORT = 'AUTO ODBC';
  szCFGAUTOODBC      = 'AUTO ODBC';
  szCFGDEFDRV        = 'DEFAULT DRIVER';
  szCFGSYSLOCALREPOSITORY = 'DATA REPOSITORY';
  szCFGSYSMEMSIZE    = 'MEMSIZE';
  szCFGSYSSHAREDMEMSIZE = 'SHAREDMEMSIZE';
  szCFGSYSSHAREDMEMLOCATION = 'SHAREDMEMLOCATION';
  szCFGSYSMTSPOOLING = 'MTS POOLING';

{----------------------------------------------------------------------------}
{ Driver Fields                                                              }
{----------------------------------------------------------------------------}

  szCFGDRVVERSION    = 'VERSION';
  szCFGDRVTYPE       = 'TYPE';
  szCFGDRVDLL        = 'DLL';
  szCFGDRVDLL32      = 'DLL32';
  szCFGDRVFLAGS      = 'DRIVER FLAGS';
  szCFGDRVLANGDRIVER = 'LANGDRIVER';
  szCFGDRVFILLFACTOR = 'FILL FACTOR';
  szCFGDRVBLOCKSIZE  = 'BLOCK SIZE';
  szCFGDRVLOCKPROTOCOL = 'LOCKPROTOCOL';
  szCFGDRVLEVEL      = 'LEVEL';
  szCFGDRVVENDINIT   = 'VENDOR INIT';
  szCFGDRVTRACEMODE  = 'TRACE MODE';

{----------------------------------------------------------------------------}
{ Dbase Driver fields                                                        }
{----------------------------------------------------------------------------}

  szCFGDRVMEMOBLOCKSIZE = 'MEMO FILE BLOCK SIZE';
  szCFGDRVMDXBLOCKSIZE = 'MDX BLOCK SIZE';


{----------------------------------------------------------------------------}
{ Driver Nodes                                                               }
{----------------------------------------------------------------------------}

  szCFGDRVINIT       = 'INIT';
  szCFGDBCREATE      = 'DB CREATE';
  szCFGDBOPEN        = 'DB OPEN';
  szCFGTBLCREATE     = 'TABLE CREATE';
  szCFGTBLOPEN       = 'TABLE OPEN';

{----------------------------------------------------------------------------}
{ Database Nodes                                                             }
{----------------------------------------------------------------------------}

  szCFGDBINFO        = 'DB INFO';

{----------------------------------------------------------------------------}
{ Database fields                                                            }
{----------------------------------------------------------------------------}

  szCFGDBTYPE        = 'TYPE';
  szCFGDBPATH        = 'PATH';
  szCFGDBDEFAULTDRIVER = 'DEFAULT DRIVER';
  szCFGDBENABLEBCD   = 'ENABLE BCD';

{----------------------------------------------------------------------------}
{ Others                                                                     }
{----------------------------------------------------------------------------}

  szCFGINIT          = 'INIT';
  szTYPe             = 'TYPE';  { Changed from szTYPE to avoid urlmon.h conflict }
  szCFGDBSTANDARD    = 'STANDARD';
  szCFGTRUE          = 'TRUE';
  szCFGFALSE         = 'FALSE';
  szOPENMODE         = 'OPEN MODE';
  szREADWRITE        = 'READ/WRITE';
  szREADONLy         = 'READ ONLY';  { Changed from szREADONLY to avoid DSIntf.hpp conflict }
  szSHAREMODE        = 'SHARE MODE';
  szEXCLUSIVE        = 'EXCLUSIVE';
  szSHARED           = 'SHARED';
  szUSERNAME         = 'USER NAME';
  szPASSWORD         = 'PASSWORD';
  szSERVERNAME       = 'SERVER NAME';
  szDATABASENAME     = 'DATABASE NAME';
  szSCHEMASIZE       = 'SCHEMA CACHE SIZE';
  szCFGSTRICTINTEGRITY = 'STRICTINTEGRTY';
  szSQLPASSMODE      = 'SQLPASSTHRU MODE';
  szNOTSHARED        = 'NOT SHARED';
  szSHAREDAUTOCOMMIT = 'SHARED AUTOCOMMIT';
  szSHAREDNOAUTOCOMMIT = 'SHARED NOAUTOCOMMIT';
  szSCHEMATIME       = 'SCHEMA CACHE TIME';
  szMAXQUERYTIME     = 'MAX QUERY TIME';
  szMAXROWS          = 'MAX ROWS';
  szLISTSYNONYMS     = 'LIST SYNONYMS';
  szSYNNONE          = 'NONE';
  szSYNALL           = 'ALL';
  szSYNPRIVATE       = 'PRIVATE';
  szBATCHCOUNT       = 'BATCH COUNT';
  szBLOBCOUNT        = 'BLOBS TO CACHE'; 
  szBLOBSIZE         = 'BLOB SIZE';
  szOBJECTMODE       = 'OBJECT MODE';
  szENABLESCHEMACACHE= 'ENABLE SCHEMA CACHE';
  szSCHEMACACHEDIR   = 'SCHEMA CACHE DIR';
  szSYBLHOST         = 'HOST NAME';
  szSYBLAPP          = 'APPLICATION NAME';
  szSYBLNATLANG      = 'NATIONAL LANG NAME';
  szTDSPACKETSIZE    = 'TDS PACKET SIZE';
  szORAINTEGER       = 'ENABLE INTEGERS';
  szDBNLS            = 'DBNLS';
  szCOLLCHAR         = 'COLLCHAR';
  szROWSETSIZE       = 'ROWSET SIZE';
  szCFG30            = '5.0';
  szCFGSERVER        = 'SERVER';
  szCFGIDODBC01      = 'IDODBC01.DLL';
  szCFGIDODBC32      = 'IDODBC32.DLL';
  szCFGODBCDRIVER    = 'ODBC DRIVER';
  szCFGNULL          = '';
  szCFGZERO          = '0';
  szCFG20            = '20';
  szCFG64            = '64';
  szCFG32            = '32';
  szCFGODBCDSN       = 'ODBC DSN';
  szCFGTWOHUNDRED    = '200';
  szCFGNEGONE        = '-1';
  szCFGEIGHT         = '8';
  szCLSID            = 'CLSID';
  szCFGSYSTEMDB      = 'SYSTEM DATABASE';
  szCFGOLEDBPROVIDER = 'OLE DB PROVIDER';
  szCFGPERSISTFILE   = 'DATA SOURCE FILE';
  szCFGOBJECTMODE    = 'OBJECT MODE';

{----------------------------------------------------------------------------}
{ Repository fields                                                          }
{----------------------------------------------------------------------------}

  szCFGDRDBNAME      = 'DATABASE NAME';
  szCFGDRTBLNAME     = 'TABLE NAME';
  szCFGDRLANGDRIVER  = 'LANGUAGE DRIVER';
  szCFGDRDESC        = 'DESCRIPTION';

{----------------------------------------------------------------------------}
{ SYSTEM DATE/TIME/NUMBER FORMATS                                            }
{ SYSTEM nodes:                                                              }
{----------------------------------------------------------------------------}

  szCFGFORMAT        = 'FORMATS';

{----------------------------------------------------------------------------}
{ Format nodes:                                                              }
{----------------------------------------------------------------------------}

  szCFGDATE          = 'DATE';
  szCFGTIME          = 'TIME';
  szCFGNUMBER        = 'NUMBER';

{----------------------------------------------------------------------------}
{ DATE and/or TIME fields:                                                   }
{----------------------------------------------------------------------------}

  szCFGSEPARATOR     = 'SEPARATOR';
  szCFGMODE          = 'MODE';
  szCFGFOURDIGITYEAR = 'FOURDIGITYEAR';
  szCFGYEARBIASED    = 'YEARBIASED';
  szCFGLEADINGZEROM  = 'LEADINGZEROM';
  szCFGLEADINGZEROD  = 'LEADINGZEROD';
  szCFGTWELVEHOUR    = 'TWELVEHOUR';
  szCFGAMSTRING      = 'AMSTRING';
  szCFGPMSTRING      = 'PMSTRING';
  szCFGSECONDS       = 'SECONDS';
  szCFGMILSECONDS    = 'MILSECONDS';

{----------------------------------------------------------------------------}
{ Number fields:                                                             }
{----------------------------------------------------------------------------}

  szCFGDECIMALSEPARATOR = 'DECIMALSEPARATOR';
  szCFGTHOUSANDSEPARATOR = 'THOUSANDSEPARATOR';
  szCFGDECIMALDIGITS = 'DECIMALDIGITS';
  szCFGLEADINGZERON  = 'LEADINGZERON';

{ String resoure id's for each string listed above }

  DBICFG_BASE        = $3A00;

{----------------------------------------------------------------------------}
{ Categories                                                                 }
{----------------------------------------------------------------------------}

  iCFGSYSTEM         = (DBICFG_BASE + 1);
  iCFGDRIVER         = (DBICFG_BASE + 2);
  iCFGDATABASE       = (DBICFG_BASE + 3);
  iCFGREPOSITORY     = (DBICFG_BASE + 210);

{----------------------------------------------------------------------------}
{ System Fields                                                              }
{----------------------------------------------------------------------------}

  iCFGSYSVERSION     = (DBICFG_BASE + 5);
  iCFGSYSNETTYPE     = (DBICFG_BASE + 6);
  iCFGSYSNETDIR      = (DBICFG_BASE + 7);
  iCFGSYSLOCALSHARE  = (DBICFG_BASE + 8);
  iCFGSYSLANGDRV     = (DBICFG_BASE + 9);
  iCFGSYSLANGDRVDIR  = (DBICFG_BASE + 10);
  iCFGSYSMINBUF      = (DBICFG_BASE + 11);
  iCFGSYSMAXBUF      = (DBICFG_BASE + 12);
  iCFGSYSLOCKRETRY   = (DBICFG_BASE + 13);
  iCFGSYSFLAGS       = (DBICFG_BASE + 14);
  iCFGMAXFILEHANDLES = (DBICFG_BASE + 15);
  iCFGSQLQRYMODE     = (DBICFG_BASE + 16);
  iCFGLOWMEMLIMIT    = (DBICFG_BASE + 17);
  iCFGSYSODBCCFGIMPORT = (DBICFG_BASE + 18);
  iCFGSYSLOCALREPOSITORY = (DBICFG_BASE + 211);
  iCFGSYSSHAREDMEMSIZE = (DBICFG_BASE + 250);
  iCFGSYSSHAREDMEMLOCATION = (DBICFG_BASE + 251);
  iCFGSYSMEMSIZE     = DBICFG_BASE + 125;


{----------------------------------------------------------------------------}
{ Driver Fields                                                              }
{----------------------------------------------------------------------------}

  iCFGDRVVERSION     = (DBICFG_BASE + 20);
  iCFGDRVTYPE        = (DBICFG_BASE + 21);
  iCFGDRVLANGDRIVER  = (DBICFG_BASE + 22);
  iCFGDRVFILLFACTOR  = (DBICFG_BASE + 23);
  iCFGDRVBLOCKSIZE   = (DBICFG_BASE + 24);
  iCFGDRVLOCKPROTOCOL = (DBICFG_BASE + 25);
  iCFGDRVLEVEL       = (DBICFG_BASE + 26);
  iCFGDRVFLAGS       = (DBICFG_BASE + 27);
  iCFGDRVTRACEMODE   = (DBICFG_BASE + 28);
  iCFGDRVDLL32       = (DBICFG_BASE + 29);

{----------------------------------------------------------------------------}
{ Dbase Driver fields                                                        }
{----------------------------------------------------------------------------}

  iCFGDRVMEMOBLOCKSIZE = (DBICFG_BASE + 30 );
  iCFGDRVMDXBLOCKSIZE = (DBICFG_BASE + 31 );

{----------------------------------------------------------------------------}
{ Driver Nodes                                                               }
{----------------------------------------------------------------------------}

  iCFGDRVINIT        = (DBICFG_BASE + 40 );
  iCFGDBCREATE       = (DBICFG_BASE + 41 );
  iCFGDBOPEN         = (DBICFG_BASE + 42 );
  iCFGTBLCREATE      = (DBICFG_BASE + 43 );
  iCFGTBLOPEN        = (DBICFG_BASE + 44 );

{----------------------------------------------------------------------------}
{ Database Nodes                                                             }
{----------------------------------------------------------------------------}

  iCFGDBINFO         = (DBICFG_BASE + 50 );

{----------------------------------------------------------------------------}
{ Database fields                                                            }
{----------------------------------------------------------------------------}

  iCFGDBTYPE         = (DBICFG_BASE + 60);
  iCFGDBPATH         = (DBICFG_BASE + 61);
  iCFGDBDEFAULTDRIVER = (DBICFG_BASE + 62);
  iCFGDBENABLEBCD    = (DBICFG_BASE + 63);

{----------------------------------------------------------------------------}
{ Others                                                                     }
{----------------------------------------------------------------------------}

  iCFGINIT           = (DBICFG_BASE + 70);
  iTYPE              = (DBICFG_BASE + 71);
  iCFGDBSTANDARD     = (DBICFG_BASE + 72);
  iCFGTRUE           = (DBICFG_BASE + 73);
  iCFGFALSE          = (DBICFG_BASE + 74);
  iOPENMODE          = (DBICFG_BASE + 75);
  iREADWRITE         = (DBICFG_BASE + 76);
  iREADONLY          = (DBICFG_BASE + 77);
  iSHAREMODE         = (DBICFG_BASE + 78);
  iEXCLUSIVE         = (DBICFG_BASE + 79);
  iSHARED            = (DBICFG_BASE + 80);
  iUSERNAME          = (DBICFG_BASE + 81);
  iSERVERNAME        = (DBICFG_BASE + 82);
  iDATABASENAME      = (DBICFG_BASE + 83);
  iSCHEMASIZE        = (DBICFG_BASE + 84);
  iCFGSTRICTINTEGRITY = (DBICFG_BASE + 85);
  iTDSPACKETSIZE     = (DBICFG_BASE + 86);
  iORAINTEGER        = (DBICFG_BASE + 87);
  iDBNLS             = (DBICFG_BASE + 88);
  iCOLLCHAR          = (DBICFG_BASE + 89);
  { numbers 90-122 used in dbiext.h }
  iROWSETSIZE        = (DBICFG_BASE + 134);
  iCFGOBJECTMODE     = DBICFG_BASE + 135; 

{----------------------------------------------------------------------------}
{ Repository Nodes                                                           }
{----------------------------------------------------------------------------}

  iCFGDRDBNAME       = (DBICFG_BASE + 213);
  iCFGDRTBLNAME      = (DBICFG_BASE + 214);
  iCFGDRDESC         = (DBICFG_BASE + 215);
  iCFGDRLANGDRIVER   = (DBICFG_BASE + 212);

{----------------------------------------------------------------------------}
{ System node:                                                               }
{----------------------------------------------------------------------------}

  iCFGFORMAT         = (DBICFG_BASE + 130);

{----------------------------------------------------------------------------}
{ Format nodes:                                                              }
{----------------------------------------------------------------------------}

  iCFGDATE           = (DBICFG_BASE + 131);
  iCFGTIME           = (DBICFG_BASE + 132);
  iCFGNUMBER         = (DBICFG_BASE + 133);

{----------------------------------------------------------------------------}
{ DATE and/or TIME fields:                                                   }
{----------------------------------------------------------------------------}

  iCFGSEPARATOR      = (DBICFG_BASE + 140);
  iCFGMODE           = (DBICFG_BASE + 141);
  iCFGFOURDIGITYEAR  = (DBICFG_BASE + 142);
  iCFGYEARBIASED     = (DBICFG_BASE + 143);
  iCFGLEADINGZEROM   = (DBICFG_BASE + 144);
  iCFGLEADINGZEROD   = (DBICFG_BASE + 145);
  iCFGTWELVEHOUR     = (DBICFG_BASE + 146);
  iCFGAMSTRING       = (DBICFG_BASE + 147);
  iCFGPMSTRING       = (DBICFG_BASE + 148);
  iCFGSECONDS        = (DBICFG_BASE + 149);
  iCFGMILSECONDS     = (DBICFG_BASE + 150);

{----------------------------------------------------------------------------}
{ Number fields:                                                             }
{----------------------------------------------------------------------------}

  iCFGDECIMALSEPARATOR  = (DBICFG_BASE + 160);
  iCFGTHOUSANDSEPARATOR = (DBICFG_BASE + 161);
  iCFGDECIMALDIGITS     = (DBICFG_BASE + 162);
  iCFGLEADINGZERON      = (DBICFG_BASE + 163);

  iCFGDEFLANGDRV        = (DBICFG_BASE + 165);
  iCFGDBASEDEFLANGDRV   = (DBICFG_BASE + 166);

{----------------------------------------------------------------------------}
{ Formats                                                                    }
{----------------------------------------------------------------------------}

  iCFGDEFSEPARATOR          = (DBICFG_BASE + 170);
  iCFGDEFMODE               = (DBICFG_BASE + 171);
  iCFGDEFFOURDIGITYEAR      = (DBICFG_BASE + 172);
  iCFGDEFYEARBIASED         = (DBICFG_BASE + 173);
  iCFGDEFLEADINGZEROM       = (DBICFG_BASE + 174);
  iCFGDEFLEADINGZEROD       = (DBICFG_BASE + 175);
  iCFGDEFTWELVEHOUR         = (DBICFG_BASE + 176);
  iCFGDEFAMSTRING           = (DBICFG_BASE + 177);
  iCFGDEFPMSTRING           = (DBICFG_BASE + 178);
  iCFGDEFSECONDS            = (DBICFG_BASE + 179);
  iCFGDEFMILSECONDS         = (DBICFG_BASE + 180);
  iCFGDEFDECIMALSEPARATOR   = (DBICFG_BASE + 181);
  iCFGDEFTHOUSANDSEPARATOR  = (DBICFG_BASE + 182);
  iCFGDEFLEADINGZERO        = (DBICFG_BASE + 183);

  iCFGDEFVERSION            = (DBICFG_BASE + 184);
  iCFGDEFLOCALSHARE         = (DBICFG_BASE + 185);
  iCFGDEFMINBUFSIZE         = (DBICFG_BASE + 186);
  iCFGDEFMAXBUFSIZE         = (DBICFG_BASE + 187);
  iCFGDEFMAXFILEHANDLES     = (DBICFG_BASE + 188);
  iCFGDEFSYSFLAGS           = (DBICFG_BASE + 189);
  iCFGDEFLOWMEM             = (DBICFG_BASE + 190);
  iCFGDEFAUTOODBC           = (DBICFG_BASE + 191);
  iCFGDEFDEFDRV             = (DBICFG_BASE + 192);

  iCFGDEFDECIMALDIGITS      = (DBICFG_BASE + 193);
  iCFGDEFLEADINGZERON       = (DBICFG_BASE + 194);

  iCFGDEFPDXTYPE            = (DBICFG_BASE + 195);
  iCFGDEFPDXNETDIR          = (DBICFG_BASE + 196);
  iCFGDEFPDXLANGDRV         = (DBICFG_BASE + 197);
  iCFGDEFPDXLEVEL           = (DBICFG_BASE + 198);
  iCFGDEFPDXBLOCKSIZE       = (DBICFG_BASE + 199);
  iCFGDEFPDXFILLFACTOR      = (DBICFG_BASE + 200);
  iCFGDEFPDXSTRICTINTEGRTY  = (DBICFG_BASE + 201);

  iCFGDEFDBASETYPE          = (DBICFG_BASE + 202);
  iCFGDEFDBASELANGDRV       = (DBICFG_BASE + 203);
  iCFGDEFDBASELEVEL         = (DBICFG_BASE + 204);
  iCFGDEFDBASEMDXBLOCKSIZE  = (DBICFG_BASE + 205);
  iCFGDEFDBASEMEMOBLOCKSIZE = (DBICFG_BASE + 206);

  iCFGAUTOODBC              = (DBICFG_BASE + 207);
  iCFGDEFDRV                = (DBICFG_BASE + 208);
  iCFGENABLEBCD             = (DBICFG_BASE + 209);
  iCFGDEFSHAREDMEMSIZE      = (DBICFG_BASE + 252);
  iCFGDEFSHAREDMEMLOCATION  = (DBICFG_BASE + 253);
  iCFGDEFREPOSITORY         = (DBICFG_BASE + 254);
  iCFGDEFSQLQRYMODE         = (DBICFG_BASE + 255);
  iCFGDEFMEMSIZE            = (DBICFG_BASE + 126); 

{ MSACCESS default driver field values }
  iCFGDEFMSACCESSVER        = (DBICFG_BASE + 220);
  iCFGDEFMSACCESSTYPE       = (DBICFG_BASE + 221);
  iCFGDEFMSACCESSDLL32      = (DBICFG_BASE + 222);
  iCFGDEFMSACCESSLANGDRV    = (DBICFG_BASE + 223);
  iCFGDEFDATABASENAME       = (DBICFG_BASE + 224);
  iCFGDEFUSERNAME           = (DBICFG_BASE + 225);
  iCFGDEFTRACEMODE          = (DBICFG_BASE + 226);
  iCFGDEFDRVFLAGS           = (DBICFG_BASE + 227);
  iCFGCFGSYSTEMDB           = (DBICFG_BASE + 231);

{ FOXPRO default driver field values }
  iCFGDEFFOXPROTYPE         = (DBICFG_BASE + 228);
  iCFGDEFFOXPROLANGDRV      = (DBICFG_BASE + 229);
  iCFGDEFFOXPROLEVEL        = (DBICFG_BASE + 230);



  CFGHLP_BASE               = $3B00;

  iCFGHLP_SYSNODE           = (CFGHLP_BASE +1);
  iCFGHLP_SYSINITNODE       = (CFGHLP_BASE +2);
  iCFGHLP_SYSVERSION        = (CFGHLP_BASE +3);
  iCFGHLP_SYSLOCALSHARE     = (CFGHLP_BASE +4);
  iCFGHLP_SYSMINBUFSIZE     = (CFGHLP_BASE +5);
  iCFGHLP_SYSMAXBUFSIZE     = (CFGHLP_BASE +6);
  iCFGHLP_SYSLANGDRIVER     = (CFGHLP_BASE +7);
  iCFGHLP_SYSNETTYPE        = (CFGHLP_BASE +8);
  iCFGHLP_SYSFLAGS          = (CFGHLP_BASE +9);
  iCFGHLP_SYSMAXFILE        = (CFGHLP_BASE +10);
  iCFGHLP_SYSLOWMEM         = (CFGHLP_BASE +11);
  iCFGHLP_SYSAUTOODBC       = (CFGHLP_BASE +12);
  iCFGHLP_SYSDEFDRV         = (CFGHLP_BASE +13);
  iCFGHLP_SYSSQLQRYMODE     = (CFGHLP_BASE +14);
  iCFGHLP_SYSSQLPASSTHRU    = (CFGHLP_BASE +15);
  iCFGHLP_SYSFORMATNODE     = (CFGHLP_BASE +16);
  iCFGHLP_DATENODE          = (CFGHLP_BASE +17);
  iCFGHLP_DATESEPARATOR     = (CFGHLP_BASE +18);
  iCFGHLP_DATEMODE          = (CFGHLP_BASE +19);
  iCFGHLP_DATEFOURDIGIT     = (CFGHLP_BASE +20);
  iCFGHLP_DATEYEARBIASED    = (CFGHLP_BASE +21);
  iCFGHLP_DATEZEROM         = (CFGHLP_BASE +22);
  iCFGHLP_DATEZEROD         = (CFGHLP_BASE +23);
  iCFGHLP_TIMENODE          = (CFGHLP_BASE +24);
  iCFGHLP_TIMETWELVEHOUR    = (CFGHLP_BASE +25);
  iCFGHLP_TIMEAMSTRING      = (CFGHLP_BASE +26);
  iCFGHLP_TIMEPMSTRING      = (CFGHLP_BASE +27);
  iCFGHLP_TIMESECONDS       = (CFGHLP_BASE +28);
  iCFGHLP_TIMEMILSEC        = (CFGHLP_BASE +29);
  iCFGHLP_NUMNODE           = (CFGHLP_BASE +30);
  iCFGHLP_NUMDECIMALSEPARATOR = (CFGHLP_BASE +31);
  iCFGHLP_NUMTHOUSANDSEPARATOR = (CFGHLP_BASE +32);
  iCFGHLP_NUMDECIMALDIGITS  = (CFGHLP_BASE +33);
  iCFGHLP_NUMZERON          = (CFGHLP_BASE +34);
  iCFGHLP_DRVNODE           = (CFGHLP_BASE +35);
  iCFGHLP_PDXNODE           = (CFGHLP_BASE +36);
  iCFGHLP_PDXINITNODE       = (CFGHLP_BASE +37);
  iCFGHLP_DRVTYPE           = (CFGHLP_BASE +38);
  iCFGHLP_PDXNETDIR         = (CFGHLP_BASE +39);
  iCFGHLP_PDXTBLNODE        = (CFGHLP_BASE +40);
  iCFGHLP_PDXLEVEL          = (CFGHLP_BASE +41);
  iCFGHLP_PDXBLOCKSIZE      = (CFGHLP_BASE +42);
  iCFGHLP_PDXFILLFACTOR     = (CFGHLP_BASE +43);
  iCFGHLP_PDXSTRICT         = (CFGHLP_BASE +44);
  iCFGHLP_DBNODE            = (CFGHLP_BASE +45);
  iCFGHLP_DBINITNODE        = (CFGHLP_BASE +46);
  iCFGHLP_DBVERSION         = (CFGHLP_BASE +47);
  iCFGHLP_DBTBLNODE         = (CFGHLP_BASE +48);
  iCFGHLP_DBLEVEL           = (CFGHLP_BASE +49);
  iCFGHLP_DBMDXBLOCKSIZE    = (CFGHLP_BASE +50);
  iCFGHLP_DBMEMOFILEBLOCKSIZE = (CFGHLP_BASE +51);
  iCFGHLP_INTNODE           = (CFGHLP_BASE +52);
  iCFGHLP_INTINITNODE       = (CFGHLP_BASE +53);
  iCFGHLP_INTVERSION        = (CFGHLP_BASE +54);
  iCFGHLP_SQLDLL            = (CFGHLP_BASE +55);
  iCFGHLP_SQLDLL32          = (CFGHLP_BASE +56);
  iCFGHLP_SQLDRIVERFLAGS    = (CFGHLP_BASE +57);
  iCFGHLP_INTDBNODE         = (CFGHLP_BASE +58);
  iCFGHLP_SQLSERVER         = (CFGHLP_BASE +59);
  iCFGHLP_SQLUSER           = (CFGHLP_BASE +60);
  iCFGHLP_SQLOPENMODE       = (CFGHLP_BASE +61);
  iCFGHLP_SQLSCHEMASIZE     = (CFGHLP_BASE +62);
  iCFGHLP_SQLSCHEMATIME     = (CFGHLP_BASE +63);
  iCFGHLP_SYBNODE           = (CFGHLP_BASE +64);
  iCFGHLP_SYBINITNODE       = (CFGHLP_BASE +65);
  iCFGHLP_SYBVERSION        = (CFGHLP_BASE +66);
  iCFGHLP_SQLCONNECT        = (CFGHLP_BASE +67);
  iCFGHLP_SQLTIMEOUT        = (CFGHLP_BASE +68);
  iCFGHLP_SYBDBNODE         = (CFGHLP_BASE +69);
  iCFGHLP_SQLDATABASE       = (CFGHLP_BASE +70);
  iCFGHLP_SQLBLOBEDIT       = (CFGHLP_BASE +71);
  iCFGHLP_SQLMAXQUERY       = (CFGHLP_BASE +72);
  iCFGHLP_ORANODE           = (CFGHLP_BASE +73);
  iCFGHLP_ORAINITNODE       = (CFGHLP_BASE +74);
  iCFGHLP_ORAVERSION        = (CFGHLP_BASE +75);
  iCFGHLP_SQLVENDOR         = (CFGHLP_BASE +76);
  iCFGHLP_ORADBNODE         = (CFGHLP_BASE +77);
  iCFGHLP_SQLNETPROTOCOL    = (CFGHLP_BASE +78);
  iCFGHLP_MSSNODE           = (CFGHLP_BASE +79);
  iCFGHLP_MSSINITNODE       = (CFGHLP_BASE +80);
  iCFGHLP_MSSVERSION        = (CFGHLP_BASE +81);
  iCFGHLP_MSSDBNODE         = (CFGHLP_BASE +82);
  iCFGHLP_INFNODE           = (CFGHLP_BASE +83);
  iCFGHLP_INFINITNODE       = (CFGHLP_BASE +84);
  iCFGHLP_INFVERSION        = (CFGHLP_BASE +85);
  iCFGHLP_INFDBNODE         = (CFGHLP_BASE +86);
  iCFGHLP_SQLLOCKMODE       = (CFGHLP_BASE +87);
  iCFGHLP_SQLTRACEMODE      = (CFGHLP_BASE +88);
  iCFGHLP_SQLMAXROWS        = (CFGHLP_BASE +89);
  iCFGHLP_SQLBATCHCOUNT     = (CFGHLP_BASE +90);
  iCFGHLP_SYSSHAREDMEMSIZ   = (CFGHLP_BASE +91);
  iCFGHLP_SYSSHAREDMEMLOC   = (CFGHLP_BASE +92);
  iCFGHLP_SYSDATAREP        = (CFGHLP_BASE +93);
  iCFGHLP_ALIASTYPE         = (CFGHLP_BASE +94);
  iCFGHLP_ALIASPATH         = (CFGHLP_BASE +95);
  iCFGHLP_ALIASDEFDRV       = (CFGHLP_BASE +96);
  iCFGHLP_ENABLESCHEMACACHE = (CFGHLP_BASE +97);
  iCFGHLP_SCHEMACACHEDIR    = (CFGHLP_BASE +98);
  iCFGHLP_HOSTNAME          = (CFGHLP_BASE +99);
  iCFGHLP_APPLICATIONNAME   = (CFGHLP_BASE +100);
  iCFGHLP_NATIONALLANGNAME  = (CFGHLP_BASE +101);
  iCFGHLP_ALIASENABLEBCD    = (CFGHLP_BASE +102);
  iCFGHLP_TDSPACKETSIZE     = (CFGHLP_BASE +103);
  iCFGHLP_ORAINTEGER        = (CFGHLP_BASE +104);
  iCFGHLP_ORALISTSYNONYMS   = (CFGHLP_BASE +105);
  iCFGHLP_ROWSETSIZE        = (CFGHLP_BASE +106);
  iCFGHLP_DB2DRIVER         = (CFGHLP_BASE +107);
  iCFGHLP_DB2DSN            = (CFGHLP_BASE +108);
  iCFGHLP_DB2NODE           = (CFGHLP_BASE +109);
  iCFGHLP_DB2INITNODE       = (CFGHLP_BASE +110);
  iCFGHLP_DB2VERSION        = (CFGHLP_BASE +111);
  iCFGHLP_DB2DBNODE         = (CFGHLP_BASE +112);
  iCFGHLP_COLLCHAR          = (CFGHLP_BASE +113);
  iCFGHLP_DBNLS             = (CFGHLP_BASE +114);
  iCFGHLP_MSACCNODE         = (CFGHLP_BASE +115);
  iCFGHLP_MSACCINITNODE     = (CFGHLP_BASE +116);
  iCFGHLP_MSACCVERSION      = (CFGHLP_BASE +117);
  iCFGHLP_MSACCDBNODE       = (CFGHLP_BASE +118);
  iCFGHLP_ODBCDRIVER        = (CFGHLP_BASE +119);
  iCFGHLP_ODBCVERSION       = (CFGHLP_BASE +120);
  iCFGHLP_ODBCDSN           = (CFGHLP_BASE +121);
  iCFGHLP_OLEVERSION        = (CFGHLP_BASE +122);
  iCFGHLP_OLECLSID          = (CFGHLP_BASE +123);
  iCFGHLP_BLOBSTOCACHE      = (CFGHLP_BASE +124);
  iCFGHLP_BLOBSIZE          = (CFGHLP_BASE +125);
  iCFGHLP_SYSMEMSIZE        = (CFGHLP_BASE +126);
  iCFGHLP_MAXDBPROCS        = (CFGHLP_BASE +127);
  iCFGHLP_FPNODE            = (CFGHLP_BASE +128);
  iCFGHLP_FPINITNODE        = (CFGHLP_BASE +129);
  iCFGHLP_FPVERSION         = (CFGHLP_BASE +130);
  iCFGHLP_FPTBLNODE         = (CFGHLP_BASE +131);
  iCFGHLP_FPLEVEL           = (CFGHLP_BASE +132);
  iCFGHLP_FPMEMOFILEBLOCKSIZE= (CFGHLP_BASE +133);
  iCFGHLP_CFGSYSTEMDB       = (CFGHLP_BASE +134);
  iCFGHLP_OBJECTMODE        = (CFGHLP_BASE +135); 

{============================================================================}
{                            Error Categories                                }
{============================================================================}

function ErrCat(rslt: Word): Word;
function ErrCode(rslt: Word): Word;

const
  ERRCAT_NONE                   = 0;      {  0   No error }
  ERRCAT_SYSTEM                 = $21;    {  33  System related (Fatal Error) }
  ERRCAT_NOTFOUND               = $22;    {  34  Object of interest Not Found }
  ERRCAT_DATACORRUPT            = $23;    {  35  Physical Data Corruption }
  ERRCAT_IO                     = $24;    {  36  I/O related error }
  ERRCAT_LIMIT                  = $25;    {  37  Resource or Limit error }
  ERRCAT_INTEGRITY              = $26;    {  38  Integrity Violation }
  ERRCAT_INVALIDREQ             = $27;    {  39  Invalid Request }
  ERRCAT_LOCKCONFLICT           = $28;    {  40  Locking/Contention related }
  ERRCAT_SECURITY               = $29;    {  41  Access Violation - Security related }
  ERRCAT_INVALIDCONTEXT         = $2A;    {  42  Invalid context }
  ERRCAT_OS                     = $2B;    {  43  Os Error not handled by Idapi }
  ERRCAT_NETWORK                = $2C;    {  44  Network related }
  ERRCAT_OPTPARAM               = $2D;    {  45  Optional parameter related }
  ERRCAT_QUERY                  = $2E;    {  46  Query related }
  ERRCAT_VERSION                = $2F;    {  47  Version Mismatch Category }
  ERRCAT_CAPABILITY             = $30;    {  48  Capability not supported }
  ERRCAT_CONFIG                 = $31;    {  49  System configuration error }
  ERRCAT_WARNING                = $32;    {  50 }
  ERRCAT_OTHER                  = $33;    {  51  Miscellaneous }
  ERRCAT_COMPATIBILITY          = $34;    {  52  Compatibility related }
  ERRCAT_REPOSITORY             = $35;    {  53  Data Repository related }

  ERRCAT_DRIVER                 = $3E;    {  62  Driver related }
  ERRCAT_RC                     = $3F;    {  63  Internal }


  ERRBASE_NONE                  = 0;      { No error }
  ERRBASE_SYSTEM                = $2100;  { System related (Fatal Error) }
  ERRBASE_NOTFOUND              = $2200;  { Object of interest Not Found }
  ERRBASE_DATACORRUPT           = $2300;  { Physical Data Corruption }
  ERRBASE_IO                    = $2400;  { I/O related error }
  ERRBASE_LIMIT                 = $2500;  { Resource or Limit error }
  ERRBASE_INTEGRITY             = $2600;  { Integrity Violation }
  ERRBASE_INVALIDREQ            = $2700;  { Invalid Request }
  ERRBASE_LOCKCONFLICT          = $2800;  { Locking/Contention related }
  ERRBASE_SEC                   = $2900;  { Access Violation - Security related }
  ERRBASE_IC                    = $2A00;  { Invalid context }
  ERRBASE_OS                    = $2B00;  { Os Error not handled by Idapi }
  ERRBASE_NETWORK               = $2C00;  { Network related }
  ERRBASE_OPTPARAM              = $2D00;  { Optional Parameter related }
  ERRBASE_QUERY                 = $2E00;  { Query related }
  ERRBASE_VERSION               = $2F00;  { Version Mismatch Category }
  ERRBASE_CAPABILITY            = $3000;  { Capability not supported }
  ERRBASE_CONFIG                = $3100;  { System configuration error }
  ERRBASE_WARNING               = $3200;
  ERRBASE_OTHER                 = $3300;  { Miscellaneous }
  ERRBASE_COMPATIBILITY         = $3400;  { Compatibility related }
  ERRBASE_REPOSITORY            = $3500;  { Data Repository related }

  ERRBASE_DRIVER                = $3E00;  { Driver related }
  ERRBASE_RC                    = $3F00;  { Internal }


{=============================================================================}
{                           Error Codes By Category                           }
{=============================================================================}

{ ERRCAT_NONE                  (0) }
{ ===========                      }

  ERRCODE_NONE                  = 0;

  DBIERR_NONE                   = (ERRBASE_NONE + ERRCODE_NONE);

{  ERRCAT_SYSTEM }
{  ============= }

  ERRCODE_SYSFILEOPEN           = 1;      { Cannot open a system file }
  ERRCODE_SYSFILEIO             = 2;      { I/O error on a system file }
  ERRCODE_SYSCORRUPT            = 3;      { Data structure corruption }
  ERRCODE_NOCONFIGFILE          = 4;      { Cannot find config file }
  ERRCODE_CFGCANNOTWRITE        = 5;      { Cannot write config file (READONLY) }
  ERRCODE_CFGMULTIFILE          = 6;      { Initializing with different ini file }
  ERRCODE_REENTERED             = 7;      { System has been illegally re-entered }
  ERRCODE_CANTFINDIDAPI         = 8;      { Cannot locate IDAPIxx.DLL }
  ERRCODE_CANTLOADIDAPI         = 9;      { Cannot load IDAPIxx.DLL }
  ERRCODE_CANTLOADLIBRARY       = 10;     { Cannot load a service DLL }
  ERRCODE_TEMPFILEERR           = 11;     { Cannot create or open temporary file }
  ERRCODE_MULTIPLEIDAPI         = 12;     { Trying to load multiple IDAPIxx.DLL }
  ERRCODE_SHAREDMEMCONFLICT     = 13;     { Shared memory conflict. }

  DBIERR_SYSFILEOPEN            = (ERRBASE_SYSTEM + ERRCODE_SYSFILEOPEN);
  DBIERR_SYSFILEIO              = (ERRBASE_SYSTEM + ERRCODE_SYSFILEIO);
  DBIERR_SYSCORRUPT             = (ERRBASE_SYSTEM + ERRCODE_SYSCORRUPT);
  DBIERR_NOCONFIGFILE           = (ERRBASE_SYSTEM + ERRCODE_NOCONFIGFILE);
  DBIERR_CFGCANNOTWRITE         = (ERRBASE_SYSTEM + ERRCODE_CFGCANNOTWRITE);
  DBIERR_CFGMULTIFILE           = (ERRBASE_SYSTEM + ERRCODE_CFGMULTIFILE);
  DBIERR_REENTERED              = (ERRBASE_SYSTEM + ERRCODE_REENTERED);
  DBIERR_CANTFINDIDAPI          = (ERRBASE_SYSTEM + ERRCODE_CANTFINDIDAPI);
  DBIERR_CANTLOADIDAPI          = (ERRBASE_SYSTEM + ERRCODE_CANTLOADIDAPI);
  DBIERR_CANTLOADLIBRARY        = (ERRBASE_SYSTEM + ERRCODE_CANTLOADLIBRARY);
  DBIERR_TEMPFILEERR            = (ERRBASE_SYSTEM + ERRCODE_TEMPFILEERR);
  DBIERR_MULTIPLEIDAPI          = (ERRBASE_SYSTEM + ERRCODE_MULTIPLEIDAPI);
  DBIERR_SHAREDMEMCONFLICT      = (ERRBASE_SYSTEM + ERRCODE_SHAREDMEMCONFLICT);

  DBIERR_CANTFINDODAPI = DBIERR_CANTFINDIDAPI;
  DBIERR_CANTLOADODAPI = DBIERR_CANTLOADIDAPI;

{  ERRCAT_NOTFOUND }
{  =============== }

  ERRCODE_BOF                   = 1;      { Beginning of Virtual table }
  ERRCODE_EOF                   = 2;      { End of Virtual table }
  ERRCODE_RECMOVED              = 3;      { Fly-away }
  ERRCODE_KEYORRECDELETED       = 4;      { Record Deleted/Key Modified }
  ERRCODE_NOCURRREC             = 5;      { No current record }
  ERRCODE_RECNOTFOUND           = 6;      { Record was not found }
  ERRCODE_ENDOFBLOB             = 7;      { End of Blob reached }
  ERRCODE_OBJNOTFOUND           = 8;      { Generic Not found }
  ERRCODE_FMLMEMBERNOTFOUND     = 9;      { Family member not found }
  ERRCODE_BLOBFILEMISSING       = 10;     { 0x0a Blob file for table is missing }
  ERRCODE_LDNOTFOUND            = 11;     { 0x0b Language driver not found }

  DBIERR_BOF                    = (ERRBASE_NOTFOUND + ERRCODE_BOF);
  DBIERR_EOF                    = (ERRBASE_NOTFOUND + ERRCODE_EOF);
  DBIERR_RECMOVED               = (ERRBASE_NOTFOUND + ERRCODE_RECMOVED);
  DBIERR_RECDELETED             = (ERRBASE_NOTFOUND + ERRCODE_KEYORRECDELETED);
  DBIERR_KEYORRECDELETED        = (ERRBASE_NOTFOUND + ERRCODE_KEYORRECDELETED);
  DBIERR_NOCURRREC              = (ERRBASE_NOTFOUND + ERRCODE_NOCURRREC);
  DBIERR_RECNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_RECNOTFOUND);
  DBIERR_ENDOFBLOB              = (ERRBASE_NOTFOUND + ERRCODE_ENDOFBLOB);
  DBIERR_OBJNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_OBJNOTFOUND);
  DBIERR_FMLMEMBERNOTFOUND      = (ERRBASE_NOTFOUND + ERRCODE_FMLMEMBERNOTFOUND);
  DBIERR_BLOBFILEMISSING        = (ERRBASE_NOTFOUND + ERRCODE_BLOBFILEMISSING);
  DBIERR_LDNOTFOUND             = (ERRBASE_NOTFOUND + ERRCODE_LDNOTFOUND);

{ ERRCAT_DATACORRUPT }
{ ================== }

  ERRCODE_HEADERCORRUPT         = 1;      { Corrupt Header }
  ERRCODE_FILECORRUPT           = 2;      { File corrupt - other than header }
  ERRCODE_MEMOCORRUPT           = 3;      { Memo file corrupted }
  ERRCODE_BMPCORRUPT            = 4;      { BitMap is corrupt (Internal error) }
  ERRCODE_INDEXCORRUPT          = 5;      { Index is corrupt }
  ERRCODE_CORRUPTLOCKFILE       = 6;      { Corrupt lock file }
  ERRCODE_FAMFILEINVALID        = 7;      { Corrupt family file }
  ERRCODE_VALFILECORRUPT        = 8;      { Val file is missing or corrupt }
  ERRCODE_FOREIGNINDEX          = 9;      { Index is in a foreign format - import first }


  DBIERR_HEADERCORRUPT          = (ERRBASE_DATACORRUPT + ERRCODE_HEADERCORRUPT);
  DBIERR_FILECORRUPT            = (ERRBASE_DATACORRUPT + ERRCODE_FILECORRUPT);
  DBIERR_MEMOCORRUPT            = (ERRBASE_DATACORRUPT + ERRCODE_MEMOCORRUPT);
  DBIERR_BMPCORRUPT             = (ERRBASE_DATACORRUPT + ERRCODE_BMPCORRUPT);
  DBIERR_INDEXCORRUPT           = (ERRBASE_DATACORRUPT + ERRCODE_INDEXCORRUPT);
  DBIERR_CORRUPTLOCKFILE        = (ERRBASE_DATACORRUPT + ERRCODE_CORRUPTLOCKFILE);
  DBIERR_FAMFILEINVALID         = (ERRBASE_DATACORRUPT + ERRCODE_FAMFILEINVALID);
  DBIERR_VALFILECORRUPT         = (ERRBASE_DATACORRUPT + ERRCODE_VALFILECORRUPT);
  DBIERR_FOREIGNINDEX           = (ERRBASE_DATACORRUPT + ERRCODE_FOREIGNINDEX);


{ ERRCAT_IO }
{ ========= }

  ERRCODE_READERR               = 1;      { Read failure (not expected) }
  ERRCODE_WRITEERR              = 2;      { Write failure (not expected) }
  ERRCODE_DIRNOACCESS           = 3;      { No access to dir }
  ERRCODE_FILEDELETEFAIL        = 4;      { File delete failed }
  ERRCODE_FILENOACCESS          = 5;      { No access to file }
  ERRCODE_ACCESSDISABLED        = 6;      { Access to table disabled (previous error) }

  DBIERR_READERR                = (ERRBASE_IO + ERRCODE_READERR);
  DBIERR_WRITEERR               = (ERRBASE_IO + ERRCODE_WRITEERR);
  DBIERR_DIRNOACCESS            = (ERRBASE_IO + ERRCODE_DIRNOACCESS);
  DBIERR_FILEDELETEFAIL         = (ERRBASE_IO + ERRCODE_FILEDELETEFAIL);
  DBIERR_FILENOACCESS           = (ERRBASE_IO + ERRCODE_FILENOACCESS);
  DBIERR_ACCESSDISABLED         = (ERRBASE_IO + ERRCODE_ACCESSDISABLED);

{ ERRCAT_LIMIT }
{ ============ }

  ERRCODE_NOMEMORY              = 1;      { Not enough Memory for this op }
  ERRCODE_NOFILEHANDLES         = 2;      { Not enough File handles }
  ERRCODE_NODISKSPACE           = 3;      { Not enough Disk space }
  ERRCODE_NOTEMPTBLSPACE        = 4;      { Temporary Table resource limit }
  ERRCODE_RECTOOBIG             = 5;      { Too big a record size for table }
  ERRCODE_CURSORLIMIT           = 6;      { Too many open cursors }
  ERRCODE_TABLEFULL             = 7;      { Table is full }
  ERRCODE_WSSESLIMIT            = 8;      { Too many sessions from this WS }
  ERRCODE_SERNUMLIMIT           = 9;      { Serial number limit (paradox) }
  ERRCODE_INTERNALLIMIT         = 10;     { 0x0a Some internal limit (see context) }
  ERRCODE_OPENTBLLIMIT          = 11;     { 0x0b Too many open tables }
  ERRCODE_TBLCURSORLIMIT        = 12;     { 0x0c Too many cursors per table }
  ERRCODE_RECLOCKLIMIT          = 13;     { 0x0d Too many record locks on table }
  ERRCODE_CLIENTSLIMIT          = 14;     { 0x0e Too many clients }
  ERRCODE_INDEXLIMIT            = 15;     { 0x0f Too many indexes (also in Table Create) }
  ERRCODE_SESSIONSLIMIT         = 16;     { 0x10 Too many sessions }
  ERRCODE_DBLIMIT               = 17;     { 0x11 Too many databases }
  ERRCODE_PASSWORDLIMIT         = 18;     { 0x12 Too many passwords }
  ERRCODE_DRIVERLIMIT           = 19;     { 0x13 Too many active drivers }
  ERRCODE_FLDLIMIT              = 20;     { 0x14 Too many Fields in Table Create }
  ERRCODE_TBLLOCKLIMIT          = 21;     { 0x15 Too many table locks }
  ERRCODE_OPENBLOBLIMIT         = 22;     { 0x16 Too many open blobs }
  ERRCODE_LOCKFILELIMIT         = 23;     { 0x17 Lock file has grown too big }
  ERRCODE_OPENQRYLIMIT          = 24;     { 0x18 Too many open queries }
  ERRCODE_THREADLIMIT           = 25;     { 0x19 Too many threads for client }
  ERRCODE_BLOBLIMIT             = 26;     { 0x1a Too many blobs }
  ERRCODE_PDX50NAMELIMIT        = 27;     { 0x1b Pathname is too long for a Paradox 5.0 or less table }
  ERRCODE_ROWFETCHLIMIT         = 28;     { 0x1c Row fetch limit }
  ERRCODE_LONGNAMENOTALLOWED    = 29;     { 0x1d Long name is not allowed for this tableversion }
  ERRCODE_NOSHAREDMEMORY        = 30;     { 0x1e Not enough shared Memory for this operation }

  DBIERR_NOMEMORY               = (ERRBASE_LIMIT + ERRCODE_NOMEMORY);
  DBIERR_NOFILEHANDLES          = (ERRBASE_LIMIT + ERRCODE_NOFILEHANDLES);
  DBIERR_NODISKSPACE            = (ERRBASE_LIMIT + ERRCODE_NODISKSPACE);
  DBIERR_NOTEMPTBLSPACE         = (ERRBASE_LIMIT + ERRCODE_NOTEMPTBLSPACE);
  DBIERR_RECTOOBIG              = (ERRBASE_LIMIT + ERRCODE_RECTOOBIG);
  DBIERR_CURSORLIMIT            = (ERRBASE_LIMIT + ERRCODE_CURSORLIMIT);
  DBIERR_TABLEFULL              = (ERRBASE_LIMIT + ERRCODE_TABLEFULL);
  DBIERR_WSSESLIMIT             = (ERRBASE_LIMIT + ERRCODE_WSSESLIMIT);
  DBIERR_SERNUMLIMIT            = (ERRBASE_LIMIT + ERRCODE_SERNUMLIMIT);
  DBIERR_INTERNALLIMIT          = (ERRBASE_LIMIT + ERRCODE_INTERNALLIMIT);
  DBIERR_OPENTBLLIMIT           = (ERRBASE_LIMIT + ERRCODE_OPENTBLLIMIT);
  DBIERR_TBLCURSORLIMIT         = (ERRBASE_LIMIT + ERRCODE_TBLCURSORLIMIT);
  DBIERR_RECLOCKLIMIT           = (ERRBASE_LIMIT + ERRCODE_RECLOCKLIMIT);
  DBIERR_CLIENTSLIMIT           = (ERRBASE_LIMIT + ERRCODE_CLIENTSLIMIT);
  DBIERR_INDEXLIMIT             = (ERRBASE_LIMIT + ERRCODE_INDEXLIMIT);
  DBIERR_SESSIONSLIMIT          = (ERRBASE_LIMIT + ERRCODE_SESSIONSLIMIT);
  DBIERR_DBLIMIT                = (ERRBASE_LIMIT + ERRCODE_DBLIMIT);
  DBIERR_PASSWORDLIMIT          = (ERRBASE_LIMIT + ERRCODE_PASSWORDLIMIT);
  DBIERR_DRIVERLIMIT            = (ERRBASE_LIMIT + ERRCODE_DRIVERLIMIT);
  DBIERR_FLDLIMIT               = (ERRBASE_LIMIT + ERRCODE_FLDLIMIT);
  DBIERR_TBLLOCKLIMIT           = (ERRBASE_LIMIT + ERRCODE_TBLLOCKLIMIT);
  DBIERR_OPENBLOBLIMIT          = (ERRBASE_LIMIT + ERRCODE_OPENBLOBLIMIT);
  DBIERR_LOCKFILELIMIT          = (ERRBASE_LIMIT + ERRCODE_LOCKFILELIMIT);
  DBIERR_OPENQRYLIMIT           = (ERRBASE_LIMIT + ERRCODE_OPENQRYLIMIT);
  DBIERR_THREADLIMIT            = (ERRBASE_LIMIT + ERRCODE_THREADLIMIT);
  DBIERR_BLOBLIMIT              = (ERRBASE_LIMIT + ERRCODE_BLOBLIMIT);
  DBIERR_PDX50NAMELIMIT         = (ERRBASE_LIMIT + ERRCODE_PDX50NAMELIMIT);
  DBIERR_ROWFETCHLIMIT          = (ERRBASE_LIMIT + ERRCODE_ROWFETCHLIMIT);
  DBIERR_LONGNAMENOTALLOWED     = (ERRBASE_LIMIT + ERRCODE_LONGNAMENOTALLOWED);
  DBIERR_NOSHAREDMEMORY         = (ERRBASE_LIMIT + ERRCODE_NOSHAREDMEMORY);


{ ERRCAT_INTEGRITY }
{ ================ }

  ERRCODE_KEYVIOL               = 1;      { Key violation }
  ERRCODE_MINVALERR             = 2;      { Min val check failed }
  ERRCODE_MAXVALERR             = 3;      { Max val check failed }
  ERRCODE_REQDERR               = 4;      { Field value required }
  ERRCODE_FORIEGNKEYERR         = 5;      { Master record missing }
  ERRCODE_DETAILRECORDSEXIST    = 6;      { Cannot MODIFY or DELETE this Master record }
  ERRCODE_MASTERTBLLEVEL        = 7;      { Master Table Level is incorrect }
  ERRCODE_LOOKUPTABLEERR        = 8;      { Field value out of lookup tbl range }
  ERRCODE_LOOKUPTBLOPENERR      = 9;      { Lookup Table Open failed }
  ERRCODE_DETAILTBLOPENERR      = 10;     { 0x0a Detail Table Open failed }
  ERRCODE_MASTERTBLOPENERR      = 11;     { 0x0b Master Table Open failed }
  ERRCODE_FIELDISBLANK          = 12;     { 0x0c Field is blank }

  ERRCODE_MASTEREXISTS          = 13;     { 0x0d Master Table exists }
  ERRCODE_MASTERTBLOPEN         = 14;     { 0x0e Master Table is open }

  ERRCODE_DETAILTABLESEXIST     = 15;     { 0x0f Detail Tables exist ( cannot delete, rename ... ) }
  ERRCODE_DETAILRECEXISTEMPTY   = 16;     { 0x10 Cannot empty because details exist }
  ERRCODE_MASTERREFERENCEERR    = 17;     { 0x11 Cannot modify while adding self referencing Referential Integrity }
  ERRCODE_DETAILTBLOPEN         = 18;     { 0x12 Detail Table is opened }
  ERRCODE_DEPENDENTSMUSTBEEMPTY = 19;     { 0x13 Cannot make a master a detail of another table if its details are not empty. }
  ERRCODE_RINTREQINDEX          = 20;     { 0x14 Ref. integrity fields must be indexed }
  ERRCODE_LINKEDTBLPROTECTED    = 21;     { 0x15 Master Table is protected ( requires password to open) }
  ERRCODE_FIELDMULTILINKED      = 22;     { 0x16 Field has more than one master }
  ERRCODE_EXPRVALERR            = 23;     { 0x17 Expr val check failed }

  DBIERR_KEYVIOL                = (ERRBASE_INTEGRITY + ERRCODE_KEYVIOL);
  DBIERR_MINVALERR              = (ERRBASE_INTEGRITY + ERRCODE_MINVALERR);
  DBIERR_MAXVALERR              = (ERRBASE_INTEGRITY + ERRCODE_MAXVALERR);
  DBIERR_REQDERR                = (ERRBASE_INTEGRITY + ERRCODE_REQDERR);
  DBIERR_FORIEGNKEYERR          = (ERRBASE_INTEGRITY + ERRCODE_FORIEGNKEYERR);
  DBIERR_DETAILRECORDSEXIST     = (ERRBASE_INTEGRITY + ERRCODE_DETAILRECORDSEXIST);
  DBIERR_MASTERTBLLEVEL         = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLLEVEL);
  DBIERR_LOOKUPTABLEERR         = (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTABLEERR);
  DBIERR_LOOKUPTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTBLOPENERR);
  DBIERR_DETAILTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPENERR);
  DBIERR_MASTERTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPENERR);
  DBIERR_FIELDISBLANK           = (ERRBASE_INTEGRITY + ERRCODE_FIELDISBLANK);
  DBIERR_MASTEREXISTS           = (ERRBASE_INTEGRITY + ERRCODE_MASTEREXISTS);
  DBIERR_MASTERTBLOPEN          = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPEN);
  DBIERR_DETAILTABLESEXIST      = (ERRBASE_INTEGRITY + ERRCODE_DETAILTABLESEXIST);
  DBIERR_DETAILRECEXISTEMPTY    = (ERRBASE_INTEGRITY + ERRCODE_DETAILRECEXISTEMPTY);
  DBIERR_MASTERREFERENCEERR     = (ERRBASE_INTEGRITY + ERRCODE_MASTERREFERENCEERR);
  DBIERR_DETAILTBLOPEN          = (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPEN);
  DBIERR_DEPENDENTSMUSTBEEMPTY  = (ERRBASE_INTEGRITY + ERRCODE_DEPENDENTSMUSTBEEMPTY);
  DBIERR_RINTREQINDEX           = (ERRBASE_INTEGRITY + ERRCODE_RINTREQINDEX);
  DBIERR_LINKEDTBLPROTECTED     = (ERRBASE_INTEGRITY + ERRCODE_LINKEDTBLPROTECTED);
  DBIERR_FIELDMULTILINKED       = (ERRBASE_INTEGRITY + ERRCODE_FIELDMULTILINKED);
  DBIERR_EXPRVALERR             = (ERRBASE_INTEGRITY + ERRCODE_EXPRVALERR);


{ ERRCAT_INVALIDREQ }
{ ================= }

  ERRCODE_OUTOFRANGE            = 1;      { Number out of range (e.g field no) }
  ERRCODE_INVALIDPARAM          = 2;      { Generic invalid parameter }
  ERRCODE_INVALIDFILENAME       = 3;      { Invalid file name }
  ERRCODE_NOSUCHFILE            = 4;      { No such file }
  ERRCODE_INVALIDOPTION         = 5;      { Invalid option for a parameter }
  ERRCODE_INVALIDHNDL           = 6;      { Invalid handle to the function }
  ERRCODE_UNKNOWNTBLTYPE        = 7;      { Table type given not known }
  ERRCODE_UNKNOWNFILE           = 8;      { Dont know how to open file }
  ERRCODE_PRIMARYKEYREDEFINE    = 9;      { Cannot redefine primary key }
  ERRCODE_INVALIDRINTDESCNUM    = 10;     { 0x0a Cannot change this RINTDesc }
  ERRCODE_KEYFLDTYPEMISMATCH    = 11;     { 0x0b Foreign & Primary Key Mismatch }
  ERRCODE_INVALIDMODIFYREQUEST  = 12;     { 0x0c Invalid modify request }
  ERRCODE_NOSUCHINDEX           = 13;     { 0x0d Index does not exist }
  ERRCODE_INVALIDBLOBOFFSET     = 14;     { 0x0e Invalid Offset into the Blob }
  ERRCODE_INVALIDDESCNUM        = 15;     { 0x0f Invalid descriptor number }
  ERRCODE_INVALIDFLDTYPE        = 16;     { 0x10 Invalid field type }
  ERRCODE_INVALIDFLDDESC        = 17;     { 0x11 Invalid field descriptor }
  ERRCODE_INVALIDFLDXFORM       = 18;     { 0x12 Invalid field transform }
  ERRCODE_INVALIDRECSTRUCT      = 19;     { 0x13 Invalid record structure }
  ERRCODE_INVALIDDESC           = 20;     { 0x14 Generic: invalid descriptor }
  ERRCODE_INVALIDINDEXSTRUCT    = 21;     { 0x15 Invalid array of indexes descriptors }
  ERRCODE_INVALIDVCHKSTRUCT     = 22;     { 0x16 Invalid array of  val. check descriptors }
  ERRCODE_INVALIDRINTSTRUCT     = 23;     { 0x17 Invalid array of ref. integrity descriptors }
  ERRCODE_INVALIDRESTRTBLORDER  = 24;     { 0x18 Invalid ordering of tables during restructure }
  ERRCODE_NAMENOTUNIQUE         = 25;     { 0x19 Name not unique in this context }
  ERRCODE_INDEXNAMEREQUIRED     = 26;     { 0x1a Index name required }
  ERRCODE_INVALIDSESHANDLE      = 27;     { 0x1b Invalid ses handle }
  ERRCODE_INVALIDRESTROP        = 28;     { 0x1c Invalid restructure operation }
  ERRCODE_UNKNOWNDRIVER         = 29;     { 0x1d Driver not known to system }
  ERRCODE_UNKNOWNDB             = 30;     { 0x1e Unknown db }
  ERRCODE_INVALIDPASSWORD       = 31;     { 0x1f Invalid password given }
  ERRCODE_NOCALLBACK            = 32;     { 0x20 No callback function }
  ERRCODE_INVALIDCALLBACKBUFLEN = 33;     { 0x21 Invalid callback buffer length }
  ERRCODE_INVALIDDIR            = 34;     { 0x22 Invalid directory }
  ERRCODE_INVALIDXLATION        = 35;     { 0x23 Translate Error - Translate DID NOT happen }
  ERRCODE_DIFFERENTTABLES       = 36;     { 0x24 Cannot Set Cursor of one Table to another }
  ERRCODE_INVALIDBOOKMARK       = 37;     { 0x25 Bookmarks does not match table, etc. }
  ERRCODE_INVALIDINDEXNAME      = 38;     { 0x26 Index/Tag Name is invalid }
  ERRCODE_INVALIDIDXDESC        = 39;     { 0x27 Invalid index descriptor }
  ERRCODE_NOSUCHTABLE           = 40;     { 0x28 No such table }
  ERRCODE_USECOUNT              = 41;     { 0x29 Table has too many users }
  ERRCODE_INVALIDKEY            = 42;     { 0x2a Key does not pass filter condition }
  ERRCODE_INDEXEXISTS           = 43;     { 0x2b Index already exists }
  ERRCODE_INDEXOPEN             = 44;     { 0x2c Index is open }
  ERRCODE_INVALIDBLOBLEN        = 45;     { 0x2d Invalid Blob Length }
  ERRCODE_INVALIDBLOBHANDLE     = 46;     { 0x2e Invalid Blob handle (in record buffer) }
  ERRCODE_TABLEOPEN             = 47;     { 0x2f Table is open }
  ERRCODE_NEEDRESTRUCTURE       = 48;     { 0x30 Need to do (hard) restructure }
  ERRCODE_INVALIDMODE           = 49;     { 0x31 Invalid mode }
  ERRCODE_CANNOTCLOSE           = 50;     { 0x32 Cannot close index }
  ERRCODE_ACTIVEINDEX           = 51;     { 0x33 Index is being used to order tbl }
  ERRCODE_INVALIDUSRPASS        = 52;     { 0x34 Bad user name or password }
  ERRCODE_MULTILEVELCASCADE     = 53;     { 0x35 Multi level Cascade not supported }
  ERRCODE_INVALIDFIELDNAME      = 54;     { 0x36 Invalid field name }
  ERRCODE_INVALIDTABLENAME      = 55;     { 0x37 Invalid table name }
  ERRCODE_INVALIDLINKEXPR       = 56;     { 0x38 Invalid linked cursor expression }
  ERRCODE_NAMERESERVED          = 57;     { 0x39 Name is reserved }
  ERRCODE_INVALIDFILEEXTN       = 58;     { 0x3a Invalid file extention }
  ERRCODE_INVALIDLANGDRV        = 59;     { 0x3b Invalid language driver }
  ERRCODE_ALIASNOTOPEN          = 60;     { 0x3c Requested alias in not open }
  ERRCODE_INCOMPATRECSTRUCTS    = 61;     { 0x3d Incompatible record structures }
  ERRCODE_RESERVEDDOSNAME       = 62;     { 0x3e Reserved dos name }
  ERRCODE_DESTMUSTBEINDEXED     = 63;     { 0x3f Destination must be indexed }
  ERRCODE_INVALIDINDEXTYPE      = 64;     { 0x40 Invalid index type }
  ERRCODE_LANGDRVMISMATCH       = 65;     { 0x41 Language driver of table and index do not match }
  ERRCODE_NOSUCHFILTER          = 66;     { 0x42 Filter handle is invalid }
  ERRCODE_INVALIDFILTER         = 67;     { 0x43 Invalid filter }

  ERRCODE_INVALIDTABLECREATE    = 68;     { 0x44 Bad table create request (exact prob unknown) }
  ERRCODE_INVALIDTABLEDELETE    = 69;     { 0x45 Bad table delete request (exact prob unknown) }
  ERRCODE_INVALIDINDEXCREATE    = 70;     { 0x46 Bad index create request (exact prob unknown) }
  ERRCODE_INVALIDINDEXDELETE    = 71;     { 0x47 Bad index delete request (exact prob unknown) }
  ERRCODE_INVALIDTABLE          = 72;     { 0x48 Invalid table name specified }
  ERRCODE_MULTIRESULTS          = 73;     { 0X49 Multi results }
  ERRCODE_INVALIDTIME           = 74;     { 0X4A Multi results }
  ERRCODE_INVALIDDATE           = 75;     { 0X4B Multi results }
  ERRCODE_INVALIDTIMESTAMP      = 76;     { 0X4C Multi results }
  ERRCODE_DIFFERENTPATH         = 77;     { 0X4d Tables in different paths }
  ERRCODE_MISMATCHARGS          = 78;     { 0x4e MisMatch in the # of arguments }
  ERRCODE_FUNCTIONNOTFOUND      = 79;     { 0x4f Loaderlib cant find a func in the DLL (bad version?) }
  ERRCODE_MUSTUSEBASEORDER      = 80;     { 0x50 Must use baseorder for this operation }
  ERRCODE_INVALIDPROCEDURENAME  = 81;     { 0x51 Invalid procedure name }
  ERRCODE_INVALIDFLDMAP         = 82;     { 0x52 invalid field map }


  DBIERR_OUTOFRANGE             = (ERRBASE_INVALIDREQ + ERRCODE_OUTOFRANGE);
  DBIERR_INVALIDPARAM           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPARAM);
  DBIERR_INVALIDFILENAME        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILENAME);
  DBIERR_NOSUCHFILE             = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILE);
  DBIERR_INVALIDOPTION          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDOPTION);
  DBIERR_INVALIDHNDL            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDHNDL);
  DBIERR_UNKNOWNTBLTYPE         = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNTBLTYPE);
  DBIERR_UNKNOWNFILE            = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNFILE);
  DBIERR_PRIMARYKEYREDEFINE     = (ERRBASE_INVALIDREQ + ERRCODE_PRIMARYKEYREDEFINE);
  DBIERR_INVALIDRINTDESCNUM     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRINTDESCNUM);
  DBIERR_KEYFLDTYPEMISMATCH     = (ERRBASE_INVALIDREQ + ERRCODE_KEYFLDTYPEMISMATCH);
  DBIERR_INVALIDMODIFYREQUEST   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDMODIFYREQUEST);
  DBIERR_NOSUCHINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHINDEX);
  DBIERR_INVALIDBLOBOFFSET      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBOFFSET);
  DBIERR_INVALIDDESCNUM         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDESCNUM);
  DBIERR_INVALIDFLDTYPE         = (ERRBASE_INVALIDREQ +  ERRCODE_INVALIDFLDTYPE);
  DBIERR_INVALIDFLDDESC         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDDESC);
  DBIERR_INVALIDFLDXFORM        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDXFORM);
  DBIERR_INVALIDRECSTRUCT       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRECSTRUCT);
  DBIERR_INVALIDDESC            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDESC);
  DBIERR_INVALIDINDEXSTRUCT     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXSTRUCT);
  DBIERR_INVALIDVCHKSTRUCT      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDVCHKSTRUCT);
  DBIERR_INVALIDRINTSTRUCT      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRINTSTRUCT);
  DBIERR_INVALIDRESTRTBLORDER   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRESTRTBLORDER);
  DBIERR_NAMENOTUNIQUE          = (ERRBASE_INVALIDREQ + ERRCODE_NAMENOTUNIQUE);
  DBIERR_INDEXNAMEREQUIRED      = (ERRBASE_INVALIDREQ + ERRCODE_INDEXNAMEREQUIRED);
  DBIERR_INVALIDSESHANDLE       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDSESHANDLE);
  DBIERR_INVALIDRESTROP         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRESTROP);
  DBIERR_UNKNOWNDRIVER          = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNDRIVER);
  DBIERR_UNKNOWNDB              = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNDB);
  DBIERR_INVALIDPASSWORD        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPASSWORD);
  DBIERR_NOCALLBACK             = (ERRBASE_INVALIDREQ + ERRCODE_NOCALLBACK);
  DBIERR_INVALIDCALLBACKBUFLEN  = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDCALLBACKBUFLEN );
  DBIERR_INVALIDDIR             = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDIR);
  DBIERR_INVALIDXLATION         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDXLATION);
  DBIERR_DIFFERENTTABLES        = (ERRBASE_INVALIDREQ + ERRCODE_DIFFERENTTABLES);
  DBIERR_INVALIDBOOKMARK        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBOOKMARK);
  DBIERR_INVALIDINDEXNAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXNAME);
  DBIERR_INVALIDIDXDESC         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDIDXDESC);
  DBIERR_NOSUCHTABLE            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHTABLE);
  DBIERR_USECOUNT               = (ERRBASE_INVALIDREQ + ERRCODE_USECOUNT);
  DBIERR_INVALIDKEY             = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDKEY);
  DBIERR_INDEXEXISTS            = (ERRBASE_INVALIDREQ + ERRCODE_INDEXEXISTS);
  DBIERR_INDEXOPEN              = (ERRBASE_INVALIDREQ + ERRCODE_INDEXOPEN);
  DBIERR_INVALIDBLOBLEN         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBLEN);
  DBIERR_INVALIDBLOBHANDLE      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBHANDLE);
  DBIERR_TABLEOPEN              = (ERRBASE_INVALIDREQ + ERRCODE_TABLEOPEN);
  DBIERR_NEEDRESTRUCTURE        = (ERRBASE_INVALIDREQ + ERRCODE_NEEDRESTRUCTURE);
  DBIERR_INVALIDMODE            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDMODE);
  DBIERR_CANNOTCLOSE            = (ERRBASE_INVALIDREQ + ERRCODE_CANNOTCLOSE);
  DBIERR_ACTIVEINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_ACTIVEINDEX);
  DBIERR_INVALIDUSRPASS         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDUSRPASS);
  DBIERR_MULTILEVELCASCADE      = (ERRBASE_INVALIDREQ + ERRCODE_MULTILEVELCASCADE);
  DBIERR_INVALIDFIELDNAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFIELDNAME);
  DBIERR_INVALIDTABLENAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLENAME);
  DBIERR_INVALIDLINKEXPR        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDLINKEXPR);
  DBIERR_NAMERESERVED           = (ERRBASE_INVALIDREQ + ERRCODE_NAMERESERVED);
  DBIERR_INVALIDFILEEXTN        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILEEXTN);
  DBIERR_INVALIDLANGDRV         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDLANGDRV);
  DBIERR_ALIASNOTOPEN           = (ERRBASE_INVALIDREQ + ERRCODE_ALIASNOTOPEN);
  DBIERR_INCOMPATRECSTRUCTS     = (ERRBASE_INVALIDREQ + ERRCODE_INCOMPATRECSTRUCTS);
  DBIERR_RESERVEDOSNAME         = (ERRBASE_INVALIDREQ + ERRCODE_RESERVEDDOSNAME);
  DBIERR_DESTMUSTBEINDEXED      = (ERRBASE_INVALIDREQ + ERRCODE_DESTMUSTBEINDEXED);
  DBIERR_INVALIDINDEXTYPE       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXTYPE);
  DBIERR_LANGDRVMISMATCH        = (ERRBASE_INVALIDREQ + ERRCODE_LANGDRVMISMATCH);
  DBIERR_NOSUCHFILTER           = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILTER);
  DBIERR_INVALIDFILTER          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILTER);
  DBIERR_INVALIDTABLECREATE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLECREATE);
  DBIERR_INVALIDTABLEDELETE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLEDELETE);
  DBIERR_INVALIDINDEXCREATE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXCREATE);
  DBIERR_INVALIDINDEXDELETE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXDELETE);
  DBIERR_INVALIDTABLE           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLE);
  DBIERR_MULTIRESULTS           = (ERRBASE_INVALIDREQ + ERRCODE_MULTIRESULTS);
  DBIERR_INVALIDTIME            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTIME);
  DBIERR_INVALIDDATE            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDATE);
  DBIERR_INVALIDTIMESTAMP       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTIMESTAMP);
  DBIERR_DIFFERENTPATH          = (ERRBASE_INVALIDREQ + ERRCODE_DIFFERENTPATH);
  DBIERR_MISMATCHARGS           = (ERRBASE_INVALIDREQ + ERRCODE_MISMATCHARGS);
  DBIERR_FUNCTIONNOTFOUND       = (ERRBASE_INVALIDREQ + ERRCODE_FUNCTIONNOTFOUND);
  DBIERR_MUSTUSEBASEORDER       = (ERRBASE_INVALIDREQ + ERRCODE_MUSTUSEBASEORDER);
  DBIERR_INVALIDPROCEDURENAME   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPROCEDURENAME);
  DBIERR_INVALIDFLDMAP          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDMAP);

{ ERRCAT_LOCKCONFLICT }
{ =================== }

  ERRCODE_LOCKED                = 1;
  ERRCODE_UNLOCKFAILED          = 2;
  ERRCODE_FILEBUSY              = 3;
  ERRCODE_DIRBUSY               = 4;
  ERRCODE_FILELOCKED            = 5;
  ERRCODE_DIRLOCKED             = 6;
  ERRCODE_ALREADYLOCKED         = 7;
  ERRCODE_NOTLOCKED             = 8;
  ERRCODE_LOCKTIMEOUT           = 9;
  ERRCODE_GROUPLOCKED           = 10;     { 0x0a }
  ERRCODE_LOSTTBLLOCK           = 11;     { 0x0b }
  ERRCODE_LOSTEXCLACCESS        = 12;     { 0x0c }
  ERRCODE_NEEDEXCLACCESS        = 13;     { 0x0d }
  ERRCODE_RECGROUPCONFLICT      = 14;     { 0x0e }
  ERRCODE_DEADLOCK              = 15;
  ERRCODE_ACTIVETRAN            = 16;
  ERRCODE_NOACTIVETRAN          = 17;
  ERRCODE_RECLOCKFAILED         = 18;
  ERRCODE_OPTRECLOCKFAILED      = 19;
  ERRCODE_OPTRECLOCKRECDEL      = 20;
  ERRCODE_LOCKEDRECS            = 21;
  ERRCODE_NEEDWRITELOCK         = 22;
  ERRCODE_ENLISTFAILED          = 23; 

  DBIERR_LOCKED                 = (ERRBASE_LOCKCONFLICT + ERRCODE_LOCKED);
  DBIERR_UNLOCKFAILED           = (ERRBASE_LOCKCONFLICT + ERRCODE_UNLOCKFAILED);
  DBIERR_FILEBUSY               = (ERRBASE_LOCKCONFLICT + ERRCODE_FILEBUSY);
  DBIERR_DIRBUSY                = (ERRBASE_LOCKCONFLICT + ERRCODE_DIRBUSY);
  DBIERR_FILELOCKED             = (ERRBASE_LOCKCONFLICT + ERRCODE_FILELOCKED);
  DBIERR_DIRLOCKED              = (ERRBASE_LOCKCONFLICT + ERRCODE_DIRLOCKED);
  DBIERR_ALREADYLOCKED          = (ERRBASE_LOCKCONFLICT + ERRCODE_ALREADYLOCKED);
  DBIERR_NOTLOCKED              = (ERRBASE_LOCKCONFLICT + ERRCODE_NOTLOCKED);
  DBIERR_LOCKTIMEOUT            = (ERRBASE_LOCKCONFLICT + ERRCODE_LOCKTIMEOUT);
  DBIERR_GROUPLOCKED            = (ERRBASE_LOCKCONFLICT + ERRCODE_GROUPLOCKED);
  DBIERR_LOSTTBLLOCK            = (ERRBASE_LOCKCONFLICT + ERRCODE_LOSTTBLLOCK);
  DBIERR_LOSTEXCLACCESS         = (ERRBASE_LOCKCONFLICT + ERRCODE_LOSTEXCLACCESS);
  DBIERR_NEEDEXCLACCESS         = (ERRBASE_LOCKCONFLICT  + ERRCODE_NEEDEXCLACCESS);
  DBIERR_RECGROUPCONFLICT       = (ERRBASE_LOCKCONFLICT + ERRCODE_RECGROUPCONFLICT);
  DBIERR_DEADLOCK               = (ERRBASE_LOCKCONFLICT + ERRCODE_DEADLOCK);
  DBIERR_ACTIVETRAN             = (ERRBASE_LOCKCONFLICT + ERRCODE_ACTIVETRAN);
  DBIERR_NOACTIVETRAN           = (ERRBASE_LOCKCONFLICT + ERRCODE_NOACTIVETRAN);
  DBIERR_RECLOCKFAILED          = (ERRBASE_LOCKCONFLICT + ERRCODE_RECLOCKFAILED);
  DBIERR_OPTRECLOCKFAILED       = (ERRBASE_LOCKCONFLICT + ERRCODE_OPTRECLOCKFAILED);
  DBIERR_OPTRECLOCKRECDEL       = (ERRBASE_LOCKCONFLICT + ERRCODE_OPTRECLOCKRECDEL);
  DBIERR_ENLISTFAILED           = (ERRBASE_LOCKCONFLICT + ERRCODE_ENLISTFAILED); 

{ ERRCAT_SECURITY }
{ =============== }

  ERRCODE_NOTSUFFFIELDRIGHTS    = 1;      { Not sufficient field  rights for operation }
  ERRCODE_NOTSUFFTABLERIGHTS    = 2;      { Not sufficient table  rights for operation }
  ERRCODE_NOTSUFFFAMILYRIGHTS   = 3;      { Not sufficient family rights for operation }
  ERRCODE_READONLYDIR           = 4;      { Is a read-only directory }
  ERRCODE_READONLYDB            = 5;      { Database is read-only }
  ERRCODE_READONLYFLD           = 6;      { Trying to modify read-only field }
  ERRCODE_TBLENCRYPTED          = 7;      { Table is encrypted (dBASE only) }
  ERRCODE_NOTSUFFSQLRIGHTS      = 8;      { Not sufficient sql rights for operation }


  DBIERR_NOTSUFFFIELDRIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFFIELDRIGHTS);
  DBIERR_NOTSUFFTABLERIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFTABLERIGHTS);
  DBIERR_NOTSUFFFAMILYRIGHTS    = (ERRBASE_SEC + ERRCODE_NOTSUFFFAMILYRIGHTS);
  DBIERR_READONLYDIR            = (ERRBASE_SEC + ERRCODE_READONLYDIR);
  DBIERR_READONLYDB             = (ERRBASE_SEC + ERRCODE_READONLYDB);
  DBIERR_READONLYFLD            = (ERRBASE_SEC + ERRCODE_READONLYFLD);
  DBIERR_TBLENCRYPTED           = (ERRBASE_SEC + ERRCODE_TBLENCRYPTED);
  DBIERR_NOTSUFFSQLRIGHTS       = (ERRBASE_SEC + ERRCODE_NOTSUFFSQLRIGHTS);


{ ERRCAT_INVALIDCONTEXT }
{ ===================== }

  ERRCODE_NOTABLOB              = 1;      { Field is not a blob }
  ERRCODE_BLOBOPENED            = 2;      { Blob already opened }
  ERRCODE_BLOBNOTOPENED         = 3;      { Blob not opened }
  ERRCODE_NA                    = 4;      { Operation not applicable }
  ERRCODE_NOTINDEXED            = 5;      { Table is not indexed }
  ERRCODE_NOTINITIALIZED        = 6;      { Engine not initialized }
  ERRCODE_MULTIPLEINIT          = 7;      { Attempt to re-initialize engine }
  ERRCODE_NOTSAMESESSION        = 8;      { Attempt to mix objs from diff ses }
  ERRCODE_PDXDRIVERNOTACTIVE    = 9;      { Paradox driver not active }
  ERRCODE_DRIVERNOTLOADED       = 10;     { 0x0a Driver not loaded }
  ERRCODE_TABLEREADONLY         = 11;     { 0x0b Table is read only }
  ERRCODE_NOASSOCINDEX          = 12;     { 0x0c No index associated with the cursor }
  ERRCODE_HASOPENCURSORS        = 13;     { 0x0d Has open cursors }
  ERRCODE_NOTABLESUPPORT        = 14;     { 0x0e Op cannot be done on this table }
  ERRCODE_INDEXREADONLY         = 15;     { 0x0f Index is read only }
  ERRCODE_NOUNIQUERECS          = 16;     { 0x10 Records are not unique }
  ERRCODE_NOTCURSESSION         = 17;     { 0x11 Not the current/active session }
  ERRCODE_INVALIDKEYWORD        = 18;     { 0x12 Invalid use of keyword. }
  ERRCODE_CONNECTINUSE          = 19;     { 0x13 Connection in use }
  ERRCODE_CONNECTNOTSHARED      = 20;     { 0x14 Passthru SQL connection not share }


  DBIERR_NOTABLOB               = (ERRBASE_IC + ERRCODE_NOTABLOB);
  DBIERR_BLOBOPENED             = (ERRBASE_IC + ERRCODE_BLOBOPENED);
  DBIERR_BLOBNOTOPENED          = (ERRBASE_IC + ERRCODE_BLOBNOTOPENED);
  DBIERR_NA                     = (ERRBASE_IC + ERRCODE_NA);
  DBIERR_NOTINDEXED             = (ERRBASE_IC + ERRCODE_NOTINDEXED);
  DBIERR_NOTINITIALIZED         = (ERRBASE_IC + ERRCODE_NOTINITIALIZED);
  DBIERR_MULTIPLEINIT           = (ERRBASE_IC + ERRCODE_MULTIPLEINIT);
  DBIERR_NOTSAMESESSION         = (ERRBASE_IC + ERRCODE_NOTSAMESESSION);
  DBIERR_PDXDRIVERNOTACTIVE     = (ERRBASE_IC + ERRCODE_PDXDRIVERNOTACTIVE);
  DBIERR_DRIVERNOTLOADED        = (ERRBASE_IC + ERRCODE_DRIVERNOTLOADED);
  DBIERR_TABLEREADONLY          = (ERRBASE_IC + ERRCODE_TABLEREADONLY);
  DBIERR_NOASSOCINDEX           = (ERRBASE_IC + ERRCODE_NOASSOCINDEX);
  DBIERR_HASOPENCURSORS         = (ERRBASE_IC + ERRCODE_HASOPENCURSORS);
  DBIERR_NOTABLESUPPORT         = (ERRBASE_IC + ERRCODE_NOTABLESUPPORT);
  DBIERR_INDEXREADONLY          = (ERRBASE_IC + ERRCODE_INDEXREADONLY);
  DBIERR_NOUNIQUERECS           = (ERRBASE_IC + ERRCODE_NOUNIQUERECS);
  DBIERR_NOTCURSESSION          = (ERRBASE_IC + ERRCODE_NOTCURSESSION);
  DBIERR_INVALIDKEYWORD         = (ERRBASE_IC + ERRCODE_INVALIDKEYWORD);
  DBIERR_CONNECTINUSE           = (ERRBASE_IC + ERRCODE_CONNECTINUSE);
  DBIERR_CONNECTNOTSHARED       = (ERRBASE_IC + ERRCODE_CONNECTNOTSHARED);


{ ERRCAT_OS }
{ ========= }
{ DOS extended errors: }

  ERRCODE_OSEINVFNC             = 1;      { Invalid function number }
  ERRCODE_OSENOENT              = 2;      { No such file or directory }
  ERRCODE_OSENOPATH             = 3;      { Path not found }
  ERRCODE_OSEMFILE              = 4;      { Too many open files }
  ERRCODE_OSEACCES              = 5;      { Permission denied }
  ERRCODE_OSEBADF               = 6;      { Bad file number }
  ERRCODE_OSECONTR              = 7;      { Memory blocks destroyed }
  ERRCODE_OSENOMEM              = 8;      { Not enough core }
  ERRCODE_OSEINVMEM             = 9;      { Invalid memory block address }
  ERRCODE_OSEINVENV             = 10;     { 0x0a Invalid environment }
  ERRCODE_OSEINVFMT             = 11;     { 0x0b Invalid format }
  ERRCODE_OSEINVACC             = 12;     { 0x0c Invalid access code }
  ERRCODE_OSEINVDAT             = 13;     { 0x0d Invalid data }
  ERRCODE_OSENODEV              = 15;     { 0x0f No such device }
  ERRCODE_OSECURDIR             = 16;     { 0x10 Attempt to remove curdir }
  ERRCODE_OSENOTSAM             = 17;     { 0x11 Not same device }
  ERRCODE_OSENMFILE             = 18;     { 0x12 No more files }
  ERRCODE_OSEINVAL              = 19;     { 0x13 Invalid argument }
  ERRCODE_OSE2BIG               = 20;     { 0x14 Arg list too long }
  ERRCODE_OSENOEXEC             = 21;     { 0x15 Exec format error }
  ERRCODE_OSEXDEV               = 22;     { 0x16 Cross-device link }
  ERRCODE_OSEDOM                = 33;     { 0x21 Math argument }
  ERRCODE_OSERANGE              = 34;     { 0x22 Result to large }
  ERRCODE_OSEEXIST              = 35;     { 0x23 File already exists }
  ERRCODE_OSUNKNOWN             = 39;     { 0x27 Unkown | illegal error from rtl }

  ERRCODE_OSSHAREVIOL           = 50;     { 0x32 Share viol, ext. err 0x20 }
  ERRCODE_OSLOCKVIOL            = 51;     { 0x33 Lock viol, ext. err 0x21 }
  ERRCODE_OSINT24FAIL           = 52;     { 0x34 INT24 called }
  ERRCODE_OSDRIVENOTREADY       = 53;     { 0x35 Drive not ready }



{ OTHER Os errors: }
{ 1. idapi errors  }
{ 2. errors from non-dos systems ( i.e. NOVELL ) }

  ERRCODE_NOTEXACT              = 100;    { 0x64 Not exact read/write }
  ERRCODE_OSNETERR              = 101;    { 0x65 Generic network error }
  ERRCODE_OSUNKNOWNSRVERR       = 102;    { 0x66 Error from file server }
  ERRCODE_SERVERNOMEMORY        = 103;    { 0x67 Server out of memory }
  ERRCODE_OSALREADYLOCKED       = 104;    { 0x68 Record already locked (by you) }
  ERRCODE_OSNOTLOCKED           = 105;    { 0x69 Record not locked }
  ERRCODE_NOSERVERSW            = 106;    { 0x6a Server software not running the workstation/server }


  DBIERR_OSEINVFNC              = ( ERRBASE_OS + ERRCODE_OSEINVFNC );
  DBIERR_OSENOENT               = ( ERRBASE_OS + ERRCODE_OSENOENT );
  DBIERR_OSENOPATH              = ( ERRBASE_OS + ERRCODE_OSENOPATH );
  DBIERR_OSEMFILE               = ( ERRBASE_OS + ERRCODE_OSEMFILE );
  DBIERR_OSEACCES               = ( ERRBASE_OS + ERRCODE_OSEACCES );
  DBIERR_OSEBADF                = ( ERRBASE_OS + ERRCODE_OSEBADF );
  DBIERR_OSECONTR               = ( ERRBASE_OS + ERRCODE_OSECONTR );
  DBIERR_OSENOMEM               = ( ERRBASE_OS + ERRCODE_OSENOMEM );
  DBIERR_OSEINVMEM              = ( ERRBASE_OS + ERRCODE_OSEINVMEM );
  DBIERR_OSEINVENV              = ( ERRBASE_OS + ERRCODE_OSEINVENV );
  DBIERR_OSEINVFMT              = ( ERRBASE_OS + ERRCODE_OSEINVFMT );
  DBIERR_OSEINVACC              = ( ERRBASE_OS + ERRCODE_OSEINVACC );
  DBIERR_OSEINVDAT              = ( ERRBASE_OS + ERRCODE_OSEINVDAT );
  DBIERR_OSENODEV               = ( ERRBASE_OS + ERRCODE_OSENODEV );
  DBIERR_OSECURDIR              = ( ERRBASE_OS + ERRCODE_OSECURDIR );
  DBIERR_OSENOTSAM              = ( ERRBASE_OS + ERRCODE_OSENOTSAM );
  DBIERR_OSENMFILE              = ( ERRBASE_OS + ERRCODE_OSENMFILE );
  DBIERR_OSEINVAL               = ( ERRBASE_OS + ERRCODE_OSEINVAL );
  DBIERR_OSE2BIG                = ( ERRBASE_OS + ERRCODE_OSE2BIG );
  DBIERR_OSENOEXEC              = ( ERRBASE_OS + ERRCODE_OSENOEXEC );
  DBIERR_OSEXDEV                = ( ERRBASE_OS + ERRCODE_OSEXDEV );
  DBIERR_OSEDOM                 = ( ERRBASE_OS + ERRCODE_OSEDOM );
  DBIERR_OSERANGE               = ( ERRBASE_OS + ERRCODE_OSERANGE );
  DBIERR_OSEEXIST               = ( ERRBASE_OS + ERRCODE_OSEEXIST );
  DBIERR_OSUNKNOWN              = ( ERRBASE_OS + ERRCODE_OSUNKNOWN );
  DBIERR_OSSHAREVIOL            = ( ERRBASE_OS + ERRCODE_OSSHAREVIOL );
  DBIERR_OSLOCKVIOL             = ( ERRBASE_OS + ERRCODE_OSLOCKVIOL );
  DBIERR_OSNETERR               = ( ERRBASE_OS + ERRCODE_OSNETERR );
  DBIERR_OSINT24FAIL            = ( ERRBASE_OS + ERRCODE_OSINT24FAIL );
  DBIERR_OSDRIVENOTREADY        = ( ERRBASE_OS + ERRCODE_OSDRIVENOTREADY );


  DBIERR_NOTEXACT               = ( ERRBASE_OS + ERRCODE_NOTEXACT );
  DBIERR_OSUNKNOWNSRVERR        = ( ERRBASE_OS + ERRCODE_OSUNKNOWNSRVERR );
  DBIERR_SERVERNOMEMORY         = ( ERRBASE_OS + ERRCODE_SERVERNOMEMORY );
  DBIERR_OSALREADYLOCKED        = ( ERRBASE_OS + ERRCODE_OSALREADYLOCKED );
  DBIERR_OSNOTLOCKED            = ( ERRBASE_OS + ERRCODE_OSNOTLOCKED );
  DBIERR_NOSERVERSW             = ( ERRBASE_OS + ERRCODE_NOSERVERSW);

{ ERRCAT_NETWORK }
{ ============== }

  ERRCODE_NETINITERR            = 1;      { Net init failed }
  ERRCODE_NETUSERLIMIT          = 2;      { Net user limit exceeded }
  ERRCODE_NETFILEVERSION        = 3;      { Wrong net file version }
  ERRCODE_NETFILELOCKED         = 4;      { Not able to lock net file }
  ERRCODE_DIRNOTPRIVATE         = 5;
  ERRCODE_NETMULTIPLE           = 6;      { Multiple net files in use }
  ERRCODE_NETUNKNOWN            = 7;      { Unknown net error }
  ERRCODE_SHAREDFILE            = 8;      { Cannot access a shared file }
  ERRCODE_SHARENOTLOADED        = 9;      { Share not loaded }
  ERRCODE_NOTONANETWORK         = 10;     { 0x0a Not an Network }
  ERRCODE_SQLCOMMLOST           = 11;     { 0x0b Lost Communication with SQL server }
  ERRCODE_SERVERCOMMLOST        = 12;     { 0x0c Lost Communication with IDAPI server }
  ERRCODE_SQLSERVERNOTFOUND     = 13;     { 0x0d SQL Server not found }
  ERRCODE_SERVERNOTFOUND        = 14;     { 0x0e SQL Server not found }

  DBIERR_NETINITERR             = (ERRBASE_NETWORK + ERRCODE_NETINITERR);
  DBIERR_NETUSERLIMIT           = (ERRBASE_NETWORK + ERRCODE_NETUSERLIMIT);
  DBIERR_NETFILEVERSION         = (ERRBASE_NETWORK + ERRCODE_NETFILEVERSION);
  DBIERR_NETFILELOCKED          = (ERRBASE_NETWORK + ERRCODE_NETFILELOCKED);
  DBIERR_DIRNOTPRIVATE          = (ERRBASE_NETWORK + ERRCODE_DIRNOTPRIVATE);
  DBIERR_NETMULTIPLE            = (ERRBASE_NETWORK + ERRCODE_NETMULTIPLE);
  DBIERR_NETUNKNOWN             = (ERRBASE_NETWORK + ERRCODE_NETUNKNOWN);
  DBIERR_SHAREDFILE             = (ERRBASE_NETWORK + ERRCODE_SHAREDFILE);
  DBIERR_SHARENOTLOADED         = (ERRBASE_NETWORK + ERRCODE_SHARENOTLOADED);
  DBIERR_NOTONANETWORK          = (ERRBASE_NETWORK + ERRCODE_NOTONANETWORK);
  DBIERR_SQLCOMMLOST            = (ERRBASE_NETWORK + ERRCODE_SQLCOMMLOST);
  DBIERR_SERVERCOMMLOST         = (ERRBASE_NETWORK + ERRCODE_SERVERCOMMLOST);
  DBIERR_SQLSERVERNOTFOUND      = (ERRBASE_NETWORK + ERRCODE_SQLSERVERNOTFOUND);
  DBIERR_SERVERNOTFOUND         = (ERRBASE_NETWORK + ERRCODE_SERVERNOTFOUND);

{ ERRCAT_DRIVER }
{ ============= }

  ERRCODE_WRONGDRVNAME          = 1;      { Wrong driver name }
  ERRCODE_WRONGSYSVER           = 2;      { Wrong system version }
  ERRCODE_WRONGDRVVER           = 3;      { Wrong driver version }
  ERRCODE_WRONGDRVTYPE          = 4;      { Wrong driver type }
  ERRCODE_CANNOTLOADDRV         = 5;      { Can not load driver }
  ERRCODE_CANNOTLOADLDDRV       = 6;      { Can not load language driver }
  ERRCODE_VENDINITFAIL          = 7;      { Vendor init failure }
  ERRCODE_DRIVERRESTRICTED      = 8;      { Client not enabled for this driver }


  DBIERR_WRONGDRVNAME           = (ERRBASE_DRIVER + ERRCODE_WRONGDRVNAME);
  DBIERR_WRONGSYSVER            = (ERRBASE_DRIVER + ERRCODE_WRONGSYSVER);
  DBIERR_WRONGDRVVER            = (ERRBASE_DRIVER + ERRCODE_WRONGDRVVER);
  DBIERR_WRONGDRVTYPE           = (ERRBASE_DRIVER + ERRCODE_WRONGDRVTYPE);
  DBIERR_CANNOTLOADDRV          = (ERRBASE_DRIVER + ERRCODE_CANNOTLOADDRV);
  DBIERR_CANNOTLOADLDDRV        = (ERRBASE_DRIVER + ERRCODE_CANNOTLOADLDDRV);
  DBIERR_VENDINITFAIL           = (ERRBASE_DRIVER + ERRCODE_VENDINITFAIL);
  DBIERR_DRIVERRESTRICTED       = (ERRBASE_DRIVER + ERRCODE_DRIVERRESTRICTED);


{ ERRCAT_QUERY }
{ ============ }



  DBICODE_AMBJOASY              = 1;      { obsolete }
  DBICODE_AMBJOSYM              = 2;      { obsolete }
  DBICODE_AMBOUTEX              = 3;
  DBICODE_AMBOUTPR              = 4;      { obsolete }
  DBICODE_AMBSYMAS              = 5;      { obsolete }
  DBICODE_ASETOPER              = 6;
  DBICODE_AVENUMDA              = 7;
  DBICODE_BADEXPR1              = 8;
  DBICODE_BADFLDOR              = 9;
  DBICODE_BADVNAME              = 10;     { 0x0a }
  DBICODE_BITMAPER              = 11;     { 0x0b }
  DBICODE_CALCBADR              = 12;     { 0x0c }
  DBICODE_CALCTYPE              = 13;     { 0x0d }
  DBICODE_CHGTO1TI              = 14;     { 0x0e }
  DBICODE_CHGTOCHG              = 15;     { 0x0f }
  DBICODE_CHGTOEXP              = 16;     { 0x10 }
  DBICODE_CHGTOINS              = 17;     { 0x11 }
  DBICODE_CHGTONEW              = 18;     { 0x12 }
  DBICODE_CHGTOVAL              = 19;     { 0x13 }
  DBICODE_CHKMRKFI              = 20;     { 0x14 }
  DBICODE_CHNAMBIG              = 21;     { 0x15 }
  DBICODE_CHUNKERR              = 22;     { 0x16 }
  DBICODE_COLUM255              = 23;     { 0x17 }
  DBICODE_CONAFTAS              = 24;     { 0x18 }
  DBICODE_DEL1TIME              = 25;     { 0x19 }
  DBICODE_DELAMBIG              = 26;     { 0x1a }
  DBICODE_DELFRDEL              = 27;     { 0x1b }
  DBICODE_EGFLDTYP              = 28;     { 0x1c }
  DBICODE_EXAMINOR              = 29;     { 0x1d }
  DBICODE_EXPRTYPS              = 30;     { 0x1e }
  DBICODE_EXTRACOM              = 31;     { 0x1f }
  DBICODE_EXTRAORO              = 32;     { 0x20 }
  DBICODE_EXTRAQRO              = 33;     { 0x21 }
  DBICODE_FIND1ATT              = 34;     { 0x22 }
  DBICODE_FINDANST              = 35;     { 0x23 }
  DBICODE_GRPNOSET              = 36;     { 0x24 }
  DBICODE_GRPSTROW              = 37;     { 0x25 }
  DBICODE_IDFINLCO              = 38;     { 0x26 }
  DBICODE_IDFPERLI              = 39;     { 0x27 }
  DBICODE_INANEXPR              = 40;     { 0x28 }
  DBICODE_INS1TIME              = 41;     { 0x29 }
  DBICODE_INSAMBIG              = 42;     { 0x2a }
  DBICODE_INSDELCH              = 43;     { 0x2b }
  DBICODE_INSEXPRR              = 44;     { 0x2c }
  DBICODE_INSTOINS              = 45;     { 0x2d }
  DBICODE_ISARRAY               = 46;     { 0x2e }
  DBICODE_LABELERR              = 47;     { 0x2f }
  DBICODE_LINKCALC              = 48;     { 0x30 }
  DBICODE_LNGVNAME              = 49;     { 0x31 }
  DBICODE_LONGQURY              = 50;     { 0x32 }
  DBICODE_MEMVPROC              = 51;     { 0x33 }
  DBICODE_MISNGCOM              = 52;     { 0x34 }
  DBICODE_MISNGRPA              = 53;     { 0x35 }
  DBICODE_MISSRTQU              = 54;     { 0x36 }
  DBICODE_NAMTWICE              = 55;     { 0x37 }
  DBICODE_NOCHKMAR              = 56;     { 0x38 }
  DBICODE_NODEFOCC              = 57;     { 0x39 }
  DBICODE_NOGROUPS              = 58;     { 0x3a }
  DBICODE_NONSENSE              = 59;     { 0x3b }
  DBICODE_NOPATTER              = 60;     { 0x3c }
  DBICODE_NOSUCHDA              = 61;     { 0x3d }
  DBICODE_NOVALUE               = 62;     { 0x3e }
  DBICODE_ONLYCONS              = 63;     { 0x3f }
  DBICODE_ONLYSETR              = 64;     { 0x40 }
  DBICODE_OUTSENS1              = 65;     { 0x41 }
  DBICODE_OUTTWIC1              = 66;     { 0x42 }
  DBICODE_PAROWCNT              = 67;     { 0x43 }
  DBICODE_PERSEPAR              = 68;     { 0x44 }
  DBICODE_PROCPLSW              = 69;     { 0x45 }
  DBICODE_PWINSRTS              = 70;     { 0x46 }
  DBICODE_PWMODRTS              = 71;     { 0x47 }
  DBICODE_QBEFLDFOUND           = 72;     { 0x48 }
  DBICODE_QBENOFENCE            = 73;     { 0x49 }
  DBICODE_QBENOFENCET           = 74;     { 0x4a }
  DBICODE_QBENOHEADERT          = 75;     { 0x4b }
  DBICODE_QBENOTAB              = 76;     { 0x4c }
  DBICODE_QBENUMCOLS            = 77;     { 0x4d }
  DBICODE_QBEOPENTAB            = 78;     { 0x4e }
  DBICODE_QBETWICE              = 79;     { 0x4f }
  DBICODE_QRYNOANSWER           = 80;     { 0x50 }
  DBICODE_QRYNOTPREP            = 81;     { 0x51 }
  DBICODE_QUAINDEL              = 82;     { 0x52 }
  DBICODE_QUAININS              = 83;     { 0x53 }
  DBICODE_RAGININS              = 84;     { 0x54 }
  DBICODE_RAGINSET              = 85;     { 0x55 }
  DBICODE_ROWUSERR              = 86;     { 0x56 }
  DBICODE_SETEXPEC              = 87;     { 0x57 }
  DBICODE_SETVAMB1              = 88;     { 0x58 }
  DBICODE_SETVBAD1              = 89;     { 0x59 }
  DBICODE_SETVDEF1              = 90;     { 0x5a }
  DBICODE_SUMNUMBE              = 91;     { 0x5b }
  DBICODE_TBLISWP3              = 92;     { 0x5c }
  DBICODE_TOKENNOT              = 93;     { 0x5d }
  DBICODE_TWOOUTR1              = 94;     { 0x5e }
  DBICODE_TYPEMISM              = 95;     { 0x5f }
  DBICODE_UNRELQ1               = 96;     { 0x60 }
  DBICODE_UNUSEDST              = 97;     { 0x61 }
  DBICODE_USEINSDE              = 98;     { 0x62 }
  DBICODE_USEOFCHG              = 99;     { 0x63 }
  DBICODE_VARMUSTF              = 100;    { 0x64 }
  DBICODE_REGISTER              = 101;    { 0x65 }
  DBICODE_LONGEXPR              = 102;    { 0x66 }
  DBICODE_REFRESH               = 103;    { 0x67 }
  DBICODE_CANCEXCEPT            = 104;    { 0x68 }
  DBICODE_DBEXCEPT              = 105;    { 0x69 }
  DBICODE_MEMEXCEPT             = 106;    { 0x6a }
  DBICODE_FATALEXCEPT           = 107;    { 0x6b }
  DBICODE_QRYNIY                = 108;    { 0x6c }
  DBICODE_BADFORMAT             = 109;    { 0x6d }
  DBICODE_QRYEMPTY              = 110;    { 0x6e }
  DBICODE_NOQRYTOPREP           = 111;    { 0x6f }
  DBICODE_BUFFTOOSMALL          = 112;    { 0x70 }
  DBICODE_QRYNOTPARSE           = 113;    { 0x71 }
  DBICODE_NOTHANDLE             = 114;    { 0x72 }
  DBICODE_QRYSYNTERR            = 115;    { 0x73 }
  DBICODE_QXFLDCOUNT            = 116;    { 0x74 }
  DBICODE_QXFLDSYMNOTFOUND      = 117;    { 0x75 }
  DBICODE_QXTBLSYMNOTFOUND      = 118;    { 0x76 }
  DBICODE_BLOBTERM              = 119;    { 0x77 }
  DBICODE_BLOBERR               = 120;    { 0x78 }
  DBICODE_RESTARTQRY            = 121;    { 0x79 }
  DBICODE_UNKNOWNANSTYPE        = 122;    { 0x7a }

{ Internal QBE use Only. }
  DBICODE_SQLG_MDIST            = 123;    { 0x7b }
  DBICODE_SQLG_NOARI            = 124;    { 0x7c }
  DBICODE_SQLG_LIKEN            = 125;    { 0x7d }
  DBICODE_SQLG_ALPHO            = 126;    { 0x7e }
  DBICODE_SQLG_DATEO            = 127;    { 0x7f }
  DBICODE_SQLG_RELOP            = 128;    { 0x80 }
  DBICODE_SQLG_ONLYC            = 129;    { 0x81 }
  DBICODE_SQLG_CNTLN            = 130;    { 0x82 }
  DBICODE_SQLG_CHINI            = 131;    { 0x83 }
  DBICODE_SQLG_UNION            = 132;    { 0x84 }
  DBICODE_SQLG_SLFIN            = 133;    { 0x85 }
  DBICODE_SQLG_OTJVR            = 134;    { 0x86 }
  DBICODE_SQLG_STROW            = 135;    { 0x87 }
  DBICODE_SQLG_QUANT            = 136;    { 0x88 }
  DBICODE_SQLG_REGSO            = 137;    { 0x89 }
  DBICODE_SQLG_COUNT            = 138;    { 0x8a }
  DBICODE_SQLG_AVERA            = 139;    { 0x8b }
  DBICODE_SQLG_DATEA            = 140;    { 0x8c }
  DBICODE_SQLG_BADPT            = 141;    { 0x8d }
  DBICODE_SQLG_RELPA            = 142;    { 0x8e }
  DBICODE_SQLG_PATRN            = 143;    { 0x8f }
  DBICODE_SQLG_FNDSU            = 144;    { 0x90 }
  DBICODE_SQLG_IFDCS            = 145;    { 0x91 }
  DBICODE_SQLG_IDCCO            = 146;    { 0x92 }
  DBICODE_SQLG_ONLYI            = 147;    { 0x93 }
  DBICODE_SQLG_SQLDIALECT       = 148;    { 0x94 }
  DBICODE_SQLG_NOQUERY          = 149;    { 0x95 }
{ End of Internal.       }

  DBICODE_BLOBGROUP             = 150;    { 0x96 }
  DBICODE_QRYNOPROP             = 151;    { 0x97 }
  DBICODE_ANSTYPNOTSUP          = 152;    { 0x98 }
  DBICODE_ANSALIASNOTSUP        = 153;    { 0x99 }
  DBICODE_INSBLOBREQ            = 154;    { 0x9a }
  DBICODE_CHGUNIQUENDXREQ       = 155;    { 0x9b }
  DBICODE_DELUNIQUENDXREQ       = 156;    { 0x9c }
  DBICODE_SQLNOFULLUPDATE       = 157;    { 0x9d }
  DBICODE_CANTEXECREMOTE        = 158;    { 0x9e }
  DBICODE_UNEXPECTEDEOC         = 159;    { 0x9f }
  DBICODE_SQLPARAMNOTSET        = 160;    { 0xA0 }
  DBICODE_QUERYTOOLONG          = 161;    { 0xA1 }

{ Errors added for localsql }
  DBICODE_NOSUCHRELORALIAS      = 170;
  DBICODE_TYPEAMBIGUITY         = 171;
  DBICODE_ORDERBYNOTAPROJ       = 172;
  DBICODE_SQLPARSE              = 173;
  DBICODE_CONSTRAINTFAILED      = 174;
  DBICODE_NOTGROUPINGFIELD      = 175;
  DBICODE_UDFNOTDEFINED         = 176;
  DBICODE_UDFERROR              = 177;
  DBICODE_SINGLEROWERROR        = 178;
  DBICODE_GROUPEXPR             = 179;
  DBICODE_QUERYTEXT             = 180;
  DBICODE_ANSIJOINSUP           = 181;
  DBICODE_DISTUNION             = 182;
  DBICODE_GROUPBYREQ            = 183;
  DBICODE_INSUPDAUTOIC          = 184;
  DBICODE_UPDREFINTSINGLE       = 185;
  DBICODE_NOMSACCESS            = 186;
  DBICODE_FIELDCONSTPREP        = 187;
  DBICODE_FIELDDEFPREP          = 188;
  DBICODE_RECCONSTPREP          = 189;

  DBICODE_TOOFEWSUBEXPR         = 190;    { 0xBE }
  DBICODE_TOOMANYSUBEXPR        = 191;    { 0xBF }
  DBICODE_INVALIDNODETYPE       = 192;    { 0xC0 }
  DBICODE_BOOLEXPR              = 193;    { 0xC1 }
  DBICODE_NONBOOLEXPR           = 194;    { 0xC2 }
  DBICODE_NOOUTERJOIN           = 195;    { 0xC3 }
  DBICODE_USERCONSTRERR         = 196;    { 0xC4 }


  DBIERR_AMBJOASY               = (ERRBASE_QUERY+DBICODE_AMBJOASY);
  DBIERR_AMBJOSYM               = (ERRBASE_QUERY+DBICODE_AMBJOSYM);
  DBIERR_AMBOUTEX               = (ERRBASE_QUERY+DBICODE_AMBOUTEX);
  DBIERR_AMBOUTPR               = (ERRBASE_QUERY+DBICODE_AMBOUTPR);
  DBIERR_AMBSYMAS               = (ERRBASE_QUERY+DBICODE_AMBSYMAS);
  DBIERR_ASETOPER               = (ERRBASE_QUERY+DBICODE_ASETOPER);
  DBIERR_AVENUMDA               = (ERRBASE_QUERY+DBICODE_AVENUMDA);
  DBIERR_BADEXPR1               = (ERRBASE_QUERY+DBICODE_BADEXPR1);
  DBIERR_BADFLDOR               = (ERRBASE_QUERY+DBICODE_BADFLDOR);
  DBIERR_BADVNAME               = (ERRBASE_QUERY+DBICODE_BADVNAME);
  DBIERR_BITMAPER               = (ERRBASE_QUERY+DBICODE_BITMAPER);
  DBIERR_CALCBADR               = (ERRBASE_QUERY+DBICODE_CALCBADR);
  DBIERR_CALCTYPE               = (ERRBASE_QUERY+DBICODE_CALCTYPE);
  DBIERR_CHGTO1TI               = (ERRBASE_QUERY+DBICODE_CHGTO1TI);
  DBIERR_CHGTOCHG               = (ERRBASE_QUERY+DBICODE_CHGTOCHG);
  DBIERR_CHGTOEXP               = (ERRBASE_QUERY+DBICODE_CHGTOEXP);
  DBIERR_CHGTOINS               = (ERRBASE_QUERY+DBICODE_CHGTOINS);
  DBIERR_CHGTONEW               = (ERRBASE_QUERY+DBICODE_CHGTONEW);
  DBIERR_CHGTOVAL               = (ERRBASE_QUERY+DBICODE_CHGTOVAL);
  DBIERR_CHKMRKFI               = (ERRBASE_QUERY+DBICODE_CHKMRKFI);
  DBIERR_CHNAMBIG               = (ERRBASE_QUERY+DBICODE_CHNAMBIG);
  DBIERR_CHUNKERR               = (ERRBASE_QUERY+DBICODE_CHUNKERR);
  DBIERR_COLUM255               = (ERRBASE_QUERY+DBICODE_COLUM255);
  DBIERR_CONAFTAS               = (ERRBASE_QUERY+DBICODE_CONAFTAS);
  DBIERR_DEL1TIME               = (ERRBASE_QUERY+DBICODE_DEL1TIME);
  DBIERR_DELAMBIG               = (ERRBASE_QUERY+DBICODE_DELAMBIG);
  DBIERR_DELFRDEL               = (ERRBASE_QUERY+DBICODE_DELFRDEL);
  DBIERR_EGFLDTYP               = (ERRBASE_QUERY+DBICODE_EGFLDTYP);
  DBIERR_EXAMINOR               = (ERRBASE_QUERY+DBICODE_EXAMINOR);
  DBIERR_EXPRTYPS               = (ERRBASE_QUERY+DBICODE_EXPRTYPS);
  DBIERR_EXTRACOM               = (ERRBASE_QUERY+DBICODE_EXTRACOM);
  DBIERR_EXTRAORO               = (ERRBASE_QUERY+DBICODE_EXTRAORO);
  DBIERR_EXTRAQRO               = (ERRBASE_QUERY+DBICODE_EXTRAQRO);
  DBIERR_FIND1ATT               = (ERRBASE_QUERY+DBICODE_FIND1ATT);
  DBIERR_FINDANST               = (ERRBASE_QUERY+DBICODE_FINDANST);
  DBIERR_GRPNOSET               = (ERRBASE_QUERY+DBICODE_GRPNOSET);
  DBIERR_GRPSTROW               = (ERRBASE_QUERY+DBICODE_GRPSTROW);
  DBIERR_IDFINLCO               = (ERRBASE_QUERY+DBICODE_IDFINLCO);
  DBIERR_IDFPERLI               = (ERRBASE_QUERY+DBICODE_IDFPERLI);
  DBIERR_INANEXPR               = (ERRBASE_QUERY+DBICODE_INANEXPR);
  DBIERR_INS1TIME               = (ERRBASE_QUERY+DBICODE_INS1TIME);
  DBIERR_INSAMBIG               = (ERRBASE_QUERY+DBICODE_INSAMBIG);
  DBIERR_INSDELCH               = (ERRBASE_QUERY+DBICODE_INSDELCH);
  DBIERR_INSEXPRR               = (ERRBASE_QUERY+DBICODE_INSEXPRR);
  DBIERR_INSTOINS               = (ERRBASE_QUERY+DBICODE_INSTOINS);
  DBIERR_ISARRAY                = (ERRBASE_QUERY+DBICODE_ISARRAY);
  DBIERR_LABELERR               = (ERRBASE_QUERY+DBICODE_LABELERR);
  DBIERR_LINKCALC               = (ERRBASE_QUERY+DBICODE_LINKCALC);
  DBIERR_LNGVNAME               = (ERRBASE_QUERY+DBICODE_LNGVNAME);
  DBIERR_LONGQURY               = (ERRBASE_QUERY+DBICODE_LONGQURY);
  DBIERR_MEMVPROC               = (ERRBASE_QUERY+DBICODE_MEMVPROC);
  DBIERR_MISNGCOM               = (ERRBASE_QUERY+DBICODE_MISNGCOM);
  DBIERR_MISNGRPA               = (ERRBASE_QUERY+DBICODE_MISNGRPA);
  DBIERR_MISSRTQU               = (ERRBASE_QUERY+DBICODE_MISSRTQU);
  DBIERR_NAMTWICE               = (ERRBASE_QUERY+DBICODE_NAMTWICE);
  DBIERR_NOCHKMAR               = (ERRBASE_QUERY+DBICODE_NOCHKMAR);
  DBIERR_NODEFOCC               = (ERRBASE_QUERY+DBICODE_NODEFOCC);
  DBIERR_NOGROUPS               = (ERRBASE_QUERY+DBICODE_NOGROUPS);
  DBIERR_NONSENSE               = (ERRBASE_QUERY+DBICODE_NONSENSE);
  DBIERR_NOPATTER               = (ERRBASE_QUERY+DBICODE_NOPATTER);
  DBIERR_NOSUCHDA               = (ERRBASE_QUERY+DBICODE_NOSUCHDA);
  DBIERR_NOVALUE                = (ERRBASE_QUERY+DBICODE_NOVALUE);
  DBIERR_ONLYCONS               = (ERRBASE_QUERY+DBICODE_ONLYCONS);
  DBIERR_ONLYSETR               = (ERRBASE_QUERY+DBICODE_ONLYSETR);
  DBIERR_OUTSENS1               = (ERRBASE_QUERY+DBICODE_OUTSENS1);
  DBIERR_OUTTWIC1               = (ERRBASE_QUERY+DBICODE_OUTTWIC1);
  DBIERR_PAROWCNT               = (ERRBASE_QUERY+DBICODE_PAROWCNT);
  DBIERR_PERSEPAR               = (ERRBASE_QUERY+DBICODE_PERSEPAR);
  DBIERR_PROCPLSW               = (ERRBASE_QUERY+DBICODE_PROCPLSW);
  DBIERR_PWINSRTS               = (ERRBASE_QUERY+DBICODE_PWINSRTS);
  DBIERR_PWMODRTS               = (ERRBASE_QUERY+DBICODE_PWMODRTS);
  DBIERR_QBEFLDFOUND            = (ERRBASE_QUERY+DBICODE_QBEFLDFOUND);
  DBIERR_QBENOFENCE             = (ERRBASE_QUERY+DBICODE_QBENOFENCE);
  DBIERR_QBENOFENCET            = (ERRBASE_QUERY+DBICODE_QBENOFENCET);
  DBIERR_QBENOHEADERT           = (ERRBASE_QUERY+DBICODE_QBENOHEADERT);
  DBIERR_QBENOTAB               = (ERRBASE_QUERY+DBICODE_QBENOTAB);
  DBIERR_QBENUMCOLS             = (ERRBASE_QUERY+DBICODE_QBENUMCOLS);
  DBIERR_QBEOPENTAB             = (ERRBASE_QUERY+DBICODE_QBEOPENTAB);
  DBIERR_QBETWICE               = (ERRBASE_QUERY+DBICODE_QBETWICE);
  DBIERR_QRYNOANSWER            = (ERRBASE_QUERY+DBICODE_QRYNOANSWER);
  DBIERR_QRYNOTPREP             = (ERRBASE_QUERY+DBICODE_QRYNOTPREP);
  DBIERR_QUAINDEL               = (ERRBASE_QUERY+DBICODE_QUAINDEL);
  DBIERR_QUAININS               = (ERRBASE_QUERY+DBICODE_QUAININS);
  DBIERR_RAGININS               = (ERRBASE_QUERY+DBICODE_RAGININS);
  DBIERR_RAGINSET               = (ERRBASE_QUERY+DBICODE_RAGINSET);
  DBIERR_ROWUSERR               = (ERRBASE_QUERY+DBICODE_ROWUSERR);
  DBIERR_SETEXPEC               = (ERRBASE_QUERY+DBICODE_SETEXPEC);
  DBIERR_SETVAMB1               = (ERRBASE_QUERY+DBICODE_SETVAMB1);
  DBIERR_SETVBAD1               = (ERRBASE_QUERY+DBICODE_SETVBAD1);
  DBIERR_SETVDEF1               = (ERRBASE_QUERY+DBICODE_SETVDEF1);
  DBIERR_SUMNUMBE               = (ERRBASE_QUERY+DBICODE_SUMNUMBE);
  DBIERR_TBLISWP3               = (ERRBASE_QUERY+DBICODE_TBLISWP3);
  DBIERR_TOKENNOT               = (ERRBASE_QUERY+DBICODE_TOKENNOT);
  DBIERR_TWOOUTR1               = (ERRBASE_QUERY+DBICODE_TWOOUTR1);
  DBIERR_TYPEMISM               = (ERRBASE_QUERY+DBICODE_TYPEMISM);
  DBIERR_UNRELQ1                = (ERRBASE_QUERY+DBICODE_UNRELQ1);
  DBIERR_UNUSEDST               = (ERRBASE_QUERY+DBICODE_UNUSEDST);
  DBIERR_USEINSDE               = (ERRBASE_QUERY+DBICODE_USEINSDE);
  DBIERR_USEOFCHG               = (ERRBASE_QUERY+DBICODE_USEOFCHG);
  DBIERR_VARMUSTF               = (ERRBASE_QUERY+DBICODE_VARMUSTF);
  DBIERR_REGISTER               = (ERRBASE_QUERY+DBICODE_REGISTER);
  DBIERR_LONGEXPR               = (ERRBASE_QUERY+DBICODE_LONGEXPR);
  DBIERR_REFRESH                = (ERRBASE_QUERY+DBICODE_REFRESH);
  DBIERR_CANCEXCEPT             = (ERRBASE_QUERY+DBICODE_CANCEXCEPT);
  DBIERR_DBEXCEPT               = (ERRBASE_QUERY+DBICODE_DBEXCEPT);
  DBIERR_MEMEXCEPT              = (ERRBASE_QUERY+DBICODE_MEMEXCEPT);
  DBIERR_FATALEXCEPT            = (ERRBASE_QUERY+DBICODE_FATALEXCEPT);
  DBIERR_QRYNIY                 = (ERRBASE_QUERY+ DBICODE_QRYNIY);
  DBIERR_BADFORMAT              = (ERRBASE_QUERY+ DBICODE_BADFORMAT);
  DBIERR_QRYEMPTY               = (ERRBASE_QUERY+ DBICODE_QRYEMPTY);
  DBIERR_NOQRYTOPREP            = (ERRBASE_QUERY+ DBICODE_NOQRYTOPREP);
  DBIERR_BUFFTOOSMALL           = (ERRBASE_QUERY+ DBICODE_BUFFTOOSMALL);
  DBIERR_QRYNOTPARSE            = (ERRBASE_QUERY+ DBICODE_QRYNOTPARSE);
  DBIERR_NOTHANDLE              = (ERRBASE_QUERY+ DBICODE_NOTHANDLE);
  DBIERR_QRYSYNTERR             = (ERRBASE_QUERY+ DBICODE_QRYSYNTERR);
  DBIERR_QXFLDCOUNT             = (ERRBASE_QUERY+ DBICODE_QXFLDCOUNT);
  DBIERR_QXFLDSYMNOTFOUND       = (ERRBASE_QUERY+ DBICODE_QXFLDSYMNOTFOUND);
  DBIERR_QXTBLSYMNOTFOUND       = (ERRBASE_QUERY+ DBICODE_QXTBLSYMNOTFOUND);
  DBIERR_BLOBTERM               = (ERRBASE_QUERY+ DBICODE_BLOBTERM);
  DBIERR_BLOBERR                = (ERRBASE_QUERY+ DBICODE_BLOBERR);
  DBIERR_RESTARTQRY             = (ERRBASE_QUERY+ DBICODE_RESTARTQRY);
  DBIERR_UNKNOWNANSTYPE         = (ERRBASE_QUERY+ DBICODE_UNKNOWNANSTYPE);
  DBIERR_SQLG_MDIST             = (ERRBASE_QUERY+ DBICODE_SQLG_MDIST);
  DBIERR_SQLG_NOARI             = (ERRBASE_QUERY+ DBICODE_SQLG_NOARI);
  DBIERR_SQLG_LIKEN             = (ERRBASE_QUERY+ DBICODE_SQLG_LIKEN);
  DBIERR_SQLG_ALPHO             = (ERRBASE_QUERY+ DBICODE_SQLG_ALPHO);
  DBIERR_SQLG_DATEO             = (ERRBASE_QUERY+ DBICODE_SQLG_DATEO);
  DBIERR_SQLG_RELOP             = (ERRBASE_QUERY+ DBICODE_SQLG_RELOP);
  DBIERR_SQLG_ONLYC             = (ERRBASE_QUERY+ DBICODE_SQLG_ONLYC);
  DBIERR_SQLG_CNTLN             = (ERRBASE_QUERY+ DBICODE_SQLG_CNTLN);
  DBIERR_SQLG_CHINI             = (ERRBASE_QUERY+ DBICODE_SQLG_CHINI);
  DBIERR_SQLG_UNION             = (ERRBASE_QUERY+ DBICODE_SQLG_UNION);
  DBIERR_SQLG_SLFIN             = (ERRBASE_QUERY+ DBICODE_SQLG_SLFIN);
  DBIERR_SQLG_OTJVR             = (ERRBASE_QUERY+ DBICODE_SQLG_OTJVR);
  DBIERR_SQLG_STROW             = (ERRBASE_QUERY+ DBICODE_SQLG_STROW);
  DBIERR_SQLG_QUANT             = (ERRBASE_QUERY+ DBICODE_SQLG_QUANT);
  DBIERR_SQLG_REGSO             = (ERRBASE_QUERY+ DBICODE_SQLG_REGSO);
  DBIERR_SQLG_COUNT             = (ERRBASE_QUERY+ DBICODE_SQLG_COUNT);
  DBIERR_SQLG_AVERA             = (ERRBASE_QUERY+ DBICODE_SQLG_AVERA);
  DBIERR_SQLG_DATEA             = (ERRBASE_QUERY+ DBICODE_SQLG_DATEA);
  DBIERR_SQLG_BADPT             = (ERRBASE_QUERY+ DBICODE_SQLG_BADPT);
  DBIERR_SQLG_RELPA             = (ERRBASE_QUERY+ DBICODE_SQLG_RELPA);
  DBIERR_SQLG_PATRN             = (ERRBASE_QUERY+ DBICODE_SQLG_PATRN);
  DBIERR_SQLG_FNDSU             = (ERRBASE_QUERY+ DBICODE_SQLG_FNDSU);
  DBIERR_SQLG_IFDCS             = (ERRBASE_QUERY+ DBICODE_SQLG_IFDCS);
  DBIERR_SQLG_IDCCO             = (ERRBASE_QUERY+ DBICODE_SQLG_IDCCO);
  DBIERR_SQLG_ONLYI             = (ERRBASE_QUERY+ DBICODE_SQLG_ONLYI);
  DBIERR_SQLG_SQLDIALECT        = (ERRBASE_QUERY+ DBICODE_SQLG_SQLDIALECT);
  DBIERR_SQLG_NOQUERY           = (ERRBASE_QUERY+ DBICODE_SQLG_NOQUERY);
  DBIERR_BLOBGROUP              = (ERRBASE_QUERY+ DBICODE_BLOBGROUP);
  DBIERR_QRYNOPROP              = (ERRBASE_QUERY+DBICODE_QRYNOPROP);
  DBIERR_ANSTYPNOTSUP           = (ERRBASE_QUERY+DBICODE_ANSTYPNOTSUP);
  DBIERR_ANSALIASNOTSUP         = (ERRBASE_QUERY+DBICODE_ANSALIASNOTSUP);
  DBIERR_INSBLOBREQ             = (ERRBASE_QUERY+DBICODE_INSBLOBREQ     ); { 0x9a }
  DBIERR_CHGUNIQUENDXREQ        = (ERRBASE_QUERY+DBICODE_CHGUNIQUENDXREQ); { 0x9b }
  DBIERR_DELUNIQUENDXREQ        = (ERRBASE_QUERY+DBICODE_DELUNIQUENDXREQ); { 0x9c }
  DBIERR_SQLNOFULLUPDATE        = (ERRBASE_QUERY+DBICODE_SQLNOFULLUPDATE); { 0x9d }
  DBIERR_CANTEXECREMOTE         = (ERRBASE_QUERY+DBICODE_CANTEXECREMOTE); { 0x9e }
  DBIERR_UNEXPECTEDEOC          = (ERRBASE_QUERY+DBICODE_UNEXPECTEDEOC);
  DBIERR_SQLPARAMNOTSET         = (ERRBASE_QUERY+DBICODE_SQLPARAMNOTSET);
  DBIERR_QUERYTOOLONG           = (ERRBASE_QUERY+DBICODE_QUERYTOOLONG);

  DBIERR_NOSUCHRELORALIAS       = (ERRBASE_QUERY+DBICODE_NOSUCHRELORALIAS);
  DBIERR_TYPEAMBIGUITY          = (ERRBASE_QUERY+DBICODE_TYPEAMBIGUITY);
  DBIERR_ORDERBYNOTAPROJ        = (ERRBASE_QUERY+DBICODE_ORDERBYNOTAPROJ);
  DBIERR_SQLPARSE               = (ERRBASE_QUERY+DBICODE_SQLPARSE);
  DBIERR_CONSTRAINTFAILED       = (ERRBASE_QUERY+DBICODE_CONSTRAINTFAILED);
  DBIERR_NOTGROUPINGFIELD       = (ERRBASE_QUERY+DBICODE_NOTGROUPINGFIELD);
  DBIERR_UDFNOTDEFINED          = (ERRBASE_QUERY+DBICODE_UDFNOTDEFINED);
  DBIERR_UDFERROR               = (ERRBASE_QUERY+DBICODE_UDFERROR);
  DBIERR_SINGLEROWERROR         = (ERRBASE_QUERY+DBICODE_SINGLEROWERROR);
  DBIERR_GROUPEXPR              = (ERRBASE_QUERY+DBICODE_GROUPEXPR);
  DBIERR_QUERYTEXT              = (ERRBASE_QUERY+DBICODE_QUERYTEXT);
  DBIERR_ANSIJOINSUP            = (ERRBASE_QUERY+DBICODE_ANSIJOINSUP);
  DBIERR_DISTUNION              = (ERRBASE_QUERY+DBICODE_DISTUNION);
  DBIERR_GROUPBYREQ             = (ERRBASE_QUERY+DBICODE_GROUPBYREQ);
  DBIERR_INSUPDAUTOINC          = (ERRBASE_QUERY+DBICODE_INSUPDAUTOIC);
  DBIERR_UPDREFINTSINGLE        = (ERRBASE_QUERY+DBICODE_UPDREFINTSINGLE);
  DBIERR_NOMSACCESS             = (ERRBASE_QUERY+DBICODE_NOMSACCESS);
  DBIERR_FIELDCONSTPREP         = (ERRBASE_QUERY+DBICODE_FIELDCONSTPREP);
  DBIERR_FIELDDEFPREP           = (ERRBASE_QUERY+DBICODE_FIELDDEFPREP);
  DBIERR_RECCONSTPREP           = (ERRBASE_QUERY+DBICODE_RECCONSTPREP);

  DBIERR_TOOFEWSUBEXPR          = (ERRBASE_QUERY+DBICODE_TOOFEWSUBEXPR);
  DBIERR_TOOMANYSUBEXPR         = (ERRBASE_QUERY+DBICODE_TOOMANYSUBEXPR);
  DBIERR_INVALIDNODETYPE        = (ERRBASE_QUERY+DBICODE_INVALIDNODETYPE);
  DBIERR_BOOLEXPR               = (ERRBASE_QUERY+DBICODE_BOOLEXPR);
  DBIERR_NONBOOLEXPR            = (ERRBASE_QUERY+DBICODE_NONBOOLEXPR);
  DBIERR_NOOUTERJOIN            = (ERRBASE_QUERY+DBICODE_NOOUTERJOIN);
  DBIERR_USERCONSTRERR          = (ERRBASE_QUERY+DBICODE_USERCONSTRERR);

{ END_OF_QUERY_MESSAGES }

{ ERRCAT_VERSION }
{ ============== }

  ERRCODE_INTERFACEVER          = 1;      { Interface mismatch }
  ERRCODE_INDEXOUTOFDATE        = 2;      { Index is out of date }
  ERRCODE_OLDVERSION            = 3;      { Older version (see context) }
  ERRCODE_VALFILEINVALID        = 4;      { Val. file is out of date }
  ERRCODE_BLOBVERSION           = 5;      { Old Blob file version }
  ERRCODE_ENGQRYMISMATCH        = 6;      { Query and IDAPI are mismatched }
  ERRCODE_SERVERVERSION         = 7;      { Server is incompatible version }
  ERRCODE_TABLELEVEL            = 8;      { Higher table level required }

  DBIERR_INTERFACEVER           = (ERRBASE_VERSION + ERRCODE_INTERFACEVER);
  DBIERR_INDEXOUTOFDATE         = (ERRBASE_VERSION + ERRCODE_INDEXOUTOFDATE);
  DBIERR_OLDVERSION             = (ERRBASE_VERSION + ERRCODE_OLDVERSION);
  DBIERR_VALFILEINVALID         = (ERRBASE_VERSION + ERRCODE_VALFILEINVALID);
  DBIERR_BLOBVERSION            = (ERRBASE_VERSION + ERRCODE_BLOBVERSION);
  DBIERR_ENGQRYMISMATCH         = (ERRBASE_VERSION + ERRCODE_ENGQRYMISMATCH);
  DBIERR_SERVERVERSION          = (ERRBASE_VERSION + ERRCODE_SERVERVERSION);
  DBIERR_TABLELEVEL             = (ERRBASE_VERSION + ERRCODE_TABLELEVEL);

{ ERRCAT_CAPABILITY }
{ ================= }

  ERRCODE_NOTSUPPORTED          = 1;      { Capability not supported }
  ERRCODE_NIY                   = 2;      { Not Implemented Yet }
  ERRCODE_TABLESQL              = 3;      { Cannot access SQL replica }
  ERRCODE_SEARCHCOLREQD         = 4;      { Searchable (Non-blob column) required }
  ERRCODE_NOMULTCONNECT         = 5;      { Multiple connections not supported }
  ERRCODE_NODBASEEXPR           = 6;      { Full dBASE Expressions not supported }
  ERRCODE_NONESTEDTRAN          = 7;      { Nested transactions not supported }

  DBIERR_NOTSUPPORTED           = (ERRBASE_CAPABILITY + ERRCODE_NOTSUPPORTED);
  DBIERR_NIY                    = (ERRBASE_CAPABILITY + ERRCODE_NIY);
  DBIERR_TABLESQL               = (ERRBASE_CAPABILITY + ERRCODE_TABLESQL);
  DBIERR_SEARCHCOLREQD          = (ERRBASE_CAPABILITY + ERRCODE_SEARCHCOLREQD);
  DBIERR_NOMULTCONNECT          = (ERRBASE_CAPABILITY + ERRCODE_NOMULTCONNECT);
  DBIERR_NODBASEEXPR            = (ERRBASE_CAPABILITY + ERRCODE_NODBASEEXPR);
  DBIERR_NONESTEDTRAN           = (ERRBASE_CAPABILITY + ERRCODE_NONESTEDTRAN);

{ ERRCAT_CONFIG }
{ ============= }

  ERRCODE_INVALIDDBSPEC         = 1;
  ERRCODE_UNKNOWNDBTYPE         = 2;
  ERRCODE_INVALIDSYSDATA        = 3;
  ERRCODE_UNKNOWNNETTYPE        = 4;
  ERRCODE_NOTONTHATNET          = 5;
  ERRCODE_INVALIDCFGPARAM       = 6;      { Generic invalid config param }


  DBIERR_INVALIDDBSPEC          = (ERRBASE_CONFIG + ERRCODE_INVALIDDBSPEC);
  DBIERR_UNKNOWNDBTYPE          = (ERRBASE_CONFIG + ERRCODE_UNKNOWNDBTYPE);
  DBIERR_INVALIDSYSDATA         = (ERRBASE_CONFIG + ERRCODE_INVALIDSYSDATA);
  DBIERR_UNKNOWNNETTYPE         = (ERRBASE_CONFIG + ERRCODE_UNKNOWNNETTYPE);
  DBIERR_NOTONTHATNET           = (ERRBASE_CONFIG + ERRCODE_NOTONTHATNET);
  DBIERR_INVALIDCFGPARAM        = (ERRBASE_CONFIG + ERRCODE_INVALIDCFGPARAM);

{ ERRCAT_WARNING  non-fatal warnings:               }
{ warn user of action, or ask for optional behavior }
{ ================================================= }
  ERRCODE_OBJIMPLICITLYDROPPED  = 1;
  ERRCODE_OBJMAYBETRUNCATED     = 2;
  ERRCODE_OBJIMPLICITLYMODIFIED = 3;
  ERRCODE_VALIDATEDATA          = 4;
  ERRCODE_VALFIELDMODIFIED      = 5;
  ERRCODE_TABLELEVELCHANGED     = 6;
  ERRCODE_COPYLINKEDTABLES      = 7;
  ERRCODE_OTHERSERVERLOADED     = 8;
  ERRCODE_OBJIMPLICITLYTRUNCATED = 9;
  ERRCODE_VCHKMAYNOTBEENFORCED  = 10;
  ERRCODE_MULTIPLEUNIQRECS      = 11;
  ERRCODE_FIELDMUSTBETRIMMED    = 12;

  DBIERR_OBJIMPLICITLYDROPPED   = ( ERRBASE_WARNING + ERRCODE_OBJIMPLICITLYDROPPED);
  DBIERR_OBJMAYBETRUNCATED      = ( ERRBASE_WARNING + ERRCODE_OBJMAYBETRUNCATED);
  DBIERR_OBJIMPLICITLYMODIFIED  = ( ERRBASE_WARNING + ERRCODE_OBJIMPLICITLYMODIFIED);
  DBIERR_VALIDATEDATA           = ( ERRBASE_WARNING + ERRCODE_VALIDATEDATA);
  DBIERR_VALFIELDMODIFIED       = ( ERRBASE_WARNING + ERRCODE_VALFIELDMODIFIED);
  DBIERR_TABLELEVELCHANGED      = ( ERRBASE_WARNING + ERRCODE_TABLELEVELCHANGED);
  DBIERR_COPYLINKEDTABLES       = ( ERRBASE_WARNING + ERRCODE_COPYLINKEDTABLES);
  DBIERR_OTHERSERVERLOADED      = ( ERRBASE_WARNING + ERRCODE_OTHERSERVERLOADED);
  DBIERR_OBJIMPLICITLYTRUNCATED = ( ERRBASE_WARNING + ERRCODE_OBJIMPLICITLYTRUNCATED);
  DBIERR_VCHKMAYNOTBEENFORCED   = ( ERRBASE_WARNING + ERRCODE_VCHKMAYNOTBEENFORCED );
  DBIERR_MULTIPLEUNIQRECS       = ( ERRBASE_WARNING + ERRCODE_MULTIPLEUNIQRECS );
  DBIERR_FIELDMUSTBETRIMMED     = ( ERRBASE_WARNING + ERRCODE_FIELDMUSTBETRIMMED );


{ ERRCAT_OTHER }
{ ============ }

  ERRCODE_FILEEXISTS            = 1;      { File already exsits }
  ERRCODE_BLOBMODIFIED          = 2;      { Another user modified Blob }
  ERRCODE_UNKNOWNSQL            = 3;      { Unknown SQL error }
  ERRCODE_TABLEEXISTS           = 4;      { Table already exsits }
  ERRCODE_PDX10TABLE            = 5;      { Paradox 1.0 tables not supported }
  ERRCODE_UPDATEABORT           = 6;      { Update operation aborted }


  DBIERR_FILEEXISTS             = (ERRBASE_OTHER + ERRCODE_FILEEXISTS);
  DBIERR_BLOBMODIFIED           = (ERRBASE_OTHER + ERRCODE_BLOBMODIFIED);
  DBIERR_UNKNOWNSQL             = (ERRBASE_OTHER + ERRCODE_UNKNOWNSQL);
  DBIERR_TABLEEXISTS            = (ERRBASE_OTHER + ERRCODE_TABLEEXISTS);
  DBIERR_PDX10TABLE             = (ERRBASE_OTHER + ERRCODE_PDX10TABLE);
  DBIERR_UPDATEABORT            = (ERRBASE_OTHER + ERRCODE_UPDATEABORT);


{ ERRCAT_COMPATIBILITY }
{ ==================== }

  ERRCODE_DIFFSORTORDER         = 1;      { Sortorders not compatible }
  ERRCODE_DIRINUSEBYOLDVER      = 2;      { Directory in use by old version }
  ERRCODE_PDX35LDDRIVER         = 3;      { Needs Pdox 3.5 compatible language driver }

  DBIERR_DIFFSORTORDER          = (ERRBASE_COMPATIBILITY + ERRCODE_DIFFSORTORDER);
  DBIERR_DIRINUSEBYOLDVER       = (ERRBASE_COMPATIBILITY + ERRCODE_DIRINUSEBYOLDVER);
  DBIERR_PDX35LDDRIVER          = (ERRBASE_COMPATIBILITY + ERRCODE_PDX35LDDRIVER);

{ ERRCAT_OPTPARAM }
{ =============== }

  ERRCODE_REQOPTPARAM           = 1;      { Required optional parameter missing }
  ERRCODE_INVALIDOPTPARAM       = 2;      { Optional param out-of-range or bad }


  DBIERR_REQOPTPARAM            = (ERRBASE_OPTPARAM + ERRCODE_REQOPTPARAM);
  DBIERR_INVALIDOPTPARAM        = (ERRBASE_OPTPARAM + ERRCODE_INVALIDOPTPARAM);

{  ERRCAT_REPOSITORY }
{  ================= }

  ERRCODE_REPOSITORYCORRUPT     = 1;    { Data Repository is corrupt }
  ERRCODE_INFOBLOBCORRUPT       = 2;    { Info Blob corrupted }
  ERRCODE_SCHEMACORRUPT         = 3;    { DR Schema is corrupt }
  ERRCODE_ATTRTYPEEXISTS        = 4;    { Attribute Type exists }
  ERRCODE_INVALIDOBJTYPE        = 5;    { Invalid Object Type }
  ERRCODE_INVALIDRELATIONTYPE   = 6;    { Invalid Relation Type }
  ERRCODE_VIEWEXISTS            = 7;    { View already exists }
  ERRCODE_NOSUCHVIEW            = 8;    { No such View exists }
  ERRCODE_INVALIDRECCONSTRAINT  = 9;    { Invalid Record Constraint }
  ERRCODE_LDBCONNECTION         = 10;   { Object is in a Logical DB }
  ERRCODE_REPOSITORYEXISTS      = 11;   { Repository already exists }
  ERRCODE_NOSUCHREPOSITORY      = 12;   { Repository does not exist }
  ERRCODE_REPOSITORYDBMISSING   = 13;   { Repository database does not exist }
  ERRCODE_REPOSITORYOUTOFDATE   = 14;   { Repository info is out of date }
  ERRCODE_REPOSITORYVERSION     = 15;   { DR Version mismatch }
  ERRCODE_REPOSITORYNAME        = 16;   { Invalid Repository name }
  ERRCODE_DEPENDENTOBJECTS      = 17;   { Dependent Objects exist }
  ERRCODE_RELATIONLIMIT         = 18;   { Too many Relationships for this Object Type }
  ERRCODE_RELATIONSHIPSEXIST    = 19;   { Relationships to the Object exist }
  ERRCODE_EXCHANGEFILECORRUPT   = 20;   { Exchange File Corrupt }
  ERRCODE_EXCHANGEFILEVERSION   = 21;   { Exchange File Version Mismatch }
  ERRCODE_TYPEMISMATCH          = 22;   { Exchange File and Repository Types don't match }
  ERRCODE_OBJECTEXISTS          = 23;   { Object Exists in the Target Repository }
  ERRCODE_REPOSITORYACCESS      = 24;   { Access to Repository Denied }
  ERRCODE_REPOSITORYCREATE      = 25;   { Cannot Create Repository }
  ERRCODE_DATABASEOPENFAILED    = 26;   { Cannot Open a Database }


  DBIERR_REPOSITORYCORRUPT      = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYCORRUPT);
  DBIERR_INFOBLOBCORRUPT        = (ERRBASE_REPOSITORY + ERRCODE_INFOBLOBCORRUPT);
  DBIERR_SCHEMACORRUPT          = (ERRBASE_REPOSITORY + ERRCODE_SCHEMACORRUPT);
  DBIERR_ATTRTYPEEXISTS         = (ERRBASE_REPOSITORY + ERRCODE_ATTRTYPEEXISTS);
  DBIERR_INVALIDOBJTYPE         = (ERRBASE_REPOSITORY + ERRCODE_INVALIDOBJTYPE);
  DBIERR_INVALIDRELATIONTYPE    = (ERRBASE_REPOSITORY + ERRCODE_INVALIDRELATIONTYPE);
  DBIERR_VIEWEXISTS             = (ERRBASE_REPOSITORY + ERRCODE_VIEWEXISTS);
  DBIERR_NOSUCHVIEW             = (ERRBASE_REPOSITORY + ERRCODE_NOSUCHVIEW);
  DBIERR_INVALIDRECCONSTRAINT   = (ERRBASE_REPOSITORY + ERRCODE_INVALIDRECCONSTRAINT);
  DBIERR_LDBCONNECTION          = (ERRBASE_REPOSITORY + ERRCODE_LDBCONNECTION);
  DBIERR_REPOSITORYEXISTS       = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYEXISTS);
  DBIERR_NOSUCHREPOSITORY       = (ERRBASE_REPOSITORY + ERRCODE_NOSUCHREPOSITORY);
  DBIERR_REPOSITORYDBMISSING    = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYDBMISSING);
  DBIERR_REPOSITORYOUTOFDATE    = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYOUTOFDATE);
  DBIERR_REPOSITORYVERSION      = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYVERSION);
  DBIERR_REPOSITORYNAME         = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYNAME);
  DBIERR_DEPENDENTOBJECTS       = (ERRBASE_REPOSITORY + ERRCODE_DEPENDENTOBJECTS);
  DBIERR_RELATIONLIMIT          = (ERRBASE_REPOSITORY + ERRCODE_RELATIONLIMIT);
  DBIERR_RELATIONSHIPSEXIST     = (ERRBASE_REPOSITORY + ERRCODE_RELATIONSHIPSEXIST);
  DBIERR_EXCHANGEFILECORRUPT    = (ERRBASE_REPOSITORY + ERRCODE_EXCHANGEFILECORRUPT);
  DBIERR_EXCHANGEFILEVERSION    = (ERRBASE_REPOSITORY + ERRCODE_EXCHANGEFILEVERSION);
  DBIERR_TYPEMISMATCH           = (ERRBASE_REPOSITORY + ERRCODE_TYPEMISMATCH);
  DBIERR_OBJECTEXISTS           = (ERRBASE_REPOSITORY + ERRCODE_OBJECTEXISTS);
  DBIERR_REPOSITORYACCESS       = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYACCESS);
  DBIERR_REPOSITORYCREATE       = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYCREATE);
  DBIERR_DATABASEOPENFAILED     = (ERRBASE_REPOSITORY + ERRCODE_DATABASEOPENFAILED);

{============================================================================}
{                          DBI prototypes                                    }
{============================================================================}
{                          Environmental                                     }
{----------------------------------------------------------------------------}

function DbiInitFn (                    { Initialize the Engine }
      iVer          : Word;             { Interface Version }
      pEnv          : pDBIEnv           { Environment Structure/NULL }
   ): DBIResult stdcall;

 { DO NOT CALL THIS DIRECTLY. Use the 'DbiInit' define below. }

 { Initializes the engine environment. Default settings can be overwritten  }
 { by supplying the appropriate settings. Defaults are read from the system }
 { configuration file. pEnv can be NULLP to get the system defautls.        }

function DbiInit (pEnv: PDbiEnv): DBIResult stdcall;

function DbiExit: DBIResult stdcall;            { Exit engine }

function DbiDLLExit: DBIResult stdcall;         { Exit DLL }

{============================================================================}
{                         System Level Info                                  }
{============================================================================}

function DbiGetSysVersion (             { Get system version info }
var   sysVersion    : SYSVersion
   ): DBIResult stdcall;

function DbiGetSysConfig (              { System configuration }
var   sysConfig     : SYSConfig
   ): DBIResult stdcall;

function DbiGetClientInfo (             { Get Client info }
var   clientInfo    : CLIENTInfo
   ): DBIResult stdcall;

function DbiGetSysInfo (                { Get system status/info }
var   sysInfo       : SYSInfo
   ): DBIResult stdcall;

function DbiLoadDriver (                { Load a given driver }
      pszDriverType : PAnsiChar             { Driver name }
   ): DBIResult stdcall;

{============================================================================}
{                            Sessions                                        }
{============================================================================}

function DbiStartSession (              { Start a new session }
      pszName       : PAnsiChar;            { Name (Optional) }
var   hSes          : hDBISes;          { Session }
      pNetDir       : PAnsiChar             { Netfile directory for session (opt) }
   ): DBIResult stdcall;

function DbiGetCurrSession (            { Get the current session }
var   hSes          : hDBISes           { Session }
   ): DBIResult stdcall;

function DbiSetCurrSession (            { Set the current session }
      hSes          : hDBISes           { Session/NULL }
   ): DBIResult stdcall;

function DbiCloseSession (              { Close the current session }
      hSes          : hDBISes           { Session }
   ): DBIResult stdcall;

function DbiGetSesInfo (                { Get current session info }
var   sesInfo       : SESInfo
   ): DBIResult stdcall;

function DbiSetPrivateDir (             { Set Private Directory for session }
      pszDir        : PAnsiChar             { Directory name/NULL }
   ): DBIResult stdcall;


{============================================================================}
{                     Datababase, Schema and File inquiry                    }
{============================================================================}

function DbiOpenDatabase (              { Open a database }
      pszDbName     : PAnsiChar;            { Database name }
      pszDbType     : PAnsiChar;            { Database type (NULL: Universal) }
      eOpenMode     : DBIOpenMode;      { Open type }
      eShareMode    : DBIShareMode;     { Share type }
      pszPassword   : PAnsiChar;            { Password }
      iOptFlds      : Word;             { Number of optional Params }
      pOptFldDesc   : pFLDDesc;         { Optional Field Descriptors }
      pOptParams    : Pointer;          { Optional Params }
var   hDb           : hDBIDb            { Returnd database handle }
   ): DBIResult stdcall;

function DbiSetDirectory (              { Set the current directory }
      hDb           : hDBIDb;           { Universal database handle }
      pszDir        : PAnsiChar             { Directory/NULL }
   ): DBIResult stdcall;

function DbiGetDirectory (              { Get the current/default directory }
      hDb           : hDBIDb;           { Universal database handle }
      bDefault      : Bool;             { True for default }
      pszDir        : PAnsiChar             { Returned directory }
   ): DBIResult stdcall;

function DbiOpenTableList (             { Open a cursor on "Tables" }
      hDb           : hDBIDb;           { Database handle }
      bExtended     : Bool;             { True for extended info }
      bSystem       : Bool;             { True to include system tables }
      pszWild       : PAnsiChar;            { Wild card name }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiOpenFileList (              { Open a cursor on "Files" }
      hDb           : hDBIDb;           { Universal database handle }
      pszWild       : PAnsiChar;            { Wild card name }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiOpenIndexList (             { Return "Indexes" for a table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type }
var   hCur          : hDBICur           { Returned cursor on "Indexes" }
   ): DBIResult stdcall;

function DbiOpenFieldList (             { Return "Fields" for a table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type }
      bPhyTypes     : Bool;             { True, for physical types }
var   hCur          : hDBICur           { Returned cursor on "Fields" }
   ): DBIResult stdcall;

function DbiOpenVchkList (              { Return "Checks" for a table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver Type }
var   hChkCur       : hDBICur           { Returned cursor on "Checks" }
   ): DBIResult stdcall;

function DbiOpenRintList (              { Return Integrity checks }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type }
var   hChkCur       : hDBICur           { Returned cursor on "Ref Int". }
   ): DBIResult stdcall;

function DbiOpenSecurityList (          { Return security descriptions }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type }
var   hSecCur       : hDBICur           { Returned cursor on sec list }
   ): DBIResult stdcall;

function DbiOpenFamilyList (            { Return family members }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type }
var   hFmlCur       : hDBICur           { Returned cursor on "Family" }
   ): DBIResult stdcall;

function DbiOpenSPList (                { Open a cursor on "Stored Procedures" }
      hDb           : hDBIDb;           { Universal database handle }
      bExtended     : Bool;             { True for extended info (N/A) }
      bSystem       : Bool;             { True to include system procedures }
      pszQual       : PAnsiChar;            { Qualifier (N/A) }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiOpenSPParamList (           { Return "Parameters" for a stored procedure }
      hDb           : hDBIDb;           { Database handle }
      pszSPName     : PAnsiChar;            { Stored procedure name }
      bPhyTypes     : Bool;             { True, for physical types }
      uOverload     : Word;             { Overload number }
var   hCur          : hDBICur           { Returned cursor on "Parameters" }
   ): DBIResult stdcall;

function DbiOpenFunctionList (          { Open a cursor on "Functions" }
      hDb           : hDBIDb;           { Universal database handle }
      eoptBits      : DBIFUNCOpts;      { Options for function list }
      phCur         : phDBICur          { Returned cursor on "Functions" }
                                        { Record desc is of type DBIFUNCDesc }
   ): DBIResult stdcall;

function DbiOpenFunctionArgList (       { Return "Arguments" for a function }
      hDb           : hDBIDb;           { Database handle }
      pszFuncName   : PAnsiChar;            { Function name }
      uOverload     : Word;             { Overload number }
      phCur         : phDBICur          { Returned cursor on "Arguments" }
   ): DBIResult stdcall;

function DbiCloseDatabase (             { Close a database }
var   hDb           : hDBIDb            { Pointer to database handle }
   ): DBIResult stdcall;

{============================================================================}
{                                  Capabilities                              }
{============================================================================}

function DbiOpenDriverList (            { Get a list of driver names }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiGetDriverDesc (             { Get description for a given type }
      pszDriverType : PAnsiChar;            { Symbolic name for driver type }
var   drvType       : DRVType           { Driver type description }
   ): DBIResult stdcall;

function DbiOpenDatabaseList (          { Get a list of registered databases }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiGetDatabaseDesc (           { Get a description of a logical db }
      pszName       : PAnsiChar;            { Name of logical database }
      pdbDesc       : pDBDesc           { Database description }
   ): DBIResult stdcall;

function DbiOpenTableTypesList (        { Get a list of table types }
      pszDriverType : PAnsiChar;            { Driver type }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiGetTableTypeDesc (          { Get Table capabilities }
      pszDriverType : PAnsiChar;            { Driver type }
      pszTableType  : PAnsiChar;            { Table type }
var   tblType       : TBLType           { Table Capabilities }
   ): DBIResult stdcall;


function DbiOpenFieldTypesList (        { Get a list of field types }
      pszDriverType : PAnsiChar;            { Driver type }
      pszTblType    : PAnsiChar;            { Table type (Optional) }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiGetFieldTypeDesc (          { Get list of field types }
      pszDriverType : PAnsiChar;            { Driver type }
      pszTableType  : PAnsiChar;            { Table type }
      pszFieldType  : PAnsiChar;            { Field type  (Physical only) }
var   fldType       : FLDType           { Field type description }
   ): DBIResult stdcall;

function DbiOpenIndexTypesList (        { Get list of index types }
      pszDriverType : PAnsiChar;            { Driver type }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiGetIndexTypeDesc (          { Get description of given idx type }
      pszDriverType : PAnsiChar;            { Driver type }
      pszIndexType  : PAnsiChar;            { Index type }
var   idxType       : IDXType           { Index description }
   ): DBIResult stdcall;

function DbiOpenLdList (                { Get a list of Lang Drivers }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

{===========================================================================}
{                      Table Open, Properties & Structure                   }
{===========================================================================}

function DbiOpenTable (                 { Open a table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name or file name }
      pszDriverType : PAnsiChar;            { Driver type                 /NULL }
      pszIndexName  : PAnsiChar;            { Index to be used for access /NULL }
      pszIndexTagName : PAnsiChar;          { Index tag name              /NULL }
      iIndexId      : Word;             { Index number                /0 }
      eOpenMode     : DBIOpenMode;      { Read or RW }
      eShareMode    : DBIShareMode;     { Excl or Share }
      exltMode      : XLTMode;          { Xlate mode }
      bUniDirectional : Bool;           { Uni or Bi directional }
      pOptParams    : Pointer;          { Optional params /NULL }
var   hCursor       : hDBICur           { Returns Cursor handle }
   ): DBIResult stdcall;

function DbiOpenNestedTable(            { Get nested table }
      hCursor       : hDBICur;          { Master cursor }
      iFieldNo      : Word;             { FieldID of nested table field }
      bReadOnly     : Bool;             { if TRUE, read only mode }
      bUniDirectional: Bool;            { if TRUE, unidirectional }
var   hCursorNested : hDBICur           { Returns Cursor handle for nested table }
   ): DBIResult; stdcall;

function DbiOpenRef(
      hCursor       : hDBICur;          { Master cursor }
      iFieldNo      : Word;             { FieldID }
      bReadOnly     : Bool;             { if TRUE, read only mode }
      bUniDirectional: Bool;            { If TRUE, Uni directional }
var   hRefCursor    : hDBICur           { Returns Cursor handle for REF }
   ): DBIResult; stdcall;

function DbiDatabaseFlush(
      hDb              : hDBIDb         { database handle }
): DBIResult;

function DbiGetCursorProps (            { Get Cursor properties }
      hCursor       : hDBICur;          { Cursor handle }
var   curProps      : CURProps          { Cursor properties }
   ): DBIResult stdcall;

function DbiGetObjFromName (            { Get object from name }
      eObjType      : DBIOBJType;       { Object handle }
      pszObjName    : PAnsiChar;            { Name of object /NULL }
var   hObj          : hDBIObj           { Returned object handle }
   ): DBIResult stdcall;

function DbiGetObjFromObj (             { Get associated object }
      hObj          : hDBIObj;          { Object handle }
      eObjType      : DBIOBJType;       { Type of associated object }
var   hObjOut       : hDBIObj           { Returns object of eObjType }
   ): DBIResult stdcall;

function DbiGetProp (                   { Get property }
      hObj          : hDBIObj;          { Object handle }
      iProp         : Longint;          { Property to retrieve }
      PropValue     : Pointer;          { == NULL, validate iProp for getting }
      iMaxLen       : Word;             { Length of buffer pPropValue }
var   iLen          : Word              { Returns required length }
   ): DBIResult stdcall;

function DbiSetProp (                   { Set property }
      hObj          : hDBIObj;          { Object handle }
      iProp         : Longint;          { Property to set }
      iPropValue    : Longint           { Property value }
   ): DBIResult stdcall;

function DbiValidateProp (              { Validate a property }
      hObj          : hDBIObj;          { Object handle }
      iProp         : Longint;          { property to validate }
      bSetting      : Bool              { TRUE:setting, FALSE:getting }
   ): DBIResult stdcall;

function DbiGetFieldDescs (             { Get field descriptions }
      hCursor       : hDBICur;          { Cursor handle }
      pfldDesc      : pFLDDesc          { Array of field descriptors }
   ): DBIResult stdcall;

function DbiGetCursorForTable (         { Find cursor for a given table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type / NULL }
var   hCursor       : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiCloneCursor (               { Return a duplicate cursor }
      hCurSrc       : hDBICur;          { Source cursor }
      bReadOnly     : Bool;             { If TRUE, read only mode }
      bUniDirectional : Bool;           { If TRUE, Uni directional }
var   hCurNew       : hDBICur           { Destination cursor address }
   ): DBIResult stdcall;

function DbiCloseCursor (               { Closes cursor }
var   hCursor       : hDBICur           { Pntr to Cursor handle }
   ): DBIResult stdcall;


{============================================================================}
{                      Index Manipulation & Usage                            }
{============================================================================}

function DbiOpenIndex (                 { Open an index }
      hCursor       : hDBICur;          { Cursor handle }
      pszIndexName  : PAnsiChar;            { Index Name }
      iIndexId      : Word              { Index number (if applicable) }
   ): DBIResult stdcall;

function DbiCloseIndex (                { Close an index }
      hCursor       : hDBICur;          { Cursor handle }
      pszIndexName  : PAnsiChar;            { Index Name }
      iIndexId      : Word              { Index number }
   ): DBIResult stdcall;

function DbiSwitchToIndex (             { Change index order of access }
var   hCursor       : hDBICur;          { Cursor handle (In/Out) }
      pszIndexName  : PAnsiChar;            { Index name }
      pszTagName    : PAnsiChar;            { Tag name (if applicable) }
      iIndexId      : Word;             { Index number }
      bCurrRec      : Bool              { Position at current rec }
   ): DBIResult stdcall;

function DbiGetIndexDesc (              { Get index properties }
      hCursor       : hDBICur;          { Cursor handle }
      iIndexSeqNo   : Word;             { Index number }
var   idxDesc       : IDXDesc           { Returned index description }
   ): DBIResult stdcall;

function DbiGetIndexDescs (             { Get index properties }
      hCursor       : hDBICur;          { Cursor handle }
      idxDesc       : PIDXDesc          { Returned index descriptors }
   ): DBIResult stdcall;

function DbiGetIndexForField (          { Get index desc for given field }
      hCursor       : hDBICur;          { Cursor handle }
      iFld          : Word;             { Field Number (1..N) }
      bProdTagOnly  : Bool;             { If TRUE, only xBASE prod tags will be returned }
var   idxDesc       : IDXDesc           { (Optional) }
   ): DBIResult stdcall;

function DbiGetIndexSeqNo (             { Get index sequence number }
      hCursor       : hDBICur;          { Cursor handle }
      pszIndexName  : PAnsiChar;            { Index name }
      pszTagName    : PAnsiChar;            { Tag name (if applicable) }
      iIndexId      : Word;             { Index number }
var   iIndexSeqNo   : Word              { Index number }
   ): DBIResult stdcall;

function DbiSetToKey (                  { Set key condition }
      hCursor       : hDBICur;          { Cursor handle }
      eSearchCond   : DBISearchCond;    { Search condition (default is =) }
      bDirectKey    : Bool;             { Key is supplied directly }
      iFields       : Word;             { No of full fields to match }
      iLen          : Word;             { Partial key len of last field }
      pBuff         : Pointer           { Either Record buffer or Key itself }
   ): DBIResult stdcall;

function DbiExtractKey (                { Get the key value of current record }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record buffer (optional) }
      pKeyBuf       : Pointer           { Returned. Key bytes. }
   ): DBIResult stdcall;

function DbiSetRange (                  { Set cursor to a range }
      hCursor       : hDBICur;          { Cursor }
      bKeyItself    : Bool;             { Whether Key or Record buffer }
      iFields1      : Word;             { Key fields to be mathced in full }
      iLen1         : Word;             { Key length to compare }
      pKey1         : Pointer;          { Top/Left key in Range }
      bKey1Incl     : Bool;             { If Inclusive of Key1 }
      iFields2      : Word;             { Key fields to be mathced in full }
      iLen2         : Word;             { Key length to compare }
      pKey2         : Pointer;          { Bottom/Right key in Range }
      bKey2Incl     : Bool              { If Inclusive of Key2 }
   ): DBIResult stdcall;

function DbiResetRange (                { Reset range }
      hCursor       : hDBICur           { cursor handle }
   ): DBIResult stdcall;

function DbiCompareKeys (               { Compare two keys }
      hCursor       : hDBICur;          { Cursor handle }
      pKey1         : Pointer;          { Key buffer 1 to compare }
      pKey2         : Pointer;          { Key buffer 2 (Or NULL) }
      iFields       : Word;             { Fields to compare in full }
      iLen          : Word;             { Partial key to compare }
var   iResult       : SmallInt          { Compare result }
   ): DBIResult stdcall;

function DbiGetRecordForKey (           { Find a record matching key }
      hCursor       : hDBICur;          { Cursor handle }
      bDirectKey    : Bool;             { Key is supplied directly }
      iFields       : Word;             { No of full fields to match }
      iLen          : Word;             { Partial key len of last field }
      pKey          : Pointer;          { Either Record buffer or Key itself }
      pRecBuff      : Pointer           { (Optional) Record buffer }
   ): DBIResult stdcall;

{=============================================================================}
{                          Validity check and referential integrity           }
{=============================================================================}

function DbiGetVchkDesc (               { Get valcheck descriptor }
      hCursor       : hDBICur;          { Cursor handle }
      iValSeqNo     : Word;             { Valcheck sequence number }
      pvalDesc      : pVCHKDesc         { Returned valcheck description }
   ): DBIResult stdcall;

function DbiGetRintDesc (               { Get referential integrity descriptor }
      hCursor       : hDBICur;          { Cursor handle }
      iRintSeqNo    : Word;             { Rint sequence number }
      printDesc     : pRINTDesc         { Returned rint description }
   ): DBIResult stdcall;

{=============================================================================}
{                              Cursor Maintenance                             }
{=============================================================================}


function DbiSetToBegin (                { Reset cursor to beginning }
      hCursor       : hDBICur           { Cursor handle }
   ): DBIResult stdcall;

function DbiSetToEnd (                  { Reset cursor to ending }
      hCursor       : hDBICur           { Cursor handle }
   ): DBIResult stdcall;

function DbiSetToCursor (               { Set cursor to another cursor position }
      hDest         : hDBICur;          { Destination cursor }
      hSrc          : hDBICur           { Source cursor }
   ): DBIResult stdcall;

function DbiGetBookMark (               { Get a book-mark }
      hCur          : hDBICur;          { Cursor }
      pBookMark     : Pointer           { Pointer to Book-Mark }
   ): DBIResult stdcall;

function DbiSetToBookMark (             { Position to a Book-Mark }
      hCur          : hDBICur;          { Cursor }
      pBookMark     : Pointer           { Pointer to Book-Mark }
   ): DBIResult stdcall;

function DbiCompareBookMarks (          { Compare two Book-marks }
      hCur          : hDBICur;          { Cursor }
      pBookMark1    : Pointer;          { Book mark 1 }
      pBookMark2    : Pointer;          { Book mark 2 }
var   CmpBkmkResult : CmpBkmkRslt       { Compare result }
   ): DBIResult stdcall;

{============================================================================}
{                      Data Access: Logical Record Level                     }
{============================================================================}


function DbiGetNextRecord (             { Find/Get the next record }
      hCursor       : hDBICur;          { Cursor handle }
      eLock         : DBILockType;      { Optional lock request }
      pRecBuff      : Pointer;          { Record buffer(client) }
      precProps     : pRECProps         { Optional record properties }
   ): DBIResult stdcall;

function DbiGetPriorRecord (            { Find/Get the prior record }
      hCursor       : hDBICur;          { Cursor handle }
      eLock         : DBILockType;      { Optional lock request }
      pRecBuff      : Pointer;          { Record buffer (client) }
      precProps     : pRECProps         { Optional record properties }
   ): DBIResult stdcall;

function DbiGetRecord (                 { Gets the current record }
      hCursor       : hDBICur;          { Cursor handle }
      eLock         : DBILockType;      { Optional lock request }
      pRecBuff      : Pointer;          { Record buffer(client) }
      precProps     : pRECProps         { Optional record properties }
   ): DBIResult stdcall;

function DbiGetRelativeRecord (         { Find/Get a record by record number }
      hCursor       : hDBICur;          { Cursor handle }
      iPosOffset    : Longint;          { offset from current position }
      eLock         : DBILockType;      { Optional lock request }
      pRecBuff      : Pointer;          { Record buffer(client) }
      precProps     : pRECProps         { Optional record properties }
   ): DBIResult stdcall;

function DbiInitRecord (                { Initialize record area }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuff      : Pointer           { Record buffer }
   ): DBIResult stdcall;

function DbiInsertRecord (              { Inserts a new record }
      hCursor       : hDBICur;          { Cursor handle }
      eLock         : DBILockType;      { Optional lock on this rec }
      pRecBuff      : Pointer           { New Record (client) }
   ): DBIResult stdcall;

function DbiModifyRecord (              { Updates the current record }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Modified record }
      bFreeLock     : Bool              { Free record lock }
   ): DBIResult stdcall;

function DbiDeleteRecord (              { Deletes the current record }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer           { Copy of deleted record }
   ): DBIResult stdcall;

function DbiReadBlock (                 { Read a block of records }
      hCursor       : hDBICur;          { Cursor handle }
var   iRecords      : Longint;          { Number of records to read }
      pBuf          : Pointer           { Buffer }
   ): DBIResult stdcall;

function DbiWriteBlock (                { Write a block of records }
      hCursor       : hDBICur;          { Cursor handle }
var   iRecords      : Longint;          { Number of records to write/written }
      pBuf          : Pointer           { Buffer }
   ): DBIResult stdcall;

function DbiAppendRecord (              { Inserts a new record }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuff      : Pointer           { New Record (client) }
   ): DBIResult stdcall;

function DbiUndeleteRecord (            { Undeletes the current record }
      hCursor       : hDBICur           { Cursor handle }
   ): DBIResult stdcall;

function DbiGetSeqNo (                  { Get logical record number }
      hCursor       : hDBICur;          { Cursor handle }
var   iSeqNo        : Longint           { Pointer to sequence number }
   ): DBIResult stdcall;

function DbiSetToSeqNo (                { Position to a logical record number }
      hCursor       : hDBICur;          { Cursor handle }
      iSeqNo        : Longint           { Sequence number }
   ): DBIResult stdcall;

function DbiGetRecordCount (            { Get the current number of records }
      hCursor       : hDBICur;          { Cursor handle }
var   iRecCount     : Longint           { Number of records }
   ): DBIResult stdcall;


function DbiGetExactRecordCount (       { Get the exact number of records }
      hCursor       : hDBICur;          { Cursor handle }
var   iRecCount     : Longint           { Number of records }
   ): DBIResult stdcall;


function DbiSetToRecordNo (             { Position to Physical Rec# }
      hCursor       : hDBICur;          { Cursor handle }
      iRecNo        : Longint           { Physical record number }
   ): DBIResult stdcall;

function DbiSaveChanges (               { Flush all buffered changes }
      hCursor       : hDBICur           { Cursor handle }
   ): DBIResult stdcall;

function DbiForceReread (               { Force Reread of buffers from Disk }
      hCursor       : hDBICur           { Cursor }
   ): DBIResult stdcall;

function DbiCheckRefresh: DBIResult stdcall;    { Check refresh for session }

function DbiMakePermanent (             { Make temporary table permanent }
      hCursor       : hDBICur;          { Cursor handle }
      pszName       : PAnsiChar;            { Rename temporary table }
      bOverWrite    : Bool              { Overwrite existing file }
   ): DBIResult stdcall;

function DbiForceRecordReread (         { Force Reread of current record from Server }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuff      : Pointer           { Returned : record buffer }
   ): DBIResult stdcall;

{============================================================================}
{                            Field Level Access                              }
{============================================================================}

function DbiGetField (                  { Get Field value }
      hCursor       : hDBICur;          { Cursor }
      iField        : Word;             { Field # (1..n) }
      pRecBuff      : Pointer;          { Record buffer }
      pDest         : Pointer;          { Destination field buffer }
var   bBlank        : Bool              { Returned : is field blank }
   ): DBIResult stdcall;

function DbiPutField (                  { Put a value in the record buffer }
      hCursor       : hDBICur;          { Cursor }
      iField        : Word;             { Field # (1..n) }
      pRecBuff      : Pointer;          { Record buffer }
      pSrc          : Pointer           { Source field buffer }
   ): DBIResult stdcall;

function DbiVerifyField (               { Verifies the field value }
      hCursor       : hDBICur;          { Cursor }
      iField        : Word;             { Field # (1..n) }
      pSrc          : Pointer;          { Field Value }
var   bBlank        : Bool              { Field is Blank (Returned) }
   ): DBIResult stdcall;

function DbiOpenBlob (                  { Open a blob for access }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record Buffer }
      iField        : Word;             { Field number (1..n) }
      eOpenMode     : DBIOpenMode       { Open for Read or RW }
   ): DBIResult stdcall;

function DbiGetBlobSize (               { Gets the size of a blob }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record Buffer }
      iField        : Word;             { Field number of blob (1..n) }
var   iSize         : Longint           { Blob size in bytes }
   ): DBIResult stdcall;

function DbiGetBlob (                   { Read bytes from blob }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record Buffer }
      iField        : Word;             { Field number of blob (1..n) }
      iOffSet       : Longint;          { Starting position }
      iLen          : Longint;          { No of bytes to be read }
      pDest         : Pointer;          { Destination }
var   iRead         : Longint           { Actual no of bytes read }
   ): DBIResult stdcall;

function DbiPutBlob (                   { Write bytes to blob }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record Buffer }
      iField        : Word;             { Field number of blob (1..n) }
      iOffSet       : Longint;          { Starting position }
      iLen          : Longint;          { No of bytes to put }
      pSrc          : Pointer           { pntr to Source }
   ): DBIResult stdcall;

function DbiTruncateBlob (              { Reduces the blob size }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record Buffer }
      iField        : Word;             { Field number of blob (1..n) }
      iLen          : Longint           { New blob length }
   ): DBIResult stdcall;

function DbiFreeBlob (                  { Closes the blob }
      hCursor       : hDBICur;          { Cursor handle }
      pRecBuf       : Pointer;          { Record Buffer }
      iField        : Word              { Field number of blob (0..n) }
   ): DBIResult stdcall;

function DbiGetBlobHeading (            { Get Blob Heading }
      hCursor       : hDBICur;          { Cursor handle }
      iField        : Word;             { Field number of blob (1..n) }
      pRecBuf       : Pointer;          { Record buffer of owner record }
      pDest         : Pointer           { Destination buffer }
   ): DBIResult stdcall;

function DbiSetFieldMap (               { Set a fieldmap }
      hCur          : hDBICur;          { Cursor handle }
      iFields       : Word;             { Number of fields }
      pFldDesc      : pFLDDesc          { Array of field descriptions }
   ): DBIResult stdcall;


{=============================================================================}
{                                TRANSACTIONS                                 }
{=============================================================================}

function DbiBeginTran (                 { Begin a transaction }
      hDb           : hDBIDb;           { Database handle }
      eXIL          : eXILType;         { Transaction isolation level }
var   hXact         : hDBIXact          { Returned Xact handle }
   ): DBIResult stdcall;

function DbiEndTran (                   { End a transaction }
      hDb           : hDBIDb;           { Database handle }
      hXact         : hDBIXact;         { Xact handle }
      eEnd          : eXEnd             { Xact end type }
   ): DBIResult stdcall;

function DbiGetTranInfo (               { Get transaction info }
      hDb           : hDBIDb;           { Database handle }
      hXact         : hDBIXact;         { Xact handle }
      pxInfo        : pXInfo            { Xact info }
   ): DBIResult stdcall;

{=============================================================================}
{                                  LOCKING                                    }
{=============================================================================}

function DbiAcqTableLock (              { Lock a table }
      hCursor       : hDBICur;          { Cursor handle }
      eLockType     : DBILockType       { Lock type }
   ): DBIResult stdcall;

function DbiAcqPersistTableLock (       { Get a persistent lock }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar             { Driver type / NULL }
   ): DBIResult stdcall;

function DbiRelPersistTableLock (       { Releases a persistent lock }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar             { Driver type / NULL }
   ): DBIResult stdcall;

function DbiRelTableLock (              { Unlocks Table level locks }
      hCursor       : hDBICur;          { Cursor handle }
      bAll          : Bool;             { True for all table level locks }
      eLockType     : DBILockType       { Specific lock type }
   ): DBIResult stdcall;

function DbiRelRecordLock (             { Releases record level locks }
      hCursor       : hDBICur;          { Cursor handle }
      bAll          : Bool              { True for all. Default Current. }
   ): DBIResult stdcall;

function DbiIsRecordLocked (            { Check if current record is locked }
      hCursor       : hDBICur;          { Cursor handle }
var   bLocked       : Bool              { Rec lock status }
   ): DBIResult stdcall;

function DbiIsTableLocked (             { Verify if Table is locked }
      hCursor       : hDBICur;          { Cursor handle }
      epdxLock      : DBILockType;      { Lock type to verify }
var   iLocks        : Word              { Nbr of locks of the given type }
   ): DBIResult stdcall;

function DbiIsTableShared (             { Verify if this is a shared table }
      hCursor       : hDBICur;          { Cursor handle }
var   bShared       : Bool              { Shared status }
   ): DBIResult stdcall;

function DbiOpenLockList (              { Get a list of locks }
      hCursor       : hDBICur;          { Cursor handle }
      bAllUsers     : Bool;             { True, for all Users locks }
      bAllLockTypes : Bool;             { True, for all lock types }
var   hLocks        : hDBICur           { Returned cursor on Lock list }
   ): DBIResult stdcall;

function DbiOpenUserList (              { Get a list of users loggedin }
var   hUsers        : hDBICur           { Returned cursor on user list }
   ): DBIResult stdcall;

function DbiSetLockRetry (              { Set Lock wait time }
      iWait         : SmallInt          { Time in seconds }
   ): DBIResult stdcall;

{============================================================================}
{                              Batch Operations                              }
{============================================================================}


function DbiBatchMove (                 { Copy records to destination table }
      pSrcTblDesc   : pBATTblDesc;      { Source table identification, }
      hSrcCur       : hDBICur;          {  OR source cursor  ( one must be NULL ) }
      pDstTblDesc   : pBATTblDesc;      { Destination table identification, }
      hDstCur       : hDBICur;          {  OR destination cursor ( one must be NULL ) }
      ebatMode      : eBATMode;         { Batch mode }
      iFldCount     : Word;             { Size of field maps }
      pSrcFldMap    : PWord;            { Array of source field numbers }
      pszIndexName  : PAnsiChar;            { If update mode, used to match records }
      pszIndexTagName : PAnsiChar;          { Index tag name }
      iIndexId      : Word;             { Index  id }
      pszKeyviolName : PAnsiChar;           { Keyviol table name  (optional) }
      pszProblemsName : PAnsiChar;          { Problems table name (optional) }
      pszChangedName : PAnsiChar;           { Changed table name (optional) }
      lProbRecs     : PLongint;         { Number records written to problem table }
      lKeyvRecs     : PLongint;         { Number records written to keyv table }
      lChangedRecs  : PLongint;         { Number records written to changed table }
      bAbortOnFirstProb : Bool;         { If TRUE, abort on first problem rec }
      bAbortOnFirstKeyviol : Bool;      { If TRUE, abort on first keyviol rec }
var   lRecsToMove   : Longint;          { Number of records to read from source }
      bTransliterate : Bool             { If TRUE, transliterate character data }
   ): DBIResult stdcall;

function DbiCopyTable (                 { Copy one table to another }
      hDb           : hDBIDb;           { Database handle }
      bOverWrite    : Bool;             { True, to overwrite existing file }
      pszSrcTableName : PAnsiChar;          { Source table name }
      pszSrcDriverType : PAnsiChar;         { Source driver type }
      pszDestTableName : PAnsiChar          { Destination table name }
   ): DBIResult stdcall;

function DbiEmptyTable (                { Deletes all records }
      hDb           : hDBIDb;           { Database handle }
      hCursor       : hDBICur;          { Cursor (OR) }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar             { Driver type /NULL }
   ): DBIResult stdcall;

function DbiPackTable (                 { Pack a table }
      hDb           : hDBIDb;           { Database handle }
      hCursor       : hDBICur;          { Cursor (OR) }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
      bRegenIdxs    : Bool              { Regenerate indexes }
   ): DBIResult stdcall;

function DbiRegenIndex (                { Regenerate an index }
      hDb           : hDBIDb;           { Database handle }
      hCursor       : hDBICur;          { Cursor (OR) }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
      pszIndexName  : PAnsiChar;            { Index name }
      pszIndexTagName : PAnsiChar;          { Index tagname (xbase MDX) }
      iIndexId      : Word              { Index number }
   ): DBIResult stdcall;

function DbiRegenIndexes (              { Regenerate all indexes }
      hCursor       : hDBICur           { Cursor }
   ): DBIResult stdcall;

function DbiSortTable (                 { Sort table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name of source }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
      hSrcCur       : hDBICur;          { OR cursor of table to sort }
      pszSortedName : PAnsiChar;            { Destination table (NULL if sort to self) }
      phSortedCur   : phDBICur;         { If non-null, return cursor on destination }
      hDstCur       : hDBICur;          { OR cursor of destination }
      iSortFields   : Word;             { Number of sort fields }
      piFieldNum    : PWord;            { Array of field numbers }
      pbCaseInsensitive : PBool;        { Which fields should sort c-i (Opt) }
      pSortOrder    : pSORTOrder;       { Array of Sort orders (Opt) }
      ppfSortFn     : ppfSORTCompFn;    { Array of compare fn pntrs (Opt) }
      bRemoveDups   : Bool;             { TRUE : Remove duplicates }
      hDuplicatesCur : hDBICur;         { Cursor to duplicates table (Opt) }
var   lRecsSort     : Longint           { In/out param. - sort this number }
   ): DBIResult stdcall;


{============================================================================}
{                           Create & Restructure                             }
{============================================================================}

function DbiCreateTable (               { Create a new table }
      hDb           : hDBIDb;           { Database handle }
      bOverWrite    : Bool;             { True, to overwrite existing file. }
var   crTblDsc      : CRTblDesc         { Table description }
   ): DBIResult stdcall;

function DbiCreateInMemTable (          { Create a temporary table (Logical) }
      hDb           : hDBIDb;           { Database handle }
      pszName       : PAnsiChar;            { Logical Name }
      iFields       : Word;             { No of fields }
      pfldDesc      : pFLDDesc;         { Array of field descriptors }
var   hCursor       : hDBICur           { Returned cursor handle }
   ): DBIResult stdcall;

function DbiCreateTempTable (           { Create temporary table (Physical) }
      hDb           : hDBIDb;           { Database handle }
var   crTblDsc      : CRTblDesc;        { Table description }
var   hCursor       : hDBICur           { Returned cursor on table }
   ): DBIResult stdcall;

function DbiDoRestructure (             { Restructure a table }
      hDb           : hDBIDb;           { Database handle }
      iTblDescCount : Word;             { Number of table descriptors (1) }
      pTblDesc      : pCRTblDesc;       { Array of table descs }
      pszSaveAs     : PAnsiChar;            { Restructure to this table }
      pszKeyviolName : PAnsiChar;           { Keyviol table name  (optional) }
      pszProblemsName : PAnsiChar;          { Problems table name (optional) }
      bAnalyzeOnly  : Bool              { Analyze restructure }
   ): DBIResult stdcall;

function DbiRenameTable (               { Rename table & family }
      hDb           : hDBIDb;           { Database handle }
      pszOldName    : PAnsiChar;            { Old name }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
      pszNewName    : PAnsiChar             { New name }
   ): DBIResult stdcall;

function DbiDeleteTable (               { Delete a table }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Name including any path }
      pszDriverType : PAnsiChar             { Driver type /NULL }
   ): DBIResult stdcall;

function DbiAddIndex (                  { Add a new index }
      hDb           : hDBIDb;           { Database handle }
      hCursor       : hDBICur;          { Cursor (OR) }
      pszTableName  : PAnsiChar;            { Table name including any path }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
var   IdxDesc       : IDXDesc;          { Description of the index }
      pszKeyviolName : PAnsiChar            { Keyviol table name (optional) }
   ): DBIResult stdcall;

function DbiDeleteIndex (               { Delete index }
      hDb           : hDBIDb;           { Database handle }
      hCursor       : hDBICur;          { Cursor (OR) }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
      pszIndexName  : PAnsiChar;            { Index name }
      pszIndexTagName : PAnsiChar;          { Index tagname (xbase MDX) }
      iIndexId      : Word              { Index number }
   ): DBIResult stdcall;

{===========================================================================}
{                            Error Info                                     }
{===========================================================================}

function DbiGetErrorEntry (             { Get error entry }
      uEntry        : Word;             { Error stack entry }
var   ulNativeError : Longint;          { Returned. Native error code, if any }
      pszError      : PAnsiChar             { Returned. Error string, if any }
   ): DBIResult stdcall;

function DbiGetErrorInfo (              { Return info on last error }
      bFull         : Bool;             { If Full details }
var   ErrInfo       : DBIErrInfo        { Error Info }
   ): DBIResult stdcall;

function DbiGetErrorString (            { Get message for error code }
      rslt          : DBIResult;        { Engine error code }
      pszError      : PAnsiChar             { Error string for the error }
   ): DBIResult stdcall;

function DbiGetErrorContext (           { Get specific Context if available }
      eContext      : SmallInt;         { Context type }
      pszContext    : PAnsiChar             { Context string (MAXMSGLEN +1) }
   ): DBIResult stdcall;

{============================================================================}
{                              Dbi Services                                  }
{============================================================================}

function DbiDateEncode (                { Encode Date components into Date }
      iMon          : Word;             { Month    (1..12) }
      iDay          : Word;             { Day      (1..31) }
      iYear         : SmallInt;         { Year     (0..2**16-1) }
var   dateD         : DBIDATE           { Encoded date }
   ): DBIResult stdcall;

function DbiDateDecode (                { Decode Date into components }
      dateD         : DBIDATE;          { Encoded Date }
var   iMon          : Word;             { Month }
var   iDay          : Word;             { Day }
var   iYear         : SmallInt          { Year }
   ): DBIResult stdcall;

function DbiTimeEncode (                { Encode Time components into TIME }
      iHour         : Word;             { Hours (0..23) }
      iMin          : Word;             { Minutes (0..59) }
      iMilSec       : Word;             { Milli Seconds (0..59999) }
var   timeT         : Time              { Encoded Time }
   ): DBIResult stdcall;

function DbiTimeDecode (                { Decode TIME into components }
      timeT         : Time;             { Encoded Time }
var   iHour         : Word;             { Hours (0..23) }
var   iMin          : Word;             { Minutes (0..59) }
var   iMilSec       : Word              { Milli Seconds (0..59999) }
   ): DBIResult stdcall;

function DbiTimeStampEncode (           { Encode Date & TIme into Date+Time }
      dateD         : DBIDATE;          { Encoded Date }
      timeT         : Time;             { Encoded Time }
var   tsTS          : TimeStamp         { Encoded Date+Time }
   ): DBIResult stdcall;

function DbiTimeStampDecode (           { Decode Date & Time from Date+Time }
      tsTS          : TIMESTAMP;        { Encoded Date+Time }
var   dateD         : DBIDATE;          { Encoded Date }
var   timeT         : Time              { Encoded Time }
   ): DBIResult stdcall;


function DbiBcdFromFloat (              { Converts FLOAT number into FMTBcd format }
var   iVal          : Double;           { Float to convert }
      iPrecision    : Word;             { Precision of BCD }
      iPlaces       : Word;             { Number of decimals }
var   Bcd           : FMTBcd            { returns Bcd number (length = iPrecision +2) }
   ): DBIResult stdcall;

function DbiBcdToFloat (                { Converts FMTBcd number to FLOAT }
var   Bcd           : FMTBcd;           { Bcd number to convert }
var   iVal          : Double            { Returns converted float }
   ): DBIResult stdcall;

{===========================================================================}
{                           CallBacks                                       }
{===========================================================================}

function DbiRegisterCallBack (          { Register a call back fn }
      hCursor       : hDBICur;          { Cursor (Optional) }
      ecbType       : CBType;           { Type of call back }
      iClientData   : Longint;          { Pass-thru client data }
      iCbBufLen     : Word;             { Callback buffer len }
      CbBuf         : Pointer;          { Pointer to callback buffer }
      pfCb          : pfDBICallBack     { Call back fn being registered }
   ): DBIResult stdcall;

function DbiGetCallBack (               { Register a call back fn }
      hCursor       : hDBICur;          { Cursor (Optional) }
      ecbType       : CBType;           { Type of call back }
      piClientData  : PLongint;         { Pass-thru client data }
      piCbBufLen    : PWord;            { Callback buffer len }
      ppCbBuf       : Pointer;          { Pointer to callback buffer }
var   pfCb          : pfDBICallBack     { Call back fn being registered }
   ): DBIResult stdcall;

{============================================================================}
{                          Date, time formats                                }
{============================================================================}


function DbiGetDateFormat (             { Get current date format }
var   fmtDate       : FMTDate
   ): DBIResult stdcall;

function DbiSetDateFormat (             { Set current date format }
var   fmtDate       : FMTDate
   ): DBIResult stdcall;

function DbiGetTimeFormat (             { Get current time format }
var   fmtTime       : FMTTime
   ): DBIResult stdcall;

function DbiSetTimeFormat (             { Set current time format }
var   fmtTime       : FMTTime
   ): DBIResult stdcall;

function DbiGetNumberFormat (           { Get current number format }
var   fmtNumber     : FMTNumber
   ): DBIResult stdcall;

function DbiSetNumberFormat (           { Set current number format }
var   fmtNumber     : FMTNumber
   ): DBIResult stdcall;

{============================================================================}
{                      Conversions                                           }
{============================================================================}

function DbiNativeToAnsi (              { Convert from native to Ansi }
      LdObj         : Pointer;          { Language driver }
      pAnsiStr      : PAnsiChar;            { Destination buffer (opt) }
      pNativeStr    : PAnsiChar;            { Source buffer }
      iLen          : Longint;          { Length of buffer (opt) }
var   bDataLoss     : Bool              { Returns TRUE if conversion will loose data (opt) }
   ): DBIResult stdcall;

function DbiAnsiToNative (              { Convert from Ansi to native }
      LdObj         : Pointer;          { Language driver }
      pNativeStr    : PAnsiChar;            { Destination buffer (opt) }
      pAnsiStr      : PAnsiChar;            { Source buffer }
      iLen          : Longint;          { Length of buffer (opt) }
var   bDataLoss     : Bool              { Returns TRUE if conversion will loose data (opt) }
   ): DBIResult stdcall;

{============================================================================}
{                            Filters                                         }
{============================================================================}

function DbiAddFilter (                 { Add a filter to the cursor }
      hCursor       : hDBICur;          { Cursor handle }
      iClientData   : Longint;          { Client supplied data      (opt) }
      iPriority     : Word;             { 1..N with 1 being highest (opt) }
      bCanAbort     : Bool;             { TRUE if pfFiltercan return ABORT (opt) }
      pcanExpr      : pCANExpr;         { Expression tree        (opt) }
      pfFilter      : pfGENFilter;      { ptr to filter function (opt) }
var   hFilter       : hDBIFilter        { Returns filter handle }
   ): DBIResult stdcall;

function DbiDropFilter (                { Drop a filter }
      hCursor       : hDBICur;          { Cursor handle }
      hFilter       : hDBIFilter        { Filter handle }
   ): DBIResult stdcall;

function DbiActivateFilter (            { Activate a Filter }
      hCursor       : hDBICur;          { Cursor handle }
      hFilter       : hDBIFilter        { Filter handle }
   ): DBIResult stdcall;

function DbiDeactivateFilter (          { Deactivate Filter }
      hCursor       : hDBICur;          { Cursor handle }
      hFilter       : hDBIFilter        { Filter handle }
   ): DBIResult stdcall;

function DbiGetFilterInfo (             { Get filter information }
      hCur          : hDBICur;          { Cursor handle }
      hFilter       : hDBIFilter;       { Filter handle          /NULL }
      iFilterId     : Word;             { Filter id              /0 }
      iFilterSeqNo  : Word;             { Filter sequence number /0 }
var   Filterinfo    : FilterInfo        { Returns filter info }
   ): DBIResult stdcall;

{============================================================================}
{                            Linked Cursors                                  }
{============================================================================}

function DbiBeginLinkMode (             { Convert cursor to a link cursor }
var   hCursor       : hDBICur           { In/Out : returns new cursor }
   ): DBIResult stdcall;

function DbiEndLinkMode (               { Convert cursor back to normal cursor }
var   hCursor       : hDBICur           { In/Out : returns original cursor }
   ): DBIResult stdcall;

function DbiLinkDetail (                { Link detail to master }
      hMstrCursor   : hDBICur;          { Master cursor }
      hDetlCursor   : hDBICur;          { Detail cursor }
      iLnkFields    : Word;             { Number of link fields }
      piMstrFields  : PWord;            { Array of fields in master }
      piDetlFields  : PWord             { Array of fields in detail }
   ): DBIResult stdcall;

function DbiLinkDetailToExp (           { Link detail to a master using exp }
      hCursorMstr   : hDBICur;          { Master cursor }
      hCursorDetl   : hDBICur;          { Detail cursor }
      iKeyLen       : Word;             { Key length to match }
      pszMstrExp    : PAnsiChar             { Expression string }
   ): DBIResult stdcall;

function DbiUnlinkDetail (              { Unlink detail from master }
      hDetlCursor   : hDBICur           { Detail cursor to unlink }
   ): DBIResult stdcall;

function DbiGetLinkStatus (             { Query linkage info for table }
      hCursor       : hDBICur;          { Cursor handle }
var   hCursorMstr   : hDBICur;          { Returns master cursor, if any   (opt) }
var   hCursorDet    : hDBICur;          { Returns first detail cursor, if any (opt) }
var   hCursorSib    : hDBICur           { Returns next sibling detail cursor, if any (opt) }
   ): DBIResult stdcall;


{===========================================================================}
{                            Translation                                    }
{===========================================================================}


function DbiTranslateRecordStructure (  { Translate a record }
      pszSrcDriverType : PAnsiChar;         { Source driver type }
      iFlds         : Word;             { Number of fields }
      pfldsSrc      : pFLDDesc;         { Array of source fields: logical or physical types }
      pszDstDriverType : PAnsiChar;         { Destination driver type }
      pszLangDriver : PAnsiChar;            { Language driver for destination }
      pfldsDst      : pFLDDesc;         { Array of dest. fields returned }
      bCreatable    : Bool              { TRUE -> map to creatable fields only. }
   ): DBIResult stdcall;

function DbiOpenFieldXlt (              { Open translation object }
      pszSrcTblType : PAnsiChar;            { NULL for Logical }
      pszSrcLangDrv : PAnsiChar;            { NULL if no tranliteration }
      pfldSrc       : pFLDDesc;         { source field descriptor }
      pszDestTblType : PAnsiChar;           { NULL for Logical }
      pszDstLangDrv : PAnsiChar;            { NULL if no tranliteration }
      pfldDest      : pFLDDesc;         { Source field descriptor }
var   bDataLoss     : Bool;             { Set to TRUE, for data loss }
var   hXlt          : hDBIXlt           { Returned translate handle }
   ): DBIResult stdcall;

function DbiTranslateField (            { Translate a field }
      hXlt          : hDBIXlt;          { Translation handle }
      pSrc          : Pointer;          { Source field }
      pDest         : Pointer           { Destination field }
   ): DBIResult stdcall;

function DbiCloseFieldXlt (             { Close translation object }
      hXlt          : hDBIXlt           { Translation handle }
   ): DBIResult stdcall;

{=========================================================================}
{    Delayed Updates                                                      }
{=========================================================================}

function DbiBeginDelayedUpdates (       { put cursor in delayed update mode }
var   hCursor       : hDBICur           { In/Out : returns new Cursor }
   ): DBIResult stdcall;

function DbiEndDelayedUpdates (         { Convert cursor back to normal cursor }
var   hCursor       : hDBICur           { In/Out : returns original Cursor }
   ): DBIResult stdcall;

function DbiApplyDelayedUpdates (       { Perform the specified operation. }
      hCursor       : hDBICur;          { Delayed update cursor handle }
      eUpdCmd       : DBIDelayedUpdCmd  { Op Type: Commit or Rollback. }
   ): DBIResult stdcall;

{===========================================================================}
{                                 MISC.                                     }
{===========================================================================}


function DbiGetTableOpenCount (         {  Get local cursor count }
      hDb           : hDBIDb;           { Database }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
var   iOpenCount    : Word              { returned number of cursors }
   ): DBIResult stdcall;

function DbiUseIdleTime: DBIResult stdcall;     { Use Idle time }

function DbiGetLdObj (                  { Get language driver }
      hCursor       : hDBICur;          { Cursor handle }
var   pLdObj        : Pointer           { Returned language driver object }
   ): DBIResult stdcall;

function DbiGetLdName (                 { Get language driver name from table }
      pszDriver     : PAnsiChar;            { Driver name }
      pObjName      : PAnsiChar;            { Name of object, i.e. table name }
      pLdName       : PAnsiChar             { Returned language driver name }
   ): DBIResult stdcall;

function DbiFormFullName (              { Form Full Name }
      hDb           : hDBIDb;           { Database handle }
      pszTableName  : PAnsiChar;            { Table name }
      pszDriverType : PAnsiChar;            { Driver type /NULL }
      pszFullName   : PAnsiChar             { Returns full name }
   ): DBIResult stdcall;

function DbiAddPassword (               { Add a password to current session }
      pszPassword   : PAnsiChar             { Password }
   ): DBIResult stdcall;

function DbiDropPassword (              { Drop a password from current session }
      pszPassword   : PAnsiChar             { password/NULL }
   ): DBIResult stdcall;

function DbiGetNetUserName (            { Get network username }
      pszNetUserName : PAnsiChar            { Returns username }
   ): DBIResult stdcall;


function DbiDebugLayerOptions (         { Get SDK debug layer options }
      iOption       : Word;             { Option }
      pDebugFile    : PAnsiChar
   ): DBIResult stdcall;

function DbiOpenCfgInfoList (           { Open a cursor on "Config" }
      hCfg          : hDBICfg;          { NULL }
      eOpenMode     : DBIOpenMode;      { ReadWrite or readonly }
      eConfigMode   : CFGMode;          { Config mode }
      pszCfgPath    : PAnsiChar;            { Path }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult stdcall;

function DbiImportODBC(                 { Import current ODBC drivers and data sources }
      hCfg          : hDBICfg;          { NULL }
      bPersist      : Bool              { Persistent or session relative }
   ): DBIResult stdcall;

function DbiAddAlias (                  { Add a new alias }
      hCfg          : hDBICfg;          { NULL }
      pszAliasName  : PAnsiChar;            { Alias name }
      pszDriverType : PAnsiChar;            { Driver type for alias }
      pszParams     : PAnsiChar;            { Optional parameters }
      bPersist      : Bool              { Persistent or session relative }
   ): DBIResult stdcall;

function DbiDeleteAlias (               { Delete an alias }
      hCfg          : hDBICfg;          { NULL }
      pszAliasName  : PAnsiChar             { Alias name }
   ): DBIResult stdcall;

function DbiAddDriver(                  { Add a new driver }
      hCfg          : hDBICfg;          { NULL }
      pszDriverName : PAnsiChar;            { Driver name }
      pszParams     : PAnsiChar;            { Optional parameters }
      bPersist      : Bool              { Persistent or session relative }
   ): DBIResult stdcall;

function DbiDeleteDriver(               { Delete a driver }
      hCfg          : hDBICfg;          { NULL }
      pszDriverName : PAnsiChar;            { Driver name }
      bSave         : Bool              { Save config file }
   ): DBIResult stdcall;

function DbiGetSQLRequest(
      hDb           : hDBIDb;           { Database handle }
      pszSQLStmt    : pByte;            { SQL statement }
      var bIsDDL    : Boolean;          { Returned TRUE if statement is DDL. }
      var pReq      : PSQLRequest       { Returned Handle to SQLRequest. }
   ): DBIResult stdcall;

function DbiFreeSQLRequest(
      pReq          : PSQLRequest
   ): DBIResult stdcall;

{$IFNDEF UNDOCUMENTED}

{ This IFDEF is here so PP.EXE will strip out these undocumented BDE API's }

function DbiOpenConfigFile (            { Open/Create configuration }
      pszDirPath    : PAnsiChar;            { Directory }
      bCreate       : Bool;             { TRUE to create/overwrite }
var   hCfg          : hDBICfg           { Handle to config }
   ): DBIResult stdcall;

function DbiCloseConfigFile (           { Close the config file }
var   hCfg          : hDBICfg;          { Handle }
      bSave         : Bool;             { To save the changes }
      bDefault      : Bool;             { To make this file the default }
      bSaveAs16     : Bool              { To save as a 16-bit config file }
   ): DBIResult stdcall;

function DbiCfgSave (                   { Save current configuration }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszNewFile    : PAnsiChar;            { (Optional): Save as this file }
      bSaveAs16     : Bool              { To save as a 16-bit config file }
   ): DBIResult stdcall;

function DbiCfgBuildPath (              { Build a path for accessing config. }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszCategory   : PAnsiChar;            { NULL for root }
      pszGroup      : PAnsiChar;            { NULL for no group }
      pszSection    : PAnsiChar;            { NULL fro no section }
      pszCfgPath    : PAnsiChar             { Returned path }
   ): DBIResult stdcall;

function DbiCfgPosition (               { Position the config path }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszCfgPath    : PAnsiChar             { Path }
   ): DBIResult stdcall;

function DbiCfgGetNextNode (            { Get next defined node }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszNodeName   : PAnsiChar             { Node name (Returned) }
   ): DBIResult stdcall;

function DbiCfgGetRecord (              { Get a record }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszCfgPath    : PAnsiChar;            { Path }
var   iFields       : Word;             { Returned nbr of fields }
      pfldDesc      : pFLDDesc;         { Field descriptors }
      pRec          : Pointer           { Field values }
   ): DBIResult stdcall;

function DbiCfgAddRecord (              { Add a record }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszCfgPath    : PAnsiChar;            { Path }
      iFields       : Word;             { No of fields }
      pfldDesc      : pFLDDesc;         { Field descriptors }
      pRec          : Pointer           { Data values }
   ): DBIResult stdcall;

function DbiCfgModifyRecord (           { Modify a record }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszCfgPath    : PAnsiChar;            { Path }
      iFields       : Word;             { Nbr of fields }
      pfldDesc      : pFLDDesc;         { Field descriptors }
      pRec          : Pointer           { Data values }
   ): DBIResult stdcall;

function DbiCfgDropRecord (             { Delete a record }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszCfgPath    : PAnsiChar             { Path }
   ): DBIResult stdcall;

function DbiCfgTranslate (              { Translate configuration string }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszConfigPath : PAnsiChar;            { configuration path }
      pszTrans      : PAnsiChar;            { translated path }
      iFields       : Word;             { number of fields, 0 if path only }
      pfldDesc      : pFLDDesc;         { field descriptors, NULL if path only }
      pRec          : Pointer           { record buffer, NULL if path only }
   ): DBIResult stdcall;

function DbiCfgGetHelp (                { Get help message }
      hCfg          : hDBICfg;          { Config Handle/NULL }
      pszConfigPath : PAnsiChar;            { configuration path }
      pszFldName    : PAnsiChar;            { field name  ( optional ) }
      pszHelpString : PAnsiChar             { help string ( returned ) }
   ): DBIResult stdcall;

function DbiCfgMerge (                  { Merge config files }
      hCfgDest      : hDBICfg;          { Destination Cfg handle/ NULL }
      hCfgSrc       : hDBICfg;          { Source Cfg handle/ NULL }
      pszSrcPath    : PAnsiChar             { Source Path / NULL }
   ): DBIResult stdcall;

{$ENDIF}

{===========================================================================}
{                      Query Management                                     }
{===========================================================================}

function DbiQExecDirect (               { Execute query }
      hDb           : hDBIDb;           { Database handle }
      eQryLang      : DBIQryLang;       { Query language }
      pszQuery      : PAnsiChar;            { Query }
      phCur         : phDBICur          { Returned cursor on result set }
   ): DBIResult stdcall;

function DbiQAlloc (                    { Allocates a statement handle }
      hDb           : hDBIDb;           { Database handle }
      eQryLang      : DBIQryLang;       { Query language }
var   hStmt         : hDBIStmt          { Returned statment handle }
   ): DBIResult stdcall;

function DbiQPrepare (                  { Prepare a query }
      hStmt         : hDBIStmt;         { Returned statment handle }
      pszQuery      : PAnsiChar             { Query }
   ): DBIResult stdcall;

function DbiQPrepareExt (               { Prepare a query }
      hDb           : hDBIDb;           { Database handle }
      eQryLang      : DBIQryLang;       { Query language }
      pszQuery      : PAnsiChar;            { Query }
      propBits      : Word;             { properties for Prepare, e.g. qprepFORUPDATE }
var   hStmt         : hDBIStmt          { Returned statment handle }
   ): DBIResult stdcall;

function DbiQExec (                     { Execute prepared query }
      hStmt         : hDBIStmt;         { Statement handle }
      phCur         : phDBICur          { Returned handle on result set }
   ): DBIResult stdcall;

function DbiQFree (                     { Free statement handle }
var   hStmt         : hDBIStmt          { Statement handle }
   ): DBIResult stdcall;

function DbiQSetParams (                { Set query options }
      hStmt         : hDBIStmt;         { Statement handle }
      uFldDescs     : Word;             { Number of parameter field descriptors }
      paFldDescs    : pFLDDesc;         { Array of parameter field descriptors }
      pRecBuff      : Pointer           { Record buffer }
   ): DBIResult stdcall;

function DbiQInstantiateAnswer (        { Create answer table }
      hStmt         : hDBIStmt;         { Statement Handle }
      hCur          : hDBICur;          { Cursor Handle }
      pszAnswerName : PAnsiChar;            { Answer Table Name/NULL }
      pszAnswerType : PAnsiChar;            { Answer Table Type/NULL }
      bOverWrite    : Bool;             { Overwrite Flag }
      phCur         : phDBICur          { cursor to instantiated table (output)(optional) }
   ): DBIResult stdcall;

function DbiQExecProcDirect (           { Direct execution of stored procedure }
      hDb           : hDBIDb;           { Database handle }
      pszProc       : PAnsiChar;            { Stored procedure name }
      uParamDescs   : Word;             { Number of parameter descriptors }
      paParamDescs  : pSPParamDesc;     { Array of parameter descriptors }
      pRecBuff      : Pointer;          { Record buffer }
var   hCur          : hDBICur           { Returned handle on result set }
   ): DBIResult stdcall;

function DbiQPrepareProc (              { Prepare a stored procedure }
      hDb           : hDBIDb;           { Database handle }
      pszProc       : PAnsiChar;            { Stored procedure name }
      uParamDescs   : Word;             { Number of parameter descriptors }
      paParamDescs  : pSPParamDesc;     { Array of parameter descriptors }
      pRecBuff      : Pointer;          { Record buffer }
var   hStmt         : hDBIStmt          { Returned statment handle }
   ): DBIResult stdcall;

function DbiQSetProcParams (            { Set procedure params }
      hStmt         : hDBIStmt;         { Statement handle }
      uParamDescs   : Word;             { Number of parameter descriptors }
      paParamDescs  : pSPParamDesc;     { Array of parameter descriptors }
      pRecBuff      : Pointer           { Record buffer }
   ): DBIResult stdcall;

type
  PSTMTBaseDesc = ^STMTBaseDesc;
  STMTBaseDesc = packed record
    szDatabase   : DBINAME;
    szTableName  : DBITBLNAME;
    szFieldName  : DBINAME;
    bExpression  : Bool;
    bAggregate   : Bool;
    bConstant    : Bool;
  end;

function DbiQGetBaseDescs(
      hStmt         : hDBIStmt;         { Statement Handle }
var   hCur          : hDBICur           { Cursor of type STMTBaseDesc }
   ): DBIResult stdcall;

{$IFNDEF UNDOCUMENTED}

{ This IFDEF is here so PP.EXE will strip out these undocumented BDE API's }

{===========================================================================}
{                      Language drivers                                     }
{===========================================================================}

const
  MAXLDNAME = 20;
  MAX_LCNAME_LEN = 32;
  DOS_CP = 1;
  WIN_CP = 2;
  UC_CP = 13;

type
  TOSBLObj = packed record
    LangId: SmallInt;
    LdName: array[0..MAXLDNAME - 1] of AnsiChar;
    EnglishDesc: array[0..MAX_LCNAME_LEN - 1] of AnsiChar;
    LdDescriptor: array[0..19] of AnsiChar;
    LocaleHdl: Pointer;
    CodeSet: SmallInt;
    LocaleType: SmallInt;
    LanguageSortType: SmallInt;
    LocaleCount: SmallInt;
    PrimaryCpPlatform: SmallInt;
    PrimaryCodePageID: Word;
    AlternateCodePageID: Word;
    ProductID: Word;
    NoConversions: Bool;
    LdLCID: Integer;
  end;

function OsLdInit (                     { Initialize a Language Driver }
      pDefDir       : PAnsiChar;
      pDefLangDr    : PAnsiChar
   ): DBIResult stdcall;

function OsLdExit: DBIResult stdcall;

function OsLdGetDefaultObj (            { Get default language driver }
var   pLdObj        : PLocale
   ): DBIResult stdcall;

function OsLdGetSymbName (              { Get symbolic name }
      pLdObj        : PLocale;
      pSymbName     : PByte
   ): DBIResult stdcall;

function OsLdGetDriverId (              { Get driver id }
      pLdObj        : PLocale;
var   DriverId      : Word
   ): DBIResult stdcall;

function OsLdGetDescName (              { Get desciptive name }
      pLdObj        : PLocale;
      pDescName     : PByte
   ): DBIResult stdcall;

function OsLdGetSortsig (               { Get sort signature (paradox) }
      pLdObj        : PLocale;
var   SortSig       : SmallInt
   ): DBIResult stdcall;

function OsLdGetProductID (             { Get product ID (DBASE, PDOX, etc.) }
      pLdObj        : Pointer;
      piProdID      : PWord
   ): DBIResult stdcall;

function OsLdGetCodePage (              { Get codepage }
      pLdObj        : PLocale;
var   CodePage      : SmallInt
): DBIResult stdcall;

function OsLdGetFName (                 { Get filename (DOS only) }
      pLdObj        : PLocale;
      PName         : PAnsiChar
   ): DBIResult stdcall;

{ Init scan through available language drivers }
function OsLdSearchInit(var SHandle: Pointer): DBIResult stdcall;
function OsLdSearchEnd(SHandle: Pointer): DBIResult stdcall;

{ Get LdObj for next language driver in list }
{ (Note: the returned ppLdObj can only be used for the informational functions : }
{ OsLdGetSymbName, etc. }
function OsLdSearchNext(SHandle: Pointer; var ppLdObj: Pointer): DBIResult stdcall;

function OsLdLoadBySortSig (
      SortSig       : Word;
      CodePage      : Word;
var   pLdObj        : PLocale
    ): DBIResult stdcall;

function OsLdLoadByFName (
      pFName        : PAnsiChar;
var   pLdObj         :PLocale
   ): DBIResult stdcall;

function OsLdLoadBySymbName (
      pSymbName     : PAnsiChar;
var   pLdObj        : PLocale
   ): DBIResult stdcall;

function OsLdLoadByDriverId (
      iDriverId     : Word;
var   pLdObj        : PLocale
   ): DBIResult stdcall;

function OsLdLoadByLCID (
      _lcid         : Integer;
var   pLdObj: Pointer
   ): DBIResult;

function OsLdUnloadObj (
var   pLdObj        : PLocale
   ): DBIResult stdcall;

function OsLdGetMinMax (
      pLdObj        : Pointer;
      pcMin         : PAnsiChar;
      pcMax         : PAnsiChar
   ): DBIResult stdcall;

function OsLdStrnCmp (
      pLdObj        : PLocale;
      Str1          : PAnsiChar;
      Str2          : PAnsiChar;
      Len           : Word
   ): DBIResult stdcall;

function OsLdStrCmp (
      pLdObj        : PLocale;
      Str1          : PAnsiChar;
      Str2          : PAnsiChar
   ): DBIResult stdcall;

function OsLdStrToUpper (
      pLdObj        : PLocale;
      Str           : PByte
   ): DBIResult stdcall;

function OsLdStrnToUpper (
      pLdObj        : PLocale;
      Str           : PByte;
      Len           : Word
   ): DBIResult stdcall;

function OsLdStrToLower (
      pLdObj        : PLocale;
      Str           : PByte
   ): DBIResult stdcall;

function OsLdStrnToLower (
      pLdObj        : PLocale;
      Str           : PByte;
      Len           : Word
   ): DBIResult stdcall;

function OsLdStrCmpi (
      pLdObj        : PLocale;
      str1          : PAnsiChar;
      str2          : PAnsiChar
   ): SmallInt stdcall;

function OsLdStrnCmpi (
      pLdObj        : PLocale;
      Str1          : PAnsiChar;
      Str2          : PAnsiChar;
      Len           : Word
   ): SmallInt stdcall;

{ Multi purpose translate function : }
{ If destination is NULL, performs check only. }
{ If iLen = 0, assumes zero-terminated string, otherwise iLen is lenght of buffer to convert }
{ if destination == source, in place conversion is performed }
{ Works on both Ansi and Oem drivers. }
{ NOTE: does not take into account Japanese problem. }
function OsLdSetConvChars (
      o2a           : Shortint;
      a2o           : Shortint
   ): DBIResult;

function OsLdOemToAnsi (
      pLdObj        : PLocale;
      AnsiStr       : PAnsiChar;
      OemStr        : PAnsiChar;
      Len           : Word;
var   DataLoss      : BOOL
   ): DBIResult stdcall;

function OsLdAnsiToOem (
      pLdObj        : PLocale;
      OemStr        : PAnsiChar;
      AnsiStr       : PAnsiChar;
      Len           : Word;
var   DataLoss      : BOOL
): DBIResult stdcall;

function OsLdIsAnsiCharInOemCp (
      pLdObj        : PLocale;
      AnsiCh        : AnsiChar
   ): Bool stdcall;

function OsLdIsOemCharInAnsiCp (
      pLdObj        : PLocale;
      OemCh         : AnsiChar
   ): Bool stdcall;

function OsLdAnsiCharToOem (
      pLdObj        : PLocale;
      ch            : Byte
   ): Byte stdcall;

function OsLdOemCharToAnsi (
      pLdObj        : PLocale;
      ch            : Byte
   ): Byte stdcall;

function OsLdCharToUpper (
      pLdObj        : PLocale;
      ch            : Byte
   ): Byte stdcall;

function OsLdCharToLower (
      pLdObj        : PLocale;
      ch            : Byte
   ): Byte stdcall;

function OsLdIsAlpha (
      pLdObj        : PLocale;
      ch            : Byte
   ): Bool stdcall;

function OsLdIsDigit (
      pLdObj        : PLocale;
      ch            : Byte
   ): Bool stdcall;

function OsLdBuildCharSetXform (
      pCharMap      : PAnsiChar;
      pLdObjSrc     : PLocale;
      pLdObjDest    : PLocale;
var   DataLoss      : Bool
   ): Bool stdcall;

function OsLdGetFuncAddrLdStrnColli_ (
var   ppf           : Pointer
   ): DBIResult stdcall;

function OsLdGetFuncAddrLdStrnColl_ (
var   ppf           : Pointer
   ): DBIResult stdcall;

function OsLdGetFuncAddrLdStrnCmpi_ (
var   ppf           : Pointer
   ): DBIResult stdcall;


function OsLdStrnColl (
      pLdObj        : Pointer;
      Str1          : PByte;
      Str2          : PByte;
      Maxlen        : Word
   ): Shortint stdcall;

function OsLdStrnColli (
      pBlObj        : Pointer;
      str1          : PAnsiChar;
      str2          : PAnsiChar;
      iLen          : Word
   ): Shortint stdcall;

function OsLdExStrCmp (
      pLdObj        : Pointer;
      Right         : PByte;
      RightLen      : Shortint;
      Left          : PByte;
      LeftLen       : Shortint;
      Exact: Bool
   ): Shortint stdcall;

function DbiGetLdNameFromDb (
      hDb           : hDbiDb;
      DbName        : PAnsiChar;
      pldName       : PAnsiChar
   ): DBIResult stdcall;

{===========================================================================}
{                      Data Repository                                      }
{===========================================================================}

{ Translated from DR.H,  Revsion 4.20 }

{Client Data Repository General Functions : Set Up, Create, etc. }
{--------------------------------------------------------------- }

type
  pDRDesc = ^DRDesc;
  DRDesc = packed record
    szDRName        : DBINAME;          { Repository name }
    szDRDescText    : DBIDRTYPEDESC;    { Documentary description }
    szDRDbName      : DBINAME;          { Name of Repository Database (Alias) }
    szDRDbDir       : DBIPATH;          { Name of DB directory (valid only for Standard DBs that don't exist) }
    szDRTableName   : DBINAME;          { Name of Repository Table }
    szLdName        : DBINAME;          { Language driver name }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;

function DbiDRCreate (                  { Create a new Data Repository }
      pdrDesc       : pDRDesc;          { Repository Details }
      phDataRepos   : phDBIDR           { OUT - Data Repository handle }
   ): DBIResult; stdcall;

   { Creates a New Client Data Repository. The location of the Database for }
   { this Repository is specified in pszDrDbName. The Repository Database   }
   { could be local e.g. Paradox tables, or Remote e.g. Oracle Table. This  }
   { Database should exist before DbiDRCreate() is called.                  }
   { The Repository tables are created and an entry about this Repository   }
   { is made in the Registry.                                               }
   { szLdName specifies the Language driver for the Repository. This MUST be }
   { an ANSI language driver and should be supported by the underlying       }
   { Database.                                                               }

function DbiDRDelete (                  { Deletes a Data Repository }
      pszDrName     : PAnsiChar             { Repository name }
   ): DBIResult; stdcall;

   { Deletes a Client Data Repository. The Repository Database is NOT }
   { deleted.                                                         }
   { The Repository Tables are deleted and                            }
   { The entry about this Repository is deleted from the Registry.    }

function DbiDRGetDesc (                 { Gets info about a Data Repository }
      pszDrName     : PAnsiChar;            { Repository name }
      pdrDesc       : pDRDesc           { Repository Details }
   ): DBIResult; stdcall;

   { Get information about a Data Repository known to the system. }

function DbiOpenRepositoryList (        { Gets a list of known Repositories }
var   hCur          : hDBICur           { Returned cursor }
   ): DBIResult; stdcall;

   {Each record is of type DRDesc }

function DbiDRAdd (                     { Adds a Repository to the Registry/Config }
      pdrdesc       : pDRDesc           { Repository descriptor }
   ): DBIResult; stdcall;

   { Adds the Repository descriptor to the BDE Configuration. This is a way }
   { to allow a user to know about an existing Repository                   }

function DbiDRDrop (                    { Deletes a Repository from the Registry/Config }
      pszDrName     : PAnsiChar             { Repository name }
   ): DBIResult; stdcall;

   { Drops the Repository from the BDE Configuration. This is a way  }
   { to allow a user to delete the reference to a Repository without  }
   { deleting the Repository itself                                   }

function DbiSetDefaultRepository (      { Sets the default Repository for BDE }
      pszDRName     : PAnsiChar             { Name of the Repository }
   ): DBIResult; stdcall;

function DbiGetDefaultRepository (      { Gets the default Repository for BDE }
      pszDRName     : PAnsiChar             { Name of the Repository }
   ): DBIResult; stdcall;

function DbiDROpen (                    { Open a Data Repository }
      pszDrName     : PAnsiChar;            { Repository name }
      phDataRepos   : phDBIDR           { OUT - Data Repository handle }
   ): DBIResult; stdcall;

   { This is used mainly while setting up a Repository. At run time the  }
   { BDE automatically Opens a Client Data Repository based on Registry  }
   { settings                                                            }

function DbiDRClose (                   { Close a Data Repository }
      phDataRepos   : phDBIDR           { IN/OUT - Data Repository handle }
   ): DBIResult; stdcall;


{Import/Export of Objects in a Repository to/from a Data file }
{------------------------------------------------------------ }

type
  pDROBJDesc = ^DROBJDesc;
  DROBJDesc = packed record
    szObjTypeName   : DBINAME;          { Name of the Object Type (REQUIRED for INPUT) }
    szObjRealName   : DBINAME;          { Name of the Object (REQUIRED for INPUT) }
    ulObjId         : Longint;          { Object ID }
    iVersion        : Word;             { Object Version }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;
  PDROBJDescList = ^TDROBJDescList;
  TDROBJDescList = array[0..1023] of DROBJDesc;


  pDRExportDesc = ^DRExportDesc;
  DRExportDesc = packed record
    objDesc         : DROBJDesc;
    bInclRelated    : Bool;
    aulReserved     : packed array [0..3] of Longint; { Reserved for future use }
  end;

function DbiDRImportFromFile (          { Import objects into a Repository }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      ulNumObjs     : Longint;          { Number of objects to be imported }
      padrExportDesc : pDRExportDesc;   { Used only for Names of objects to be imported }
      pszImportFile : PAnsiChar             { Full name of the Import File }
   ): DBIResult; stdcall;

   {pszImportFile is the Name of a file created using DbiDRExport.  }
   {padrExportDesc specifies an array of Objects to be Imported. For now the }
   {first Object has to be a Database Name optionally (if bInclRelated is    }
   {FALSE) followed by some Table Names.                                     }
   {This can be followed by more Database names.                             }

function DbiDRExportToFile (            { Export objects from a Repository }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      ulNumObjs     : Longint;          { Number of objects to be exported }
      padrExportDesc : pDRExportDesc;   { Objects to be exported }
      pszExportFile : PAnsiChar             { Full name of the Export File }
   ): DBIResult; stdcall;

   {pszExportFile is the Name of a file created using DbiDRExport.  }
   {pdrExportDesc specifies an array Objects to be Exported - For now the }
   {first Object has to be a Database Name optionally (if bInclRelated is }
   {FALSE) followed by some Table Names.                                  }
   {This can be followed by more Database names.                          }


{Loading/Refreshing of Repository Objects from the Database Catalog }
{------------------------------------------------------------------ }

function DbiDRLoadDBObject (            { Loads/Refreshes Database objects into the Repository }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      hDb           : hDBIDb;           { Database handle }
      pszDatabaseName : PAnsiChar;          { Name of the Database (hDb has precedence) }
      bSystemTables : Bool;             { Include SYSTEM tables (for SQL Links) }
      lNumObjs      : Longint;          { Number of objects to be loaded }
      paObjDesc     : pDROBJDesc        { Names of Tables, etc. to be loaded }
   ): DBIResult; stdcall;

   {Loads or Refreshes the specified Database objects from the actual Database }
   {Catalog. This uses BDE schema inquiry functions to load the Object         }
   {defintions into the Repository                                             }
   {For now, only Table Objects are supported i.e. Field definitions,          }
   {Referential Integrity and Index information for Tables is loaded).         }
   {If refreshing, Extended Field Attribute info is preserved for field        }
   {names that have not changed.                                               }
   {Either a hDb OR a pszDatabaseName can be supplied                          }
   {If lNumObjs = 0 or paObjDesc is NULL, all tables in the Database are       }
   {loaded.                                                                    }
   {If lNumObjs = -1 only the Database object is loaded                        }

{ Data Repository schema related API : }
{ ------------------------------------ }


{ Object Type and Relation Type Maintenance }
{ ----------------------------------------- }

{ The BDE Client Data Repository supports a data model based on Objects, }
{ Attributes and Relationships. There are two types of Objects :         }
{ - Schema Objects like Databases, Tables, Fields, Indexes, Keys, etc.   }
{ - Application Objects like Forms, Reports, Modules, Projects, Users, etc. }
{                                                                           }
{ The Data Model also has the following characteristics:                    }
{ * Object Types can be derived from other Object Types. Only single        }
{   inheritance is supported.                                               }
{ * Attributes define Objects. Only BDE logical types are supported.        }
{ * Relation types can optionally have attributes                           }
{ * Relationships can be between Object Types and can be 1:1, 1:m, m:m or m:1 }
{   All relationship types have an implicit inverse relationship type.        }
{   Special semantics for CONTAINS and REFERS relationship type are built in. }
{ * Object Types, Relation Types and Attribute Types have names. The rules    }
{   are as follows :                                                          }
{   - All Object types have unique names in the Data Repository               }
{   - All Relation types have unique names in the Data Repository             }
{   - Attribute type names are unique for a given Object/Relation type        }

{ Attributes can be of following types : (Only BDE Logical types) }

{ #define fldZSTRING      1                 { Null terminated string (Max length 64K) }
{ #define fldDATE         2                 { Date                                    }
{ #define fldBOOL         4                 { Boolean  (16 bit)                       }
{ #define fldINT16        5                 { 16 bit signed number                    }
{ #define fldINT32        6                 { 32 bit signed number                    }
{ #define fldFLOAT        7                 { 64 bit floating point                   }
{ #define fldTIMESTAMP    11                { Time-stamp  (64 bit)                    }

{ -------------- Object Types -------------- { }

type
  pDROBJType = ^DROBJType;
  DROBJType = packed record
    szObjTypeName   : DBINAME;          { Identifies the Object Type }
    iNumAttr        : Word;             { Number of attributes }
    iNumRel         : Word;             { Number of Relationships }
    ulObjFlag       : Longint;          { Extended Properties of this type (BIT VECTOR) }
    szObjTypeDesc   : DBIDRTYPEDESC;    { Documentary description }
    szObjTypeDisplayName : DBINAME;     { Identifies the Display Name for Type }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;

{Bit Vector details for ulObjFlag and ulRelFlag (in OBJType and RELType) }

const
  SCHEMATYPE         = $0001;
  CORETYPE           = $0002;
  LOGICALMODELTYPE   = $0004;
  PHYSICALMODELTYPE  = $0008;
  NONDISPLAYTYPE     = $0010;
  INTERNALTYPE       = $0020;

{.....The highest 4 bits can be user defined }

{ -------------- Attribute Types and Attribute Domains -------------- { }

{Enumerated Attribute Domains }

const
  DBIMAXENUMFLDLEN   = 95;

type
  pDBIEnumFld = ^DBIEnumFld;
  DBIEnumFld = packed record
    szDisplayStr    : DBINAME;          { Display string for the Value }
    abVal           : packed array [0..DBIMAXENUMFLDLEN] of Byte; { Data Value }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;

const
  DOMCLOSED          = $0001;

type
  pDREnumAttrDomain = ^DREnumAttrDomain;
  DREnumAttrDomain = packed record
    szEnumAttrDomainName : DBINAME;     { Identifies the Attr Domain Name }
    ulEDProp        : Longint;          { Enumerated Domain Properties - see defines above }
    iNumVals        : Word;             { Number of elements in padbiEnumFld Array }
    padbiEnumFld    : pDBIEnumFld;      { Array of Valid values }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;

{Attribute Types }

{Defines for iDisplayProp }

const
  NODISPLAY          = 0;
  SUMMARYDISPLAY     = 1;
  DETAILDISPLAY      = 2;
  INDIVIDUALDISPLAY  = 3;

{Different values for iExtendedFlag }

  iCOMMONATTR        = 1;
  iCOMMONUSERATTR    = 2;
  iEXTATTR           = 3;

{Different BIT values for iEditMask }

  EDITNEW            = $0001;
  EDITUNCHANGED      = $0002;
  EDITMODIFIED       = $0004;

{Bit Vector details for ulAttrFalg }

  SCHEMAATTR         = $0001;           { Is this part of the Database schema? }
  EXTENDEDATTR       = $0002;           { Is this an Extended attribute? }
  UIATTR             = $0004;           { Is this a UI attribute? }
  LOGICALMODELATTR   = $0008;           { Is this attribute relevant to the Logical Data Model }
  PHYSICALMODELATTR  = $0010;           { Is this attribute relevant to the Physical Data Model }
  INTERNALATTR       = $0020;           { Is this an Internal BDE attribute? }

type
  pDRATTRDesc = ^DRATTRDesc;
  DRATTRDesc = packed record
    szAttrName      : DBINAME;          { Identifies the Attribute }
    szTypeName      : DBINAME;          { Identifies the Object/Relation Type }
    szAttrDomainName : DBINAME;         { Specifies Attribute Domain for Valid values (Optional) }
    iAttrType       : Word;             { One of the above BDE logical types }
    iUnits1         : Word;             { Usually the length }
    iUnits2         : Word;             { Usually the scale }
    bReqd           : WordBool;         { TRUE if attribute is required. }
    ulAttrFlag      : Longint;          { see defines above (BIT VECTOR) }
    szAttrDisplayName : DBINAME;        { The Display Name for the Attribute }
    szAttrTypeDesc  : DBIDRTYPEDESC;    { Documentary description }
    iDisplayProp    : Word;             { Can be 0-3 based on defines above }
    iExtendedFlag   : Word;             { Internal property - see defines above }
    bReadOnly       : WordBool;         { TRUE if this is a ReadOnly attribute }
    iEditProp       : Word;             { Edit Mask property - see BIT defines above }
    ulTag           : Longint;          { To be used by applications }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;

  eRELConstraint = (
    eRelManyMany,
    eRelManyOne,
    eRelOneMany,
    eRelOneOne
  );

  eRELCategory = (
    eRelGENERAL,
    eRelCONTAINS,
    eRelREFERS,
    eRelBASEDON
  );

  pDRRELType = ^DRRELType;
  DRRELType = packed record
    szRelTypeName   : DBINAME;          { Identifies the Relation Type }
    szSrcObjTypeName : DBINAME;         { Source Object type }
    szDestObjTypeName : DBINAME;        { Destination object type }
    szRevRelTypeName : DBINAME;         { Name of reverse relation }
    iNumAttr        : Word;             { Number of attributes }
    eRelConstraint  : eRELConstraint;   { m:m, m:1, 1:m, or 1:1 }
    eRelCategory    : eRELCategory;     { CONTAINS, REFERENCE, etc. }
    szRelTypeDisplayName : DBINAME;     { Identifies the Display Name for Type }
    szRevRelDisplayName : DBINAME;        { Display Name of reverse relation }
    szRelTypeDesc   : DBIDRTYPEDESC;    { Documentary description }
    ulRelFlag       : Longint;          { Extended Properties of this type }
    aulReserved     : packed array [0..9] of Longint; { Reserved for future use }
  end;

{Bit Vector details for ulRelFlag given above (same as ulObjFlag) }

{Objects and relationship instances are stored as DRObjects in the Data }
{Repository database                                                    }

  pDRObject = ^DRObject;
  DRObject = packed record
    ulObjId         : Longint;
    iVersion        : Word;
  end;


{ ------------------------------------------------------------------------ }
{ Repository Schema : Create/Delete/Modify Object, Relation and Attr Types }
{ ------------------------------------------------------------------------ }

function DbiDRCreateObjectType (        { Create new Object Type }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pobjType      : pDROBJType;       { Describes Object Type }
      pattrDesc     : pDRATTRDesc       { Array of Attribute descriptors }
   ): DBIResult; stdcall;

   { Creates an Object Type called szObjTypeName. szObjectTypeDesc is  }
   { documentary. iAttr describes number of attributes and pattrDesc   }
   { describes the attributes themselves.                              }

function DbiDRAddAttr (                 { Add new Attribute descriptions }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszTypeName   : PAnsiChar;            { Object/Relation Type Name }
      iAttr         : Word;             { How many attributes? }
      pattrDesc     : pDRATTRDesc       { Array of Attribute descriptors }
   ): DBIResult; stdcall;

   { iAttr describes number of attributes and pattrDesc describes the  }
   { attributes themselves. Will fail if the Attributes given already exist. }
   { pszObjTypeName overrides the names in ATTRDesc.                         }


function DbiDRDropAttr (                { Drop previously added Attribute descriptions }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszTypeName   : PAnsiChar;            { Object/Relation Type Name }
      iAttr         : Word;             { How many attributes? }
      pattrDesc     : pDRATTRDesc       { Array of Attribute descriptors }
   ): DBIResult; stdcall;

   { iAttr describes number of attributes and pattrDesc describes the  }
   { attributes themselves. Will fail if the Attributes given don't exist. }



function DbiDRCreateEnumAttrDomain (    { Create new Attribute Domain (List of valid values) }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      peAttrDomain  : pDREnumAttrDomain { The Enum Attribute Domain descriptor }
   ): DBIResult; stdcall;

   { Creates a new enumerated Attribute Domain. This can be used later to  }
   { describe Attribute types.                                             }

function DbiDRModifyEnumAttrDomain (    { Modifies existing Attribute Domain (List of valid values) }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      peAttrDomain  : pDREnumAttrDomain { The Enum Attribute Domain descriptor }
   ): DBIResult; stdcall;

   { Modifies a enumerated Attribute Domain. All valid values need to  }
   { specified again                                                   }

function DbiDRDeleteEnumAttrDomain (    { Delete Attribute Domain (List of valid values) }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszEnumAttrDomain : PAnsiChar         { The name of the Enum Attr Domain }
   ): DBIResult; stdcall;

   { This will work only when there are no Attribute types that refer to  }
   { this Enum Attribute Domain                                           }

function DbiDRGetEnumAttrDomain (       { Get Attribute Domain (List of valid values) }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      iNumVals      : Word;             { Length of padbiEnumFld array }
      peAttrDomain  : pDREnumAttrDomain { The Enum Attribute Domain descriptor }
   ): DBIResult; stdcall;

   { Get Details about the Enumerated Attribute Domain. The number of  }
   { elements in the padbiEnumFld Array is passed in the iNumVals INPUT }
   { parameter. If this is greater than or equal to the actual number of  }
   { valid values, the values are returned.                               }
   { In other words if iNumVals <> peAttrDomain.iNumVals, the caller     }
   { should reallocate the padbiEnumFld Array and call this function again. }


function DbiDRCreateRelationType (      { Creates new Realtion Type }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      prelType      : pDRRELType;       { Describes Relation Type }
      pattrDesc     : pDRATTRDesc       { Array of Attribute descriptors }
   ): DBIResult; stdcall;

   { Creates a Relation Type called szRelTypeName. szRelTypeDesc is  }
   { documentary. szSrcObjTypeName and szDestObjTypeName are names of  }
   { Source and Destination Object Types.                              }
   { NOTE: Though there is a source and destination, all relations are }
   {       actually bidirectional                                      }

function DbiDRDeleteRelationType (
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszRelTypeName : PAnsiChar
   ): DBIResult; stdcall;

   {Deletes the Relationship type specified }

function DbiDRDeleteObjectType (
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszObjTypeName : PAnsiChar;
      bDeleteAssocRelTypes : Bool
   ): DBIResult; stdcall;

   {Will fail if there are any Object Types derived from this type. }
   {Will fail if there are any Relation types with this Object Type as }
   {Source or Destination unless bDeleteAssocRelTypes = TRUE           }



{ Object Type and Relationship Type Inquiry }
{ ----------------------------------------- }

function DbiDROpenObjectTypeList (
      hDataRepos    : hDBIDR;           { The Data Repository handle }
var   hObjTypeCur   : hDBICur
   ): DBIResult; stdcall;

   { Opens an BDE cursor on the virtual table of Object Types in the }
   { Repository. Use DBIGetNext .... to get all the Object types.    }

function DbiDROpenAttrTypeList (
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszTypeName   : PAnsiChar;            { Object/Relation Type Name }
var   hAttrTypeCur  : hDBICur
   ): DBIResult; stdcall;

   { Opens an BDE cursor on the virtual table of Attribute Types in the }
   { Repository. Use DBIGetNext .... to get all the Object types.       }
   { If pszObjTypeName is NOT NULL it returns only Attribute Types for this }
   { Object type                                                            }

function DbiDROpenRelTypeList (
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszObjTypeName : PAnsiChar;           { Name of the object type }
var   hRelTypeCur   : hDBICur
   ): DBIResult; stdcall;

   { Opens a BDE cursor on the virtual table of Relation Types in the }
   { Repository. Use DBIGetNext .... to get all the Relation types.   }
   { If pszObjTypeName is NOT NULL, only Relation types for this object are  }
   { returned                                                                }

function DbiDRGetObjTypeInfo (          { Get Object Type info }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszObjTypeName : PAnsiChar;           { Object Type Name }
      pobjType      : pDROBJType        { Object Type Descriptor }
   ): DBIResult; stdcall;

   { Returns a description of this Object type }

   { Returns a Schema Descriptor for this Object type. This is valid only }
   { for Object Types which are of the "Schema" type.                     }

function DbiDRGetAttrDescs (            { Get Attribute descriptions }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszTypeName   : PAnsiChar;            { Object/Relation Type Name }
      iNumAttr      : Word;             { Size of pattrDesc array }
      pattrDesc     : pDRATTRDesc       { Array of Attribute descriptors }
   ): DBIResult; stdcall;

   { Returns a description of all the Attributes in this Object/Relation type }

function DbiDRGetRelTypeInfo (          { Get Relation Type info }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszRelTypeName : PAnsiChar;           { Relation Type Name }
      prelType      : pDRRELType        { Relation Type Descriptor }
   ): DBIResult; stdcall;

   { Returns a description of this Relation type }


{-----------------------------------------------------------------------}

{ Data Repository data related API : }
{ ---------------------------------- }

{ The goal is to try and use as much of the existing BDE API as possible. }
{ Though we have a different data model for the Data Repository database, }
{ we will still try and make object sets and relationship sets look like  }
{ BDE cursors. The API to set up the Object/Relation Sets will be         }
{ different. The attributes of the Object/Relationship instance will look }
{ like fields of a record. We will also add functions to :                }
{ - Get the current DRObject handle from a cursor.                        }
{ - Position at a particular DRObject                                     }
{ If the cursor is not ordered, the same functionality can be achieved    }
{ using Bookmarks but the DRObject handle is like an ObjectID that        }
{ never changes during the life of the object (like RowID in some databases) }


function DbiDROpenObjSet (              { Opens a cursor on a set of objects }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszObjTypeName : PAnsiChar;           { Object Type Name }
      pdrSrcObjID   : pDRObject;        { Object ID of nav relation source object }
      pszNavRelName : PAnsiChar;            { Name of navigating relation }
      pszCondList   : PAnsiChar;            { A set of SQL like conditions (Optional) }
var   hCurObj       : hDBICur           { Returns Cursor handle }
   ): DBIResult; stdcall;

   { A cursor can be opened using just the (Object Type Name)  }
   { OR optionally by providing the (Object Type Name) and (a hdrSrcObjID  }
   { and pszNavRelName).                                                   }
   { Once a cursor is opened on a Object type, it can be used like a BDE   }
   { cursor. Use GetCursorProps, GetFldDescs and record access functions   }
   { to view objects and their attributes. Also use InsertRecord,          }
   { ModifyRecord and DeleteRecord to add, modify or delete objects. In    }
   { addition to this a special function is supported to get the ObjID. Use }
   { this to supply hdrSrcObjID and hdrDestObjID to various DbiDR API functions }
   {                                                                            }
   { A cursor can be opened and positioned on a specific object using this      }
   { function and specifying a pszCondList like "ALIASNAME = database/table/field" }
   { Similar functionality can be achieved by giving a pszCondList like            }
   { "ALIASNAME = database/table/*" and then calling DbiSetToObjName(). The        }
   { latter approach is preferred if you want to look at mutliple objects of       }
   { the same type (e.g. FIELDS)                                                   }


function DbiDROpenRelSet (              { Opens a cursor on a set of relation instances }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pszRelTypeName : PAnsiChar;           { Object Type Name }
      pdrSrcObjID   : pDRObject;        { Object ID of source - optional }
      pdrDestObjID  : pDRObject;        { Object ID of source - optional }
      pszCondList   : PAnsiChar;            { A set of SQL like conditions on attributes }
var   hCurObj       : hDBICur           { Returns Cursor handle }
   ): DBIResult; stdcall;

   { This function is used to open a cursor on a set of relation instances. }
   { There are 2 possible uses for this function:                           }
   { 1) It is used mainly to look at the relationship attributes - not a    }
   { common case. Most relation types will NOT have attributes and you can  }
   { navigate relationships using the (hdrSrcObjID and pszNavRelName) flavor }
   { of DbiDROpenObjSet()                                                    }
   { 2) Use this function to also create new relationships. To do this, first }
   { open a cursor with the right SrcId, DestID and szTypeName. This will     }
   { result in an empty cursor - now use DbiInitRec(), DbiPutField ()...      }
   { and DbiInsertRecord()                                                    }

function DbiDRGetObjID (                { Gets the object ID of current object/relation instance }
      hCurObj       : hDBICur;          { The Cursor handle }
      pdrObjID      : pDRObject         { Object ID of current object or relation instance }
   ): DBIResult; stdcall;

   { Gets the object ID of the current object/relation instance in a DR cursor }
   { - need to pass as input to various API functions                          }

function DbiDRSetToObjID (              { Positions cursor on given Object ID }
      hCurObj       : hDBICur;          { The Cursor handle }
      pdrObjID      : pDRObject         { Object ID of object or relation instance }
   ): DBIResult; stdcall;

   { Positions hCurObj on the object referred to by hdrObjID }

function DbiDRGetRelatedObject (        { Get Object Related to given object }
      hDataRepos    : hDBIDR;           { The Data Repository handle }
      pdrObjIDSrc   : pDRObject;        { The Source Object }
      pszRelTypeName : PAnsiChar;           { Relation Type Name (m:1) }
      pdrObjIDDest  : pDRObject         { The Destination Object }
   ): DBIResult; stdcall;

   { Gets the ObjectID of the object related to the given Object by the }
   { given RelType. Used to quickly navigate through m:1 Relations      }
   { e.g. BASEDON relation types.                                       }

function DbiDRSetToObjName (            { Positions cursor on given Object Name }
      hCurObj       : hDBICur;          { The Cursor handle }
      pszName       : PAnsiChar;            { Object Name (RealName/AliasName) }
      bUseAliasName : Bool              { Indicates if pszName is AliasName }
   ): DBIResult; stdcall;

   { Positions hCurObj on the object referred to by pszName. If bUseAliasName }
   { is TRUE AliasNames for Objects override the RealName. In other words,    }
   { the system tries to match AliasName for each object and then  tries to   }
   { match RealName if AliasName does not match                               }

{ -------------------------------------------------------------------{ }
{ -------------------------------------------------------------------{ }
{ -------------------------------------------------------------------{ }


{ ------------------------------- }
{ Common Data Repository Schema : }
{ ------------------------------- }

{ In order for the Data Repository to be useful, we need to agree on some }
{ common objects, relationships and atributes. The following is a list of }
{ Object, Relation and Attribute Type names. This part of the DR schema   }
{ models a "standard" relational database and has a couple of extended    }
{ attributes. If you need to change or add to any part of this, please    }
{ contact the BDE group                                                   }



{Object Types and their special attributes }
{----------------------------------------- }


{Common attributes for all object types }

const
  szOBJID            = 'OBJID';
  szOBJVER           = 'VERSION';
  szALIASNAME        = 'ALIASNAME';
  szREALNAME         = 'NAME';
  szOUTOFDATE        = 'OUTOFDATE';
  szCREATEDATE       = 'CREATEDATE';
  szLASTUPDATE       = 'LASTUPDATE';
  szDATABLOB         = 'DATABLOB';


{In all sections below, the first line is the name of the object type and  }
{the next few lines are the names of the attributes                        }

  szDATAREP          = 'DATAREP';
  szVERSIONNUM       = 'VERSIONNUM';

  szDB               = 'DATABASE';
  szDBPATH           = 'DBPATH';
  szDBTYPE           = 'DBTYPE';

  szTABLE            = 'TABLE';
  szPATH             = 'PATH';
  szTBLTYPE          = 'DRIVERTYPE';
  szISFILE           = 'ISFILE';
  szISVIEW           = 'ISVIEW';
  szISSYNONYM        = 'ISSYNONYM';

  szFIELD            = 'FIELD';
  szPHYTYPE          = 'PHYTYPE';
  szPHYSUBTYPE       = 'PHYSUBTYPE';
  szLOGTYPE          = 'LOGTYPE';
  szLOGSUBTYPE       = 'LOGSUBTYPE';
  szUNITS1           = 'UNITS1';
  szUNITS2           = 'UNITS2';
  szPOSITION         = 'POSITION';
  szDEFVAL           = 'DEFVAL';
  szMINVAL           = 'MINVAL';
  szMAXVAL           = 'MAXVAL';
  szREQUIRED         = 'REQUIRED';
  szFREADONLY        = 'READONLY';
  szCASTTYPE         = 'CASTTYPE';
  szCASTSUBTYPE      = 'CASTSUBTYPE';
  szDOMCONSTRAINT    = 'CHECK';

  szRECCONSTR        = 'CONSTRAINT';
  szSQLCONSTRAINT    = 'SQLCONSTRAINT';

  szSRVCONSTRAINT    = 'SRVCHECK';
  szLOCCONSTRAINT    = 'LOCCHECK';
  szSRVDEFAULT       = 'DEFAULT';
  szUSERERROR        = 'ERRORSTR';

  szEXTFIELD         = 'EXTFIELD';
  szFIELDCLASS       = 'FIELDCLASS';
  szCONTROLCLASS     = 'CONTROLCLASS';
  szALIGNMENT        = 'ALIGNMENT';
  szDISPLABEL        = 'DISPLABEL';
  szDISPWIDTH        = 'DISPWIDTH';
  szVISIBLE          = 'VISIBLE';
  szTRANSLITERATE    = 'TRANSLITERATE';
  szEDITMASK         = 'EDITMASK';
  szDISPFORMAT       = 'DISPFORMAT';
  szEDITFORMAT       = 'EDITFORMAT';
  szCURRENCY         = 'CURRENCY';
  szPRECISION        = 'PRECISION';
  szDISPLAYVALUES    = 'DISPLAYVALUES';
  szBLOBTYPE         = 'BLOBTYPE';


{Relation types and their attributes }
{----------------------------------- }


{Common additional attributes for relationship types }

  szSRCOBJID         = 'SRCOBJID';
  szSRCOBJVER        = 'SRCOBJVER';
  szDESTOBJID        = 'DESTOBJID';
  szDESTOBJVER       = 'DESTOBJVER';

  szRIPOS            = 'IPOS';

{In all sections below, the first line is the name of the relation type, }
{the second line is the name of the Inverse relation type and the next   }
{few lines are the names of the attributes                               }

  szRDRDATABASE      = 'DRDATABASES';    { Databases in a repository (1:m) }
  szIRDBDR           = 'DBDR';

  szRDREXTFIELD      = 'DREXTFIELD';     { Extended Field Attributes in a repository (1:m) }
  szIREXTFIELDDR     = 'EXTFIELDDR';

  szRDBTABLES        = 'DBTABLES';       { Tables in a database (1:m) }
  szIRTABLEDB        = 'TBLDB';

  szRTABLEFLDS       = 'TABLEFLDS';      { Fields in a table (1:m) }
  szIRFLDTABLE       = 'FLDTABLE';

  szRDBVIEWS         = 'DBVIEWS';        { Views in a database (1:m) }
  szIRVIEWDB         = 'VIEWDB';

  szRTABLECONSTR     = 'TABLECONSTRS';   { Constraints for a table (1:m) }
  szIRCONSTRTABLE    = 'CONSTRTABLE';

  szTABLETRIG        = 'TABLETRIG';      { Triggers for a table (1:m) }
  szTRIGTABLE        = 'TRIGTABLE';

  szRFLDEXTFIELD     = 'FLDEXTFIELD';    { EFA object for Field (m:1) }
  szIREXTFIELDFLDS   = 'EXTFIELDFLDS';

  szRPARENTEXTFIELD  = 'PARENTEXTFIELD'; { Parent EFA object (m:1) }
  szIRCHILDEXTFIELD  = 'CHILDEXTFIELD';

{ FROM DBIEXT.H }

{Repository Management functions }

function DbiOpenSessionRepository (     { Opens the Repository for the Session }
      hSes          : hDBISes;          { Session/NULL }
var   hDataRepos    : hDBIDR            { OUT - Data Repository handle }
   ): DBIResult; stdcall;

{ Schema Cache functions }
function DbiSchemaCacheFlush(hDb: hDBIDb; pszTableName: PAnsiChar): DBIResult; stdcall;

const
  clSQLRESTRICT = $FF070002;            { Bitmask representing invalid drivers }

{ Constraints }

type
  pDataSources = ^DataSources;
  DataSources = packed record
    iNumElem        : Word;       { Number of elements in list or indiv. elem }
    szDbName        : DBINAME;    {  individual elements are numbered 1..N }
    szTblName       : DBITBLNAME;
    szSourceFldName : DBINAME;
    szOrigFldName   : DBINAME;
    szSQLExprImport : DBISQLSTR;
    szSQLExprCustom : DBISQLSTR;
    szErrStrImport  : DBIMSG;
    szErrStrCustom  : DBIMSG;
    bRequired       : BOOL;
  end;

  DsInfoReq = (
    dsNumSources,       { Callee fills in iNumElem for number of data sources }
    dsDefSources,       { Callee fills in  db,tbl to use as defaults }
    dsSource,           { Callee fills in iNumElem, caller fills in db/tbl }
    dsFieldSource,      { Caller fills in szSourceFldName, callee fills in }
                        {  db/tbl/orig fld name }
    dsFieldDefault,     { Caller fills in szSourceFldName, callee fills in }
                        {  szSQLExpr and szErrStr }
    dsFieldDomainExpr,  { Caller fills in szSourceFldName, callee fills in }
                        {  szSQLExpr and szErrStr }
    dsTblConstraint,    { Callee fills in iNumElem for number of rec constr }
    dsNumTblConstraint  { Caller fills in iNumElem, callee fills in }
                        {  szSQLExpr and szErrStr }
  );

type
  pfDataSourcesCallback = function(
    lUserVal    : Integer;       { User-specific pass-back value }
    Req         : DsInfoReq;     { Request type }
    pDsSources  : pDataSources   { Information returned, caller allocs/frees }
): DBIResult; stdcall;

function DbiBeginConstraintLayer(
     hDb          : hDBIDb;
var  hCur         : hDBICur;
     pDsCb        : pfDataSourcesCallback;
     lUserVal     : Integer
  ): DBIResult; stdcall;

function DbiEndConstraintLayer(
var  hCur         : hDBICur
  ): DBIResult; stdcall;

type
  EXPType = (
     expDomain,
     expRecConstr,
     expDefault
  );

function DbiSQLTextToCanEx(
     hStmt         :  hDBIStmt;
     pExprText     :  PAnsiChar;
     iFields       :  Word;
     pFlds         :  pFLDDesc;
     exType        :  EXPType;
     pszFldName    :  PAnsiChar;
     pszSourceDb   :  PAnsiChar;
var  piCanSize     :  Word;
     pCanEx        :  PByte
   ): DBIResult; stdcall;


function DbiCheckSQLExpression(
     hCur          :  hDBICur;
     pszExpr       :  PAnsiChar;
     pszFldName    :  PAnsiChar;
     expType       :  EXPType;
     pDsCB         :  pfDataSourcesCallback;
     iClientData   :  Integer
   ): DBIResult; stdcall;

function DsProviderGetDataPacket(
    hCur             : hDBICur;         { Result cursor handle }
    eProvOptions     : Integer;         { Options for provider }
    pDsCB            : Pointer;         { Callback for field origin }
    iClientData      : Integer;         { Client Data for callback }
    piMaxRecords     : PInteger;        { In: Max recs, Out: Actual Recs }
var Packet           : PVarArray;
var pbEndOfData      : Bool             { End of file was reached }
): DBIResult; stdcall;

const
  MaxDS = 4;

type
  PNameList = ^NameList;
  NameList = array[1..MaxDS] of PAnsiChar;
  PPacketList = ^PacketList;
  PacketList = array[1..MaxDS] of PVarArray;
  PCBList = ^CBList;
  CBList = array[1..MaxDS] of Pointer;
  PIntList = ^IntList;
  IntList = array[1..MaxDS] of Integer;

function DsResolver(
    iNoOfDataSources  : Integer;               { Number of data sources. }
    Packets           : PPacketList;           { Packet Buffer }
    hDb               : hDBIDb;                { Database handle. }
    szDbName          : PAnsiChar;                 { Database name }
    szDbType          : PAnsiChar;                 { Database type }
    szDbPassword      : PAnsiChar;                 { Password }
    pszSqlStr         : PNameList;             { SQL str used to generate Packet. }
    ppTableName       : PNameList;             { If open table, name of table }
    piClientData      : PInteger;              { Client Data for Resolver callback }
    ppfReconcile      : PCBList;               { Reconcile Callback-fn }
    ppDsCB            : PCBList;               { PpfDataSourcesCallback; }
    ClientData        : Integer;               { Client Data for DSCallback }
    piMaxErrorRecords : PInteger;              { In: MaxErrors, Out: ErrorCount } 
    pppErrPacket      : PPacketList            { Packet Buffer for error set }
): DBIResult; stdcall;

const
  { Flag for MTS Context object, to indicate that the database should be open under  }
  { this context.                                                                    }
  szMTXCONTEXTOBJ           = 'MTS CONTEXT OBJECT';
  { Flag to pass isolation level in eOpenMode of DbiOpenDatabase.  Used for mts      }
  { transctions                                                                      }
  OPENMODEFLAG_DIRTYREAD       = $0000;
  OPENMODEFLAG_READCOMMITTED   = $0100;
  OPENMODEFLAG_REPEATABLEREAD  = $0200;

{$ENDIF}

implementation

function ErrCat(rslt: Word): Word;
begin
  ErrCat := rslt shr 8;
end;

function ErrCode(rslt: Word): Word;
begin
  ErrCode := rslt and $00FF;
end;

function DbiInit(pEnv: PDBIEnv): DBIResult;
begin
  DbiInit := DbiInitFn(DbiINTFVER, pEnv);
end;

procedure __LOADFUNC; external;
procedure __APIINFOTABLE; external;
procedure __APIINFOTABLEEND; external;
procedure INFO_IDAPI32; external;

{$L IDPAS32.OBJ}
{$L DBI.OBJ}
{$L INTFPAS.OBJ}

function DBIEXIT;                       external;
function DBIDLLEXIT;                    external;
function DBIINITFN;                     external;
function DBIGETSYSVERSION;              external;
function DBIGETSYSCONFIG;               external;
function DBIGETCLIENTINFO;              external;
function DBIGETSYSINFO;                 external;
function DBILOADDRIVER;                 external;
function DBISTARTSESSION;               external;
function DBIGETCURRSESSION;             external;
function DBISETCURRSESSION;             external;
function DBICLOSESESSION;               external;
function DBIGETSESINFO;                 external;
function DBISETPRIVATEDIR;              external;
function DBIOPENDATABASE;               external;
function DBISETDIRECTORY;               external;
function DBIGETDIRECTORY;               external;
function DBIOPENTABLELIST;              external;
function DBIOPENFILELIST;               external;
function DBIOPENINDEXLIST;              external;
function DBIOPENFIELDLIST;              external;
function DBIOPENVCHKLIST;               external;
function DBIOPENRINTLIST;               external;
function DBIOPENSECURITYLIST;           external;
function DBIOPENFAMILYLIST;             external;
function DBIOPENSPLIST;                 external;
function DBIOPENSPPARAMLIST;            external;
function DBIOPENFUNCTIONLIST;           external;
function DBIOPENFUNCTIONARGLIST;        external;
function DBICLOSEDATABASE;              external;
function DBIOPENDRIVERLIST;             external;
function DBIGETDRIVERDESC;              external;
function DBIOPENDATABASELIST;           external;
function DBIGETDATABASEDESC;            external;
function DBIOPENTABLETYPESLIST;         external;
function DBIGETTABLETYPEDESC;           external;
function DBIOPENFIELDTYPESLIST;         external;
function DBIGETFIELDTYPEDESC;           external;
function DBIOPENINDEXTYPESLIST;         external;
function DBIGETINDEXTYPEDESC;           external;
function DBIOPENLDLIST;                 external;
function DBIOPENTABLE;                  external;
function DBIOPENNESTEDTABLE;            external;
function DBIOPENREF;                    external;
function DBIDATABASEFLUSH;              external;
function DBIGETCURSORPROPS;             external;
function DBIGETOBJFROMNAME;             external;
function DBIGETOBJFROMOBJ;              external;
function DBIGETPROP;                    external;
function DBISETPROP;                    external;
function DBIVALIDATEPROP;               external;
function DBIGETFIELDDESCS;              external;
function DBIGETCURSORFORTABLE;          external;
function DBICLONECURSOR;                external;
function DBICLOSECURSOR;                external;
function DBIOPENINDEX;                  external;
function DBICLOSEINDEX;                 external;
function DBISWITCHTOINDEX;              external;
function DBIGETINDEXDESC;               external;
function DBIGETINDEXDESCS;              external;
function DBIGETINDEXFORFIELD;           external;
function DBIGETINDEXSEQNO;              external;
function DBIEXTRACTKEY;                 external;
function DBISETRANGE;                   external;
function DBIRESETRANGE;                 external;
function DBICOMPAREKEYS;                external;
function DBIGETRECORDFORKEY;            external;
function DBIGETVCHKDESC;                external;
function DBIGETRINTDESC;                external;
function DBISETTOBEGIN;                 external;
function DBISETTOEND;                   external;
function DBISETTOCURSOR;                external;
function DBIGETBOOKMARK;                external;
function DBICOMPAREBOOKMARKS;           external;
function DBIGETNEXTRECORD;              external;
function DBIGETPRIORRECORD;             external;
function DBIGETRECORD;                  external;
function DBIGETRELATIVERECORD;          external;
function DBIINITRECORD;                 external;
function DBIINSERTRECORD;               external;
function DBIMODIFYRECORD;               external;
function DBIDELETERECORD;               external;
function DBIREADBLOCK;                  external;
function DBIWRITEBLOCK;                 external;
function DBIAPPENDRECORD;               external;
function DBIUNDELETERECORD;             external;
function DBIGETSEQNO;                   external;
function DBISETTOSEQNO;                 external;
function DBIGETRECORDCOUNT;             external;
function DBIGETEXACTRECORDCOUNT;        external;
function DBISETTORECORDNO;              external;
function DBISAVECHANGES;                external;
function DBIFORCEREREAD;                external;
function DBIFORCERECORDREREAD;          external;
function DBICHECKREFRESH;               external;
function DBIMAKEPERMANENT;              external;
function DBIGETFIELD;                   external;
function DBIPUTFIELD;                   external;
function DBIVERIFYFIELD;                external;
function DBIOPENBLOB;                   external;
function DBIGETBLOBSIZE;                external;
function DBIGETBLOB;                    external;
function DBIPUTBLOB;                    external;
function DBITRUNCATEBLOB;               external;
function DBIFREEBLOB;                   external;
function DBIGETBLOBHEADING;             external;
function DBISETFIELDMAP;                external;
function DBIBEGINTRAN;                  external;
function DBIENDTRAN;                    external;
function DBIGETTRANINFO;                external;
function DBIACQTABLELOCK;               external;
function DBIACQPERSISTTABLELOCK;        external;
function DBIRELPERSISTTABLELOCK;        external;
function DBIRELTABLELOCK;               external;
function DBIRELRECORDLOCK;              external;
function DBIISRECORDLOCKED;             external;
function DBIISTABLELOCKED;              external;
function DBIISTABLESHARED;              external;
function DBIOPENLOCKLIST;               external;
function DBIOPENUSERLIST;               external;
function DBISETLOCKRETRY;               external;
function DBIBATCHMOVE;                  external;
function DBICOPYTABLE;                  external;
function DBIEMPTYTABLE;                 external;
function DBIPACKTABLE;                  external;
function DBIREGENINDEX;                 external;
function DBIREGENINDEXES;               external;
function DBISORTTABLE;                  external;
function DBICREATETABLE;                external;
function DBICREATEINMEMTABLE;           external;
function DBICREATETEMPTABLE;            external;
function DBIDORESTRUCTURE;              external;
function DBIRENAMETABLE;                external;
function DBIDELETETABLE;                external;
function DBIADDINDEX;                   external;
function DBIDELETEINDEX;                external;
function DBIGETERRORENTRY;              external;
function DBIGETERRORINFO;               external;
function DBIGETERRORSTRING;             external;
function DBIGETERRORCONTEXT;            external;
function DBIDATEENCODE;                 external;
function DBIDATEDECODE;                 external;
function DBITIMEENCODE;                 external;
function DBITIMEDECODE;                 external;
function DBITIMESTAMPENCODE;            external;
function DBITIMESTAMPDECODE;            external;
function DBIBCDFROMFLOAT;               external;
function DBIBCDTOFLOAT;                 external;
function DBIREGISTERCALLBACK;           external;
function DBIGETCALLBACK;                external;
function DBIGETDATEFORMAT;              external;
function DBISETDATEFORMAT;              external;
function DBIGETTIMEFORMAT;              external;
function DBISETTIMEFORMAT;              external;
function DBIGETNUMBERFORMAT;            external;
function DBISETNUMBERFORMAT;            external;
function DBINATIVETOANSI;               external;
function DBIANSITONATIVE;               external;
function DBIADDFILTER;                  external;
function DBIDROPFILTER;                 external;
function DBIACTIVATEFILTER;             external;
function DBIDEACTIVATEFILTER;           external;
function DBIGETFILTERINFO;              external;
function DBIBEGINLINKMODE;              external;
function DBIENDLINKMODE;                external;
function DBILINKDETAIL;                 external;
function DBILINKDETAILTOEXP;            external;
function DBIUNLINKDETAIL;               external;
function DBIGETLINKSTATUS;              external;
function DBITRANSLATERECORDSTRUCTURE;   external;
function DBIOPENFIELDXLT;               external;
function DBITRANSLATEFIELD;             external;
function DBICLOSEFIELDXLT;              external;
function DBIGETTABLEOPENCOUNT;          external;
function DBIUSEIDLETIME;                external;
function DBIGETLDOBJ;                   external;
function DBIGETLDNAME;                  external;
function DBIFORMFULLNAME;               external;
function DBIADDPASSWORD;                external;
function DBIDROPPASSWORD;               external;
function DBIGETNETUSERNAME;             external;
function DBIDEBUGLAYEROPTIONS;          external;
function DBIOPENCFGINFOLIST;            external;
function DBIQEXECDIRECT;                external;
function DBIQALLOC;                     external;
function DBIQPREPARE;                   external;
function DBIQEXEC;                      external;
function DBIQFREE;                      external;
function DBIQSETPARAMS;                 external;
function DBIGETLDNAMEFROMDB;            external;
function DBIOPENCONFIGFILE;             external;
function DBICLOSECONFIGFILE;            external;
function DBIQPREPAREEXT;                external;
function DBIQINSTANTIATEANSWER;         external;
function DBIQEXECPROCDIRECT;            external;
function DBIQPREPAREPROC;               external;
function DBIQSETPROCPARAMS;             external;
function DBIQGETBASEDESCS;              external;
function DBISETTOBOOKMARK;              external;
function DBISETTOKEY;                   external;
function DBIADDALIAS;                   external;
function DBIDELETEALIAS;                external;
function DBIBEGINDELAYEDUPDATES;        external;
function DBIENDDELAYEDUPDATES;          external;
function DBIAPPLYDELAYEDUPDATES;        external;
function DBIIMPORTODBC;                 external;
function DBIADDDRIVER;                  external;
function DBIDELETEDRIVER;               external;
function DbiGetSQLRequest;              external;
function DbiFreeSQLRequest;             external;

{$IFNDEF UNDOCUMENTED}
function DBICFGSAVE;                    external;
function DBICFGBUILDPATH;               external;
function DBICFGPOSITION;                external;
function DBICFGGETNEXTNODE;             external;
function DBICFGGETRECORD;               external;
function DBICFGADDRECORD;               external;
function DBICFGMODIFYRECORD;            external;
function DBICFGDROPRECORD;              external;
function DBICFGTRANSLATE;               external;
function DBICFGGETHELP;                 external;
function DBICFGMERGE;                   external;

function OSLDBUILDCHARSETXFORM;         external;
function OSLDLOADBYDRIVERID;            external;
function OSLDGETDRIVERID;               external;
function OSLDINIT;                      external;
function OSLDGETDEFAULTOBJ;             external;
function OSLDEXIT;                      external;
function OSLDLOADBYSORTSIG;             external;
function OSLDLOADBYFNAME;               external;
function OSLDLOADBYSYMBNAME;            external;
function OSLDSTRNCMP;                   external;
function OSLDSTRCMP;                    external;
function OSLDSTRTOLOWER;                external;
function OSLDSTRNTOLOWER;               external;
function OSLDSTRCMPI;                   external;
function OSLDSTRNCMPI;                  external;
function OSLDSTRTOUPPER;                external;
function OSLDSTRNTOUPPER;               external;
function OSLDGETSYMBNAME;               external;
function OSLDGETSORTSIG;                external;
function OSLDGETCODEPAGE;               external;
function OSLDGETFNAME;                  external;
function OSLDANSITOOEM;                 external;
function OSLDOEMTOANSI;                 external;
function OSLDISANSICHARINOEMCP;         external;
function OSLDISOEMCHARINANSICP;         external;
function OSLDANSICHARTOOEM;             external;
function OSLDOEMCHARTOANSI;             external;
function OSLDUNLOADOBJ;                 external;
function OSLDISALPHA;                   external;
function OSLDISDIGIT;                   external;
function OSLDCHARTOUPPER;               external;
function OSLDCHARTOLOWER;               external;
function OSLDGETDESCNAME;               external;
function OSLDSEARCHINIT;                external;
function OSLDSEARCHEND;                 external;
function OSLDSEARCHNEXT;                external;
function OSLDGETPRODUCTID;              external;
function OSLDLOADBYLCID;                external;
function OSLDGETMINMAX;                 external;
function OSLDSETCONVCHARS;              external;
function OSLDGETFUNCADDRLDSTRNCOLLI_;   external;
function OSLDGETFUNCADDRLDSTRNCOLL_;    external;
function OSLDGETFUNCADDRLDSTRNCMPI_;    external;
function OSLDSTRNCOLL;                  external;
function OSLDSTRNCOLLI;                 external;
function OSLDEXSTRCMP;                  external;

function DbiDRCreate;                   external;
function DbiDRDelete;                   external;
function DbiDRGetDesc;                  external;
function DbiOpenRepositoryList;         external;
function DbiDRAdd;                      external;
function DbiDRDrop;                     external;
function DbiSetDefaultRepository;       external;
function DbiGetDefaultRepository;       external;
function DbiDROpen;                     external;
function DbiDRClose;                    external;
function DbiDRImportFromFile;           external;
function DbiDRExportToFile;             external;
function DbiDRLoadDBObject;             external;
function DbiDRCreateObjectType;         external;
function DbiDRAddAttr;                  external;
function DbiDRDropAttr;                 external;
function DbiDRCreateEnumAttrDomain;     external;
function DbiDRModifyEnumAttrDomain;     external;
function DbiDRDeleteEnumAttrDomain;     external;
function DbiDRGetEnumAttrDomain;        external;
function DbiDRCreateRelationType;       external;
function DbiDRDeleteRelationType;       external;
function DbiDRDeleteObjectType;         external;
function DbiDROpenObjectTypeList;       external;
function DbiDROpenAttrTypeList;         external;
function DbiDROpenRelTypeList;          external;
function DbiDRGetObjTypeInfo;           external;
function DbiDRGetAttrDescs;             external;
function DbiDRGetRelTypeInfo;           external;
function DbiDROpenObjSet;               external;
function DbiDROpenRelSet;               external;
function DbiDRGetObjID;                 external;
function DbiDRSetToObjID;               external;
function DbiDRGetRelatedObject;         external;
function DbiDRSetToObjName;             external;
function DbiOpenSessionRepository;      external;

function DbiSchemaCacheFlush;           external;
function DbiBeginConstraintLayer;       external;
function DbiEndConstraintLayer;         external;
function DbiSQLTextToCanEx;             external;
function DbiCheckSQLExpression;         external;
function DsProviderGetDataPacket;       external;
function DsResolver;                    external;

{$ENDIF}
end.
