{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2022 Embarcadero Technologies, Inc.      }
{        All rights reserved                            }
{                                                       }
{*******************************************************}

const
  EncodingHashCount = 252;

  EncodingHashList: array[0..EncodingHashCount - 1] of Cardinal = (
    $85005FB3,  // "us-ascii"
    $C5002920,  // "ascii"
    $05002A24,  // "csascii"
    $ED73B2E0,  // "utf-7"
    $F0691DE8,  // "csunicode11utf7"
    $ED73B300,  // "utf-8"
    $076B9F00,  // "utf8"
    $72E63C46,  // "iso-8859-1"
    $C11CC320,  // "cp819"
    $82CCC33A,  // "ibm819"
    $A7624872,  // "iso-ir-100"
    $73416256,  // "iso8859-1"
    $72E63FD6,  // "iso_8859-1"
    $DBE9AA9F,  // "iso_8859-1:1987"
    $F5346E43,  // "csisolatin1"
    $72E63C26,  // "iso-8859-2"
    $A7624852,  // "iso-ir-101"
    $73416236,  // "iso8859-2"
    $72E63FB6,  // "iso_8859-2"
    $1BE9AA9F,  // "iso_8859-2:1987"
    $F5346E23,  // "csisolatin2"
    $72E63C06,  // "iso-8859-3"
    $A7624952,  // "iso-ir-109"
    $73416216,  // "iso8859-3"
    $72E63F96,  // "iso_8859-3"
    $5BE9AB7F,  // "iso_8859-3:1988"
    $F5346E03,  // "csisolatin3"
    $72E63CE6,  // "iso-8859-4"
    $A7624C72,  // "iso-ir-110"
    $734162F6,  // "iso8859-4"
    $72E63F76,  // "iso_8859-4"
    $9BE9AB7E,  // "iso_8859-4:1988"
    $F5346EE3,  // "csisolatin4"
    $72E63CC6,  // "iso-8859-5"
    $54F7C8B4,  // "cyrillic"
    $A76258F2,  // "iso-ir-144"
    $734162D6,  // "iso8859-5"
    $72E63F56,  // "iso_8859-5"
    $DBE9AB7E,  // "iso_8859-5:1988"
    $FD548BAB,  // "csisolatincyrillic"
    $72E63CA6,  // "iso-8859-6"
    $A220A878,  // "arabic"
    $9CCB2583,  // "asmo-708"
    $80C82483,  // "ecma-114"
    $A7624092,  // "iso-ir-127"
    $734162B6,  // "iso8859-6"
    $72E63F36,  // "iso_8859-6"
    $1BE9AA9E,  // "iso_8859-6:1987"
    $65CAC0A8,  // "csisolatinarabic"
    $72E63C86,  // "iso-8859-7"
    $C9131960,  // "greek"
    $22632B19,  // "greek8"
    $73416296,  // "iso8859-7"
    $72E63F16,  // "iso_8859-7"
    $5BE9AA9E,  // "iso_8859-7:1987"
    $5BDD0A1E,  // "iso_8859-7:2003"
    $72E63D66,  // "iso-8859-8"
    $CC189AFA,  // "hebrew"
    $A7624572,  // "iso-ir-138"
    $73416376,  // "iso8859-8"
    $72E63EF6,  // "iso_8859-8"
    $9BE9AB7D,  // "iso_8859-8:1988"
    $0BF2F22A,  // "csisolatinhebrew"
    $72E63D46,  // "iso-8859-9"
    $A7625972,  // "iso-ir-148"
    $73416356,  // "iso8859-9"
    $72E63ED6,  // "iso_8859-9"
    $DBE9AB5D,  // "iso_8859-9:1989"
    $F5346EC3,  // "csisolatin5"
    $5CC78ECE,  // "iso-8859-10"
    $A7625C92,  // "iso-ir-157"
    $682C4CCE,  // "iso8859-10"
    $5CC7FCCE,  // "iso_8859-10"
    $EBB5EDB9,  // "iso_8859-10:1992"
    $F5346EA3,  // "csisolatin6"
    $5CC78EEE,  // "iso-8859-11"
    $682C4CEE,  // "iso8859-11"
    $5CC7FCEE,  // "iso_8859-11"
    $5CC78EAE,  // "iso-8859-13"
    $A7625552,  // "iso-ir-179"
    $682C4CAE,  // "iso8859-13"
    $5CC7FCAE,  // "iso_8859-13"
    $5CC78E4E,  // "iso-8859-14"
    $0D51A610,  // "iso-celtic"
    $A7626D52,  // "iso-ir-199"
    $682C4C4E,  // "iso8859-14"
    $5CC7FC4E,  // "iso_8859-14"
    $EBB5ECF8,  // "iso_8859-14:1998"
    $5CC78E6E,  // "iso-8859-15"
    $A763C812,  // "iso-ir-203"
    $682C4C6E,  // "iso8859-15"
    $5CC7FC6E,  // "iso_8859-15"
    $ABB5ECF8,  // "iso_8859-15:1998"
    $5CC78E0E,  // "iso-8859-16"
    $A763C0B2,  // "iso-ir-226"
    $682C4C0E,  // "iso8859-16"
    $5CC7FC0E,  // "iso_8859-16"
    $6B8149D8,  // "iso_8859-16:2001"
    $6DCAA131,  // "shift_jis"
    $C11C4A40,  // "cp932"
    $6AEAA131,  // "shift-jis"
    $0704AA60,  // "sjis"
    $8AF6BA3A,  // "csshiftjis"
    $00320560,  // "gbk"
    $C11C4AC0,  // "cp936"
    $1EE5EDA5,  // "windows-936"
    $07394259,  // "gb2312"
    $073922C1,  // "csgb2312"
    $D9F6EB4F,  // "ks_c_5601-1987"
    $C11C5720,  // "cp949"
    $A7625952,  // "iso-ir-149"
    $191309DA,  // "korean"
    $7D4BAEA0,  // "ksc_5601"
    $D9F6EA8F,  // "ks_c_5601-1989"
    $654C329D,  // "csksc56011987"
    $06151AA0,  // "big5"
    $C11C5200,  // "cp950"
    $C2A332A0,  // "big-5"
    $9C5535F1,  // "big-five"
    $885557AA,  // "bigfive"
    $DC1519A3,  // "cn-big5"
    $20151AB8,  // "csbig5"
    $EC093240,  // "ucs-2"
    $540A0C13,  // "csunicode"
    $AE7642DD,  // "utf-16"
    $D90ACA19,  // "utf-16le"
    $D90AF219,  // "utf-16be"
    $DC3D3EA3,  // "windows-1250"
    $2309D218,  // "cp1250"
    $DC3D3E83,  // "windows-1251"
    $2309D238,  // "cp1251"
    $DC3D3EE3,  // "windows-1252"
    $2309D258,  // "cp1252"
    $DC3D3EC3,  // "windows-1253"
    $2309D278,  // "cp1253"
    $DC3D3E23,  // "windows-1254"
    $2309D298,  // "cp1254"
    $DC3D3E03,  // "windows-1255"
    $2309D2B8,  // "cp1255"
    $DC3D3E63,  // "windows-1256"
    $2309D2D8,  // "cp1256"
    $DC3D3E43,  // "windows-1257"
    $2309D2F8,  // "cp1257"
    $DC3D3FA3,  // "windows-1258"
    $2309D318,  // "cp1258"
    $188CBA5A,  // "koi8-r"
    $A0A6789C,  // "gb_2312-80"
    $D4D343A2,  // "chinese"
    $953B0373,  // "iso-ir-58"
    $E2598609,  // "csiso58gb231280"
    $188CBABA,  // "koi8-u"
    $5276EA9C,  // "iso-2022-jp"
    $D379F674,  // "csiso2022jp"
    $5276EEDC,  // "iso-2022-kr"
    $D379F234,  // "csiso2022kr"
    $5276CD5C,  // "iso-2022-cn"
    $AC272619,  // "euc-jp"
    $CD602600,  // "eucjp"
    $F6BFAD54,  // "cseucpkdfmtjapanese"
    $AC2701D9,  // "euc-cn"
    $CD6001C0,  // "euccn"
    $AC272259,  // "euc-kr"
    $CD602240,  // "euckr"
    $AC275EF9,  // "euc-tw"
    $CD605EE0,  // "euctw"
    $0D605DE4,  // "cseuctw"
    $D8962C60,  // "hz-gb-2312"
    $E198C920,  // "gb18030"
    $82CACAFA,  // "ibm437"
    $001ACAE0,  // "437"
    $C11ACAE0,  // "cp437"
    $381AA8BD,  // "cspc8codepage437"
    $C11B4AE0,  // "cp737"
    $82CB5ABA,  // "ibm775"
    $C11B5AA0,  // "cp775"
    $4D717E11,  // "cspc775baltic"
    $82CCD21A,  // "ibm850"
    $001CD200,  // "850"
    $C11CD200,  // "cp850"
    $33172EEC,  // "cspc850multilingual"
    $82CCD25A,  // "ibm852"
    $001CD240,  // "852"
    $C11CD240,  // "cp852"
    $C11CB2C4,  // "cspcp852"
    $C11CD260,  // "cp853"
    $82CCD2BA,  // "ibm855"
    $001CD2A0,  // "855"
    $C11CD2A0,  // "cp855"
    $82CCB222,  // "csibm855"
    $82CCD2FA,  // "ibm857"
    $001CD2E0,  // "857"
    $C11CD2E0,  // "cp857"
    $82CCB262,  // "csibm857"
    $C11CD300,  // "cp858"
    $82CCDE1A,  // "ibm860"
    $001CDE00,  // "860"
    $C11CDE00,  // "cp860"
    $82CCBE82,  // "csibm860"
    $82CCDE3A,  // "ibm861"
    $001CDE20,  // "861"
    $C1172A60,  // "cp-is"
    $C11CDE20,  // "cp861"
    $82CCBEA2,  // "csibm861"
    $82CCDE5A,  // "ibm862"
    $001CDE40,  // "862"
    $C11CDE40,  // "cp862"
    $598166E4,  // "cspc862latinhebrew"
    $82CCDE7A,  // "ibm863"
    $001CDE60,  // "863"
    $C11CDE60,  // "cp863"
    $82CCBEE2,  // "csibm863"
    $82CCDE9A,  // "ibm864"
    $C11CDE80,  // "cp864"
    $82CCBE02,  // "csibm864"
    $82CCDEBA,  // "ibm865"
    $001CDEA0,  // "865"
    $C11CDEA0,  // "cp865"
    $82CCBE22,  // "csibm865"
    $82CCDEDA,  // "ibm866"
    $001CDEC0,  // "866"
    $C11CDEC0,  // "cp866"
    $82CCBE42,  // "csibm866"
    $82CCDF3A,  // "ibm869"
    $001CDF20,  // "869"
    $C1171240,  // "cp-gr"
    $C11CDF20,  // "cp869"
    $82CCBFA2,  // "csibm869"
    $1EE57DE5,  // "windows-874"
    $C11CDA80,  // "cp874"
    $A76250B2,  // "iso-ir-166"
    $A4CBCDBA,  // "tis-620"
    $D52BCE1D,  // "tis620"
    $AF38C554,  // "tis620-0"
    $406243B2,  // "tis620.2529-1"
    $40774392,  // "tis620.2533-0"
    $407743B2,  // "tis620.2533-1"
    $23095E38,  // "cp1361"
    $D2C58840,  // "johab"
    $00370860,  // "mac"
    $9B7B8302,  // "macintosh"
    $22C767D0,  // "macroman"
    $A22D6A60,  // "macarabic"
    $CC1558E2,  // "machebrew"
    $09137770,  // "macgreek"
    $63FFA8B4,  // "maccyrillic"
    $1D9EE8AB,  // "macromania"
    $239D7412,  // "macukraine"
    $81758A50,  // "macthai"
    $CBA67ED9,  // "maccentraleurope"
    $0D4974D0,  // "maciceland"
    $A33D01BD,  // "macturkish"
    $327D094B); // "maccroatian"

  // Codepage in the high word, offset into name list in the low word
  EncodingDataList: array[0..EncodingHashCount - 1] of Cardinal = (
    $4E9F0000,  // "us-ascii"
    $4E9F0009,  // "ascii"
    $4E9F000F,  // "csascii"
    $FDE80017,  // "utf-7"
    $FDE8001D,  // "csunicode11utf7"
    $FDE9002D,  // "utf-8"
    $FDE90033,  // "utf8"
    $6FAF0038,  // "iso-8859-1"
    $6FAF0043,  // "cp819"
    $6FAF0049,  // "ibm819"
    $6FAF0050,  // "iso-ir-100"
    $6FAF005B,  // "iso8859-1"
    $6FAF0065,  // "iso_8859-1"
    $6FAF0070,  // "iso_8859-1:1987"
    $6FAF0080,  // "csisolatin1"
    $6FB0008C,  // "iso-8859-2"
    $6FB00097,  // "iso-ir-101"
    $6FB000A2,  // "iso8859-2"
    $6FB000AC,  // "iso_8859-2"
    $6FB000B7,  // "iso_8859-2:1987"
    $6FB000C7,  // "csisolatin2"
    $6FB100D3,  // "iso-8859-3"
    $6FB100DE,  // "iso-ir-109"
    $6FB100E9,  // "iso8859-3"
    $6FB100F3,  // "iso_8859-3"
    $6FB100FE,  // "iso_8859-3:1988"
    $6FB1010E,  // "csisolatin3"
    $6FB2011A,  // "iso-8859-4"
    $6FB20125,  // "iso-ir-110"
    $6FB20130,  // "iso8859-4"
    $6FB2013A,  // "iso_8859-4"
    $6FB20145,  // "iso_8859-4:1988"
    $6FB20155,  // "csisolatin4"
    $6FB30161,  // "iso-8859-5"
    $6FB3016C,  // "cyrillic"
    $6FB30175,  // "iso-ir-144"
    $6FB30180,  // "iso8859-5"
    $6FB3018A,  // "iso_8859-5"
    $6FB30195,  // "iso_8859-5:1988"
    $6FB301A5,  // "csisolatincyrillic"
    $6FB401B8,  // "iso-8859-6"
    $6FB401C3,  // "arabic"
    $6FB401CA,  // "asmo-708"
    $6FB401D3,  // "ecma-114"
    $6FB401DC,  // "iso-ir-127"
    $6FB401E7,  // "iso8859-6"
    $6FB401F1,  // "iso_8859-6"
    $6FB401FC,  // "iso_8859-6:1987"
    $6FB4020C,  // "csisolatinarabic"
    $6FB5021D,  // "iso-8859-7"
    $6FB50228,  // "greek"
    $6FB5022E,  // "greek8"
    $6FB50235,  // "iso8859-7"
    $6FB5023F,  // "iso_8859-7"
    $6FB5024A,  // "iso_8859-7:1987"
    $6FB5025A,  // "iso_8859-7:2003"
    $6FB6026A,  // "iso-8859-8"
    $6FB60275,  // "hebrew"
    $6FB6027C,  // "iso-ir-138"
    $6FB60287,  // "iso8859-8"
    $6FB60291,  // "iso_8859-8"
    $6FB6029C,  // "iso_8859-8:1988"
    $6FB602AC,  // "csisolatinhebrew"
    $6FB702BD,  // "iso-8859-9"
    $6FB702C8,  // "iso-ir-148"
    $6FB702D3,  // "iso8859-9"
    $6FB702DD,  // "iso_8859-9"
    $6FB702E8,  // "iso_8859-9:1989"
    $6FB702F8,  // "csisolatin5"
    $6FB80304,  // "iso-8859-10"
    $6FB80310,  // "iso-ir-157"
    $6FB8031B,  // "iso8859-10"
    $6FB80326,  // "iso_8859-10"
    $6FB80332,  // "iso_8859-10:1992"
    $6FB80343,  // "csisolatin6"
    $6FB9034F,  // "iso-8859-11"
    $6FB9035B,  // "iso8859-11"
    $6FB90366,  // "iso_8859-11"
    $6FBB0372,  // "iso-8859-13"
    $6FBB037E,  // "iso-ir-179"
    $6FBB0389,  // "iso8859-13"
    $6FBB0394,  // "iso_8859-13"
    $6FBC03A0,  // "iso-8859-14"
    $6FBC03AC,  // "iso-celtic"
    $6FBC03B7,  // "iso-ir-199"
    $6FBC03C2,  // "iso8859-14"
    $6FBC03CD,  // "iso_8859-14"
    $6FBC03D9,  // "iso_8859-14:1998"
    $6FBD03EA,  // "iso-8859-15"
    $6FBD03F6,  // "iso-ir-203"
    $6FBD0401,  // "iso8859-15"
    $6FBD040C,  // "iso_8859-15"
    $6FBD0418,  // "iso_8859-15:1998"
    $6FBE0429,  // "iso-8859-16"
    $6FBE0435,  // "iso-ir-226"
    $6FBE0440,  // "iso8859-16"
    $6FBE044B,  // "iso_8859-16"
    $6FBE0457,  // "iso_8859-16:2001"
    $03A40468,  // "shift_jis"
    $03A40472,  // "cp932"
    $03A40478,  // "shift-jis"
    $03A40482,  // "sjis"
    $03A40487,  // "csshiftjis"
    $03A80492,  // "gbk"
    $03A80496,  // "cp936"
    $03A8049C,  // "windows-936"
    $03A804A8,  // "gb2312"
    $03A804AF,  // "csgb2312"
    $03B504B8,  // "ks_c_5601-1987"
    $03B504C7,  // "cp949"
    $03B504CD,  // "iso-ir-149"
    $03B504D8,  // "korean"
    $03B504DF,  // "ksc_5601"
    $03B504E8,  // "ks_c_5601-1989"
    $03B504F7,  // "csksc56011987"
    $03B60505,  // "big5"
    $03B6050A,  // "cp950"
    $03B60510,  // "big-5"
    $03B60516,  // "big-five"
    $03B6051F,  // "bigfive"
    $03B60527,  // "cn-big5"
    $03B6052F,  // "csbig5"
    $04B00536,  // "ucs-2"
    $04B0053C,  // "csunicode"
    $04B00546,  // "utf-16"
    $04B0054D,  // "utf-16le"
    $04B10556,  // "utf-16be"
    $04E2055F,  // "windows-1250"
    $04E2056C,  // "cp1250"
    $04E30573,  // "windows-1251"
    $04E30580,  // "cp1251"
    $04E40587,  // "windows-1252"
    $04E40594,  // "cp1252"
    $04E5059B,  // "windows-1253"
    $04E505A8,  // "cp1253"
    $04E605AF,  // "windows-1254"
    $04E605BC,  // "cp1254"
    $04E705C3,  // "windows-1255"
    $04E705D0,  // "cp1255"
    $04E805D7,  // "windows-1256"
    $04E805E4,  // "cp1256"
    $04E905EB,  // "windows-1257"
    $04E905F8,  // "cp1257"
    $04EA05FF,  // "windows-1258"
    $04EA060C,  // "cp1258"
    $51820613,  // "koi8-r"
    $51C8061A,  // "gb_2312-80"
    $51C80625,  // "chinese"
    $51C8062D,  // "iso-ir-58"
    $51C80637,  // "csiso58gb231280"
    $556A0647,  // "koi8-u"
    $C42C064E,  // "iso-2022-jp"
    $C42C065A,  // "csiso2022jp"
    $C4310666,  // "iso-2022-kr"
    $C4310672,  // "csiso2022kr"
    $C433067E,  // "iso-2022-cn"
    $CADC068A,  // "euc-jp"
    $CADC0691,  // "eucjp"
    $CADC0697,  // "cseucpkdfmtjapanese"
    $CAE006AB,  // "euc-cn"
    $CAE006B2,  // "euccn"
    $CAED06B8,  // "euc-kr"
    $CAED06BF,  // "euckr"
    $CAEE06C5,  // "euc-tw"
    $CAEE06CC,  // "euctw"
    $CAEE06D2,  // "cseuctw"
    $CEC806DA,  // "hz-gb-2312"
    $D69806E5,  // "gb18030"
    $01B506ED,  // "ibm437"
    $01B506F4,  // "437"
    $01B506F8,  // "cp437"
    $01B506FE,  // "cspc8codepage437"
    $02E1070F,  // "cp737"
    $03070715,  // "ibm775"
    $0307071C,  // "cp775"
    $03070722,  // "cspc775baltic"
    $03520730,  // "ibm850"
    $03520737,  // "850"
    $0352073B,  // "cp850"
    $03520741,  // "cspc850multilingual"
    $03540755,  // "ibm852"
    $0354075C,  // "852"
    $03540760,  // "cp852"
    $03540766,  // "cspcp852"
    $0355076F,  // "cp853"
    $03570775,  // "ibm855"
    $0357077C,  // "855"
    $03570780,  // "cp855"
    $03570786,  // "csibm855"
    $0359078F,  // "ibm857"
    $03590796,  // "857"
    $0359079A,  // "cp857"
    $035907A0,  // "csibm857"
    $035A07A9,  // "cp858"
    $035C07AF,  // "ibm860"
    $035C07B6,  // "860"
    $035C07BA,  // "cp860"
    $035C07C0,  // "csibm860"
    $035D07C9,  // "ibm861"
    $035D07D0,  // "861"
    $035D07D4,  // "cp-is"
    $035D07DA,  // "cp861"
    $035D07E0,  // "csibm861"
    $035E07E9,  // "ibm862"
    $035E07F0,  // "862"
    $035E07F4,  // "cp862"
    $035E07FA,  // "cspc862latinhebrew"
    $035F080D,  // "ibm863"
    $035F0814,  // "863"
    $035F0818,  // "cp863"
    $035F081E,  // "csibm863"
    $03600827,  // "ibm864"
    $0360082E,  // "cp864"
    $03600834,  // "csibm864"
    $0361083D,  // "ibm865"
    $03610844,  // "865"
    $03610848,  // "cp865"
    $0361084E,  // "csibm865"
    $03620857,  // "ibm866"
    $0362085E,  // "866"
    $03620862,  // "cp866"
    $03620868,  // "csibm866"
    $03650871,  // "ibm869"
    $03650878,  // "869"
    $0365087C,  // "cp-gr"
    $03650882,  // "cp869"
    $03650888,  // "csibm869"
    $036A0891,  // "windows-874"
    $036A089D,  // "cp874"
    $036A08A3,  // "iso-ir-166"
    $036A08AE,  // "tis-620"
    $036A08B6,  // "tis620"
    $036A08BD,  // "tis620-0"
    $036A08C6,  // "tis620.2529-1"
    $036A08D4,  // "tis620.2533-0"
    $036A08E2,  // "tis620.2533-1"
    $055108F0,  // "cp1361"
    $055108F7,  // "johab"
    $271008FD,  // "mac"
    $27100901,  // "macintosh"
    $2710090B,  // "macroman"
    $27140914,  // "macarabic"
    $2715091E,  // "machebrew"
    $27160928,  // "macgreek"
    $27170931,  // "maccyrillic"
    $271A093D,  // "macromania"
    $27210948,  // "macukraine"
    $27250953,  // "macthai"
    $272D095B,  // "maccentraleurope"
    $275F096C,  // "maciceland"
    $27610977,  // "macturkish"
    $27620982); // "maccroatian"

  EncodingNameList: array[0..2445] of Byte = (
    $75, $73, $2D, $61, $73, $63, $69, $69, $00, $61, $73, $63, $69, $69, $00, $63,
    $73, $61, $73, $63, $69, $69, $00, $75, $74, $66, $2D, $37, $00, $63, $73, $75,
    $6E, $69, $63, $6F, $64, $65, $31, $31, $75, $74, $66, $37, $00, $75, $74, $66,
    $2D, $38, $00, $75, $74, $66, $38, $00, $69, $73, $6F, $2D, $38, $38, $35, $39,
    $2D, $31, $00, $63, $70, $38, $31, $39, $00, $69, $62, $6D, $38, $31, $39, $00,
    $69, $73, $6F, $2D, $69, $72, $2D, $31, $30, $30, $00, $69, $73, $6F, $38, $38,
    $35, $39, $2D, $31, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $31, $00,
    $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $31, $3A, $31, $39, $38, $37, $00,
    $63, $73, $69, $73, $6F, $6C, $61, $74, $69, $6E, $31, $00, $69, $73, $6F, $2D,
    $38, $38, $35, $39, $2D, $32, $00, $69, $73, $6F, $2D, $69, $72, $2D, $31, $30,
    $31, $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $32, $00, $69, $73, $6F, $5F,
    $38, $38, $35, $39, $2D, $32, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D,
    $32, $3A, $31, $39, $38, $37, $00, $63, $73, $69, $73, $6F, $6C, $61, $74, $69,
    $6E, $32, $00, $69, $73, $6F, $2D, $38, $38, $35, $39, $2D, $33, $00, $69, $73,
    $6F, $2D, $69, $72, $2D, $31, $30, $39, $00, $69, $73, $6F, $38, $38, $35, $39,
    $2D, $33, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $33, $00, $69, $73,
    $6F, $5F, $38, $38, $35, $39, $2D, $33, $3A, $31, $39, $38, $38, $00, $63, $73,
    $69, $73, $6F, $6C, $61, $74, $69, $6E, $33, $00, $69, $73, $6F, $2D, $38, $38,
    $35, $39, $2D, $34, $00, $69, $73, $6F, $2D, $69, $72, $2D, $31, $31, $30, $00,
    $69, $73, $6F, $38, $38, $35, $39, $2D, $34, $00, $69, $73, $6F, $5F, $38, $38,
    $35, $39, $2D, $34, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $34, $3A,
    $31, $39, $38, $38, $00, $63, $73, $69, $73, $6F, $6C, $61, $74, $69, $6E, $34,
    $00, $69, $73, $6F, $2D, $38, $38, $35, $39, $2D, $35, $00, $63, $79, $72, $69,
    $6C, $6C, $69, $63, $00, $69, $73, $6F, $2D, $69, $72, $2D, $31, $34, $34, $00,
    $69, $73, $6F, $38, $38, $35, $39, $2D, $35, $00, $69, $73, $6F, $5F, $38, $38,
    $35, $39, $2D, $35, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $35, $3A,
    $31, $39, $38, $38, $00, $63, $73, $69, $73, $6F, $6C, $61, $74, $69, $6E, $63,
    $79, $72, $69, $6C, $6C, $69, $63, $00, $69, $73, $6F, $2D, $38, $38, $35, $39,
    $2D, $36, $00, $61, $72, $61, $62, $69, $63, $00, $61, $73, $6D, $6F, $2D, $37,
    $30, $38, $00, $65, $63, $6D, $61, $2D, $31, $31, $34, $00, $69, $73, $6F, $2D,
    $69, $72, $2D, $31, $32, $37, $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $36,
    $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $36, $00, $69, $73, $6F, $5F,
    $38, $38, $35, $39, $2D, $36, $3A, $31, $39, $38, $37, $00, $63, $73, $69, $73,
    $6F, $6C, $61, $74, $69, $6E, $61, $72, $61, $62, $69, $63, $00, $69, $73, $6F,
    $2D, $38, $38, $35, $39, $2D, $37, $00, $67, $72, $65, $65, $6B, $00, $67, $72,
    $65, $65, $6B, $38, $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $37, $00, $69,
    $73, $6F, $5F, $38, $38, $35, $39, $2D, $37, $00, $69, $73, $6F, $5F, $38, $38,
    $35, $39, $2D, $37, $3A, $31, $39, $38, $37, $00, $69, $73, $6F, $5F, $38, $38,
    $35, $39, $2D, $37, $3A, $32, $30, $30, $33, $00, $69, $73, $6F, $2D, $38, $38,
    $35, $39, $2D, $38, $00, $68, $65, $62, $72, $65, $77, $00, $69, $73, $6F, $2D,
    $69, $72, $2D, $31, $33, $38, $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $38,
    $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $38, $00, $69, $73, $6F, $5F,
    $38, $38, $35, $39, $2D, $38, $3A, $31, $39, $38, $38, $00, $63, $73, $69, $73,
    $6F, $6C, $61, $74, $69, $6E, $68, $65, $62, $72, $65, $77, $00, $69, $73, $6F,
    $2D, $38, $38, $35, $39, $2D, $39, $00, $69, $73, $6F, $2D, $69, $72, $2D, $31,
    $34, $38, $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $39, $00, $69, $73, $6F,
    $5F, $38, $38, $35, $39, $2D, $39, $00, $69, $73, $6F, $5F, $38, $38, $35, $39,
    $2D, $39, $3A, $31, $39, $38, $39, $00, $63, $73, $69, $73, $6F, $6C, $61, $74,
    $69, $6E, $35, $00, $69, $73, $6F, $2D, $38, $38, $35, $39, $2D, $31, $30, $00,
    $69, $73, $6F, $2D, $69, $72, $2D, $31, $35, $37, $00, $69, $73, $6F, $38, $38,
    $35, $39, $2D, $31, $30, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $31,
    $30, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $31, $30, $3A, $31, $39,
    $39, $32, $00, $63, $73, $69, $73, $6F, $6C, $61, $74, $69, $6E, $36, $00, $69,
    $73, $6F, $2D, $38, $38, $35, $39, $2D, $31, $31, $00, $69, $73, $6F, $38, $38,
    $35, $39, $2D, $31, $31, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $31,
    $31, $00, $69, $73, $6F, $2D, $38, $38, $35, $39, $2D, $31, $33, $00, $69, $73,
    $6F, $2D, $69, $72, $2D, $31, $37, $39, $00, $69, $73, $6F, $38, $38, $35, $39,
    $2D, $31, $33, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D, $31, $33, $00,
    $69, $73, $6F, $2D, $38, $38, $35, $39, $2D, $31, $34, $00, $69, $73, $6F, $2D,
    $63, $65, $6C, $74, $69, $63, $00, $69, $73, $6F, $2D, $69, $72, $2D, $31, $39,
    $39, $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $31, $34, $00, $69, $73, $6F,
    $5F, $38, $38, $35, $39, $2D, $31, $34, $00, $69, $73, $6F, $5F, $38, $38, $35,
    $39, $2D, $31, $34, $3A, $31, $39, $39, $38, $00, $69, $73, $6F, $2D, $38, $38,
    $35, $39, $2D, $31, $35, $00, $69, $73, $6F, $2D, $69, $72, $2D, $32, $30, $33,
    $00, $69, $73, $6F, $38, $38, $35, $39, $2D, $31, $35, $00, $69, $73, $6F, $5F,
    $38, $38, $35, $39, $2D, $31, $35, $00, $69, $73, $6F, $5F, $38, $38, $35, $39,
    $2D, $31, $35, $3A, $31, $39, $39, $38, $00, $69, $73, $6F, $2D, $38, $38, $35,
    $39, $2D, $31, $36, $00, $69, $73, $6F, $2D, $69, $72, $2D, $32, $32, $36, $00,
    $69, $73, $6F, $38, $38, $35, $39, $2D, $31, $36, $00, $69, $73, $6F, $5F, $38,
    $38, $35, $39, $2D, $31, $36, $00, $69, $73, $6F, $5F, $38, $38, $35, $39, $2D,
    $31, $36, $3A, $32, $30, $30, $31, $00, $73, $68, $69, $66, $74, $5F, $6A, $69,
    $73, $00, $63, $70, $39, $33, $32, $00, $73, $68, $69, $66, $74, $2D, $6A, $69,
    $73, $00, $73, $6A, $69, $73, $00, $63, $73, $73, $68, $69, $66, $74, $6A, $69,
    $73, $00, $67, $62, $6B, $00, $63, $70, $39, $33, $36, $00, $77, $69, $6E, $64,
    $6F, $77, $73, $2D, $39, $33, $36, $00, $67, $62, $32, $33, $31, $32, $00, $63,
    $73, $67, $62, $32, $33, $31, $32, $00, $6B, $73, $5F, $63, $5F, $35, $36, $30,
    $31, $2D, $31, $39, $38, $37, $00, $63, $70, $39, $34, $39, $00, $69, $73, $6F,
    $2D, $69, $72, $2D, $31, $34, $39, $00, $6B, $6F, $72, $65, $61, $6E, $00, $6B,
    $73, $63, $5F, $35, $36, $30, $31, $00, $6B, $73, $5F, $63, $5F, $35, $36, $30,
    $31, $2D, $31, $39, $38, $39, $00, $63, $73, $6B, $73, $63, $35, $36, $30, $31,
    $31, $39, $38, $37, $00, $62, $69, $67, $35, $00, $63, $70, $39, $35, $30, $00,
    $62, $69, $67, $2D, $35, $00, $62, $69, $67, $2D, $66, $69, $76, $65, $00, $62,
    $69, $67, $66, $69, $76, $65, $00, $63, $6E, $2D, $62, $69, $67, $35, $00, $63,
    $73, $62, $69, $67, $35, $00, $75, $63, $73, $2D, $32, $00, $63, $73, $75, $6E,
    $69, $63, $6F, $64, $65, $00, $75, $74, $66, $2D, $31, $36, $00, $75, $74, $66,
    $2D, $31, $36, $6C, $65, $00, $75, $74, $66, $2D, $31, $36, $62, $65, $00, $77,
    $69, $6E, $64, $6F, $77, $73, $2D, $31, $32, $35, $30, $00, $63, $70, $31, $32,
    $35, $30, $00, $77, $69, $6E, $64, $6F, $77, $73, $2D, $31, $32, $35, $31, $00,
    $63, $70, $31, $32, $35, $31, $00, $77, $69, $6E, $64, $6F, $77, $73, $2D, $31,
    $32, $35, $32, $00, $63, $70, $31, $32, $35, $32, $00, $77, $69, $6E, $64, $6F,
    $77, $73, $2D, $31, $32, $35, $33, $00, $63, $70, $31, $32, $35, $33, $00, $77,
    $69, $6E, $64, $6F, $77, $73, $2D, $31, $32, $35, $34, $00, $63, $70, $31, $32,
    $35, $34, $00, $77, $69, $6E, $64, $6F, $77, $73, $2D, $31, $32, $35, $35, $00,
    $63, $70, $31, $32, $35, $35, $00, $77, $69, $6E, $64, $6F, $77, $73, $2D, $31,
    $32, $35, $36, $00, $63, $70, $31, $32, $35, $36, $00, $77, $69, $6E, $64, $6F,
    $77, $73, $2D, $31, $32, $35, $37, $00, $63, $70, $31, $32, $35, $37, $00, $77,
    $69, $6E, $64, $6F, $77, $73, $2D, $31, $32, $35, $38, $00, $63, $70, $31, $32,
    $35, $38, $00, $6B, $6F, $69, $38, $2D, $72, $00, $67, $62, $5F, $32, $33, $31,
    $32, $2D, $38, $30, $00, $63, $68, $69, $6E, $65, $73, $65, $00, $69, $73, $6F,
    $2D, $69, $72, $2D, $35, $38, $00, $63, $73, $69, $73, $6F, $35, $38, $67, $62,
    $32, $33, $31, $32, $38, $30, $00, $6B, $6F, $69, $38, $2D, $75, $00, $69, $73,
    $6F, $2D, $32, $30, $32, $32, $2D, $6A, $70, $00, $63, $73, $69, $73, $6F, $32,
    $30, $32, $32, $6A, $70, $00, $69, $73, $6F, $2D, $32, $30, $32, $32, $2D, $6B,
    $72, $00, $63, $73, $69, $73, $6F, $32, $30, $32, $32, $6B, $72, $00, $69, $73,
    $6F, $2D, $32, $30, $32, $32, $2D, $63, $6E, $00, $65, $75, $63, $2D, $6A, $70,
    $00, $65, $75, $63, $6A, $70, $00, $63, $73, $65, $75, $63, $70, $6B, $64, $66,
    $6D, $74, $6A, $61, $70, $61, $6E, $65, $73, $65, $00, $65, $75, $63, $2D, $63,
    $6E, $00, $65, $75, $63, $63, $6E, $00, $65, $75, $63, $2D, $6B, $72, $00, $65,
    $75, $63, $6B, $72, $00, $65, $75, $63, $2D, $74, $77, $00, $65, $75, $63, $74,
    $77, $00, $63, $73, $65, $75, $63, $74, $77, $00, $68, $7A, $2D, $67, $62, $2D,
    $32, $33, $31, $32, $00, $67, $62, $31, $38, $30, $33, $30, $00, $69, $62, $6D,
    $34, $33, $37, $00, $34, $33, $37, $00, $63, $70, $34, $33, $37, $00, $63, $73,
    $70, $63, $38, $63, $6F, $64, $65, $70, $61, $67, $65, $34, $33, $37, $00, $63,
    $70, $37, $33, $37, $00, $69, $62, $6D, $37, $37, $35, $00, $63, $70, $37, $37,
    $35, $00, $63, $73, $70, $63, $37, $37, $35, $62, $61, $6C, $74, $69, $63, $00,
    $69, $62, $6D, $38, $35, $30, $00, $38, $35, $30, $00, $63, $70, $38, $35, $30,
    $00, $63, $73, $70, $63, $38, $35, $30, $6D, $75, $6C, $74, $69, $6C, $69, $6E,
    $67, $75, $61, $6C, $00, $69, $62, $6D, $38, $35, $32, $00, $38, $35, $32, $00,
    $63, $70, $38, $35, $32, $00, $63, $73, $70, $63, $70, $38, $35, $32, $00, $63,
    $70, $38, $35, $33, $00, $69, $62, $6D, $38, $35, $35, $00, $38, $35, $35, $00,
    $63, $70, $38, $35, $35, $00, $63, $73, $69, $62, $6D, $38, $35, $35, $00, $69,
    $62, $6D, $38, $35, $37, $00, $38, $35, $37, $00, $63, $70, $38, $35, $37, $00,
    $63, $73, $69, $62, $6D, $38, $35, $37, $00, $63, $70, $38, $35, $38, $00, $69,
    $62, $6D, $38, $36, $30, $00, $38, $36, $30, $00, $63, $70, $38, $36, $30, $00,
    $63, $73, $69, $62, $6D, $38, $36, $30, $00, $69, $62, $6D, $38, $36, $31, $00,
    $38, $36, $31, $00, $63, $70, $2D, $69, $73, $00, $63, $70, $38, $36, $31, $00,
    $63, $73, $69, $62, $6D, $38, $36, $31, $00, $69, $62, $6D, $38, $36, $32, $00,
    $38, $36, $32, $00, $63, $70, $38, $36, $32, $00, $63, $73, $70, $63, $38, $36,
    $32, $6C, $61, $74, $69, $6E, $68, $65, $62, $72, $65, $77, $00, $69, $62, $6D,
    $38, $36, $33, $00, $38, $36, $33, $00, $63, $70, $38, $36, $33, $00, $63, $73,
    $69, $62, $6D, $38, $36, $33, $00, $69, $62, $6D, $38, $36, $34, $00, $63, $70,
    $38, $36, $34, $00, $63, $73, $69, $62, $6D, $38, $36, $34, $00, $69, $62, $6D,
    $38, $36, $35, $00, $38, $36, $35, $00, $63, $70, $38, $36, $35, $00, $63, $73,
    $69, $62, $6D, $38, $36, $35, $00, $69, $62, $6D, $38, $36, $36, $00, $38, $36,
    $36, $00, $63, $70, $38, $36, $36, $00, $63, $73, $69, $62, $6D, $38, $36, $36,
    $00, $69, $62, $6D, $38, $36, $39, $00, $38, $36, $39, $00, $63, $70, $2D, $67,
    $72, $00, $63, $70, $38, $36, $39, $00, $63, $73, $69, $62, $6D, $38, $36, $39,
    $00, $77, $69, $6E, $64, $6F, $77, $73, $2D, $38, $37, $34, $00, $63, $70, $38,
    $37, $34, $00, $69, $73, $6F, $2D, $69, $72, $2D, $31, $36, $36, $00, $74, $69,
    $73, $2D, $36, $32, $30, $00, $74, $69, $73, $36, $32, $30, $00, $74, $69, $73,
    $36, $32, $30, $2D, $30, $00, $74, $69, $73, $36, $32, $30, $2E, $32, $35, $32,
    $39, $2D, $31, $00, $74, $69, $73, $36, $32, $30, $2E, $32, $35, $33, $33, $2D,
    $30, $00, $74, $69, $73, $36, $32, $30, $2E, $32, $35, $33, $33, $2D, $31, $00,
    $63, $70, $31, $33, $36, $31, $00, $6A, $6F, $68, $61, $62, $00, $6D, $61, $63,
    $00, $6D, $61, $63, $69, $6E, $74, $6F, $73, $68, $00, $6D, $61, $63, $72, $6F,
    $6D, $61, $6E, $00, $6D, $61, $63, $61, $72, $61, $62, $69, $63, $00, $6D, $61,
    $63, $68, $65, $62, $72, $65, $77, $00, $6D, $61, $63, $67, $72, $65, $65, $6B,
    $00, $6D, $61, $63, $63, $79, $72, $69, $6C, $6C, $69, $63, $00, $6D, $61, $63,
    $72, $6F, $6D, $61, $6E, $69, $61, $00, $6D, $61, $63, $75, $6B, $72, $61, $69,
    $6E, $65, $00, $6D, $61, $63, $74, $68, $61, $69, $00, $6D, $61, $63, $63, $65,
    $6E, $74, $72, $61, $6C, $65, $75, $72, $6F, $70, $65, $00, $6D, $61, $63, $69,
    $63, $65, $6C, $61, $6E, $64, $00, $6D, $61, $63, $74, $75, $72, $6B, $69, $73,
    $68, $00, $6D, $61, $63, $63, $72, $6F, $61, $74, $69, $61, $6E, $00);