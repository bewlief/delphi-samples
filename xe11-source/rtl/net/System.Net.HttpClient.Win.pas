{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Net.HttpClient.Win;

interface

{$HPPEMIT NOUSINGNAMESPACE}

{$HPPEMIT '#pragma comment(lib, "winhttp")'}
{$HPPEMIT '#pragma comment(lib, "crypt32")'}

function ShowSelectCertificateDialog(AParentWnd: UIntPtr;
  const ATitle, ADisplayString: string; var ACertificate): Boolean;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.Net.URLClient,
  System.NetConsts,
  System.Net.HttpClient,
  System.Types,
  Winapi.Windows,
  Winapi.WinHTTP,
  System.NetEncoding,
  System.Net.Mime;

{ Crypto functions needed }
type
  HCRYPTPROV = ULONG_PTR;
  {$EXTERNALSYM HCRYPTPROV}

  _CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: LPBYTE;
  end;
  {$EXTERNALSYM _CRYPTOAPI_BLOB}
  CRYPT_INTEGER_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_INTEGER_BLOB}
  PCRYPT_INTEGER_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_INTEGER_BLOB}
  CRYPT_OBJID_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_OBJID_BLOB}
  CERT_NAME_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM CERT_NAME_BLOB}
  PCERT_NAME_BLOB = ^CERT_NAME_BLOB;
  {$EXTERNALSYM PCERT_NAME_BLOB}

  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;
  {$EXTERNALSYM PCRYPT_BIT_BLOB}
  _CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: LPBYTE;
    cUnusedBits: DWORD;
  end;
  {$EXTERNALSYM _CRYPT_BIT_BLOB}
  CRYPT_BIT_BLOB = _CRYPT_BIT_BLOB;
  {$EXTERNALSYM CRYPT_BIT_BLOB}

  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;
  {$EXTERNALSYM PCRYPT_ALGORITHM_IDENTIFIER}
  _CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM _CRYPT_ALGORITHM_IDENTIFIER}
  CRYPT_ALGORITHM_IDENTIFIER = _CRYPT_ALGORITHM_IDENTIFIER;
  {$EXTERNALSYM CRYPT_ALGORITHM_IDENTIFIER}

  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;
  {$EXTERNALSYM PCERT_PUBLIC_KEY_INFO}
  _CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;
  {$EXTERNALSYM _CERT_PUBLIC_KEY_INFO}
  CERT_PUBLIC_KEY_INFO = _CERT_PUBLIC_KEY_INFO;
  {$EXTERNALSYM CERT_PUBLIC_KEY_INFO}

  PCERT_EXTENSION = ^CERT_EXTENSION;
  {$EXTERNALSYM PCERT_EXTENSION}
  _CERT_EXTENSION = record
    pszObjId: LPSTR;
    fCritical: BOOL;
    Value: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM _CERT_EXTENSION}
  CERT_EXTENSION = _CERT_EXTENSION;
  {$EXTERNALSYM CERT_EXTENSION}


  PCERT_INFO = ^CERT_INFO;
  {$EXTERNALSYM PCERT_INFO}
  _CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: FILETIME;
    NotAfter: FILETIME;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;
  {$EXTERNALSYM _CERT_INFO}
  CERT_INFO = _CERT_INFO;
  {$EXTERNALSYM CERT_INFO}


  HCERTSTORE = Pointer;
  {$EXTERNALSYM HCERTSTORE}
  PCERT_CONTEXT = ^CERT_CONTEXT;

  {$EXTERNALSYM PCERT_CONTEXT}
  _CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: LPBYTE;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;
  {$EXTERNALSYM _CERT_CONTEXT}
  CERT_CONTEXT = _CERT_CONTEXT;
  {$EXTERNALSYM CERT_CONTEXT}
  PCCERT_CONTEXT = PCERT_CONTEXT;
  {$EXTERNALSYM PCCERT_CONTEXT}

const
  Crypt32 = 'Crypt32.dll';

  CERT_CHAIN_FIND_BY_ISSUER_COMPARE_KEY_FLAG         = $0001;
  CERT_CHAIN_FIND_BY_ISSUER_COMPLEX_CHAIN_FLAG       = $0002;
  CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG      = $0004;
  CERT_CHAIN_FIND_BY_ISSUER_LOCAL_MACHINE_FLAG       = $0008;
  CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG              = $4000;
  CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG          = $8000;

  CERT_CHAIN_FIND_BY_ISSUER              = 1;

//+-------------------------------------------------------------------------
//  Certificate versions
//--------------------------------------------------------------------------

  CERT_V1 = 0;
  {$EXTERNALSYM CERT_V1}
  CERT_V2 = 1;
  {$EXTERNALSYM CERT_V2}
  CERT_V3 = 2;
  {$EXTERNALSYM CERT_V3}

//+-------------------------------------------------------------------------
//  Certificate Information Flags
//--------------------------------------------------------------------------

  CERT_INFO_VERSION_FLAG                 = 1;
  {$EXTERNALSYM CERT_INFO_VERSION_FLAG}
  CERT_INFO_SERIAL_NUMBER_FLAG           = 2;
  {$EXTERNALSYM CERT_INFO_SERIAL_NUMBER_FLAG}
  CERT_INFO_SIGNATURE_ALGORITHM_FLAG     = 3;
  {$EXTERNALSYM CERT_INFO_SIGNATURE_ALGORITHM_FLAG}
  CERT_INFO_ISSUER_FLAG                  = 4;
  {$EXTERNALSYM CERT_INFO_ISSUER_FLAG}
  CERT_INFO_NOT_BEFORE_FLAG              = 5;
  {$EXTERNALSYM CERT_INFO_NOT_BEFORE_FLAG}
  CERT_INFO_NOT_AFTER_FLAG               = 6;
  {$EXTERNALSYM CERT_INFO_NOT_AFTER_FLAG}
  CERT_INFO_SUBJECT_FLAG                 = 7;
  {$EXTERNALSYM CERT_INFO_SUBJECT_FLAG}
  CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = 8;
  {$EXTERNALSYM CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG}
  CERT_INFO_ISSUER_UNIQUE_ID_FLAG        = 9;
  {$EXTERNALSYM CERT_INFO_ISSUER_UNIQUE_ID_FLAG}
  CERT_INFO_SUBJECT_UNIQUE_ID_FLAG       = 10;
  {$EXTERNALSYM CERT_INFO_SUBJECT_UNIQUE_ID_FLAG}
  CERT_INFO_EXTENSION_FLAG               = 11;

//+-------------------------------------------------------------------------
// Certificate comparison functions
//--------------------------------------------------------------------------

  {$EXTERNALSYM CERT_INFO_EXTENSION_FLAG}
  CERT_COMPARE_MASK           = $FFFF;
  {$EXTERNALSYM CERT_COMPARE_MASK}
  CERT_COMPARE_SHIFT          = 16;
  {$EXTERNALSYM CERT_COMPARE_SHIFT}
  CERT_COMPARE_ANY            = 0;
  {$EXTERNALSYM CERT_COMPARE_ANY}
  CERT_COMPARE_SHA1_HASH      = 1;
  {$EXTERNALSYM CERT_COMPARE_SHA1_HASH}
  CERT_COMPARE_NAME           = 2;
  {$EXTERNALSYM CERT_COMPARE_NAME}
  CERT_COMPARE_ATTR           = 3;
  {$EXTERNALSYM CERT_COMPARE_ATTR}
  CERT_COMPARE_MD5_HASH       = 4;
  {$EXTERNALSYM CERT_COMPARE_MD5_HASH}
  CERT_COMPARE_PROPERTY       = 5;
  {$EXTERNALSYM CERT_COMPARE_PROPERTY}
  CERT_COMPARE_PUBLIC_KEY     = 6;
  {$EXTERNALSYM CERT_COMPARE_PUBLIC_KEY}
  CERT_COMPARE_HASH           = CERT_COMPARE_SHA1_HASH;
  {$EXTERNALSYM CERT_COMPARE_HASH}
  CERT_COMPARE_NAME_STR_A     = 7;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_A}
  CERT_COMPARE_NAME_STR_W     = 8;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_W}
  CERT_COMPARE_KEY_SPEC       = 9;
  {$EXTERNALSYM CERT_COMPARE_KEY_SPEC}
  CERT_COMPARE_ENHKEY_USAGE   = 10;
  {$EXTERNALSYM CERT_COMPARE_ENHKEY_USAGE}
  CERT_COMPARE_CTL_USAGE      = CERT_COMPARE_ENHKEY_USAGE;
  {$EXTERNALSYM CERT_COMPARE_CTL_USAGE}
  CERT_COMPARE_SUBJECT_CERT   = 11;
  {$EXTERNALSYM CERT_COMPARE_SUBJECT_CERT}
  CERT_COMPARE_ISSUER_OF      = 12;
  {$EXTERNALSYM CERT_COMPARE_ISSUER_OF}
  CERT_COMPARE_EXISTING       = 13;
  {$EXTERNALSYM CERT_COMPARE_EXISTING}
  CERT_COMPARE_SIGNATURE_HASH = 14;
  {$EXTERNALSYM CERT_COMPARE_SIGNATURE_HASH}
  CERT_COMPARE_KEY_IDENTIFIER = 15;
  {$EXTERNALSYM CERT_COMPARE_KEY_IDENTIFIER}
  CERT_COMPARE_CERT_ID        = 16;
  {$EXTERNALSYM CERT_COMPARE_CERT_ID}

//+-------------------------------------------------------------------------
//  dwFindType
//
//  The dwFindType definition consists of two components:
//   - comparison function
//   - certificate information flag
//--------------------------------------------------------------------------

  CERT_FIND_ANY            = CERT_COMPARE_ANY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ANY}
  CERT_FIND_SHA1_HASH      = CERT_COMPARE_SHA1_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SHA1_HASH}
  CERT_FIND_MD5_HASH       = CERT_COMPARE_MD5_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_MD5_HASH}
  CERT_FIND_SIGNATURE_HASH = CERT_COMPARE_SIGNATURE_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SIGNATURE_HASH}
  CERT_FIND_KEY_IDENTIFIER = CERT_COMPARE_KEY_IDENTIFIER shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_KEY_IDENTIFIER}
  CERT_FIND_HASH           = CERT_FIND_SHA1_HASH;
  {$EXTERNALSYM CERT_FIND_HASH}
  CERT_FIND_PROPERTY       = CERT_COMPARE_PROPERTY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_PROPERTY}
  CERT_FIND_PUBLIC_KEY     = CERT_COMPARE_PUBLIC_KEY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_PUBLIC_KEY}
  CERT_FIND_SUBJECT_NAME   = CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_NAME}
  CERT_FIND_SUBJECT_ATTR   = CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_ATTR}
  CERT_FIND_ISSUER_NAME    = CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_NAME}
  CERT_FIND_ISSUER_ATTR    = CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_ATTR}
  CERT_FIND_SUBJECT_STR_A  = CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_A}
  CERT_FIND_SUBJECT_STR_W  = CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_W}
  CERT_FIND_SUBJECT_STR    = CERT_FIND_SUBJECT_STR_W;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR}
  CERT_FIND_ISSUER_STR_A   = CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR_A}
  CERT_FIND_ISSUER_STR_W   = CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR_W}
  CERT_FIND_ISSUER_STR     = CERT_FIND_ISSUER_STR_W;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR}
  CERT_FIND_KEY_SPEC       = CERT_COMPARE_KEY_SPEC shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_KEY_SPEC}
  CERT_FIND_ENHKEY_USAGE   = CERT_COMPARE_ENHKEY_USAGE shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ENHKEY_USAGE}
  CERT_FIND_CTL_USAGE      = CERT_FIND_ENHKEY_USAGE;
  {$EXTERNALSYM CERT_FIND_CTL_USAGE}

  CERT_FIND_SUBJECT_CERT = CERT_COMPARE_SUBJECT_CERT shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SUBJECT_CERT}
  CERT_FIND_ISSUER_OF    = CERT_COMPARE_ISSUER_OF shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ISSUER_OF}
  CERT_FIND_EXISTING     = CERT_COMPARE_EXISTING shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_EXISTING}
  CERT_FIND_CERT_ID      = CERT_COMPARE_CERT_ID shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_CERT_ID}

//+-------------------------------------------------------------------------
//  CERT_FIND_ANY
//
//  Find any certificate.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_HASH
//
//  Find a certificate with the specified hash.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_IDENTIFIER
//
//  Find a certificate with the specified KeyIdentifier. Gets the
//  CERT_KEY_IDENTIFIER_PROP_ID property and compares with the input
//  CRYPT_HASH_BLOB.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PROPERTY
//
//  Find a certificate having the specified property.
//
//  pvFindPara points to a DWORD containing the PROP_ID
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PUBLIC_KEY
//
//  Find a certificate matching the specified public key.
//
//  pvFindPara points to a CERT_PUBLIC_KEY_INFO containing the public key
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_NAME
//  CERT_FIND_ISSUER_NAME
//
//  Find a certificate with the specified subject/issuer name. Does an exact
//  match of the entire name.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_NAME_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_ATTR
//  CERT_FIND_ISSUER_ATTR
//
//  Find a certificate with the specified subject/issuer attributes.
//
//  Compares the attributes in the subject/issuer name with the
//  Relative Distinguished Name's (CERT_RDN) array of attributes specified in
//  pvFindPara. The comparison iterates through the CERT_RDN attributes and looks
//  for an attribute match in any of the subject/issuer's RDNs.
//
//  The CERT_RDN_ATTR fields can have the following special values:
//    pszObjId == NULL              - ignore the attribute object identifier
//    dwValueType == RDN_ANY_TYPE   - ignore the value type
//    Value.pbData == NULL          - match any value
//
//  CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags to do
//  a case insensitive match. Otherwise, defaults to an exact, case sensitive
//  match.
//
//  CERT_UNICODE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags if the RDN was
//  initialized with unicode strings as for
//  CryptEncodeObject(X509_UNICODE_NAME).
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_RDN (defined in wincert.h).
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_STR_A
//  CERT_FIND_SUBJECT_STR_W | CERT_FIND_SUBJECT_STR
//  CERT_FIND_ISSUER_STR_A
//  CERT_FIND_ISSUER_STR_W  | CERT_FIND_ISSUER_STR
//
//  Find a certificate containing the specified subject/issuer name string.
//
//  First, the certificate's subject/issuer is converted to a name string
//  via CertNameToStrA/CertNameToStrW(CERT_SIMPLE_NAME_STR). Then, a
//  case insensitive substring within string match is performed.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  For *_STR_A, pvFindPara points to a null terminated character string.
//  For *_STR_W, pvFindPara points to a null terminated wide character string.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_SPEC
//
//  Find a certificate having a CERT_KEY_SPEC_PROP_ID property matching
//  the specified KeySpec.
//
//  pvFindPara points to a DWORD containing the KeySpec.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_ENHKEY_USAGE
//
//  Find a certificate having the szOID_ENHANCED_KEY_USAGE extension or
//  the CERT_ENHKEY_USAGE_PROP_ID and matching the specified pszUsageIdentifers.
//
//  pvFindPara points to a CERT_ENHKEY_USAGE data structure. If pvFindPara
//  is NULL or CERT_ENHKEY_USAGE's cUsageIdentifier is 0, then, matches any
//  certificate having enhanced key usage.
//
//  If the CERT_FIND_VALID_ENHKEY_USAGE_FLAG is set, then, only does a match
//  for certificates that are valid for the specified usages. By default,
//  the ceriticate must be valid for all usages. CERT_FIND_OR_ENHKEY_USAGE_FLAG
//  can be set, if the certificate only needs to be valid for one of the
//  specified usages. Note, CertGetValidUsages() is called to get the
//  certificate's list of valid usages. Only the CERT_FIND_OR_ENHKEY_USAGE_FLAG
//  is applicable when this flag is set.
//
//  The CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG can be set in dwFindFlags to
//  also match a certificate without either the extension or property.
//
//  If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set in dwFindFlags, finds
//  certificates without the key usage extension or property. Setting this
//  flag takes precedence over pvFindPara being NULL.
//
//  If the CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the extension. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the extension. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the extension. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the extension.
//
//  If the CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the property. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the property. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the property. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the property.
//
//  If CERT_FIND_OR_ENHKEY_USAGE_FLAG is set, does an "OR" match of any of
//  the specified pszUsageIdentifiers. If not set, then, does an "AND" match
//  of all of the specified pszUsageIdentifiers.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_CERT_ID
//
//  Find a certificate with the specified CERT_ID.
//
//  pvFindPara points to a CERT_ID.
//--------------------------------------------------------------------------

  CRYPT_ASN_ENCODING  = $00000001;
  {$EXTERNALSYM CRYPT_ASN_ENCODING}
  CRYPT_NDR_ENCODING  = $00000002;
  {$EXTERNALSYM CRYPT_NDR_ENCODING}
  X509_ASN_ENCODING   = $00000001;
  {$EXTERNALSYM X509_ASN_ENCODING}
  X509_NDR_ENCODING   = $00000002;
  {$EXTERNALSYM X509_NDR_ENCODING}
  PKCS_7_ASN_ENCODING = $00010000;
  {$EXTERNALSYM PKCS_7_ASN_ENCODING}
  PKCS_7_NDR_ENCODING = $00020000;
  {$EXTERNALSYM PKCS_7_NDR_ENCODING}

//+-------------------------------------------------------------------------
//  Certificate name string types
//--------------------------------------------------------------------------
  CERT_SIMPLE_NAME_STR = 1;
  {$EXTERNALSYM CERT_SIMPLE_NAME_STR}
  CERT_OID_NAME_STR    = 2;
  {$EXTERNALSYM CERT_OID_NAME_STR}
  CERT_X500_NAME_STR   = 3;
  {$EXTERNALSYM CERT_X500_NAME_STR}

  CERT_NAME_STR_CRLF_FLAG = $08000000;
  {$EXTERNALSYM CERT_NAME_STR_CRLF_FLAG}

//+-------------------------------------------------------------------------
//  Certificate name types
//--------------------------------------------------------------------------

const
  CERT_NAME_EMAIL_TYPE            = 1;
  {$EXTERNALSYM CERT_NAME_EMAIL_TYPE}
  CERT_NAME_RDN_TYPE              = 2;
  {$EXTERNALSYM CERT_NAME_RDN_TYPE}
  CERT_NAME_ATTR_TYPE             = 3;
  {$EXTERNALSYM CERT_NAME_ATTR_TYPE}
  CERT_NAME_SIMPLE_DISPLAY_TYPE   = 4;
  {$EXTERNALSYM CERT_NAME_SIMPLE_DISPLAY_TYPE}
  CERT_NAME_FRIENDLY_DISPLAY_TYPE = 5;
  {$EXTERNALSYM CERT_NAME_FRIENDLY_DISPLAY_TYPE}

//+-------------------------------------------------------------------------
//  Certificate name flags
//--------------------------------------------------------------------------

  CERT_NAME_ISSUER_FLAG           = $1;
  {$EXTERNALSYM CERT_NAME_ISSUER_FLAG}
  CERT_NAME_DISABLE_IE4_UTF8_FLAG = $00010000;
  {$EXTERNALSYM CERT_NAME_DISABLE_IE4_UTF8_FLAG}

type
  _SecPkgContext_IssuerListInfoEx = record
    aIssuers: PCERT_NAME_BLOB;
    cIssuers: DWORD;
  end;
  {$EXTERNALSYM _SecPkgContext_IssuerListInfoEx}
  SecPkgContext_IssuerListInfoEx = _SecPkgContext_IssuerListInfoEx;
  {$EXTERNALSYM SecPkgContext_IssuerListInfoEx}
  PSecPkgContext_IssuerListInfoEx = ^SecPkgContext_IssuerListInfoEx;
  {$EXTERNALSYM PSecPkgContext_IssuerListInfoEx}

  _CERT_TRUST_STATUS = record
    dwErrorStatus: DWORD;
    dwInfoStatus: DWORD;
  end;
  {$EXTERNALSYM _CERT_TRUST_STATUS}
  CERT_TRUST_STATUS = _CERT_TRUST_STATUS;
  {$EXTERNALSYM CERT_TRUST_STATUS}
  PCERT_TRUST_STATUS = ^_CERT_TRUST_STATUS;
  {$EXTERNALSYM PCERT_TRUST_STATUS}


  _CERT_CHAIN_ELEMENT = record
    cbSize: DWORD;
    pCertContext: PCCERT_CONTEXT;
    TrustStatus: CERT_TRUST_STATUS;
    pRevocationInfo: Pointer;//    pRevocationInfo: PCERT_REVOCATION_INFO;
    pIssuanceUsage: Pointer;//    pIssuanceUsage: PCERT_ENHKEY_USAGE;
    pApplicationUsage: Pointer;//    pApplicationUsage: PCERT_ENHKEY_USAGE;
    pwszExtendedErrorInfo: LPCWSTR;
  end;
  {$EXTERNALSYM _CERT_CHAIN_ELEMENT}
  CERT_CHAIN_ELEMENT = _CERT_CHAIN_ELEMENT;
  {$EXTERNALSYM CERT_CHAIN_ELEMENT}
  PCERT_CHAIN_ELEMENT = ^_CERT_CHAIN_ELEMENT;
  {$EXTERNALSYM PCERT_CHAIN_ELEMENT}
  PPCERT_CHAIN_ELEMENT = ^PCERT_CHAIN_ELEMENT;

  _CERT_SIMPLE_CHAIN = record
    cbSize: DWORD;
    TrustStatus: CERT_TRUST_STATUS;
    cElement: DWORD;
    rgpElement: PPCERT_CHAIN_ELEMENT;
    pTrustListInfo: Pointer;//pTrustListInfo: PCERT_TRUST_LIST_INFO;
    fHasRevocationFreshnessTime: BOOL;
    dwRevocationFreshnessTime: DWORD;
  end;
  {$EXTERNALSYM _CERT_SIMPLE_CHAIN}
  CERT_SIMPLE_CHAIN = _CERT_SIMPLE_CHAIN;
  {$EXTERNALSYM CERT_SIMPLE_CHAIN}
  PCERT_SIMPLE_CHAIN = ^_CERT_SIMPLE_CHAIN;
  {$EXTERNALSYM PCERT_SIMPLE_CHAIN}
  PPCERT_SIMPLE_CHAIN = ^PCERT_SIMPLE_CHAIN;


  PCCERT_CHAIN_CONTEXT = ^_CERT_CHAIN_CONTEXT;
  {$EXTERNALSYM PCCERT_CHAIN_CONTEXT}
  PPCCERT_CHAIN_CONTEXT = ^PCCERT_CHAIN_CONTEXT;
  _CERT_CHAIN_CONTEXT = record
    cbSize: DWORD;
    TrustStatus: CERT_TRUST_STATUS;
    cChain: DWORD;
    rgpChain: PPCERT_SIMPLE_CHAIN;
    cLowerQualityChainContext: DWORD;
    rgpLowerQualityChainContext: PPCCERT_CHAIN_CONTEXT;
    fHasRevocationFreshnessTime: BOOL;
    dwRevocationFreshnessTime: DWORD;
  end;
  {$EXTERNALSYM _CERT_CHAIN_CONTEXT}
  CERT_CHAIN_CONTEXT = _CERT_CHAIN_CONTEXT;
  {$EXTERNALSYM CERT_CHAIN_CONTEXT}

  PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK = procedure;

  _CERT_CHAIN_FIND_BY_ISSUER_PARA = record
    cbSize: DWORD;
    pszUsageIdentifier: LPCSTR;
    dwKeySpec: DWORD;
    dwAcquirePrivateKeyFlags: DWORD;
    cIssuer: DWORD;
    rgIssuer: PCERT_NAME_BLOB;
    pfnFindCallback: PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK;
    pvFindArg: Pointer;
    pdwIssuerChainIndex: PDWORD;
    pdwIssuerElementIndex: PDWORD;
  end;
  {$EXTERNALSYM _CERT_CHAIN_FIND_BY_ISSUER_PARA}
  CERT_CHAIN_FIND_BY_ISSUER_PARA = _CERT_CHAIN_FIND_BY_ISSUER_PARA;
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_PARA}
  PCERT_CHAIN_FIND_BY_ISSUER_PARA = ^CERT_CHAIN_FIND_BY_ISSUER_PARA;
  {$EXTERNALSYM PCERT_CHAIN_FIND_BY_ISSUER_PARA}

const
  CERT_CLOSE_STORE_FORCE_FLAG = $00000001;
  {$EXTERNALSYM CERT_CLOSE_STORE_FORCE_FLAG}
  CERT_CLOSE_STORE_CHECK_FLAG = $00000002;
  {$EXTERNALSYM CERT_CLOSE_STORE_CHECK_FLAG}

const
  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG  = $1;
  {$EXTERNALSYM CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG}
  CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG  = $2;
  {$EXTERNALSYM CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG}
  CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG = $4;
  {$EXTERNALSYM CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG}
  CERT_FIND_NO_ENHKEY_USAGE_FLAG        = $8;
  {$EXTERNALSYM CERT_FIND_NO_ENHKEY_USAGE_FLAG}
  CERT_FIND_OR_ENHKEY_USAGE_FLAG        = $10;
  {$EXTERNALSYM CERT_FIND_OR_ENHKEY_USAGE_FLAG}
  CERT_FIND_VALID_ENHKEY_USAGE_FLAG     = $20;
  {$EXTERNALSYM CERT_FIND_VALID_ENHKEY_USAGE_FLAG}

  CERT_FIND_OPTIONAL_CTL_USAGE_FLAG = CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_OPTIONAL_CTL_USAGE_FLAG}

  CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG = CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG}

  CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG = CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG}

  CERT_FIND_NO_CTL_USAGE_FLAG    = CERT_FIND_NO_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_NO_CTL_USAGE_FLAG}
  CERT_FIND_OR_CTL_USAGE_FLAG    = CERT_FIND_OR_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_OR_CTL_USAGE_FLAG}
  CERT_FIND_VALID_CTL_USAGE_FLAG = CERT_FIND_VALID_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_VALID_CTL_USAGE_FLAG}

const
//+-------------------------------------------------------------------------
//  Certificate Store Provider Types
//--------------------------------------------------------------------------
  CERT_STORE_PROV_MSG                 = 1;
  CERT_STORE_PROV_MEMORY              = 2;
  CERT_STORE_PROV_FILE                = 3;
  CERT_STORE_PROV_REG                 = 4;

  CERT_STORE_PROV_PKCS7               = 5;
  CERT_STORE_PROV_SERIALIZED          = 6;
  CERT_STORE_PROV_FILENAME_A          = 7;
  CERT_STORE_PROV_FILENAME_W          = 8;
  CERT_STORE_PROV_FILENAME            = CERT_STORE_PROV_FILENAME_W;
  CERT_STORE_PROV_SYSTEM_A            = 9;
  CERT_STORE_PROV_SYSTEM_W            = 10;
  CERT_STORE_PROV_SYSTEM              = CERT_STORE_PROV_SYSTEM_W;

  CERT_STORE_PROV_COLLECTION          = 11;
  CERT_STORE_PROV_SYSTEM_REGISTRY_A   = 12;
  CERT_STORE_PROV_SYSTEM_REGISTRY_W   = 13;
  CERT_STORE_PROV_SYSTEM_REGISTRY     = CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  CERT_STORE_PROV_PHYSICAL_W          = 14;
  CERT_STORE_PROV_PHYSICAL            = CERT_STORE_PROV_PHYSICAL_W;

  // SmartCard Store Provider isn't supported
  CERT_STORE_PROV_SMART_CARD_W        = 15;
  CERT_STORE_PROV_SMART_CARD          = CERT_STORE_PROV_SMART_CARD_W;

  CERT_STORE_PROV_LDAP_W              = 16;
  CERT_STORE_PROV_LDAP                = CERT_STORE_PROV_LDAP_W;
  CERT_STORE_PROV_PKCS12              = 17;

  sz_CERT_STORE_PROV_MEMORY           = 'Memory';
  sz_CERT_STORE_PROV_FILENAME_W       = 'File';
  sz_CERT_STORE_PROV_FILENAME         = sz_CERT_STORE_PROV_FILENAME_W;
  sz_CERT_STORE_PROV_SYSTEM_W         = 'System';
  sz_CERT_STORE_PROV_SYSTEM           = sz_CERT_STORE_PROV_SYSTEM_W;
  sz_CERT_STORE_PROV_PKCS7            = 'PKCS7';
  sz_CERT_STORE_PROV_PKCS12           = 'PKCS12';
  sz_CERT_STORE_PROV_SERIALIZED       = 'Serialized';

  sz_CERT_STORE_PROV_COLLECTION       = 'Collection';
  sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W = 'SystemRegistry';
  sz_CERT_STORE_PROV_SYSTEM_REGISTRY  = sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  sz_CERT_STORE_PROV_PHYSICAL_W       = 'Physical';
  sz_CERT_STORE_PROV_PHYSICAL         = sz_CERT_STORE_PROV_PHYSICAL_W;

  // SmartCard Store Provider isn't supported
  sz_CERT_STORE_PROV_SMART_CARD_W     = 'SmartCard';
  sz_CERT_STORE_PROV_SMART_CARD       = sz_CERT_STORE_PROV_SMART_CARD_W;

  sz_CERT_STORE_PROV_LDAP_W           = 'Ldap';
  sz_CERT_STORE_PROV_LDAP             = sz_CERT_STORE_PROV_LDAP_W;

//+-------------------------------------------------------------------------
//  Certificate Store open/property flags
//--------------------------------------------------------------------------
  CERT_STORE_NO_CRYPT_RELEASE_FLAG                = $00000001;
  CERT_STORE_SET_LOCALIZED_NAME_FLAG              = $00000002;
  CERT_STORE_DEFER_CLOSE_UNTIL_LAST_FREE_FLAG     = $00000004;
  CERT_STORE_DELETE_FLAG                          = $00000010;
  CERT_STORE_UNSAFE_PHYSICAL_FLAG                 = $00000020;
  CERT_STORE_SHARE_STORE_FLAG                     = $00000040;
  CERT_STORE_SHARE_CONTEXT_FLAG                   = $00000080;
  CERT_STORE_MANIFOLD_FLAG                        = $00000100;
  CERT_STORE_ENUM_ARCHIVED_FLAG                   = $00000200;
  CERT_STORE_UPDATE_KEYID_FLAG                    = $00000400;
  CERT_STORE_BACKUP_RESTORE_FLAG                  = $00000800;
  CERT_STORE_READONLY_FLAG                        = $00008000;
  CERT_STORE_OPEN_EXISTING_FLAG                   = $00004000;
  CERT_STORE_CREATE_NEW_FLAG                      = $00002000;
  CERT_STORE_MAXIMUM_ALLOWED_FLAG                 = $00001000;

//+-------------------------------------------------------------------------
//  Additional predefined data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  RSA_CSP_PUBLICKEYBLOB                = LPCSTR(19);

type
  ALG_ID = Cardinal;

  _PUBLICKEYSTRUC = record
    bType: BYTE;
    bVersion: BYTE;
    reserved: WORD;
    aiKeyAlg: ALG_ID;
  end;
  BLOBHEADER = _PUBLICKEYSTRUC;
  PUBLICKEYSTRUC = _PUBLICKEYSTRUC;

  _RSAPUBKEY = record
    magic: DWORD;                  // Has to be RSA1
    bitlen: DWORD;                 // # of bits in modulus
    pubexp: DWORD;                 // public exponent
                                   // Modulus data follows
  end;
  RSAPUBKEY = _RSAPUBKEY;

  _RSA_PUBLIC_KEY_XX = packed record
    _PublicKeyStruct: PUBLICKEYSTRUC;
    _RsaPubKey: RSAPUBKEY;
    RsaModulus: array [0..ANYSIZE_ARRAY-1] of Byte;
  end;
  RSA_PUBLIC_KEY_XX = _RSA_PUBLIC_KEY_XX;
  PRSA_PUBLIC_KEY_XX = ^_RSA_PUBLIC_KEY_XX;
  PCRSA_PUBLIC_KEY_XX = PRSA_PUBLIC_KEY_XX;

const
  SZ_ALG_MAX_SIZE = 64;

  SP_PROT_SSL2_CLIENT = $8;
  SP_PROT_SSL3_CLIENT = $20;
  SP_PROT_TLS1_CLIENT = $80;
  SP_PROT_TLS1_1_CLIENT = $200;
  SP_PROT_TLS1_2_CLIENT = $800;

  ALG_CLASS_DATA_ENCRYPT = 3 shl 13;
  ALG_TYPE_BLOCK = 3 shl 9;
  ALG_TYPE_STREAM = 4 shl 9;

  ALG_SID_3DES = 3;
  ALG_SID_AES_128 = 14;
  ALG_SID_AES_192 = 15;
  ALG_SID_AES_256 = 16;
  ALG_SID_DES = 1;
  ALG_SID_RC2 = 2;
  ALG_SID_RC4 = 1;

  CALG_3DES = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES;
  CALG_AES_128 = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_128;
  CALG_AES_192 = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_192;
  CALG_AES_256 = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_256;
  CALG_DES = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DES;
  CALG_RC2 = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC2;
  CALG_RC4 = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_RC4;

type
  SecPkgContext_ConnectionInfo = record
    dwProtocol: DWORD;
    aiCipher: ALG_ID;
    dwCipherStrength: DWORD;
    aiHash: ALG_ID;
    dwHashStrength: DWORD;
    aiExch: ALG_ID;
    dwExchStrength: DWORD;
  end;

  SecPkgContext_CipherInfo = record
    dwVersion: DWORD;
    dwProtocol: DWORD;
    dwCipherSuite: DWORD;
    dwBaseCipherSuite: DWORD;
    szCipherSuite: array [0 .. SZ_ALG_MAX_SIZE - 1] of WCHAR;
    szCipher: array [0 .. SZ_ALG_MAX_SIZE - 1] of WCHAR;
    dwCipherLen: DWORD;
    dwCipherBlockLen: DWORD;
    szHash: array [0 .. SZ_ALG_MAX_SIZE - 1] of WCHAR;
    dwHashLen: DWORD;
    szExchange: array [0 .. SZ_ALG_MAX_SIZE - 1] of WCHAR;
    dwMinExchangeLen: DWORD;
    dwMaxExchangeLen: DWORD;
    szCertificate: array [0 .. SZ_ALG_MAX_SIZE - 1] of WCHAR;
    dwKeyType: DWORD;
  end;

  WINHTTP_SECURITY_INFO = record
    ConnectionInfo: SecPkgContext_ConnectionInfo;
    CipherInfo: SecPkgContext_CipherInfo;
  end;
  TWinHttpSecurityInfo = WINHTTP_SECURITY_INFO;
  PWinHttpSecurityInfo = ^TWinHttpSecurityInfo;

const
  // Crypt UI API DLL
  CryptUI = 'cryptui.dll';

  // flags for dwDontUseColumn
  CRYPTUI_SELECT_ISSUEDTO_COLUMN       = $000000001;
  CRYPTUI_SELECT_ISSUEDBY_COLUMN       = $000000002;
  CRYPTUI_SELECT_INTENDEDUSE_COLUMN    = $000000004;
  CRYPTUI_SELECT_FRIENDLYNAME_COLUMN   = $000000008;
  CRYPTUI_SELECT_LOCATION_COLUMN       = $000000010;
  CRYPTUI_SELECT_EXPIRATION_COLUMN     = $000000020;

// -----------------------------------------------------------------------------
// CryptUIDlgSelectCertificateFromStore
// http://msdn.microsoft.com/en-us/library/aa380288(VS.85).aspx
//
// The CryptUIDlgSelectCertificateFromStore function displays a dialog box
// that allows the selection of a certificate from a specified store
// -----------------------------------------------------------------------------
type
  CryptUIDlgSelectCertificateFromStoreProc =
                                  function (hCertStore: HCERTSTORE;
                                            hwnd: HWND;
                                            pwszTitle: LPCWSTR;
                                            pwszDisplayString: LPCWSTR;
                                            dwDontUseColumn: DWORD;
                                            dwFlags: DWORD;
                                            pvReserved: Pointer): PCCERT_CONTEXT; stdcall;
var
  pCryptUIDlgSelectCertificateFromStore: CryptUIDlgSelectCertificateFromStoreProc;

{$WARN SYMBOL_PLATFORM OFF}

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall; external Crypt32 name 'CertEnumCertificatesInStore' delayed;
{$EXTERNALSYM CertEnumCertificatesInStore}

function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD; pvFindPara: Pointer;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall; external Crypt32 name 'CertFindCertificateInStore' delayed;
{$EXTERNALSYM CertFindCertificateInStore}

function CertFindChainInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD; pvFindPara: Pointer;
  pPrevChainContext: PCCERT_CHAIN_CONTEXT): PCCERT_CHAIN_CONTEXT; stdcall; external Crypt32 name 'CertFindChainInStore' delayed;
{$EXTERNALSYM CertFindChainInStore}

function CertOpenStore(lpszStoreProvider: LPCSTR; dwEncodingType: DWORD;
  hCryptProv: HCRYPTPROV; dwFlags: DWORD; pvPara: Pointer): HCERTSTORE; stdcall; external Crypt32 name 'CertOpenStore' delayed;
{$EXTERNALSYM CertOpenStore}

function CertOpenSystemStore(hProv: HCRYPTPROV; szSubsystemProtocol: LPCTSTR): HCERTSTORE; stdcall; external Crypt32 name 'CertOpenSystemStoreW' delayed;
{$EXTERNALSYM CertOpenSystemStore}

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall; external Crypt32 name 'CertCloseStore' delayed;
{$EXTERNALSYM CertCloseStore}

procedure CertFreeCertificateChain(pChainContext: PCCERT_CHAIN_CONTEXT); stdcall; external Crypt32 name 'CertFreeCertificateChain' delayed;
{$EXTERNALSYM CertFreeCertificateChain}

function CertDuplicateCertificateContext(pCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall; external Crypt32 name 'CertDuplicateCertificateContext' delayed;
{$EXTERNALSYM CertDuplicateCertificateContext}

function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall; external Crypt32 name 'CertFreeCertificateContext' delayed;
{$EXTERNALSYM CertFreeCertificateContext}

function CertNameToStr(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB; dwStrType: DWORD; psz: LPTSTR;
  csz: DWORD): DWORD; stdcall; external Crypt32 name 'CertNameToStrW' delayed;
{$EXTERNALSYM CertNameToStr}

function CertGetNameString(pCertContext: PCCERT_CONTEXT; dwType, dwFlags: DWORD;
  pvTypePara: Pointer; pszNameString: LPTSTR; cchNameString: DWORD): DWORD; stdcall; external Crypt32 name 'CertGetNameStringW' delayed;
{$EXTERNALSYM CertGetNameString}

function PFXImportCertStore(pPFX: PCRYPT_DATA_BLOB; szPassword: LPCWSTR;
  dwFlags: DWORD): HCERTSTORE; stdcall; external Crypt32 name 'PFXImportCertStore' delayed;
{$EXTERNALSYM PFXImportCertStore}

function CryptDecodeObject(dwCertEncodingType: DWORD; lpszStructType: LPCSTR; pbEncoded: PBYTE;
  cbEncoded: DWORD; dwFlags: DWORD; pvStructInfo: Pointer; var pcbStructInfo: DWORD): BOOL; stdcall; external Crypt32 name 'CryptDecodeObject' delayed;
{$EXTERNALSYM CryptDecodeObject}

function GetCertSerialNumber(Source: PCRYPT_INTEGER_BLOB): string; forward;
{$NODEFINE GetCertSerialNumber}

function GetCertPublicKey(CertContext: PCCERT_CONTEXT; Source: PCERT_PUBLIC_KEY_INFO): string; forward;
{$NODEFINE GetCertPublicKey}

function FindCertWithSerialNumber(AStore: HCERTSTORE;
                                  ASerialNumber: string): PCERT_CONTEXT; forward;
{$NODEFINE FindCertWithSerialNumber}

function GetCertInfo(Context: PCERT_CONTEXT;
                     InfoFlag: Integer = 0;
                     InfoType: Integer = CERT_NAME_SIMPLE_DISPLAY_TYPE): string; forward;
{$NODEFINE GetCertInfo}

function BlobToStr(AEncoding: DWORD; Blob: PCERT_NAME_BLOB): PWideChar; forward;
{$NODEFINE GetCertInfo}

procedure HttpCertWinInfoToTCertificate(const AWinCert: TWinHttpCertificateInfo; var ACertificate: TCertificate); forward;
{$NODEFINE GetCertInfo}

function GetKeySize(ACryptCert: PCCERT_CONTEXT): DWORD; forward;
{$NODEFINE GetCertInfo}

procedure CryptCertToHttpCert(ACryptCert: PCCERT_CONTEXT; var AHttpCert: TWinHttpCertificateInfo); forward;
{$NODEFINE GetCertInfo}

procedure CryptCertToTCertificate(ACryptCert: PCCERT_CONTEXT; var ACertificate: TCertificate); forward;
{$NODEFINE GetCertInfo}

function CryptUIDlgSelectCertificateFromStore(hCertStore: HCERTSTORE;
                                              hwnd: HWND;
                                              pwszTitle: LPCWSTR;
                                              pwszDisplayString: LPCWSTR;
                                              dwDontUseColumn: DWORD;
                                              dwFlags: DWORD;
                                              pvReserved: Pointer): PCCERT_CONTEXT; stdcall; forward;
{$NODEFINE CryptUIDlgSelectCertificateFromStore}

{$WARN SYMBOL_PLATFORM DEFAULT}

///////////////////////
///  main classes   ///
///////////////////////

type
  TWinHttpLib = class(TObject)
  private const
    winhttp = 'winhttp.dll';
  private class var
    FLock: TCriticalSection;
    FHandle: THandle;
    FSession: HINTERNET;
    FStore: HCERTSTORE;
    FProxyConfig: TWinHTTPCurrentUserIEProxyConfig;
    FProxyConfigResult: BOOL;
    FProxyConfigAcquired: Boolean;
  protected
    class constructor Create;
    class destructor Destroy;
    class procedure LockHandleGC;
    class function GetCertStore: HCERTSTORE;
    class function GetProxyConfig: PWinHttpCurrentUserIEProxyConfig;
  public
    class procedure Reset;
    class property Handle: THandle read FHandle;
  end;

  TWinHTTPClient = class(THTTPClient)
  private
    FWSession: HINTERNET;

    FCertificateList: TList<TCertificate>;
    FWinCertList: TList<PCCERT_CONTEXT>;

    function ChooseAuthScheme(SupportedSchemes: DWORD): DWORD;
    function HandleExecuteError(AErrorMsg: PResStringRec; const ARequest: THTTPRequest): TWinHTTPClient.TExecutionResult;
//    function ConvertAuthSchemeFromWin(AnAuthScheme: DWORD): TCredentialsStorage.TAuthSchemeType;
//    function ConvertAuthSchemeToWin(AnAuthScheme: TCredentialsStorage.TAuthSchemeType): DWORD;
  protected
    function DoSetCredential(AnAuthTargetType: TAuthTargetType; const ARequest: THTTPRequest;
      const ACredential: TCredentialsStorage.TCredential): Boolean; override;
    function DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
      const AContentStream: TStream): TWinHTTPClient.TExecutionResult; override;
    /// <summary>Returns a new response instance from the client.</summary>
    function DoGetResponseInstance(const AContext: TObject; const AProc: TProc;
      const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: IURLRequest; const AContentStream: TStream): IAsyncResult; override;

    function DoGetHTTPRequestInstance(const AClient: THTTPClient; const ARequestMethod: string;
      const AURI: TURI): IHTTPRequest; override;
    function DoProcessStatus(const ARequest: IHTTPRequest; const  AResponse: IHTTPResponse): Boolean; override;

    function DoGetSSLCertificateFromServer(const ARequest: THTTPRequest): TCertificate; override;
    procedure DoServerCertificateAccepted(const ARequest: THTTPRequest); override;

    procedure DoGetClientCertificates(const ARequest: THTTPRequest; const ACertificateList: TList<TCertificate>); override;
    function DoNoClientCertificate(const ARequest: THTTPRequest): Boolean; override;
    function DoClientCertificateAccepted(const ARequest: THTTPRequest; const AnIndex: Integer): Boolean; override;

    class function CreateInstance: TURLClient; override;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TWinHTTPResponse = class;

  TWinHTTPRequest = class(THTTPRequest)
  private
    FWConnect: HINTERNET;
    FWRequest: HINTERNET;
    FLastProxyAuthScheme: DWORD;
    FLastServerAuthScheme: DWORD;
    FResponseLink: TWinHTTPResponse;
    FHeaders: TNetHeaders;
    FServerCertificateValidated: Boolean;
    FServerCertificateAccepted: Boolean;

    procedure CreateHandles(const AURI: TURI);
    procedure UpdateRequest(const AURI: TURI);

    procedure SetWinHttpVersion;
    procedure SetWinProxySettings;
    procedure SetWinSecureProtocols;
    procedure SetWinDecompression;
    procedure SetWinLogonPolicy;
    procedure SetWinCertificate;
  protected
    function GetHeaders: TNetHeaders; override;

    function GetHeaderValue(const AName: string): string; override;
    procedure SetHeaderValue(const AName, AValue: string); override;

    /// <summary>Add a Header to the current request.</summary>
    /// <param name="AName: string">Name of the Header.</param>
    /// <param name="AValue: string">Value associted.</param>
    procedure AddHeader(const AName, AValue: string); override;

    /// <summary>Removes a Header from the request</summary>
    /// <param name="AName: string">Header to be removed.</param>
    function RemoveHeader(const AName: string): Boolean; override;

    procedure DoPrepare; override;
    procedure DoCancel; override;
    procedure DoResetCancel; override;

    /// <summary> Setter for the ConnectionTimeout property.</summary>
    procedure SetConnectionTimeout(const Value: Integer); override;
    /// <summary> Setter for the SendTimeout property.</summary>
    procedure SetSendTimeout(const Value: Integer); override;
    /// <summary> Setter for the ResponseTimeout property.</summary>
    procedure SetResponseTimeout(const Value: Integer); override;
  public
    constructor Create(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI);
    destructor Destroy; override;
  end;

  TWinHTTPResponse = class(THTTPResponse)
  private
    FWRequest: HINTERNET;
    FWConnect: HINTERNET;
    FRequestLink: TWinHTTPRequest;

    procedure UpdateHandles(AWConnect, AWRequest: HINTERNET);
    function GetIsCancelled: Boolean;
  protected
    function GetDecompressResponse: Boolean; override;
    procedure DoReadData(const AStream: TStream); override;

    {IURLResponse}
    function GetHeaders: TNetHeaders; override;

    {IHTTPResponse}
    function GetStatusCode: Integer; override;
    function GetStatusText: string; override;
    function GetVersion: THTTPProtocolVersion; override;

  public
    constructor Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback; const AAsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: TWinHTTPRequest; const AContentStream: TStream);
    destructor Destroy; override;
  end;

{ Crypt API helper functions }

function GetCertSerialNumber(Source: PCRYPT_INTEGER_BLOB): string;
begin
  SetLength(Result, Source.cbData * 2);
  BinToHex(Source.pbData, PChar(Result), Source.cbData);
  Result := SwapHexEndianness(Result);
end;

function GetCertPublicKey(CertContext: PCCERT_CONTEXT; Source: PCERT_PUBLIC_KEY_INFO): string;
var
  LSize: DWORD;
  LBytes: TBytes;
  LRsaPubKey: PRSA_PUBLIC_KEY_XX;
  LModulusSize: DWORD;
  // LExponent: DWORD;
  LMagic: DWORD;
begin
                                                                                              
  Result := '';
  if not CryptDecodeObject(CertContext^.dwCertEncodingType, RSA_CSP_PUBLICKEYBLOB,
      Source^.PublicKey.pbData, Source^.PublicKey.cbData, 0, nil, LSize) then
    Exit;
  SetLength(LBytes, LSize);
  if not CryptDecodeObject(CertContext^.dwCertEncodingType, RSA_CSP_PUBLICKEYBLOB,
      Source^.PublicKey.pbData, Source^.PublicKey.cbData, 0, @LBytes[0], LSize) then
    Exit;

  LRsaPubKey := PRSA_PUBLIC_KEY_XX(@LBytes[0]);
  LModulusSize := LRsaPubKey^._RsaPubKey.bitlen div 8;
  // LExponent := LRsaPubKey^._RsaPubKey.pubexp;
  LMagic := LRsaPubKey^._RsaPubKey.magic;
  if LMagic <> $31415352 then // RSA1
    Exit;

  SetLength(Result, LModulusSize * 2);
  BinToHex(@LRsaPubKey^.RsaModulus[0], PChar(Result), LModulusSize);
  Result := SwapHexEndianness(Result);
end;

function FindCertWithSerialNumber(AStore: HCERTSTORE;
                                  ASerialNumber: string): PCERT_CONTEXT;
var
  PrevContext, CurContext: PCERT_CONTEXT;
  CertInfo: String;
begin
  Result := nil;
  if AStore <> nil then
  begin
    PrevContext := nil;
    CurContext := CertEnumCertificatesInStore(AStore, PrevContext);
    while CurContext <> nil do
    begin
      CertInfo := GetCertSerialNumber(@CurContext^.pCertInfo^.SerialNumber);
      if SameText(CertInfo, ASerialNumber) then
      begin
        Result := CurContext;
        Exit;
      end;
      PrevContext := CurContext;
      CurContext := CertEnumCertificatesInStore(AStore, PrevContext);
    end;
  end;
end;

function GetCertInfo(Context: PCERT_CONTEXT;
                     InfoFlag: Integer = 0;
                     InfoType: Integer = CERT_NAME_SIMPLE_DISPLAY_TYPE): string;
var
  cbSize: DWORD;
begin
  Result := '';
  cbSize := CertGetNameString(Context, InfoType, InfoFlag, nil, nil, 0);
  if cbSize > 0 then
  begin
    SetLength(Result, cbSize-1);
    CertGetNameString(Context, InfoType, InfoFlag,
                      nil, PChar(Result), cbSize);
  end;
end;

function CryptUIDlgSelectCertificateFromStore(hCertStore: HCERTSTORE;
                                              hwnd: HWND;
                                              pwszTitle: LPCWSTR;
                                              pwszDisplayString: LPCWSTR;
                                              dwDontUseColumn: DWORD;
                                              dwFlags: DWORD;
                                              pvReserved: Pointer): PCCERT_CONTEXT; stdcall;
var
  AModule: HMODULE;
begin
  Result := nil;
  if not Assigned(pCryptUIDlgSelectCertificateFromStore) then
  begin
    AModule := GetModuleHandle(PChar(CryptUI));
    if AModule = 0 then
      AModule := LoadLibrary(PChar(CryptUI));
    if AModule <> 0 then
      pCryptUIDlgSelectCertificateFromStore := GetProcAddress(AModule, 'CryptUIDlgSelectCertificateFromStore');
  end;
  if Assigned(pCryptUIDlgSelectCertificateFromStore) then
    Result := pCryptUIDlgSelectCertificateFromStore(hCertStore, hwnd, pwszTitle,
                                                    pwszDisplayString, dwDontUseColumn,
                                                    dwFlags, pvReserved);
end;

function ShowSelectCertificateDialog(AParentWnd: UIntPtr;
  const ATitle, ADisplayString: string; var ACertificate): Boolean;
var
  Context: PCERT_CONTEXT;
  hStore: HCERTSTORE;
begin
  Result := False;
  { Could add support for other Stores }
  hStore := CertOpenSystemStore(0, PChar('MY'));
  if hStore <> nil then
  begin
    try
      Context := CryptUIDlgSelectCertificateFromStore(hStore,
                                                      AParentWnd,
                                                      PChar(ATitle),
                                                      PChar(ADisplayString),
                                                      CRYPTUI_SELECT_LOCATION_COLUMN or
                                                      CRYPTUI_SELECT_EXPIRATION_COLUMN,
                                                      0,
                                                      nil);
      if Context <> nil then
      begin
        CryptCertToTCertificate(Context, TCertificate(ACertificate));
        CertFreeCertificateContext(Context);
        Result := True;
      end;
    finally
      CertCloseStore(hStore, CERT_CLOSE_STORE_FORCE_FLAG);
    end;
  end;
end;

function BlobToStr(AEncoding: DWORD; Blob: PCERT_NAME_BLOB): PWideChar;
var
  LSize: DWORD;
  LFormat: DWORD;
begin
  LFormat := CERT_SIMPLE_NAME_STR or CERT_NAME_STR_CRLF_FLAG;
  LSize := CertNameToStr(AEncoding, Blob, LFormat, nil, 0);
  GetMem(Result, LSize * SizeOf(Char));
  CertNameToStr(AEncoding, Blob, LFormat, Result, LSize);
end;

procedure HttpCertWinInfoToTCertificate(const AWinCert: TWinHttpCertificateInfo; var ACertificate: TCertificate);
var
  LTmpTime: TSystemTime;
begin
  FileTimeToSystemTime(AWinCert.ftExpiry, LTmpTime);
  ACertificate.Expiry := SystemTimeToDateTime(LTmpTime);
  FileTimeToSystemTime(AWinCert.ftStart, LTmpTime);
  ACertificate.Start := SystemTimeToDateTime(LTmpTime);
  ACertificate.Subject := String(AWinCert.lpszSubjectInfo).Replace(sLineBreak, ';', [rfReplaceAll]);
  ACertificate.Issuer := String(AWinCert.lpszIssuerInfo).Replace(sLineBreak, ';', [rfReplaceAll]);
  ACertificate.ProtocolName := AWinCert.lpszProtocolName;
  ACertificate.AlgSignature := AWinCert.lpszSignatureAlgName;
  ACertificate.AlgEncryption := AWinCert.lpszEncryptionAlgName;
  ACertificate.KeySize := AWinCert.dwKeySize;
end;

function GetKeySize(ACryptCert: PCCERT_CONTEXT): DWORD;
begin
                                                      
  Result := 0;
end;

procedure CryptCertToHttpCert(ACryptCert: PCCERT_CONTEXT; var AHttpCert: TWinHttpCertificateInfo);
begin
// lpszSubjectInfo & lpszIssuerInfo need to be released using FreeMem
  AHttpCert.ftExpiry := ACryptCert.pCertInfo.NotAfter;
  AHttpCert.ftStart := ACryptCert.pCertInfo.NotBefore;
  AHttpCert.lpszSubjectInfo := BlobToStr(ACryptCert.dwCertEncodingType, @ACryptCert.pCertInfo.Subject);
  AHttpCert.lpszIssuerInfo := BlobToStr(ACryptCert.dwCertEncodingType, @ACryptCert.pCertInfo.Issuer);
  AHttpCert.lpszProtocolName := nil;
  AHttpCert.lpszSignatureAlgName := nil;
  AHttpCert.lpszEncryptionAlgName := nil;
  AHttpCert.dwKeySize := GetKeySize(ACryptCert);
end;

procedure CryptCertToTCertificate(ACryptCert: PCCERT_CONTEXT; var ACertificate: TCertificate);
var
  LHttpCert: TWinHttpCertificateInfo;
begin
  CryptCertToHttpCert(ACryptCert, LHttpCert);
  HttpCertWinInfoToTCertificate(LHttpCert, ACertificate);
  ACertificate.SerialNum := GetCertSerialNumber(@ACryptCert^.pCertInfo^.SerialNumber);
  ACertificate.CertName := GetCertInfo(ACryptCert);
  FreeMem(LHttpCert.lpszSubjectInfo);
  FreeMem(LHttpCert.lpszIssuerInfo);
end;

{ Helper functions }

procedure StringToHeaders(const Text: string; var AHeadersArray: TNetHeaders; const AResponse: TWinHTTPResponse);
var
  LLines: TArray<string>;
  LLine: string;
  LHeader: string;
  LValue: string;
  J, K: Integer;
  LFound: Boolean;
  LPos: Integer;

  function CheckMethod(const ALine, AMethod: string): boolean;
  begin
    Result := (StrLIComp(PChar(ALine), PChar(AMethod), AMethod.Length) = 0) and
      (ALine.Length > AMethod.Length) and (ALine.Chars[AMethod.Length] = ' ');
  end;

begin
  LLines := Text.Split([#13#10], TStringSplitOptions.ExcludeEmpty);
  SetLength(AHeadersArray, Length(LLines));
  J := 0;
  for LLine in LLines do
  begin
    // Skip "GET /url/?a=: HTTP/1.1"
    if CheckMethod(LLine, sHTTPMethodGet) or
       CheckMethod(LLine, sHTTPMethodPost) or
       CheckMethod(LLine, sHTTPMethodPut) or
       CheckMethod(LLine, sHTTPMethodDelete) or
       CheckMethod(LLine, sHTTPMethodHead) or
       CheckMethod(LLine, sHTTPMethodConnect) or
       CheckMethod(LLine, sHTTPMethodOptions) or
       CheckMethod(LLine, sHTTPMethodTrace) or
       CheckMethod(LLine, sHTTPMethodMerge) or
       CheckMethod(LLine, sHTTPMethodPatch) then
      Continue;

    LPos := LLine.IndexOf(':');
    if LPos > 0 then
    begin
      LHeader := LLine.Substring(0, LPos).Trim;
      LValue := LLine.Substring(LPos + 1).Trim;

      if (AResponse <> nil) and SameText(LHeader, sSetCookie) then
         AResponse.InternalAddCookie(LValue)
      else
      begin
        LFound := False;
        for K := 0 to J - 1 do
          if string.CompareText(AHeadersArray[K].Name, LHeader) = 0 then
          begin
            AHeadersArray[K].Value := AHeadersArray[K].Value + ', ' + LValue;
            LFound := True;
          end;
        if not LFound then
        begin
          AHeadersArray[J].Create(LHeader, LValue);
          Inc(J);
        end;
      end;
    end;
  end;
  SetLength(AHeadersArray, J);
end;

function ObjectName(const AURI: TURI): string;
begin
  Result := AURI.Path;
  if AURI.Query <> '' then
    Result := Result + '?' + AURI.Query;
end;

function ReadHeader(ARequest: HINTERNET; AHeaderFlag: DWORD; const AHeaderName: string = ''): string;
var
  LSize: Cardinal;
  LFlags: DWORD;
  LHeaderName: PWideChar;
begin
  LFLags := AHeaderFlag;
  if AHeaderName <> '' then
  begin
    LFLags := LFLags or WINHTTP_QUERY_CUSTOM;
    LHeaderName := PWideChar(AHeaderName);
  end
  else
    LHeaderName := WINHTTP_HEADER_NAME_BY_INDEX;

  LSize := 0;
  WinHttpQueryHeaders(ARequest, LFLags, LHeaderName, nil, LSize, WINHTTP_NO_HEADER_INDEX);

  if GetLastError = ERROR_WINHTTP_HEADER_NOT_FOUND then
    Result := ''
  else
  begin
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      raise ENetHTTPException.CreateResFmt(@SNetHttpHeadersError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);

    SetLength(Result, LSize div SizeOf(Char) - 1);
    if Length(Result) > 0 then
      if WinHttpQueryHeaders(ARequest, LFLags, LHeaderName, PChar(Result), LSize, WINHTTP_NO_HEADER_INDEX) = False then
        raise ENetHTTPException.CreateResFmt(@SNetHttpHeadersError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
  end;
end;

{ TWinHTTPClient }

class function TWinHTTPClient.CreateInstance: TURLClient;
begin
  Result := TWinHTTPClient.Create;
end;

//function TWinHTTPClient.ConvertAuthSchemeFromWin(AnAuthScheme: DWORD): TCredentialsStorage.TAuthSchemeType;
//begin
//  case AnAuthScheme of
//    WINHTTP_AUTH_SCHEME_NEGOTIATE:
//      Result := TCredentialsStorage.TAuthSchemeType.Negotiate;
//    WINHTTP_AUTH_SCHEME_NTLM:
//      Result := TCredentialsStorage.TAuthSchemeType.NTLM;
//    WINHTTP_AUTH_SCHEME_DIGEST:
//      Result := TCredentialsStorage.TAuthSchemeType.Digest;
//    WINHTTP_AUTH_SCHEME_BASIC:
//      Result := TCredentialsStorage.TAuthSchemeType.Basic;
//    else
//      raise ENetCredentialException.Create('Authentication Scheme not supported');
//  end;
//end;
//
//function TWinHTTPClient.ConvertAuthSchemeToWin(AnAuthScheme: TCredentialsStorage.TAuthSchemeType): DWORD;
//begin
//  case AnAuthScheme of
//    TCredentialsStorage.TAuthSchemeType.Negotiate:
//      Result := WINHTTP_AUTH_SCHEME_NEGOTIATE;
//    TCredentialsStorage.TAuthSchemeType.NTLM:
//      Result := WINHTTP_AUTH_SCHEME_NTLM;
//    TCredentialsStorage.TAuthSchemeType.Digest:
//      Result := WINHTTP_AUTH_SCHEME_DIGEST;
//    TCredentialsStorage.TAuthSchemeType.Basic:
//      Result := WINHTTP_AUTH_SCHEME_BASIC;
//    else
//      raise ENetCredentialException.Create('Authentication Scheme not supported');
//  end;
//end;

procedure HTTPCallback(hInternet: HINTERNET; dwContext: Pointer; dwInternetStatus: DWORD;
  lpvStatusInformation: Pointer; dwStatusInformationLength: DWORD); stdcall;
var
  LRequest: TWinHTTPRequest;
  LClient: TWinHTTPClient;
  LInfo: DWORD;
  LReasons: THTTPSecureFailureReasons;
  LCertificate: TCertificate;
  LAccepted: Boolean;
begin
  LRequest := TWinHTTPRequest(dwContext);
  if LRequest = nil then
    Exit;
  LClient := TWinHTTPClient(LRequest.FClient);
  case dwInternetStatus of
  WINHTTP_CALLBACK_STATUS_SECURE_FAILURE:
    begin
      LReasons := [];
      LInfo := PDWORD(lpvStatusInformation)^;
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED <> 0 then
        Include(LReasons, THTTPSecureFailureReason.CertRevFailed);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT <> 0 then
        Include(LReasons, THTTPSecureFailureReason.InvalidCert);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED <> 0 then
        Include(LReasons, THTTPSecureFailureReason.CertRevoked);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA <> 0 then
        Include(LReasons, THTTPSecureFailureReason.InvalidCA);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID <> 0 then
        Include(LReasons, THTTPSecureFailureReason.CertCNInvalid);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID <> 0 then
        Include(LReasons, THTTPSecureFailureReason.CertDateInvalid);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE <> 0 then
        Include(LReasons, THTTPSecureFailureReason.CertWrongUsage);
      if LInfo and WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR <> 0 then
        Include(LReasons, THTTPSecureFailureReason.SecurityChannelError);
      LClient.FSecureFailureReasons := LReasons;
    end;
  WINHTTP_CALLBACK_STATUS_SENDING_REQUEST:
    if (LRequest.FURL.Scheme = TURI.SCHEME_HTTPS) and
       (LClient.FSecureFailureReasons = []) and not LRequest.FServerCertificateValidated and
       (Assigned(LClient.FValidateServerCertificateCallback) or
        Assigned(LClient.FValidateServerCertificateEvent)) then
    begin
      LRequest.FServerCertificateValidated := True;
      LAccepted := True;
      LCertificate := LClient.DoGetSSLCertificateFromServer(LRequest);
      if Assigned(LClient.FValidateServerCertificateCallback) then
        LClient.FValidateServerCertificateCallback(LClient, LRequest, LCertificate, LAccepted)
      else
        LClient.FValidateServerCertificateEvent(LClient, LRequest, LCertificate, LAccepted);
      if not LAccepted then
      begin
        LClient.FSecureFailureReasons := [THTTPSecureFailureReason.CertNotAccepted];
        WinHttpCloseHandle(hInternet);
      end;
    end;
  end;
end;

constructor TWinHTTPClient.Create;
begin
  inherited Initializer;

  FWinCertList := TList<PCCERT_CONTEXT>.Create;
  FCertificateList := TList<TCertificate>.Create;

  TWinHttpLib.LockHandleGC;
  FWSession := WinHttpOpen('', WINHTTP_ACCESS_TYPE_NO_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  if FWSession = nil then
    raise ENetHTTPClientException.CreateRes(@SNetHttpClientHandleError);

  WinHttpSetStatusCallback(FWSession, HTTPCallback, WINHTTP_CALLBACK_FLAG_SECURE_FAILURE or
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST, 0);
end;

destructor TWinHTTPClient.Destroy;
var
  I: Integer;
begin
  for I := 0 to FWinCertList.Count - 1 do
    CertFreeCertificateContext(FWinCertList[I]);
  FWinCertList.Free;
  FCertificateList.Free;
  if FWSession <> nil then
    WinHttpCloseHandle(FWSession);
  inherited;
end;

function TWinHTTPClient.DoClientCertificateAccepted(const ARequest: THTTPRequest; const AnIndex: Integer): Boolean;
var
  LRequest: TWinHTTPRequest;
begin
  inherited;
  LRequest := TWinHTTPRequest(ARequest);
  Result := WinHttpSetOption(LRequest.FWRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, FWinCertList[AnIndex], SizeOf(CERT_CONTEXT) );
end;

function TWinHTTPClient.HandleExecuteError(AErrorMsg: PResStringRec; const ARequest: THTTPRequest): TWinHTTPClient.TExecutionResult;
var
  LastError: Cardinal;
begin
  LastError := GetLastError;
  case LastError of
    ERROR_WINHTTP_SECURE_FAILURE:
      if (SecureFailureReasons <> [THTTPSecureFailureReason.SecurityChannelError]) and
         not TWinHTTPRequest(ARequest).FServerCertificateAccepted then
        Exit(TExecutionResult.ServerCertificateInvalid)
      else
        raise ENetHTTPClientException.CreateResFmt(AErrorMsg, [LastError, SysErrorMessage(LastError, TWinHttpLib.Handle)]);
    ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:
      Exit(TExecutionResult.ClientCertificateNeeded);
    ERROR_WINHTTP_RESEND_REQUEST:
      Exit(TExecutionResult.Retry);
    else
      if (LastError = ERROR_WINHTTP_OPERATION_CANCELLED) and
         (SecureFailureReasons = [THTTPSecureFailureReason.CertNotAccepted]) then
        raise ENetHTTPCertificateException.CreateRes(@SNetHttpServerCertificateNotAccepted)
      else
      if (LastError = ERROR_WINHTTP_OPERATION_CANCELLED) or TWinHTTPRequest(ARequest).FCancelled then
        Exit(TExecutionResult.Success)
      else
        raise ENetHTTPClientException.CreateResFmt(AErrorMsg, [LastError, SysErrorMessage(LastError, TWinHttpLib.Handle)]);
  end;
end;

function TWinHTTPClient.DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
  const AContentStream: TStream): TWinHTTPClient.TExecutionResult;
var
  LBuffer: TArray<System.Byte>;
  LBytesWritten: DWORD;
  LRequest: TWinHTTPRequest;
  LDataLength: Int64;
  LWritten: Int64;
  LToWrite: LongInt;
  LOptionValue: DWORD;
  LHeader: TNetHeader;
  LAbort: Boolean;
const
  BUFFERSIZE = 64 * 1024;  // Usual TCP Window Size
begin
  TMonitor.Enter(ARequest);
  try
    Result := TExecutionResult.Success;
    LRequest := TWinHTTPRequest(ARequest);
    if LRequest.FCancelled then
      Exit;

    if LRequest.FHeaders = nil then
      LRequest.FHeaders := LRequest.GetHeaders
    else
    begin
      for LHeader in LRequest.FHeaders do
        if LRequest.GetHeaderValue(LHeader.Name) = '' then
          LRequest.AddHeader(LHeader.Name, LHeader.Value);
    end;

    LDataLength := 0;
    if LRequest.FSourceStream <> nil then
      LDataLength := LRequest.FSourceStream.Size - LRequest.FSourceStream.Position;

    // Set callback context to LRequest
    WinHttpSetOption(LRequest.FWRequest, WINHTTP_OPTION_CONTEXT_VALUE, @LRequest, SizeOf(Pointer));

    // Disable automatic redirects
    LOptionValue := WINHTTP_DISABLE_REDIRECTS;
    WinHttpSetOption(LRequest.FWRequest, WINHTTP_OPTION_DISABLE_FEATURE, @LOptionValue, sizeof(LOptionValue));

    // Disable automatic addition of cookie headers to requests, it's done by framework
    LOptionValue := WINHTTP_DISABLE_COOKIES;
    WinHttpSetOption(LRequest.FWRequest, WINHTTP_OPTION_DISABLE_FEATURE, @LOptionValue, sizeof(LOptionValue));
  finally
    TMonitor.Exit(ARequest);
  end;

  // Send Request
  if not WinHttpSendRequest(LRequest.FWRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, LDataLength, 0) then
    Exit(HandleExecuteError(@SNetHttpClientSendError, ARequest));

  // Send data
  if LDataLength > 0 then
  begin
    SetLength(LBuffer, BUFFERSIZE);
    LWritten := 0;
    LRequest.DoSendDataProgress(LDataLength, LWritten, LAbort, True);
    while not LAbort and (LWritten < LDataLength) do
    begin
      LToWrite := BUFFERSIZE;
      if LDataLength - LWritten < LToWrite then
        LToWrite := LDataLength - LWritten;
      LRequest.FSourceStream.ReadBuffer(LBuffer, LToWrite);
      // Write data to the server.
      if not WinHttpWriteData(LRequest.FWRequest, LBuffer[0], LToWrite, @LBytesWritten) then
        Exit(HandleExecuteError(@SNetHttpClientSendError, ARequest));
      if LongInt(LBytesWritten) < LToWrite then
        LRequest.FSourceStream.Position := LRequest.FSourceStream.Position - (LToWrite - LongInt(LBytesWritten));
      LWritten := LWritten + LBytesWritten;
      LRequest.DoSendDataProgress(LDataLength, LWritten, LAbort, True);
    end;
  end;
  if LRequest.FCancelled then
    Exit;

  // Wait to receive response
  if not WinHttpReceiveResponse(LRequest.FWRequest, nil) then
    Exit(HandleExecuteError(@SNetHttpClientReceiveError, ARequest));

  SetLength(TWinHTTPResponse(AResponse).FHeaders, 0); // Reset response headers
  LRequest.FResponseLink := TWinHTTPResponse(AResponse);
end;

procedure TWinHTTPClient.DoGetClientCertificates(const ARequest: THTTPRequest;
  const ACertificateList: TList<TCertificate>);
var
  LRequest: TWinHTTPRequest;
  LStore: HCERTSTORE;
  LIssuerList: PSecPkgContext_IssuerListInfoEx;
  LClientCert: PCCERT_CONTEXT;
  LSearchCriteria: CERT_CHAIN_FIND_BY_ISSUER_PARA;
  LIssuerListSize: DWORD;
  LPrevChainContext, LClientCertChain: PCCERT_CHAIN_CONTEXT;

  procedure AddToCertificateList(const AClientCert: PCCERT_CONTEXT);
  var
    LCertificate: TCertificate;
  begin
    CertDuplicateCertificateContext(AClientCert); // Need to be released (CertFreeCertificateContext)
    CryptCertToTCertificate(AClientCert, LCertificate);
    FCertificateList.Add(LCertificate);
    FWinCertList.Add(AClientCert);
  end;
begin
  inherited;

  if FWinCertList.Count = 0 then
  begin
    LRequest := TWinHTTPRequest(ARequest);

    LIssuerList := nil;
    LIssuerListSize := SizeOf(LIssuerList);
    LStore := TWinHttpLib.GetCertStore;

    if WinHttpQueryOption(LRequest.FWRequest, WINHTTP_OPTION_CLIENT_CERT_ISSUER_LIST, LIssuerList, LIssuerListSize) and (LIssuerList <> nil) then
    begin
      FillChar(LSearchCriteria, SizeOf(LSearchCriteria), 0);
      LSearchCriteria.cbSize := SizeOf(LSearchCriteria);
      LSearchCriteria.cIssuer := LIssuerList.cIssuers;
      LSearchCriteria.rgIssuer := LIssuerList.aIssuers;

      if LStore <> nil then
      begin
        LPrevChainContext := nil;
        while True do
        begin
          LClientCertChain := CertFindChainInStore(LStore, X509_ASN_ENCODING,
            CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG or CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG,
            CERT_CHAIN_FIND_BY_ISSUER, @LSearchCriteria, LPrevChainContext);

          if LClientCertChain <> nil then
          begin
            LPrevChainContext := LClientCertChain;
            LClientCert := LClientCertChain.rgpChain^.rgpElement^.pCertContext;
            AddToCertificateList(LClientCert);
          end else
            Break;
        end;
      end;
      GlobalFree(HGLOBAL(LIssuerList));
    end else
    begin
      if LStore <> nil then
      begin
        LClientCert := nil;
        while True do
        begin
          LClientCert := CertFindCertificateInStore(LStore,
            X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
            0, CERT_FIND_ANY, nil, LClientCert);
          if LClientCert <> nil then
            AddToCertificateList(LClientCert)
          else
            Break;
        end;
      end;
    end;
  end;
  ACertificateList.Clear;
  ACertificateList.AddRange(FCertificateList);
end;

function TWinHTTPClient.DoGetHTTPRequestInstance(const AClient: THTTPClient; const ARequestMethod: string;
  const AURI: TURI): IHTTPRequest;
begin
  Result := TWinHTTPRequest.Create(TWinHTTPClient(AClient), ARequestMethod, AURI);
end;

function TWinHTTPClient.DoGetResponseInstance(const AContext: TObject; const AProc: TProc;
  const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent;
  const ARequest: IURLRequest; const AContentStream: TStream): IAsyncResult;
begin
  Result := TWinHTTPResponse.Create(AContext, AProc, AsyncCallback, AsyncCallbackEvent, ARequest as TWinHttpRequest, AContentStream);
end;

function TWinHTTPClient.DoGetSSLCertificateFromServer(const ARequest: THTTPRequest): TCertificate;
var
  LRequest: TWinHTTPRequest;
  LCert: TWinHttpCertificateInfo;
  LCertContext: PCCERT_CONTEXT;
  LSecInfo: TWinHttpSecurityInfo;
  LSize: DWORD;
  s: string;
begin
  Result := Default(TCertificate);
  LRequest := TWinHTTPRequest(ARequest);
  LSize := SizeOf(LCert);
  if WinHttpQueryOption(LRequest.FWRequest, WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT, LCert, LSize) then
    try
      HttpCertWinInfoToTCertificate(LCert, Result);
    finally
      LocalFree(HLOCAL(LCert.lpszSubjectInfo));
      LocalFree(HLOCAL(LCert.lpszIssuerInfo));
    end;
  LSize := SizeOf(LCertContext);
  if WinHttpQueryOption(LRequest.FWRequest, WINHTTP_OPTION_SERVER_CERT_CONTEXT, LCertContext, LSize) then
    try
      Result.SerialNum := GetCertSerialNumber(@LCertContext^.pCertInfo^.SerialNumber);
      Result.PublicKey := GetCertPublicKey(LCertContext, @LCertContext^.pCertInfo^.SubjectPublicKeyInfo);
      if Result.KeySize = 0 then
        Result.KeySize := Length(Result.PublicKey) div 2;
      Result.CertName := GetCertInfo(LCertContext);
      s := string(AnsiString(LCertContext^.pCertInfo^.SignatureAlgorithm.pszObjId));
      // https://oidref.com/1.2.840.113549.1.1.11
      if SameStr(s, '1.2.840.113549.1.1.11') then
        Result.AlgSignature := 'sha256WithRSAEncryption'
      else
        Result.AlgSignature := s;
    finally
      CertFreeCertificateContext(LCertContext);
    end;
  LSize := SizeOf(LSecInfo);
  if WinHttpQueryOption(LRequest.FWRequest, WINHTTP_OPTION_SECURITY_INFO, LSecInfo, LSize) then
  begin
    // https://docs.microsoft.com/en-us/windows/win32/api/winhttp/ns-winhttp-winhttp_security_info
    case LSecInfo.ConnectionInfo.dwProtocol of
    SP_PROT_SSL2_CLIENT: Result.ProtocolName := 'SSL2';
    SP_PROT_SSL3_CLIENT: Result.ProtocolName := 'SSL3';
    SP_PROT_TLS1_CLIENT: Result.ProtocolName := 'TLS1';
    SP_PROT_TLS1_1_CLIENT: Result.ProtocolName := 'TLS1_1';
    SP_PROT_TLS1_2_CLIENT: Result.ProtocolName := 'TLS1_2';
    0: ;
    else Result.ProtocolName := '<other> ' + LSecInfo.ConnectionInfo.dwProtocol.ToString;
    end;
    case LSecInfo.ConnectionInfo.aiCipher of
    CALG_3DES: Result.AlgEncryption := '3DES';
    CALG_AES_128: Result.AlgEncryption := 'AES_128';
    CALG_AES_192: Result.AlgEncryption := 'AES_192';
    CALG_AES_256: Result.AlgEncryption := 'AES_256';
    CALG_DES: Result.AlgEncryption := 'DES';
    CALG_RC2: Result.AlgEncryption := 'RC2';
    CALG_RC4: Result.AlgEncryption := 'RC4';
    0: Result.AlgEncryption := '<unencrypted>';
    else Result.AlgEncryption := '<other> ' + LSecInfo.ConnectionInfo.aiCipher.ToString;
    end;
  end;
end;

function TWinHTTPClient.DoProcessStatus(const ARequest: IHTTPRequest; const AResponse: IHTTPResponse): Boolean;
var
  LRequest: TWinHTTPRequest;
  LResponse: TWinHTTPResponse;
  LURI: TURI;
begin
  LRequest := ARequest as TWinHTTPRequest;
  LResponse := AResponse as TWinHTTPResponse;
  // If the result is true then the while ends
  Result := True;
  if IsAutoRedirect(LResponse) then
  begin
    LURI := ComposeRedirectURL(LRequest, LResponse);
    if IsAutoRedirectWithGET(LRequest, LResponse) then
    begin
      LRequest.FMethodString := sHTTPMethodGet; // Change to GET
      LRequest.FSourceStream := nil;            // Dont send any data
      LRequest.RemoveHeader(sContentLength);
      LRequest.RemoveHeader(sContentType);      // Dont set content type
    end;

    LRequest.UpdateRequest(LURI);
    LResponse.UpdateHandles(LRequest.FWConnect, LRequest.FWRequest);
    Result := False;
  end;
end;

function TWinHTTPClient.DoNoClientCertificate(
  const ARequest: THTTPRequest): Boolean;
var
  LRequest: TWinHTTPRequest;
begin
  LRequest := ARequest as TWinHTTPRequest;
  Result := WinHttpSetOption(LRequest.FWRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
    WINHTTP_NO_CLIENT_CERT_CONTEXT, 0);
end;

procedure TWinHTTPClient.DoServerCertificateAccepted(const ARequest: THTTPRequest);
var
  LFlags: DWORD;
begin
  inherited;
  LFlags := SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE or
    SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;

  WinHttpSetOption(TWinHTTPRequest(ARequest).FWRequest, WINHTTP_OPTION_SECURITY_FLAGS, @LFlags, SizeOf(LFlags));
  TWinHTTPRequest(ARequest).FServerCertificateValidated := True;
  TWinHTTPRequest(ARequest).FServerCertificateAccepted := True;
end;

function TWinHTTPClient.DoSetCredential(AnAuthTargetType: TAuthTargetType; const ARequest: THTTPRequest;
  const ACredential: TCredentialsStorage.TCredential): Boolean;
var
  AuthTarget: DWORD;
  Target: DWORD;
  FirstScheme: DWORD;
  SupportedSchemes: DWORD;
  AuthScheme: DWORD;
  LRequest: TWinHTTPRequest;
  LHeader: string;
  LList: THeaderValueList;
  LUseCustom: Boolean;
begin
  LRequest := TWinHTTPRequest(ARequest);
  if AnAuthTargetType = TAuthTargetType.Server then
  begin
    AuthTarget := WINHTTP_AUTH_TARGET_SERVER;
    AuthScheme := LRequest.FLastServerAuthScheme;
  end
  else
  begin
    AuthTarget := WINHTTP_AUTH_TARGET_PROXY;
    AuthScheme := LRequest.FLastProxyAuthScheme;
  end;

  if AuthScheme = 0 then // we haven't a previous scheme
  begin
    if WinHttpQueryAuthSchemes(TWinHTTPRequest(ARequest).FWRequest, SupportedSchemes, FirstScheme, Target) then
    begin
      AuthScheme := ChooseAuthScheme(SupportedSchemes);
      if AnAuthTargetType = TAuthTargetType.Server then
        LRequest.FLastServerAuthScheme := AuthScheme
      else
        LRequest.FLastProxyAuthScheme := AuthScheme;
    end
    else
      AuthScheme := WINHTTP_AUTH_SCHEME_BASIC;
  end;

  // 1) WinHTTP uses Win-1252 to encode username:password at least for Basic authentication.
  // So, when server requests UTF-8 encoding for user credentials we will use custom
  // Authorization header with UTF-8 encoded username:password.
  // 2) Above works only for servers, not for proxies.
  LUseCustom := (AnAuthTargetType = TAuthTargetType.Server) and
    (AuthScheme = WINHTTP_AUTH_SCHEME_BASIC) and (ACredential.UserName <> '');
  if LUseCustom and (TWinHTTPRequest(ARequest).FResponseLink <> nil) then
  begin
    if AnAuthTargetType = TAuthTargetType.Server then
      LHeader := sWWWAuthenticate
    else
      LHeader := sProxyAuthenticate;
    LList := THeaderValueList.Create(TWinHTTPRequest(ARequest).FResponseLink.GetHeaderValue(LHeader));
    try
      LUseCustom := SameText(LList.Value['charset'], 'UTF-8'); // Do not translate
    finally
      LList.Free;
    end;
  end;

  if LUseCustom then
  begin
    if AnAuthTargetType = TAuthTargetType.Server then
      LHeader := sAuthorization
    else
      LHeader := sProxyAuthorization;
    TWinHTTPRequest(ARequest).SetHeaderValue(LHeader,
      'Basic ' + TNetEncoding.Base64String.Encode(ACredential.UserName + ':' + ACredential.Password)); // Do not translate
    Result := True;
  end
  else
    Result := WinHttpSetCredentials(TWinHTTPRequest(ARequest).FWRequest, AuthTarget, AuthScheme,
      PWideChar(ACredential.UserName), PWideChar(ACredential.Password), nil);
end;

function TWinHTTPClient.ChooseAuthScheme(SupportedSchemes: DWORD): DWORD;
begin
  if (SupportedSchemes and WINHTTP_AUTH_SCHEME_NEGOTIATE) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_NEGOTIATE
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_NTLM) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_NTLM
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_PASSPORT) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_PASSPORT
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_DIGEST) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_DIGEST
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_BASIC) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_BASIC
  else
    Result := 0;
end;

{ TWinHTTPRequest }

constructor TWinHTTPRequest.Create(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI);
begin
  inherited Create(AClient, ARequestMethod, AURI);

  if FResponseLink = nil then
    CreateHandles(AURI);
end;

procedure TWinHTTPRequest.CreateHandles(const AURI: TURI);
var
  LFlags: DWORD;
  LPort: INTERNET_PORT;
begin
  FServerCertificateAccepted := False;
  FServerCertificateValidated := False;
  if AURI.Port > 0 then
    LPort := AURI.Port
  else
    LPort := INTERNET_DEFAULT_PORT;
  FWConnect := WinHttpConnect(TWinHTTPClient(FClient).FWSession, PWideChar(AURI.Host), LPort, 0);
  if FWConnect = nil then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestConnectError, [AURI.Host]);

  LFlags := 0;
  if AURI.Scheme = TURI.SCHEME_HTTPS then
    LFlags := LFlags or WINHTTP_FLAG_SECURE;

  FWRequest := WinHttpOpenRequest( FWConnect, PWideChar(GetMethodString), PWideChar(ObjectName(AURI)), nil,
    WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, LFlags);
  if FWRequest = nil then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestOpenError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);

  if WinHttpSetTimeouts(FWRequest, ConnectionTimeout, ConnectionTimeout, SendTimeout, ResponseTimeout) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestSetTimeoutError, [GetLastError,
      SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
end;

destructor TWinHTTPRequest.Destroy;
begin
  if FResponseLink = nil then
  begin
    if FWRequest <> nil then
      WinHttpCloseHandle(FWRequest);
    if FWConnect <> nil then
      WinHttpCloseHandle(FWConnect);
  end
  else
    FResponseLink.FRequestLink := nil;
  inherited;
end;

procedure TWinHTTPRequest.AddHeader(const AName, AValue: string);
begin
  inherited;
  if WinHttpAddRequestHeaders(FWRequest, PWideChar(AName+': '+ AValue), $ffffffff,
    WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestAddHeaderError, [GetLastError, SysErrorMessage(GetLastError), TWinHttpLib.Handle]);
end;

function TWinHTTPRequest.GetHeaders: TNetHeaders;
begin
  StringToHeaders(ReadHeader(FWRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF or WINHTTP_QUERY_FLAG_REQUEST_HEADERS), Result, nil);
end;

function TWinHTTPRequest.GetHeaderValue(const AName: string): string;
begin
  Result := ReadHeader(FWRequest, WINHTTP_QUERY_FLAG_REQUEST_HEADERS, AName);
end;

procedure TWinHTTPRequest.DoPrepare;
begin
  inherited;
  SetWinHttpVersion;
  SetWinProxySettings;
  SetWinSecureProtocols;
  SetWinDecompression;
  SetWinLogonPolicy;
  SetWinCertificate;
end;

procedure TWinHTTPRequest.DoCancel;
var
  LReq: HINTERNET;
begin
  TMonitor.Enter(Self);
  try
    if FWRequest <> nil then
    begin
      LReq := FWRequest;
      FWRequest := nil;
      WinHttpCloseHandle(LReq);
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWinHTTPRequest.DoResetCancel;
begin
  inherited DoResetCancel;
  UpdateRequest(FURL);
end;

function TWinHTTPRequest.RemoveHeader(const AName: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  if GetHeaderValue(AName) = '' then
    Result := False
  else if WinHttpAddRequestHeaders(FWRequest, PWideChar(AName+':'), $ffffffff, WINHTTP_ADDREQ_FLAG_REPLACE) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestRemoveHeaderError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
  for i := 0 to Length(FHeaders) - 1 do
    if SameText(FHeaders[i].Name, AName) then
    begin
      Delete(FHeaders, i, 1);
      Break;
    end;
end;

procedure TWinHTTPRequest.SetConnectionTimeout(const Value: Integer);
begin
  inherited;
  if WinHttpSetTimeouts(FWRequest, Value, Value, SendTimeout, ResponseTimeout) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestSetTimeoutError, [GetLastError,
      SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
end;

procedure TWinHTTPRequest.SetHeaderValue(const AName, AValue: string);
var
  i: Integer;
begin
  // Add or replace a header value
  if WinHttpAddRequestHeaders(FWRequest, PWideChar(AName+': '+ AValue), $ffffffff, WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestAddHeaderError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
  for i := 0 to Length(FHeaders) - 1 do
    if SameText(FHeaders[i].Name, AName) then
    begin
      FHeaders[i].Value := AValue;
      Break;
    end;
end;

procedure TWinHTTPRequest.SetResponseTimeout(const Value: Integer);
begin
  inherited;
  if WinHttpSetTimeouts(FWRequest, ConnectionTimeout, ConnectionTimeout, SendTimeout, Value) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestSetTimeoutError, [GetLastError,
      SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
end;

procedure TWinHTTPRequest.SetSendTimeout(const Value: Integer);
begin
  inherited;
  if WinHttpSetTimeouts(FWRequest, ConnectionTimeout, ConnectionTimeout, Value, ResponseTimeout) = False then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpRequestSetTimeoutError, [GetLastError,
      SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
end;

procedure TWinHTTPRequest.UpdateRequest(const AURI: TURI);
begin
  FURL := AURI;
  if FWRequest <> nil then
  begin
    WinHttpCloseHandle(FWRequest);
    FWRequest := nil;
  end;
  if FWConnect <> nil then
  begin
    WinHttpCloseHandle(FWConnect);
    FWConnect := nil;
  end;
  CreateHandles(AURI);
end;

procedure TWinHTTPRequest.SetWinHttpVersion;
var
  LClient: TWinHTTPClient;
  LOption: DWORD;
begin
  LClient := TWinHTTPClient(FClient);
  case LClient.ProtocolVersion of
    THTTPProtocolVersion.UNKNOWN_HTTP:
      Exit;
    THTTPProtocolVersion.HTTP_1_0,
    THTTPProtocolVersion.HTTP_1_1:
      LOption := 0;
    THTTPProtocolVersion.HTTP_2_0:
      LOption := WINHTTP_PROTOCOL_FLAG_HTTP2;
  end;
  WinHttpSetOption(FWRequest, WINHTTP_OPTION_ENABLE_HTTP_PROTOCOL, @LOption, SizeOf(LOption));
end;

procedure TWinHTTPRequest.SetWinProxySettings;
var
  LClient: TWinHTTPClient;

  function GetProxyInfo(const AURL: string; var AProxy, AProxyBypass: string): Boolean;
  var
    LAutoDetectProxy: Boolean;
    LpProxyConfig: PWinHTTPCurrentUserIEProxyConfig;
    LProxyInfo: TWinHTTPProxyInfo;
    LAutoProxyOptions: TWinHTTPAutoProxyOptions;
    LRes: Boolean;
  begin
    Result := True;
    AProxy := '';
    AProxyBypass := '';
    FillChar(LAutoProxyOptions, SizeOf(LAutoProxyOptions), 0);

    LpProxyConfig := TWinHttpLib.GetProxyConfig;
    if LpProxyConfig = nil then
    begin
      // if the proxy configuration is not found then try to autodetect it
      // (If the Internet Explorer settings are not configured for system accounts)
      LAutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
      LAutoProxyOptions.dwAutoDetectFlags := WINHTTP_AUTO_DETECT_TYPE_DHCP or WINHTTP_AUTO_DETECT_TYPE_DNS_A;
      LAutoDetectProxy := True;
    end
    else if LpProxyConfig^.lpszAutoConfigURL <> '' then
    begin
      LAutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_CONFIG_URL;
      LAutoProxyOptions.lpszAutoConfigUrl := LpProxyConfig^.lpszAutoConfigUrl;
      LAutoDetectProxy := True;
    end
    else if LpProxyConfig^.fAutoDetect then
    begin
      LAutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
      LAutoProxyOptions.dwAutoDetectFlags := WINHTTP_AUTO_DETECT_TYPE_DHCP or WINHTTP_AUTO_DETECT_TYPE_DNS_A;
      LAutoDetectProxy := True;
    end
    else
    begin
      AProxy := LpProxyConfig^.lpszProxy;
      AProxyBypass := LpProxyConfig^.lpszProxyBypass;
      LAutoDetectProxy := False;
    end;

    if LAutoDetectProxy then
    begin
      // From https://msdn.microsoft.com/en-us/library/aa383153%28VS.85%29.aspx
      // Try with fAutoLogonIfChallenged parameter set to false, if ERROR_WINHTTP_LOGIN_FAILURE then try
      // with fAutoLogonIfChallenged parameter set to true.
      LAutoProxyOptions.fAutoLogonIfChallenged := False;
      LRes := WinHttpGetProxyForUrl(LClient.FWSession, LPCWSTR(AURL), LAutoProxyOptions, LProxyInfo);
      if LRes then
      begin
        AProxy := LProxyInfo.lpszProxy;
        AProxyBypass := LProxyInfo.lpszProxyBypass;
        GlobalFree(HGLOBAL(LProxyInfo.lpszProxy));
        GlobalFree(HGLOBAL(LProxyInfo.lpszProxyBypass));
      end
      else if GetLastError = ERROR_WINHTTP_LOGIN_FAILURE then
      begin
        LAutoProxyOptions.fAutoLogonIfChallenged := True;
        LRes := WinHttpGetProxyForUrl(LClient.FWSession, LPCWSTR(AURL), LAutoProxyOptions, LProxyInfo);
        if LRes then
        begin
          AProxy := LProxyInfo.lpszProxy;
          AProxyBypass := LProxyInfo.lpszProxyBypass;
          GlobalFree(HGLOBAL(LProxyInfo.lpszProxy));
          GlobalFree(HGLOBAL(LProxyInfo.lpszProxyBypass));
        end;
      end;
      if not LRes then
        Result := False;
    end;

    if AProxy = '' then
      Result := False;
  end;

  function GetProxyString: string;
  begin
    Result := '';
    if LClient.ProxySettings.Scheme <> '' then
      Result := Result + LClient.ProxySettings.Scheme + '://';
    Result := Result + LClient.ProxySettings.Host;
    if LClient.ProxySettings.Port <> 0 then
      Result := Result + ':' + LClient.ProxySettings.Port.ToString;
  end;

var
  LProxyInfo: TWinHttpProxyInfo;
  LOptionValue: DWORD;
  LProxyString: string;
  LProxy, LProxyBypass: string;
begin
  LClient := TWinHTTPClient(FClient);

  if LClient.ProxySettings.Host <> '' then
  begin
    LProxyString := GetProxyString;
    if not SameText('http://direct:80', LProxyString) then
    begin
      LProxyInfo.dwAccessType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
      LProxyInfo.lpszProxy := PWideChar(LProxyString);
      LProxyInfo.lpszProxyBypass := PWideChar('');
      LOptionValue := SizeOf(LProxyInfo);
      WinHttpSetOption(FWRequest, WINHTTP_OPTION_PROXY, @LProxyInfo, LOptionValue);
                                                                                                 
      if LClient.ProxySettings.UserName <> '' then
        // we try to use the usual auth scheme to try to avoid a round trip
        WinHttpSetCredentials(FWRequest, WINHTTP_AUTH_TARGET_PROXY, WINHTTP_AUTH_SCHEME_BASIC,
          PWideChar(LClient.ProxySettings.UserName), PWideChar(LClient.ProxySettings.Password), nil);
    end;
  end
  else
  begin
    if GetProxyInfo(FURL.ToString, LProxy, LProxyBypass) then
    begin
      LProxyInfo.dwAccessType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
      LProxyInfo.lpszProxy := PWideChar(LProxy);
      LProxyInfo.lpszProxyBypass := PWideChar(LProxyBypass);
      LOptionValue := SizeOf(LProxyInfo);
      WinHttpSetOption(FWRequest, WINHTTP_OPTION_PROXY, @LProxyInfo, LOptionValue);
    end;
  end;
end;

procedure TWinHTTPRequest.SetWinSecureProtocols;
var
  LClient: TWinHTTPClient;
  LProtocols: THTTPSecureProtocols;
  LOption: DWORD;
begin
  LClient := TWinHTTPClient(FClient);
  LProtocols := LClient.SecureProtocols;
  if LProtocols <> CHTTPDefSecureProtocols then
  begin
    LOption := 0;
    if THTTPSecureProtocol.SSL2 in LProtocols then
      LOption := LOption or WINHTTP_FLAG_SECURE_PROTOCOL_SSL2;
    if THTTPSecureProtocol.SSL3 in LProtocols then
      LOption := LOption or WINHTTP_FLAG_SECURE_PROTOCOL_SSL3;
    if THTTPSecureProtocol.TLS1 in LProtocols then
      LOption := LOption or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
    if THTTPSecureProtocol.TLS11 in LProtocols then
      LOption := LOption or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1;
    if THTTPSecureProtocol.TLS12 in LProtocols then
      LOption := LOption or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
    if THTTPSecureProtocol.TLS13 in LProtocols then
      LOption := LOption or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
    if not WinHttpSetOption(LClient.FWSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @LOption, SizeOf(LOption)) then
    begin
      LOption := LOption and not WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
      WinHttpSetOption(LClient.FWSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @LOption, SizeOf(LOption));
    end;
  end;
end;

procedure TWinHTTPRequest.SetWinDecompression;
var
  LClient: TWinHTTPClient;
  LDecompress: THTTPCompressionMethods;
  LOption: DWORD;
begin
  LClient := TWinHTTPClient(FClient);
  LDecompress := LClient.AutomaticDecompression;
  if LDecompress <> [] then
  begin
    LOption := 0;
    if THTTPCompressionMethod.Any in LDecompress then
      LOption := LOption or WINHTTP_DECOMPRESSION_FLAG_ALL
    else
    begin
      if THTTPCompressionMethod.Deflate in LDecompress then
        LOption := LOption or WINHTTP_DECOMPRESSION_FLAG_DEFLATE;
      if THTTPCompressionMethod.GZip in LDecompress then
        LOption := LOption or WINHTTP_DECOMPRESSION_FLAG_GZIP;
    end;
    WinHttpSetOption(FWRequest, WINHTTP_OPTION_DECOMPRESSION, @LOption, SizeOf(LOption));
  end;
end;

procedure TWinHTTPRequest.SetWinLogonPolicy;
var
  LClient: TWinHTTPClient;
  LOption: DWORD;
begin
  LClient := TWinHTTPClient(FClient);
  if LClient.UseDefaultCredentials then
    LOption := WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW
  else
    LOption := WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH;
  WinHttpSetOption(FWRequest, WINHTTP_OPTION_AUTOLOGON_POLICY, @LOption, SizeOf(LOption));
end;

procedure TWinHTTPRequest.SetWinCertificate;
var
  LStore: HCERTSTORE;
  LCertContext: PCCERT_CONTEXT;
  LBlob: CRYPT_DATA_BLOB;
  LBytes: TBytes;
begin
  if (FClientCertPath = '') and (FClientCertificate = nil) then
    Exit;

  if FClientCertPath <> '' then
    LStore := CertOpenStore(PAnsiChar(CERT_STORE_PROV_FILENAME), X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      0, CERT_STORE_OPEN_EXISTING_FLAG or CERT_STORE_READONLY_FLAG, PChar(FClientCertPath))
  else
  begin
    LBlob.cbData := FClientCertificate.Size;
    SetLength(LBytes, LBlob.cbData);
    FClientCertificate.Position := 0;
    FClientCertificate.Read(LBytes, LBlob.cbData);
    LBlob.pbData := PByte(@LBytes[0]);
    LStore := PFXImportCertStore(@LBlob, PChar(FClientCertPassword), 0);
  end;
  if LStore = nil then
    raise ENetHTTPRequestException.CreateResFmt(@SNetHttpCertFileOpenError,
      [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
  try
    LCertContext := CertFindCertificateInStore(LStore, X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      0, CERT_FIND_ANY, nil, nil);
    if LCertContext = nil then
      raise ENetHTTPRequestException.CreateResFmt(@SNetHttpCertNotFoundError,
        [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);
    try
      WinHttpSetOption(FWRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
        LCertContext, SizeOf(CERT_CONTEXT));
    finally
      CertFreeCertificateContext(LCertContext);
    end;
  finally
    CertCloseStore(LStore, 0);
  end;
end;

{ TWinHTTPResponse }

constructor TWinHTTPResponse.Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback;
  const AAsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: TWinHTTPRequest; const AContentStream: TStream);
begin
  inherited Create(AContext, AProc, AAsyncCallback, AAsyncCallbackEvent, ARequest, AContentStream);
  FRequestLink := ARequest;
  FWConnect := ARequest.FWConnect;
  FWRequest := ARequest.FWRequest;
end;

destructor TWinHTTPResponse.Destroy;
begin
  if FRequestLink <> nil then
    FRequestLink.FResponseLink := nil
  else
  begin
    if FWRequest <> nil then
      WinHttpCloseHandle(FWRequest);
    if FWConnect <> nil then
      WinHttpCloseHandle(FWConnect);
  end;
  inherited;
end;

function TWinHTTPResponse.GetIsCancelled: Boolean;
begin
  Result := (FRequestLink <> nil) and FRequestLink.GetIsCancelled;
end;

function TWinHTTPResponse.GetStatusCode: Integer;
begin
  if GetIsCancelled then
    Result := 0
  else
    Result := ReadHeader(FWRequest, WINHTTP_QUERY_STATUS_CODE).ToInteger;
end;

function TWinHTTPResponse.GetHeaders: TNetHeaders;
begin
  if GetIsCancelled then
    Result := nil
  else
  begin
    if Length(FHeaders) = 0 then
      StringToHeaders(ReadHeader(FWRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF), FHeaders, Self);
    Result := FHeaders;
  end;
end;

function TWinHTTPResponse.GetStatusText: string;
begin
  if GetIsCancelled then
    Result := ''
  else
    Result := ReadHeader(FWRequest, WINHTTP_QUERY_STATUS_TEXT);
end;

function TWinHTTPResponse.GetVersion: THTTPProtocolVersion;
var
  Version: string;
begin
  if GetIsCancelled then
    Exit(THTTPProtocolVersion.UNKNOWN_HTTP)
  else
  begin
    Version := ReadHeader(FWRequest, WINHTTP_QUERY_VERSION);

    if string.CompareText(Version, 'HTTP/1.0') = 0 then
      Result := THTTPProtocolVersion.HTTP_1_0
    else if string.CompareText(Version, 'HTTP/1.1') = 0 then
      Result := THTTPProtocolVersion.HTTP_1_1
    else if string.CompareText(Version, 'HTTP/2.0') = 0 then
      Result := THTTPProtocolVersion.HTTP_2_0
    else
      Result := THTTPProtocolVersion.UNKNOWN_HTTP;
  end;
end;

procedure TWinHTTPResponse.UpdateHandles(AWConnect, AWRequest: HINTERNET);
begin
  FWConnect := AWConnect;
  FWRequest := AWRequest;
end;

function TWinHTTPResponse.GetDecompressResponse: Boolean;
var
  LContEnc: string;
begin
  LContEnc := GetContentEncoding;
  Result :=
    ((LContEnc = 'gzip') or (LContEnc = 'deflate')) // do not translate
     and (THTTPClient(TWinHTTPRequest(FRequest).FClient).AutomaticDecompression *
          [THTTPCompressionMethod.GZip, THTTPCompressionMethod.Deflate, THTTPCompressionMethod.Any] = []);
end;

procedure TWinHTTPResponse.DoReadData(const AStream: TStream);
var
  LSize: Cardinal;
  LDownloaded: Cardinal;
  LBuffer: TBytes;
  LExpected, LReaded: Int64;
  LStatusCode: Integer;
  LAbort: Boolean;
begin
  LReaded := 0;
  LExpected := GetContentLength;
  if LExpected = 0 then
    LExpected := -1;
  LStatusCode := GetStatusCode;
  FRequestLink.DoReceiveDataProgress(LStatusCode, LExpected, LReaded, LAbort);
  if not LAbort then
    repeat
      // Get the size of readed data in LSize
      if not WinHttpQueryDataAvailable(FWRequest, @LSize) then
        raise ENetHTTPResponseException.CreateResFmt(@SNetHttpRequestReadDataError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);

      if LSize = 0 then
        Break;

      if Length(LBuffer) < Integer(LSize + 1) then
        SetLength(LBuffer, LSize + 1);

      if not WinHttpReadData(FWRequest, LBuffer[0], LSize, @LDownloaded) then
        raise ENetHTTPResponseException.CreateResFmt(@SNetHttpRequestReadDataError, [GetLastError, SysErrorMessage(GetLastError, TWinHttpLib.Handle)]);

      // This condition should never be reached since WinHttpQueryDataAvailable
      // reported that there are bits to read.
      if LDownloaded = 0 then
        Break;

      AStream.WriteBuffer(LBuffer, LDownloaded);
      LReaded := LReaded + LDownloaded;
      FRequestLink.DoReceiveDataProgress(LStatusCode, LExpected, LReaded, LAbort);
    until (LSize = 0) or LAbort;
end;

{ TWinHttpLib }

class constructor TWinHttpLib.Create;
begin
  FLock := TCriticalSection.Create;
  FHandle := GetModuleHandle(winhttp);
end;

class destructor TWinHttpLib.Destroy;
begin
  Reset;
  FLock.Free;
end;

class procedure TWinHttpLib.Reset;
begin
  if FStore <> nil then
  begin
    CertCloseStore(FStore, 0);
    FStore := nil;
  end;
  if FSession <> nil then
  begin
    WinHttpCloseHandle(FSession);
    FSession := nil;
  end;
  if FProxyConfigAcquired then
  begin
    GlobalFree(HGLOBAL(FProxyConfig.lpszAutoConfigUrl));
    GlobalFree(HGLOBAL(FProxyConfig.lpszProxy));
    GlobalFree(HGLOBAL(FProxyConfig.lpszProxyBypass));
    FProxyConfigAcquired := False;
  end;
end;

class procedure TWinHttpLib.LockHandleGC;
begin
  FLock.Enter;
  try
    // Create a long living session to stop a background handle garbage collection
    if FSession = nil then
    begin
      FSession := WinHttpOpen('', WINHTTP_ACCESS_TYPE_NO_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
      if FSession = nil then
        raise ENetHTTPClientException.CreateRes(@SNetHttpClientHandleError);
    end;
  finally
    FLock.Leave;
  end;
end;

class function TWinHttpLib.GetCertStore: HCERTSTORE;
begin
  FLock.Enter;
  try
    if FStore = nil then
      FStore := CertOpenSystemStore(0, 'MY');
    Result := FStore;
  finally
    FLock.Leave;
  end;
end;

class function TWinHttpLib.GetProxyConfig: PWinHttpCurrentUserIEProxyConfig;
begin
  FLock.Enter;
  try
    if not FProxyConfigAcquired then
    begin
      FProxyConfigResult := WinHttpGetIEProxyConfigForCurrentUser(FProxyConfig);
      FProxyConfigAcquired := True;
    end;
    if FProxyConfigResult then
      Result := @FProxyConfig
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

initialization
  TURLSchemes.RegisterURLClientScheme(TWinHTTPClient, 'HTTP');
  TURLSchemes.RegisterURLClientScheme(TWinHTTPClient, 'HTTPS');

finalization
  TURLSchemes.UnRegisterURLClientScheme('HTTP');
  TURLSchemes.UnRegisterURLClientScheme('HTTPS');
end.
