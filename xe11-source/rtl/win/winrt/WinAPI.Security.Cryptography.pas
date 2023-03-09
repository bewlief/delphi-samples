{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Security.Cryptography;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonTypes, 
  Winapi.Storage.Streams, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Security.Cryptography.Certificates.IChainValidationParameters
  Certificates_IChainValidationParameters = interface;
  PCertificates_IChainValidationParameters = ^Certificates_IChainValidationParameters;

  // Windows.Security.Cryptography.Certificates.ICertificateChain
  Certificates_ICertificateChain = interface;
  PCertificates_ICertificateChain = ^Certificates_ICertificateChain;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Certificates.ICertificateChain>
  AsyncOperationCompletedHandler_1__Certificates_ICertificateChain = interface;
  PAsyncOperationCompletedHandler_1__Certificates_ICertificateChain = ^AsyncOperationCompletedHandler_1__Certificates_ICertificateChain;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Certificates.ICertificateChain>
  IAsyncOperation_1__Certificates_ICertificateChain = interface;
  PIAsyncOperation_1__Certificates_ICertificateChain = ^IAsyncOperation_1__Certificates_ICertificateChain;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IIterator_1__Certificates_ICertificate = interface;
  PIIterator_1__Certificates_ICertificate = ^IIterator_1__Certificates_ICertificate;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IIterable_1__Certificates_ICertificate = interface;
  PIIterable_1__Certificates_ICertificate = ^IIterable_1__Certificates_ICertificate;

  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IVector_1__Certificates_ICertificate = interface;
  PIVector_1__Certificates_ICertificate = ^IVector_1__Certificates_ICertificate;

  // Windows.Security.Cryptography.Certificates.IChainBuildingParameters
  Certificates_IChainBuildingParameters = interface;
  PCertificates_IChainBuildingParameters = ^Certificates_IChainBuildingParameters;

  // Windows.Security.Cryptography.Certificates.ICertificate
  Certificates_ICertificate = interface;
  PCertificates_ICertificate = ^Certificates_ICertificate;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IVectorView_1__Certificates_ICertificate = interface;
  PIVectorView_1__Certificates_ICertificate = ^IVectorView_1__Certificates_ICertificate;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IIterator_1__Certificates_ChainValidationResult = interface;
  PIIterator_1__Certificates_ChainValidationResult = ^IIterator_1__Certificates_ChainValidationResult;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IIterable_1__Certificates_ChainValidationResult = interface;
  PIIterable_1__Certificates_ChainValidationResult = ^IIterable_1__Certificates_ChainValidationResult;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IVectorView_1__Certificates_ChainValidationResult = interface;
  PIVectorView_1__Certificates_ChainValidationResult = ^IVectorView_1__Certificates_ChainValidationResult;

  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IVector_1__Certificates_ChainValidationResult = interface;
  PIVector_1__Certificates_ChainValidationResult = ^IVector_1__Certificates_ChainValidationResult;

  // Windows.Security.Cryptography.Core.ICryptographicKey
  Core_ICryptographicKey = interface;
  PCore_ICryptographicKey = ^Core_ICryptographicKey;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Core.ICryptographicKey>
  AsyncOperationCompletedHandler_1__Core_ICryptographicKey = interface;
  PAsyncOperationCompletedHandler_1__Core_ICryptographicKey = ^AsyncOperationCompletedHandler_1__Core_ICryptographicKey;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Core.ICryptographicKey>
  IAsyncOperation_1__Core_ICryptographicKey = interface;
  PIAsyncOperation_1__Core_ICryptographicKey = ^IAsyncOperation_1__Core_ICryptographicKey;

  // Windows.Security.Cryptography.Certificates.ICertificateKeyUsages
  Certificates_ICertificateKeyUsages = interface;
  PCertificates_ICertificateKeyUsages = ^Certificates_ICertificateKeyUsages;

  // Windows.Security.Cryptography.Certificates.ISubjectAlternativeNameInfo
  Certificates_ISubjectAlternativeNameInfo = interface;
  PCertificates_ISubjectAlternativeNameInfo = ^Certificates_ISubjectAlternativeNameInfo;

  // Windows.Security.Cryptography.Certificates.ICertificate2
  Certificates_ICertificate2 = interface;
  PCertificates_ICertificate2 = ^Certificates_ICertificate2;

  // Windows.Security.Cryptography.Certificates.ICertificate3
  Certificates_ICertificate3 = interface;
  PCertificates_ICertificate3 = ^Certificates_ICertificate3;

  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties
  Certificates_ICertificateRequestProperties = interface;
  PCertificates_ICertificateRequestProperties = ^Certificates_ICertificateRequestProperties;

  // Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics
  Certificates_ICertificateEnrollmentManagerStatics = interface;
  PCertificates_ICertificateEnrollmentManagerStatics = ^Certificates_ICertificateEnrollmentManagerStatics;

  // Windows.Security.Cryptography.Certificates.IUserCertificateEnrollmentManager
  Certificates_IUserCertificateEnrollmentManager = interface;
  PCertificates_IUserCertificateEnrollmentManager = ^Certificates_IUserCertificateEnrollmentManager;

  // Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics2
  Certificates_ICertificateEnrollmentManagerStatics2 = interface;
  PCertificates_ICertificateEnrollmentManagerStatics2 = ^Certificates_ICertificateEnrollmentManagerStatics2;

  // Windows.Security.Cryptography.Certificates.IPfxImportParameters
  Certificates_IPfxImportParameters = interface;
  PCertificates_IPfxImportParameters = ^Certificates_IPfxImportParameters;

  // Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics3
  Certificates_ICertificateEnrollmentManagerStatics3 = interface;
  PCertificates_ICertificateEnrollmentManagerStatics3 = ^Certificates_ICertificateEnrollmentManagerStatics3;

  // Windows.Security.Cryptography.Certificates.ICertificateExtension
  Certificates_ICertificateExtension = interface;
  PCertificates_ICertificateExtension = ^Certificates_ICertificateExtension;

  // Windows.Security.Cryptography.Certificates.ICertificateFactory
  Certificates_ICertificateFactory = interface;
  PCertificates_ICertificateFactory = ^Certificates_ICertificateFactory;

  // Windows.Security.Cryptography.Certificates.ICertificateQuery
  Certificates_ICertificateQuery = interface;
  PCertificates_ICertificateQuery = ^Certificates_ICertificateQuery;

  // Windows.Security.Cryptography.Certificates.ICertificateQuery2
  Certificates_ICertificateQuery2 = interface;
  PCertificates_ICertificateQuery2 = ^Certificates_ICertificateQuery2;

  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties2
  Certificates_ICertificateRequestProperties2 = interface;
  PCertificates_ICertificateRequestProperties2 = ^Certificates_ICertificateRequestProperties2;

  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties3
  Certificates_ICertificateRequestProperties3 = interface;
  PCertificates_ICertificateRequestProperties3 = ^Certificates_ICertificateRequestProperties3;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IIterator_1__Certificates_ICertificateExtension = interface;
  PIIterator_1__Certificates_ICertificateExtension = ^IIterator_1__Certificates_ICertificateExtension;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IIterable_1__Certificates_ICertificateExtension = interface;
  PIIterable_1__Certificates_ICertificateExtension = ^IIterable_1__Certificates_ICertificateExtension;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IVectorView_1__Certificates_ICertificateExtension = interface;
  PIVectorView_1__Certificates_ICertificateExtension = ^IVectorView_1__Certificates_ICertificateExtension;

  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IVector_1__Certificates_ICertificateExtension = interface;
  PIVector_1__Certificates_ICertificateExtension = ^IVector_1__Certificates_ICertificateExtension;

  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties4
  Certificates_ICertificateRequestProperties4 = interface;
  PCertificates_ICertificateRequestProperties4 = ^Certificates_ICertificateRequestProperties4;

  // Windows.Security.Cryptography.Certificates.ICertificateStore
  Certificates_ICertificateStore = interface;
  PCertificates_ICertificateStore = ^Certificates_ICertificateStore;

  // Windows.Security.Cryptography.Certificates.ICertificateStore2
  Certificates_ICertificateStore2 = interface;
  PCertificates_ICertificateStore2 = ^Certificates_ICertificateStore2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate = ^AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>>
  IAsyncOperation_1__IVectorView_1__Certificates_ICertificate = interface;
  PIAsyncOperation_1__IVectorView_1__Certificates_ICertificate = ^IAsyncOperation_1__IVectorView_1__Certificates_ICertificate;

  // Windows.Security.Cryptography.Certificates.ICertificateStoresStatics
  Certificates_ICertificateStoresStatics = interface;
  PCertificates_ICertificateStoresStatics = ^Certificates_ICertificateStoresStatics;

  // Windows.Security.Cryptography.Certificates.IUserCertificateStore
  Certificates_IUserCertificateStore = interface;
  PCertificates_IUserCertificateStore = ^Certificates_IUserCertificateStore;

  // Windows.Security.Cryptography.Certificates.ICertificateStoresStatics2
  Certificates_ICertificateStoresStatics2 = interface;
  PCertificates_ICertificateStoresStatics2 = ^Certificates_ICertificateStoresStatics2;

  // Windows.Security.Cryptography.Certificates.ICmsTimestampInfo
  Certificates_ICmsTimestampInfo = interface;
  PCertificates_ICmsTimestampInfo = ^Certificates_ICmsTimestampInfo;

  // Windows.Security.Cryptography.Certificates.ICmsSignerInfo
  Certificates_ICmsSignerInfo = interface;
  PCertificates_ICmsSignerInfo = ^Certificates_ICmsSignerInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IIterator_1__Certificates_ICmsSignerInfo = interface;
  PIIterator_1__Certificates_ICmsSignerInfo = ^IIterator_1__Certificates_ICmsSignerInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IIterable_1__Certificates_ICmsSignerInfo = interface;
  PIIterable_1__Certificates_ICmsSignerInfo = ^IIterable_1__Certificates_ICmsSignerInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IVectorView_1__Certificates_ICmsSignerInfo = interface;
  PIVectorView_1__Certificates_ICmsSignerInfo = ^IVectorView_1__Certificates_ICmsSignerInfo;

  // Windows.Security.Cryptography.Certificates.ICmsAttachedSignature
  Certificates_ICmsAttachedSignature = interface;
  PCertificates_ICmsAttachedSignature = ^Certificates_ICmsAttachedSignature;

  // Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureFactory
  Certificates_ICmsAttachedSignatureFactory = interface;
  PCertificates_ICmsAttachedSignatureFactory = ^Certificates_ICmsAttachedSignatureFactory;

  // Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureStatics
  Certificates_ICmsAttachedSignatureStatics = interface;
  PCertificates_ICmsAttachedSignatureStatics = ^Certificates_ICmsAttachedSignatureStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Certificates.SignatureValidationResult>
  AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult = interface;
  PAsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult = ^AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Certificates.SignatureValidationResult>
  IAsyncOperation_1__Certificates_SignatureValidationResult = interface;
  PIAsyncOperation_1__Certificates_SignatureValidationResult = ^IAsyncOperation_1__Certificates_SignatureValidationResult;

  // Windows.Security.Cryptography.Certificates.ICmsDetachedSignature
  Certificates_ICmsDetachedSignature = interface;
  PCertificates_ICmsDetachedSignature = ^Certificates_ICmsDetachedSignature;

  // Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureFactory
  Certificates_ICmsDetachedSignatureFactory = interface;
  PCertificates_ICmsDetachedSignatureFactory = ^Certificates_ICmsDetachedSignatureFactory;

  // Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureStatics
  Certificates_ICmsDetachedSignatureStatics = interface;
  PCertificates_ICmsDetachedSignatureStatics = ^Certificates_ICmsDetachedSignatureStatics;

  // Windows.Security.Cryptography.Certificates.IKeyAlgorithmNamesStatics
  Certificates_IKeyAlgorithmNamesStatics = interface;
  PCertificates_IKeyAlgorithmNamesStatics = ^Certificates_IKeyAlgorithmNamesStatics;

  // Windows.Security.Cryptography.Certificates.IKeyAlgorithmNamesStatics2
  Certificates_IKeyAlgorithmNamesStatics2 = interface;
  PCertificates_IKeyAlgorithmNamesStatics2 = ^Certificates_IKeyAlgorithmNamesStatics2;

  // Windows.Security.Cryptography.Certificates.IKeyAttestationHelperStatics
  Certificates_IKeyAttestationHelperStatics = interface;
  PCertificates_IKeyAttestationHelperStatics = ^Certificates_IKeyAttestationHelperStatics;

  // Windows.Security.Cryptography.Certificates.IKeyAttestationHelperStatics2
  Certificates_IKeyAttestationHelperStatics2 = interface;
  PCertificates_IKeyAttestationHelperStatics2 = ^Certificates_IKeyAttestationHelperStatics2;

  // Windows.Security.Cryptography.Certificates.IKeyStorageProviderNamesStatics
  Certificates_IKeyStorageProviderNamesStatics = interface;
  PCertificates_IKeyStorageProviderNamesStatics = ^Certificates_IKeyStorageProviderNamesStatics;

  // Windows.Security.Cryptography.Certificates.IKeyStorageProviderNamesStatics2
  Certificates_IKeyStorageProviderNamesStatics2 = interface;
  PCertificates_IKeyStorageProviderNamesStatics2 = ^Certificates_IKeyStorageProviderNamesStatics2;

  // Windows.Security.Cryptography.Certificates.IStandardCertificateStoreNamesStatics
  Certificates_IStandardCertificateStoreNamesStatics = interface;
  PCertificates_IStandardCertificateStoreNamesStatics = ^Certificates_IStandardCertificateStoreNamesStatics;

  // Windows.Security.Cryptography.Certificates.ISubjectAlternativeNameInfo2
  Certificates_ISubjectAlternativeNameInfo2 = interface;
  PCertificates_ISubjectAlternativeNameInfo2 = ^Certificates_ISubjectAlternativeNameInfo2;

  // Windows.Security.Cryptography.Certificates.IUserCertificateEnrollmentManager2
  Certificates_IUserCertificateEnrollmentManager2 = interface;
  PCertificates_IUserCertificateEnrollmentManager2 = ^Certificates_IUserCertificateEnrollmentManager2;

  // Windows.Security.Cryptography.Core.IAsymmetricAlgorithmNamesStatics
  Core_IAsymmetricAlgorithmNamesStatics = interface;
  PCore_IAsymmetricAlgorithmNamesStatics = ^Core_IAsymmetricAlgorithmNamesStatics;

  // Windows.Security.Cryptography.Core.IAsymmetricAlgorithmNamesStatics2
  Core_IAsymmetricAlgorithmNamesStatics2 = interface;
  PCore_IAsymmetricAlgorithmNamesStatics2 = ^Core_IAsymmetricAlgorithmNamesStatics2;

  // Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProvider
  Core_IAsymmetricKeyAlgorithmProvider = interface;
  PCore_IAsymmetricKeyAlgorithmProvider = ^Core_IAsymmetricKeyAlgorithmProvider;

  // Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProvider2
  Core_IAsymmetricKeyAlgorithmProvider2 = interface;
  PCore_IAsymmetricKeyAlgorithmProvider2 = ^Core_IAsymmetricKeyAlgorithmProvider2;

  // Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProviderStatics
  Core_IAsymmetricKeyAlgorithmProviderStatics = interface;
  PCore_IAsymmetricKeyAlgorithmProviderStatics = ^Core_IAsymmetricKeyAlgorithmProviderStatics;

  // Windows.Security.Cryptography.Core.IEncryptedAndAuthenticatedData
  Core_IEncryptedAndAuthenticatedData = interface;
  PCore_IEncryptedAndAuthenticatedData = ^Core_IEncryptedAndAuthenticatedData;

  // Windows.Security.Cryptography.Core.IKeyDerivationParameters
  Core_IKeyDerivationParameters = interface;
  PCore_IKeyDerivationParameters = ^Core_IKeyDerivationParameters;

  // Windows.Security.Cryptography.Core.ICryptographicEngineStatics
  Core_ICryptographicEngineStatics = interface;
  PCore_ICryptographicEngineStatics = ^Core_ICryptographicEngineStatics;

  // Windows.Security.Cryptography.Core.ICryptographicEngineStatics2
  Core_ICryptographicEngineStatics2 = interface;
  PCore_ICryptographicEngineStatics2 = ^Core_ICryptographicEngineStatics2;

  // Windows.Security.Cryptography.Core.IEccCurveNamesStatics
  Core_IEccCurveNamesStatics = interface;
  PCore_IEccCurveNamesStatics = ^Core_IEccCurveNamesStatics;

  // Windows.Security.Cryptography.Core.IHashAlgorithmNamesStatics
  Core_IHashAlgorithmNamesStatics = interface;
  PCore_IHashAlgorithmNamesStatics = ^Core_IHashAlgorithmNamesStatics;

  // Windows.Security.Cryptography.Core.IHashComputation
  Core_IHashComputation = interface;
  PCore_IHashComputation = ^Core_IHashComputation;

  // Windows.Security.Cryptography.Core.IHashAlgorithmProvider
  Core_IHashAlgorithmProvider = interface;
  PCore_IHashAlgorithmProvider = ^Core_IHashAlgorithmProvider;

  // Windows.Security.Cryptography.Core.IHashAlgorithmProviderStatics
  Core_IHashAlgorithmProviderStatics = interface;
  PCore_IHashAlgorithmProviderStatics = ^Core_IHashAlgorithmProviderStatics;

  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmNamesStatics
  Core_IKeyDerivationAlgorithmNamesStatics = interface;
  PCore_IKeyDerivationAlgorithmNamesStatics = ^Core_IKeyDerivationAlgorithmNamesStatics;

  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmNamesStatics2
  Core_IKeyDerivationAlgorithmNamesStatics2 = interface;
  PCore_IKeyDerivationAlgorithmNamesStatics2 = ^Core_IKeyDerivationAlgorithmNamesStatics2;

  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmProvider
  Core_IKeyDerivationAlgorithmProvider = interface;
  PCore_IKeyDerivationAlgorithmProvider = ^Core_IKeyDerivationAlgorithmProvider;

  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmProviderStatics
  Core_IKeyDerivationAlgorithmProviderStatics = interface;
  PCore_IKeyDerivationAlgorithmProviderStatics = ^Core_IKeyDerivationAlgorithmProviderStatics;

  // Windows.Security.Cryptography.Core.IKeyDerivationParameters2
  Core_IKeyDerivationParameters2 = interface;
  PCore_IKeyDerivationParameters2 = ^Core_IKeyDerivationParameters2;

  // Windows.Security.Cryptography.Core.IKeyDerivationParametersStatics
  Core_IKeyDerivationParametersStatics = interface;
  PCore_IKeyDerivationParametersStatics = ^Core_IKeyDerivationParametersStatics;

  // Windows.Security.Cryptography.Core.IKeyDerivationParametersStatics2
  Core_IKeyDerivationParametersStatics2 = interface;
  PCore_IKeyDerivationParametersStatics2 = ^Core_IKeyDerivationParametersStatics2;

  // Windows.Security.Cryptography.Core.IMacAlgorithmNamesStatics
  Core_IMacAlgorithmNamesStatics = interface;
  PCore_IMacAlgorithmNamesStatics = ^Core_IMacAlgorithmNamesStatics;

  // Windows.Security.Cryptography.Core.IMacAlgorithmProvider
  Core_IMacAlgorithmProvider = interface;
  PCore_IMacAlgorithmProvider = ^Core_IMacAlgorithmProvider;

  // Windows.Security.Cryptography.Core.IMacAlgorithmProvider2
  Core_IMacAlgorithmProvider2 = interface;
  PCore_IMacAlgorithmProvider2 = ^Core_IMacAlgorithmProvider2;

  // Windows.Security.Cryptography.Core.IMacAlgorithmProviderStatics
  Core_IMacAlgorithmProviderStatics = interface;
  PCore_IMacAlgorithmProviderStatics = ^Core_IMacAlgorithmProviderStatics;

  // Windows.Security.Cryptography.Core.IPersistedKeyProviderStatics
  Core_IPersistedKeyProviderStatics = interface;
  PCore_IPersistedKeyProviderStatics = ^Core_IPersistedKeyProviderStatics;

  // Windows.Security.Cryptography.Core.ISymmetricAlgorithmNamesStatics
  Core_ISymmetricAlgorithmNamesStatics = interface;
  PCore_ISymmetricAlgorithmNamesStatics = ^Core_ISymmetricAlgorithmNamesStatics;

  // Windows.Security.Cryptography.Core.ISymmetricKeyAlgorithmProvider
  Core_ISymmetricKeyAlgorithmProvider = interface;
  PCore_ISymmetricKeyAlgorithmProvider = ^Core_ISymmetricKeyAlgorithmProvider;

  // Windows.Security.Cryptography.Core.ISymmetricKeyAlgorithmProviderStatics
  Core_ISymmetricKeyAlgorithmProviderStatics = interface;
  PCore_ISymmetricKeyAlgorithmProviderStatics = ^Core_ISymmetricKeyAlgorithmProviderStatics;

  // Windows.Security.Cryptography.DataProtection.IDataProtectionProvider
  DataProtection_IDataProtectionProvider = interface;
  PDataProtection_IDataProtectionProvider = ^DataProtection_IDataProtectionProvider;

  // Windows.Security.Cryptography.DataProtection.IDataProtectionProviderFactory
  DataProtection_IDataProtectionProviderFactory = interface;
  PDataProtection_IDataProtectionProviderFactory = ^DataProtection_IDataProtectionProviderFactory;

  // Windows.Security.Cryptography.ICryptographicBufferStatics
  ICryptographicBufferStatics = interface;
  PICryptographicBufferStatics = ^ICryptographicBufferStatics;

  // Windows.Security.Cryptography Enums

  // Windows.Security.Cryptography.BinaryStringEncoding
  BinaryStringEncoding = (
    Utf8 = 0,
    Utf16LE = 1,
    Utf16BE = 2
  );
  PBinaryStringEncoding = ^BinaryStringEncoding;

  // Windows.Security.Cryptography.Certificates.CertificateChainPolicy
  Certificates_CertificateChainPolicy = (
    Base = 0,
    Ssl = 1,
    NTAuthentication = 2,
    MicrosoftRoot = 3
  );
  PCertificates_CertificateChainPolicy = ^Certificates_CertificateChainPolicy;

  // Windows.Security.Cryptography.Certificates.ChainValidationResult
  Certificates_ChainValidationResult = (
    Success = 0,
    Untrusted = 1,
    Revoked = 2,
    Expired = 3,
    IncompleteChain = 4,
    InvalidSignature = 5,
    WrongUsage = 6,
    InvalidName = 7,
    InvalidCertificateAuthorityPolicy = 8,
    BasicConstraintsError = 9,
    UnknownCriticalExtension = 10,
    RevocationInformationMissing = 11,
    RevocationFailure = 12,
    OtherErrors = 13
  );
  PCertificates_ChainValidationResult = ^Certificates_ChainValidationResult;

  // Windows.Security.Cryptography.Certificates.EnrollKeyUsages
  Certificates_EnrollKeyUsages = (
    None = 0,
    Decryption = 1,
    Signing = 2,
    KeyAgreement = 4,
    All = 16777215
  );
  PCertificates_EnrollKeyUsages = ^Certificates_EnrollKeyUsages;

  // Windows.Security.Cryptography.Certificates.ExportOption
  Certificates_ExportOption = (
    NotExportable = 0,
    Exportable = 1
  );
  PCertificates_ExportOption = ^Certificates_ExportOption;

  // Windows.Security.Cryptography.Certificates.InstallOptions
  Certificates_InstallOptions = (
    None = 0,
    DeleteExpired = 1
  );
  PCertificates_InstallOptions = ^Certificates_InstallOptions;

  // Windows.Security.Cryptography.Certificates.KeyProtectionLevel
  Certificates_KeyProtectionLevel = (
    NoConsent = 0,
    ConsentOnly = 1,
    ConsentWithPassword = 2,
    ConsentWithFingerprint = 3
  );
  PCertificates_KeyProtectionLevel = ^Certificates_KeyProtectionLevel;

  // Windows.Security.Cryptography.Certificates.KeySize
  Certificates_KeySize = (
    Invalid = 0,
    Rsa2048 = 2048,
    Rsa4096 = 4096
  );
  PCertificates_KeySize = ^Certificates_KeySize;

  // Windows.Security.Cryptography.Certificates.SignatureValidationResult
  Certificates_SignatureValidationResult = (
    Success = 0,
    InvalidParameter = 1,
    BadMessage = 2,
    InvalidSignature = 3,
    OtherErrors = 4
  );
  PCertificates_SignatureValidationResult = ^Certificates_SignatureValidationResult;

  // Windows.Security.Cryptography.Core.Capi1KdfTargetAlgorithm
  Core_Capi1KdfTargetAlgorithm = (
    NotAes = 0,
    Aes = 1
  );
  PCore_Capi1KdfTargetAlgorithm = ^Core_Capi1KdfTargetAlgorithm;

  // Windows.Security.Cryptography.Core.CryptographicPadding
  Core_CryptographicPadding = (
    None = 0,
    RsaOaep = 1,
    RsaPkcs1V15 = 2,
    RsaPss = 3
  );
  PCore_CryptographicPadding = ^Core_CryptographicPadding;

  // Windows.Security.Cryptography.Core.CryptographicPrivateKeyBlobType
  Core_CryptographicPrivateKeyBlobType = (
    Pkcs8RawPrivateKeyInfo = 0,
    Pkcs1RsaPrivateKey = 1,
    BCryptPrivateKey = 2,
    Capi1PrivateKey = 3,
    BCryptEccFullPrivateKey = 4
  );
  PCore_CryptographicPrivateKeyBlobType = ^Core_CryptographicPrivateKeyBlobType;

  // Windows.Security.Cryptography.Core.CryptographicPublicKeyBlobType
  Core_CryptographicPublicKeyBlobType = (
    X509SubjectPublicKeyInfo = 0,
    Pkcs1RsaPublicKey = 1,
    BCryptPublicKey = 2,
    Capi1PublicKey = 3,
    BCryptEccFullPublicKey = 4
  );
  PCore_CryptographicPublicKeyBlobType = ^Core_CryptographicPublicKeyBlobType;

  // Windows.Security.Cryptography Interfaces

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IChainValidationParameters
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_ChainValidationParameters)]
  Certificates_IChainValidationParameters = interface(IInspectable)
  ['{C4743B4A-7EB0-4B56-A040-B9C8E655DDF3}']
    function get_CertificateChainPolicy: Certificates_CertificateChainPolicy; safecall;
    procedure put_CertificateChainPolicy(value: Certificates_CertificateChainPolicy); safecall;
    function get_ServerDnsName: IHostName; safecall;
    procedure put_ServerDnsName(value: IHostName); safecall;
    property CertificateChainPolicy: Certificates_CertificateChainPolicy read get_CertificateChainPolicy write put_CertificateChainPolicy;
    property ServerDnsName: IHostName read get_ServerDnsName write put_ServerDnsName;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateChain
  Certificates_ICertificateChain = interface(IInspectable)
  ['{20BF5385-3691-4501-A62C-FD97278B31EE}']
    function Validate: Certificates_ChainValidationResult; overload; safecall;
    function Validate(parameter: Certificates_IChainValidationParameters): Certificates_ChainValidationResult; overload; safecall;
    function GetCertificates(includeRoot: Boolean): IVectorView_1__Certificates_ICertificate; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Certificates.ICertificateChain>
  AsyncOperationCompletedHandler_1__Certificates_ICertificateChain_Delegate_Base = interface(IUnknown)
  ['{4C3F50E9-90E3-5A30-9015-4AA0376904F3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Certificates_ICertificateChain; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Certificates.ICertificateChain>
  AsyncOperationCompletedHandler_1__Certificates_ICertificateChain = interface(AsyncOperationCompletedHandler_1__Certificates_ICertificateChain_Delegate_Base)
  ['{873FE295-70EF-5FE5-B9E8-296FA74E6A45}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Certificates.ICertificateChain>
  IAsyncOperation_1__Certificates_ICertificateChain_Base = interface(IInspectable)
  ['{F618C7D4-AEE1-58AE-AFE8-FC336DAF0395}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Certificates_ICertificateChain); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Certificates_ICertificateChain; safecall;
    function GetResults: Certificates_ICertificateChain; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Certificates_ICertificateChain read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Certificates.ICertificateChain>
  IAsyncOperation_1__Certificates_ICertificateChain = interface(IAsyncOperation_1__Certificates_ICertificateChain_Base)
  ['{1B690477-AF74-5CEA-9C16-BE5CF3255DCF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IIterator_1__Certificates_ICertificate_Base = interface(IInspectable)
  ['{676FC159-F15C-58BD-91A7-28F7E795C756}']
    function get_Current: Certificates_ICertificate; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCertificates_ICertificate): Cardinal; safecall;
    property Current: Certificates_ICertificate read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IIterator_1__Certificates_ICertificate = interface(IIterator_1__Certificates_ICertificate_Base)
  ['{1219E502-5A43-57B7-87F0-D0C678080E9F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IIterable_1__Certificates_ICertificate_Base = interface(IInspectable)
  ['{0C7D1423-E8FD-5A91-B55C-8BFBE7AC2D40}']
    function First: IIterator_1__Certificates_ICertificate; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IIterable_1__Certificates_ICertificate = interface(IIterable_1__Certificates_ICertificate_Base)
  ['{44600A39-E90B-5D5A-9328-9943725C732E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IVector_1__Certificates_ICertificate_Base = interface(IInspectable)
  ['{36282C0F-2F1F-57F4-B2B1-867AF90C3D13}']
    function GetAt(index: Cardinal): Certificates_ICertificate; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Certificates_ICertificate; safecall;
    function IndexOf(value: Certificates_ICertificate; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Certificates_ICertificate); safecall;
    procedure InsertAt(index: Cardinal; value: Certificates_ICertificate); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Certificates_ICertificate); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ICertificate): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCertificates_ICertificate); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IVector_1__Certificates_ICertificate = interface(IVector_1__Certificates_ICertificate_Base)
  ['{8C75B574-4DFB-558D-906F-6FEDC13229F9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IChainBuildingParameters
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_ChainBuildingParameters)]
  Certificates_IChainBuildingParameters = interface(IInspectable)
  ['{422BA922-7C8D-47B7-B59B-B12703733AC3}']
    function get_EnhancedKeyUsages: IVector_1__HSTRING; safecall;
    function get_ValidationTimestamp: DateTime; safecall;
    procedure put_ValidationTimestamp(value: DateTime); safecall;
    function get_RevocationCheckEnabled: Boolean; safecall;
    procedure put_RevocationCheckEnabled(value: Boolean); safecall;
    function get_NetworkRetrievalEnabled: Boolean; safecall;
    procedure put_NetworkRetrievalEnabled(value: Boolean); safecall;
    function get_AuthorityInformationAccessEnabled: Boolean; safecall;
    procedure put_AuthorityInformationAccessEnabled(value: Boolean); safecall;
    function get_CurrentTimeValidationEnabled: Boolean; safecall;
    procedure put_CurrentTimeValidationEnabled(value: Boolean); safecall;
    function get_ExclusiveTrustRoots: IVector_1__Certificates_ICertificate; safecall;
    property AuthorityInformationAccessEnabled: Boolean read get_AuthorityInformationAccessEnabled write put_AuthorityInformationAccessEnabled;
    property CurrentTimeValidationEnabled: Boolean read get_CurrentTimeValidationEnabled write put_CurrentTimeValidationEnabled;
    property EnhancedKeyUsages: IVector_1__HSTRING read get_EnhancedKeyUsages;
    property ExclusiveTrustRoots: IVector_1__Certificates_ICertificate read get_ExclusiveTrustRoots;
    property NetworkRetrievalEnabled: Boolean read get_NetworkRetrievalEnabled write put_NetworkRetrievalEnabled;
    property RevocationCheckEnabled: Boolean read get_RevocationCheckEnabled write put_RevocationCheckEnabled;
    property ValidationTimestamp: DateTime read get_ValidationTimestamp write put_ValidationTimestamp;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificate
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_Certificate)]
  Certificates_ICertificate = interface(IInspectable)
  ['{333F740C-04D8-43B3-B278-8C5FCC9BE5A0}']
    function BuildChainAsync(certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__Certificates_ICertificateChain; overload; safecall;
    function BuildChainAsync(certificates: IIterable_1__Certificates_ICertificate; parameters: Certificates_IChainBuildingParameters): IAsyncOperation_1__Certificates_ICertificateChain; overload; safecall;
    function get_SerialNumber(resultSize: Cardinal; resultValue: PByte): HRESULT; stdcall;
    function GetHashValue(resultSize: Cardinal; resultValue: PByte): HRESULT; overload; stdcall;
    function GetHashValue(hashAlgorithmName: HSTRING; resultSize: Cardinal; resultValue: PByte): HRESULT; overload; stdcall;
    function GetCertificateBlob: IBuffer; safecall;
    function get_Subject: HSTRING; safecall;
    function get_Issuer: HSTRING; safecall;
    function get_HasPrivateKey: Boolean; safecall;
    function get_IsStronglyProtected: Boolean; safecall;
    function get_ValidFrom: DateTime; safecall;
    function get_ValidTo: DateTime; safecall;
    function get_EnhancedKeyUsages: IVectorView_1__HSTRING; safecall;
    procedure put_FriendlyName(value: HSTRING); safecall;
    function get_FriendlyName: HSTRING; safecall;
    property EnhancedKeyUsages: IVectorView_1__HSTRING read get_EnhancedKeyUsages;
    property FriendlyName: HSTRING read get_FriendlyName write put_FriendlyName;
    property HasPrivateKey: Boolean read get_HasPrivateKey;
    property IsStronglyProtected: Boolean read get_IsStronglyProtected;
    property Issuer: HSTRING read get_Issuer;
    // property SerialNumber: PByte read get_SerialNumber;
    property Subject: HSTRING read get_Subject;
    property ValidFrom: DateTime read get_ValidFrom;
    property ValidTo: DateTime read get_ValidTo;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>
  IVectorView_1__Certificates_ICertificate = interface(IInspectable)
  ['{4AEA65EF-C0C3-5EC0-BC9F-09310B896626}']
    function GetAt(index: Cardinal): Certificates_ICertificate; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Certificates_ICertificate; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ICertificate): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IIterator_1__Certificates_ChainValidationResult_Base = interface(IInspectable)
  ['{8BCAD2B7-0E3B-5EAE-BF69-E1F6D9C888F8}']
    function get_Current: Certificates_ChainValidationResult; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCertificates_ChainValidationResult): Cardinal; safecall;
    property Current: Certificates_ChainValidationResult read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IIterator_1__Certificates_ChainValidationResult = interface(IIterator_1__Certificates_ChainValidationResult_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IIterable_1__Certificates_ChainValidationResult_Base = interface(IInspectable)
  ['{2628F58F-3F02-54F2-808F-E1117709D6D0}']
    function First: IIterator_1__Certificates_ChainValidationResult; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IIterable_1__Certificates_ChainValidationResult = interface(IIterable_1__Certificates_ChainValidationResult_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IVectorView_1__Certificates_ChainValidationResult = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Certificates_ChainValidationResult; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Certificates_ChainValidationResult; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ChainValidationResult): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IVector_1__Certificates_ChainValidationResult_Base = interface(IInspectable)
  ['{D7828CF7-4301-58D3-AAB5-06E5EEFCF79F}']
    function GetAt(index: Cardinal): Certificates_ChainValidationResult; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Certificates_ChainValidationResult; safecall;
    function IndexOf(value: Certificates_ChainValidationResult; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Certificates_ChainValidationResult); safecall;
    procedure InsertAt(index: Cardinal; value: Certificates_ChainValidationResult); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Certificates_ChainValidationResult); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ChainValidationResult): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCertificates_ChainValidationResult); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ChainValidationResult>
  IVector_1__Certificates_ChainValidationResult = interface(IVector_1__Certificates_ChainValidationResult_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.ICryptographicKey
  Core_ICryptographicKey = interface(IInspectable)
  ['{ED2A3B70-8E7B-4009-8401-FFD1A62EEB27}']
    function get_KeySize: Cardinal; safecall;
    function &Export: IBuffer; overload; safecall;
    function &Export(BlobType: Core_CryptographicPrivateKeyBlobType): IBuffer; overload; safecall;
    function ExportPublicKey: IBuffer; overload; safecall;
    function ExportPublicKey(BlobType: Core_CryptographicPublicKeyBlobType): IBuffer; overload; safecall;
    property KeySize: Cardinal read get_KeySize;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Core.ICryptographicKey>
  AsyncOperationCompletedHandler_1__Core_ICryptographicKey_Delegate_Base = interface(IUnknown)
  ['{04CA4378-F594-5DE6-A555-304F62CB4FAF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Core_ICryptographicKey; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Core.ICryptographicKey>
  AsyncOperationCompletedHandler_1__Core_ICryptographicKey = interface(AsyncOperationCompletedHandler_1__Core_ICryptographicKey_Delegate_Base)
  ['{ECEB7CEE-B88A-56B8-9C64-662D79A102CB}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Core.ICryptographicKey>
  IAsyncOperation_1__Core_ICryptographicKey_Base = interface(IInspectable)
  ['{81CA789B-98DF-5C6A-9531-966238E3E7AE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Core_ICryptographicKey); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Core_ICryptographicKey; safecall;
    function GetResults: Core_ICryptographicKey; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Core_ICryptographicKey read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Core.ICryptographicKey>
  IAsyncOperation_1__Core_ICryptographicKey = interface(IAsyncOperation_1__Core_ICryptographicKey_Base)
  ['{4E5B8FAA-80B0-5A57-BC29-3686A5BD10AC}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateKeyUsages
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateKeyUsages)]
  Certificates_ICertificateKeyUsages = interface(IInspectable)
  ['{6AC6206F-E1CF-486A-B485-A69C83E46FD1}']
    function get_EncipherOnly: Boolean; safecall;
    procedure put_EncipherOnly(value: Boolean); safecall;
    function get_CrlSign: Boolean; safecall;
    procedure put_CrlSign(value: Boolean); safecall;
    function get_KeyCertificateSign: Boolean; safecall;
    procedure put_KeyCertificateSign(value: Boolean); safecall;
    function get_KeyAgreement: Boolean; safecall;
    procedure put_KeyAgreement(value: Boolean); safecall;
    function get_DataEncipherment: Boolean; safecall;
    procedure put_DataEncipherment(value: Boolean); safecall;
    function get_KeyEncipherment: Boolean; safecall;
    procedure put_KeyEncipherment(value: Boolean); safecall;
    function get_NonRepudiation: Boolean; safecall;
    procedure put_NonRepudiation(value: Boolean); safecall;
    function get_DigitalSignature: Boolean; safecall;
    procedure put_DigitalSignature(value: Boolean); safecall;
    property CrlSign: Boolean read get_CrlSign write put_CrlSign;
    property DataEncipherment: Boolean read get_DataEncipherment write put_DataEncipherment;
    property DigitalSignature: Boolean read get_DigitalSignature write put_DigitalSignature;
    property EncipherOnly: Boolean read get_EncipherOnly write put_EncipherOnly;
    property KeyAgreement: Boolean read get_KeyAgreement write put_KeyAgreement;
    property KeyCertificateSign: Boolean read get_KeyCertificateSign write put_KeyCertificateSign;
    property KeyEncipherment: Boolean read get_KeyEncipherment write put_KeyEncipherment;
    property NonRepudiation: Boolean read get_NonRepudiation write put_NonRepudiation;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ISubjectAlternativeNameInfo
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_SubjectAlternativeNameInfo)]
  Certificates_ISubjectAlternativeNameInfo = interface(IInspectable)
  ['{582859F1-569D-4C20-BE7B-4E1C9A0BC52B}']
    function get_EmailName: IVectorView_1__HSTRING; safecall;
    function get_IPAddress: IVectorView_1__HSTRING; safecall;
    function get_Url: IVectorView_1__HSTRING; safecall;
    function get_DnsName: IVectorView_1__HSTRING; safecall;
    function get_DistinguishedName: IVectorView_1__HSTRING; safecall;
    function get_PrincipalName: IVectorView_1__HSTRING; safecall;
    property DistinguishedName: IVectorView_1__HSTRING read get_DistinguishedName;
    property DnsName: IVectorView_1__HSTRING read get_DnsName;
    property EmailName: IVectorView_1__HSTRING read get_EmailName;
    property IPAddress: IVectorView_1__HSTRING read get_IPAddress;
    property PrincipalName: IVectorView_1__HSTRING read get_PrincipalName;
    property Url: IVectorView_1__HSTRING read get_Url;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificate2
  Certificates_ICertificate2 = interface(IInspectable)
  ['{17B8374C-8A25-4D96-A492-8FC29AC4FDA6}']
    function get_IsSecurityDeviceBound: Boolean; safecall;
    function get_KeyUsages: Certificates_ICertificateKeyUsages; safecall;
    function get_KeyAlgorithmName: HSTRING; safecall;
    function get_SignatureAlgorithmName: HSTRING; safecall;
    function get_SignatureHashAlgorithmName: HSTRING; safecall;
    function get_SubjectAlternativeName: Certificates_ISubjectAlternativeNameInfo; safecall;
    property IsSecurityDeviceBound: Boolean read get_IsSecurityDeviceBound;
    property KeyAlgorithmName: HSTRING read get_KeyAlgorithmName;
    property KeyUsages: Certificates_ICertificateKeyUsages read get_KeyUsages;
    property SignatureAlgorithmName: HSTRING read get_SignatureAlgorithmName;
    property SignatureHashAlgorithmName: HSTRING read get_SignatureHashAlgorithmName;
    property SubjectAlternativeName: Certificates_ISubjectAlternativeNameInfo read get_SubjectAlternativeName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificate3
  Certificates_ICertificate3 = interface(IInspectable)
  ['{BE51A966-AE5F-4652-ACE7-C6D7E7724CF3}']
    function get_IsPerUser: Boolean; safecall;
    function get_StoreName: HSTRING; safecall;
    function get_KeyStorageProviderName: HSTRING; safecall;
    property IsPerUser: Boolean read get_IsPerUser;
    property KeyStorageProviderName: HSTRING read get_KeyStorageProviderName;
    property StoreName: HSTRING read get_StoreName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateRequestProperties)]
  Certificates_ICertificateRequestProperties = interface(IInspectable)
  ['{487E84F6-94E2-4DCE-8833-1A700A37A29A}']
    function get_Subject: HSTRING; safecall;
    procedure put_Subject(value: HSTRING); safecall;
    function get_KeyAlgorithmName: HSTRING; safecall;
    procedure put_KeyAlgorithmName(value: HSTRING); safecall;
    function get_KeySize: Cardinal; safecall;
    procedure put_KeySize(value: Cardinal); safecall;
    function get_FriendlyName: HSTRING; safecall;
    procedure put_FriendlyName(value: HSTRING); safecall;
    function get_HashAlgorithmName: HSTRING; safecall;
    procedure put_HashAlgorithmName(value: HSTRING); safecall;
    function get_Exportable: Certificates_ExportOption; safecall;
    procedure put_Exportable(value: Certificates_ExportOption); safecall;
    function get_KeyUsages: Certificates_EnrollKeyUsages; safecall;
    procedure put_KeyUsages(value: Certificates_EnrollKeyUsages); safecall;
    function get_KeyProtectionLevel: Certificates_KeyProtectionLevel; safecall;
    procedure put_KeyProtectionLevel(value: Certificates_KeyProtectionLevel); safecall;
    function get_KeyStorageProviderName: HSTRING; safecall;
    procedure put_KeyStorageProviderName(value: HSTRING); safecall;
    property Exportable: Certificates_ExportOption read get_Exportable write put_Exportable;
    property FriendlyName: HSTRING read get_FriendlyName write put_FriendlyName;
    property HashAlgorithmName: HSTRING read get_HashAlgorithmName write put_HashAlgorithmName;
    property KeyAlgorithmName: HSTRING read get_KeyAlgorithmName write put_KeyAlgorithmName;
    property KeyProtectionLevel: Certificates_KeyProtectionLevel read get_KeyProtectionLevel write put_KeyProtectionLevel;
    property KeySize: Cardinal read get_KeySize write put_KeySize;
    property KeyStorageProviderName: HSTRING read get_KeyStorageProviderName write put_KeyStorageProviderName;
    property KeyUsages: Certificates_EnrollKeyUsages read get_KeyUsages write put_KeyUsages;
    property Subject: HSTRING read get_Subject write put_Subject;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateEnrollmentManager)]
  Certificates_ICertificateEnrollmentManagerStatics = interface(IInspectable)
  ['{8846EF3F-A986-48FB-9FD7-9AEC06935BF1}']
    function CreateRequestAsync(request: Certificates_ICertificateRequestProperties): IAsyncOperation_1__HSTRING; safecall;
    function InstallCertificateAsync(certificate: HSTRING; installOption: Certificates_InstallOptions): IAsyncAction; safecall;
    function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING): IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IUserCertificateEnrollmentManager
  Certificates_IUserCertificateEnrollmentManager = interface(IInspectable)
  ['{96313718-22E1-4819-B20B-AB46A6ECA06E}']
    function CreateRequestAsync(request: Certificates_ICertificateRequestProperties): IAsyncOperation_1__HSTRING; safecall;
    function InstallCertificateAsync(certificate: HSTRING; installOption: Certificates_InstallOptions): IAsyncAction; safecall;
    function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING): IAsyncAction; overload; safecall;
    function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING; keyStorageProvider: HSTRING): IAsyncAction; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateEnrollmentManager)]
  Certificates_ICertificateEnrollmentManagerStatics2 = interface(IInspectable)
  ['{DC5B1C33-6429-4014-999C-5D9735802D1D}']
    function get_UserCertificateEnrollmentManager: Certificates_IUserCertificateEnrollmentManager; safecall;
    function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING; keyStorageProvider: HSTRING): IAsyncAction; safecall;
    property UserCertificateEnrollmentManager: Certificates_IUserCertificateEnrollmentManager read get_UserCertificateEnrollmentManager;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IPfxImportParameters
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_PfxImportParameters)]
  Certificates_IPfxImportParameters = interface(IInspectable)
  ['{680D3511-9A08-47C8-864A-2EDD4D8EB46C}']
    function get_Exportable: Certificates_ExportOption; safecall;
    procedure put_Exportable(value: Certificates_ExportOption); safecall;
    function get_KeyProtectionLevel: Certificates_KeyProtectionLevel; safecall;
    procedure put_KeyProtectionLevel(value: Certificates_KeyProtectionLevel); safecall;
    function get_InstallOptions: Certificates_InstallOptions; safecall;
    procedure put_InstallOptions(value: Certificates_InstallOptions); safecall;
    function get_FriendlyName: HSTRING; safecall;
    procedure put_FriendlyName(value: HSTRING); safecall;
    function get_KeyStorageProviderName: HSTRING; safecall;
    procedure put_KeyStorageProviderName(value: HSTRING); safecall;
    function get_ContainerNamePrefix: HSTRING; safecall;
    procedure put_ContainerNamePrefix(value: HSTRING); safecall;
    function get_ReaderName: HSTRING; safecall;
    procedure put_ReaderName(value: HSTRING); safecall;
    property ContainerNamePrefix: HSTRING read get_ContainerNamePrefix write put_ContainerNamePrefix;
    property Exportable: Certificates_ExportOption read get_Exportable write put_Exportable;
    property FriendlyName: HSTRING read get_FriendlyName write put_FriendlyName;
    property InstallOptions: Certificates_InstallOptions read get_InstallOptions write put_InstallOptions;
    property KeyProtectionLevel: Certificates_KeyProtectionLevel read get_KeyProtectionLevel write put_KeyProtectionLevel;
    property KeyStorageProviderName: HSTRING read get_KeyStorageProviderName write put_KeyStorageProviderName;
    property ReaderName: HSTRING read get_ReaderName write put_ReaderName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics3
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateEnrollmentManager)]
  Certificates_ICertificateEnrollmentManagerStatics3 = interface(IInspectable)
  ['{FDEC82BE-617C-425A-B72D-398B26AC7264}']
    function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; pfxImportParameters: Certificates_IPfxImportParameters): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateExtension
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateExtension)]
  Certificates_ICertificateExtension = interface(IInspectable)
  ['{84CF0656-A9E6-454D-8E45-2EA7C4BCD53B}']
    function get_ObjectId: HSTRING; safecall;
    procedure put_ObjectId(value: HSTRING); safecall;
    function get_IsCritical: Boolean; safecall;
    procedure put_IsCritical(value: Boolean); safecall;
    procedure EncodeValue(value: HSTRING); safecall;
    function get_Value(resultSize: Cardinal; resultValue: PByte): HRESULT; stdcall;
    procedure put_Value(valueSize: Cardinal; value: PByte); safecall;
    property IsCritical: Boolean read get_IsCritical write put_IsCritical;
    property ObjectId: HSTRING read get_ObjectId write put_ObjectId;
    // property Value: PByte read get_Value write put_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateFactory
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_Certificate)]
  Certificates_ICertificateFactory = interface(IInspectable)
  ['{17B4221C-4BAF-44A2-9608-04FB62B16942}']
    function CreateCertificate(certBlob: IBuffer): Certificates_ICertificate; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateQuery
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateQuery)]
  Certificates_ICertificateQuery = interface(IInspectable)
  ['{5B082A31-A728-4916-B5EE-FFCB8ACF2417}']
    function get_EnhancedKeyUsages: IVector_1__HSTRING; safecall;
    function get_IssuerName: HSTRING; safecall;
    procedure put_IssuerName(value: HSTRING); safecall;
    function get_FriendlyName: HSTRING; safecall;
    procedure put_FriendlyName(value: HSTRING); safecall;
    function get_Thumbprint(resultSize: Cardinal; resultValue: PByte): HRESULT; stdcall;
    procedure put_Thumbprint(valueSize: Cardinal; value: PByte); safecall;
    function get_HardwareOnly: Boolean; safecall;
    procedure put_HardwareOnly(value: Boolean); safecall;
    property EnhancedKeyUsages: IVector_1__HSTRING read get_EnhancedKeyUsages;
    property FriendlyName: HSTRING read get_FriendlyName write put_FriendlyName;
    property HardwareOnly: Boolean read get_HardwareOnly write put_HardwareOnly;
    property IssuerName: HSTRING read get_IssuerName write put_IssuerName;
    // property Thumbprint: PByte read get_Thumbprint write put_Thumbprint;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateQuery2
  Certificates_ICertificateQuery2 = interface(IInspectable)
  ['{935A0AF7-0BD9-4F75-B8C2-E27A7F74EECD}']
    function get_IncludeDuplicates: Boolean; safecall;
    procedure put_IncludeDuplicates(value: Boolean); safecall;
    function get_IncludeExpiredCertificates: Boolean; safecall;
    procedure put_IncludeExpiredCertificates(value: Boolean); safecall;
    function get_StoreName: HSTRING; safecall;
    procedure put_StoreName(value: HSTRING); safecall;
    property IncludeDuplicates: Boolean read get_IncludeDuplicates write put_IncludeDuplicates;
    property IncludeExpiredCertificates: Boolean read get_IncludeExpiredCertificates write put_IncludeExpiredCertificates;
    property StoreName: HSTRING read get_StoreName write put_StoreName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties2
  Certificates_ICertificateRequestProperties2 = interface(IInspectable)
  ['{3DA0C954-D73F-4FF3-A0A6-0677C0ADA05B}']
    function get_SmartcardReaderName: HSTRING; safecall;
    procedure put_SmartcardReaderName(value: HSTRING); safecall;
    function get_SigningCertificate: Certificates_ICertificate; safecall;
    procedure put_SigningCertificate(value: Certificates_ICertificate); safecall;
    function get_AttestationCredentialCertificate: Certificates_ICertificate; safecall;
    procedure put_AttestationCredentialCertificate(value: Certificates_ICertificate); safecall;
    property AttestationCredentialCertificate: Certificates_ICertificate read get_AttestationCredentialCertificate write put_AttestationCredentialCertificate;
    property SigningCertificate: Certificates_ICertificate read get_SigningCertificate write put_SigningCertificate;
    property SmartcardReaderName: HSTRING read get_SmartcardReaderName write put_SmartcardReaderName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties3
  Certificates_ICertificateRequestProperties3 = interface(IInspectable)
  ['{E687F616-734D-46B1-9D4C-6EDFDBFC845B}']
    function get_CurveName: HSTRING; safecall;
    procedure put_CurveName(value: HSTRING); safecall;
    function get_CurveParameters(resultSize: Cardinal; resultValue: PByte): HRESULT; stdcall;
    procedure put_CurveParameters(valueSize: Cardinal; value: PByte); safecall;
    function get_ContainerNamePrefix: HSTRING; safecall;
    procedure put_ContainerNamePrefix(value: HSTRING); safecall;
    function get_ContainerName: HSTRING; safecall;
    procedure put_ContainerName(value: HSTRING); safecall;
    function get_UseExistingKey: Boolean; safecall;
    procedure put_UseExistingKey(value: Boolean); safecall;
    property ContainerName: HSTRING read get_ContainerName write put_ContainerName;
    property ContainerNamePrefix: HSTRING read get_ContainerNamePrefix write put_ContainerNamePrefix;
    property CurveName: HSTRING read get_CurveName write put_CurveName;
    // property CurveParameters: PByte read get_CurveParameters write put_CurveParameters;
    property UseExistingKey: Boolean read get_UseExistingKey write put_UseExistingKey;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IIterator_1__Certificates_ICertificateExtension_Base = interface(IInspectable)
  ['{5E5AF982-332D-54EC-8E54-E62C1A1EACE9}']
    function get_Current: Certificates_ICertificateExtension; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCertificates_ICertificateExtension): Cardinal; safecall;
    property Current: Certificates_ICertificateExtension read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IIterator_1__Certificates_ICertificateExtension = interface(IIterator_1__Certificates_ICertificateExtension_Base)
  ['{BEA422C3-E307-5F42-9BEF-A0853E3314AF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IIterable_1__Certificates_ICertificateExtension_Base = interface(IInspectable)
  ['{1BDD7127-73B3-5192-8BDE-20C136281260}']
    function First: IIterator_1__Certificates_ICertificateExtension; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IIterable_1__Certificates_ICertificateExtension = interface(IIterable_1__Certificates_ICertificateExtension_Base)
  ['{9D4F36EE-6735-5ECD-86FE-436E890F6220}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IVectorView_1__Certificates_ICertificateExtension = interface(IInspectable)
  ['{DB6B3C5B-7F00-57CB-AEF0-162C0961053D}']
    function GetAt(index: Cardinal): Certificates_ICertificateExtension; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Certificates_ICertificateExtension; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ICertificateExtension): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IVector_1__Certificates_ICertificateExtension_Base = interface(IInspectable)
  ['{4C2523E8-9773-50FE-B870-483FD8B906DC}']
    function GetAt(index: Cardinal): Certificates_ICertificateExtension; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Certificates_ICertificateExtension; safecall;
    function IndexOf(value: Certificates_ICertificateExtension; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Certificates_ICertificateExtension); safecall;
    procedure InsertAt(index: Cardinal; value: Certificates_ICertificateExtension); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Certificates_ICertificateExtension); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ICertificateExtension): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCertificates_ICertificateExtension); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Cryptography.Certificates.ICertificateExtension>
  IVector_1__Certificates_ICertificateExtension = interface(IVector_1__Certificates_ICertificateExtension_Base)
  ['{1F8CFC9E-8728-58B4-918C-B76AE9F0D50F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateRequestProperties4
  Certificates_ICertificateRequestProperties4 = interface(IInspectable)
  ['{4E429AD2-1C61-4FEA-B8FE-135FB19CDCE4}']
    function get_SuppressedDefaults: IVector_1__HSTRING; safecall;
    function get_SubjectAlternativeName: Certificates_ISubjectAlternativeNameInfo; safecall;
    function get_Extensions: IVector_1__Certificates_ICertificateExtension; safecall;
    property Extensions: IVector_1__Certificates_ICertificateExtension read get_Extensions;
    property SubjectAlternativeName: Certificates_ISubjectAlternativeNameInfo read get_SubjectAlternativeName;
    property SuppressedDefaults: IVector_1__HSTRING read get_SuppressedDefaults;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateStore
  Certificates_ICertificateStore = interface(IInspectable)
  ['{B0BFF720-344E-4331-AF14-A7F7A7EBC93A}']
    procedure Add(certificate: Certificates_ICertificate); safecall;
    procedure Delete(certificate: Certificates_ICertificate); safecall;
  end;

  // Windows.Security.Cryptography.Certificates.ICertificateStore2
  Certificates_ICertificateStore2 = interface(IInspectable)
  ['{C7E68E4A-417D-4D1A-BABD-15687E549974}']
    function get_Name: HSTRING; safecall;
    property Name: HSTRING read get_Name;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate_Delegate_Base = interface(IUnknown)
  ['{1896FAEE-23E2-59CA-9802-0F48EED98EF4}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Certificates_ICertificate; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate_Delegate_Base)
  ['{51129186-5D3A-56FB-BD45-9CA0951CEA8A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>>
  IAsyncOperation_1__IVectorView_1__Certificates_ICertificate_Base = interface(IInspectable)
  ['{9B26648E-B32F-5909-A635-78E6D3BB4067}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate; safecall;
    function GetResults: IVectorView_1__Certificates_ICertificate; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Certificates_ICertificate read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICertificate>>
  IAsyncOperation_1__IVectorView_1__Certificates_ICertificate = interface(IAsyncOperation_1__IVectorView_1__Certificates_ICertificate_Base)
  ['{720719F8-F0AE-5DE5-9216-C7CBCE599D0D}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateStoresStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateStores)]
  Certificates_ICertificateStoresStatics = interface(IInspectable)
  ['{FBECC739-C6FE-4DE7-99CF-74C3E596E032}']
    function FindAllAsync: IAsyncOperation_1__IVectorView_1__Certificates_ICertificate; overload; safecall;
    function FindAllAsync(query: Certificates_ICertificateQuery): IAsyncOperation_1__IVectorView_1__Certificates_ICertificate; overload; safecall;
    function get_TrustedRootCertificationAuthorities: Certificates_ICertificateStore; safecall;
    function get_IntermediateCertificationAuthorities: Certificates_ICertificateStore; safecall;
    function GetStoreByName(storeName: HSTRING): Certificates_ICertificateStore; safecall;
    property IntermediateCertificationAuthorities: Certificates_ICertificateStore read get_IntermediateCertificationAuthorities;
    property TrustedRootCertificationAuthorities: Certificates_ICertificateStore read get_TrustedRootCertificationAuthorities;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IUserCertificateStore
  Certificates_IUserCertificateStore = interface(IInspectable)
  ['{C9FB1D83-789F-4B4E-9180-045A757AAC6D}']
    function RequestAddAsync(certificate: Certificates_ICertificate): IAsyncOperation_1__Boolean; safecall;
    function RequestDeleteAsync(certificate: Certificates_ICertificate): IAsyncOperation_1__Boolean; safecall;
    function get_Name: HSTRING; safecall;
    property Name: HSTRING read get_Name;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICertificateStoresStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CertificateStores)]
  Certificates_ICertificateStoresStatics2 = interface(IInspectable)
  ['{FA900B79-A0D4-4B8C-BC55-C0A37EB141ED}']
    function GetUserStoreByName(storeName: HSTRING): Certificates_IUserCertificateStore; safecall;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsTimestampInfo
  Certificates_ICmsTimestampInfo = interface(IInspectable)
  ['{2F5F00F2-2C18-4F88-8435-C534086076F5}']
    function get_SigningCertificate: Certificates_ICertificate; safecall;
    function get_Certificates: IVectorView_1__Certificates_ICertificate; safecall;
    function get_Timestamp: DateTime; safecall;
    property Certificates: IVectorView_1__Certificates_ICertificate read get_Certificates;
    property SigningCertificate: Certificates_ICertificate read get_SigningCertificate;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsSignerInfo
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsSignerInfo)]
  Certificates_ICmsSignerInfo = interface(IInspectable)
  ['{50D020DB-1D2F-4C1A-B5C5-D0188FF91F47}']
    function get_Certificate: Certificates_ICertificate; safecall;
    procedure put_Certificate(value: Certificates_ICertificate); safecall;
    function get_HashAlgorithmName: HSTRING; safecall;
    procedure put_HashAlgorithmName(value: HSTRING); safecall;
    function get_TimestampInfo: Certificates_ICmsTimestampInfo; safecall;
    property Certificate: Certificates_ICertificate read get_Certificate write put_Certificate;
    property HashAlgorithmName: HSTRING read get_HashAlgorithmName write put_HashAlgorithmName;
    property TimestampInfo: Certificates_ICmsTimestampInfo read get_TimestampInfo;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IIterator_1__Certificates_ICmsSignerInfo_Base = interface(IInspectable)
  ['{BA691628-D419-5E0A-B924-03EBC236B11E}']
    function get_Current: Certificates_ICmsSignerInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCertificates_ICmsSignerInfo): Cardinal; safecall;
    property Current: Certificates_ICmsSignerInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IIterator_1__Certificates_ICmsSignerInfo = interface(IIterator_1__Certificates_ICmsSignerInfo_Base)
  ['{183B6AAA-A097-5128-B7AA-2DCCB7E48633}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IIterable_1__Certificates_ICmsSignerInfo_Base = interface(IInspectable)
  ['{6AF24174-2DDA-5A54-A0B9-4D6690059427}']
    function First: IIterator_1__Certificates_ICmsSignerInfo; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IIterable_1__Certificates_ICmsSignerInfo = interface(IIterable_1__Certificates_ICmsSignerInfo_Base)
  ['{33D54C83-7E0F-5ED6-BEEE-743FEA24AC25}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Cryptography.Certificates.ICmsSignerInfo>
  IVectorView_1__Certificates_ICmsSignerInfo = interface(IInspectable)
  ['{58AA114E-13A8-50F9-A484-A6C0D265E5C0}']
    function GetAt(index: Cardinal): Certificates_ICmsSignerInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Certificates_ICmsSignerInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCertificates_ICmsSignerInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsAttachedSignature
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsAttachedSignature)]
  Certificates_ICmsAttachedSignature = interface(IInspectable)
  ['{61899D9D-3757-4ECB-BDDC-0CA357D7A936}']
    function get_Certificates: IVectorView_1__Certificates_ICertificate; safecall;
    function get_Content(resultSize: Cardinal; resultValue: PByte): HRESULT; stdcall;
    function get_Signers: IVectorView_1__Certificates_ICmsSignerInfo; safecall;
    function VerifySignature: Certificates_SignatureValidationResult; safecall;
    property Certificates: IVectorView_1__Certificates_ICertificate read get_Certificates;
    // property Content: PByte read get_Content;
    property Signers: IVectorView_1__Certificates_ICmsSignerInfo read get_Signers;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureFactory
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsAttachedSignature)]
  Certificates_ICmsAttachedSignatureFactory = interface(IInspectable)
  ['{D0C8FC15-F757-4C64-A362-52CC1C77CFFB}']
    function CreateCmsAttachedSignature(inputBlob: IBuffer): Certificates_ICmsAttachedSignature; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsAttachedSignature)]
  Certificates_ICmsAttachedSignatureStatics = interface(IInspectable)
  ['{87989C8E-B0AD-498D-A7F5-78B59BCE4B36}']
    function GenerateSignatureAsync(data: IBuffer; signers: IIterable_1__Certificates_ICmsSignerInfo; certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__IBuffer; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Certificates.SignatureValidationResult>
  AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult_Delegate_Base = interface(IUnknown)
  ['{DFF50005-78AD-5F4F-A085-CB614A674A25}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Certificates_SignatureValidationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Cryptography.Certificates.SignatureValidationResult>
  AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult = interface(AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Certificates.SignatureValidationResult>
  IAsyncOperation_1__Certificates_SignatureValidationResult_Base = interface(IInspectable)
  ['{F09C0BCF-CE3B-5DFF-971F-2C3FFE6FD04D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult; safecall;
    function GetResults: Certificates_SignatureValidationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Certificates_SignatureValidationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Cryptography.Certificates.SignatureValidationResult>
  IAsyncOperation_1__Certificates_SignatureValidationResult = interface(IAsyncOperation_1__Certificates_SignatureValidationResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsDetachedSignature
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsDetachedSignature)]
  Certificates_ICmsDetachedSignature = interface(IInspectable)
  ['{0F1EF154-F65E-4536-8339-5944081DB2CA}']
    function get_Certificates: IVectorView_1__Certificates_ICertificate; safecall;
    function get_Signers: IVectorView_1__Certificates_ICmsSignerInfo; safecall;
    function VerifySignatureAsync(data: IInputStream): IAsyncOperation_1__Certificates_SignatureValidationResult; safecall;
    property Certificates: IVectorView_1__Certificates_ICertificate read get_Certificates;
    property Signers: IVectorView_1__Certificates_ICmsSignerInfo read get_Signers;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureFactory
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsDetachedSignature)]
  Certificates_ICmsDetachedSignatureFactory = interface(IInspectable)
  ['{C4AB3503-AE7F-4387-AD19-00F150E48EBB}']
    function CreateCmsDetachedSignature(inputBlob: IBuffer): Certificates_ICmsDetachedSignature; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_CmsDetachedSignature)]
  Certificates_ICmsDetachedSignatureStatics = interface(IInspectable)
  ['{3D114CFD-BF9B-4682-9BE6-91F57C053808}']
    function GenerateSignatureAsync(data: IInputStream; signers: IIterable_1__Certificates_ICmsSignerInfo; certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__IBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IKeyAlgorithmNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_KeyAlgorithmNames)]
  Certificates_IKeyAlgorithmNamesStatics = interface(IInspectable)
  ['{479065D7-7AC7-4581-8C3B-D07027140448}']
    function get_Rsa: HSTRING; safecall;
    function get_Dsa: HSTRING; safecall;
    function get_Ecdh256: HSTRING; safecall;
    function get_Ecdh384: HSTRING; safecall;
    function get_Ecdh521: HSTRING; safecall;
    function get_Ecdsa256: HSTRING; safecall;
    function get_Ecdsa384: HSTRING; safecall;
    function get_Ecdsa521: HSTRING; safecall;
    property Dsa: HSTRING read get_Dsa;
    property Ecdh256: HSTRING read get_Ecdh256;
    property Ecdh384: HSTRING read get_Ecdh384;
    property Ecdh521: HSTRING read get_Ecdh521;
    property Ecdsa256: HSTRING read get_Ecdsa256;
    property Ecdsa384: HSTRING read get_Ecdsa384;
    property Ecdsa521: HSTRING read get_Ecdsa521;
    property Rsa: HSTRING read get_Rsa;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IKeyAlgorithmNamesStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_KeyAlgorithmNames)]
  Certificates_IKeyAlgorithmNamesStatics2 = interface(IInspectable)
  ['{C99B5686-E1FD-4A4A-893D-A26F33DD8BB4}']
    function get_Ecdsa: HSTRING; safecall;
    function get_Ecdh: HSTRING; safecall;
    property Ecdh: HSTRING read get_Ecdh;
    property Ecdsa: HSTRING read get_Ecdsa;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IKeyAttestationHelperStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_KeyAttestationHelper)]
  Certificates_IKeyAttestationHelperStatics = interface(IInspectable)
  ['{1648E246-F644-4326-88BE-3AF102D30E0C}']
    function DecryptTpmAttestationCredentialAsync(credential: HSTRING): IAsyncOperation_1__HSTRING; safecall;
    function GetTpmAttestationCredentialId(credential: HSTRING): HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IKeyAttestationHelperStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_KeyAttestationHelper)]
  Certificates_IKeyAttestationHelperStatics2 = interface(IInspectable)
  ['{9C590B2C-A6C6-4A5E-9E64-E85D5279DF97}']
    function DecryptTpmAttestationCredentialAsync(credential: HSTRING; containerName: HSTRING): IAsyncOperation_1__HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IKeyStorageProviderNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_KeyStorageProviderNames)]
  Certificates_IKeyStorageProviderNamesStatics = interface(IInspectable)
  ['{AF186AE0-5529-4602-BD94-0AAB91957B5C}']
    function get_SoftwareKeyStorageProvider: HSTRING; safecall;
    function get_SmartcardKeyStorageProvider: HSTRING; safecall;
    function get_PlatformKeyStorageProvider: HSTRING; safecall;
    property PlatformKeyStorageProvider: HSTRING read get_PlatformKeyStorageProvider;
    property SmartcardKeyStorageProvider: HSTRING read get_SmartcardKeyStorageProvider;
    property SoftwareKeyStorageProvider: HSTRING read get_SoftwareKeyStorageProvider;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IKeyStorageProviderNamesStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_KeyStorageProviderNames)]
  Certificates_IKeyStorageProviderNamesStatics2 = interface(IInspectable)
  ['{262D743D-9C2E-41CC-8812-C4D971DD7C60}']
    function get_PassportKeyStorageProvider: HSTRING; safecall;
    property PassportKeyStorageProvider: HSTRING read get_PassportKeyStorageProvider;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.IStandardCertificateStoreNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Certificates_StandardCertificateStoreNames)]
  Certificates_IStandardCertificateStoreNamesStatics = interface(IInspectable)
  ['{0C154ADB-A496-41F8-8FE5-9E96F36EFBF8}']
    function get_Personal: HSTRING; safecall;
    function get_TrustedRootCertificationAuthorities: HSTRING; safecall;
    function get_IntermediateCertificationAuthorities: HSTRING; safecall;
    property IntermediateCertificationAuthorities: HSTRING read get_IntermediateCertificationAuthorities;
    property Personal: HSTRING read get_Personal;
    property TrustedRootCertificationAuthorities: HSTRING read get_TrustedRootCertificationAuthorities;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Certificates.ISubjectAlternativeNameInfo2
  Certificates_ISubjectAlternativeNameInfo2 = interface(IInspectable)
  ['{437A78C6-1C51-41EA-B34A-3D654398A370}']
    function get_EmailNames: IVector_1__HSTRING; safecall;
    function get_IPAddresses: IVector_1__HSTRING; safecall;
    function get_Urls: IVector_1__HSTRING; safecall;
    function get_DnsNames: IVector_1__HSTRING; safecall;
    function get_DistinguishedNames: IVector_1__HSTRING; safecall;
    function get_PrincipalNames: IVector_1__HSTRING; safecall;
    function get_Extension: Certificates_ICertificateExtension; safecall;
    property DistinguishedNames: IVector_1__HSTRING read get_DistinguishedNames;
    property DnsNames: IVector_1__HSTRING read get_DnsNames;
    property EmailNames: IVector_1__HSTRING read get_EmailNames;
    property Extension: Certificates_ICertificateExtension read get_Extension;
    property IPAddresses: IVector_1__HSTRING read get_IPAddresses;
    property PrincipalNames: IVector_1__HSTRING read get_PrincipalNames;
    property Urls: IVector_1__HSTRING read get_Urls;
  end;

  // Windows.Security.Cryptography.Certificates.IUserCertificateEnrollmentManager2
  Certificates_IUserCertificateEnrollmentManager2 = interface(IInspectable)
  ['{0DAD9CB1-65DE-492A-B86D-FC5C482C3747}']
    function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; pfxImportParameters: Certificates_IPfxImportParameters): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IAsymmetricAlgorithmNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_AsymmetricAlgorithmNames)]
  Core_IAsymmetricAlgorithmNamesStatics = interface(IInspectable)
  ['{CAF6FCE4-67C0-46AA-84F9-752E77449F9B}']
    function get_RsaPkcs1: HSTRING; safecall;
    function get_RsaOaepSha1: HSTRING; safecall;
    function get_RsaOaepSha256: HSTRING; safecall;
    function get_RsaOaepSha384: HSTRING; safecall;
    function get_RsaOaepSha512: HSTRING; safecall;
    function get_EcdsaP256Sha256: HSTRING; safecall;
    function get_EcdsaP384Sha384: HSTRING; safecall;
    function get_EcdsaP521Sha512: HSTRING; safecall;
    function get_DsaSha1: HSTRING; safecall;
    function get_DsaSha256: HSTRING; safecall;
    function get_RsaSignPkcs1Sha1: HSTRING; safecall;
    function get_RsaSignPkcs1Sha256: HSTRING; safecall;
    function get_RsaSignPkcs1Sha384: HSTRING; safecall;
    function get_RsaSignPkcs1Sha512: HSTRING; safecall;
    function get_RsaSignPssSha1: HSTRING; safecall;
    function get_RsaSignPssSha256: HSTRING; safecall;
    function get_RsaSignPssSha384: HSTRING; safecall;
    function get_RsaSignPssSha512: HSTRING; safecall;
    property DsaSha1: HSTRING read get_DsaSha1;
    property DsaSha256: HSTRING read get_DsaSha256;
    property EcdsaP256Sha256: HSTRING read get_EcdsaP256Sha256;
    property EcdsaP384Sha384: HSTRING read get_EcdsaP384Sha384;
    property EcdsaP521Sha512: HSTRING read get_EcdsaP521Sha512;
    property RsaOaepSha1: HSTRING read get_RsaOaepSha1;
    property RsaOaepSha256: HSTRING read get_RsaOaepSha256;
    property RsaOaepSha384: HSTRING read get_RsaOaepSha384;
    property RsaOaepSha512: HSTRING read get_RsaOaepSha512;
    property RsaPkcs1: HSTRING read get_RsaPkcs1;
    property RsaSignPkcs1Sha1: HSTRING read get_RsaSignPkcs1Sha1;
    property RsaSignPkcs1Sha256: HSTRING read get_RsaSignPkcs1Sha256;
    property RsaSignPkcs1Sha384: HSTRING read get_RsaSignPkcs1Sha384;
    property RsaSignPkcs1Sha512: HSTRING read get_RsaSignPkcs1Sha512;
    property RsaSignPssSha1: HSTRING read get_RsaSignPssSha1;
    property RsaSignPssSha256: HSTRING read get_RsaSignPssSha256;
    property RsaSignPssSha384: HSTRING read get_RsaSignPssSha384;
    property RsaSignPssSha512: HSTRING read get_RsaSignPssSha512;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IAsymmetricAlgorithmNamesStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_AsymmetricAlgorithmNames)]
  Core_IAsymmetricAlgorithmNamesStatics2 = interface(IInspectable)
  ['{F141C0D6-4BFF-4F23-BA66-6045B137D5DF}']
    function get_EcdsaSha256: HSTRING; safecall;
    function get_EcdsaSha384: HSTRING; safecall;
    function get_EcdsaSha512: HSTRING; safecall;
    property EcdsaSha256: HSTRING read get_EcdsaSha256;
    property EcdsaSha384: HSTRING read get_EcdsaSha384;
    property EcdsaSha512: HSTRING read get_EcdsaSha512;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProvider
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_AsymmetricKeyAlgorithmProvider)]
  Core_IAsymmetricKeyAlgorithmProvider = interface(IInspectable)
  ['{E8D2FF37-6259-4E88-B7E0-94191FDE699E}']
    function get_AlgorithmName: HSTRING; safecall;
    function CreateKeyPair(keySize: Cardinal): Core_ICryptographicKey; safecall;
    function ImportKeyPair(keyBlob: IBuffer): Core_ICryptographicKey; overload; safecall;
    function ImportKeyPair(keyBlob: IBuffer; BlobType: Core_CryptographicPrivateKeyBlobType): Core_ICryptographicKey; overload; safecall;
    function ImportPublicKey(keyBlob: IBuffer): Core_ICryptographicKey; overload; safecall;
    function ImportPublicKey(keyBlob: IBuffer; BlobType: Core_CryptographicPublicKeyBlobType): Core_ICryptographicKey; overload; safecall;
    property AlgorithmName: HSTRING read get_AlgorithmName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProvider2
  Core_IAsymmetricKeyAlgorithmProvider2 = interface(IInspectable)
  ['{4E322A7E-7C4D-4997-AC4F-1B848B36306E}']
    function CreateKeyPairWithCurveName(curveName: HSTRING): Core_ICryptographicKey; safecall;
    function CreateKeyPairWithCurveParameters(parametersSize: Cardinal; parameters: PByte): Core_ICryptographicKey; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProviderStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_AsymmetricKeyAlgorithmProvider)]
  Core_IAsymmetricKeyAlgorithmProviderStatics = interface(IInspectable)
  ['{425BDE18-A7F3-47A6-A8D2-C48D6033A65C}']
    function OpenAlgorithm(algorithm: HSTRING): Core_IAsymmetricKeyAlgorithmProvider; safecall;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IEncryptedAndAuthenticatedData
  Core_IEncryptedAndAuthenticatedData = interface(IInspectable)
  ['{6FA42FE7-1ECB-4B00-BEA5-60B83F862F17}']
    function get_EncryptedData: IBuffer; safecall;
    function get_AuthenticationTag: IBuffer; safecall;
    property AuthenticationTag: IBuffer read get_AuthenticationTag;
    property EncryptedData: IBuffer read get_EncryptedData;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationParameters
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationParameters)]
  Core_IKeyDerivationParameters = interface(IInspectable)
  ['{7BF05967-047B-4A8C-964A-469FFD5522E2}']
    function get_KdfGenericBinary: IBuffer; safecall;
    procedure put_KdfGenericBinary(value: IBuffer); safecall;
    function get_IterationCount: Cardinal; safecall;
    property IterationCount: Cardinal read get_IterationCount;
    property KdfGenericBinary: IBuffer read get_KdfGenericBinary write put_KdfGenericBinary;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.ICryptographicEngineStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_CryptographicEngine)]
  Core_ICryptographicEngineStatics = interface(IInspectable)
  ['{9FEA0639-6FF7-4C85-A095-95EB31715EB9}']
    function Encrypt(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IBuffer; safecall;
    function Decrypt(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IBuffer; safecall;
    function EncryptAndAuthenticate(key: Core_ICryptographicKey; data: IBuffer; nonce: IBuffer; authenticatedData: IBuffer): Core_IEncryptedAndAuthenticatedData; safecall;
    function DecryptAndAuthenticate(key: Core_ICryptographicKey; data: IBuffer; nonce: IBuffer; authenticationTag: IBuffer; authenticatedData: IBuffer): IBuffer; safecall;
    function Sign(key: Core_ICryptographicKey; data: IBuffer): IBuffer; safecall;
    function VerifySignature(key: Core_ICryptographicKey; data: IBuffer; signature: IBuffer): Boolean; safecall;
    function DeriveKeyMaterial(key: Core_ICryptographicKey; parameters: Core_IKeyDerivationParameters; desiredKeySize: Cardinal): IBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.ICryptographicEngineStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_CryptographicEngine)]
  Core_ICryptographicEngineStatics2 = interface(IInspectable)
  ['{675948FE-DF9F-4191-92C7-6CE6F58420E0}']
    function SignHashedData(key: Core_ICryptographicKey; data: IBuffer): IBuffer; safecall;
    function VerifySignatureWithHashInput(key: Core_ICryptographicKey; data: IBuffer; signature: IBuffer): Boolean; safecall;
    function DecryptAsync(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IAsyncOperation_1__IBuffer; safecall;
    function SignAsync(key: Core_ICryptographicKey; data: IBuffer): IAsyncOperation_1__IBuffer; safecall;
    function SignHashedDataAsync(key: Core_ICryptographicKey; data: IBuffer): IAsyncOperation_1__IBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IEccCurveNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_EccCurveNames)]
  Core_IEccCurveNamesStatics = interface(IInspectable)
  ['{B3FF930C-AEEB-409E-B7D4-9B95295AAECF}']
    function get_BrainpoolP160r1: HSTRING; safecall;
    function get_BrainpoolP160t1: HSTRING; safecall;
    function get_BrainpoolP192r1: HSTRING; safecall;
    function get_BrainpoolP192t1: HSTRING; safecall;
    function get_BrainpoolP224r1: HSTRING; safecall;
    function get_BrainpoolP224t1: HSTRING; safecall;
    function get_BrainpoolP256r1: HSTRING; safecall;
    function get_BrainpoolP256t1: HSTRING; safecall;
    function get_BrainpoolP320r1: HSTRING; safecall;
    function get_BrainpoolP320t1: HSTRING; safecall;
    function get_BrainpoolP384r1: HSTRING; safecall;
    function get_BrainpoolP384t1: HSTRING; safecall;
    function get_BrainpoolP512r1: HSTRING; safecall;
    function get_BrainpoolP512t1: HSTRING; safecall;
    function get_Curve25519: HSTRING; safecall;
    function get_Ec192wapi: HSTRING; safecall;
    function get_NistP192: HSTRING; safecall;
    function get_NistP224: HSTRING; safecall;
    function get_NistP256: HSTRING; safecall;
    function get_NistP384: HSTRING; safecall;
    function get_NistP521: HSTRING; safecall;
    function get_NumsP256t1: HSTRING; safecall;
    function get_NumsP384t1: HSTRING; safecall;
    function get_NumsP512t1: HSTRING; safecall;
    function get_SecP160k1: HSTRING; safecall;
    function get_SecP160r1: HSTRING; safecall;
    function get_SecP160r2: HSTRING; safecall;
    function get_SecP192k1: HSTRING; safecall;
    function get_SecP192r1: HSTRING; safecall;
    function get_SecP224k1: HSTRING; safecall;
    function get_SecP224r1: HSTRING; safecall;
    function get_SecP256k1: HSTRING; safecall;
    function get_SecP256r1: HSTRING; safecall;
    function get_SecP384r1: HSTRING; safecall;
    function get_SecP521r1: HSTRING; safecall;
    function get_Wtls7: HSTRING; safecall;
    function get_Wtls9: HSTRING; safecall;
    function get_Wtls12: HSTRING; safecall;
    function get_X962P192v1: HSTRING; safecall;
    function get_X962P192v2: HSTRING; safecall;
    function get_X962P192v3: HSTRING; safecall;
    function get_X962P239v1: HSTRING; safecall;
    function get_X962P239v2: HSTRING; safecall;
    function get_X962P239v3: HSTRING; safecall;
    function get_X962P256v1: HSTRING; safecall;
    function get_AllEccCurveNames: IVectorView_1__HSTRING; safecall;
    property AllEccCurveNames: IVectorView_1__HSTRING read get_AllEccCurveNames;
    property BrainpoolP160r1: HSTRING read get_BrainpoolP160r1;
    property BrainpoolP160t1: HSTRING read get_BrainpoolP160t1;
    property BrainpoolP192r1: HSTRING read get_BrainpoolP192r1;
    property BrainpoolP192t1: HSTRING read get_BrainpoolP192t1;
    property BrainpoolP224r1: HSTRING read get_BrainpoolP224r1;
    property BrainpoolP224t1: HSTRING read get_BrainpoolP224t1;
    property BrainpoolP256r1: HSTRING read get_BrainpoolP256r1;
    property BrainpoolP256t1: HSTRING read get_BrainpoolP256t1;
    property BrainpoolP320r1: HSTRING read get_BrainpoolP320r1;
    property BrainpoolP320t1: HSTRING read get_BrainpoolP320t1;
    property BrainpoolP384r1: HSTRING read get_BrainpoolP384r1;
    property BrainpoolP384t1: HSTRING read get_BrainpoolP384t1;
    property BrainpoolP512r1: HSTRING read get_BrainpoolP512r1;
    property BrainpoolP512t1: HSTRING read get_BrainpoolP512t1;
    property Curve25519: HSTRING read get_Curve25519;
    property Ec192wapi: HSTRING read get_Ec192wapi;
    property NistP192: HSTRING read get_NistP192;
    property NistP224: HSTRING read get_NistP224;
    property NistP256: HSTRING read get_NistP256;
    property NistP384: HSTRING read get_NistP384;
    property NistP521: HSTRING read get_NistP521;
    property NumsP256t1: HSTRING read get_NumsP256t1;
    property NumsP384t1: HSTRING read get_NumsP384t1;
    property NumsP512t1: HSTRING read get_NumsP512t1;
    property SecP160k1: HSTRING read get_SecP160k1;
    property SecP160r1: HSTRING read get_SecP160r1;
    property SecP160r2: HSTRING read get_SecP160r2;
    property SecP192k1: HSTRING read get_SecP192k1;
    property SecP192r1: HSTRING read get_SecP192r1;
    property SecP224k1: HSTRING read get_SecP224k1;
    property SecP224r1: HSTRING read get_SecP224r1;
    property SecP256k1: HSTRING read get_SecP256k1;
    property SecP256r1: HSTRING read get_SecP256r1;
    property SecP384r1: HSTRING read get_SecP384r1;
    property SecP521r1: HSTRING read get_SecP521r1;
    property Wtls12: HSTRING read get_Wtls12;
    property Wtls7: HSTRING read get_Wtls7;
    property Wtls9: HSTRING read get_Wtls9;
    property X962P192v1: HSTRING read get_X962P192v1;
    property X962P192v2: HSTRING read get_X962P192v2;
    property X962P192v3: HSTRING read get_X962P192v3;
    property X962P239v1: HSTRING read get_X962P239v1;
    property X962P239v2: HSTRING read get_X962P239v2;
    property X962P239v3: HSTRING read get_X962P239v3;
    property X962P256v1: HSTRING read get_X962P256v1;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IHashAlgorithmNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_HashAlgorithmNames)]
  Core_IHashAlgorithmNamesStatics = interface(IInspectable)
  ['{6B5E0516-DE96-4F0A-8D57-DCC9DAE36C76}']
    function get_Md5: HSTRING; safecall;
    function get_Sha1: HSTRING; safecall;
    function get_Sha256: HSTRING; safecall;
    function get_Sha384: HSTRING; safecall;
    function get_Sha512: HSTRING; safecall;
    property Md5: HSTRING read get_Md5;
    property Sha1: HSTRING read get_Sha1;
    property Sha256: HSTRING read get_Sha256;
    property Sha384: HSTRING read get_Sha384;
    property Sha512: HSTRING read get_Sha512;
  end;

  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IHashComputation
  Core_IHashComputation = interface(IInspectable)
  ['{5904D1B6-AD31-4603-A3A4-B1BDA98E2562}']
    procedure Append(data: IBuffer); safecall;
    function GetValueAndReset: IBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IHashAlgorithmProvider
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_HashAlgorithmProvider)]
  Core_IHashAlgorithmProvider = interface(IInspectable)
  ['{BE9B3080-B2C3-422B-BCE1-EC90EFB5D7B5}']
    function get_AlgorithmName: HSTRING; safecall;
    function get_HashLength: Cardinal; safecall;
    function HashData(data: IBuffer): IBuffer; safecall;
    function CreateHash: Core_IHashComputation; safecall;
    property AlgorithmName: HSTRING read get_AlgorithmName;
    property HashLength: Cardinal read get_HashLength;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IHashAlgorithmProviderStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_HashAlgorithmProvider)]
  Core_IHashAlgorithmProviderStatics = interface(IInspectable)
  ['{9FAC9741-5CC4-4336-AE38-6212B75A915A}']
    function OpenAlgorithm(algorithm: HSTRING): Core_IHashAlgorithmProvider; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationAlgorithmNames)]
  Core_IKeyDerivationAlgorithmNamesStatics = interface(IInspectable)
  ['{7B6E363E-94D2-4739-A57B-022E0C3A402A}']
    function get_Pbkdf2Md5: HSTRING; safecall;
    function get_Pbkdf2Sha1: HSTRING; safecall;
    function get_Pbkdf2Sha256: HSTRING; safecall;
    function get_Pbkdf2Sha384: HSTRING; safecall;
    function get_Pbkdf2Sha512: HSTRING; safecall;
    function get_Sp800108CtrHmacMd5: HSTRING; safecall;
    function get_Sp800108CtrHmacSha1: HSTRING; safecall;
    function get_Sp800108CtrHmacSha256: HSTRING; safecall;
    function get_Sp800108CtrHmacSha384: HSTRING; safecall;
    function get_Sp800108CtrHmacSha512: HSTRING; safecall;
    function get_Sp80056aConcatMd5: HSTRING; safecall;
    function get_Sp80056aConcatSha1: HSTRING; safecall;
    function get_Sp80056aConcatSha256: HSTRING; safecall;
    function get_Sp80056aConcatSha384: HSTRING; safecall;
    function get_Sp80056aConcatSha512: HSTRING; safecall;
    property Pbkdf2Md5: HSTRING read get_Pbkdf2Md5;
    property Pbkdf2Sha1: HSTRING read get_Pbkdf2Sha1;
    property Pbkdf2Sha256: HSTRING read get_Pbkdf2Sha256;
    property Pbkdf2Sha384: HSTRING read get_Pbkdf2Sha384;
    property Pbkdf2Sha512: HSTRING read get_Pbkdf2Sha512;
    property Sp800108CtrHmacMd5: HSTRING read get_Sp800108CtrHmacMd5;
    property Sp800108CtrHmacSha1: HSTRING read get_Sp800108CtrHmacSha1;
    property Sp800108CtrHmacSha256: HSTRING read get_Sp800108CtrHmacSha256;
    property Sp800108CtrHmacSha384: HSTRING read get_Sp800108CtrHmacSha384;
    property Sp800108CtrHmacSha512: HSTRING read get_Sp800108CtrHmacSha512;
    property Sp80056aConcatMd5: HSTRING read get_Sp80056aConcatMd5;
    property Sp80056aConcatSha1: HSTRING read get_Sp80056aConcatSha1;
    property Sp80056aConcatSha256: HSTRING read get_Sp80056aConcatSha256;
    property Sp80056aConcatSha384: HSTRING read get_Sp80056aConcatSha384;
    property Sp80056aConcatSha512: HSTRING read get_Sp80056aConcatSha512;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmNamesStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationAlgorithmNames)]
  Core_IKeyDerivationAlgorithmNamesStatics2 = interface(IInspectable)
  ['{57953FAB-6044-466F-97F4-337B7808384D}']
    function get_CapiKdfMd5: HSTRING; safecall;
    function get_CapiKdfSha1: HSTRING; safecall;
    function get_CapiKdfSha256: HSTRING; safecall;
    function get_CapiKdfSha384: HSTRING; safecall;
    function get_CapiKdfSha512: HSTRING; safecall;
    property CapiKdfMd5: HSTRING read get_CapiKdfMd5;
    property CapiKdfSha1: HSTRING read get_CapiKdfSha1;
    property CapiKdfSha256: HSTRING read get_CapiKdfSha256;
    property CapiKdfSha384: HSTRING read get_CapiKdfSha384;
    property CapiKdfSha512: HSTRING read get_CapiKdfSha512;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmProvider
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationAlgorithmProvider)]
  Core_IKeyDerivationAlgorithmProvider = interface(IInspectable)
  ['{E1FBA83B-4671-43B7-9158-763AAA98B6BF}']
    function get_AlgorithmName: HSTRING; safecall;
    function CreateKey(keyMaterial: IBuffer): Core_ICryptographicKey; safecall;
    property AlgorithmName: HSTRING read get_AlgorithmName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmProviderStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationAlgorithmProvider)]
  Core_IKeyDerivationAlgorithmProviderStatics = interface(IInspectable)
  ['{0A22097A-0A1C-443B-9418-B9498AEB1603}']
    function OpenAlgorithm(algorithm: HSTRING): Core_IKeyDerivationAlgorithmProvider; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationParameters2
  Core_IKeyDerivationParameters2 = interface(IInspectable)
  ['{CD4166D1-417E-4F4C-B666-C0D879F3F8E0}']
    function get_Capi1KdfTargetAlgorithm: Core_Capi1KdfTargetAlgorithm; safecall;
    procedure put_Capi1KdfTargetAlgorithm(value: Core_Capi1KdfTargetAlgorithm); safecall;
    property Capi1KdfTargetAlgorithm: Core_Capi1KdfTargetAlgorithm read get_Capi1KdfTargetAlgorithm write put_Capi1KdfTargetAlgorithm;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationParametersStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationParameters)]
  Core_IKeyDerivationParametersStatics = interface(IInspectable)
  ['{EA961FBE-F37F-4146-9DFE-A456F1735F4B}']
    function BuildForPbkdf2(pbkdf2Salt: IBuffer; iterationCount: Cardinal): Core_IKeyDerivationParameters; safecall;
    function BuildForSP800108(&label: IBuffer; context: IBuffer): Core_IKeyDerivationParameters; safecall;
    function BuildForSP80056a(algorithmId: IBuffer; partyUInfo: IBuffer; partyVInfo: IBuffer; suppPubInfo: IBuffer; suppPrivInfo: IBuffer): Core_IKeyDerivationParameters; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IKeyDerivationParametersStatics2
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_KeyDerivationParameters)]
  Core_IKeyDerivationParametersStatics2 = interface(IInspectable)
  ['{A5783DD5-58E3-4EFB-B283-A1653126E1BE}']
    function BuildForCapi1Kdf(capi1KdfTargetAlgorithm: Core_Capi1KdfTargetAlgorithm): Core_IKeyDerivationParameters; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IMacAlgorithmNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_MacAlgorithmNames)]
  Core_IMacAlgorithmNamesStatics = interface(IInspectable)
  ['{41412678-FB1E-43A4-895E-A9026E4390A3}']
    function get_HmacMd5: HSTRING; safecall;
    function get_HmacSha1: HSTRING; safecall;
    function get_HmacSha256: HSTRING; safecall;
    function get_HmacSha384: HSTRING; safecall;
    function get_HmacSha512: HSTRING; safecall;
    function get_AesCmac: HSTRING; safecall;
    property AesCmac: HSTRING read get_AesCmac;
    property HmacMd5: HSTRING read get_HmacMd5;
    property HmacSha1: HSTRING read get_HmacSha1;
    property HmacSha256: HSTRING read get_HmacSha256;
    property HmacSha384: HSTRING read get_HmacSha384;
    property HmacSha512: HSTRING read get_HmacSha512;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IMacAlgorithmProvider
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_MacAlgorithmProvider)]
  Core_IMacAlgorithmProvider = interface(IInspectable)
  ['{4A3FC5C3-1CBD-41CE-A092-AA0BC5D2D2F5}']
    function get_AlgorithmName: HSTRING; safecall;
    function get_MacLength: Cardinal; safecall;
    function CreateKey(keyMaterial: IBuffer): Core_ICryptographicKey; safecall;
    property AlgorithmName: HSTRING read get_AlgorithmName;
    property MacLength: Cardinal read get_MacLength;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IMacAlgorithmProvider2
  Core_IMacAlgorithmProvider2 = interface(IInspectable)
  ['{6DA32A15-D931-42ED-8E7E-C301CAEE119C}']
    function CreateHash(keyMaterial: IBuffer): Core_IHashComputation; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IMacAlgorithmProviderStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_MacAlgorithmProvider)]
  Core_IMacAlgorithmProviderStatics = interface(IInspectable)
  ['{C9BDC147-CC77-4DF0-9E4E-B921E080644C}']
    function OpenAlgorithm(algorithm: HSTRING): Core_IMacAlgorithmProvider; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.IPersistedKeyProviderStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_PersistedKeyProvider)]
  Core_IPersistedKeyProviderStatics = interface(IInspectable)
  ['{77274814-D9D4-4CF5-B668-E0457DF30894}']
    function OpenKeyPairFromCertificateAsync(certificate: Certificates_ICertificate; hashAlgorithmName: HSTRING; padding: Core_CryptographicPadding): IAsyncOperation_1__Core_ICryptographicKey; safecall;
    function OpenPublicKeyFromCertificate(certificate: Certificates_ICertificate; hashAlgorithmName: HSTRING; padding: Core_CryptographicPadding): Core_ICryptographicKey; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.ISymmetricAlgorithmNamesStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_SymmetricAlgorithmNames)]
  Core_ISymmetricAlgorithmNamesStatics = interface(IInspectable)
  ['{6870727B-C996-4EAE-84D7-79B2AEB73B9C}']
    function get_DesCbc: HSTRING; safecall;
    function get_DesEcb: HSTRING; safecall;
    function get_TripleDesCbc: HSTRING; safecall;
    function get_TripleDesEcb: HSTRING; safecall;
    function get_Rc2Cbc: HSTRING; safecall;
    function get_Rc2Ecb: HSTRING; safecall;
    function get_AesCbc: HSTRING; safecall;
    function get_AesEcb: HSTRING; safecall;
    function get_AesGcm: HSTRING; safecall;
    function get_AesCcm: HSTRING; safecall;
    function get_AesCbcPkcs7: HSTRING; safecall;
    function get_AesEcbPkcs7: HSTRING; safecall;
    function get_DesCbcPkcs7: HSTRING; safecall;
    function get_DesEcbPkcs7: HSTRING; safecall;
    function get_TripleDesCbcPkcs7: HSTRING; safecall;
    function get_TripleDesEcbPkcs7: HSTRING; safecall;
    function get_Rc2CbcPkcs7: HSTRING; safecall;
    function get_Rc2EcbPkcs7: HSTRING; safecall;
    function get_Rc4: HSTRING; safecall;
    property AesCbc: HSTRING read get_AesCbc;
    property AesCbcPkcs7: HSTRING read get_AesCbcPkcs7;
    property AesCcm: HSTRING read get_AesCcm;
    property AesEcb: HSTRING read get_AesEcb;
    property AesEcbPkcs7: HSTRING read get_AesEcbPkcs7;
    property AesGcm: HSTRING read get_AesGcm;
    property DesCbc: HSTRING read get_DesCbc;
    property DesCbcPkcs7: HSTRING read get_DesCbcPkcs7;
    property DesEcb: HSTRING read get_DesEcb;
    property DesEcbPkcs7: HSTRING read get_DesEcbPkcs7;
    property Rc2Cbc: HSTRING read get_Rc2Cbc;
    property Rc2CbcPkcs7: HSTRING read get_Rc2CbcPkcs7;
    property Rc2Ecb: HSTRING read get_Rc2Ecb;
    property Rc2EcbPkcs7: HSTRING read get_Rc2EcbPkcs7;
    property Rc4: HSTRING read get_Rc4;
    property TripleDesCbc: HSTRING read get_TripleDesCbc;
    property TripleDesCbcPkcs7: HSTRING read get_TripleDesCbcPkcs7;
    property TripleDesEcb: HSTRING read get_TripleDesEcb;
    property TripleDesEcbPkcs7: HSTRING read get_TripleDesEcbPkcs7;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.ISymmetricKeyAlgorithmProvider
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_SymmetricKeyAlgorithmProvider)]
  Core_ISymmetricKeyAlgorithmProvider = interface(IInspectable)
  ['{3D7E4A33-3BD0-4902-8AC8-470D50D21376}']
    function get_AlgorithmName: HSTRING; safecall;
    function get_BlockLength: Cardinal; safecall;
    function CreateSymmetricKey(keyMaterial: IBuffer): Core_ICryptographicKey; safecall;
    property AlgorithmName: HSTRING read get_AlgorithmName;
    property BlockLength: Cardinal read get_BlockLength;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.Core.ISymmetricKeyAlgorithmProviderStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_Core_SymmetricKeyAlgorithmProvider)]
  Core_ISymmetricKeyAlgorithmProviderStatics = interface(IInspectable)
  ['{8D3B2326-1F37-491F-B60E-F5431B26B483}']
    function OpenAlgorithm(algorithm: HSTRING): Core_ISymmetricKeyAlgorithmProvider; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.DataProtection.IDataProtectionProvider
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_DataProtection_DataProtectionProvider)]
  DataProtection_IDataProtectionProvider = interface(IInspectable)
  ['{09639948-ED22-4270-BD1C-6D72C00F8787}']
    function ProtectAsync(data: IBuffer): IAsyncOperation_1__IBuffer; safecall;
    function UnprotectAsync(data: IBuffer): IAsyncOperation_1__IBuffer; safecall;
    function ProtectStreamAsync(src: IInputStream; dest: IOutputStream): IAsyncAction; safecall;
    function UnprotectStreamAsync(src: IInputStream; dest: IOutputStream): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.DataProtection.IDataProtectionProviderFactory
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_DataProtection_DataProtectionProvider)]
  DataProtection_IDataProtectionProviderFactory = interface(IInspectable)
  ['{ADF33DAC-4932-4CDF-AC41-7214333514CA}']
    function CreateOverloadExplicit(protectionDescriptor: HSTRING): DataProtection_IDataProtectionProvider; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Cryptography.ICryptographicBufferStatics
  [WinRTClassNameAttribute(SWindows_Security_Cryptography_CryptographicBuffer)]
  ICryptographicBufferStatics = interface(IInspectable)
  ['{320B7E22-3CB0-4CDF-8663-1D28910065EB}']
    function Compare(object1: IBuffer; object2: IBuffer): Boolean; safecall;
    function GenerateRandom(length: Cardinal): IBuffer; safecall;
    function GenerateRandomNumber: Cardinal; safecall;
    function CreateFromByteArray(valueSize: Cardinal; value: PByte): IBuffer; safecall;
    procedure CopyToByteArray(buffer: IBuffer; valueSize: Cardinal; value: PByte); safecall;
    function DecodeFromHexString(value: HSTRING): IBuffer; safecall;
    function EncodeToHexString(buffer: IBuffer): HSTRING; safecall;
    function DecodeFromBase64String(value: HSTRING): IBuffer; safecall;
    function EncodeToBase64String(buffer: IBuffer): HSTRING; safecall;
    function ConvertStringToBinary(value: HSTRING; encoding: BinaryStringEncoding): IBuffer; safecall;
    function ConvertBinaryToString(encoding: BinaryStringEncoding; buffer: IBuffer): HSTRING; safecall;
  end;

  // Windows.Security.Cryptography.Certificates.Certificate
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICertificate
  // Implements: Windows.Security.Cryptography.Certificates.ICertificate2
  // Implements: Windows.Security.Cryptography.Certificates.ICertificate3
  // Factory: "Windows.Security.Cryptography.Certificates.ICertificateFactory"
  TCertificates_Certificate = class(TWinRTGenericImportF<Certificates_ICertificateFactory>)
  public
    // -> Certificates_ICertificateFactory
    class function CreateCertificate(certBlob: IBuffer): Certificates_ICertificate; static; inline;
  end;

  // Windows.Security.Cryptography.Certificates.CertificateEnrollmentManager
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics"
  // Statics: "Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics2"
  // Statics: "Windows.Security.Cryptography.Certificates.ICertificateEnrollmentManagerStatics3"
  TCertificates_CertificateEnrollmentManager = class(TWinRTGenericImportS3<Certificates_ICertificateEnrollmentManagerStatics, Certificates_ICertificateEnrollmentManagerStatics2, Certificates_ICertificateEnrollmentManagerStatics3>)
  public
    // -> Certificates_ICertificateEnrollmentManagerStatics
    class function CreateRequestAsync(request: Certificates_ICertificateRequestProperties): IAsyncOperation_1__HSTRING; static; inline;
    class function InstallCertificateAsync(certificate: HSTRING; installOption: Certificates_InstallOptions): IAsyncAction; static; inline;
    class function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING): IAsyncAction; overload; static; inline;

    // -> Certificates_ICertificateEnrollmentManagerStatics2
    class function get_UserCertificateEnrollmentManager: Certificates_IUserCertificateEnrollmentManager; static; inline;
    class function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING; keyStorageProvider: HSTRING): IAsyncAction; overload; static; inline;
    class property UserCertificateEnrollmentManager: Certificates_IUserCertificateEnrollmentManager read get_UserCertificateEnrollmentManager;

    // -> Certificates_ICertificateEnrollmentManagerStatics3
    class function ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; pfxImportParameters: Certificates_IPfxImportParameters): IAsyncAction; overload; static; inline;
  end;

  // Windows.Security.Cryptography.Certificates.CertificateExtension
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateExtension
  // Instantiable: "Certificates_ICertificateExtension"
  TCertificates_CertificateExtension = class(TWinRTGenericImportI<Certificates_ICertificateExtension>) end;

  // Windows.Security.Cryptography.Certificates.CertificateKeyUsages
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateKeyUsages
  // Instantiable: "Certificates_ICertificateKeyUsages"
  TCertificates_CertificateKeyUsages = class(TWinRTGenericImportI<Certificates_ICertificateKeyUsages>) end;

  // Windows.Security.Cryptography.Certificates.CertificateQuery
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateQuery
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateQuery2
  // Instantiable: "Certificates_ICertificateQuery"
  TCertificates_CertificateQuery = class(TWinRTGenericImportI<Certificates_ICertificateQuery>) end;

  // Windows.Security.Cryptography.Certificates.CertificateRequestProperties
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateRequestProperties
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateRequestProperties2
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateRequestProperties3
  // Implements: Windows.Security.Cryptography.Certificates.ICertificateRequestProperties4
  // Instantiable: "Certificates_ICertificateRequestProperties"
  TCertificates_CertificateRequestProperties = class(TWinRTGenericImportI<Certificates_ICertificateRequestProperties>) end;

  // Windows.Security.Cryptography.Certificates.CertificateStores
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Certificates.ICertificateStoresStatics"
  // Statics: "Windows.Security.Cryptography.Certificates.ICertificateStoresStatics2"
  TCertificates_CertificateStores = class(TWinRTGenericImportS2<Certificates_ICertificateStoresStatics, Certificates_ICertificateStoresStatics2>)
  public
    // -> Certificates_ICertificateStoresStatics
    class function FindAllAsync: IAsyncOperation_1__IVectorView_1__Certificates_ICertificate; overload; static; inline;
    class function FindAllAsync(query: Certificates_ICertificateQuery): IAsyncOperation_1__IVectorView_1__Certificates_ICertificate; overload; static; inline;
    class function get_TrustedRootCertificationAuthorities: Certificates_ICertificateStore; static; inline;
    class function get_IntermediateCertificationAuthorities: Certificates_ICertificateStore; static; inline;
    class function GetStoreByName(storeName: HSTRING): Certificates_ICertificateStore; static; inline;
    class property IntermediateCertificationAuthorities: Certificates_ICertificateStore read get_IntermediateCertificationAuthorities;
    class property TrustedRootCertificationAuthorities: Certificates_ICertificateStore read get_TrustedRootCertificationAuthorities;

    // -> Certificates_ICertificateStoresStatics2
    class function GetUserStoreByName(storeName: HSTRING): Certificates_IUserCertificateStore; static; inline;
  end;

  // Windows.Security.Cryptography.Certificates.ChainBuildingParameters
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.IChainBuildingParameters
  // Instantiable: "Certificates_IChainBuildingParameters"
  TCertificates_ChainBuildingParameters = class(TWinRTGenericImportI<Certificates_IChainBuildingParameters>) end;

  // Windows.Security.Cryptography.Certificates.ChainValidationParameters
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.IChainValidationParameters
  // Instantiable: "Certificates_IChainValidationParameters"
  TCertificates_ChainValidationParameters = class(TWinRTGenericImportI<Certificates_IChainValidationParameters>) end;

  // Windows.Security.Cryptography.Certificates.CmsAttachedSignature
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICmsAttachedSignature
  // Statics: "Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureStatics"
  // Factory: "Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureFactory"
  TCertificates_CmsAttachedSignature = class(TWinRTGenericImportFS<Certificates_ICmsAttachedSignatureFactory, Certificates_ICmsAttachedSignatureStatics>)
  public
    // -> Certificates_ICmsAttachedSignatureStatics
    class function GenerateSignatureAsync(data: IBuffer; signers: IIterable_1__Certificates_ICmsSignerInfo; certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__IBuffer; static; inline;

    // -> Certificates_ICmsAttachedSignatureFactory
    class function CreateCmsAttachedSignature(inputBlob: IBuffer): Certificates_ICmsAttachedSignature; static; inline;
  end;

  // Windows.Security.Cryptography.Certificates.CmsDetachedSignature
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICmsDetachedSignature
  // Statics: "Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureStatics"
  // Factory: "Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureFactory"
  TCertificates_CmsDetachedSignature = class(TWinRTGenericImportFS<Certificates_ICmsDetachedSignatureFactory, Certificates_ICmsDetachedSignatureStatics>)
  public
    // -> Certificates_ICmsDetachedSignatureStatics
    class function GenerateSignatureAsync(data: IInputStream; signers: IIterable_1__Certificates_ICmsSignerInfo; certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__IBuffer; static; inline;

    // -> Certificates_ICmsDetachedSignatureFactory
    class function CreateCmsDetachedSignature(inputBlob: IBuffer): Certificates_ICmsDetachedSignature; static; inline;
  end;

  // Windows.Security.Cryptography.Certificates.CmsSignerInfo
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ICmsSignerInfo
  // Instantiable: "Certificates_ICmsSignerInfo"
  TCertificates_CmsSignerInfo = class(TWinRTGenericImportI<Certificates_ICmsSignerInfo>) end;

  // Windows.Security.Cryptography.Certificates.KeyAlgorithmNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Certificates.IKeyAlgorithmNamesStatics"
  // Statics: "Windows.Security.Cryptography.Certificates.IKeyAlgorithmNamesStatics2"
  TCertificates_KeyAlgorithmNames = class(TWinRTGenericImportS2<Certificates_IKeyAlgorithmNamesStatics, Certificates_IKeyAlgorithmNamesStatics2>)
  public
    // -> Certificates_IKeyAlgorithmNamesStatics
    class function get_Rsa: HSTRING; static; inline;
    class function get_Dsa: HSTRING; static; inline;
    class function get_Ecdh256: HSTRING; static; inline;
    class function get_Ecdh384: HSTRING; static; inline;
    class function get_Ecdh521: HSTRING; static; inline;
    class function get_Ecdsa256: HSTRING; static; inline;
    class function get_Ecdsa384: HSTRING; static; inline;
    class function get_Ecdsa521: HSTRING; static; inline;
    class property Dsa: HSTRING read get_Dsa;
    class property Ecdh256: HSTRING read get_Ecdh256;
    class property Ecdh384: HSTRING read get_Ecdh384;
    class property Ecdh521: HSTRING read get_Ecdh521;
    class property Ecdsa256: HSTRING read get_Ecdsa256;
    class property Ecdsa384: HSTRING read get_Ecdsa384;
    class property Ecdsa521: HSTRING read get_Ecdsa521;
    class property Rsa: HSTRING read get_Rsa;

    // -> Certificates_IKeyAlgorithmNamesStatics2
    class function get_Ecdsa: HSTRING; static; inline;
    class function get_Ecdh: HSTRING; static; inline;
    class property Ecdh: HSTRING read get_Ecdh;
    class property Ecdsa: HSTRING read get_Ecdsa;
  end;

  // Windows.Security.Cryptography.Certificates.KeyAttestationHelper
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Certificates.IKeyAttestationHelperStatics"
  // Statics: "Windows.Security.Cryptography.Certificates.IKeyAttestationHelperStatics2"
  TCertificates_KeyAttestationHelper = class(TWinRTGenericImportS2<Certificates_IKeyAttestationHelperStatics, Certificates_IKeyAttestationHelperStatics2>)
  public
    // -> Certificates_IKeyAttestationHelperStatics
    class function DecryptTpmAttestationCredentialAsync(credential: HSTRING): IAsyncOperation_1__HSTRING; overload; static; inline;
    class function GetTpmAttestationCredentialId(credential: HSTRING): HSTRING; static; inline;

    // -> Certificates_IKeyAttestationHelperStatics2
    class function DecryptTpmAttestationCredentialAsync(credential: HSTRING; containerName: HSTRING): IAsyncOperation_1__HSTRING; overload; static; inline;
  end;

  // Windows.Security.Cryptography.Certificates.KeyStorageProviderNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Certificates.IKeyStorageProviderNamesStatics"
  // Statics: "Windows.Security.Cryptography.Certificates.IKeyStorageProviderNamesStatics2"
  TCertificates_KeyStorageProviderNames = class(TWinRTGenericImportS2<Certificates_IKeyStorageProviderNamesStatics, Certificates_IKeyStorageProviderNamesStatics2>)
  public
    // -> Certificates_IKeyStorageProviderNamesStatics
    class function get_SoftwareKeyStorageProvider: HSTRING; static; inline;
    class function get_SmartcardKeyStorageProvider: HSTRING; static; inline;
    class function get_PlatformKeyStorageProvider: HSTRING; static; inline;
    class property PlatformKeyStorageProvider: HSTRING read get_PlatformKeyStorageProvider;
    class property SmartcardKeyStorageProvider: HSTRING read get_SmartcardKeyStorageProvider;
    class property SoftwareKeyStorageProvider: HSTRING read get_SoftwareKeyStorageProvider;

    // -> Certificates_IKeyStorageProviderNamesStatics2
    class function get_PassportKeyStorageProvider: HSTRING; static; inline;
    class property PassportKeyStorageProvider: HSTRING read get_PassportKeyStorageProvider;
  end;

  // Windows.Security.Cryptography.Certificates.PfxImportParameters
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.IPfxImportParameters
  // Instantiable: "Certificates_IPfxImportParameters"
  TCertificates_PfxImportParameters = class(TWinRTGenericImportI<Certificates_IPfxImportParameters>) end;

  // Windows.Security.Cryptography.Certificates.StandardCertificateStoreNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Certificates.IStandardCertificateStoreNamesStatics"
  TCertificates_StandardCertificateStoreNames = class(TWinRTGenericImportS<Certificates_IStandardCertificateStoreNamesStatics>)
  public
    // -> Certificates_IStandardCertificateStoreNamesStatics
    class function get_Personal: HSTRING; static; inline;
    class function get_TrustedRootCertificationAuthorities: HSTRING; static; inline;
    class function get_IntermediateCertificationAuthorities: HSTRING; static; inline;
    class property IntermediateCertificationAuthorities: HSTRING read get_IntermediateCertificationAuthorities;
    class property Personal: HSTRING read get_Personal;
    class property TrustedRootCertificationAuthorities: HSTRING read get_TrustedRootCertificationAuthorities;
  end;

  // Windows.Security.Cryptography.Certificates.SubjectAlternativeNameInfo
  // DualAPI
  // Implements: Windows.Security.Cryptography.Certificates.ISubjectAlternativeNameInfo
  // Implements: Windows.Security.Cryptography.Certificates.ISubjectAlternativeNameInfo2
  // Instantiable: "Certificates_ISubjectAlternativeNameInfo"
  TCertificates_SubjectAlternativeNameInfo = class(TWinRTGenericImportI<Certificates_ISubjectAlternativeNameInfo>) end;

  // Windows.Security.Cryptography.Core.AsymmetricAlgorithmNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.IAsymmetricAlgorithmNamesStatics"
  // Statics: "Windows.Security.Cryptography.Core.IAsymmetricAlgorithmNamesStatics2"
  TCore_AsymmetricAlgorithmNames = class(TWinRTGenericImportS2<Core_IAsymmetricAlgorithmNamesStatics, Core_IAsymmetricAlgorithmNamesStatics2>)
  public
    // -> Core_IAsymmetricAlgorithmNamesStatics
    class function get_RsaPkcs1: HSTRING; static; inline;
    class function get_RsaOaepSha1: HSTRING; static; inline;
    class function get_RsaOaepSha256: HSTRING; static; inline;
    class function get_RsaOaepSha384: HSTRING; static; inline;
    class function get_RsaOaepSha512: HSTRING; static; inline;
    class function get_EcdsaP256Sha256: HSTRING; static; inline;
    class function get_EcdsaP384Sha384: HSTRING; static; inline;
    class function get_EcdsaP521Sha512: HSTRING; static; inline;
    class function get_DsaSha1: HSTRING; static; inline;
    class function get_DsaSha256: HSTRING; static; inline;
    class function get_RsaSignPkcs1Sha1: HSTRING; static; inline;
    class function get_RsaSignPkcs1Sha256: HSTRING; static; inline;
    class function get_RsaSignPkcs1Sha384: HSTRING; static; inline;
    class function get_RsaSignPkcs1Sha512: HSTRING; static; inline;
    class function get_RsaSignPssSha1: HSTRING; static; inline;
    class function get_RsaSignPssSha256: HSTRING; static; inline;
    class function get_RsaSignPssSha384: HSTRING; static; inline;
    class function get_RsaSignPssSha512: HSTRING; static; inline;
    class property DsaSha1: HSTRING read get_DsaSha1;
    class property DsaSha256: HSTRING read get_DsaSha256;
    class property EcdsaP256Sha256: HSTRING read get_EcdsaP256Sha256;
    class property EcdsaP384Sha384: HSTRING read get_EcdsaP384Sha384;
    class property EcdsaP521Sha512: HSTRING read get_EcdsaP521Sha512;
    class property RsaOaepSha1: HSTRING read get_RsaOaepSha1;
    class property RsaOaepSha256: HSTRING read get_RsaOaepSha256;
    class property RsaOaepSha384: HSTRING read get_RsaOaepSha384;
    class property RsaOaepSha512: HSTRING read get_RsaOaepSha512;
    class property RsaPkcs1: HSTRING read get_RsaPkcs1;
    class property RsaSignPkcs1Sha1: HSTRING read get_RsaSignPkcs1Sha1;
    class property RsaSignPkcs1Sha256: HSTRING read get_RsaSignPkcs1Sha256;
    class property RsaSignPkcs1Sha384: HSTRING read get_RsaSignPkcs1Sha384;
    class property RsaSignPkcs1Sha512: HSTRING read get_RsaSignPkcs1Sha512;
    class property RsaSignPssSha1: HSTRING read get_RsaSignPssSha1;
    class property RsaSignPssSha256: HSTRING read get_RsaSignPssSha256;
    class property RsaSignPssSha384: HSTRING read get_RsaSignPssSha384;
    class property RsaSignPssSha512: HSTRING read get_RsaSignPssSha512;

    // -> Core_IAsymmetricAlgorithmNamesStatics2
    class function get_EcdsaSha256: HSTRING; static; inline;
    class function get_EcdsaSha384: HSTRING; static; inline;
    class function get_EcdsaSha512: HSTRING; static; inline;
    class property EcdsaSha256: HSTRING read get_EcdsaSha256;
    class property EcdsaSha384: HSTRING read get_EcdsaSha384;
    class property EcdsaSha512: HSTRING read get_EcdsaSha512;
  end;

  // Windows.Security.Cryptography.Core.AsymmetricKeyAlgorithmProvider
  // DualAPI
  // Implements: Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProvider
  // Implements: Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProvider2
  // Statics: "Windows.Security.Cryptography.Core.IAsymmetricKeyAlgorithmProviderStatics"
  TCore_AsymmetricKeyAlgorithmProvider = class(TWinRTGenericImportS<Core_IAsymmetricKeyAlgorithmProviderStatics>)
  public
    // -> Core_IAsymmetricKeyAlgorithmProviderStatics
    class function OpenAlgorithm(algorithm: HSTRING): Core_IAsymmetricKeyAlgorithmProvider; static; inline;
  end;

  // Windows.Security.Cryptography.Core.CryptographicEngine
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.ICryptographicEngineStatics"
  // Statics: "Windows.Security.Cryptography.Core.ICryptographicEngineStatics2"
  TCore_CryptographicEngine = class(TWinRTGenericImportS2<Core_ICryptographicEngineStatics, Core_ICryptographicEngineStatics2>)
  public
    // -> Core_ICryptographicEngineStatics
    class function Encrypt(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IBuffer; static; inline;
    class function Decrypt(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IBuffer; static; inline;
    class function EncryptAndAuthenticate(key: Core_ICryptographicKey; data: IBuffer; nonce: IBuffer; authenticatedData: IBuffer): Core_IEncryptedAndAuthenticatedData; static; inline;
    class function DecryptAndAuthenticate(key: Core_ICryptographicKey; data: IBuffer; nonce: IBuffer; authenticationTag: IBuffer; authenticatedData: IBuffer): IBuffer; static; inline;
    class function Sign(key: Core_ICryptographicKey; data: IBuffer): IBuffer; static; inline;
    class function VerifySignature(key: Core_ICryptographicKey; data: IBuffer; signature: IBuffer): Boolean; static; inline;
    class function DeriveKeyMaterial(key: Core_ICryptographicKey; parameters: Core_IKeyDerivationParameters; desiredKeySize: Cardinal): IBuffer; static; inline;

    // -> Core_ICryptographicEngineStatics2
    class function SignHashedData(key: Core_ICryptographicKey; data: IBuffer): IBuffer; static; inline;
    class function VerifySignatureWithHashInput(key: Core_ICryptographicKey; data: IBuffer; signature: IBuffer): Boolean; static; inline;
    class function DecryptAsync(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IAsyncOperation_1__IBuffer; static; inline;
    class function SignAsync(key: Core_ICryptographicKey; data: IBuffer): IAsyncOperation_1__IBuffer; static; inline;
    class function SignHashedDataAsync(key: Core_ICryptographicKey; data: IBuffer): IAsyncOperation_1__IBuffer; static; inline;
  end;

  // Windows.Security.Cryptography.Core.EccCurveNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.IEccCurveNamesStatics"
  TCore_EccCurveNames = class(TWinRTGenericImportS<Core_IEccCurveNamesStatics>)
  public
    // -> Core_IEccCurveNamesStatics
    class function get_BrainpoolP160r1: HSTRING; static; inline;
    class function get_BrainpoolP160t1: HSTRING; static; inline;
    class function get_BrainpoolP192r1: HSTRING; static; inline;
    class function get_BrainpoolP192t1: HSTRING; static; inline;
    class function get_BrainpoolP224r1: HSTRING; static; inline;
    class function get_BrainpoolP224t1: HSTRING; static; inline;
    class function get_BrainpoolP256r1: HSTRING; static; inline;
    class function get_BrainpoolP256t1: HSTRING; static; inline;
    class function get_BrainpoolP320r1: HSTRING; static; inline;
    class function get_BrainpoolP320t1: HSTRING; static; inline;
    class function get_BrainpoolP384r1: HSTRING; static; inline;
    class function get_BrainpoolP384t1: HSTRING; static; inline;
    class function get_BrainpoolP512r1: HSTRING; static; inline;
    class function get_BrainpoolP512t1: HSTRING; static; inline;
    class function get_Curve25519: HSTRING; static; inline;
    class function get_Ec192wapi: HSTRING; static; inline;
    class function get_NistP192: HSTRING; static; inline;
    class function get_NistP224: HSTRING; static; inline;
    class function get_NistP256: HSTRING; static; inline;
    class function get_NistP384: HSTRING; static; inline;
    class function get_NistP521: HSTRING; static; inline;
    class function get_NumsP256t1: HSTRING; static; inline;
    class function get_NumsP384t1: HSTRING; static; inline;
    class function get_NumsP512t1: HSTRING; static; inline;
    class function get_SecP160k1: HSTRING; static; inline;
    class function get_SecP160r1: HSTRING; static; inline;
    class function get_SecP160r2: HSTRING; static; inline;
    class function get_SecP192k1: HSTRING; static; inline;
    class function get_SecP192r1: HSTRING; static; inline;
    class function get_SecP224k1: HSTRING; static; inline;
    class function get_SecP224r1: HSTRING; static; inline;
    class function get_SecP256k1: HSTRING; static; inline;
    class function get_SecP256r1: HSTRING; static; inline;
    class function get_SecP384r1: HSTRING; static; inline;
    class function get_SecP521r1: HSTRING; static; inline;
    class function get_Wtls7: HSTRING; static; inline;
    class function get_Wtls9: HSTRING; static; inline;
    class function get_Wtls12: HSTRING; static; inline;
    class function get_X962P192v1: HSTRING; static; inline;
    class function get_X962P192v2: HSTRING; static; inline;
    class function get_X962P192v3: HSTRING; static; inline;
    class function get_X962P239v1: HSTRING; static; inline;
    class function get_X962P239v2: HSTRING; static; inline;
    class function get_X962P239v3: HSTRING; static; inline;
    class function get_X962P256v1: HSTRING; static; inline;
    class function get_AllEccCurveNames: IVectorView_1__HSTRING; static; inline;
    class property AllEccCurveNames: IVectorView_1__HSTRING read get_AllEccCurveNames;
    class property BrainpoolP160r1: HSTRING read get_BrainpoolP160r1;
    class property BrainpoolP160t1: HSTRING read get_BrainpoolP160t1;
    class property BrainpoolP192r1: HSTRING read get_BrainpoolP192r1;
    class property BrainpoolP192t1: HSTRING read get_BrainpoolP192t1;
    class property BrainpoolP224r1: HSTRING read get_BrainpoolP224r1;
    class property BrainpoolP224t1: HSTRING read get_BrainpoolP224t1;
    class property BrainpoolP256r1: HSTRING read get_BrainpoolP256r1;
    class property BrainpoolP256t1: HSTRING read get_BrainpoolP256t1;
    class property BrainpoolP320r1: HSTRING read get_BrainpoolP320r1;
    class property BrainpoolP320t1: HSTRING read get_BrainpoolP320t1;
    class property BrainpoolP384r1: HSTRING read get_BrainpoolP384r1;
    class property BrainpoolP384t1: HSTRING read get_BrainpoolP384t1;
    class property BrainpoolP512r1: HSTRING read get_BrainpoolP512r1;
    class property BrainpoolP512t1: HSTRING read get_BrainpoolP512t1;
    class property Curve25519: HSTRING read get_Curve25519;
    class property Ec192wapi: HSTRING read get_Ec192wapi;
    class property NistP192: HSTRING read get_NistP192;
    class property NistP224: HSTRING read get_NistP224;
    class property NistP256: HSTRING read get_NistP256;
    class property NistP384: HSTRING read get_NistP384;
    class property NistP521: HSTRING read get_NistP521;
    class property NumsP256t1: HSTRING read get_NumsP256t1;
    class property NumsP384t1: HSTRING read get_NumsP384t1;
    class property NumsP512t1: HSTRING read get_NumsP512t1;
    class property SecP160k1: HSTRING read get_SecP160k1;
    class property SecP160r1: HSTRING read get_SecP160r1;
    class property SecP160r2: HSTRING read get_SecP160r2;
    class property SecP192k1: HSTRING read get_SecP192k1;
    class property SecP192r1: HSTRING read get_SecP192r1;
    class property SecP224k1: HSTRING read get_SecP224k1;
    class property SecP224r1: HSTRING read get_SecP224r1;
    class property SecP256k1: HSTRING read get_SecP256k1;
    class property SecP256r1: HSTRING read get_SecP256r1;
    class property SecP384r1: HSTRING read get_SecP384r1;
    class property SecP521r1: HSTRING read get_SecP521r1;
    class property Wtls12: HSTRING read get_Wtls12;
    class property Wtls7: HSTRING read get_Wtls7;
    class property Wtls9: HSTRING read get_Wtls9;
    class property X962P192v1: HSTRING read get_X962P192v1;
    class property X962P192v2: HSTRING read get_X962P192v2;
    class property X962P192v3: HSTRING read get_X962P192v3;
    class property X962P239v1: HSTRING read get_X962P239v1;
    class property X962P239v2: HSTRING read get_X962P239v2;
    class property X962P239v3: HSTRING read get_X962P239v3;
    class property X962P256v1: HSTRING read get_X962P256v1;
  end;

  // Windows.Security.Cryptography.Core.HashAlgorithmNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.IHashAlgorithmNamesStatics"
  TCore_HashAlgorithmNames = class(TWinRTGenericImportS<Core_IHashAlgorithmNamesStatics>)
  public
    // -> Core_IHashAlgorithmNamesStatics
    class function get_Md5: HSTRING; static; inline;
    class function get_Sha1: HSTRING; static; inline;
    class function get_Sha256: HSTRING; static; inline;
    class function get_Sha384: HSTRING; static; inline;
    class function get_Sha512: HSTRING; static; inline;
    class property Md5: HSTRING read get_Md5;
    class property Sha1: HSTRING read get_Sha1;
    class property Sha256: HSTRING read get_Sha256;
    class property Sha384: HSTRING read get_Sha384;
    class property Sha512: HSTRING read get_Sha512;
  end;

  // Windows.Security.Cryptography.Core.HashAlgorithmProvider
  // DualAPI
  // Implements: Windows.Security.Cryptography.Core.IHashAlgorithmProvider
  // Statics: "Windows.Security.Cryptography.Core.IHashAlgorithmProviderStatics"
  TCore_HashAlgorithmProvider = class(TWinRTGenericImportS<Core_IHashAlgorithmProviderStatics>)
  public
    // -> Core_IHashAlgorithmProviderStatics
    class function OpenAlgorithm(algorithm: HSTRING): Core_IHashAlgorithmProvider; static; inline;
  end;

  // Windows.Security.Cryptography.Core.KeyDerivationAlgorithmNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmNamesStatics"
  // Statics: "Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmNamesStatics2"
  TCore_KeyDerivationAlgorithmNames = class(TWinRTGenericImportS2<Core_IKeyDerivationAlgorithmNamesStatics, Core_IKeyDerivationAlgorithmNamesStatics2>)
  public
    // -> Core_IKeyDerivationAlgorithmNamesStatics
    class function get_Pbkdf2Md5: HSTRING; static; inline;
    class function get_Pbkdf2Sha1: HSTRING; static; inline;
    class function get_Pbkdf2Sha256: HSTRING; static; inline;
    class function get_Pbkdf2Sha384: HSTRING; static; inline;
    class function get_Pbkdf2Sha512: HSTRING; static; inline;
    class function get_Sp800108CtrHmacMd5: HSTRING; static; inline;
    class function get_Sp800108CtrHmacSha1: HSTRING; static; inline;
    class function get_Sp800108CtrHmacSha256: HSTRING; static; inline;
    class function get_Sp800108CtrHmacSha384: HSTRING; static; inline;
    class function get_Sp800108CtrHmacSha512: HSTRING; static; inline;
    class function get_Sp80056aConcatMd5: HSTRING; static; inline;
    class function get_Sp80056aConcatSha1: HSTRING; static; inline;
    class function get_Sp80056aConcatSha256: HSTRING; static; inline;
    class function get_Sp80056aConcatSha384: HSTRING; static; inline;
    class function get_Sp80056aConcatSha512: HSTRING; static; inline;
    class property Pbkdf2Md5: HSTRING read get_Pbkdf2Md5;
    class property Pbkdf2Sha1: HSTRING read get_Pbkdf2Sha1;
    class property Pbkdf2Sha256: HSTRING read get_Pbkdf2Sha256;
    class property Pbkdf2Sha384: HSTRING read get_Pbkdf2Sha384;
    class property Pbkdf2Sha512: HSTRING read get_Pbkdf2Sha512;
    class property Sp800108CtrHmacMd5: HSTRING read get_Sp800108CtrHmacMd5;
    class property Sp800108CtrHmacSha1: HSTRING read get_Sp800108CtrHmacSha1;
    class property Sp800108CtrHmacSha256: HSTRING read get_Sp800108CtrHmacSha256;
    class property Sp800108CtrHmacSha384: HSTRING read get_Sp800108CtrHmacSha384;
    class property Sp800108CtrHmacSha512: HSTRING read get_Sp800108CtrHmacSha512;
    class property Sp80056aConcatMd5: HSTRING read get_Sp80056aConcatMd5;
    class property Sp80056aConcatSha1: HSTRING read get_Sp80056aConcatSha1;
    class property Sp80056aConcatSha256: HSTRING read get_Sp80056aConcatSha256;
    class property Sp80056aConcatSha384: HSTRING read get_Sp80056aConcatSha384;
    class property Sp80056aConcatSha512: HSTRING read get_Sp80056aConcatSha512;

    // -> Core_IKeyDerivationAlgorithmNamesStatics2
    class function get_CapiKdfMd5: HSTRING; static; inline;
    class function get_CapiKdfSha1: HSTRING; static; inline;
    class function get_CapiKdfSha256: HSTRING; static; inline;
    class function get_CapiKdfSha384: HSTRING; static; inline;
    class function get_CapiKdfSha512: HSTRING; static; inline;
    class property CapiKdfMd5: HSTRING read get_CapiKdfMd5;
    class property CapiKdfSha1: HSTRING read get_CapiKdfSha1;
    class property CapiKdfSha256: HSTRING read get_CapiKdfSha256;
    class property CapiKdfSha384: HSTRING read get_CapiKdfSha384;
    class property CapiKdfSha512: HSTRING read get_CapiKdfSha512;
  end;

  // Windows.Security.Cryptography.Core.KeyDerivationAlgorithmProvider
  // DualAPI
  // Implements: Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmProvider
  // Statics: "Windows.Security.Cryptography.Core.IKeyDerivationAlgorithmProviderStatics"
  TCore_KeyDerivationAlgorithmProvider = class(TWinRTGenericImportS<Core_IKeyDerivationAlgorithmProviderStatics>)
  public
    // -> Core_IKeyDerivationAlgorithmProviderStatics
    class function OpenAlgorithm(algorithm: HSTRING): Core_IKeyDerivationAlgorithmProvider; static; inline;
  end;

  // Windows.Security.Cryptography.Core.KeyDerivationParameters
  // DualAPI
  // Implements: Windows.Security.Cryptography.Core.IKeyDerivationParameters
  // Implements: Windows.Security.Cryptography.Core.IKeyDerivationParameters2
  // Statics: "Windows.Security.Cryptography.Core.IKeyDerivationParametersStatics"
  // Statics: "Windows.Security.Cryptography.Core.IKeyDerivationParametersStatics2"
  TCore_KeyDerivationParameters = class(TWinRTGenericImportS2<Core_IKeyDerivationParametersStatics, Core_IKeyDerivationParametersStatics2>)
  public
    // -> Core_IKeyDerivationParametersStatics
    class function BuildForPbkdf2(pbkdf2Salt: IBuffer; iterationCount: Cardinal): Core_IKeyDerivationParameters; static; inline;
    class function BuildForSP800108(&label: IBuffer; context: IBuffer): Core_IKeyDerivationParameters; static; inline;
    class function BuildForSP80056a(algorithmId: IBuffer; partyUInfo: IBuffer; partyVInfo: IBuffer; suppPubInfo: IBuffer; suppPrivInfo: IBuffer): Core_IKeyDerivationParameters; static; inline;

    // -> Core_IKeyDerivationParametersStatics2
    class function BuildForCapi1Kdf(capi1KdfTargetAlgorithm: Core_Capi1KdfTargetAlgorithm): Core_IKeyDerivationParameters; static; inline;
  end;

  // Windows.Security.Cryptography.Core.MacAlgorithmNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.IMacAlgorithmNamesStatics"
  TCore_MacAlgorithmNames = class(TWinRTGenericImportS<Core_IMacAlgorithmNamesStatics>)
  public
    // -> Core_IMacAlgorithmNamesStatics
    class function get_HmacMd5: HSTRING; static; inline;
    class function get_HmacSha1: HSTRING; static; inline;
    class function get_HmacSha256: HSTRING; static; inline;
    class function get_HmacSha384: HSTRING; static; inline;
    class function get_HmacSha512: HSTRING; static; inline;
    class function get_AesCmac: HSTRING; static; inline;
    class property AesCmac: HSTRING read get_AesCmac;
    class property HmacMd5: HSTRING read get_HmacMd5;
    class property HmacSha1: HSTRING read get_HmacSha1;
    class property HmacSha256: HSTRING read get_HmacSha256;
    class property HmacSha384: HSTRING read get_HmacSha384;
    class property HmacSha512: HSTRING read get_HmacSha512;
  end;

  // Windows.Security.Cryptography.Core.MacAlgorithmProvider
  // DualAPI
  // Implements: Windows.Security.Cryptography.Core.IMacAlgorithmProvider
  // Implements: Windows.Security.Cryptography.Core.IMacAlgorithmProvider2
  // Statics: "Windows.Security.Cryptography.Core.IMacAlgorithmProviderStatics"
  TCore_MacAlgorithmProvider = class(TWinRTGenericImportS<Core_IMacAlgorithmProviderStatics>)
  public
    // -> Core_IMacAlgorithmProviderStatics
    class function OpenAlgorithm(algorithm: HSTRING): Core_IMacAlgorithmProvider; static; inline;
  end;

  // Windows.Security.Cryptography.Core.PersistedKeyProvider
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.IPersistedKeyProviderStatics"
  TCore_PersistedKeyProvider = class(TWinRTGenericImportS<Core_IPersistedKeyProviderStatics>)
  public
    // -> Core_IPersistedKeyProviderStatics
    class function OpenKeyPairFromCertificateAsync(certificate: Certificates_ICertificate; hashAlgorithmName: HSTRING; padding: Core_CryptographicPadding): IAsyncOperation_1__Core_ICryptographicKey; static; inline;
    class function OpenPublicKeyFromCertificate(certificate: Certificates_ICertificate; hashAlgorithmName: HSTRING; padding: Core_CryptographicPadding): Core_ICryptographicKey; static; inline;
  end;

  // Windows.Security.Cryptography.Core.SymmetricAlgorithmNames
  // DualAPI
  // Statics: "Windows.Security.Cryptography.Core.ISymmetricAlgorithmNamesStatics"
  TCore_SymmetricAlgorithmNames = class(TWinRTGenericImportS<Core_ISymmetricAlgorithmNamesStatics>)
  public
    // -> Core_ISymmetricAlgorithmNamesStatics
    class function get_DesCbc: HSTRING; static; inline;
    class function get_DesEcb: HSTRING; static; inline;
    class function get_TripleDesCbc: HSTRING; static; inline;
    class function get_TripleDesEcb: HSTRING; static; inline;
    class function get_Rc2Cbc: HSTRING; static; inline;
    class function get_Rc2Ecb: HSTRING; static; inline;
    class function get_AesCbc: HSTRING; static; inline;
    class function get_AesEcb: HSTRING; static; inline;
    class function get_AesGcm: HSTRING; static; inline;
    class function get_AesCcm: HSTRING; static; inline;
    class function get_AesCbcPkcs7: HSTRING; static; inline;
    class function get_AesEcbPkcs7: HSTRING; static; inline;
    class function get_DesCbcPkcs7: HSTRING; static; inline;
    class function get_DesEcbPkcs7: HSTRING; static; inline;
    class function get_TripleDesCbcPkcs7: HSTRING; static; inline;
    class function get_TripleDesEcbPkcs7: HSTRING; static; inline;
    class function get_Rc2CbcPkcs7: HSTRING; static; inline;
    class function get_Rc2EcbPkcs7: HSTRING; static; inline;
    class function get_Rc4: HSTRING; static; inline;
    class property AesCbc: HSTRING read get_AesCbc;
    class property AesCbcPkcs7: HSTRING read get_AesCbcPkcs7;
    class property AesCcm: HSTRING read get_AesCcm;
    class property AesEcb: HSTRING read get_AesEcb;
    class property AesEcbPkcs7: HSTRING read get_AesEcbPkcs7;
    class property AesGcm: HSTRING read get_AesGcm;
    class property DesCbc: HSTRING read get_DesCbc;
    class property DesCbcPkcs7: HSTRING read get_DesCbcPkcs7;
    class property DesEcb: HSTRING read get_DesEcb;
    class property DesEcbPkcs7: HSTRING read get_DesEcbPkcs7;
    class property Rc2Cbc: HSTRING read get_Rc2Cbc;
    class property Rc2CbcPkcs7: HSTRING read get_Rc2CbcPkcs7;
    class property Rc2Ecb: HSTRING read get_Rc2Ecb;
    class property Rc2EcbPkcs7: HSTRING read get_Rc2EcbPkcs7;
    class property Rc4: HSTRING read get_Rc4;
    class property TripleDesCbc: HSTRING read get_TripleDesCbc;
    class property TripleDesCbcPkcs7: HSTRING read get_TripleDesCbcPkcs7;
    class property TripleDesEcb: HSTRING read get_TripleDesEcb;
    class property TripleDesEcbPkcs7: HSTRING read get_TripleDesEcbPkcs7;
  end;

  // Windows.Security.Cryptography.Core.SymmetricKeyAlgorithmProvider
  // DualAPI
  // Implements: Windows.Security.Cryptography.Core.ISymmetricKeyAlgorithmProvider
  // Statics: "Windows.Security.Cryptography.Core.ISymmetricKeyAlgorithmProviderStatics"
  TCore_SymmetricKeyAlgorithmProvider = class(TWinRTGenericImportS<Core_ISymmetricKeyAlgorithmProviderStatics>)
  public
    // -> Core_ISymmetricKeyAlgorithmProviderStatics
    class function OpenAlgorithm(algorithm: HSTRING): Core_ISymmetricKeyAlgorithmProvider; static; inline;
  end;

  // Windows.Security.Cryptography.CryptographicBuffer
  // DualAPI
  // Statics: "Windows.Security.Cryptography.ICryptographicBufferStatics"
  TCryptographicBuffer = class(TWinRTGenericImportS<ICryptographicBufferStatics>)
  public
    // -> ICryptographicBufferStatics
    class function Compare(object1: IBuffer; object2: IBuffer): Boolean; static; inline;
    class function GenerateRandom(length: Cardinal): IBuffer; static; inline;
    class function GenerateRandomNumber: Cardinal; static; inline;
    class function CreateFromByteArray(valueSize: Cardinal; value: PByte): IBuffer; static; inline;
    class procedure CopyToByteArray(buffer: IBuffer; valueSize: Cardinal; value: PByte); static; inline;
    class function DecodeFromHexString(value: HSTRING): IBuffer; static; inline;
    class function EncodeToHexString(buffer: IBuffer): HSTRING; static; inline;
    class function DecodeFromBase64String(value: HSTRING): IBuffer; static; inline;
    class function EncodeToBase64String(buffer: IBuffer): HSTRING; static; inline;
    class function ConvertStringToBinary(value: HSTRING; encoding: BinaryStringEncoding): IBuffer; static; inline;
    class function ConvertBinaryToString(encoding: BinaryStringEncoding; buffer: IBuffer): HSTRING; static; inline;
  end;

  // Windows.Security.Cryptography.DataProtection.DataProtectionProvider
  // DualAPI
  // Implements: Windows.Security.Cryptography.DataProtection.IDataProtectionProvider
  // Factory: "Windows.Security.Cryptography.DataProtection.IDataProtectionProviderFactory"
  // Instantiable: "DataProtection_IDataProtectionProvider"
  TDataProtection_DataProtectionProvider = class(TWinRTGenericImportFI<DataProtection_IDataProtectionProviderFactory, DataProtection_IDataProtectionProvider>)
  public
    // -> DataProtection_IDataProtectionProviderFactory
    class function CreateOverloadExplicit(protectionDescriptor: HSTRING): DataProtection_IDataProtectionProvider; static; inline;
  end;

implementation

{ TCertificates_Certificate }
// Factories for : "Certificates_Certificate"
// Factory: "Windows.Security.Cryptography.Certificates.ICertificateFactory"
// -> Certificates_ICertificateFactory

class function TCertificates_Certificate.CreateCertificate(certBlob: IBuffer): Certificates_ICertificate;
begin
  Result := Factory.CreateCertificate(certBlob);
end;


{ TCertificates_CertificateEnrollmentManager }

class function TCertificates_CertificateEnrollmentManager.CreateRequestAsync(request: Certificates_ICertificateRequestProperties): IAsyncOperation_1__HSTRING;
begin
  Result := Statics.CreateRequestAsync(request);
end;

class function TCertificates_CertificateEnrollmentManager.InstallCertificateAsync(certificate: HSTRING; installOption: Certificates_InstallOptions): IAsyncAction;
begin
  Result := Statics.InstallCertificateAsync(certificate, installOption);
end;

class function TCertificates_CertificateEnrollmentManager.ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING): IAsyncAction;
begin
  Result := Statics.ImportPfxDataAsync(pfxData, password, exportable, keyProtectionLevel, installOption, friendlyName);
end;


class function TCertificates_CertificateEnrollmentManager.get_UserCertificateEnrollmentManager: Certificates_IUserCertificateEnrollmentManager;
begin
  Result := Statics2.get_UserCertificateEnrollmentManager;
end;

class function TCertificates_CertificateEnrollmentManager.ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; exportable: Certificates_ExportOption; keyProtectionLevel: Certificates_KeyProtectionLevel; installOption: Certificates_InstallOptions; friendlyName: HSTRING; keyStorageProvider: HSTRING): IAsyncAction;
begin
  Result := Statics2.ImportPfxDataAsync(pfxData, password, exportable, keyProtectionLevel, installOption, friendlyName, keyStorageProvider);
end;


class function TCertificates_CertificateEnrollmentManager.ImportPfxDataAsync(pfxData: HSTRING; password: HSTRING; pfxImportParameters: Certificates_IPfxImportParameters): IAsyncAction;
begin
  Result := Statics3.ImportPfxDataAsync(pfxData, password, pfxImportParameters);
end;


{ TCertificates_CertificateExtension }

{ TCertificates_CertificateKeyUsages }

{ TCertificates_CertificateQuery }

{ TCertificates_CertificateRequestProperties }

{ TCertificates_CertificateStores }

class function TCertificates_CertificateStores.FindAllAsync: IAsyncOperation_1__IVectorView_1__Certificates_ICertificate;
begin
  Result := Statics.FindAllAsync;
end;

class function TCertificates_CertificateStores.FindAllAsync(query: Certificates_ICertificateQuery): IAsyncOperation_1__IVectorView_1__Certificates_ICertificate;
begin
  Result := Statics.FindAllAsync(query);
end;

class function TCertificates_CertificateStores.get_TrustedRootCertificationAuthorities: Certificates_ICertificateStore;
begin
  Result := Statics.get_TrustedRootCertificationAuthorities;
end;

class function TCertificates_CertificateStores.get_IntermediateCertificationAuthorities: Certificates_ICertificateStore;
begin
  Result := Statics.get_IntermediateCertificationAuthorities;
end;

class function TCertificates_CertificateStores.GetStoreByName(storeName: HSTRING): Certificates_ICertificateStore;
begin
  Result := Statics.GetStoreByName(storeName);
end;


class function TCertificates_CertificateStores.GetUserStoreByName(storeName: HSTRING): Certificates_IUserCertificateStore;
begin
  Result := Statics2.GetUserStoreByName(storeName);
end;


{ TCertificates_ChainBuildingParameters }

{ TCertificates_ChainValidationParameters }

{ TCertificates_CmsAttachedSignature }

class function TCertificates_CmsAttachedSignature.GenerateSignatureAsync(data: IBuffer; signers: IIterable_1__Certificates_ICmsSignerInfo; certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__IBuffer;
begin
  Result := Statics.GenerateSignatureAsync(data, signers, certificates);
end;

// Factories for : "Certificates_CmsAttachedSignature"
// Factory: "Windows.Security.Cryptography.Certificates.ICmsAttachedSignatureFactory"
// -> Certificates_ICmsAttachedSignatureFactory

class function TCertificates_CmsAttachedSignature.CreateCmsAttachedSignature(inputBlob: IBuffer): Certificates_ICmsAttachedSignature;
begin
  Result := Factory.CreateCmsAttachedSignature(inputBlob);
end;


{ TCertificates_CmsDetachedSignature }

class function TCertificates_CmsDetachedSignature.GenerateSignatureAsync(data: IInputStream; signers: IIterable_1__Certificates_ICmsSignerInfo; certificates: IIterable_1__Certificates_ICertificate): IAsyncOperation_1__IBuffer;
begin
  Result := Statics.GenerateSignatureAsync(data, signers, certificates);
end;

// Factories for : "Certificates_CmsDetachedSignature"
// Factory: "Windows.Security.Cryptography.Certificates.ICmsDetachedSignatureFactory"
// -> Certificates_ICmsDetachedSignatureFactory

class function TCertificates_CmsDetachedSignature.CreateCmsDetachedSignature(inputBlob: IBuffer): Certificates_ICmsDetachedSignature;
begin
  Result := Factory.CreateCmsDetachedSignature(inputBlob);
end;


{ TCertificates_CmsSignerInfo }

{ TCertificates_KeyAlgorithmNames }

class function TCertificates_KeyAlgorithmNames.get_Rsa: HSTRING;
begin
  Result := Statics.get_Rsa;
end;

class function TCertificates_KeyAlgorithmNames.get_Dsa: HSTRING;
begin
  Result := Statics.get_Dsa;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdh256: HSTRING;
begin
  Result := Statics.get_Ecdh256;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdh384: HSTRING;
begin
  Result := Statics.get_Ecdh384;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdh521: HSTRING;
begin
  Result := Statics.get_Ecdh521;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdsa256: HSTRING;
begin
  Result := Statics.get_Ecdsa256;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdsa384: HSTRING;
begin
  Result := Statics.get_Ecdsa384;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdsa521: HSTRING;
begin
  Result := Statics.get_Ecdsa521;
end;


class function TCertificates_KeyAlgorithmNames.get_Ecdsa: HSTRING;
begin
  Result := Statics2.get_Ecdsa;
end;

class function TCertificates_KeyAlgorithmNames.get_Ecdh: HSTRING;
begin
  Result := Statics2.get_Ecdh;
end;


{ TCertificates_KeyAttestationHelper }

class function TCertificates_KeyAttestationHelper.DecryptTpmAttestationCredentialAsync(credential: HSTRING): IAsyncOperation_1__HSTRING;
begin
  Result := Statics.DecryptTpmAttestationCredentialAsync(credential);
end;

class function TCertificates_KeyAttestationHelper.GetTpmAttestationCredentialId(credential: HSTRING): HSTRING;
begin
  Result := Statics.GetTpmAttestationCredentialId(credential);
end;


class function TCertificates_KeyAttestationHelper.DecryptTpmAttestationCredentialAsync(credential: HSTRING; containerName: HSTRING): IAsyncOperation_1__HSTRING;
begin
  Result := Statics2.DecryptTpmAttestationCredentialAsync(credential, containerName);
end;


{ TCertificates_KeyStorageProviderNames }

class function TCertificates_KeyStorageProviderNames.get_SoftwareKeyStorageProvider: HSTRING;
begin
  Result := Statics.get_SoftwareKeyStorageProvider;
end;

class function TCertificates_KeyStorageProviderNames.get_SmartcardKeyStorageProvider: HSTRING;
begin
  Result := Statics.get_SmartcardKeyStorageProvider;
end;

class function TCertificates_KeyStorageProviderNames.get_PlatformKeyStorageProvider: HSTRING;
begin
  Result := Statics.get_PlatformKeyStorageProvider;
end;


class function TCertificates_KeyStorageProviderNames.get_PassportKeyStorageProvider: HSTRING;
begin
  Result := Statics2.get_PassportKeyStorageProvider;
end;


{ TCertificates_PfxImportParameters }

{ TCertificates_StandardCertificateStoreNames }

class function TCertificates_StandardCertificateStoreNames.get_Personal: HSTRING;
begin
  Result := Statics.get_Personal;
end;

class function TCertificates_StandardCertificateStoreNames.get_TrustedRootCertificationAuthorities: HSTRING;
begin
  Result := Statics.get_TrustedRootCertificationAuthorities;
end;

class function TCertificates_StandardCertificateStoreNames.get_IntermediateCertificationAuthorities: HSTRING;
begin
  Result := Statics.get_IntermediateCertificationAuthorities;
end;


{ TCertificates_SubjectAlternativeNameInfo }

{ TCore_AsymmetricAlgorithmNames }

class function TCore_AsymmetricAlgorithmNames.get_RsaPkcs1: HSTRING;
begin
  Result := Statics.get_RsaPkcs1;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaOaepSha1: HSTRING;
begin
  Result := Statics.get_RsaOaepSha1;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaOaepSha256: HSTRING;
begin
  Result := Statics.get_RsaOaepSha256;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaOaepSha384: HSTRING;
begin
  Result := Statics.get_RsaOaepSha384;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaOaepSha512: HSTRING;
begin
  Result := Statics.get_RsaOaepSha512;
end;

class function TCore_AsymmetricAlgorithmNames.get_EcdsaP256Sha256: HSTRING;
begin
  Result := Statics.get_EcdsaP256Sha256;
end;

class function TCore_AsymmetricAlgorithmNames.get_EcdsaP384Sha384: HSTRING;
begin
  Result := Statics.get_EcdsaP384Sha384;
end;

class function TCore_AsymmetricAlgorithmNames.get_EcdsaP521Sha512: HSTRING;
begin
  Result := Statics.get_EcdsaP521Sha512;
end;

class function TCore_AsymmetricAlgorithmNames.get_DsaSha1: HSTRING;
begin
  Result := Statics.get_DsaSha1;
end;

class function TCore_AsymmetricAlgorithmNames.get_DsaSha256: HSTRING;
begin
  Result := Statics.get_DsaSha256;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPkcs1Sha1: HSTRING;
begin
  Result := Statics.get_RsaSignPkcs1Sha1;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPkcs1Sha256: HSTRING;
begin
  Result := Statics.get_RsaSignPkcs1Sha256;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPkcs1Sha384: HSTRING;
begin
  Result := Statics.get_RsaSignPkcs1Sha384;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPkcs1Sha512: HSTRING;
begin
  Result := Statics.get_RsaSignPkcs1Sha512;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPssSha1: HSTRING;
begin
  Result := Statics.get_RsaSignPssSha1;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPssSha256: HSTRING;
begin
  Result := Statics.get_RsaSignPssSha256;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPssSha384: HSTRING;
begin
  Result := Statics.get_RsaSignPssSha384;
end;

class function TCore_AsymmetricAlgorithmNames.get_RsaSignPssSha512: HSTRING;
begin
  Result := Statics.get_RsaSignPssSha512;
end;


class function TCore_AsymmetricAlgorithmNames.get_EcdsaSha256: HSTRING;
begin
  Result := Statics2.get_EcdsaSha256;
end;

class function TCore_AsymmetricAlgorithmNames.get_EcdsaSha384: HSTRING;
begin
  Result := Statics2.get_EcdsaSha384;
end;

class function TCore_AsymmetricAlgorithmNames.get_EcdsaSha512: HSTRING;
begin
  Result := Statics2.get_EcdsaSha512;
end;


{ TCore_AsymmetricKeyAlgorithmProvider }

class function TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(algorithm: HSTRING): Core_IAsymmetricKeyAlgorithmProvider;
begin
  Result := Statics.OpenAlgorithm(algorithm);
end;


{ TCore_CryptographicEngine }

class function TCore_CryptographicEngine.Encrypt(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IBuffer;
begin
  Result := Statics.Encrypt(key, data, iv);
end;

class function TCore_CryptographicEngine.Decrypt(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IBuffer;
begin
  Result := Statics.Decrypt(key, data, iv);
end;

class function TCore_CryptographicEngine.EncryptAndAuthenticate(key: Core_ICryptographicKey; data: IBuffer; nonce: IBuffer; authenticatedData: IBuffer): Core_IEncryptedAndAuthenticatedData;
begin
  Result := Statics.EncryptAndAuthenticate(key, data, nonce, authenticatedData);
end;

class function TCore_CryptographicEngine.DecryptAndAuthenticate(key: Core_ICryptographicKey; data: IBuffer; nonce: IBuffer; authenticationTag: IBuffer; authenticatedData: IBuffer): IBuffer;
begin
  Result := Statics.DecryptAndAuthenticate(key, data, nonce, authenticationTag, authenticatedData);
end;

class function TCore_CryptographicEngine.Sign(key: Core_ICryptographicKey; data: IBuffer): IBuffer;
begin
  Result := Statics.Sign(key, data);
end;

class function TCore_CryptographicEngine.VerifySignature(key: Core_ICryptographicKey; data: IBuffer; signature: IBuffer): Boolean;
begin
  Result := Statics.VerifySignature(key, data, signature);
end;

class function TCore_CryptographicEngine.DeriveKeyMaterial(key: Core_ICryptographicKey; parameters: Core_IKeyDerivationParameters; desiredKeySize: Cardinal): IBuffer;
begin
  Result := Statics.DeriveKeyMaterial(key, parameters, desiredKeySize);
end;


class function TCore_CryptographicEngine.SignHashedData(key: Core_ICryptographicKey; data: IBuffer): IBuffer;
begin
  Result := Statics2.SignHashedData(key, data);
end;

class function TCore_CryptographicEngine.VerifySignatureWithHashInput(key: Core_ICryptographicKey; data: IBuffer; signature: IBuffer): Boolean;
begin
  Result := Statics2.VerifySignatureWithHashInput(key, data, signature);
end;

class function TCore_CryptographicEngine.DecryptAsync(key: Core_ICryptographicKey; data: IBuffer; iv: IBuffer): IAsyncOperation_1__IBuffer;
begin
  Result := Statics2.DecryptAsync(key, data, iv);
end;

class function TCore_CryptographicEngine.SignAsync(key: Core_ICryptographicKey; data: IBuffer): IAsyncOperation_1__IBuffer;
begin
  Result := Statics2.SignAsync(key, data);
end;

class function TCore_CryptographicEngine.SignHashedDataAsync(key: Core_ICryptographicKey; data: IBuffer): IAsyncOperation_1__IBuffer;
begin
  Result := Statics2.SignHashedDataAsync(key, data);
end;


{ TCore_EccCurveNames }

class function TCore_EccCurveNames.get_BrainpoolP160r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP160r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP160t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP160t1;
end;

class function TCore_EccCurveNames.get_BrainpoolP192r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP192r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP192t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP192t1;
end;

class function TCore_EccCurveNames.get_BrainpoolP224r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP224r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP224t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP224t1;
end;

class function TCore_EccCurveNames.get_BrainpoolP256r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP256r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP256t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP256t1;
end;

class function TCore_EccCurveNames.get_BrainpoolP320r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP320r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP320t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP320t1;
end;

class function TCore_EccCurveNames.get_BrainpoolP384r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP384r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP384t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP384t1;
end;

class function TCore_EccCurveNames.get_BrainpoolP512r1: HSTRING;
begin
  Result := Statics.get_BrainpoolP512r1;
end;

class function TCore_EccCurveNames.get_BrainpoolP512t1: HSTRING;
begin
  Result := Statics.get_BrainpoolP512t1;
end;

class function TCore_EccCurveNames.get_Curve25519: HSTRING;
begin
  Result := Statics.get_Curve25519;
end;

class function TCore_EccCurveNames.get_Ec192wapi: HSTRING;
begin
  Result := Statics.get_Ec192wapi;
end;

class function TCore_EccCurveNames.get_NistP192: HSTRING;
begin
  Result := Statics.get_NistP192;
end;

class function TCore_EccCurveNames.get_NistP224: HSTRING;
begin
  Result := Statics.get_NistP224;
end;

class function TCore_EccCurveNames.get_NistP256: HSTRING;
begin
  Result := Statics.get_NistP256;
end;

class function TCore_EccCurveNames.get_NistP384: HSTRING;
begin
  Result := Statics.get_NistP384;
end;

class function TCore_EccCurveNames.get_NistP521: HSTRING;
begin
  Result := Statics.get_NistP521;
end;

class function TCore_EccCurveNames.get_NumsP256t1: HSTRING;
begin
  Result := Statics.get_NumsP256t1;
end;

class function TCore_EccCurveNames.get_NumsP384t1: HSTRING;
begin
  Result := Statics.get_NumsP384t1;
end;

class function TCore_EccCurveNames.get_NumsP512t1: HSTRING;
begin
  Result := Statics.get_NumsP512t1;
end;

class function TCore_EccCurveNames.get_SecP160k1: HSTRING;
begin
  Result := Statics.get_SecP160k1;
end;

class function TCore_EccCurveNames.get_SecP160r1: HSTRING;
begin
  Result := Statics.get_SecP160r1;
end;

class function TCore_EccCurveNames.get_SecP160r2: HSTRING;
begin
  Result := Statics.get_SecP160r2;
end;

class function TCore_EccCurveNames.get_SecP192k1: HSTRING;
begin
  Result := Statics.get_SecP192k1;
end;

class function TCore_EccCurveNames.get_SecP192r1: HSTRING;
begin
  Result := Statics.get_SecP192r1;
end;

class function TCore_EccCurveNames.get_SecP224k1: HSTRING;
begin
  Result := Statics.get_SecP224k1;
end;

class function TCore_EccCurveNames.get_SecP224r1: HSTRING;
begin
  Result := Statics.get_SecP224r1;
end;

class function TCore_EccCurveNames.get_SecP256k1: HSTRING;
begin
  Result := Statics.get_SecP256k1;
end;

class function TCore_EccCurveNames.get_SecP256r1: HSTRING;
begin
  Result := Statics.get_SecP256r1;
end;

class function TCore_EccCurveNames.get_SecP384r1: HSTRING;
begin
  Result := Statics.get_SecP384r1;
end;

class function TCore_EccCurveNames.get_SecP521r1: HSTRING;
begin
  Result := Statics.get_SecP521r1;
end;

class function TCore_EccCurveNames.get_Wtls7: HSTRING;
begin
  Result := Statics.get_Wtls7;
end;

class function TCore_EccCurveNames.get_Wtls9: HSTRING;
begin
  Result := Statics.get_Wtls9;
end;

class function TCore_EccCurveNames.get_Wtls12: HSTRING;
begin
  Result := Statics.get_Wtls12;
end;

class function TCore_EccCurveNames.get_X962P192v1: HSTRING;
begin
  Result := Statics.get_X962P192v1;
end;

class function TCore_EccCurveNames.get_X962P192v2: HSTRING;
begin
  Result := Statics.get_X962P192v2;
end;

class function TCore_EccCurveNames.get_X962P192v3: HSTRING;
begin
  Result := Statics.get_X962P192v3;
end;

class function TCore_EccCurveNames.get_X962P239v1: HSTRING;
begin
  Result := Statics.get_X962P239v1;
end;

class function TCore_EccCurveNames.get_X962P239v2: HSTRING;
begin
  Result := Statics.get_X962P239v2;
end;

class function TCore_EccCurveNames.get_X962P239v3: HSTRING;
begin
  Result := Statics.get_X962P239v3;
end;

class function TCore_EccCurveNames.get_X962P256v1: HSTRING;
begin
  Result := Statics.get_X962P256v1;
end;

class function TCore_EccCurveNames.get_AllEccCurveNames: IVectorView_1__HSTRING;
begin
  Result := Statics.get_AllEccCurveNames;
end;


{ TCore_HashAlgorithmNames }

class function TCore_HashAlgorithmNames.get_Md5: HSTRING;
begin
  Result := Statics.get_Md5;
end;

class function TCore_HashAlgorithmNames.get_Sha1: HSTRING;
begin
  Result := Statics.get_Sha1;
end;

class function TCore_HashAlgorithmNames.get_Sha256: HSTRING;
begin
  Result := Statics.get_Sha256;
end;

class function TCore_HashAlgorithmNames.get_Sha384: HSTRING;
begin
  Result := Statics.get_Sha384;
end;

class function TCore_HashAlgorithmNames.get_Sha512: HSTRING;
begin
  Result := Statics.get_Sha512;
end;


{ TCore_HashAlgorithmProvider }

class function TCore_HashAlgorithmProvider.OpenAlgorithm(algorithm: HSTRING): Core_IHashAlgorithmProvider;
begin
  Result := Statics.OpenAlgorithm(algorithm);
end;


{ TCore_KeyDerivationAlgorithmNames }

class function TCore_KeyDerivationAlgorithmNames.get_Pbkdf2Md5: HSTRING;
begin
  Result := Statics.get_Pbkdf2Md5;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Pbkdf2Sha1: HSTRING;
begin
  Result := Statics.get_Pbkdf2Sha1;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Pbkdf2Sha256: HSTRING;
begin
  Result := Statics.get_Pbkdf2Sha256;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Pbkdf2Sha384: HSTRING;
begin
  Result := Statics.get_Pbkdf2Sha384;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Pbkdf2Sha512: HSTRING;
begin
  Result := Statics.get_Pbkdf2Sha512;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp800108CtrHmacMd5: HSTRING;
begin
  Result := Statics.get_Sp800108CtrHmacMd5;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp800108CtrHmacSha1: HSTRING;
begin
  Result := Statics.get_Sp800108CtrHmacSha1;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp800108CtrHmacSha256: HSTRING;
begin
  Result := Statics.get_Sp800108CtrHmacSha256;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp800108CtrHmacSha384: HSTRING;
begin
  Result := Statics.get_Sp800108CtrHmacSha384;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp800108CtrHmacSha512: HSTRING;
begin
  Result := Statics.get_Sp800108CtrHmacSha512;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp80056aConcatMd5: HSTRING;
begin
  Result := Statics.get_Sp80056aConcatMd5;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp80056aConcatSha1: HSTRING;
begin
  Result := Statics.get_Sp80056aConcatSha1;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp80056aConcatSha256: HSTRING;
begin
  Result := Statics.get_Sp80056aConcatSha256;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp80056aConcatSha384: HSTRING;
begin
  Result := Statics.get_Sp80056aConcatSha384;
end;

class function TCore_KeyDerivationAlgorithmNames.get_Sp80056aConcatSha512: HSTRING;
begin
  Result := Statics.get_Sp80056aConcatSha512;
end;


class function TCore_KeyDerivationAlgorithmNames.get_CapiKdfMd5: HSTRING;
begin
  Result := Statics2.get_CapiKdfMd5;
end;

class function TCore_KeyDerivationAlgorithmNames.get_CapiKdfSha1: HSTRING;
begin
  Result := Statics2.get_CapiKdfSha1;
end;

class function TCore_KeyDerivationAlgorithmNames.get_CapiKdfSha256: HSTRING;
begin
  Result := Statics2.get_CapiKdfSha256;
end;

class function TCore_KeyDerivationAlgorithmNames.get_CapiKdfSha384: HSTRING;
begin
  Result := Statics2.get_CapiKdfSha384;
end;

class function TCore_KeyDerivationAlgorithmNames.get_CapiKdfSha512: HSTRING;
begin
  Result := Statics2.get_CapiKdfSha512;
end;


{ TCore_KeyDerivationAlgorithmProvider }

class function TCore_KeyDerivationAlgorithmProvider.OpenAlgorithm(algorithm: HSTRING): Core_IKeyDerivationAlgorithmProvider;
begin
  Result := Statics.OpenAlgorithm(algorithm);
end;


{ TCore_KeyDerivationParameters }

class function TCore_KeyDerivationParameters.BuildForPbkdf2(pbkdf2Salt: IBuffer; iterationCount: Cardinal): Core_IKeyDerivationParameters;
begin
  Result := Statics.BuildForPbkdf2(pbkdf2Salt, iterationCount);
end;

class function TCore_KeyDerivationParameters.BuildForSP800108(&label: IBuffer; context: IBuffer): Core_IKeyDerivationParameters;
begin
  Result := Statics.BuildForSP800108(&label, context);
end;

class function TCore_KeyDerivationParameters.BuildForSP80056a(algorithmId: IBuffer; partyUInfo: IBuffer; partyVInfo: IBuffer; suppPubInfo: IBuffer; suppPrivInfo: IBuffer): Core_IKeyDerivationParameters;
begin
  Result := Statics.BuildForSP80056a(algorithmId, partyUInfo, partyVInfo, suppPubInfo, suppPrivInfo);
end;


class function TCore_KeyDerivationParameters.BuildForCapi1Kdf(capi1KdfTargetAlgorithm: Core_Capi1KdfTargetAlgorithm): Core_IKeyDerivationParameters;
begin
  Result := Statics2.BuildForCapi1Kdf(capi1KdfTargetAlgorithm);
end;


{ TCore_MacAlgorithmNames }

class function TCore_MacAlgorithmNames.get_HmacMd5: HSTRING;
begin
  Result := Statics.get_HmacMd5;
end;

class function TCore_MacAlgorithmNames.get_HmacSha1: HSTRING;
begin
  Result := Statics.get_HmacSha1;
end;

class function TCore_MacAlgorithmNames.get_HmacSha256: HSTRING;
begin
  Result := Statics.get_HmacSha256;
end;

class function TCore_MacAlgorithmNames.get_HmacSha384: HSTRING;
begin
  Result := Statics.get_HmacSha384;
end;

class function TCore_MacAlgorithmNames.get_HmacSha512: HSTRING;
begin
  Result := Statics.get_HmacSha512;
end;

class function TCore_MacAlgorithmNames.get_AesCmac: HSTRING;
begin
  Result := Statics.get_AesCmac;
end;


{ TCore_MacAlgorithmProvider }

class function TCore_MacAlgorithmProvider.OpenAlgorithm(algorithm: HSTRING): Core_IMacAlgorithmProvider;
begin
  Result := Statics.OpenAlgorithm(algorithm);
end;


{ TCore_PersistedKeyProvider }

class function TCore_PersistedKeyProvider.OpenKeyPairFromCertificateAsync(certificate: Certificates_ICertificate; hashAlgorithmName: HSTRING; padding: Core_CryptographicPadding): IAsyncOperation_1__Core_ICryptographicKey;
begin
  Result := Statics.OpenKeyPairFromCertificateAsync(certificate, hashAlgorithmName, padding);
end;

class function TCore_PersistedKeyProvider.OpenPublicKeyFromCertificate(certificate: Certificates_ICertificate; hashAlgorithmName: HSTRING; padding: Core_CryptographicPadding): Core_ICryptographicKey;
begin
  Result := Statics.OpenPublicKeyFromCertificate(certificate, hashAlgorithmName, padding);
end;


{ TCore_SymmetricAlgorithmNames }

class function TCore_SymmetricAlgorithmNames.get_DesCbc: HSTRING;
begin
  Result := Statics.get_DesCbc;
end;

class function TCore_SymmetricAlgorithmNames.get_DesEcb: HSTRING;
begin
  Result := Statics.get_DesEcb;
end;

class function TCore_SymmetricAlgorithmNames.get_TripleDesCbc: HSTRING;
begin
  Result := Statics.get_TripleDesCbc;
end;

class function TCore_SymmetricAlgorithmNames.get_TripleDesEcb: HSTRING;
begin
  Result := Statics.get_TripleDesEcb;
end;

class function TCore_SymmetricAlgorithmNames.get_Rc2Cbc: HSTRING;
begin
  Result := Statics.get_Rc2Cbc;
end;

class function TCore_SymmetricAlgorithmNames.get_Rc2Ecb: HSTRING;
begin
  Result := Statics.get_Rc2Ecb;
end;

class function TCore_SymmetricAlgorithmNames.get_AesCbc: HSTRING;
begin
  Result := Statics.get_AesCbc;
end;

class function TCore_SymmetricAlgorithmNames.get_AesEcb: HSTRING;
begin
  Result := Statics.get_AesEcb;
end;

class function TCore_SymmetricAlgorithmNames.get_AesGcm: HSTRING;
begin
  Result := Statics.get_AesGcm;
end;

class function TCore_SymmetricAlgorithmNames.get_AesCcm: HSTRING;
begin
  Result := Statics.get_AesCcm;
end;

class function TCore_SymmetricAlgorithmNames.get_AesCbcPkcs7: HSTRING;
begin
  Result := Statics.get_AesCbcPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_AesEcbPkcs7: HSTRING;
begin
  Result := Statics.get_AesEcbPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_DesCbcPkcs7: HSTRING;
begin
  Result := Statics.get_DesCbcPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_DesEcbPkcs7: HSTRING;
begin
  Result := Statics.get_DesEcbPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_TripleDesCbcPkcs7: HSTRING;
begin
  Result := Statics.get_TripleDesCbcPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_TripleDesEcbPkcs7: HSTRING;
begin
  Result := Statics.get_TripleDesEcbPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_Rc2CbcPkcs7: HSTRING;
begin
  Result := Statics.get_Rc2CbcPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_Rc2EcbPkcs7: HSTRING;
begin
  Result := Statics.get_Rc2EcbPkcs7;
end;

class function TCore_SymmetricAlgorithmNames.get_Rc4: HSTRING;
begin
  Result := Statics.get_Rc4;
end;


{ TCore_SymmetricKeyAlgorithmProvider }

class function TCore_SymmetricKeyAlgorithmProvider.OpenAlgorithm(algorithm: HSTRING): Core_ISymmetricKeyAlgorithmProvider;
begin
  Result := Statics.OpenAlgorithm(algorithm);
end;


{ TCryptographicBuffer }

class function TCryptographicBuffer.Compare(object1: IBuffer; object2: IBuffer): Boolean;
begin
  Result := Statics.Compare(object1, object2);
end;

class function TCryptographicBuffer.GenerateRandom(length: Cardinal): IBuffer;
begin
  Result := Statics.GenerateRandom(length);
end;

class function TCryptographicBuffer.GenerateRandomNumber: Cardinal;
begin
  Result := Statics.GenerateRandomNumber;
end;

class function TCryptographicBuffer.CreateFromByteArray(valueSize: Cardinal; value: PByte): IBuffer;
begin
  Result := Statics.CreateFromByteArray(valueSize, value);
end;

class procedure TCryptographicBuffer.CopyToByteArray(buffer: IBuffer; valueSize: Cardinal; value: PByte);
begin
  Statics.CopyToByteArray(buffer, valueSize, value);
end;

class function TCryptographicBuffer.DecodeFromHexString(value: HSTRING): IBuffer;
begin
  Result := Statics.DecodeFromHexString(value);
end;

class function TCryptographicBuffer.EncodeToHexString(buffer: IBuffer): HSTRING;
begin
  Result := Statics.EncodeToHexString(buffer);
end;

class function TCryptographicBuffer.DecodeFromBase64String(value: HSTRING): IBuffer;
begin
  Result := Statics.DecodeFromBase64String(value);
end;

class function TCryptographicBuffer.EncodeToBase64String(buffer: IBuffer): HSTRING;
begin
  Result := Statics.EncodeToBase64String(buffer);
end;

class function TCryptographicBuffer.ConvertStringToBinary(value: HSTRING; encoding: BinaryStringEncoding): IBuffer;
begin
  Result := Statics.ConvertStringToBinary(value, encoding);
end;

class function TCryptographicBuffer.ConvertBinaryToString(encoding: BinaryStringEncoding; buffer: IBuffer): HSTRING;
begin
  Result := Statics.ConvertBinaryToString(encoding, buffer);
end;


{ TDataProtection_DataProtectionProvider }
// Factories for : "DataProtection_DataProtectionProvider"
// Factory: "Windows.Security.Cryptography.DataProtection.IDataProtectionProviderFactory"
// -> DataProtection_IDataProtectionProviderFactory

class function TDataProtection_DataProtectionProvider.CreateOverloadExplicit(protectionDescriptor: HSTRING): DataProtection_IDataProtectionProvider;
begin
  Result := Factory.CreateOverloadExplicit(protectionDescriptor);
end;


end.
