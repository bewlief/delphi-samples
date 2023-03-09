{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
/// <summary>
/// Declares various common types and constants that are used throughout the REST library.
/// </summary>
unit REST.Types;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math;

type
  TMethod = Procedure of Object;
  TMethod<T> = Procedure(Arg1: T) of Object;

  /// <summary>
  ///   A TCompletionHandler is an anonymous method, that gets called after a asynchronous operation successfully
  ///   completes.
  /// </summary>
  TCompletionHandler = TProc;
  TCompletionHandlerWithError = TProc<TObject>;

  TRESTObjectOwnership = (ooCopy, ooREST, ooApp);

  /// <summary>
  /// Flags that control the utilization of request-parameters
  /// </summary>
  TRESTRequestParameterOption = (
    /// <summary>
    /// Indicates that the value of this parameter should be used as-is
    /// and not encoded by the component
    /// </summary>
    poDoNotEncode,
    /// <summary>
    ///  A transient parameter is typically created and managed by
    ///  attached components (like an authenticator) at runtime
    /// </summary>
    poTransient,
    /// <summary>
    /// Indicates that a parameter was created by the component
    /// while parsing the base-url or resource
    /// </summary>
    poAutoCreated,
    /// <summary>
    /// Indicates that the ';' separated list of values of this parameter
    /// should be expanded into name=val1&...&name=valN
    /// </summary>
    poFlatArray,
    /// <summary>
    /// Indicates that the ';' separated list of values of this parameter
    /// should be expanded into name[]=val1&...&name[]=valN
    /// </summary>
    poPHPArray,
    /// <summary>
    /// Indicates that the ';' separated list of values of this parameter
    /// should be expanded into name=val1,...,valN
    /// </summary>
    poListArray
  );

  TRESTRequestParameterOptions = set of TRESTRequestParameterOption;

  /// <summary>
  /// Types of parameters that can be added to requests
  /// </summary>
  TRESTRequestParameterKind = (
    /// <summary>
    /// Parameter is put into a cookie
    /// </summary>
    pkCOOKIE,
    /// <summary>
    /// Parameter will sent as URL parameter (for GET requests) or as body
    /// parameter (for POST/PUT requests)
    /// </summary>
    pkGETorPOST,
    /// <summary>
    /// <para>
    /// Parameter will used as value for a URL segment. A URL segment can be
    /// defined in a request's resource path: customer/{ID}
    /// </para>
    /// <para>
    /// If a URL Segment parameter with a name of "ID", then its value will be
    /// replaced for {ID} in the example above
    /// </para>
    /// </summary>
    pkURLSEGMENT,
    /// <summary>
    /// Parameter will be put in the request's HTTP header
    /// </summary>
    pkHTTPHEADER,
    /// <summary>
    /// The parameter's value will be used as request body. If more than one
    /// RequestBody parameter exists, the request will use a multi-part body.
    /// </summary>
    pkREQUESTBODY,
    /// <summary>
    /// The parameter's value will be used to attach file content to the request.
    /// If this parameter exists, the request will use a multi-part body.
    /// </summary>
    pkFILE,
    /// <summary>
    /// Parameter will be explicitly sent as URL parameter (for all requests) in
    /// contrast to pkGETorPOST, when parameter location depends on request type.
    /// </summary>
    pkQUERY
  );

var
  DefaultRESTRequestParameterKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkGETorPOST;

function RESTRequestParameterKindToString(const AKind: TRESTRequestParameterKind): string;
function RESTRequestParameterKindFromString(const AKindString: string): TRESTRequestParameterKind;

type
  /// <summary>
  /// Content type.
  /// </summary>
  TRESTContentType = type string;

const
  // CONTENTTYPE_XXXX ----------------------------------------------------------
  /// <summary>
  /// HTTP Content-Type (or MIME Types as per RFC 2046) header definitions.
  /// </summary>
  /// <remarks>
  /// <para>
  /// See: http://tools.ietf.org/html/rfc2046
  /// </para>
  /// <para>
  /// Values collected from https://en.wikipedia.org/wiki/MIME_type
  /// </para>
  /// </remarks>
  CONTENTTYPE_NONE = ''; // do not localize
  // Type Application
  CONTENTTYPE_APPLICATION_ATOM_XML = 'application/atom+xml'; // do not localize
  CONTENTTYPE_APPLICATION_ECMASCRIPT = 'application/ecmascript'; // do not localize
  CONTENTTYPE_APPLICATION_EDI_X12 = 'application/EDI-X12'; // do not localize
  CONTENTTYPE_APPLICATION_EDIFACT = 'application/EDIFACT'; // do not localize
  CONTENTTYPE_APPLICATION_JSON = 'application/json'; // do not localize
  CONTENTTYPE_APPLICATION_JAVASCRIPT = 'application/javascript'; // do not localize
  CONTENTTYPE_APPLICATION_OCTET_STREAM = 'application/octet-stream'; // do not localize
  CONTENTTYPE_APPLICATION_OGG = 'application/ogg'; // do not localize
  CONTENTTYPE_APPLICATION_PDF = 'application/pdf'; // do not localize
  CONTENTTYPE_APPLICATION_POSTSCRIPT = 'application/postscript'; // do not localize
  CONTENTTYPE_APPLICATION_RDF_XML = 'application/rdf+xml'; // do not localize
  CONTENTTYPE_APPLICATION_RSS_XML = 'application/rss+xml'; // do not localize
  CONTENTTYPE_APPLICATION_SOAP_XML = 'application/soap+xml'; // do not localize
  CONTENTTYPE_APPLICATION_FONT_WOFF = 'application/font-woff'; // do not localize
  CONTENTTYPE_APPLICATION_XHTML_XML = 'application/xhtml+xml'; // do not localize
  CONTENTTYPE_APPLICATION_XML = 'application/xml'; // do not localize
  CONTENTTYPE_APPLICATION_XML_DTD = 'application/xml-dtd'; // do not localize
  CONTENTTYPE_APPLICATION_XOP_XML = 'application/xop+xml'; // do not localize
  CONTENTTYPE_APPLICATION_ZIP = 'application/zip'; // do not localize
  CONTENTTYPE_APPLICATION_GZIP = 'application/gzip'; // do not localize
  // Type Text
  CONTENTTYPE_TEXT_CMD = 'text/cmd'; // do not localize
  CONTENTTYPE_TEXT_CSS = 'text/css'; // do not localize
  CONTENTTYPE_TEXT_CSV = 'text/csv'; // do not localize
  CONTENTTYPE_TEXT_HTML = 'text/html'; // do not localize
  CONTENTTYPE_TEXT_JAVASCRIPT = 'text/javascript'; // do not localize
  CONTENTTYPE_TEXT_PLAIN = 'text/plain'; // do not localize
  CONTENTTYPE_TEXT_VCARD = 'text/vcard'; // do not localize
  CONTENTTYPE_TEXT_XML = 'text/xml'; // do not localize
  // Type Audio
  CONTENTTYPE_AUDIO_BASIC = 'audio/basic'; // do not localize
  CONTENTTYPE_AUDIO_L24 = 'audio/L24'; // do not localize
  CONTENTTYPE_AUDIO_MP4 = 'audio/mp4'; // do not localize
  CONTENTTYPE_AUDIO_MPEG = 'audio/mpeg'; // do not localize
  CONTENTTYPE_AUDIO_OGG = 'audio/ogg'; // do not localize
  CONTENTTYPE_AUDIO_VORBIS = 'audio/vorbis'; // do not localize
  CONTENTTYPE_AUDIO_VND_RN_REALAUDIO = 'audio/vnd.rn-realaudio'; // do not localize
  CONTENTTYPE_AUDIO_VND_WAVE = 'audio/vnd.wave'; // do not localize
  CONTENTTYPE_AUDIO_WEBM = 'audio/webm'; // do not localize
  // Type Image
  CONTENTTYPE_IMAGE_GIF = 'image/gif'; // do not localize
  CONTENTTYPE_IMAGE_JPEG = 'image/jpeg'; // do not localize
  CONTENTTYPE_IMAGE_PJPEG = 'image/pjpeg'; // do not localize
  CONTENTTYPE_IMAGE_PNG = 'image/png'; // do not localize
  CONTENTTYPE_IMAGE_SVG_XML = 'image/svg+xml'; // do not localize
  CONTENTTYPE_IMAGE_TIFF = 'image/tiff'; // do not localize
  // Type Message
  CONTENTTYPE_MESSAGE_HTTP = 'message/http'; // do not localize
  CONTENTTYPE_MESSAGE_IMDN_XML = 'message/imdn+xml'; // do not localize
  CONTENTTYPE_MESSAGE_PARTIAL = 'message/partial'; // do not localize
  CONTENTTYPE_MESSAGE_RFC822 = 'message/rfc822'; // do not localize
  // Type Model (3D Models)
  CONTENTTYPE_MODEL_EXAMPLE = 'model/example'; // do not localize
  CONTENTTYPE_MODEL_IGES = 'model/iges'; // do not localize
  CONTENTTYPE_MODEL_MESH = 'model/mesh'; // do not localize
  CONTENTTYPE_MODEL_VRML = 'model/vrml'; // do not localize
  CONTENTTYPE_MODEL_X3D_BINARY = 'model/x3d+binary'; // do not localize
  CONTENTTYPE_MODEL_X3D_VRML = 'model/x3d+vrml'; // do not localize
  CONTENTTYPE_MODEL_X3D_XML = 'model/x3d+xml'; // do not localize
  // Type Multipart
  CONTENTTYPE_MULTIPART_MIXED = 'multipart/mixed'; // do not localize
  CONTENTTYPE_MULTIPART_ALTERNATIVE = 'multipart/alternative'; // do not localize
  CONTENTTYPE_MULTIPART_RELATED = 'multipart/related'; // do not localize
  CONTENTTYPE_MULTIPART_FORM_DATA = 'multipart/form-data'; // do not localize
  CONTENTTYPE_MULTIPART_SIGNED = 'multipart/signed'; // do not localize
  CONTENTTYPE_MULTIPART_ENCRYPTED = 'multipart/encrypted'; // do not localize
  // Type Video
  CONTENTTYPE_VIDEO_MPEG = 'video/mpeg'; // do not localize
  CONTENTTYPE_VIDEO_MP4 = 'video/mp4'; // do not localize
  CONTENTTYPE_VIDEO_OGG = 'video/ogg'; // do not localize
  CONTENTTYPE_VIDEO_QUICKTIME = 'video/quicktime'; // do not localize
  CONTENTTYPE_VIDEO_WEBM = 'video/webm'; // do not localize
  CONTENTTYPE_VIDEO_X_MATROSKA = 'video/x-matroska'; // do not localize
  CONTENTTYPE_VIDEO_X_MS_WMV = 'video/x-ms-wmv'; // do not localize
  CONTENTTYPE_VIDEO_X_FLV = 'video/x-flv'; // do not localize
  // Type Application - Vendor Specific
  CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_TEXT = 'application/vnd.oasis.opendocument.text'; // do not localize
  CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET = 'application/vnd.oasis.opendocument.spreadsheet';
  // do not localize
  CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION = 'application/vnd.oasis.opendocument.presentation';
  // do not localize
  CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS = 'application/vnd.oasis.opendocument.graphics';
  // do not localize
  CONTENTTYPE_APPLICATION_VND_MS_EXCEL = 'application/vnd.ms-excel'; // do not localize
  CONTENTTYPE_APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET =
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'; // do not localize
  CONTENTTYPE_APPLICATION_VND_MS_POWERPOINT = 'application/vnd.ms-powerpoint'; // do not localize
  CONTENTTYPE_APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION =
    'application/vnd.openxmlformats-officedocument.presentationml.presentation'; // do not localize
  CONTENTTYPE_APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT =
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document'; // do not localize
  CONTENTTYPE_APPLICATION_VND_MOZILLA_XUL_XML = 'application/vnd.mozilla.xul+xml'; // do not localize
  CONTENTTYPE_APPLICATION_VND_GOOGLE_EARTH_KML_XML = 'application/vnd.google-earth.kml+xml'; // do not localize
  CONTENTTYPE_APPLICATION_VND_GOOGLE_EARTH_KMZ = 'application/vnd.google-earth.kmz'; // do not localize
  CONTENTTYPE_APPLICATION_VND_DART = 'application/vnd.dart'; // do not localize
  CONTENTTYPE_APPLICATION_VND_ANDROID_PACKAGE_ARCHIVE = 'application/vnd.android.package-archive'; // do not localize
  // Type X (RFC 6648)
  CONTENTTYPE_APPLICATION_X_DEB = 'application/x-deb'; // do not localize
  CONTENTTYPE_APPLICATION_X_DVI = 'application/x-dvi'; // do not localize
  CONTENTTYPE_APPLICATION_X_FONT_TTF = 'application/x-font-ttf'; // do not localize
  CONTENTTYPE_APPLICATION_X_JAVASCRIPT = 'application/x-javascript'; // do not localize
  CONTENTTYPE_APPLICATION_X_LATEX = 'application/x-latex'; // do not localize
  CONTENTTYPE_APPLICATION_X_MPEGURL = 'application/x-mpegURL'; // do not localize
  CONTENTTYPE_APPLICATION_X_RAR_COMPRESSED = 'application/x-rar-compressed'; // do not localize
  CONTENTTYPE_APPLICATION_X_SHOCKWAVE_FLASH = 'application/x-shockwave-flash'; // do not localize
  CONTENTTYPE_APPLICATION_X_STUFFIT = 'application/x-stuffit'; // do not localize
  CONTENTTYPE_APPLICATION_X_TAR = 'application/x-tar'; // do not localize
  CONTENTTYPE_APPLICATION_X_WWW_FORM_URLENCODED = 'application/x-www-form-urlencoded'; // do not localize
  CONTENTTYPE_APPLICATION_X_XPINSTALL = 'application/x-xpinstall'; // do not localize
  CONTENTTYPE_AUDIO_X_AAC = 'audio/x-aac'; // do not localize
  CONTENTTYPE_AUDIO_X_CAF = 'audio/x-caf'; // do not localize
  CONTENTTYPE_IMAGE_X_XCF = 'image/x-xcf'; // do not localize
  CONTENTTYPE_TEXT_X_GWT_RPC = 'text/x-gwt-rpc'; // do not localize
  CONTENTTYPE_TEXT_X_JQUERY_TMPL = 'text/x-jquery-tmpl'; // do not localize
  CONTENTTYPE_TEXT_X_MARKDOWN = 'text/x-markdown'; // do not localize
  // Type PKCS (Cryptography)
  CONTENTTYPE_APPLICATION_X_PKCS12 = 'application/x-pkcs12'; // do not localize
  CONTENTTYPE_APPLICATION_X_PKCS7_CERTIFICATES = 'application/x-pkcs7-certificates'; // do not localize
  CONTENTTYPE_APPLICATION_X_PKCS7_CERTREQRESP = 'application/x-pkcs7-certreqresp'; // do not localize
  CONTENTTYPE_APPLICATION_X_PKCS7_MIME = 'application/x-pkcs7-mime'; // do not localize
  CONTENTTYPE_APPLICATION_X_PKCS7_SIGNATURE = 'application/x-pkcs7-signature'; // do not localize
  // Type Application - Embarcadero Specific
  CONTENTTYPE_APPLICATION_VND_EMBARCADERO_FIREDAC_JSON = 'application/vnd.embarcadero.firedac+json'; // do not localize

  // ctXXXX --------------------------------------------------------------------
  ctNONE = CONTENTTYPE_NONE;
  // Type Application
  ctAPPLICATION_ATOM_XML = CONTENTTYPE_APPLICATION_ATOM_XML;
  ctAPPLICATION_ECMASCRIPT = CONTENTTYPE_APPLICATION_ECMASCRIPT;
  ctAPPLICATION_EDI_X12 = CONTENTTYPE_APPLICATION_EDI_X12;
  ctAPPLICATION_EDIFACT = CONTENTTYPE_APPLICATION_EDIFACT;
  ctAPPLICATION_JSON = CONTENTTYPE_APPLICATION_JSON;
  ctAPPLICATION_JAVASCRIPT = CONTENTTYPE_APPLICATION_JAVASCRIPT;
  ctAPPLICATION_OCTET_STREAM = CONTENTTYPE_APPLICATION_OCTET_STREAM;
  ctAPPLICATION_OGG = CONTENTTYPE_APPLICATION_OGG;
  ctAPPLICATION_PDF = CONTENTTYPE_APPLICATION_PDF;
  ctAPPLICATION_POSTSCRIPT = CONTENTTYPE_APPLICATION_POSTSCRIPT;
  ctAPPLICATION_RDF_XML = CONTENTTYPE_APPLICATION_RDF_XML;
  ctAPPLICATION_RSS_XML = CONTENTTYPE_APPLICATION_RSS_XML;
  ctAPPLICATION_SOAP_XML = CONTENTTYPE_APPLICATION_SOAP_XML;
  ctAPPLICATION_FONT_WOFF = CONTENTTYPE_APPLICATION_FONT_WOFF;
  ctAPPLICATION_XHTML_XML = CONTENTTYPE_APPLICATION_XHTML_XML;
  ctAPPLICATION_XML = CONTENTTYPE_APPLICATION_XML;
  ctAPPLICATION_XML_DTD = CONTENTTYPE_APPLICATION_XML_DTD;
  ctAPPLICATION_XOP_XML = CONTENTTYPE_APPLICATION_XOP_XML;
  ctAPPLICATION_ZIP = CONTENTTYPE_APPLICATION_ZIP;
  ctAPPLICATION_GZIP = CONTENTTYPE_APPLICATION_GZIP;
  // Type Text
  ctTEXT_CMD = CONTENTTYPE_TEXT_CMD;
  ctTEXT_CSS = CONTENTTYPE_TEXT_CSS;
  ctTEXT_CSV = CONTENTTYPE_TEXT_CSV;
  ctTEXT_HTML = CONTENTTYPE_TEXT_HTML;
  ctTEXT_JAVASCRIPT = CONTENTTYPE_TEXT_JAVASCRIPT;
  ctTEXT_PLAIN = CONTENTTYPE_TEXT_PLAIN;
  ctTEXT_VCARD = CONTENTTYPE_TEXT_VCARD;
  ctTEXT_XML = CONTENTTYPE_TEXT_XML;
  // Type Audio
  ctAUDIO_BASIC = CONTENTTYPE_AUDIO_BASIC;
  ctAUDIO_L24 = CONTENTTYPE_AUDIO_L24;
  ctAUDIO_MP4 = CONTENTTYPE_AUDIO_MP4;
  ctAUDIO_MPEG = CONTENTTYPE_AUDIO_MPEG;
  ctAUDIO_OGG = CONTENTTYPE_AUDIO_OGG;
  ctAUDIO_VORBIS = CONTENTTYPE_AUDIO_VORBIS;
  ctAUDIO_VND_RN_REALAUDIO = CONTENTTYPE_AUDIO_VND_RN_REALAUDIO;
  ctAUDIO_VND_WAVE = CONTENTTYPE_AUDIO_VND_WAVE;
  ctAUDIO_WEBM = CONTENTTYPE_AUDIO_WEBM;
  // Type Image
  ctIMAGE_GIF = CONTENTTYPE_IMAGE_GIF;
  ctIMAGE_JPEG = CONTENTTYPE_IMAGE_JPEG;
  ctIMAGE_PJPEG = CONTENTTYPE_IMAGE_PJPEG;
  ctIMAGE_PNG = CONTENTTYPE_IMAGE_PNG;
  ctIMAGE_SVG_XML = CONTENTTYPE_IMAGE_SVG_XML;
  ctIMAGE_TIFF = CONTENTTYPE_IMAGE_TIFF;
  // Type Message
  ctMESSAGE_HTTP = CONTENTTYPE_MESSAGE_HTTP;
  ctMESSAGE_IMDN_XML = CONTENTTYPE_MESSAGE_IMDN_XML;
  ctMESSAGE_PARTIAL = CONTENTTYPE_MESSAGE_PARTIAL;
  ctMESSAGE_RFC822 = CONTENTTYPE_MESSAGE_RFC822;
  // Type Model (3D Models)
  ctMODEL_EXAMPLE = CONTENTTYPE_MODEL_EXAMPLE;
  ctMODEL_IGES = CONTENTTYPE_MODEL_IGES;
  ctMODEL_MESH = CONTENTTYPE_MODEL_MESH;
  ctMODEL_VRML = CONTENTTYPE_MODEL_VRML;
  ctMODEL_X3D_BINARY = CONTENTTYPE_MODEL_X3D_BINARY;
  ctMODEL_X3D_VRML = CONTENTTYPE_MODEL_X3D_VRML;
  ctMODEL_X3D_XML = CONTENTTYPE_MODEL_X3D_XML;
  // Type Multipart
  ctMULTIPART_MIXED = CONTENTTYPE_MULTIPART_MIXED;
  ctMULTIPART_ALTERNATIVE = CONTENTTYPE_MULTIPART_ALTERNATIVE;
  ctMULTIPART_RELATED = CONTENTTYPE_MULTIPART_RELATED;
  ctMULTIPART_FORM_DATA = CONTENTTYPE_MULTIPART_FORM_DATA;
  ctMULTIPART_SIGNED = CONTENTTYPE_MULTIPART_SIGNED;
  ctMULTIPART_ENCRYPTED = CONTENTTYPE_MULTIPART_ENCRYPTED;
  // Type Video
  ctVIDEO_MPEG = CONTENTTYPE_VIDEO_MPEG;
  ctVIDEO_MP4 = CONTENTTYPE_VIDEO_MP4;
  ctVIDEO_OGG = CONTENTTYPE_VIDEO_OGG;
  ctVIDEO_QUICKTIME = CONTENTTYPE_VIDEO_QUICKTIME;
  ctVIDEO_WEBM = CONTENTTYPE_VIDEO_WEBM;
  ctVIDEO_X_MATROSKA = CONTENTTYPE_VIDEO_X_MATROSKA;
  ctVIDEO_X_MS_WMV = CONTENTTYPE_VIDEO_X_MS_WMV;
  ctVIDEO_X_FLV = CONTENTTYPE_VIDEO_X_FLV;
  // Type Application - Vendor Specific
  ctAPPLICATION_VND_OASIS_OPENDOCUMENT_TEXT = CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_TEXT;
  ctAPPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET = CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET;
  // do not localize
  ctAPPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION = CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION;
  // do not localize
  ctAPPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS = CONTENTTYPE_APPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS;
  // do not localize
  ctAPPLICATION_VND_MS_EXCEL = CONTENTTYPE_APPLICATION_VND_MS_EXCEL;
  ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET = CONTENTTYPE_APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET;
  ctAPPLICATION_VND_MS_POWERPOINT = CONTENTTYPE_APPLICATION_VND_MS_POWERPOINT;
  ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION = CONTENTTYPE_APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION;
  ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT = CONTENTTYPE_APPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT;
  ctAPPLICATION_VND_MOZILLA_XUL_XML = CONTENTTYPE_APPLICATION_VND_MOZILLA_XUL_XML;
  ctAPPLICATION_VND_GOOGLE_EARTH_KML_XML = CONTENTTYPE_APPLICATION_VND_GOOGLE_EARTH_KML_XML;
  ctAPPLICATION_VND_GOOGLE_EARTH_KMZ = CONTENTTYPE_APPLICATION_VND_GOOGLE_EARTH_KMZ;
  ctAPPLICATION_VND_DART = CONTENTTYPE_APPLICATION_VND_DART;
  ctAPPLICATION_VND_ANDROID_PACKAGE_ARCHIVE = CONTENTTYPE_APPLICATION_VND_ANDROID_PACKAGE_ARCHIVE;
  // Type X (RFC 6648)
  ctAPPLICATION_X_DEB = CONTENTTYPE_APPLICATION_X_DEB;
  ctAPPLICATION_X_DVI = CONTENTTYPE_APPLICATION_X_DVI;
  ctAPPLICATION_X_FONT_TTF = CONTENTTYPE_APPLICATION_X_FONT_TTF;
  ctAPPLICATION_X_JAVASCRIPT = CONTENTTYPE_APPLICATION_X_JAVASCRIPT;
  ctAPPLICATION_X_LATEX = CONTENTTYPE_APPLICATION_X_LATEX;
  ctAPPLICATION_X_MPEGURL = CONTENTTYPE_APPLICATION_X_MPEGURL;
  ctAPPLICATION_X_RAR_COMPRESSED = CONTENTTYPE_APPLICATION_X_RAR_COMPRESSED;
  ctAPPLICATION_X_SHOCKWAVE_FLASH = CONTENTTYPE_APPLICATION_X_SHOCKWAVE_FLASH;
  ctAPPLICATION_X_STUFFIT = CONTENTTYPE_APPLICATION_X_STUFFIT;
  ctAPPLICATION_X_TAR = CONTENTTYPE_APPLICATION_X_TAR;
  ctAPPLICATION_X_WWW_FORM_URLENCODED = CONTENTTYPE_APPLICATION_X_WWW_FORM_URLENCODED;
  ctAPPLICATION_X_XPINSTALL = CONTENTTYPE_APPLICATION_X_XPINSTALL;
  ctAUDIO_X_AAC = CONTENTTYPE_AUDIO_X_AAC;
  ctAUDIO_X_CAF = CONTENTTYPE_AUDIO_X_CAF;
  ctIMAGE_X_XCF = CONTENTTYPE_IMAGE_X_XCF;
  ctTEXT_X_GWT_RPC = CONTENTTYPE_TEXT_X_GWT_RPC;
  ctTEXT_X_JQUERY_TMPL = CONTENTTYPE_TEXT_X_JQUERY_TMPL;
  ctTEXT_X_MARKDOWN = CONTENTTYPE_TEXT_X_MARKDOWN;
  // Type PKCS (Cryptography)
  ctAPPLICATION_X_PKCS12 = CONTENTTYPE_APPLICATION_X_PKCS12;
  ctAPPLICATION_X_PKCS7_CERTIFICATES = CONTENTTYPE_APPLICATION_X_PKCS7_CERTIFICATES;
  ctAPPLICATION_X_PKCS7_CERTREQRESP = CONTENTTYPE_APPLICATION_X_PKCS7_CERTREQRESP;
  ctAPPLICATION_X_PKCS7_MIME = CONTENTTYPE_APPLICATION_X_PKCS7_MIME;
  ctAPPLICATION_X_PKCS7_SIGNATURE = CONTENTTYPE_APPLICATION_X_PKCS7_SIGNATURE;
  // Type Application - Embarcadero Specific
  ctAPPLICATION_VND_EMBARCADERO_FIREDAC_JSON = CONTENTTYPE_APPLICATION_VND_EMBARCADERO_FIREDAC_JSON;

  // TRESTContentType.ctXXXX ---------------------------------------------------
type
  TRESTContentTypeHelper = record helper for TRESTContentType
  public const
    ctNone = REST.Types.ctNone;
    // Type Application
    ctAPPLICATION_ATOM_XML = REST.Types.ctAPPLICATION_ATOM_XML;
    ctAPPLICATION_ECMASCRIPT = REST.Types.ctAPPLICATION_ECMASCRIPT;
    ctAPPLICATION_EDI_X12 = REST.Types.ctAPPLICATION_EDI_X12;
    ctAPPLICATION_EDIFACT = REST.Types.ctAPPLICATION_EDIFACT;
    ctAPPLICATION_JSON = REST.Types.ctAPPLICATION_JSON;
    ctAPPLICATION_JAVASCRIPT = REST.Types.ctAPPLICATION_JAVASCRIPT;
    ctAPPLICATION_OCTET_STREAM = REST.Types.ctAPPLICATION_OCTET_STREAM;
    ctAPPLICATION_OGG = REST.Types.ctAPPLICATION_OGG;
    ctAPPLICATION_PDF = REST.Types.ctAPPLICATION_PDF;
    ctAPPLICATION_POSTSCRIPT = REST.Types.ctAPPLICATION_POSTSCRIPT;
    ctAPPLICATION_RDF_XML = REST.Types.ctAPPLICATION_RDF_XML;
    ctAPPLICATION_RSS_XML = REST.Types.ctAPPLICATION_RSS_XML;
    ctAPPLICATION_SOAP_XML = REST.Types.ctAPPLICATION_SOAP_XML;
    ctAPPLICATION_FONT_WOFF = REST.Types.ctAPPLICATION_FONT_WOFF;
    ctAPPLICATION_XHTML_XML = REST.Types.ctAPPLICATION_XHTML_XML;
    ctAPPLICATION_XML = REST.Types.ctAPPLICATION_XML;
    ctAPPLICATION_XML_DTD = REST.Types.ctAPPLICATION_XML_DTD;
    ctAPPLICATION_XOP_XML = REST.Types.ctAPPLICATION_XOP_XML;
    ctAPPLICATION_ZIP = REST.Types.ctAPPLICATION_ZIP;
    ctAPPLICATION_GZIP = REST.Types.ctAPPLICATION_GZIP;
    // Type Text
    ctTEXT_CMD = REST.Types.ctTEXT_CMD;
    ctTEXT_CSS = REST.Types.ctTEXT_CSS;
    ctTEXT_CSV = REST.Types.ctTEXT_CSV;
    ctTEXT_HTML = REST.Types.ctTEXT_HTML;
    ctTEXT_JAVASCRIPT = REST.Types.ctTEXT_JAVASCRIPT;
    ctTEXT_PLAIN = REST.Types.ctTEXT_PLAIN;
    ctTEXT_VCARD = REST.Types.ctTEXT_VCARD;
    ctTEXT_XML = REST.Types.ctTEXT_XML;
    // Type Audio
    ctAUDIO_BASIC = REST.Types.ctAUDIO_BASIC;
    ctAUDIO_L24 = REST.Types.ctAUDIO_L24;
    ctAUDIO_MP4 = REST.Types.ctAUDIO_MP4;
    ctAUDIO_MPEG = REST.Types.ctAUDIO_MPEG;
    ctAUDIO_OGG = REST.Types.ctAUDIO_OGG;
    ctAUDIO_VORBIS = REST.Types.ctAUDIO_VORBIS;
    ctAUDIO_VND_RN_REALAUDIO = REST.Types.ctAUDIO_VND_RN_REALAUDIO;
    ctAUDIO_VND_WAVE = REST.Types.ctAUDIO_VND_WAVE;
    ctAUDIO_WEBM = REST.Types.ctAUDIO_WEBM;
    // Type Image
    ctIMAGE_GIF = REST.Types.ctIMAGE_GIF;
    ctIMAGE_JPEG = REST.Types.ctIMAGE_JPEG;
    ctIMAGE_PJPEG = REST.Types.ctIMAGE_PJPEG;
    ctIMAGE_PNG = REST.Types.ctIMAGE_PNG;
    ctIMAGE_SVG_XML = REST.Types.ctIMAGE_SVG_XML;
    ctIMAGE_TIFF = REST.Types.ctIMAGE_TIFF;
    // Type Message
    ctMESSAGE_HTTP = REST.Types.ctMESSAGE_HTTP;
    ctMESSAGE_IMDN_XML = REST.Types.ctMESSAGE_IMDN_XML;
    ctMESSAGE_PARTIAL = REST.Types.ctMESSAGE_PARTIAL;
    ctMESSAGE_RFC822 = REST.Types.ctMESSAGE_RFC822;
    // Type Model (3D Models)
    ctMODEL_EXAMPLE = REST.Types.ctMODEL_EXAMPLE;
    ctMODEL_IGES = REST.Types.ctMODEL_IGES;
    ctMODEL_MESH = REST.Types.ctMODEL_MESH;
    ctMODEL_VRML = REST.Types.ctMODEL_VRML;
    ctMODEL_X3D_BINARY = REST.Types.ctMODEL_X3D_BINARY;
    ctMODEL_X3D_VRML = REST.Types.ctMODEL_X3D_VRML;
    ctMODEL_X3D_XML = REST.Types.ctMODEL_X3D_XML;
    // Type Multipart
    ctMULTIPART_MIXED = REST.Types.ctMULTIPART_MIXED;
    ctMULTIPART_ALTERNATIVE = REST.Types.ctMULTIPART_ALTERNATIVE;
    ctMULTIPART_RELATED = REST.Types.ctMULTIPART_RELATED;
    ctMULTIPART_FORM_DATA = REST.Types.ctMULTIPART_FORM_DATA;
    ctMULTIPART_SIGNED = REST.Types.ctMULTIPART_SIGNED;
    ctMULTIPART_ENCRYPTED = REST.Types.ctMULTIPART_ENCRYPTED;
    // Type Video
    ctVIDEO_MPEG = REST.Types.ctVIDEO_MPEG;
    ctVIDEO_MP4 = REST.Types.ctVIDEO_MP4;
    ctVIDEO_OGG = REST.Types.ctVIDEO_OGG;
    ctVIDEO_QUICKTIME = REST.Types.ctVIDEO_QUICKTIME;
    ctVIDEO_WEBM = REST.Types.ctVIDEO_WEBM;
    ctVIDEO_X_MATROSKA = REST.Types.ctVIDEO_X_MATROSKA;
    ctVIDEO_X_MS_WMV = REST.Types.ctVIDEO_X_MS_WMV;
    ctVIDEO_X_FLV = REST.Types.ctVIDEO_X_FLV;
    // Type Application - Vendor Specific
    ctAPPLICATION_VND_OASIS_OPENDOCUMENT_TEXT = REST.Types.ctAPPLICATION_VND_OASIS_OPENDOCUMENT_TEXT;
    ctAPPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET = REST.Types.ctAPPLICATION_VND_OASIS_OPENDOCUMENT_SPREADSHEET;
    // do not localize
    ctAPPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION = REST.Types.ctAPPLICATION_VND_OASIS_OPENDOCUMENT_PRESENTATION;
    // do not localize
    ctAPPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS = REST.Types.ctAPPLICATION_VND_OASIS_OPENDOCUMENT_GRAPHICS;
    // do not localize
    ctAPPLICATION_VND_MS_EXCEL = REST.Types.ctAPPLICATION_VND_MS_EXCEL;
    ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET = REST.Types.ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_SPREADSHEETML_SHEET;
    ctAPPLICATION_VND_MS_POWERPOINT = REST.Types.ctAPPLICATION_VND_MS_POWERPOINT;
    ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION = REST.Types.ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_PRESENTATIONML_PRESENTATION;
    ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT = REST.Types.ctAPPLICATION_VND_OPENXMLFORMATS_OFFICEDOCUMENT_WORDPROCESSINGML_DOCUMENT;
    ctAPPLICATION_VND_MOZILLA_XUL_XML = REST.Types.ctAPPLICATION_VND_MOZILLA_XUL_XML;
    ctAPPLICATION_VND_GOOGLE_EARTH_KML_XML = REST.Types.ctAPPLICATION_VND_GOOGLE_EARTH_KML_XML;
    ctAPPLICATION_VND_GOOGLE_EARTH_KMZ = REST.Types.ctAPPLICATION_VND_GOOGLE_EARTH_KMZ;
    ctAPPLICATION_VND_DART = REST.Types.ctAPPLICATION_VND_DART;
    ctAPPLICATION_VND_ANDROID_PACKAGE_ARCHIVE = REST.Types.ctAPPLICATION_VND_ANDROID_PACKAGE_ARCHIVE;
    // Type X (RFC 6648)
    ctAPPLICATION_X_DEB = REST.Types.ctAPPLICATION_X_DEB;
    ctAPPLICATION_X_DVI = REST.Types.ctAPPLICATION_X_DVI;
    ctAPPLICATION_X_FONT_TTF = REST.Types.ctAPPLICATION_X_FONT_TTF;
    ctAPPLICATION_X_JAVASCRIPT = REST.Types.ctAPPLICATION_X_JAVASCRIPT;
    ctAPPLICATION_X_LATEX = REST.Types.ctAPPLICATION_X_LATEX;
    ctAPPLICATION_X_MPEGURL = REST.Types.ctAPPLICATION_X_MPEGURL;
    ctAPPLICATION_X_RAR_COMPRESSED = REST.Types.ctAPPLICATION_X_RAR_COMPRESSED;
    ctAPPLICATION_X_SHOCKWAVE_FLASH = REST.Types.ctAPPLICATION_X_SHOCKWAVE_FLASH;
    ctAPPLICATION_X_STUFFIT = REST.Types.ctAPPLICATION_X_STUFFIT;
    ctAPPLICATION_X_TAR = REST.Types.ctAPPLICATION_X_TAR;
    ctAPPLICATION_X_WWW_FORM_URLENCODED = REST.Types.ctAPPLICATION_X_WWW_FORM_URLENCODED;
    ctAPPLICATION_X_XPINSTALL = REST.Types.ctAPPLICATION_X_XPINSTALL;
    ctAUDIO_X_AAC = REST.Types.ctAUDIO_X_AAC;
    ctAUDIO_X_CAF = REST.Types.ctAUDIO_X_CAF;
    ctIMAGE_X_XCF = REST.Types.ctIMAGE_X_XCF;
    ctTEXT_X_GWT_RPC = REST.Types.ctTEXT_X_GWT_RPC;
    ctTEXT_X_JQUERY_TMPL = REST.Types.ctTEXT_X_JQUERY_TMPL;
    ctTEXT_X_MARKDOWN = REST.Types.ctTEXT_X_MARKDOWN;
    // Type PKCS (Cryptography)
    ctAPPLICATION_X_PKCS12 = REST.Types.ctAPPLICATION_X_PKCS12;
    ctAPPLICATION_X_PKCS7_CERTIFICATES = REST.Types.ctAPPLICATION_X_PKCS7_CERTIFICATES;
    ctAPPLICATION_X_PKCS7_CERTREQRESP = REST.Types.ctAPPLICATION_X_PKCS7_CERTREQRESP;
    ctAPPLICATION_X_PKCS7_MIME = REST.Types.ctAPPLICATION_X_PKCS7_MIME;
    ctAPPLICATION_X_PKCS7_SIGNATURE = REST.Types.ctAPPLICATION_X_PKCS7_SIGNATURE;
    // Type Application - Embarcadero Specific
    ctAPPLICATION_VND_EMBARCADERO_FIREDAC_JSON = REST.Types.ctAPPLICATION_VND_EMBARCADERO_FIREDAC_JSON;
  end;

var
  DefaultRESTContentType: TRESTContentType = TRESTContentType.ctNone;

function ContentTypeToString(const AContentType: TRESTContentType): string;
function ContentTypeFromString(const AContentType: string): TRESTContentType;
function IsTextualContentType(const AContentType: TRESTContentType): Boolean;
function IsContentType(const AContentType: TRESTContentType; ATypes: array of TRESTContentType): Boolean;

type
  /// <summary>
  /// Structure that holds performance related information that is gathered while executing a request.<br />All
  /// values are in milliseconds.
  /// </summary>
  TExecutionPerformance = record
    FStartTime: Cardinal;
    procedure Start;
    procedure Clear;
    procedure PreProcessingDone;
    procedure ExecutionDone;
    procedure PostProcessingDone;
  public
    /// <summary>
    /// Time from starting the execution until the request is handed over to the the actual http client and sent to
    /// the server. This includes time which is needed to prepare parameter, encode a body and other things.
    /// </summary>
    PreProcessingTime: integer;

    /// <summary>
    /// The time that the request took to be sent to the server and until the response has been received. This does
    /// <i>not</i> include any JSON parsing etc.
    /// </summary>
    ExecutionTime: integer;

    /// <summary>
    /// The time from when the server response has been received until all post processing (including JSON parsing).
    /// Events, observer notification or completion handlers are <i>not</i> taken into account here.
    /// </summary>
    PostProcessingTime: integer;

    /// <summary>
    /// The total execution time, which is the sum of PreProcessingTime, ExecutionTime and PostProcessingTime
    /// </summary>
    function TotalExecutionTime: integer;
  end;

  /// <summary>
  /// Designates standard HTTP/REST Methods. All methods may affect single or
  /// multiple objects/entities.
  /// </summary>
  /// <remarks>
  /// See http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
  /// </remarks>
  TRESTRequestMethod = (
    /// <summary>
    /// Sends a NEW object/entity to the server.
    /// </summary>
    rmPOST,

    /// <summary>
    /// Updates an already existing object/entity on the server. PUT may also
    /// allow for sending a new entity (depends on the actual server/API
    /// implementation).
    /// </summary>
    rmPUT,

    /// <summary>
    /// Retrieves an object/entity from the server.
    /// </summary>
    rmGET,

    /// <summary>
    /// Deletes an object/entity from the server.
    /// </summary>
    rmDELETE,
      /// <summary>
    /// Patches an object/entity on the server, by only updating the pairs that are sent within that PATCH body.
    /// </summary>
    rmPATCH
    );

  /// <summary>
  /// Exceptions coming from the REST Library have this common ancestor
  /// </summary>
  ERESTException = class(Exception)
  end;

  ERequestError = class(ERESTException)
  private
    FResponseContent: string;
    FStatusCode: Integer;
    FStatusText: string;
  public
    constructor Create(AStatusCode: Integer; const AStatusText: string; const AResponseContent: string); reintroduce;
    property ResponseContent: string read FResponseContent write FResponseContent;
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property StatusText: string read FStatusText write FStatusText;
  end;

var
  DefaultRESTRequestMethod: TRESTRequestMethod = TRESTRequestMethod.rmGET;

function RESTRequestMethodToString(const AMethod: TRESTRequestMethod): string;

implementation

uses
  System.Net.Mime, REST.Consts;

function IsTextualContentType(const AContentType: TRESTContentType): Boolean;
var
  LExt: string;
  LKind: TMimeTypes.TKind;
begin
  TMimeTypes.Default.GetTypeInfo(AContentType, LExt, LKind);
  Result := LKind = TMimeTypes.TKind.Text;
end;

function IsContentType(const AContentType: TRESTContentType; ATypes: array of TRESTContentType): Boolean;
var
  I: Integer;
begin
  for I := Low(ATypes) to High(ATypes) do
    if SameText(AContentType, ATypes[I]) then
      Exit(True);
  Result := False;
end;

function RESTRequestParameterKindToString(const AKind: TRESTRequestParameterKind): string;
begin
  case AKind of
    TRESTRequestParameterKind.pkCOOKIE:
      result := 'COOKIE';
    TRESTRequestParameterKind.pkGETorPOST:
      result := 'GET/POST';
    TRESTRequestParameterKind.pkURLSEGMENT:
      result := 'URL-SEGMENT';
    TRESTRequestParameterKind.pkHTTPHEADER:
      result := 'HEADER';
    TRESTRequestParameterKind.pkREQUESTBODY:
      result := 'BODY';
    TRESTRequestParameterKind.pkFILE:
      result := 'FILE';
    TRESTRequestParameterKind.pkQUERY:
      result := 'QUERY';
  else
    result := Format('RESTRequestParameterKindToString - unknown Kind: %d', [integer(AKind)]);
  end;
end;

function RESTRequestParameterKindFromString(const AKindString: string): TRESTRequestParameterKind;
var
  LKind: TRESTRequestParameterKind;
begin
  result := DefaultRESTRequestParameterKind;

  for LKind in [low(TRESTRequestParameterKind) .. high(TRESTRequestParameterKind)] do
    if (SameText(AKindString, RESTRequestParameterKindToString(LKind))) then
    begin
      result := LKind;
      BREAK;
    end;
end;

function RESTRequestMethodToString(const AMethod: TRESTRequestMethod): string;
begin
  case AMethod of
    TRESTRequestMethod.rmPOST:
      result := 'POST';
    TRESTRequestMethod.rmPUT:
      result := 'PUT';
    TRESTRequestMethod.rmGET:
      result := 'GET';
    TRESTRequestMethod.rmDELETE:
      result := 'DELETE';
    TRESTRequestMethod.rmPATCH:
      result := 'PATCH'
  else
    result := Format('RESTRequestMethod2String - unknown Method: %d', [integer(AMethod)]);
  end;
end;

function ContentTypeFromString(const AContentType: string): TRESTContentType;
begin
  Result := AContentType.Trim.ToLower;
end;

function ContentTypeToString(const AContentType: TRESTContentType): string;
begin
  Result := string(AContentType).Trim.ToLower;
end;

{ TExecutionPerformance }

procedure TExecutionPerformance.Clear;
begin
  PreProcessingTime := -1;
  ExecutionTime := -1;
  PostProcessingTime := -1;
end;

procedure TExecutionPerformance.ExecutionDone;
begin
  // if Post or Pre are never set, they are at -1
  ExecutionTime := TThread.GetTickCount - FStartTime - Cardinal(max(PreProcessingTime, 0));
  // Cardinal typecast to avoid compiler warnings

end;

procedure TExecutionPerformance.PostProcessingDone;
begin
  PostProcessingTime := TThread.GetTickCount - FStartTime - Cardinal(max(ExecutionTime, 0)) -
    Cardinal(max(PreProcessingTime, 0)); // Cardinal typecast to avoid compiler warnings
end;

procedure TExecutionPerformance.PreProcessingDone;
begin
  PreProcessingTime := TThread.GetTickCount - FStartTime;
end;

procedure TExecutionPerformance.Start;
begin
  Clear;
  FStartTime := TThread.GetTickCount;
end;

function TExecutionPerformance.TotalExecutionTime: integer;
begin
  result := max(PreProcessingTime, 0) + max(ExecutionTime, 0) + max(PostProcessingTime, 0);
end;

{ ERequestError }

constructor ERequestError.Create(AStatusCode: Integer; const
    AStatusText: string; const AResponseContent: string);
begin
  if AResponseContent <> '' then
    inherited Create(AResponseContent)
  else
    inherited Create(AStatusText);

  FResponseContent := AResponseContent;
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
end;

end.
