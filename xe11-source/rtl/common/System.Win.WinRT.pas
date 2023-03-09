{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

/// <summary>Classes and Helper utililities for dealing with WinRT interfaces and strings</summary>
unit System.Win.WinRT;

interface

uses
  Winapi.WinRT, System.RTTI, System.SysUtils, System.TypInfo;

type
  /// <summary>Exception base class for all WinRT exceptions </summary>
  EWinRTException = class(Exception);

  /// <summary>Base Delphi object needed for Windows Runtime classes. Implements IInspectable interface</summary>
  TInspectableObject = class(TInterfacedObject, IInspectable)
  protected
    FIIDS: array of TGUID;
  public
    /// <summary>Gets the interfaces that are implemented by the current Windows Runtime class</summary>
    function GetIids(out iidCount: Cardinal; out iids: PGUID): HRESULT; stdcall;
    /// <summary>Gets the fully qualified name of the current Windows Runtime object</summary>
    function GetRuntimeClassName(out className: HSTRING): HRESULT; stdcall;
    /// <summary>Gets the trust level of the current Windows Runtime object</summary>
    function GetTrustLevel(out trust: TrustLevel): HRESULT; stdcall;
  end;

  /// <summary>Helper needed to automate life cycle of HSTRING type and conversions from/to string</summary>
  TWindowsString = record
  strict private
  type
    TWindowsStringNexus = class(TInterfacedObject)
    private
      FString: HSTRING;
    public
      constructor Create(AString: HSTRING);
      destructor Destroy; override;
    end;

    PWindowsString = ^TWindowsString;
  private
    FNexus: IInterface;
    FHandle: HSTRING;
    FValid: Boolean;
  public
    constructor Create(const S: string); overload;
    /// <summary>Checks that the internal HSTRING was created successfully</summary>
    property Valid: Boolean read FValid;

    class operator Implicit(const S: TWindowsString): HSTRING; inline;
    class operator Explicit(const S: string): TWindowsString;
    /// <summary>Gets the string representation of the given HSTRING</summary>
    class function HStringToString(const hs: HSTRING): string; static;
  end;

  /// <summary>Record helper for HSTRING</summary>
  THStringHelper = record helper for HSTRING
    /// <summary>Gets the string representation of the given HSTRING</summary>
    function ToString: string; inline;
  end;

  /// <summary>Custom atribute to store the Class Name of an imported WinRT class</summary>
  WinRTClassNameAttribute = class(TCustomAttribute)
  private
    FSignature: string;
  public
    constructor Create(const S: string);
    /// <summary>Gets the signature of the Class</summary>
    property Signature: string read FSignature;
  end;

{$M+}

  /// <summary>Helper used to import a WinRT class</summary>
  TWinRTImportHelper = class
  private
    class var FContext: TRttiContext;
    class constructor Create;
    class destructor Destroy;
  public
    /// <summary>Gets the UIID of a given interface</summary>
    class function GetUIID<T: IInterface>: TGUID; static;
    /// <summary>Gets the WinRT ActivationFactory related to the given TypeInfo. Also gets it's Class Name</summary>
    class function GetFactoryOrStatics(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable;
    /// <summary>Gets the WinRT ActivateInstance related to the given TypeInfo. Also gets it's Class Name</summary>
    class function CreateInstance(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable;
    /// <summary>Gets the Class Name of a Class Type</summary>
    class function GetClassName(const ClassType: TRttiType): HSTRING;
    /// <summary>Cached RTTI Context to share among all WinRTImport</summary>
    class property Context: TRttiContext read FContext;
  end;

  /// <summary>Base class of all WinRT Imports</summary>
  TWinRTImport = class
  protected
    /// <summary>Inline function to use ImporHelper sibling</summary>
    class function GetFactoryOrStatics(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable; inline;
    /// <summary>Inline function to use ImporHelper sibling</summary>
    class function CreateInstance(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable; inline;
  end;

  TWinRTGenericImportF<F: IInspectable> = class(TWinRTImport)
  private
    class var FRTClassName: HSTRING;
    class var FFactory: F;
    class function GetFactory: F; static;
    class destructor Destroy;
  public
    class property Factory: F read GetFactory;
    class property RTClassName: HSTRING read FRTClassName;
  end;

  TWinRTGenericImportS<S: IInspectable> = class(TWinRTImport)
  private
    class var FRTClassName: HSTRING;
    class var FStatics: S;
    class function GetStatics: S; static;
    class destructor Destroy;
  public
    class property RTClassName: HSTRING read FRTClassName;
    class property Statics: S read GetStatics;
  end;

  TWinRTGenericImportI<I: IInspectable> = class(TWinRTImport)
  private
    class var FRTClassName: HSTRING;
    class destructor Destroy;
  public
    class function Create: I; static;
    class property RTClassName: HSTRING read FRTClassName;
  end;

  TWinRTGenericImportF2<F1, F2: IInspectable> = class(TWinRTGenericImportF<F1>)
  private
    class var FFactory2: F2;
    class function GetFactory2: F2; static;
  public
    class property Factory2: F2 read GetFactory2;
  end;

  TWinRTGenericImportF2S<F1, F2, S: IInspectable> = class(TWinRTGenericImportF2<F1, F2>)
  private
    class var FStatics: S;
    class function GetStatics: S; static;
  public
    class property Statics: S read GetStatics;
  end;

  TWinRTGenericImportFS<F, S: IInspectable> = class(TWinRTGenericImportF<F>)
  private
    class var FStatics: S;
    class function GetStatics: S; static;
  public
    class property Statics: S read GetStatics;
  end;

  TWinRTGenericImportFI<F: IInspectable; I: IInspectable> = class(TWinRTGenericImportF<F>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS2<S1, S2: IInspectable> = class(TWinRTGenericImportS<S1>)
  private
    class var FStatics2: S2;
    class function GetStatics2: S2; static;
  public
    class property Statics2: S2 read GetStatics2;
  end;

  TWinRTGenericImportS3<S1, S2, S3: IInspectable> = class(TWinRTGenericImportS2<S1, S2>)
  private
    class var FStatics3: S3;
    class function GetStatics3: S3; static;
  public
    class property Statics3: S3 read GetStatics3;
  end;

  TWinRTGenericImportS4<S1, S2, S3, S4: IInspectable> = class(TWinRTGenericImportS3<S1, S2, S3>)
  private
    class var FStatics4: S4;
    class function GetStatics4: S4; static;
  public
    class property Statics4: S4 read GetStatics4;
  end;

  TWinRTGenericImportS5<S1, S2, S3, S4, S5: IInspectable> = class(TWinRTGenericImportS4<S1, S2, S3, S4>)
  private
    class var FStatics5: S5;
    class function GetStatics5: S5; static;
  public
    class property Statics5: S5 read GetStatics5;
  end;

  TWinRTGenericImportS6<S1, S2, S3, S4, S5, S6: IInspectable> = class(TWinRTGenericImportS5<S1, S2, S3, S4, S5>)
  private
    class var FStatics6: S6;
    class function GetStatics6: S6; static;
  public
    class property Statics6: S6 read GetStatics6;
  end;

  TWinRTGenericImportS7<S1, S2, S3, S4, S5, S6, S7: IInspectable> = class(TWinRTGenericImportS6<S1, S2, S3, S4, S5, S6>)
  private
    class var FStatics7: S7;
    class function GetStatics7: S7; static;
  public
    class property Statics7: S7 read GetStatics7;
  end;

  TWinRTGenericImportS8<S1, S2, S3, S4, S5, S6, S7, S8: IInspectable> = class(TWinRTGenericImportS7<S1, S2, S3, S4, S5, S6, S7>)
  private
    class var FStatics8: S8;
    class function GetStatics8: S8; static;
  public
    class property Statics8: S8 read GetStatics8;
  end;

  TWinRTGenericImportSI<S: IInspectable; I: IInspectable> = class(TWinRTGenericImportS<S>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS2I<S1, S2, I: IInspectable> = class(TWinRTGenericImportS2<S1, S2>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS3I<S1, S2, S3, I: IInspectable> = class(TWinRTGenericImportS3<S1, S2, S3>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS4I<S1, S2, S3, S4, I: IInspectable> = class(TWinRTGenericImportS4<S1, S2, S3, S4>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS5I<S1, S2, S3, S4, S5, I: IInspectable> = class(TWinRTGenericImportS5<S1, S2, S3, S4, S5>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS6I<S1, S2, S3, S4, S5, S6, I: IInspectable> = class(TWinRTGenericImportS6<S1, S2, S3, S4, S5, S6>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS7I<S1, S2, S3, S4, S5, S6, S7, I: IInspectable> = class(TWinRTGenericImportS7<S1, S2, S3, S4, S5, S6, S7>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportF2I<F1, F2, I: IInspectable> = class(TWinRTGenericImportF2<F1, F2>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS2<F, S1, S2: IInspectable> = class(TWinRTGenericImportFS<F, S1>)
  private
    class var FStatics2: S2;
    class function GetStatics2: S2; static;
  public
    class property Statics2: S2 read GetStatics2;
  end;

  TWinRTGenericImportFS3<F, S1, S2, S3: IInspectable> = class(TWinRTGenericImportFS2<F, S1, S2>)
  private
    class var FStatics3: S3;
    class function GetStatics3: S3; static;
  public
    class property Statics3: S3 read GetStatics3;
  end;

  TWinRTGenericImportFS4<F, S1, S2, S3, S4: IInspectable> = class(TWinRTGenericImportFS3<F, S1, S2, S3>)
  private
    class var FStatics4: S4;
    class function GetStatics4: S4; static;
  public
    class property Statics4: S4 read GetStatics4;
  end;

  TWinRTGenericImportFS5<F, S1, S2, S3, S4, S5: IInspectable> = class(TWinRTGenericImportFS4<F, S1, S2, S3, S4>)
  private
    class var FStatics5: S5;
    class function GetStatics5: S5; static;
  public
    class property Statics5: S5 read GetStatics5;
  end;

  TWinRTGenericImportFS6<F, S1, S2, S3, S4, S5, S6: IInspectable> = class(TWinRTGenericImportFS5<F, S1, S2, S3, S4, S5>)
  private
    class var FStatics6: S6;
    class function GetStatics6: S6; static;
  public
    class property Statics6: S6 read GetStatics6;
  end;

  TWinRTGenericImportFS7<F, S1, S2, S3, S4, S5, S6, S7: IInspectable> = class(TWinRTGenericImportFS6<F, S1, S2, S3, S4, S5, S6>)
  private
    class var FStatics7: S7;
    class function GetStatics7: S7; static;
  public
    class property Statics7: S7 read GetStatics7;
  end;

  TWinRTGenericImportFS8<F, S1, S2, S3, S4, S5, S6, S7, S8: IInspectable> = class(TWinRTGenericImportFS7<F, S1, S2, S3, S4, S5, S6, S7>)
  private
    class var FStatics8: S8;
    class function GetStatics8: S8; static;
  public
    class property Statics8: S8 read GetStatics8;
  end;

  TWinRTGenericImportFS9<F, S1, S2, S3, S4, S5, S6, S7, S8, S9: IInspectable> = class(TWinRTGenericImportFS8<F, S1, S2, S3, S4, S5, S6, S7, S8>)
  private
    class var FStatics9: S9;
    class function GetStatics9: S9; static;
  public
    class property Statics9: S9 read GetStatics9;
  end;

  TWinRTGenericImportFS10<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10: IInspectable> = class(TWinRTGenericImportFS9<F, S1, S2, S3, S4, S5, S6, S7, S8, S9>)
  private
    class var FStatics10: S10;
    class function GetStatics10: S10; static;
  public
    class property Statics10: S10 read GetStatics10;
  end;

  TWinRTGenericImportFSI<F, S, I: IInspectable> = class(TWinRTGenericImportFS<F, S>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS2I<F, S1, S2, I: IInspectable> = class(TWinRTGenericImportFS2<F, S1, S2>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS3I<F, S1, S2, S3, I: IInspectable> = class(TWinRTGenericImportFS3<F, S1, S2, S3>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS4I<F, S1, S2, S3, S4, I: IInspectable> = class(TWinRTGenericImportFS4<F, S1, S2, S3, S4>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS5I<F, S1, S2, S3, S4, S5, I: IInspectable> = class(TWinRTGenericImportFS5<F, S1, S2, S3, S4, S5>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS6I<F, S1, S2, S3, S4, S5, S6, I: IInspectable> = class(TWinRTGenericImportFS6<F, S1, S2, S3, S4, S5, S6>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS7I<F, S1, S2, S3, S4, S5, S6, S7, I: IInspectable> = class(TWinRTGenericImportFS7<F, S1, S2, S3, S4, S5, S6, S7>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS8I<F, S1, S2, S3, S4, S5, S6, S7, S8, I: IInspectable> = class(TWinRTGenericImportFS8<F, S1, S2, S3, S4, S5, S6, S7, S8>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS9I<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, I: IInspectable> = class(TWinRTGenericImportFS9<F, S1, S2, S3, S4, S5, S6, S7, S8, S9>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportFS10I<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, I: IInspectable> = class(TWinRTGenericImportFS10<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10>)
  public
    class function Create: I; static;
  end;

  TWinRTGenericImportS3O<S1, S2, S3: IInspectable; O: IUnknown> = class(TWinRTGenericImportS3<S1, S2, S3>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

  TWinRTGenericImportS4O<S1, S2, S3, S4: IInspectable; O: IUnknown> = class(TWinRTGenericImportS4<S1, S2, S3, S4>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

  TWinRTGenericImportSO<S: IInspectable; O: IUnknown> = class(TWinRTGenericImportS<S>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

  TWinRTGenericImportS2O<S1, S2: IInspectable; O: IUnknown> = class(TWinRTGenericImportS2<S1, S2>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

  TWinRTGenericImportSIO<S: IInspectable; I: IInspectable; O: IUnknown> = class(TWinRTGenericImportSI<S, I>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

  TWinRTGenericImportFIO<F, I: IInspectable; O: IUnknown> = class(TWinRTGenericImportFI<F, I>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

  TWinRTGenericImportFSIO<F, S, I: IInspectable; O: IUnknown> = class(TWinRTGenericImportFSI<F, S, I>)
  private
    class var FInterop: O;
    class function GetInterop: O; static;
  public
    class property Interop: O read GetInterop;
  end;

{$M-}

var
  RoInitType: RO_INIT_TYPE = RO_INIT_MULTITHREADED;

implementation

uses
  System.RTLConsts;

var
  SaveInitProc: Pointer;
  InitWinRTCalled: Boolean = False;
  NeedToUninitialize: Boolean = False;

function Succeeded(Res: HRESULT): Boolean;
begin
  Result := Res and $80000000 = 0;
end;

procedure InitWinRT;
begin
  if InitWinRTCalled then
    Exit;
  if SaveInitProc <> nil then
    TProcedure(SaveInitProc);
  if TOSVersion.Check(6, 2) then
    NeedToUninitialize := Succeeded(RoInitialize(RoInitType));
  InitWinRTCalled := True;
end;

{ TInspectableObject }
function TInspectableObject.GetIids(out iidCount: Cardinal;
  out iids: PGUID): HRESULT;
var
  Cxt: TRttiContext;
  Typ: TRttiType;
  IntfTable: PInterfaceTable;
begin
  if Length(FIIDS) = 0 then
  begin
    Cxt := TRttiContext.Create;
    try
      Typ := Cxt.GetType(Self.ClassType);
      IntfTable := Typ.GetInterfaceTable;
      SetLength(FIIDS, IntfTable^.EntryCount - 2);

      Move(IntfTable^.Entries[0], FIIDS[0], IntfTable^.EntryCount);
    finally
      Cxt.Free;
    end;
  end;
  iidCount := Length(FIIDS);
  if Length(FIIDS) > 0 then
    iids := @FIIDS[0]
  else
    iids := nil;
  Result := S_OK;
end;

function TInspectableObject.GetRuntimeClassName(out className: HSTRING): HRESULT;
var
  Str: string;
begin
  Str := Self.className;
  Result := WindowsCreateString(PChar(Str), Length(Str), className);
end;

function TInspectableObject.GetTrustLevel(out trust: TrustLevel): HRESULT;
begin
  trust := TrustLevel.BaseTrust;                                       
  Result := S_OK;
end;


{ TWindowsString.TWindowsStringNexus }

constructor TWindowsString.TWindowsStringNexus.Create(AString: HSTRING);
begin
  inherited Create;
  FString := AString;
end;

destructor TWindowsString.TWindowsStringNexus.Destroy;
begin
  WindowsDeleteString(FString);
  inherited Destroy;
end;

{ TWindowsString }

constructor TWindowsString.Create(const S: string);
begin
  FValid := Succeeded(WindowsCreateString(PChar(S), System.Length(S), FHandle));
  FNexus := TWindowsStringNexus.Create(FHandle);
end;

class operator TWindowsString.Explicit(const S: string): TWindowsString;
begin
  Result := TWindowsString.Create(S);
end;

class operator TWindowsString.Implicit(const S: TWindowsString): HSTRING;
begin
  Result := S.FHandle;
end;

class function TWindowsString.HStringToString(const hs: HSTRING): string;
begin
  Result := WindowsGetStringRawBuffer(hs, nil);
end;

{ THStringHelper }

function THStringHelper.ToString: string;
begin
  Result := TWindowsString.HStringToString(Self);
end;

{ WinRTClassNameAttribute }

constructor WinRTClassNameAttribute.Create(const S: string);
begin
  FSignature := S;
end;

{ TWinRTImportHelper }

class constructor TWinRTImportHelper.Create;
begin
  FContext := TRttiContext.Create;
end;

class destructor TWinRTImportHelper.Destroy;
begin
  FContext.Free;
end;

class function TWinRTImportHelper.CreateInstance(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable;
var
  LType: TRttiType;
  LTypeData: PTypeData;
  LCreated: IInspectable;
  LGUID: TGUID;
begin
  LType := TWinRTImportHelper.Context.GetType(ATypeInfo);
  LTypeData := GetTypeData(ATypeInfo);
  if LType = nil then
    raise EWinRTException.CreateFmt(SWinRTNoRTTI, [TWindowsString.HStringToString(AClassName)]);
  LGUID := LTypeData^.Guid;

  if AClassName <= 0 then
    AClassName := TWinRTImportHelper.GetClassName(LType);

  if not Succeeded(RoActivateInstance(AClassName, LCreated)) then
    raise EWinRTException.CreateFmt(SWinRTInstanceError, [TWindowsString.HStringToString(AClassName)]);

  if LCreated.QueryInterface(LGUID, Result) <> 0 then
    raise EWinRTException.CreateFmt(SWinRTICreatedError, [TWindowsString.HStringToString(AClassName)]);
end;

class function TWinRTImportHelper.GetFactoryOrStatics(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable;
var
  LType: TRttiType;
  LTypeData: PTypeData;
  LCreated: IInspectable;
  LGUID: TGUID;
begin
  LType := TWinRTImportHelper.Context.GetType(ATypeInfo);
  LTypeData := GetTypeData(ATypeInfo);
  if LType = nil then
    raise EWinRTException.CreateFmt(SWinRTNoRTTI, [TWindowsString.HStringToString(AClassName)]);
  LGUID := LTypeData^.Guid;

  if AClassName <= 0 then
    AClassName := TWinRTImportHelper.GetClassName(LType);

  if not Succeeded(RoGetActivationFactory(AClassName, LGUID, LCreated)) then
    raise EWinRTException.CreateFmt(SWinRTFactoryError, [TWindowsString.HStringToString(AClassName)]);

  if LCreated.QueryInterface(LGUID, Result) <> 0 then
    raise EWinRTException.CreateFmt(SWinRTWrongFactoryError, [TWindowsString.HStringToString(AClassName)]);
end;

class function TWinRTImportHelper.GetUIID<T>: TGUID;
begin
  Result := GetTypeData(TypeInfo(T))^.Guid;
end;

class function TWinRTImportHelper.GetClassName(const ClassType: TRttiType): HSTRING;
var
  Attrs: TArray<TCustomAttribute>;
  Sig: WinRTClassNameAttribute;
  LStr: string;
begin
  Attrs := ClassType.GetAttributes;
  if Length(Attrs) > 0 then
  begin
    Sig := WinRTClassNameAttribute(Attrs[0]);
    LStr := Sig.Signature;
  end
  else
    LStr := '';

  if not Succeeded(WindowsCreateString(PWideChar(LStr), Length(LStr), Result)) then
    raise EWinRTException.CreateFmt(SWinRTHStringError, [LStr]);
end;

{ TWinRTImport }

class function TWinRTImport.CreateInstance(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable;
begin
  Result := TWinRTImportHelper.CreateInstance(ATypeInfo, AClassName);
end;

class function TWinRTImport.GetFactoryOrStatics(ATypeInfo: PTypeInfo; var AClassName: HSTRING): IInspectable;
begin
  Result := TWinRTImportHelper.GetFactoryOrStatics(ATypeInfo, AClassName);
end;

{ TWinRTGenericImportFI<F,I> }

class function TWinRTGenericImportFI<F, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportF<F> }

class destructor TWinRTGenericImportF<F>.Destroy;
begin
  if TOSVersion.Check(6, 2) then
    WindowsDeleteString(FRTClassName);
end;

class function TWinRTGenericImportF<F>.GetFactory: F;
begin
  if FFactory = nil then
    FFactory := F(GetFactoryOrStatics(TypeInfo(F), FRTClassName));
  Result := FFactory;
end;

{ TWinRTGenericImportI<I> }

class function TWinRTGenericImportI<I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS<T> }

class destructor TWinRTGenericImportS<S>.Destroy;
begin
  if TOSVersion.Check(6, 2) then
    WindowsDeleteString(FRTClassName);
end;

class function TWinRTGenericImportS<S>.GetStatics: S;
begin
  if FStatics = nil then
    FStatics := S(GetFactoryOrStatics(TypeInfo(S), FRTClassName));
  Result := FStatics;
end;

class destructor TWinRTGenericImportI<I>.Destroy;
begin
  if TOSVersion.Check(6, 2) then
    WindowsDeleteString(FRTClassName);
end;

{ TWinRTGenericImportFS<F, S> }

class function TWinRTGenericImportFS<F, S>.GetStatics: S;
begin
  if FStatics = nil then
    FStatics := S(GetFactoryOrStatics(TypeInfo(S), FRTClassName));
  Result := FStatics;
end;

{ TWinRTGenericImportFS2<F, S1, S2> }

class function TWinRTGenericImportFS2<F, S1, S2>.GetStatics2: S2;
begin
  if FStatics2 = nil then
    FStatics2 := S2(GetFactoryOrStatics(TypeInfo(S2), FRTClassName));
  Result := FStatics2;
end;

{ TWinRTGenericImportFS3<F, S1, S2, S3> }

class function TWinRTGenericImportFS3<F, S1, S2, S3>.GetStatics3: S3;
begin
  if FStatics3 = nil then
    FStatics3 := S3(GetFactoryOrStatics(TypeInfo(S3), FRTClassName));
  Result := FStatics3;
end;

{ TWinRTGenericImportFS4<F, S1, S2, S3, S4> }

class function TWinRTGenericImportFS4<F, S1, S2, S3, S4>.GetStatics4: S4;
begin
  if FStatics4 = nil then
    FStatics4 := S4(GetFactoryOrStatics(TypeInfo(S4), FRTClassName));
  Result := FStatics4;
end;

{ TWinRTGenericImportFS5<F, S1, S2, S3, S4, S5> }

class function TWinRTGenericImportFS5<F, S1, S2, S3, S4, S5>.GetStatics5: S5;
begin
  if FStatics5 = nil then
    FStatics5 := S5(GetFactoryOrStatics(TypeInfo(S5), FRTClassName));
  Result := FStatics5;
end;

{ TWinRTGenericImportFS6<F, S1, S2, S3, S4, S5, S6> }

class function TWinRTGenericImportFS6<F, S1, S2, S3, S4, S5, S6>.GetStatics6: S6;
begin
  if FStatics6 = nil then
    FStatics6 := S6(GetFactoryOrStatics(TypeInfo(S6), FRTClassName));
  Result := FStatics6;
end;

{ TWinRTGenericImportFS7<F, S1, S2, S3, S4, S5, S6, S7> }

class function TWinRTGenericImportFS7<F, S1, S2, S3, S4, S5, S6, S7>.GetStatics7: S7;
begin
  if FStatics7 = nil then
    FStatics7 := S7(GetFactoryOrStatics(TypeInfo(S7), FRTClassName));
  Result := FStatics7;
end;

{ TWinRTGenericImportFS8<F, S1, S2, S3, S4, S5, S6, S7, S8> }

class function TWinRTGenericImportFS8<F, S1, S2, S3, S4, S5, S6, S7, S8>.GetStatics8: S8;
begin
  if FStatics8 = nil then
    FStatics8 := S8(GetFactoryOrStatics(TypeInfo(S8), FRTClassName));
  Result := FStatics8;
end;

{ TWinRTGenericImportFS9<F, S1, S2, S3, S4, S5, S6, S7, S8, S9> }

class function TWinRTGenericImportFS9<F, S1, S2, S3, S4, S5, S6, S7, S8, S9>.GetStatics9: S9;
begin
  if FStatics9 = nil then
    FStatics9 := S9(GetFactoryOrStatics(TypeInfo(S9), FRTClassName));
  Result := FStatics9;
end;

{ TWinRTGenericImportFS10<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10> }

class function TWinRTGenericImportFS10<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10>.GetStatics10: S10;
begin
  if FStatics10 = nil then
    FStatics10 := S10(GetFactoryOrStatics(TypeInfo(S10), FRTClassName));
  Result := FStatics10;
end;

{ TWinRTGenericImportSI<S, I> }

class function TWinRTGenericImportSI<S, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS2<S1, S2> }

class function TWinRTGenericImportS2<S1, S2>.GetStatics2: S2;
begin
  if FStatics2 = nil then
    FStatics2 := S2(GetFactoryOrStatics(TypeInfo(S2), FRTClassName));
  Result := FStatics2;
end;

{ TWinRTGenericImportS3<S1, S2, S3> }

class function TWinRTGenericImportS3<S1, S2, S3>.GetStatics3: S3;
begin
  if FStatics3 = nil then
    FStatics3 := S3(GetFactoryOrStatics(TypeInfo(S3), FRTClassName));
  Result := FStatics3;
end;

{ TWinRTGenericImportS4<S1, S2, S3, S4> }

class function TWinRTGenericImportS4<S1, S2, S3, S4>.GetStatics4: S4;
begin
  if FStatics4 = nil then
    FStatics4 := S4(GetFactoryOrStatics(TypeInfo(S4), FRTClassName));
  Result := FStatics4;
end;

{ TWinRTGenericImportS5<S1, S2, S3, S4, S5> }

class function TWinRTGenericImportS5<S1, S2, S3, S4, S5>.GetStatics5: S5;
begin
  if FStatics5 = nil then
    FStatics5 := S5(GetFactoryOrStatics(TypeInfo(S5), FRTClassName));
  Result := FStatics5;
end;

{ TWinRTGenericImportS6<S1, S2, S3, S4, S5, S6> }

class function TWinRTGenericImportS6<S1, S2, S3, S4, S5, S6>.GetStatics6: S6;
begin
  if FStatics6 = nil then
    FStatics6 := S6(GetFactoryOrStatics(TypeInfo(S6), FRTClassName));
  Result := FStatics6;
end;

{ TWinRTGenericImportS7<S1, S2, S3, S4, S5, S6, S7> }

class function TWinRTGenericImportS7<S1, S2, S3, S4, S5, S6, S7>.GetStatics7: S7;
begin
  if FStatics7 = nil then
    FStatics7 := S7(GetFactoryOrStatics(TypeInfo(S7), FRTClassName));
  Result := FStatics7;
end;

{ TWinRTGenericImportS8<S1, S2, S3, S4, S5, S6, S7, S8> }

class function TWinRTGenericImportS8<S1, S2, S3, S4, S5, S6, S7, S8>.GetStatics8: S8;
begin
  if FStatics8 = nil then
    FStatics8 := S8(GetFactoryOrStatics(TypeInfo(S8), FRTClassName));
  Result := FStatics8;
end;

{ TWinRTGenericImportFSI<F, S, I> }

class function TWinRTGenericImportFSI<F, S, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS2I<F, S1, S2, I> }

class function TWinRTGenericImportFS2I<F, S1, S2, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS3I<F, S1, S2, S3, I> }

class function TWinRTGenericImportFS3I<F, S1, S2, S3, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS4I<F, S1, S2, S3, S4, I> }

class function TWinRTGenericImportFS4I<F, S1, S2, S3, S4, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS5I<F, S1, S2, S3, S4, S5, I> }

class function TWinRTGenericImportFS5I<F, S1, S2, S3, S4, S5, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS6I<F, S1, S2, S3, S4, S5, S6, I> }

class function TWinRTGenericImportFS6I<F, S1, S2, S3, S4, S5, S6, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS7I<F, S1, S2, S3, S4, S5, S6, S7, I> }

class function TWinRTGenericImportFS7I<F, S1, S2, S3, S4, S5, S6, S7, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS8I<F, S1, S2, S3, S4, S5, S6, S7, S8, I> }

class function TWinRTGenericImportFS8I<F, S1, S2, S3, S4, S5, S6, S7, S8, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS9I<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, I> }

class function TWinRTGenericImportFS9I<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportFS10I<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, I> }

class function TWinRTGenericImportFS10I<F, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportF2<F1, F2> }

class function TWinRTGenericImportF2<F1, F2>.GetFactory2: F2;
begin
  if FFactory2 = nil then
    FFactory2 := F2(GetFactoryOrStatics(TypeInfo(F2), FRTClassName));
  Result := FFactory2;
end;

{ TWinRTGenericImportF2I<F1, F2, I> }

class function TWinRTGenericImportF2I<F1, F2, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS2I<S1, S2, I> }

class function TWinRTGenericImportS2I<S1, S2, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS3I<S1, S2, S3, I> }

class function TWinRTGenericImportS3I<S1, S2, S3, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS4I<S1, S2, S3, S4, I> }

class function TWinRTGenericImportS4I<S1, S2, S3, S4, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS5I<S1, S2, S3, S4, S5, I> }

class function TWinRTGenericImportS5I<S1, S2, S3, S4, S5, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS6I<S1, S2, S3, S4, S5, S6, I> }

class function TWinRTGenericImportS6I<S1, S2, S3, S4, S5, S6, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportS7I<S1, S2, S3, S4, S5, S6, S7, I> }

class function TWinRTGenericImportS7I<S1, S2, S3, S4, S5, S6, S7, I>.Create: I;
begin
  Result := I(CreateInstance(TypeInfo(I), FRTClassName));
end;

{ TWinRTGenericImportSO<S, O> }

class function TWinRTGenericImportSO<S, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Statics.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

{ TWinRTGenericImportS2O<S1, S2, O> }

class function TWinRTGenericImportS2O<S1, S2, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Statics.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

{ TWinRTGenericImportS3O<S1, S2, S3, O> }

class function TWinRTGenericImportS3O<S1, S2, S3, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Statics.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

{ TWinRTGenericImportS4O<S1, S2, S3, S4, O> }

class function TWinRTGenericImportS4O<S1, S2, S3, S4, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Statics.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

{ TWinRTGenericImportF2S<F1, F2, S> }

class function TWinRTGenericImportF2S<F1, F2, S>.GetStatics: S;
begin
  if FStatics = nil then
    FStatics := S(GetFactoryOrStatics(TypeInfo(S), FRTClassName));
  Result := FStatics;
end;

{ TWinRTGenericImportFIO<F, I, O> }

class function TWinRTGenericImportFIO<F, I, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Factory.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

{ TWinRTGenericImportSIO<S, I, O> }

class function TWinRTGenericImportSIO<S, I, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Statics.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

{ TWinRTGenericImportFSIO<F, S, I, O> }

class function TWinRTGenericImportFSIO<F, S, I, O>.GetInterop: O;
begin
  if FInterop = nil then
    if Statics.QueryInterface(TWinRTImportHelper.GetUIID<O>, FInterop) <> 0 then
      raise EWinRTException.Create(SWinRTInteropError);

  Result := FInterop;
end;

initialization
  if not IsLibrary then
  begin
    SaveInitProc := InitProc;
    InitProc := @InitWinRT;
  end;

finalization
  if NeedToUninitialize and TOSVersion.Check(6, 2) then
    RoUninitialize;

end.
