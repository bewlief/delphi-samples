{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Clipboard.iOS;

interface

{$SCOPEDENUMS ON}

implementation

uses
  System.Rtti, System.Classes, System.Generics.Collections, System.SysUtils, System.RTLConsts, Macapi.Helpers,
  iOSapi.Foundation, iOSapi.UIKit, FMX.Platform, FMX.Clipboard, FMX.Surfaces,FMX.Graphics, FMX.Helpers.iOS, FMX.Consts;

type
  TiOSClipboardService = class(TInterfacedObject, IFMXClipboardService, IFMXExtendedClipboardService)
  private
    FClipboardFormats: TDictionary<string, NSString>;
    procedure CheckDictionary;
    procedure SetClipboardUIImage(const Image: UIImage);
    function GetPasteboard: UIPasteboard;
  public
    destructor Destroy; override;
    { IFMXClipboardService }
    function GetClipboard: TValue;
    procedure SetClipboard(Value: TValue);
    { IFMXExtendedClipboardService }
    function HasText: Boolean;
    function GetText: string;
    procedure SetText(const Value: string);
    function HasImage: Boolean;
    function GetImage: TBitmapSurface;
    procedure SetImage(const Value: TBitmapSurface);
    procedure RegisterCustomFormat(const AFormatName: string);
    function IsCustomFormatRegistered(const AFormatName: string): Boolean;
    procedure UnregisterCustomFormat(const AFormatName: string);
    function HasCustomFormat(const AFormatName: string): Boolean;
    function GetCustomFormat(const AFormatName: string; const AStream: TStream): Boolean;
    procedure SetCustomFormat(const AFormatName: string; const AStream: TStream);
  public
    property Pasteboard: UIPasteboard read GetPasteboard;
  end;

  TPointerStream = class(TCustomMemoryStream)
  public
    constructor Create(Ptr: Pointer; Size: NativeInt); overload;
  end;

const
  cnAFormatNameParameter = 'AFormatName';
  cnAStreamParameter = 'AStream';

procedure RegisterService;
var
  ClipboardService: TiOSClipboardService;
begin
  if TUIPasteboard.OCClass.generalPasteboard <> nil then
  begin
    ClipboardService := TiOSClipboardService.Create;
    TPlatformServices.Current.AddPlatformService(IFMXClipboardService, ClipboardService);
    TPlatformServices.Current.AddPlatformService(IFMXExtendedClipboardService, ClipboardService);
  end;
end;

procedure UnregisterService;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXClipboardService);
    TPlatformServices.Current.RemovePlatformService(IFMXExtendedClipboardService);
  end;
end;

{ TPointerStream }

constructor TPointerStream.Create(Ptr: Pointer; Size: NativeInt);
begin
  inherited Create;
  SetPointer(Ptr, Size);
end;

{ TiOSClipboardService }

procedure TiOSClipboardService.CheckDictionary;
begin
  if FClipboardFormats = nil then
    FClipboardFormats := TDictionary<string, NSString>.Create;
end;

procedure TiOSClipboardService.SetClipboardUIImage(const Image: UIImage);
begin
  Pasteboard.setImage(Image);
end;

function TiOSClipboardService.GetPasteboard: UIPasteboard;
begin
  Result := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard);
end;

destructor TiOSClipboardService.Destroy;
begin
  FreeAndNil(FClipboardFormats);
  inherited;
end;

function TiOSClipboardService.GetClipboard: TValue;
begin
  if HasText then
    Result := GetText
  else if HasImage then
    Result := GetImage
  else
    Result := TValue.Empty;
end;

procedure TiOSClipboardService.SetClipboard(Value: TValue);
begin
  if Value.IsType<TBitmap> then
    SetClipboardUIImage(BitmapToUIImage(Value.AsType<TBitmap>))
  else if Value.IsType<TBitmapSurface> then
    SetClipboardUIImage(BitmapSurfaceToUIImage(Value.AsType<TBitmapSurface>))
  else if not Value.IsEmpty then
    SetText(Value.ToString);
end;

function TiOSClipboardService.HasText: Boolean;
begin
  Result := Pasteboard.hasStrings;
end;

function TiOSClipboardService.GetText: string;
begin
  Result := NSStrToStr(Pasteboard.&string);
end;

procedure TiOSClipboardService.SetText(const Value: string);
begin
  Pasteboard.setString(StrToNSStr(Value));
end;

function TiOSClipboardService.HasImage: Boolean;
begin
  Result := Pasteboard.hasImages;
end;

function TiOSClipboardService.GetImage: TBitmapSurface;
var
  Image: UIImage;
begin
  Image := Pasteboard.image;
  if Image = nil then
    Result := nil
  else
    Result := UIImageToBitmapSurface(Pasteboard.image);
end;

procedure TiOSClipboardService.SetImage(const Value: TBitmapSurface);
begin
  SetClipboardUIImage(BitmapSurfaceToUIImage(Value));
end;

procedure TiOSClipboardService.RegisterCustomFormat(const AFormatName: string);
begin
  if string.IsNullOrEmpty(AFormatName) then
    raise EArgumentException.CreateFmt(SParamIsNil, [cnAFormatNameParameter]);

  CheckDictionary;
  if FClipboardFormats.ContainsKey(AFormatName) then
    raise EClipboardFormatRegisterError.Create(Format(SFormatAlreadyRegistered, [AFormatName]));

  FClipboardFormats.Add(AFormatName, StrToNSStr(AFormatName));
end;

function TiOSClipboardService.IsCustomFormatRegistered(const AFormatName: string): Boolean;
begin
  if string.IsNullOrEmpty(AFormatName) then
    raise EArgumentException.CreateFmt(SParamIsNil, [cnAFormatNameParameter]);

  CheckDictionary;
  Result := FClipboardFormats.ContainsKey(AFormatName);
end;

procedure TiOSClipboardService.UnregisterCustomFormat(const AFormatName: string);
begin
  if string.IsNullOrEmpty(AFormatName) then
    raise EArgumentException.CreateFmt(SParamIsNil, [cnAFormatNameParameter]);

  CheckDictionary;
  if FClipboardFormats.ContainsKey(AFormatName) then
    FClipboardFormats.Remove(AFormatName)
  else
    raise EClipboardFormatNotRegistered.Create(Format(SFormatWasNotRegistered, [AFormatName]));
end;

function TiOSClipboardService.HasCustomFormat(const AFormatName: string): Boolean;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  CheckDictionary;
  if not FClipboardFormats.ContainsKey(AFormatName) then
    raise EClipboardFormatNotRegistered.Create(AFormatName);

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := Pasteboard.dataForPasteboardType(FClipboardFormats[AFormatName]) <> nil;
  finally
    AutoReleasePool.release;
  end;
end;

function TiOSClipboardService.GetCustomFormat(const AFormatName: string; const AStream: TStream): Boolean;
var
  AutoReleasePool: NSAutoreleasePool;
  CustomData: NSData;
  DataStream: TPointerStream;
begin
  if string.IsNullOrEmpty(AFormatName) then
    raise EArgumentException.CreateFmt(SParamIsNil, [cnAFormatNameParameter]);
  if AStream = nil then
    raise EArgumentNilException.CreateFmt(SParamIsNil, [cnAStreamParameter]);

  CheckDictionary;
  if not FClipboardFormats.ContainsKey(AFormatName) then
    raise EClipboardFormatNotRegistered.Create(AFormatName);

  Result := False;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    CustomData := Pasteboard.dataForPasteboardType(FClipboardFormats[AFormatName]);
    if CustomData <> nil then
    begin
      DataStream := TPointerStream.Create(CustomData.bytes, CustomData.length);
      try
        AStream.CopyFrom(DataStream, DataStream.Size);
        Result := True;
      finally
        DataStream.Free;
      end;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TiOSClipboardService.SetCustomFormat(const AFormatName: string; const AStream: TStream);
var
  AutoReleasePool: NSAutoreleasePool;
  MemoryStream: TMemoryStream;
begin
  if string.IsNullOrEmpty(AFormatName) then
    raise EArgumentException.CreateFmt(SParamIsNil, [cnAFormatNameParameter]);
  if AStream = nil then
    raise EArgumentNilException.CreateFmt(SParamIsNil, [cnAStreamParameter]);

  CheckDictionary;
  if not FClipboardFormats.ContainsKey(AFormatName) then
    raise EClipboardFormatNotRegistered.Create(AFormatName);

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.CopyFrom(AStream, AStream.Size - AStream.Position);
      MemoryStream.Position := 0;
      Pasteboard.setData(TNSData.Wrap(TNSData.OCClass.dataWithBytes(MemoryStream.Memory, MemoryStream.Size)), FClipboardFormats[AFormatName]);
    finally
      MemoryStream.Free;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

initialization
  RegisterService;
finalization
  UnregisterService;
end.

