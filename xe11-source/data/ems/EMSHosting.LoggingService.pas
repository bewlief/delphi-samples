{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2015-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSHosting.LoggingService;

{$HPPEMIT LINKUNIT}

interface

uses
  System.SysUtils, System.Classes, EMS.Services,
  System.Generics.Collections, System.JSON;

type
  TEMSLoggingService = class(TInterfacedObject, IEMSLoggingService, IEMSLoggingServiceExt)
  private
    FWriter: TStreamWriter;
    FLogIsEmpty: Boolean;
    FOutputProc: TEMSLoggingOutputProc;
    FEnabledFunc: TEMSLoggingEnabledFunc;
    FSynchronize: Boolean;
    FPrefixes: string;
    FTimeFormat: string;
    FCounter: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    // IEMSLoggingService
    function GetLoggingEnabled: Boolean;
    procedure Log(const ACategory: string; const AJSON: TJSONObject);
    procedure SetupCustomOutput(const AEnabled: TEMSLoggingEnabledFunc;
      const AOutput: TEMSLoggingOutputProc; const ASynchronize: Boolean);
    procedure SetupFileOutput(const AFileName: string; const AAppend: Boolean);
    // IEMSLoggingServiceExt
    procedure SetupCommon(const APrefixes, ATimeFormat: string);
  end;

implementation

uses
  System.DateUtils, EMSHosting.ExtensionsServices, EMSHosting.Helpers;

{ TEMSLoggingService }

constructor TEMSLoggingService.Create;
begin
  inherited Create;
  FPrefixes := 'TD';
  FTimeFormat := 'C';
end;

destructor TEMSLoggingService.Destroy;
begin
  SetupFileOutput('', False);
  SetupCustomOutput(nil, nil, False);
  inherited Destroy;
end;

procedure TEMSLoggingService.SetupFileOutput(const AFileName: string;
  const AAppend: Boolean);
var
  LJSON: TJSONObject;
  LLine: string;
begin
  if FWriter <> nil then
  begin
    if not FLogIsEmpty then
      FWriter.WriteLine;
    FWriter.Write(']}');
    FWriter.WriteLine;
    FreeAndNil(FWriter);
  end;

  if AFileName <> '' then
    try
      FWriter := TStreamWriter.Create(AFileName, AAppend, TEncoding.UTF8);
      LJSON := TJSONObject.Create;
      try
        LJSON.AddPair(TLogObjectNames.Application, GetModuleName(HInstance));
        LJSON.AddPair(TLogObjectNames.Started, DateToISO8601(Now(), False));
        LJSON.AddPair(TLogObjectNames.Log, TJSONArray.Create);
        LLine := LJSON.ToJSON;
        FWriter.Write(Copy(LLine, 1, Length(LLine) - 2));
        FWriter.WriteLine;
        FLogIsEmpty := True;
      finally
        LJSON.Free;
      end;
    except
      // in case of exception - disable logging and silent exeption
      // otherwise Apache / IIS will stop processing any RS requests
      FreeAndNil(FWriter);
    end;
end;

procedure TEMSLoggingService.SetupCommon(const APrefixes, ATimeFormat: string);
begin
  FPrefixes := APrefixes.ToUpper;
  if ATimeFormat = '' then
    FTimeFormat := 'C'
  else
    FTimeFormat := ATimeFormat;
end;

procedure TEMSLoggingService.SetupCustomOutput(const AEnabled: TEMSLoggingEnabledFunc;
  const AOutput: TEMSLoggingOutputProc; const ASynchronize: Boolean);
begin
  FEnabledFunc := AEnabled;
  FOutputProc := AOutput;
  FSynchronize := ASynchronize;
end;

function TEMSLoggingService.GetLoggingEnabled: Boolean;
begin
  if FWriter <> nil then
    Result := True
  else
  begin
    Result := Assigned(FOutputProc);
    if Result then
      if Assigned(FEnabledFunc) then
        Result := FEnabledFunc();
  end;
end;

procedure TEMSLoggingService.Log(const ACategory: string; const AJSON: TJSONObject);
var
  LJSON: TJSONObject;
  LPrivateJSON: TJSONObject;
  LLine: string;
  LPair: TJSONPair;
  I: Integer;
begin
  if not GetLoggingEnabled then
    Exit;

  LJSON := TJSONObject.Create;
  try
    for I := 1 to Length(FPrefixes) do
      case FPrefixes[I] of
      'T':
        LJSON.AddPair(TLogObjectNames.Thread, TJSONNumber.Create(TThread.CurrentThread.ThreadID));
      'D':
        LJSON.AddPair(TLogObjectNames.Time, FormatDateTime(FTimeFormat, Now));
      'I':
        begin
          LPair := AJSON.RemovePair(TLogObjectNames.ClientIP);
          if LPair <> nil then
            LJSON.AddPair(LPair);
        end;
      'C':
        begin
          TMonitor.Enter(Self);
          try
            if FCounter = Int64.MaxValue then
              FCounter := 0;
            Inc(FCounter);
            LJSON.AddPair(TLogObjectNames.Num, TJSONNumber.Create(FCounter));
          finally
            TMonitor.Exit(Self);
          end;
        end;
      end;

    LJSON.AddPair(ACategory, TJSONValue(AJSON.Clone));

    if FWriter <> nil then
    begin
      TMonitor.Enter(FWriter);
      try
        if not FLogIsEmpty then
        begin
          FWriter.Write(',');
          FWriter.WriteLine;
        end;
        LLine := LJSON.ToString;
        FWriter.Write(LLine);
        FLogIsEmpty := False;
      finally
        TMonitor.Exit(FWriter);
      end;
    end;

    if Assigned(FOutputProc) and (not Assigned(FEnabledFunc) or FEnabledFunc()) then
      if FSynchronize then
      begin
        LPrivateJSON := LJSON;
        LJSON := nil;
        TThread.Synchronize(nil,
          procedure
          begin
            FOutputProc(ACategory, LPrivateJSON);
            LPrivateJSON.Free;
          end);
      end
      else
        FOutputProc(ACategory, LJSON);
  finally
    LJSON.Free;
  end;
end;

var
  LIndex: Integer;
initialization
  LIndex := AddService(TEMSLoggingService.Create as IEMSLoggingService);
finalization
  RemoveService(LIndex);
end.
