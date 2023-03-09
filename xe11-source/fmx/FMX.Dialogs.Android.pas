{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2015-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Dialogs.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Generics.Collections, System.UITypes, Androidapi.JNIBridge, Androidapi.JNI.Embarcadero,
  Androidapi.Jni.JavaTypes, FMX.Dialogs;

type
  TFMXDialogListener = class;
  TFMXDialogListenerParentList = class(TList<TFmxDialogListener>);

  TFMXDialogListener = class(TJavaLocal, JFMXDialogListener)
  private
    [Weak] FParentList: TFMXDialogListenerParentList;
    FValues: array of string;
    FDefaultValues: array of string;
    FModalResult: TModalResult;
    FInputCloseQueryProc: TInputCloseQueryProc;
    FCallerThread: TThread;
    procedure DoDialogClosed;
    procedure SetParentList(const AList: TFMXDialogListenerParentList);
  public
    constructor Create(const AInputCloseQueryProc: TInputCloseQueryProc);
    procedure onDialogClosed(modalResult: Integer; values: TJavaObjectArray<JString>); cdecl;
    property ParentList: TFMXDialogListenerParentList read FParentList write SetParentList;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Math, System.Types, Androidapi.Jni, Androidapi.Helpers, FMX.Platform.Android,
  FMX.Helpers.Android, FMX.Platform, FMX.Consts, FMX.Forms, FMX.Types;

type
  TAndroidDialogService = class(TInterfacedObject, IFMXDialogServiceAsync, IFMXDialogService)
  private
    FAlertListeners: TFMXDialogListenerParentList;
    FDialogsQueue: TQueue<JFMXStandardDialog>;
  protected
    procedure ShowMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
                                const ADefaultButton: TMsgDlgBtn; const ACloseDialogProc: TInputCloseDialogProc);
    procedure ShowInputQuery(const ACaption: string; const APrompts: array of string;
                             const ADefaultValues: array of string; const ACloseQueryProc: TInputCloseQueryProc);
    /// <summary>If we show several dialogs in the same time, the Android shows them in reverse order. So we put all
    /// dialogs in queue for showing them one by one.</summary>
    procedure ShowOrPutInQueue(const ADialog: JFMXStandardDialog);
    procedure ShowNextDialog;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXDialogService }
    function DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
                         var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer;
                         var APrintRange: TPrintRange; AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
                                  AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
                             var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
                           const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
                           const AHelpFileName: string): Integer; overload;
    procedure MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
                            const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
                            const AHelpFileName: string; const ACloseDialogProc: TInputCloseDialogProc); overload;
    function InputQuery(const ACaption: string; const APrompts: array of string;
                        var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc = nil): Boolean; overload;
    procedure InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
                         const ACloseQueryProc: TInputCloseQueryProc); overload;

    { IFMXDialogServiceAsync }
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string); overload;
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string; const ACloseDialogProc: TInputCloseDialogProc); overload;
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string; const ACloseDialogEvent: TInputCloseDialogEvent;
                               const AContext: TObject = nil); overload;

    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    procedure MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
                                 const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext;
                                 const ACloseDialogProc: TInputCloseDialogProc); overload;
    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    procedure MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
                                 const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext;
                                 const ACloseDialogEvent: TInputCloseDialogEvent; const AContext: TObject = nil); overload;

    /// <summary>Shows an input message dialog with the specified promps and values on it. </summary>
    procedure InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
                              const ACloseQueryProc: TInputCloseQueryProc); overload;
    /// <summary>Shows an input message dialog with the specified promps and values on it. </summary>
    procedure InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
                              const ACloseQueryEvent: TInputCloseQueryWithResultEvent; const AContext: TObject = nil); overload;
  end;

function CreateJavaStringArray(const ASource: array of string): TJavaObjectArray<JString>;
var
  I: Integer;
begin
  Result := TJavaObjectArray<JString>.Create(Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    Result[I] := StringToJString(ASource[I]);
end;

{ TFMXDialogListener }

constructor TFMXDialogListener.Create(const AInputCloseQueryProc: TInputCloseQueryProc);
begin
  inherited Create;
  FInputCloseQueryProc := AInputCloseQueryProc;
  FCallerThread := TThread.Current;
end;

procedure TFMXDialogListener.DoDialogClosed;
begin
  FInputCloseQueryProc(FModalResult, FValues);
  if FParentList <> nil then
    FParentList.Remove(Self);
end;

procedure TFMXDialogListener.onDialogClosed(modalResult: Integer; values: TJavaObjectArray<JString>);
var
  I: Integer;
begin
  FModalResult := modalResult;
  if values <> nil then
  begin
    SetLength(FValues, values.Length);
    for I := 0 to values.Length - 1 do
    begin
      if FModalResult = mrOk then
        FValues[I] := JStringToString(values.Items[I])
      else if FDefaultValues <> nil then
        FValues[I] := FDefaultValues[I];
    end
  end;
  TThread.Queue(FCallerThread, DoDialogClosed);
end;

procedure TFMXDialogListener.SetParentList(const AList: TFMXDialogListenerParentList);
begin
  FParentList := AList;
  if FParentList <> nil then
    FParentList.Add(Self);
end;

{ TFMXDialogService }

constructor TAndroidDialogService.Create;
begin
  inherited;
  FAlertListeners := TFMXDialogListenerParentList.Create;
  FDialogsQueue := TQueue<JFMXStandardDialog>.Create;
end;

destructor TAndroidDialogService.Destroy;
begin
  FreeAndNil(FDialogsQueue);
  FreeAndNil(FAlertListeners);
  inherited;
end;

function TAndroidDialogService.DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
begin
  Result := False;
end;

function TAndroidDialogService.DialogPageSetup(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
  var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TAndroidDialogService.DialogPrint(var ACollate, APrintToFile: Boolean; var AFromPage, AToPage, ACopies: Integer;
  AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange; AOptions: TPrintDialogOptions): Boolean;
begin
  Result := False;
end;

function TAndroidDialogService.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TAndroidDialogService.DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
begin
  Result := False;
end;

{ IFMXDialogServiceAsync }

procedure TAndroidDialogService.ShowMessageAsync(const AMessage: string);
begin
  MessageDialogAsync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, nil);
end;

procedure TAndroidDialogService.ShowMessageAsync(const AMessage: string; const ACloseDialogProc: TInputCloseDialogProc);
begin
  MessageDialogAsync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, ACloseDialogProc);
end;

procedure TAndroidDialogService.ShowMessageAsync(const AMessage: string; const ACloseDialogEvent: TInputCloseDialogEvent;
      const AContext: TObject);
begin
  MessageDialogAsync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, ACloseDialogEvent, AContext);
end;

procedure TAndroidDialogService.ShowMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const ACloseDialogProc: TInputCloseDialogProc);

  function Length(const AButtons: TMsgDlgButtons): Integer;
  var
    Button: TMsgDlgBtn;
  begin
    Result := 0;
    for Button in AButtons do
      Inc(Result);
  end;

var
  DialogFactory: JFMXDialogFactory;
  Listener: TFMXDialogListener;
  Dialog: JFMXStandardDialog;
  PosButton, NegButton, NeutralButton: Integer;
  B: TMsgDlgBtn;
  ButtonIndex: Integer;
  ButtonsCount: Integer;
  LCaptions: TJavaObjectArray<JString>;
begin
  MessageDialogCheckInUIThread;

  ButtonIndex := 0;
  PosButton := -1;
  NegButton := -1;
  NeutralButton := -1;
  ButtonsCount := Min(Length(AButtons), 3);

  LCaptions := TJavaObjectArray<JString>.Create(ButtonsCount);
  try
    for B in AButtons do
    begin
      if ButtonIndex < ButtonsCount then
      begin
        LCaptions.Items[ButtonIndex] := StringToJString(LocalizedButtonCaption(B));
        case ButtonIndex of
          0: PosButton := ModalResults[B];
          1: NegButton := ModalResults[B];
          2: NeutralButton := ModalResults[B];
        end;
      end;
      Inc(ButtonIndex);
    end;

    DialogFactory := TJFMXDialogFactory.JavaClass.getFactory;
    Dialog := DialogFactory.createMessageDialog(MainActivity, GetNativeTheme, StringToJString(AMessage),
                                                Ord(ADialogType), LCaptions, PosButton, NegButton, NeutralButton);
    Listener := TFMXDialogListener.Create(
      procedure (const AResult: TModalResult; const AValues: array of string)
      begin
        try
          if Assigned(ACloseDialogProc) then
            ACloseDialogProc(AResult);
        finally
          ShowNextDialog;
        end;
      end);
    Listener.ParentList := FAlertListeners;
    Dialog.setListener(Listener);
    ShowOrPutInQueue(Dialog);
  finally
    FreeAndNil(LCaptions);
  end;
end;

procedure TAndroidDialogService.ShowNextDialog;
begin
  if FDialogsQueue.Count > 0 then
    FDialogsQueue.Dequeue;
  if FDialogsQueue.Count > 0 then
    FDialogsQueue.Peek.show;
end;

procedure TAndroidDialogService.ShowOrPutInQueue(const ADialog: JFMXStandardDialog);
begin
  Assert(ADialog <> nil);

  if FDialogsQueue.Count = 0 then
    ADialog.show;

  FDialogsQueue.Enqueue(ADialog);
end;

procedure TAndroidDialogService.MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogProc: TInputCloseDialogProc);
begin
  ShowMessageDialog(AMessage, ADialogType, AButtons, ADefaultButton, ACloseDialogProc);
end;

function TAndroidDialogService.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
  const AHelpFileName: string): Integer;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform, [SBlockingDialogs]);
end;

procedure TAndroidDialogService.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
  const AHelpFileName: string; const ACloseDialogProc: TInputCloseDialogProc);
begin
  ShowMessageDialog(AMessage, ADialogType, AButtons, ADefaultButton, ACloseDialogProc);
end;

procedure TAndroidDialogService.MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogEvent: TInputCloseDialogEvent;
  const AContext: TObject);
begin
  MessageDialogAsync(AMessage, ADialogType, AButtons, ADefaultButton, AHelpCtx,
    procedure (const AResult: TModalResult)
    begin
      if Assigned(ACloseDialogEvent) then
        ACloseDialogEvent(AContext, AResult);
    end);
end;

function TAndroidDialogService.PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
  AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

procedure TAndroidDialogService.ShowInputQuery(const ACaption: string; const APrompts: array of string;
  const ADefaultValues: array of string; const ACloseQueryProc: TInputCloseQueryProc);
var
  Dialog: JFMXStandardDialog;
  Listener: TFMXDialogListener;
  DialogFactory: JFMXDialogFactory;
  JavaPrompts: TJavaObjectArray<JString>;
  JavaDefaultValues: TJavaObjectArray<JString>;
  JavaCaptions: TJavaObjectArray<JString>;
begin
  if Length(ADefaultValues) < Length(APrompts) then
    raise EInvalidOperation.Create(SPromptArrayTooShort);
  if Length(APrompts) = 0 then
    raise EInvalidOperation.Create(SPromptArrayEmpty);

  MessageDialogCheckInUIThread;

  JavaPrompts := CreateJavaStringArray(APrompts);
  try
    JavaDefaultValues := CreateJavaStringArray(ADefaultValues);
    try
      JavaCaptions := CreateJavaStringArray([LocalizedButtonCaption(TMsgDlgBtn.mbOK), LocalizedButtonCaption(TMsgDlgBtn.mbCancel)]);
      try
        DialogFactory := TJFMXDialogFactory.JavaClass.getFactory;
        Dialog := DialogFactory.createInputQueryDialog(MainActivity, GetNativeTheme, StringToJString(ACaption),
                                                       JavaPrompts, JavaDefaultValues, JavaCaptions);

        Listener := TFMXDialogListener.Create(
          procedure (const AResult: TModalResult; const AValues: array of string)
          begin
            try
              if Assigned(ACloseQueryProc) then
                ACloseQueryProc(AResult, AValues);
            finally
              ShowNextDialog;
            end;
          end);
        Listener.ParentList := FAlertListeners;
        SetLength(Listener.FDefaultValues, Length(ADefaultValues));
        TArray.Copy<string>(ADefaultValues, Listener.FDefaultValues, Length(ADefaultValues));
        Dialog.setListener(Listener);
        ShowOrPutInQueue(Dialog);
      finally
        FreeAndNil(JavaCaptions);
      end;
    finally
      FreeAndNil(JavaDefaultValues);
    end;
  finally
    FreeAndNil(JavaPrompts);
  end;
end;

procedure TAndroidDialogService.InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
  const ACloseQueryProc: TInputCloseQueryProc);
begin
  ShowInputQuery(ACaption, APrompts, ADefaultValues, ACloseQueryProc);
end;

procedure TAndroidDialogService.InputQueryAsync(const ACaption: string; const APrompts, ADefaultValues: array of string;
  const ACloseQueryProc: TInputCloseQueryProc);
begin
  ShowInputQuery(ACaption, APrompts, ADefaultValues, ACloseQueryProc);
end;

function TAndroidDialogService.InputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc): Boolean;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform + sLineBreak + SInputQueryAndroidOverloads,
    [SBlockingDialogs]);
end;

procedure TAndroidDialogService.InputQueryAsync(const ACaption: string; const APrompts: array of string;
  const ADefaultValues: array of string; const ACloseQueryEvent: TInputCloseQueryWithResultEvent;
  const AContext: TObject);
begin
  InputQueryAsync(ACaption, APrompts, ADefaultValues,
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if Assigned(ACloseQueryEvent) then
        ACloseQueryEvent(AContext, AResult, AValues);
    end);
end;

initialization
  var Service := TAndroidDialogService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXDialogServiceAsync, Service);
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, Service);
end.
