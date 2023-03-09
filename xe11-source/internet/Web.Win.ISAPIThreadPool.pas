{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{       ISAPI/NSAPI Web server application components   }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$DENYPACKAGEUNIT}

unit Web.Win.ISAPIThreadPool;

interface

uses Winapi.Isapi2, Winapi.Windows, System.SyncObjs;

type
  TISAPIThreadPool = class;
  TISAPIThreadPoolClass = class of TISAPIThreadPool;

{$WARN SYMBOL_DEPRECATED OFF}
  TISAPIThreadPool = class(TObject)
  private
    FCriticalSection: TCriticalSection;
    FRequestQueue: THandle;
    FInitCOM: Boolean;
    FMin: Integer deprecated;
    FMax: Integer deprecated;
    function GetThreadCount: Integer;
    procedure SetMax(const Value: Integer); deprecated;
    procedure SetMin(const Value: Integer); deprecated;
    procedure SetInitCOM(const Value: Boolean);
  protected
    FThreads: array of THandle;
    procedure Initialize; virtual;
    function PushBack(ECB: PEXTENSION_CONTROL_BLOCK): Boolean;
    procedure ShutDown; virtual;
    property RequestQueue: THandle read FRequestQueue;
    property Handle: THandle read FRequestQueue;
  public
    constructor Create(InitCOM: Boolean = False); virtual;
    destructor Destroy; override;
    property InitCOM: Boolean read FInitCOM write SetInitCOM;
    property Min: Integer read FMin write SetMin default 1;
    property Max: Integer read FMax write SetMax default 32;
    property ThreadCount: Integer read GetThreadCount;
  end;
{$WARN SYMBOL_DEPRECATED ON}

var
  { NumberOfThreads is the number of threads used to process incoming
    connections to the server. If you want to change this value you will
    need to set it prior to the first request (ie your application's main
    program block). }
                                                                                  
  NumberOfThreads: Byte;

  { StackSize is the stack size in bytes of a thread used to process incoming
    connections to the server. The default value is 256Kb. If application
    raises "stack overflow" exception, then set StackSize to a higher
    value, for example 1Mb, which is default stack size for Delphi desktop
    applications. If you want to change this value you will need to set it
    prior to the first request (ie your application's main program block). }
  StackSize: LongWord;

  { ThreadPoolClass is a class reference which you can set in your main
    program block should you need to implement a descendant of
    TISAPIThreadPool. Setting this variable to your new thread class
    will cause your class to be used in place of the one provided. }
  ThreadPoolClass: TISAPIThreadPoolClass;
  ThreadPool: TISAPIThreadPool;

  { ShutdownTimeout is a timeout in milliseconds, which thread pool
    will wait for finalization of all worker threads. The default
    value is 2 minutes. }
  ShutdownTimeout: DWORD;

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;

implementation

uses System.SysUtils, Web.WebBroker, Web.Win.ISAPIApp, Winapi.ActiveX, System.Win.ComObj;

const
  SHUTDOWN_FLAG = $FFFFFFFF; // Posted to the completion port when shutting down

var
  IsNetscape: Boolean;     // True, if loaded on a Netscape server, false otherwise

type
  PServerContext = ^TServerContext;
  TServerContext = record
    ECB: PEXTENSION_CONTROL_BLOCK;
    Token: THandle;
  end;

  PWOHandlesSlice = ^TWOHandlesSlice;
  TWOHandlesSlice = record
    FCount: DWORD;
    FHandles: TWOHandleArray;
  end;

function WorkerFunction(ThreadPool: TISAPIThreadPool): Integer;
var
  ServerContext: PServerContext;
  NumberOfBytes: DWord;
  Overlapped: POverlapped;
begin
  // The ServerContext parameter retrieves a pointer value that is passed along
  // by the call to PostQueuedCompletionStatus, not a completion key.
  while GetQueuedCompletionStatus(ThreadPool.FRequestQueue, NumberOfBytes,
    UIntPtr(ServerContext), Overlapped, INFINITE) do
  try
    try
      if UIntPtr(OverLapped) = SHUTDOWN_FLAG then   // Shut down
        break;
      with ServerContext^ do
      begin
        ImpersonateLoggedOnUser(Token);
        Web.Win.ISAPIApp.HttpExtensionProc(ECB^);
        ECB^.ServerSupportFunction(ECB^.ConnID, HSE_REQ_DONE_WITH_SESSION, nil, nil, nil);
      end;
    finally
      if Assigned(ServerContext) then
      begin
        CloseHandle(ServerContext^.Token);
        Dispose(ServerContext);
        ServerContext := nil;
      end;
    end;
  except
    // All exceptions should have been handled by Web.Win.ISAPIApp.HttpExtensionProc
  end;
  Result := 0;
end;

// Wraps WorkerFunction with a try...finally to handle COM un/initialization.
function COMWorkerFunction(ThreadPool: TISAPIThreadPool): Integer;
begin
  CoInitializeEx(nil, CoInitFlags);
  try
    Result := WorkerFunction(ThreadPool);
  finally
    CoUninitialize;
  end;
end;

function WaitFunction(Slice: PWOHandlesSlice): Integer;
begin
  WaitForMultipleObjects(Slice^.FCount, @Slice^.FHandles[0], True, ShutdownTimeout);
  Result := 0;
end;

{ TISAPIThreadPool }

constructor TISAPIThreadPool.Create(InitCOM: Boolean);
begin
  inherited Create;
  FInitCOM := InitCOM;
  FRequestQueue := INVALID_HANDLE_VALUE;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TISAPIThreadPool.Destroy;
begin
  ShutDown;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TISAPIThreadPool.Initialize;
var
  ThreadID: DWORD;
  I: Integer;
  ThreadFunc: TThreadFunc;
begin
  FCriticalSection.Enter;
  try
    if FRequestQueue <> INVALID_HANDLE_VALUE then exit;
    // Create IO completion port to queue the ISAPI requests
    FRequestQueue := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  finally
    FCriticalSection.Leave;
  end;
  // Create and initialize worker threads
  SetLength(FThreads, NumberOfThreads);
  ThreadFunc := @WorkerFunction;
  if FInitCOM then
    ThreadFunc := @COMWorkerFunction;
  for I := 0 to NumberOfThreads - 1 do
    FThreads[I] := BeginThread(nil, StackSize, ThreadFunc, Self, 0, ThreadID)
end;

procedure TISAPIThreadPool.ShutDown;
var
  ThreadID: DWORD;
  I: Integer;
  Waiters: Integer;
  WaitSlices: array of TWOHandlesSlice;
  WaitThreads: array of THandle;
begin
  if FRequestQueue = INVALID_HANDLE_VALUE then exit;
  // Tell the threads we're shutting down
  for I := 0 to NumberOfThreads - 1 do
    PostQueuedCompletionStatus(FRequestQueue, 0, 0, POverLapped(SHUTDOWN_FLAG));

  // Wait for threads to finish
  if NumberOfThreads > MAXIMUM_WAIT_OBJECTS then
  begin
    Waiters := (NumberOfThreads + MAXIMUM_WAIT_OBJECTS - 1) div MAXIMUM_WAIT_OBJECTS;
    if Waiters > MAXIMUM_WAIT_OBJECTS then
      Waiters := MAXIMUM_WAIT_OBJECTS;
    SetLength(WaitThreads, Waiters);
    SetLength(WaitSlices, Waiters);
    for I := 0 to Waiters - 1 do
    begin
      WaitSlices[I].FCount := NumberOfThreads - I * MAXIMUM_WAIT_OBJECTS;
      if WaitSlices[I].FCount > MAXIMUM_WAIT_OBJECTS then
        WaitSlices[I].FCount := MAXIMUM_WAIT_OBJECTS;
      Move(FThreads[MAXIMUM_WAIT_OBJECTS * I], WaitSlices[I].FHandles[0], WaitSlices[I].FCount * SizeOf(THandle));
      WaitThreads[I] := BeginThread(nil, 0, @WaitFunction, @WaitSlices[I], 0, ThreadID);
    end;
    WaitForMultipleObjects(Waiters, @WaitThreads[0], True, ShutdownTimeout);
    for I := 0 to Waiters - 1 do
      CloseHandle(WaitThreads[I]);
    SetLength(WaitThreads, 0);
    SetLength(WaitSlices, 0);
  end
  else
    WaitForMultipleObjects(NumberOfThreads, @FThreads[0], True, ShutdownTimeout);

  // Close the request queue handle
  CloseHandle(FRequestQueue);

  // Clear the queue handle
  FRequestQueue := INVALID_HANDLE_VALUE;

  // Close the thread handles
  for I := 0 to NumberOfThreads - 1 do
    CloseHandle(FThreads[I]);
  SetLength(FThreads, 0);
end;

function TISAPIThreadPool.PushBack(ECB: PEXTENSION_CONTROL_BLOCK): Boolean;
var
  ServerContext: PServerContext;
begin
  Result := False;
  if FRequestQueue <> INVALID_HANDLE_VALUE then
  begin
    New(ServerContext);
    OpenThreadToken(GetCurrentThread(), TOKEN_ALL_ACCESS, False, ServerContext^.Token);
    ServerContext^.ECB := ECB;
    Result := PostQueuedCompletionStatus(FRequestQueue, 0, UIntPtr(ServerContext), nil);
  end;
end;

function TISAPIThreadPool.GetThreadCount: Integer;
begin
  Result := Length(FThreads);
end;

procedure TISAPIThreadPool.SetMax(const Value: Integer);
begin
  //  Method has been deprecated
end;

procedure TISAPIThreadPool.SetMin(const Value: Integer);
begin
  //  Method has been deprecated
end;

procedure TISAPIThreadPool.SetInitCOM(const Value: Boolean);
begin
  if FRequestQueue = INVALID_HANDLE_VALUE then exit;
  if Value <> FInitCOM then
    FInitCOM := Value
end;

{ Exported functions }

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and not IsNetscape and
     not Assigned(ThreadPool) then
    ThreadPool := ThreadPoolClass.Create;
  Result := Web.Win.ISAPIApp.GetExtensionVersion(Ver);
end;

function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and not IsNetscape and
     Assigned(ThreadPool) then
  try
    if ThreadPool.Handle = INVALID_HANDLE_VALUE then
      ThreadPool.Initialize;
    Result := HSE_STATUS_ERROR;
    if ThreadPool.PushBack(@ECB) then
      Result := HSE_STATUS_PENDING
  except
    Result := HSE_STATUS_ERROR;
  end
  else
    Result := Web.Win.ISAPIApp.HttpExtensionProc(ECB);
end;

function TerminateExtension(dwFlags: DWORD): BOOL;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and not IsNetscape and
     Assigned(ThreadPool) then
  begin
    try
      // Let application know that extension is terminating
      Web.Win.ISAPIApp.TerminateExtension(dwFlags);
    finally
      FreeAndNil(ThreadPool);
    end;
    Result := True;
  end
  else
    Result := Web.Win.ISAPIApp.TerminateExtension(dwFlags);
end;

function CheckForNetscapeServer: Boolean;
var
  LoadingModule: string;
begin
  SetLength(LoadingModule, 4096);
  SetLength(LoadingModule, GetModuleFileName(GetModuleHandle(nil),
    PChar(LoadingModule), Length(LoadingModule)));
  Result := CompareText(ExtractFileName(LoadingModule), 'httpd.exe') = 0;  {do not localize}
end;

initialization
  // If we are loading under Netscape do not use the thread pool
  IsNetscape := CheckForNetscapeServer;
  ThreadPoolClass := TISAPIThreadPool;
  NumberOfThreads := 25;
  StackSize := 0;
  ShutdownTimeout := 60000 * 2;
finalization
  FreeAndNil(ThreadPool);
end.

