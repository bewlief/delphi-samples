{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.13    03/16/05 11:15:42 AM  JSouthwell
  Named the IdNotify thread for simpler debugging.

  Rev 1.12    2004.04.13 10:22:52 PM  czhower
  Changed procedure to class method.

  Rev 1.11    4/12/2004 11:44:36 AM  BGooijen
  fix

  Rev 1.10    4/12/2004 11:36:56 AM  BGooijen
  NotifyThread can be cleaned up with procedure now

  Rev 1.9    2004.03.11 10:14:46 AM  czhower
  Improper cast fixed.

  Rev 1.8    2004.02.29 8:23:16 PM  czhower
  Fixed visibility mismatch.

  Rev 1.7    2004.02.25 10:11:42 AM  czhower
  Fixed visibility in notify

  Rev 1.6    2004.02.03 4:16:54 PM  czhower
  For unit name changes.

  Rev 1.5    1/1/2004 11:56:10 PM  PIonescu
  Fix for TIdNotifyMethod's constructor

  Rev 1.4    2003.12.31 7:33:20 PM  czhower
  Constructor bug fix.

  Rev 1.3    5/12/2003 9:17:42 AM  GGrieve
  compile fix

  Rev 1.2    2003.09.18 5:42:14 PM  czhower
  Removed TIdThreadBase

  Rev 1.1    05.6.2003 ?. 11:30:12  DBondzhev
  Mem leak fix for notifiers created in main thread. Also WaitFor for waiting
  notification to be executed.

  Rev 1.0    11/13/2002 09:00:10 AM  JPMugaas
}

unit IdSync;

// Author: Chad Z. Hower - a.k.a. Kudzu

interface

{$i IdCompilerDefines.inc}

{$UNDEF NotifyThreadNeeded}
{$IFNDEF HAS_STATIC_TThread_Synchronize}
  {$DEFINE NotifyThreadNeeded}
{$ENDIF}
{$IFNDEF HAS_STATIC_TThread_Queue}
  {$DEFINE NotifyThreadNeeded}
{$ENDIF}

uses
  Classes,
  IdGlobal
  {$IFDEF NotifyThreadNeeded}
  , IdThread
  {$ENDIF}
  ;

type
  TIdSync = class(TObject)
  protected
    {$IFNDEF HAS_STATIC_TThread_Synchronize}
    FThread: TIdThread;
    {$ENDIF}
    //
    procedure DoSynchronize; virtual; abstract;
  public
    {$IFDEF HAS_STATIC_TThread_Synchronize}
    constructor Create; virtual;
    {$ELSE}
    constructor Create; overload; virtual;
    constructor Create(AThread: TIdThread); overload; virtual;
    {$ENDIF}
    procedure Synchronize;
    class procedure SynchronizeMethod(AMethod: TThreadMethod);
    //
    {$IFNDEF HAS_STATIC_TThread_Synchronize}
    property Thread: TIdThread read FThread;
    {$ENDIF}
  end;

  TIdNotify = class(TObject)
  protected
    FMainThreadUsesNotify: Boolean;
    //
    procedure DoNotify; virtual; abstract;
  public
    constructor Create; virtual; // here to make virtual
    procedure Notify;
    {$IFNDEF HAS_STATIC_TThread_Queue}
    procedure WaitFor;
    {$ENDIF}
    class procedure NotifyMethod(AMethod: TThreadMethod);
    //
    property MainThreadUsesNotify: Boolean read FMainThreadUsesNotify write FMainThreadUsesNotify;
  end;

  TIdNotifyMethod = class(TIdNotify)
  protected
    FMethod: TThreadMethod;
    //
    procedure DoNotify; override;
  public
    constructor Create(AMethod: TThreadMethod); reintroduce; virtual;
  end;

implementation

uses
  //facilitate inlining only.
  {$IFDEF DOTNET}
    {$IFDEF USE_INLINE}
  System.Threading,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF VCL_2010_OR_ABOVE}
    {$IFDEF WINDOWS}
  Windows,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_VCL_POSIX}
  Posix.SysSelect,
  Posix.SysTime,
  {$ENDIF}
  SysUtils;

{$IFDEF NotifyThreadNeeded}
type
  // This is done with a NotifyThread instead of PostMessage because starting
  // with D6/Kylix Borland radically modified the mecanisms for .Synchronize.
  // This is a bit more code in the end, but its source compatible and does not
  // rely on Indy directly accessing any OS APIs and performance is still more
  // than acceptable, especially considering Notifications are low priority.

  TIdNotifyThread = class(TIdThread)
  protected
    FEvent: TIdLocalEvent;
    FNotifications: TThreadList;
  public
    procedure AddNotification(ASync: TIdNotify);
    constructor Create; reintroduce;
    destructor Destroy; override;
    class procedure FreeThread;
    procedure Run; override;
  end;

var
  GNotifyThread: TIdNotifyThread = nil;

// RLebeau: this function has a race condition if it is called by multiple
// threads at the same time and GNotifyThread has not been assigned yet!
procedure CreateNotifyThread;
begin
  if GNotifyThread = nil then begin
    GNotifyThread := TIdNotifyThread.Create;
  end;
end;
{$ENDIF}

{ TIdSync }

{$IFNDEF HAS_STATIC_TThread_Synchronize}
constructor TIdSync.Create(AThread: TIdThread);
begin
  inherited Create;
  FThread := AThread;
end;
{$ENDIF}

constructor TIdSync.Create;
begin
  {$IFDEF HAS_STATIC_TThread_Synchronize}
  inherited Create;
  {$ELSE}
    {$IFDEF DOTNET}
  inherited Create;
  CreateNotifyThread;
  FThread := GNotifyThread;
    {$ELSE}
  CreateNotifyThread;
  Create(GNotifyThread);
    {$ENDIF}
  {$ENDIF}
end;

procedure TIdSync.Synchronize;
begin
  {$IFDEF HAS_STATIC_TThread_Synchronize}
  TThread.Synchronize(nil, DoSynchronize);
  {$ELSE}
  FThread.Synchronize(DoSynchronize);
  {$ENDIF}
end;

class procedure TIdSync.SynchronizeMethod(AMethod: TThreadMethod);
begin
  {$IFDEF HAS_STATIC_TThread_Synchronize}
  TThread.Synchronize(nil, AMethod);
  {$ELSE}
  CreateNotifyThread;
  GNotifyThread.Synchronize(AMethod);
  {$ENDIF}
end;

{ TIdNotify }

constructor TIdNotify.Create;
begin
  inherited Create;
end;

procedure TIdNotify.Notify;
begin
  if InMainThread and (not MainThreadUsesNotify) then begin
    DoNotify;
    Free;
  end else begin
    {$IFDEF HAS_STATIC_TThread_Queue}
    TThread.Queue(nil, DoNotify);
    {$ELSE}
    CreateNotifyThread;
    GNotifyThread.AddNotification(Self);
    {$ENDIF}
  end;
end;

class procedure TIdNotify.NotifyMethod(AMethod: TThreadMethod);
begin
  {$IFDEF HAS_STATIC_TThread_Queue}
  TThread.Queue(nil, AMethod);
  {$ELSE}
  TIdNotifyMethod.Create(AMethod).Notify;
  {$ENDIF}
end;

{$IFNDEF HAS_STATIC_TThread_Queue}
// RLebeau: this method does not make sense.  The Self pointer is not
// guaranteed to remain valid while this method is running since the
// notify thread frees the object.  Also, this makes the calling thread
// block, so TIdSync should be used instead...
procedure TIdNotify.WaitFor;
var
  LNotifyIndex: Integer;
begin
  repeat
    with GNotifyThread.FNotifications.LockList do try
      LNotifyIndex := IndexOf(Self);
    finally GNotifyThread.FNotifications.UnlockList; end;
    if LNotifyIndex = -1 then begin
      Break;
    end;
    IndySleep(10);
  until False;
end;
{$ENDIF}

{$IFDEF NotifyThreadNeeded}

{ TIdNotifyThread }

procedure TIdNotifyThread.AddNotification(ASync: TIdNotify);
begin
  FNotifications.Add(ASync);
  FEvent.SetEvent;
end;

constructor TIdNotifyThread.Create;
begin
  FEvent := TIdLocalEvent.Create;
  FNotifications := TThreadList.Create;
  // Must be before - Thread starts running when we call inherited
  inherited Create(False, False, 'IdNotify');
end;

destructor TIdNotifyThread.Destroy;
begin
  // Free remaining Notifications if thre is somthing that is still in
  // the queue after thread was terminated
  with FNotifications.LockList do try
    while Count > 0 do begin
      TIdNotify(Items[0]).Free;
      Delete(0);
    end;
  finally FNotifications.UnlockList; end;
  FreeAndNil(FNotifications);
  FreeAndNil(FEvent);
  inherited Destroy;
end;

class procedure TIdNotifyThread.FreeThread;
begin
  if GNotifyThread <> nil then begin
    GNotifyThread.Stop;
    GNotifyThread.FEvent.SetEvent;
    GNotifyThread.WaitFor;
    // Instead of FreeOnTerminate so we can set the reference to nil
    FreeAndNil(GNotifyThread);
  end;
end;

procedure TIdNotifyThread.Run;
// NOTE: Be VERY careful with making changes to this proc. It is VERY delicate and the order
// of execution is very important. Small changes can have drastic effects
var
  LNotifications: TList;
  LNotify: TIdNotify;
begin
  FEvent.WaitForEver;
  // If terminated while waiting on the event or during the loop
  while not Stopped do begin
    try
      LNotifications := FNotifications.LockList;
      try
        if LNotifications.Count = 0 then begin
          Break;
        end;
        LNotify := TIdNotify(LNotifications.Items[0]);
        LNotifications.Delete(0);
      finally
        FNotifications.UnlockList;
      end;
      try
        Synchronize(LNotify.DoNotify);
      finally
        FreeAndNil(LNotify);
      end;
    except // Catch all exceptions especially these which are raised during the application close
    end;
  end;
end;

{$ENDIF} // NotifyThreadNeeded

{ TIdNotifyMethod }

constructor TIdNotifyMethod.Create(AMethod: TThreadMethod);
begin
  inherited Create;
  FMethod := AMethod;
end;

procedure TIdNotifyMethod.DoNotify;
begin
  FMethod;
end;

{$IFDEF NotifyThreadNeeded}
initialization
finalization
  TIdNotifyThread.FreeThread;
{$ENDIF}

end.

