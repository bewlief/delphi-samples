{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Messaging;

{$MINENUMSIZE 4}
{$H+}

interface

uses
{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
{$ENDIF ANDROID}
{$IFDEF IOS}
  iOSapi.UIKit,
{$ENDIF IOS}
  System.SysUtils, System.Generics.Collections;

type

  /// <summary>Base class for all messages</summary>
  TMessageBase = class abstract;
  TMessage = TMessageBase;
  {$NODEFINE TMessage} // Avoid ambiguity with 'Winapi.Messages.TMessage'

  TMessage<T> = class (TMessage)
  protected
    FValue: T;
  public
    constructor Create(const AValue: T);
    destructor Destroy; override;
    property Value: T read FValue;
  end;

  TObjectMessage<T: class> = class(TMessage<T>)
  protected
    FOwnsObject: Boolean;
  public
    constructor Create(const AValue: T; AOwnsObject: Boolean = True);
    destructor Destroy; override;
  end;

{$IFDEF IOS}
  /// <summary>Received Notification type for NotificationCenter.</summary>
  TMessageReceivedNotification = class(TMessage<UILocalNotification>);
{$ENDIF IOS}

{$IFDEF ANDROID}
  /// <summary>Received Notification type for NotificationCenter.</summary>
  TMessageReceivedNotification = class(TMessage<JIntent>);
{$ENDIF ANDROID}

  TMessageListener = reference to procedure(const Sender: TObject; const M: TMessage);
  TMessageListenerMethod = procedure (const Sender: TObject; const M: TMessage) of object;

  { TMessageManager can have many independent instances, but it
    maintains one global instance accessible by TMessageManager.DefaultManager }
  TMessageManager = class
  protected
  type
    TListenerWithId = record
      Id: Integer;
      Listener: TMessageListener;
      ListenerMethod: TMessageListenerMethod;
    end;
    PListenerWithId = ^TListenerWithId;
    TListenerList = class(TList<TListenerWithId>)
    strict private
      FProcessing: Integer;

      procedure IterateAndSend(const Sender: TObject; const AMessage: TMessage);
      procedure Compact;
    private
      FRemoveCount: Integer;

      procedure Unsubscribe(Index: Integer; Immediate: Boolean); inline;
      procedure SendMessage(const Sender: TObject; const AMessage: TMessage); inline;
      class procedure InternalCopyListener(FromListener, ToListener: PListenerWithId); inline;
    end;
    TListenerRegistry = TObjectDictionary<TClass, TListenerList>;
  private
    FListeners: TListenerRegistry;
    FLastId: Integer;
    procedure RegisterMessageClass(const AMessageClass: TClass);

    { Global instance }
    class var FDefaultManager: TMessageManager;
    class function GetDefaultManager: TMessageManager; static;
    class function SearchListener(const ArrayToSearch: array of TListenerWithId; Id: Integer; AMinValue, AMaxValue: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor UnInitialize;
    function SubscribeToMessage(const AMessageClass: TClass; const AListener: TMessageListener): Integer; overload;
    function SubscribeToMessage(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer; overload;
    procedure Unsubscribe(const AMessageClass: TClass; Id: Integer; Immediate: Boolean = False); overload;
    procedure Unsubscribe(const AMessageClass: TClass; const AListener: TMessageListener; Immediate: Boolean = False); overload;
    procedure Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
    procedure SendMessage(const Sender: TObject; AMessage: TMessage); overload; inline;
    procedure SendMessage(const Sender: TObject; AMessage: TMessage; ADispose: Boolean); overload;
    class property DefaultManager: TMessageManager read GetDefaultManager;
  end;

implementation

uses System.Types, System.RTLConsts;

type
  TFixedMessageManager = class(TMessageManager)
  protected
  type
    TListenerData = class
      Id: Integer;
      Listener: TMessageListener;
      ListenerMethod: TMessageListenerMethod;
      MarkedAsRemoved: Boolean;
      constructor Create(const AId: Integer; const AListenerMethod: TMessageListenerMethod); overload;
      constructor Create(const AId: Integer; const AListener: TMessageListener); overload;
      procedure MarkAsRemoved; inline;
    end;

    TListenerList = class
    private
      FListeners: TObjectList<TListenerData>;
      FIndicies1: TDictionary<Integer, TListenerData>;
      FIndicies2: TDictionary<TMessageListener, TListenerData>;
      FIndicies3: TDictionary<TMessageListenerMethod, TListenerData>;
      FProcessing: Integer;
      FRemoveCount: Integer;
      FRemoveThreshold: Integer;
      procedure IterateAndSend(const Sender: TObject; const AMessage: TMessage);
      procedure RemoveDelayed;
      procedure MarkAsRemoved(const AListener: TListenerData); inline;
      procedure Unsubscribe(const AListener: TListenerData); overload;
      procedure RecalculateRemoveThreshold; inline;
    public
      constructor Create;
      destructor Destroy; override;
      function Subscribe(const AId: Integer; const AListener: TMessageListener): Integer; overload;
      function Subscribe(const AId: Integer; const AListenerMethod: TMessageListenerMethod): Integer; overload;
      procedure SendMessage(const Sender: TObject; const AMessage: TMessage); inline;
      procedure Unsubscribe(const AId: Integer); overload;
      procedure Unsubscribe(const AListener: TMessageListener); overload;
      procedure Unsubscribe(const AListenerMethod: TMessageListenerMethod); overload;
    end;
    TListenerRegistry = TObjectDictionary<TClass, TListenerList>;
  private
    FListeners: TListenerRegistry;
    FLastId: Cardinal;
    procedure RegisterMessageClass(const AMessageClass: TClass; var AListeners: TListenerList);
  public
    constructor Create;
    destructor Destroy; override;
    function SubscribeToMessage(const AMessageClass: TClass; const AListener: TMessageListener): Integer; overload;
    function SubscribeToMessage(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer; overload;
    procedure Unsubscribe(const AMessageClass: TClass; Id: Integer; Immediate: Boolean = False); overload;
    procedure Unsubscribe(const AMessageClass: TClass; const AListener: TMessageListener; Immediate: Boolean = False); overload;
    procedure Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
    procedure SendMessage(const Sender: TObject; AMessage: TMessage); overload; inline;
    procedure SendMessage(const Sender: TObject; AMessage: TMessage; ADispose: Boolean); overload;
  end;

{ TFixedMessageManager }

constructor TFixedMessageManager.Create;
begin
  FListeners := TListenerRegistry.Create([doOwnsValues]);
  FLastId := 1;
end;

destructor TFixedMessageManager.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

procedure TFixedMessageManager.RegisterMessageClass(const AMessageClass: TClass; var AListeners: TListenerList);
begin
  if not FListeners.TryGetValue(AMessageClass, AListeners) then
  begin
    AListeners := TListenerList.Create;
    FListeners.Add(AMessageClass, AListeners);
  end;
end;

function TFixedMessageManager.SubscribeToMessage(const AMessageClass: TClass; const AListener: TMessageListener) : Integer;
var
  Subscribers: TListenerList;
begin
  RegisterMessageClass(AMessageClass, Subscribers);
  Inc(FLastId);
  Result := Subscribers.Subscribe(FLastId, AListener);
end;

function TFixedMessageManager.SubscribeToMessage(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer;
var
  Subscribers: TListenerList;
begin
  RegisterMessageClass(AMessageClass, Subscribers);
  Inc(FLastId);
  Result := Subscribers.Subscribe(FLastId, AListenerMethod);
end;

procedure TFixedMessageManager.Unsubscribe(const AMessageClass: TClass; const AListener: TMessageListener; Immediate: Boolean);
var
  Subscribers: TListenerList;
begin
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
    Subscribers.Unsubscribe(AListener);
end;

procedure TFixedMessageManager.Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean);
var
  Subscribers: TListenerList;
begin
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
    Subscribers.Unsubscribe(AListenerMethod);
end;

procedure TFixedMessageManager.Unsubscribe(const AMessageClass: TClass; Id: Integer; Immediate: Boolean);
var
  Subscribers: TListenerList;
begin
  if FListeners.TryGetValue(AMessageClass, Subscribers) then
    Subscribers.Unsubscribe(Id);
end;

procedure TFixedMessageManager.SendMessage(const Sender: TObject; AMessage: TMessage; ADispose: Boolean);
var
  Subscribers: TListenerList;
begin
  if AMessage = nil then
    raise Exception.CreateRes(@SArgumentInvalid);

  try
    if FListeners.TryGetValue(AMessage.ClassType, Subscribers) then
      Subscribers.SendMessage(Sender, AMessage);
  finally
    if ADispose then
      AMessage.Free;
  end;
end;

procedure TFixedMessageManager.SendMessage(const Sender: TObject; AMessage: TMessage);
begin
  SendMessage(Sender, AMessage, True);
end;

{ TFixedMessageManager.TListenerList }

constructor TFixedMessageManager.TListenerList.Create;
begin
  FListeners := TObjectList<TListenerData>.Create;
  FIndicies1 := TDictionary<Integer, TListenerData>.Create;
  FIndicies2 := TDictionary<TMessageListener, TListenerData>.Create;
  FIndicies3 := TDictionary<TMessageListenerMethod, TListenerData>.Create;
  FRemoveThreshold := 100;
end;

destructor TFixedMessageManager.TListenerList.Destroy;
begin
  FreeAndNil(FListeners);
  FreeAndNil(FIndicies3);
  FreeAndNil(FIndicies2);
  FreeAndNil(FIndicies1);
  inherited;
end;

procedure TFixedMessageManager.TListenerList.IterateAndSend(const Sender: TObject; const AMessage: TMessage);
var
  Listener: TListenerData;
  List: TList<TListenerData>.arrayofT;
begin
  List := FListeners.List; // Saving List to local variable increase performance. It saves ~10%
  for var I := 0 to FListeners.Count - 1 do
  begin
    Listener := List[I];
    // We don't c check MarkedAsRemoved since we reset all references on listener methods and anonymous procedures.
    // Additional check MarkedAsRemoved reduces performance on ~5%.
    if Assigned(Listener.ListenerMethod) then
      Listener.ListenerMethod(Sender, AMessage)
    else if Assigned(Listener.Listener) then
      Listener.Listener(Sender, AMessage);
  end;
end;

procedure TFixedMessageManager.TListenerList.MarkAsRemoved(const AListener: TListenerData);
begin
  AListener.MarkAsRemoved;
  Inc(FRemoveCount);
end;

procedure TFixedMessageManager.TListenerList.SendMessage(const Sender: TObject; const AMessage: TMessage);
begin
  Inc(FProcessing);
  try
    IterateAndSend(Sender, AMessage);
  finally
    Dec(FProcessing);
  end;

  if (FProcessing = 0) and (FRemoveCount >= FRemoveThreshold) then
    RemoveDelayed;
end;

procedure TFixedMessageManager.TListenerList.RecalculateRemoveThreshold;
begin
  FRemoveThreshold := FIndicies1.Count div 100 * 15 + 1; // Minimum value is 1, so +1
end;

function TFixedMessageManager.TListenerList.Subscribe(const AId: Integer; const AListenerMethod: TMessageListenerMethod): Integer;
var
  L: TListenerData;
begin
  L := TListenerData.Create(AId, AListenerMethod);
  Result := L.Id;

  FListeners.Add(L);
  FIndicies1.Add(AId, L);
  FIndicies3.Add(AListenerMethod, L);
  RecalculateRemoveThreshold;
end;

procedure TFixedMessageManager.TListenerList.Unsubscribe(const AId: Integer);
var
  L: TListenerData;
begin
  if FIndicies1.TryGetValue(AId, L) then
    Unsubscribe(L);
end;

procedure TFixedMessageManager.TListenerList.Unsubscribe(const AListenerMethod: TMessageListenerMethod);
var
  L: TListenerData;
begin
  if FIndicies3.TryGetValue(AListenerMethod, L) then
    Unsubscribe(L);
end;

procedure TFixedMessageManager.TListenerList.Unsubscribe(const AListener: TListenerData);
begin
  FIndicies1.Remove(AListener.id);
  FIndicies2.Remove(AListener.Listener);
  FIndicies3.Remove(AListener.ListenerMethod);

  MarkAsRemoved(AListener);

  if (FProcessing = 0) and (FRemoveCount >= FRemoveThreshold) then
    RemoveDelayed;
end;

procedure TFixedMessageManager.TListenerList.Unsubscribe(const AListener: TMessageListener);
var
  L: TListenerData;
begin
  if FIndicies2.TryGetValue(AListener, L) then
    Unsubscribe(L);
end;

function TFixedMessageManager.TListenerList.Subscribe(const AId: Integer; const AListener: TMessageListener): Integer;
var
  L: TListenerData;
begin
  L := TListenerData.Create(AId, AListener);
  Result := L.Id;

  FListeners.Add(L);
  FIndicies1.Add(AId, L);
  FIndicies2.Add(AListener, L);
  RecalculateRemoveThreshold;
end;

procedure TFixedMessageManager.TListenerList.RemoveDelayed;
begin
  for var I := FListeners.Count - 1 downto 0 do
    if FListeners[I].MarkedAsRemoved then
      FListeners.Delete(I);

  FRemoveCount := 0;
end;

{ TFixedMessageManager.TListenerData }

constructor TFixedMessageManager.TListenerData.Create(const AId: Integer; const AListener: TMessageListener);
begin
  Id := AId;
  Listener := AListener;
end;

constructor TFixedMessageManager.TListenerData.Create(const AId: Integer; const AListenerMethod: TMessageListenerMethod);
begin
  Id := AId;
  ListenerMethod := AListenerMethod;
end;

procedure TFixedMessageManager.TListenerData.MarkAsRemoved;
begin
  Id := 0;
  Listener := nil;
  ListenerMethod := nil;
  MarkedAsRemoved := True;
end;

{ TMessageManager }

constructor TMessageManager.Create;
begin
  FListeners := TListenerRegistry.Create([doOwnsValues]);
  FLastId := 1;
end;

destructor TMessageManager.Destroy;
begin
  FListeners.Free;
  inherited;
end;

class function TMessageManager.GetDefaultManager: TMessageManager;
begin
  if FDefaultManager = nil then
    FDefaultManager := TFixedMessageManager.Create;

  Result := FDefaultManager;
end;

class destructor TMessageManager.UnInitialize;
begin
  FreeAndNil(FDefaultManager);
end;

procedure TMessageManager.RegisterMessageClass(const AMessageClass: TClass);
begin
  if not FListeners.ContainsKey(AMessageClass) then
    FListeners.Add(AMessageClass, TListenerList.Create);
end;

function TMessageManager.SubscribeToMessage(const AMessageClass: TClass; const AListener: TMessageListener) : Integer;
var
  L: TListenerWithId;
  Subscribers: TListenerList;
begin
  if ClassType = TFixedMessageManager then
    Result := TFixedMessageManager(Self).SubscribeToMessage(AMessageClass, AListener)
  else
  begin
    Result := -1;
    RegisterMessageClass(AMessageClass);
    if FListeners.TryGetValue(AMessageClass, Subscribers) then
    begin
      L.Listener := AListener;
      L.ListenerMethod := nil;
      Inc(FLastId);
      L.Id := FLastId;
      Result := L.Id;
      Subscribers.Add(L);
    end;
  end;
end;

function TMessageManager.SubscribeToMessage(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer;
var
  L: TListenerWithId;
  Subscribers: TListenerList;
begin
  if ClassType = TFixedMessageManager then
    Result := TFixedMessageManager(Self).SubscribeToMessage(AMessageClass, AListenerMethod)
  else
  begin
    Result := -1;
    RegisterMessageClass(AMessageClass);
    if FListeners.TryGetValue(AMessageClass, Subscribers) then
    begin
      L.Listener := nil;
      L.ListenerMethod := AListenerMethod;
      Inc(FLastId);
      L.Id := FLastId;
      Result := L.Id;
      Subscribers.Add(L);
    end;
  end;
end;

procedure TMessageManager.Unsubscribe(const AMessageClass: TClass; const AListener: TMessageListener; Immediate: Boolean);
var
  Subscribers: TListenerList;
  I: Integer;
begin
  if ClassType = TFixedMessageManager then
    TFixedMessageManager(Self).Unsubscribe(AMessageClass, AListener, Immediate)
  else
    if FListeners.TryGetValue(AMessageClass, Subscribers) then
      for I := 0 to Subscribers.Count - 1 do
        if Pointer((@Subscribers.List[I].Listener)^) = Pointer((@AListener)^) then
        begin
          Subscribers.Unsubscribe(I,Immediate);
          Break;
        end;
end;

procedure TMessageManager.Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean);
var
  Subscribers: TListenerList;
  I: Integer;
begin
  if ClassType = TFixedMessageManager then
    TFixedMessageManager(Self).Unsubscribe(AMessageClass, AListenerMethod, Immediate)
  else
    if FListeners.TryGetValue(AMessageClass, Subscribers) then
      for I := 0 to Subscribers.Count - 1 do
        if TMethod(Subscribers[I].ListenerMethod) = TMethod(AListenerMethod) then
        begin
          Subscribers.Unsubscribe(I,Immediate);
          break;
        end;
end;

procedure TMessageManager.Unsubscribe(const AMessageClass: TClass; Id: Integer; Immediate: Boolean);
var
  Index: Integer;
  Subscribers: TListenerList;
begin
  if ClassType = TFixedMessageManager then
    TFixedMessageManager(Self).Unsubscribe(AMessageClass, Id, Immediate)
  else
    if FListeners.TryGetValue(AMessageClass, Subscribers) then
    begin
      Index := SearchListener(Subscribers.List, Id, 0, Subscribers.Count - 1);
      if Index >= 0 then
        Subscribers.Unsubscribe(Index, Immediate);
    end;
end;

procedure TMessageManager.SendMessage(const Sender: TObject; AMessage: TMessage; ADispose: Boolean);
var
  Subscribers: TListenerList;
begin
  if ClassType = TFixedMessageManager then
    TFixedMessageManager(Self).SendMessage(Sender, AMessage, ADispose)
  else
    if AMessage <> nil then
      try
        if FListeners.TryGetValue(AMessage.ClassType, Subscribers) then
          Subscribers.SendMessage(Sender, AMessage);
      finally
        if ADispose then
          AMessage.Free;
      end
    else
      raise Exception.CreateRes(@SArgumentInvalid);
end;

procedure TMessageManager.SendMessage(const Sender: TObject; AMessage: TMessage);
begin
  SendMessage(Sender, AMessage, True);
end;

class function TMessageManager.SearchListener(const ArrayToSearch: array of TListenerWithId; Id: Integer; AMinValue, AMaxValue: Integer): Integer;
var
  IMin, IMid, IMax: Integer;
begin
  if (AMaxValue < AMinValue) then
    Exit(-1);
  IMin := AMinValue;
  IMax := AMaxValue;

  while IMax >= IMin do
  begin
    IMid := (IMax + IMin) shr 1;
    if ArrayToSearch[IMid].Id < Id then
    begin
      IMin := IMid + 1;
    end
    else
      if ArrayToSearch[IMid].Id > Id then
        IMax := IMid - 1
      else
        Exit(IMid);
  end;
  Result := -1;
end;

{ TMessage<T> }

constructor TMessage<T>.Create(const AValue: T);
begin
  FValue := AValue;
end;

destructor TMessage<T>.Destroy;
begin
  inherited;  {C++: Anchor Generic destructor}
end;

{ TObjectMessage<T> }

constructor TObjectMessage<T>.Create(const AValue: T; AOwnsObject: Boolean);
begin
  inherited Create(AValue);
  FOwnsObject := AOwnsObject;
end;

destructor TObjectMessage<T>.Destroy;
begin
  if FOwnsObject then
    FValue.DisposeOf;
  inherited Destroy;
end;

{ TMessageManager.TListenerList }

class procedure TMessageManager.TListenerList.InternalCopyListener(FromListener, ToListener: PListenerWithId);
begin
  ToListener.Id := FromListener.Id;
  ToListener.Listener := FromListener.Listener;
  ToListener.ListenerMethod := FromListener.ListenerMethod;
end;

procedure TMessageManager.TListenerList.IterateAndSend(const Sender: TObject;
  const AMessage: TMessage);
var
  I: Integer;
  Listener: PListenerWithId;
begin
  for I := 0 to Count - 1 do
  begin
    Listener := @List[I];
    if Assigned(Listener.Listener) then
      Listener.Listener(Sender, AMessage)
    else
      if Assigned(Listener.ListenerMethod) then
        TMessageListenerMethod(Listener.ListenerMethod)(Sender, AMessage);
  end;
end;

procedure TMessageManager.TListenerList.SendMessage(const Sender: TObject;
  const AMessage: TMessage);
begin
  if (FProcessing = 0) and (FRemoveCount > 0) and (((FRemoveCount * 100) div Count) > 10) then
    Compact;
  Inc(FProcessing);
  try
    IterateAndSend(Sender, AMessage);
  finally
    Dec(FProcessing);
  end;
end;

procedure TMessageManager.TListenerList.Unsubscribe(Index: Integer;
  Immediate: Boolean);
begin
  if FProcessing > 0 then
  begin
    // Recursive call, no compacting should be performed
    List[Index].Listener := nil;
    List[Index].ListenerMethod := nil;
    Inc(FRemoveCount);
  end
  else
  begin
    if Immediate then
      Delete(Index)
    else
    begin
      List[Index].Listener := nil;
      List[Index].ListenerMethod := nil;
      Inc(FRemoveCount);
      if (FRemoveCount shl 1) > (Count + 4) then
        Compact;
    end;
  end;
end;

procedure TMessageManager.TListenerList.Compact;
var
  I, N: Integer;
  Listener: PListenerWithId;
begin
  N := 0;
  FRemoveCount := 0;
  for I := 0 to Count - 1 do
  begin
    Listener := @List[I];
    if Assigned(Listener.Listener) or Assigned(Listener.ListenerMethod) then
    begin
      if N <> I then
        InternalCopyListener(Listener, @List[N]);
      Inc(N);
    end;
  end;
  Count := N;
end;

end.

