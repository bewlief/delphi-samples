{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Embarcadero.Firebase;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.Firebase,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type
// ===== Forward declarations =====

  Jmessaging_ProxyFirebaseMessagingService = interface;//com.embarcadero.firebase.messaging.ProxyFirebaseMessagingService
  Jmessaging_PushNotificationListener = interface;//com.embarcadero.firebase.messaging.PushNotificationListener

// ===== Interface declarations =====

  Jmessaging_ProxyFirebaseMessagingServiceClass = interface(JFirebaseMessagingServiceClass)
    ['{5D4B23EE-7D2D-4D21-BAED-38CC4795E00F}']
    {class} function _GetDEBUG: Boolean; cdecl;
    {class} procedure _SetDEBUG(Value: Boolean); cdecl;
    {class} function init: Jmessaging_ProxyFirebaseMessagingService; cdecl;
    {class} procedure setListener(listener: Jmessaging_PushNotificationListener); cdecl;
    {class} property DEBUG: Boolean read _GetDEBUG write _SetDEBUG;
  end;

  [JavaSignature('com/embarcadero/firebase/messaging/ProxyFirebaseMessagingService')]
  Jmessaging_ProxyFirebaseMessagingService = interface(JFirebaseMessagingService)
    ['{CF35DA04-7A99-45CD-91FD-BB9A333043F3}']
    procedure onMessageReceived(message: JRemoteMessage); cdecl;
    procedure onNewToken(token: JString); cdecl;
  end;
  TJmessaging_ProxyFirebaseMessagingService = class(TJavaGenericImport<Jmessaging_ProxyFirebaseMessagingServiceClass, Jmessaging_ProxyFirebaseMessagingService>) end;

  Jmessaging_PushNotificationListenerClass = interface(IJavaClass)
    ['{9DFD082F-580D-419B-8F42-3A26B0B34A21}']
  end;

  [JavaSignature('com/embarcadero/firebase/messaging/PushNotificationListener')]
  Jmessaging_PushNotificationListener = interface(IJavaInstance)
    ['{62F3E3DD-3DA9-4E88-81E6-FBAA8D909FB9}']
    procedure onNewTokenReceived(token: JString); cdecl;
    procedure onNotificationReceived(notification: JBundle); cdecl;
  end;
  TJmessaging_PushNotificationListener = class(TJavaGenericImport<Jmessaging_PushNotificationListenerClass, Jmessaging_PushNotificationListener>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.Embarcadero.Firebase.Jmessaging_ProxyFirebaseMessagingService', TypeInfo(Androidapi.JNI.Embarcadero.Firebase.Jmessaging_ProxyFirebaseMessagingService));
  TRegTypes.RegisterType('Androidapi.JNI.Embarcadero.Firebase.Jmessaging_PushNotificationListener', TypeInfo(Androidapi.JNI.Embarcadero.Firebase.Jmessaging_PushNotificationListener));
end;

initialization
  RegisterTypes;
end.


