{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.WebUI;

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
  Winapi.ApplicationModel, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.UI.WebUI.ActivatedEventHandler
  ActivatedEventHandler = interface;
  PActivatedEventHandler = ^ActivatedEventHandler;

  // Windows.UI.WebUI.BackgroundActivatedEventHandler
  BackgroundActivatedEventHandler = interface;
  PBackgroundActivatedEventHandler = ^BackgroundActivatedEventHandler;

  // Windows.UI.WebUI.EnteredBackgroundEventHandler
  EnteredBackgroundEventHandler = interface;
  PEnteredBackgroundEventHandler = ^EnteredBackgroundEventHandler;

  // Windows.UI.WebUI.SuspendingEventHandler
  SuspendingEventHandler = interface;
  PSuspendingEventHandler = ^SuspendingEventHandler;

  // Windows.UI.WebUI.ResumingEventHandler
  ResumingEventHandler = interface;
  PResumingEventHandler = ^ResumingEventHandler;

  // Windows.UI.WebUI.LeavingBackgroundEventHandler
  LeavingBackgroundEventHandler = interface;
  PLeavingBackgroundEventHandler = ^LeavingBackgroundEventHandler;

  // Windows.UI.WebUI Enums

  // Windows.UI.WebUI.PrintContent
  PrintContent = (
    AllPages = 0,
    CurrentPage = 1,
    CustomPageRange = 2,
    CurrentSelection = 3
  );
  PPrintContent = ^PrintContent;

  // Windows.UI.WebUI Interfaces

  // Windows.UI.WebUI.ActivatedEventHandler
  ActivatedEventHandler = interface(IUnknown)
  ['{50F1E730-C5D1-4B6B-9ADB-8A11756BE29C}']
    procedure Invoke(sender: IInspectable; eventArgs: Activation_IActivatedEventArgs); safecall;
  end;

  // Windows.UI.WebUI.BackgroundActivatedEventHandler
  BackgroundActivatedEventHandler = interface(IUnknown)
  ['{EDB19FBB-0761-47CC-9A77-24D7072965CA}']
    procedure Invoke(sender: IInspectable; eventArgs: Activation_IBackgroundActivatedEventArgs); safecall;
  end;

  // Windows.UI.WebUI.EnteredBackgroundEventHandler
  EnteredBackgroundEventHandler = interface(IUnknown)
  ['{2B09A173-B68E-4DEF-88C1-8DE84E5AAB2F}']
    procedure Invoke(sender: IInspectable; e: IEnteredBackgroundEventArgs); safecall;
  end;

  // Windows.UI.WebUI.SuspendingEventHandler
  SuspendingEventHandler = interface(IUnknown)
  ['{509C429C-78E2-4883-ABC8-8960DCDE1B5C}']
    procedure Invoke(sender: IInspectable; e: ISuspendingEventArgs); safecall;
  end;

  // Windows.UI.WebUI.ResumingEventHandler
  ResumingEventHandler = interface(IUnknown)
  ['{26599BA9-A22D-4806-A728-ACADC1D075FA}']
    procedure Invoke(sender: IInspectable); safecall;
  end;

  // Windows.UI.WebUI.LeavingBackgroundEventHandler
  LeavingBackgroundEventHandler = interface(IUnknown)
  ['{00B4CCD9-7A9C-4B6B-9AC4-13474F268BC4}']
    procedure Invoke(sender: IInspectable; e: ILeavingBackgroundEventArgs); safecall;
  end;

implementation

end.
