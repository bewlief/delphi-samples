{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Delegate.Cocoa;

interface

uses
  Macapi.ObjectiveC,
{$IFDEF IOS}
  iOSapi.Foundation;
{$ELSE}
  Macapi.Foundation;
{$ENDIF IOS}

type
  TJavaScriptConfirmationResponseProc = reference to procedure(const AResponse: Boolean);
  TJavaScriptInformationResponseProc = reference to procedure;
  TJavaScriptInputQueryResponseProc = reference to procedure(const AResponse: NSString);
  TAuthenticationResponseProc = reference to procedure(const ACredential: NSURLCredential);

  TBaseWebViewDelegate = class(TOCLocal)
  public
    procedure AuthenticateForHost(const AHost: NSString; const AResponseProc: TAuthenticationResponseProc);
    procedure JavaScriptConfirmationMessage(const AMessage: NSString; const AResponseProc: TJavaScriptConfirmationResponseProc);
    procedure JavaScriptInformationMessage(const AMessage: NSString; const AResponseProc: TJavaScriptInformationResponseProc);
    procedure JavaScriptInputQuery(const AMessage, ADefaultText: NSString; const AResponseProc: TJavaScriptInputQueryResponseProc);
  end;

implementation

uses
  System.UITypes, System.SysUtils,
  Macapi.Helpers,
  FMX.Consts, FMX.Dialogs, FMX.DialogService.Async;

{ TBaseWebViewDelegate }

procedure TBaseWebViewDelegate.AuthenticateForHost(const AHost: NSString; const AResponseProc: TAuthenticationResponseProc);
var
  LTitle: string;
  LPrompts, LValues: TArray<string>;
  LUserName, LPassword: NSString;
  LCredential: NSURLCredential;
begin
  SetLength(LValues, 2);
  SetLength(LPrompts, 2);
  {$IF Defined(IOS)}
  LPrompts[0] := ''; // Workaround for https://quality.embarcadero.com/browse/RSP-27777
  {$ELSE}
  LPrompts[0] := SUsername;
  {$ENDIF IOS}
  LPrompts[1] := #1 + SPassword;
  LTitle := Format(SHostRequiresAuthentication, [NSStrToStr(AHost)]);
  TDialogServiceAsync.InputQuery(LTitle, LPrompts, LValues,
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOK then
      begin
        LUserName := StrToNSStr(AValues[0]);
        LPassword := StrToNSStr(AValues[1]);
        LCredential := TNSURLCredential.Wrap(TNSURLCredential.OCClass.credentialWithUser(LUserName, LPassword, NSURLCredentialPersistenceNone));
        AResponseProc(LCredential)
      end
      else
        AResponseProc(nil);
    end
  );
end;

procedure TBaseWebViewDelegate.JavaScriptConfirmationMessage(const AMessage: NSString; const AResponseProc: TJavaScriptConfirmationResponseProc);
begin
  TDialogServiceAsync.MessageDialog(NSStrToStr(AMessage), TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      AResponseProc(AResult = mrYes);
    end
  )
end;

procedure TBaseWebViewDelegate.JavaScriptInformationMessage(const AMessage: NSString; const AResponseProc: TJavaScriptInformationResponseProc);
begin
  TDialogServiceAsync.MessageDialog(NSStrToStr(AMessage), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
      AResponseProc;
    end
  );
end;

procedure TBaseWebViewDelegate.JavaScriptInputQuery(const AMessage, ADefaultText: NSString; const AResponseProc: TJavaScriptInputQueryResponseProc);
var
  LValues: TArray<string>;
begin
  SetLength(LValues, 1);
  LValues[0] := NSStrToStr(ADefaultText);
  TDialogServiceAsync.InputQuery(string.Empty, [NSStrToStr(AMessage)], LValues,
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOK then
        AResponseProc(StrToNSStr(AValues[0]))
      else
        AResponseProc(nil);
    end
  );
end;

end.
