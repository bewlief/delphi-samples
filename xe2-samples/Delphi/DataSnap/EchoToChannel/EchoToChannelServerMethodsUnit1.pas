
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit EchoToChannelServerMethodsUnit1;

interface

uses
  SysUtils, Classes, DSServer;

type
{$METHODINFO ON}
  TServerMethods1 = class(TComponent)
  private
    FDSServer: TDSServer;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(ADSServer: TDSServer); reintroduce;
    function EchoStringToChannel(const AChannelName, AValue: string): Boolean;
  end;
{$METHODINFO OFF}

implementation


uses StrUtils, DBXJson;

constructor TServerMethods1.Create(ADSServer: TDSServer);
begin
  FDSServer := ADSServer;
end;

function TServerMethods1.EchoStringToChannel(const AChannelName, AValue: string): Boolean;
begin
  Result := FDSServer.BroadcastMessage(AChannelName,
    TJSonString.Create(AValue));
end;

end.

