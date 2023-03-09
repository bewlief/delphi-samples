
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fmx.customcontrol;

interface

uses
  System.Types, System.SysUtils, System.Classes, FMX.Types;

type
  TMyControl = class(TStyledControl)
    function GetStyleObject: TControl; override;
  end;

procedure Register;

implementation

{$IFDEF MACOS}
{$R *.mac.res}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R *.win.res}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Samples', [TMyControl]);
end;

{ TMyControl }

function TMyControl.GetStyleObject: TControl;
var
  S: TResourceStream;
begin
  Result := nil;
  if (FStyleLookup = '') then
  begin
    if FindRCData(HInstance, 'mycontrolstyle') then
    begin
      S := TResourceStream.Create(HInstance, 'mycontrolstyle', RT_RCDATA);
      Result := TControl(CreateObjectFromStream(nil, S));
      S.Free;
    end;
  end
  else
    Result := inherited GetStyleObject;
end;

end.
