
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
namespace DelphiPrismWebApp;

interface

uses System.Collections;

type
  TCounter = class
  private
    FCount: Int32;
    { Private Declarations }
  public
    constructor Create;
    property Count : Int32 read FCount;
    procedure Inc;
  end;

  TSessionCounter = class
  public
    constructor Create;
  end;

implementation

constructor TCounter.Create;
begin
  inherited Create;
  // TODO: Add any constructor code here
end;

procedure TCounter.Inc;
begin
  FCount := FCount + 1;
end;

{ TSessionCounter }

constructor TSessionCounter.Create;
begin
  inherited Create;

end;

end.