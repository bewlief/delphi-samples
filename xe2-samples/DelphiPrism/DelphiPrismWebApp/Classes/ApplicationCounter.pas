
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
  TApplicationCounter = class(TCounter)
  private
    FCount,
    FCountUserOn : Int32;
    FLastVisit: DateTime;
    FPages: Hashtable;

    method GetCount: Int32;
  public
    constructor Create;
    property Count : Int32 read FCount;reintroduce;
    property CountUserOn : Int32 read FCountUserOn;
    property LastVisit : DateTime read FLastVisit;
    method Inc;reintroduce;
    property Pages : Hashtable read FPages;
    procedure IncPages( Key : String );
    procedure LoginUser;
    procedure LogoffUser;
  end;

implementation

constructor TApplicationCounter.Create;
begin
  inherited Create;
  // TODO: Add any constructor code here
  FPages := Hashtable.Create;
end;

METHOD TApplicationCounter.Inc;
begin
  FCount := FCount + 1;
  FLastVisit := DateTime.Now;
end;

procedure TApplicationCounter.IncPages(Key: String);
begin

  if not FPages.ContainsKey(Key) then
     FPages.Add(Key, Int32(1))
  else
     FPages.Item[Key] := Int32(FPages.Item[Key]) + 1;

end;

procedure TApplicationCounter.LoginUser;
begin
  FCountUserOn := FCountUserOn + 1;
end;

procedure TApplicationCounter.LogoffUser;
begin
  FCountUserOn := FCountUserOn - 1;
  if FCountUserOn < 0 then
     FCountUserOn := 0;
end;

method TApplicationCounter.GetCount: Int32;
begin
  result := FCount;
end;

end.