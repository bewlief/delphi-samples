
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

uses System.Data,
     System.Configuration,
     System.Data.Common,
     Borland.Data;

type
  TDBConnection = public class
  protected
    FMyAdoDbxConnection : TAdoDbxConnection;
    method getMyAdoDbxConnection: TAdoDbxConnection;
  public
    property MyAdoDbxConnection : TAdoDbxConnection read getMyAdoDbxConnection;
  end;

implementation

method TDBConnection.getMyAdoDbxConnection: TAdoDbxConnection;
begin
  if not Assigned(FMyAdoDbxConnection) and (ConfigurationManager.AppSettings['DatabaseAlias'] <> '' ) then
  begin
    FMyAdoDbxConnection := TAdoDbxProviderFactory.Instance.CreateConnection as TAdoDbxConnection;
    FMyAdoDbxConnection.ConnectionString := 'ConnectionName=' + ConfigurationManager.AppSettings['DatabaseAlias'];
    FMyAdoDbxConnection.Open;
  end;

  Result := FMyAdoDbxConnection;

end;

end.