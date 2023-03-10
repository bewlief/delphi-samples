
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit GridForm;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, DB, DBTables, ExtCtrls, StdCtrls, Buttons, GdsData,
  IBCustomDataSet, IBDatabase, IBTable;

type
  TGridViewForm = class(TStdDataForm)
    DBGrid1: TDBGrid;
    procedure DBGrid1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GridViewForm: TGridViewForm;

implementation

uses RecForm;

{$R *.dfm}

procedure TGridViewForm.DBGrid1DblClick(Sender: TObject);
begin
  inherited;
  RecViewForm.Show;
end;

end.
