program RunTimeComponentsCreation;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  ABSMain;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

var
  DB: TABSDatabase;
  Table: TABSTable;

begin
  DB := TABSDatabase.Create(nil);
  Table := TABSTable.Create(nil);
  try
    DB.DatabaseName := 'db';
    DB.DatabaseFileName := ExtractFilePath(ParamStr(0)) + DataBaseFileName;
    DB.Open;
    Table.DatabaseName := DB.DatabaseName;
    Table.TableName := 'employee';
    Table.Open;
    while not Table.Eof do
      begin
        writeln(Table.FieldByName('FirstName').AsString+' '+Table.FieldByName('LastName').AsString);
        Table.Next;
      end;
    writeln;writeln('Press any key');
    readln;
  finally
    Table.Free;
    DB.Free;
  end;
end.
 