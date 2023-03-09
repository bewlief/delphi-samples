unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D5H}
HTTPProd,
{$ENDIF}
  Windows, SysUtils, Classes, HTTPApp,
  DB, ABSMain, ABSExcept,
  DSProd, DBWeb;

const DataBaseFileName = 'Demos.abs';

type
  TWebModule1 = class(TWebModule)
    PageProducer1: TPageProducer;
    dbDemos: TABSDatabase;
    Query: TABSQuery;
    DataSetTableProducer1: TDataSetTableProducer;
    Session: TABSSession;
    procedure WebModuleCreate(Sender: TObject);
    procedure PageProducer1HTMLTag(Sender: TObject; Tag: TTag;
      const TagString: String; TagParams: TStrings;
      var ReplaceText: String);
    procedure WebModuleDestroy(Sender: TObject);
    procedure DataSetTableProducer1FormatCell(Sender: TObject; CellRow,
      CellColumn: Integer; var BgColor: THTMLBgColor;
      var Align: THTMLAlign; var VAlign: THTMLVAlign; var CustomAttrs,
      CellData: String);
  private
    ErrorMsg: String;
  end;

var
  WebModule1: TWebModule1;
  DllFileName: String;

implementation

{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  ErrorMsg := '';
  dbDemos.Close;
  try
    dbDemos.DatabaseFileName := ExtractFilePath(DllFileName) + DataBaseFileName;
    dbDemos.Open;
  except
    on e: Exception do ErrorMsg := e.Message;
  end;
end;


procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  Query.Close;
  dbDemos.Close;
end;


procedure TWebModule1.PageProducer1HTMLTag(Sender: TObject; Tag: TTag;
  const TagString: String; TagParams: TStrings; var ReplaceText: String);
var
  sl: TStringList;
  i: Integer;
  sql: String;
begin
  if TagString = 'ERROR' then
   begin
    if ErrorMsg <> '' then
      ReplaceText := '<H2><font color=red>Error openign Database: ''' + dbDemos.DatabaseFileName +
                     ''' Message: ' + ErrorMsg + '</font></H2>';
   end;

  if TagString = 'VERSION' then
   begin
    ReplaceText := dbDemos.CurrentVersion;
   end;

  if TagString = 'FILENAME' then
   begin
    ReplaceText := dbDemos.DatabaseFileName;
   end;

  if TagString = 'TABLES' then
   begin
    sl := TStringList.Create;
    try
      dbDemos.GetTablesList(sl);
      ReplaceText := '';
      for i:=0 to sl.Count-1 do
       ReplaceText := ReplaceText + '<li>' + sl[i] + #13#10;
    finally
      sl.Free;
    end;
   end;

  if TagString = 'SQL' then
   begin
    sql := Request.ContentFields.Values['sql'];
    if sql = '' then
     begin
      sl := TStringList.Create;
      try
        dbDemos.GetTablesList(sl);
        sql := 'select * from ' + sl[0];
      finally
        sl.Free;
      end;
      Request.ContentFields.Values['sql'] := sql;
     end;
    ReplaceText := sql;
   end;

  if TagString = 'RESULT_TABLE' then
   begin
    Query.SQL.Text := Request.ContentFields.Values['sql'];
    try
      DataSetTableProducer1.Columns.Clear;
      Query.Open;
      ReplaceText := Format('RecordCount = <b>%d</b><br>'#13#10, [Query.RecordCount]) +
                     DataSetTableProducer1.Content;
      Query.Close;
    except
     on e: EABSException do
      begin
       if (e.NativeError = 20001) then
        begin
         // It is ExecSQL. All OK.
         ReplaceText := Format('<b>%d</b> Rows Affected <br>'#13#10 ,[Query.RowsAffected]); 
        end
       else
        begin
         Query.Close;
         ReplaceText := '<H2><font color=red>' + e.Message + '</font></H2>';
        end;
      end;
     on e: Exception do
      begin
       Query.Close;
       ReplaceText := '<H2><font color=red>' + e.Message + '</font></H2>';
      end;
    end;

   end;
end;


procedure TWebModule1.DataSetTableProducer1FormatCell(Sender: TObject;
  CellRow, CellColumn: Integer; var BgColor: THTMLBgColor;
  var Align: THTMLAlign; var VAlign: THTMLVAlign; var CustomAttrs,
  CellData: String);
begin
  if CellRow = 0 then BgColor := '#BBBBBB'
  else if CellRow mod 2 = 0 then BgColor := '#DDDDDD';
end;




initialization
  SetLength(DllFileName, 260);
  GetModuleFileName(HInstance, PChar(DllFileName), 260);

end.
