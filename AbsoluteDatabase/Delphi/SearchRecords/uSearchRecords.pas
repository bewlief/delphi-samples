unit uSearchRecords;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, ABSMain, ComCtrls, ExtCtrls,
  Buttons;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    dbDemos: TABSDatabase;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Table: TABSTable;
    Button2: TButton;
    rgSMethod: TRadioGroup;
    gbLocateOptions: TGroupBox;
    cbPartKey: TCheckBox;
    cbCaseInSens: TCheckBox;
    Edit1: TEdit;
    GroupBox3: TGroupBox;
    mLog: TMemo;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure rgSMethodClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  index:string;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
 dbDemos.Open;
 Table.TableName := 'employee';
 Table.Open;
 mLog.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
 v: variant;
 info: string;
 Options: TLocateOptions;
begin
 info := '';
 mLog.Lines.Add(DateTimeToStr(Now));
 case rgSMethod.ItemIndex of
  0: begin
      Table.IndexName := 'ByFirstName';
      mLog.Lines.Add('< FindKey method >.  Search text: "' + Edit1.Text +'".');
      if (not Table.FindKey([Edit1.Text])) then
       mLog.Lines.Add(' Result: No records found.')
      else
       mLog.Lines.Add(' Result: Record found.');
      Table.IndexName := '';
     end;
  1: begin
      v := Table.Lookup('FirstName',Edit1.Text,'FirstName;LastName;PhoneExt');
      mLog.Lines.Add('< Lookup method >.  Search text: "' + Edit1.Text +'".');
      if (not VarIsNull(v)) then
       begin
        mLog.Lines.Add(' Result: Record found.');
        info := ' Info: FirstName is '+QuotedStr(String(v[0]));

        if (not VarIsNull(v[1])) then
         info := info + ', LastName is '+QuotedStr(String(v[1]))
        else
         info := info + ', LastName is '+QuotedStr('Null');

        if (not VarIsNull(v[2])) then
         info := info + ', PhoneExt is '+QuotedStr(String(v[2]))
        else
         info := info + ', PhoneExt is '+QuotedStr('Null');
        mLog.Lines.Add(info);
       end
      else
       mLog.Lines.Add('  Result: No records found.');
     end;
  2: begin
      mLog.Lines.Add('< Locate method >.  Search text: "' + Edit1.Text +'".');
      Options := [];
      if cbCaseInSens.Checked then
       begin
        Options := Options + [loCaseInsensitive];
        info := ' Options: CaseInSensitive = true'
       end
      else
       info := ' Options: CaseInSensitive = false';
      if cbPartKey.Checked then
       begin
        Options := Options + [loPartialKey];
        info := info + ', PartialKey = true.'
       end
      else
       info := info + ', PartialKey = false.';
      mLog.Lines.Add(info);
      if not Table.Locate('FirstName',Edit1.Text,Options) then
       mLog.Lines.Add(' Result: No records found.')
      else
       mLog.Lines.Add(' Result: Record found.');
     end;
 end;
 mLog.Lines.Add(' ')
end;

procedure TForm1.rgSMethodClick(Sender: TObject);
begin
 if rgSMethod.ItemIndex = 2 then
  begin
   cbCaseInSens.Enabled := true;
   cbPartKey.Enabled := true;
  end
 else
  begin
   cbCaseInSens.Enabled := false;
   cbPartKey.Enabled := false;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 mLog.Clear;
end;

end.
