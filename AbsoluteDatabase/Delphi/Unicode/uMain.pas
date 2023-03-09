unit uMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ShellApi, StdCtrls, Grids, DBGrids, TntDBGrids, DB, ABSMain,
  TntStdCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    dbgDictionary: TTntDBGrid;
    dbDemos: TABSDatabase;
    dsDictionary: TDataSource;
    edWord: TTntEdit;
    cbLanguage: TComboBox;
    btnTranslate: TButton;
    qryTranslate: TABSQuery;
    tblDictionary: TABSTable;
    Label3: TLabel;
    Label4: TLabel;
    dbgTranslation: TTntDBGrid;
    dsTranslate: TDataSource;
    Label5: TLabel;
    procedure Label2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

type

  TLanguagesNumbers = packed record
    LangFrom, LangTo: Word
  end;

{$R *.dfm}

procedure TForm1.Label2Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://tnt.ccci.org/delphi_unicode_controls/?ref=www.componentace.com',
    nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I, J: Integer;
  L: TLanguagesNumbers;
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  tblDictionary.Open;
  // fill cbLanguage combo
  for I := 0 to tblDictionary.FieldCount - 1 do
    for J := 0 to tblDictionary.FieldCount - 1 do
      if I <> J then
      begin
        L.LangFrom := I;
        L.LangTo := J;
        cbLanguage.AddItem(tblDictionary.Fields[I].FieldName + ' to ' +
          tblDictionary.Fields[J].FieldName, TObject(L));
      end;
  cbLanguage.ItemIndex := 0;
end;

procedure TForm1.btnTranslateClick(Sender: TObject);
var
  L: TLanguagesNumbers;
begin
  qryTRanslate.Close;
  L := TLanguagesNumbers(cbLanguage.Items.Objects[cbLanguage.ItemIndex]);

  // dynamically create query like
  // SELECT English, German FROM dictionary WHERE English LIKE :Word

  qryTRanslate.SQL.Text := 'SELECT ' + tblDictionary.Fields[L.LangFrom].FieldName +
    ', ' + tblDictionary.Fields[L.LangTo].FieldName +
    ' FROM Dictionary WHERE ' + tblDictionary.Fields[L.LangFrom].FieldName +
      ' LIKE ' + ':Word';

  // assign Word parameter
    qryTRanslate.ParamByName('Word').Value := '%' + edWord.Text + '%';
  qryTRanslate.Open;
end;

end.
