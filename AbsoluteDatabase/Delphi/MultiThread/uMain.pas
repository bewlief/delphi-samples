unit uMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, ABSMain, Grids, DBGrids;

type
  TMainForm = class(TForm)
    DBGrid1: TDBGrid;
    dsChat: TDataSource;
    dbDemos: TABSDatabase;
    tblChat: TABSTable;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TChatMemberThread = class(TThread)
  private
    FName: string;
    FSession: TABSSession;
    FDatabase: TABSDatabase;
    FTable: TABSTable;
    procedure RefreshTable;
  protected
    procedure Execute; override;
  public
    constructor Create(Name: string);
    destructor Destroy; override;
  end;

  const ChatMessages: array[0..9] of string = ('Hi', 'Hello', 'I''m here', 'Banzay!',
    'Cool', 'how are you?', 'fine', 'who is here', 'I''m here', 'BzzzZzzz...');
  const ChatMembersNames: array[0..9] of string = ('Roger', 'Janet', 'Dana', 'Bill',
    'Kim', 'Bruce', 'Phil', 'Roberto', 'Michael', 'Claudia');
  const  DataBaseFileName: String = '..\..\Data\Demos.abs';

var
  MainForm: TMainForm;

implementation

{$IFDEF D6H}
uses StrUtils;
{$ELSE}
function RandomFrom(const AValues: array of string): string;
begin
  Result := AValues[Random(High(AValues) + 1)];
end;
{$ENDIF}

{$R *.dfm}

{ TChatMemberThread }


constructor TChatMemberThread.Create(Name: string);
begin
  inherited Create(True);
  FName := Name;
  FreeOnTerminate := True;

  FSession := TABSSession.Create(nil);
  FSession.AutoSessionName := True;

  FDatabase := TABSDatabase.Create(nil);
  FDatabase.DatabaseName := 'db_' + Name + '_' + IntToStr(Random(1000));
  FDatabase.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  FDatabase.SessionName := FSession.SessionName;
  FDatabase.MultiUser := True;
  FDatabase.Open;

  FTable := TABSTable.Create(nil);
  FTable.SessionName := FSession.SessionName;
  FTable.DatabaseName := FDatabase.DatabaseName;
  FTable.TableName := 'Chat';
  FTable.Open;
end;

destructor TChatMemberThread.Destroy;
begin
  FTable.Free;
  FDatabase.Free;
  FSession.Free;
  inherited;
end;

procedure TChatMemberThread.Execute;
var
  I: Integer;
begin
  for I := 0 to 20 do
  begin
    if Terminated then Exit;
    if MainForm.tblChat.Active and MainForm.tblChat.Exists then
    begin
      FTable.AppendRecord([FName, RandomFrom(ChatMessages)]);
      { refreshing must be done in a synchronzied method since it will modify
        the contents of the grid and the grid can only be modified from the main
        VCL thread }
      Synchronize(RefreshTable);
      Sleep(Random(100));
    end;
  end;
end;



procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  tblChat.CreateTable;
  tblChat.Open;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  tblChat.DeleteTable;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 9 do
    with TChatMemberThread.Create(ChatMembersNames[I]) do
      begin
        Priority := tpLower;
        Resume;
      end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  tblChat.Close;
  tblChat.EmptyTable;
  tblChat.Open;
end;

procedure TChatMemberThread.RefreshTable;
begin
  MainForm.tblChat.Refresh;
end;

end.
