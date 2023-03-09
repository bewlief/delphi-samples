unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, DBGrids, Db, DBCtrls,
  ABSMain, ABSConst;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TMainForm = class(TForm)
    DataSource1: TDataSource;
    ABSTable1: TABSTable;
    dbDemos: TABSDatabase;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    DBGrid1: TDBGrid;
    Label2: TLabel;
    DBNavigator1: TDBNavigator;
    lbIsRecordLocked: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    lbConnectedUsers: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ABSTable1DeleteError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure ABSTable1EditError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure ABSTable1PostError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure Timer1Timer(Sender: TObject);
    procedure ABSTable1AfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName; !!!
  dbDemos.DatabaseFileName := DataBaseFileName;
  // enable multi-user mode
  dbDemos.MultiUser := True;
  dbDemos.Open;
  // open table
  ABSTable1.Active := true;
end;


procedure TMainForm.ABSTable1DeleteError(DataSet: TDataSet;
  E: EDatabaseError; var Action: TDataAction);
begin
   Action:=daAbort;
   if (E is EABSEngineError) then
        case (EABSEngineError(E).ErrorCode) of
          ABS_ERR_RECORD_LOCKED:
           begin
           if MessageDlg('The record is locked. '+
                         'Do you want to try to delete this record again?',
                          mtWarning,[mbYes,mbNo],0)=mrYes then
              Action:=daRetry;
           end;
          ABS_ERR_TABLE_LOCKED:
           begin
           if MessageDlg('The table is locked. '+
                         'Do you want to try to delete this record again?',
                          mtWarning,[mbYes,mbNo],0)=mrYes then
              Action:=daRetry;
           end;
          ABS_ERR_DELETE_RECORD_MODIFIED:
           begin
             MessageDlg('The record you are trying to delete has been modified by another user. '+
                        'The table will now be refreshed. If you want to delete this record, try again.',
                        mtWarning,[mbOk],0);
             DataSet.Refresh;
           end;
          ABS_ERR_DELETE_RECORD_DELETED:
           begin
             MessageDlg('The record you are trying to delete has been deleted by another user '+
                        'The table will now be refreshed',
                        mtWarning,[mbOk],0);
             DataSet.Refresh;
           end
      else
         MessageDlg(E.Message,mtError,[mbOK],0);
       end
   else
      MessageDlg(E.Message,mtError,[mbOK],0);
end;

procedure TMainForm.ABSTable1EditError(DataSet: TDataSet;
  E: EDatabaseError; var Action: TDataAction);
begin
   Action:=daAbort;
   if (E is EABSEngineError) then
        case (EABSEngineError(E).ErrorCode) of
          ABS_ERR_RECORD_LOCKED:
           begin
             if MessageDlg('The record you are trying to edit is locked. '+
                           'Do you want to try again?',
                           mtWarning,[mbYes,mbNo],0)=mrYes then
                Action:=daRetry;
           end;
          ABS_ERR_TABLE_LOCKED:
           begin
             if MessageDlg('The table you are trying to edit is locked. '+
                           'Do you want to try again?',
                           mtWarning,[mbYes,mbNo],0)=mrYes then
                Action:=daRetry;
           end;
          ABS_ERR_UPDATE_RECORD_MODIFIED:
           begin
             MessageDlg('The record you are trying to edit has been modified by another user. '+
                        'The table will now be refreshed',
                        mtWarning,[mbOk],0);
             DataSet.Refresh;
             Action:=daRetry;
           end;
          ABS_ERR_UPDATE_RECORD_DELETED:
           begin
             MessageDlg('The record you are trying to edit has been deleted by another user '+
                        'The table will now be refreshed',
                        mtWarning,[mbOk],0);
             DataSet.Refresh;
             Action:=daRetry;
           end
        else
          MessageDlg(E.Message,mtError,[mbOK],0);
        end
   else
      MessageDlg(E.Message,mtError,[mbOK],0);
end;

procedure TMainForm.ABSTable1PostError(DataSet: TDataSet;
  E: EDatabaseError; var Action: TDataAction);
begin
   Action:=daAbort;
   if (E is EABSEngineError) then
      begin
      if (EABSEngineError(E).ErrorCode = ABS_ERR_CONSTRAINT_VIOLATED) then
         MessageDlg(EABSEngineError(E).ErrorMessage+
                    '. Please change the record to make the constraint satisfied '+
                    'and re-post the record.',mtError,[mbOK],0)
      else if (EABSEngineError(E).ErrorCode = ABS_ERR_TABLE_LOCKED) then
         begin
         if MessageDlg('The table is locked. '+
                       'Do you want to try to post this record again?',
                        mtWarning,[mbYes,mbNo],0)=mrYes then
            Action:=daRetry;
         end
      else
         MessageDlg(E.Message,mtError,[mbOK],0);
       end
   else
      MessageDlg(E.Message,mtError,[mbOK],0);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if (ABSTable1.IsRecordLocked) then
    lbIsRecordLocked.Caption := 'Current record is locked'
  else
    lbIsRecordLocked.Caption := 'Current record is not locked';
  lbConnectedUsers.Caption := IntToStr(dbDemos.GetDBFileConnectionsCount);
end;

procedure TMainForm.ABSTable1AfterScroll(DataSet: TDataSet);
begin
  if (ABSTable1.IsRecordLocked) then
    lbIsRecordLocked.Caption := 'Current record is locked'
  else
    lbIsRecordLocked.Caption := 'Current record is not locked';
end;

end.
