unit uDatabase;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmDatabase = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbCryptoAgorithm: TComboBox;
    ePassword2: TEdit;
    ePassword1: TEdit;
    cbEncrypted: TCheckBox;
    cbPageSize: TComboBox;
    cbPageCountInExtent: TComboBox;
    Label6: TLabel;
    eMaxConnections: TEdit;
    procedure FormShow(Sender: TObject);
    procedure cbEncryptedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDatabase: TfrmDatabase;

implementation

{$R *.dfm}

uses main, ABSMain, ABSSecurity;


procedure TfrmDatabase.FormShow(Sender: TObject);
begin
  cbPageSize.Text := IntToStr(frmMain.db.PageSize);
  cbPageCountInExtent.Text := IntToStr(frmMain.db.PageCountInExtent);
  eMaxConnections.Text := IntToStr(frmMain.db.MaxConnections);
  ePassword1.Text := frmMain.db.Password;
  ePassword2.Text := frmMain.db.Password;
  cbCryptoAgorithm.ItemIndex := Integer(frmMain.db.CryptoAlgorithm);
  cbEncrypted.Checked := (frmMain.db.Password <> '');
  cbEncryptedClick(nil);
end;

procedure TfrmDatabase.cbEncryptedClick(Sender: TObject);
begin
  ePassword1.Enabled := cbEncrypted.Checked;
  ePassword2.Enabled := cbEncrypted.Checked;
  cbCryptoAgorithm.Enabled := cbEncrypted.Checked;
end;

procedure TfrmDatabase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if (ModalResult = mrOk) and (cbEncrypted.Checked) and
    (ePassword1.Text <> ePassword2.Text) then
  begin
   CanClose := False;
   MessageDlg('Password and Confirm password are not equal', mtError,[mbOk], 0);
  end;
end;

end.
