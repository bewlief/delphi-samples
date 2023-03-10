{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit AddActn;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, DesignIntf, DesignEditors, DesignWindows;

type
  TAddActionDialog = class(TForm)
    URLEdit: TEdit;
    ComponentEdit: TEdit;
    TypeComboBox: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure TypeComboBoxChange(Sender: TObject);
  public
    Designer: IFormDesigner;
  end;

implementation

{$R *.dfm}

procedure TAddActionDialog.TypeComboBoxChange(Sender: TObject);
begin
  ComponentEdit.Text := Designer.UniqueName(TypeComboBox.Items[TypeComboBox.ItemIndex]);
end;

end.
