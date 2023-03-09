
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit customlistfrm;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types, System.UITypes,
  FMX.Forms, FMX.Dialogs, FMX.Types, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMX.Controls, FMX.Edit;

type
  TfrmCustomList = class(TForm)
    ListBox1: TListBox;
    Resources1: TStyleBook;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    InfoLabel: TLabel;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure DoInfoClick(Sender: TObject);
    procedure DoVisibleChange(Sender: TObject);
    procedure DoApplyStyleLookup(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmCustomList: TfrmCustomList;

implementation

{$R *.fmx}

procedure TfrmCustomList.Button1Click(Sender: TObject);
var
  Item: TListBoxItem;
  B: TBitmap;
  S: TPointF;
begin
  OpenDialog1.Filter := DefaultBitmapCodecClass.GetFileTypes;
  if OpenDialog1.Execute then
  begin
    // create thumbnail
    B := TBitmap.Create(1, 1);
    B.LoadThumbnailFromFile(OpenDialog1.FileName, 100, 100, true);
    // get image size
    S := DefaultBitmapCodecClass.GetImageSize(OpenDialog1.FileName);
    // create custom item
    Item := TListBoxItem.Create(nil);
    Item.Parent := ListBox1;
    // this code force our style to new item
    Item.StyleLookup := 'CustomItem';
    // use this to set our child controls value - this code use BindingName in style to search
    Item.Binding['image'] := ObjectToVariant(B); // set thumbnail
    Item.Binding['text'] := ExtractFileName(OpenDialog1.FileName); // set filename
    Item.Binding['resolution'] := IntToStr(trunc(S.X)) + 'x' + IntToStr(trunc(S.Y)) + ' px'; // set size
    Item.Binding['depth'] := '32 bit';
    Item.Binding['visible'] := EventToVariant(DoVisibleChange); // set OnChange value
    Item.Binding['info'] := EventToVariant(DoInfoClick); // set OnClick value
    // free thumbnail
    B.Free;
  end;
end;

procedure TfrmCustomList.Button2Click(Sender: TObject);
var
  Item: TListBoxItem;
begin
  // create custom item
  Item := TListBoxItem.Create(nil);
  Item.Parent := ListBox1;
  // this code set event - when we need to setup item
  Item.OnApplyStyleLookup := DoApplyStyleLookup;
  // this set our style to new item
  Item.StyleLookup := 'CustomItem';
end;

procedure TfrmCustomList.DoInfoClick(Sender: TObject);
begin
  InfoLabel.Text := 'Info Button click on ' + IntToStr(ListBox1.ItemIndex) + ' listbox item';
end;

procedure TfrmCustomList.DoVisibleChange(Sender: TObject);
begin
  InfoLabel.Text := 'Checkbox changed ' + IntToStr(ListBox1.ItemIndex) + ' listbox item to ' + BoolToStr(Listbox1.Selected.Binding['visible'], true);
end;

procedure TfrmCustomList.Button3Click(Sender: TObject);
var
  i: integer;
begin
  ListBox1.BeginUpdate;
  for i := 1 to 1000 do
    Button2Click(Sender);
  ListBox1.EndUpdate;
end;

procedure TfrmCustomList.DoApplyStyleLookup(Sender: TObject);
var
  B: TBitmap;
  Item: TListboxItem;
begin
  Item := TListBoxItem(Sender);
  // create thumbnail
  B := TBitmap.Create(10 + random(50), 10 + random(50));
  B.Clear($FF000000 or TAlphaColor(random($FFFFFF)));
  // use this to set our child controls value - this code use BindingName in style to search
  Item.Binding['image'] := ObjectToVariant(B); // set thumbnail
  Item.Binding['text'] := 'item ' + IntToStr(Item.Index); // set filename
  Item.Binding['resolution'] := IntToStr(B.Width) + 'x' + IntToStr(B.Height) + ' px'; // set size
  Item.Binding['depth'] := '32 bit';
  Item.Binding['visible'] := true; // set Checkbox value
  Item.Binding['visible'] := EventToVariant(DoVisibleChange); // set OnChange value
  Item.Binding['info'] := EventToVariant(DoInfoClick); // set OnClick value
  // free thumbnail
  B.Free;
end;

procedure TfrmCustomList.CheckBox1Change(Sender: TObject);
begin
  ListBox1.AllowDrag := CheckBox1.IsChecked;
end;

end.
