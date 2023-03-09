{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Design.Path;

interface

uses
  System.SysUtils, System.Types, System.Classes, FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls,
  FMX.Dialogs, FMX.Layouts, FMX.Objects, FMX.Memo, FmxDesignWindows, FMX.ScrollBox, FMX.Controls.Presentation,
  FMX.ListBox;

type
  TPathDataDesigner = class(TFmxDesignWindow)
    previewLayout: TLayout;
    Label1: TLabel;
    PreviewPath: TPath;
    Layout2: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    LabelMemo: TLabel;
    PathData: TMemo;
    TimerUpdatePreview: TTimer;
    PreviewUpdatingIndicator: TAniIndicator;
    ComboBoxPathWrapMode: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    Splitter1: TSplitter;
    Layout3: TLayout;
    LabelInvalidFormat: TLabel;
    procedure PathDataChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TimerUpdatePreviewTimer(Sender: TObject);
    procedure PathDataChangeTracking(Sender: TObject);
    procedure ComboBoxPathWrapModeChange(Sender: TObject);
  public
    procedure StartGeneratingPreview;
    procedure FinishGeneratingPreview;
  end;

var
  PathDataDesigner: TPathDataDesigner;

implementation

uses System.UITypes;

{$R *.fmx}

procedure TPathDataDesigner.Button1Click(Sender: TObject);
begin
  pathData.SelectAll;
  pathData.DeleteSelection;
  pathData.PasteFromClipboard;
end;

procedure TPathDataDesigner.ComboBoxPathWrapModeChange(Sender: TObject);
begin
  PreviewPath.WrapMode := TPathWrapMode(ComboBoxPathWrapMode.ItemIndex);
end;

procedure TPathDataDesigner.FinishGeneratingPreview;
begin
  try
    if previewLayout.Visible then
      previewPath.Data.Data:= PathData.Text;
  except on E: Exception do
    LabelInvalidFormat.Visible := True;
    // Ignore the invalid path format during the input process.
  end;
  TimerUpdatePreview.Enabled := False;
  PreviewUpdatingIndicator.Enabled := False;
  PreviewUpdatingIndicator.Visible := False;
end;

procedure TPathDataDesigner.PathDataChange(Sender: TObject);
begin
  if previewLayout.Visible then
    previewPath.Data.Data:= PathData.Text;
end;

procedure TPathDataDesigner.PathDataChangeTracking(Sender: TObject);
begin
  StartGeneratingPreview;
end;

procedure TPathDataDesigner.StartGeneratingPreview;
begin
  // Restart update preview timer
  TimerUpdatePreview.Enabled := False;
  TimerUpdatePreview.Enabled := True;
  PreviewUpdatingIndicator.Visible := True;
  PreviewUpdatingIndicator.Enabled := True;
  LabelInvalidFormat.Visible := False;
end;

procedure TPathDataDesigner.TimerUpdatePreviewTimer(Sender: TObject);
begin
  FinishGeneratingPreview;
end;

end.
