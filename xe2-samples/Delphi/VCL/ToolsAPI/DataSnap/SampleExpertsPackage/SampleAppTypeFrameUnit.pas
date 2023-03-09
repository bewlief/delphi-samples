
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SampleAppTypeFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, SampleExpertTypes;

type

  TApplicationTypeFrame = class(TFrame)
    RadioButtonVCLApplication: TRadioButton;
    RadioButtonConsoleApplication: TRadioButton;
    procedure RadioButtonVCLApplicationClick(Sender: TObject);
  private
    FOnApplicationTypeChange: TNotifyEvent;
    function GetApplicationType: TSampleApplicationType;
    procedure SetApplicationType(AType: TSampleApplicationType);
    { Private declarations }
  public
    { Public declarations }
    property OnApplicationTypeChange: TNotifyEvent read FOnApplicationTypeChange write FOnApplicationTypeChange;
    property ApplicationType: TSampleApplicationType read GetApplicationType write SetApplicationType;
  end;

implementation

{$R *.dfm}

function TApplicationTypeFrame.GetApplicationType: TSampleApplicationType;
begin
  if RadioButtonVCLApplication.Checked then
    Exit(appVCL)
  else if RadioButtonConsoleApplication.Checked then
    Exit(appConsole)
  else
    raise Exception.Create('Unexpected');
end;

procedure TApplicationTypeFrame.SetApplicationType(AType: TSampleApplicationType);
begin
  case AType of
    appVCL: RadioButtonVCLApplication.Checked := True;
    appConsole: RadioButtonConsoleApplication.Checked := True;
  else
    raise Exception.Create('Unexpected');
  end;
end;

procedure TApplicationTypeFrame.RadioButtonVCLApplicationClick(Sender: TObject);
begin
  if Assigned(FOnApplicationTypeChange) then
    FOnApplicationTypeChange(Self);
end;

end.
