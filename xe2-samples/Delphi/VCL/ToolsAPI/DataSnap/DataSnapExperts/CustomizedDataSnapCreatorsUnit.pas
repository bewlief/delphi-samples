
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomizedDataSnapCreatorsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, DSServerExpertsCreators,
  ExpertsTemplates, ExpertsModules, ExpertsProject,
  DSServerFeatures, DSServerExpertsTemplateProperties,
  DSServerMethodsExpertsCreators;

type
  TCustomizedDataSnapCreatorsModule = class(TDSServerExpertsCreatorsModule)
  private
    { Private declarations }
    FComments: string;
    procedure SetComments(const Value: string);
    function FormatComments(const AText: string): string;
  protected
    function GetServerMethodsCreatorModuleClass: TDSServerMethodsCreatorModuleClass; override;
  public
    { Public declarations }
    procedure UpdateProperties; override;
    property Comments: string read FComments write SetComments;
  end;

var
  CustomizedDataSnapCreatorsModule: TCustomizedDataSnapCreatorsModule;

implementation

{$R *.dfm}

uses CustomizedFeatures, CustomizedServerMethodsCreatorUnit;

procedure TCustomizedDataSnapCreatorsModule.SetComments(const Value: string);
begin
  FComments := Value;
  UpdateProperties;
end;

function TCustomizedDataSnapCreatorsModule.FormatComments(const AText: string): string;
var
  LResult: TStrings;
  LLines: TStrings;
  S: string;
begin
  LLines := TStringList.Create;
  try
    LLines.Text := AText;
    LResult := TStringList.Create;
    try
      for S in LLines do
        LResult.Add('//' + S);
      Result := LResult.Text;
    finally
      LResult.Free;
    end;
  finally
    LLines.Free;
  end;
end;

function TCustomizedDataSnapCreatorsModule.GetServerMethodsCreatorModuleClass: TDSServerMethodsCreatorModuleClass;
begin
  Result := TCustomizedServerMethodsCreator;
end;

procedure TCustomizedDataSnapCreatorsModule.UpdateProperties;
var
  LNow: TDateTime;
begin
  inherited;
  // Customization: Set properties used in templates
  SetBoolTemplateProperty(sCustomFeatureCommentModule, IsFeatureEnabled(cCustomFeatureCommentModule));
  SetBoolTemplateProperty(sCustomFeatureTimeStamp, IsFeatureEnabled(cCustomFeatureTimeStampModule));
  SetBoolTemplateProperty(sCustomSampleMethods, IsFeatureEnabled(cCustomFeatureSampleMethods));
  CommonTemplateProperties.Properties.Values[sCommentModuleText] := FormatComments(FComments);
  LNow := Now;
  CommonTemplateProperties.Properties.Values[sTimeStampText] :=
    FormatDateTime(FormatSettings.ShortDateFormat, LNow) + ' ' + FormatDateTime(FormatSettings.LongTimeFormat, LNow);
end;


end.
