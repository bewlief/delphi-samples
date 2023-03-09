
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomizedDSRESTCreatorsUnit;

interface

uses
  System.SysUtils, System.Classes, DSRESTExpertsCreators,
  DSServerWebBrokerExpertsCreators, ExpertsTemplates, ExpertsProject,
  ExpertsModules, DSServerExpertsCreators, DSServerMethodsExpertsCreators;

type
  TCustomizedDSRESTCreatorsModule = class(TDSRESTExpertsCreatorsModule)
  private
    FComments: string;
    FAddFiles: TArray<string>;
    function FormatComments(const AText: string): string;
    procedure SetComments(const Value: string);
  protected
    function GetServerMethodsCreatorModuleClass: TDSServerMethodsCreatorModuleClass; override;
    procedure UpdateProperties; override;
    procedure AddFiles(const APersonality: string); override;
    { Private declarations }
  public
    { Public declarations }
    property Comments: string read FComments write SetComments;
    property FilesToAdd: TArray<string> read FAddFiles write FAddFiles;
  end;

var
  CustomizedDSRESTCreatorsModule: TCustomizedDSRESTCreatorsModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses CustomizedServerMethodsCreatorUnit, CustomizedFeatures, DSServerScriptGen,
 Windows, ToolsAPI;

procedure TCustomizedDSRESTCreatorsModule.SetComments(const Value: string);
begin
  FComments := Value;
  UpdateProperties;
end;

procedure TCustomizedDSRESTCreatorsModule.AddFiles(const APersonality: string);
var
  S: string;
  LProject: IOTAProject;
  LPath: string;
  LDestination: string;
begin
  inherited;

  LProject := GetActiveProject;
  LPath := ExtractFilePath(LProject.FileName);
  LPath := LPath + 'addedfiles\';
  // Add files without opening in the project
  // Assumes files are not units
  for S in FAddFiles do
  begin
    LDestination := LPath + ExtractFileName(S);
    ForceDirectories(LPath);
    if CopyFile(PChar(S), PChar(LDestination), True) then
    begin
      // Add as if not a unit
      LProject.AddFile(LDestination,
        False); // Not a unit
    end;

  end;

end;

function TCustomizedDSRESTCreatorsModule.FormatComments(const AText: string): string;
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

function TCustomizedDSRESTCreatorsModule.GetServerMethodsCreatorModuleClass: TDSServerMethodsCreatorModuleClass;
begin
  Result := TCustomizedServerMethodsCreator;
end;

procedure TCustomizedDSRESTCreatorsModule.UpdateProperties;
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
