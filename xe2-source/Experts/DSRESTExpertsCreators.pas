{*******************************************************}
{                                                       }
{               Delphi DataSnap Framework               }
{                                                       }
{ Copyright(c) 2011 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit DSRESTExpertsCreators;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSServerWebBrokerExpertsCreators, ExpertsTemplates, ExpertsProject,
  ExpertsModules;

type
  TDSRESTExpertsCreatorsModule = class(TDSServerWebBrokerExpertsCreatorsModule)
  private
    procedure AddScriptFiles(const APersonality: string; IncludeOptional: Boolean);
  protected
    procedure CreateModules(const APersonality: string); override;
    procedure UpdateProperties; override;
  end;


implementation

{$R *.dfm}

uses DSServerFeatures, DSServerScriptGen, DSServerExpertsTemplateProperties, ToolsAPI,
  DCCStrs, CommonOptionStrs;

{ TDSRESTExpertsCreatorsModule }

procedure TDSRESTExpertsCreatorsModule.CreateModules(
  const APersonality: string);
begin
  inherited;
  AddScriptFiles(APersonality, dsSampleWebFiles in Features);
end;

procedure TDSRESTExpertsCreatorsModule.AddScriptFiles(const APersonality: string; IncludeOptional: Boolean);
begin
  DSServerScriptGen.AddScriptFiles(Personality);
  if IncludeOptional then
    DSServerScriptGen.AddSampleScriptFiles(Personality);
end;

procedure TDSRESTExpertsCreatorsModule.UpdateProperties;
begin
  inherited;
  // Other properties set by ancestor

  SetBoolTemplateProperty(sIncludeSampleWebFiles_9, IsFeatureEnabled(dsSampleWebFiles));
  SetBoolTemplateProperty(sDataSnapREST_10, True);
  SetBoolTemplateProperty(sDataSnapConnectors_16, IsFeatureEnabled(dsConnectors));
end;

end.
