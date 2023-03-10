{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
unit ExpertsProjectCreators;

interface

uses ToolsApi, ExpertsBaseCreators, ExpertsIntf, SysUtils;

type
  TExpertsProjectCreator = class(TCreator, IOTACreator, IOTAProjectCreator, IOTAProjectCreator80, IOTAProjectCreator160)
  private
  private
    FExpertsProject: IExpertsProjectAccessor;
    FPersonality: string;
    property Personality: string read FPersonality;
    { IOTACreator }
    function GetCreatorType: string;
    { IOTAProjectCreator80 }
    function GetProjectPersonality: string;
    { IOTAProjectCreator }
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    { IOTAProjectCreator50 }
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    { IOTACreator }
    function GetUnnamed: Boolean;
     { IOTAProjectCreator160 }
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  public
    constructor Create(const APersonality: string; const AExpertsProject: IExpertsProjectAccessor);
  end;


implementation

uses PlatformAPI;

{ TProjectCreator }

constructor TExpertsProjectCreator.Create(const APersonality: string; const AExpertsProject: IExpertsProjectAccessor);
begin
  FPersonality := APersonality;
  FExpertsProject := AExpertsProject;
end;

function TExpertsProjectCreator.GetCreatorType: string;
begin
  Result := FExpertsProject.GetCreatorType;
end;

//function DefaultProjectFile(const APersonality: string; const ADirectory: string): string;
//  function ValidProjectName(var AFileName: string): Boolean;
//  var
//    LBaseName: string;
//    I: Integer;
//  begin
//    Result := False;
//    LBaseName := ChangeFileExt(AFileName, '');
//    if (Length(LBaseName) > 0) then
//    begin
//      Result := True;
//      for I := 1 to Length(LBaseName) do
//        if not (CharInSet(LBaseName[I], ['0'..'9','A'..'Z','a'..'z','_'])) then
//          LBaseName[I] := '_';
//      AFileName := ChangeFileExt(LBaseName, ExtractFileExt(AFileName));
//    end;
//  end;
//var
//  LTemplate: string;
//  I: Integer;
//  LExt: string;
//begin
//  if APersonality = sCBuilderPersonality then
//    LExt := '.cpp'
//  else
//    LExt := '.dpr';
//
//  Result := ChangeFileExt(ExtractFileName(ExcludeTrailingPathDelimiter(ADirectory)), LExt);
//  if ValidProjectName(Result) and  not FileExists(IncludeTrailingPathDelimiter(ADirectory) + Result) then
//    Exit;
//  LTemplate := 'Project%d' + LExt;
//  I := 1;
//  repeat
//    Result := Format(LTemplate, [I]);
//  Inc(I);
//  until not FileExists(IncludeTrailingPathDelimiter(ADirectory) + Result);
//end;

//function DefaultProjectDirectory: string;
//var
//  LTemplate: string;
//  I: Integer;
//begin
//  LTemplate := IncludeTrailingPathDelimiter((BorlandIDEServices as IOTAServices).GetStartupDirectory) +
//    sProjectDirTemplate;
//  I := 1;
//  repeat
//    Result := Format(LTemplate, [I]);
//  Inc(I);
//  until not DirectoryExists(Result);
//end;

function TExpertsProjectCreator.GetFileName: string;
var
  LSuffix: string;
  LFileName: string;
  LDirectory: string;
begin
  Result := '';
  LDirectory := FExpertsProject.GetDirectory;
  LFileName := FExpertsProject.GetFileName;
  if (LDirectory <> '') or (LFileName <> '') then
  begin
    if LFileName <> '' then
    begin
      if Personality = sCBuilderPersonality then
        LFileName := ChangeFileExt(LFileName, '.cpp')
      else
        LFileName := ChangeFileExt(LFileName, '.dpr');
    end;
    if LDirectory = '' then
    begin
      LDirectory := (BorlandIDEServices as IOTAServices).GetStartupDirectory;
    end;
    Result := GetNewModuleFileName('Project', LDirectory,                            
      LFileName, False, LSuffix)
  end;
end;

function TExpertsProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TExpertsProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TExpertsProjectCreator.GetPlatforms: TArray<string>;
begin
  Result := TArray<string>.Create(cWin32Platform, cWin64Platform);
end;

function TExpertsProjectCreator.GetPreferredPlatform: string;
begin
  Result := GetPlatforms[0];
end;

function TExpertsProjectCreator.GetProjectPersonality: string;
begin
  Result := Personality;
end;

function TExpertsProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TExpertsProjectCreator.GetUnnamed: Boolean;
begin
  Result := FExpertsProject.GetUnnamed;
end;

procedure TExpertsProjectCreator.NewDefaultModule;
begin
  FExpertsProject.NewDefaultModule;
end;

procedure TExpertsProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  NewDefaultModule;
end;

function TExpertsProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TExpertsProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  { do nothing here }
end;

function TExpertsProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result :=  FExpertsProject.NewProjectSource(ProjectName);
end;

procedure TExpertsProjectCreator.SetInitialOptions(
  const NewProject: IOTAProject);
begin
  // do nothing
end;

end.
