program TasksClientProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  TasksTypesU in 'TasksTypesU.pas',
  TasksClientModuleU in 'TasksClientModuleU.pas' {TasksClientModule: TDataModule},
  TasksAdapterModuleU in 'TasksAdapterModuleU.pas' {TasksAdapterModule: TDataModule},
  TasksClientFormU in 'TasksClientFormU.pas' {TasksClientForm},
  TasksClientSettingsU in 'TasksClientSettingsU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTasksClientModule, TasksClientModule);
  Application.CreateForm(TTasksAdapterModule, TasksAdapterModule);
  Application.CreateForm(TTasksClientForm, TasksClientForm);
  Application.Run;
end.
