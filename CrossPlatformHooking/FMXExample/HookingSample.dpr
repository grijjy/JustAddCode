program HookingSample;

uses
  InstanceTracker in '..\Source\InstanceTracker.pas',
  InstanceHashMap in '..\Source\InstanceHashMap.pas',
  Hooking in '..\Source\Hooking.pas',
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
