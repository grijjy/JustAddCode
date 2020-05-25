program FmxDesktop3D;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMain in 'FMain.pas' {FormMain};

{$R *.res}

begin
  {$IFDEF USE_METAL}
  GlobalUseMetal := True;
  {$ENDIF}
  {$IFDEF USE_GPU_CANVAS}
  GlobalUseGPUCanvas := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
