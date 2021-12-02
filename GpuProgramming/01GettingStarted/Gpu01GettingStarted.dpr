program Gpu01GettingStarted;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  Materials in 'Materials.pas';

{$R *.res}

begin
  {$IFDEF ALTERNATIVE_BACKEND}
  GlobalUseDX := False;   // On Windows, use DX9 instead of DX11
  GlobalUseMetal := True; // On macOS/iOS, use Metal instead of OpenGL
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
