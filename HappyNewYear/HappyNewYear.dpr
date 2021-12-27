program HappyNewYear;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  Materials in 'Materials.pas';

{$R *.res}

begin
  {$IFDEF ALTERNATIVE_BACKEND}
  // On Windows, use DX9 instead of DX11
  // On macOS/iOS, use OpenGL instead of Metal
  GlobalUseDX := False;
  {$ELSE}
  GlobalUseMetal := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
