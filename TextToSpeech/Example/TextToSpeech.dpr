program TextToSpeech;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
