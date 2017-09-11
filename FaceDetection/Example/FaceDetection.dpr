program FaceDetection;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  Grijjy.FaceDetection in '..\Grijjy.FaceDetection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
