program SIMDDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  CalcProcs in 'Source\CalcProcs.pas',
  CalcTests in 'Source\CalcTests.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
