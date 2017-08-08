program UniformMemoryManagement;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  UBase in 'UBase.pas',
  USampleClasses in 'USampleClasses.pas',
  FBase in 'FBase.pas' {FrameBase: TFrame},
  FOwnership in 'FOwnership.pas' {FrameOwnership: TFrame},
  FFreeNotifications in 'FFreeNotifications.pas' {FrameFreeNotifications: TFrame},
  FManagedTypes in 'FManagedTypes.pas' {FrameManagedTypes: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
