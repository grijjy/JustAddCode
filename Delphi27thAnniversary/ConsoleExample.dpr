program ConsoleExample;
{ Example of console application with unified command line parameters and settings }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Grijjy.System.Console,
  Settings;

begin
  { Load settings from remote, command line and/or disk }
  TSettings.Load('https://mydomain.com/settings');

  { Load settings from command line and/or disk }
  //TSettings.Load;

  { Run loop }
  Writeln('Press Ctrl-C to Exit');
  WaitForCtrlC;
end.
