program CRGPSamples;
{ Sample code for the blog post "The Curiously Recurring Generic Pattern" }

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}

{$R *.res}

uses
  System.SysUtils,
  LinkedList.Standard in 'Samples\LinkedList.Standard.pas',
  LinkedList.CRGP in 'Samples\LinkedList.CRGP.pas',
  Utils in 'Samples\Utils.pas',
  Counter.CRGP in 'Samples\Counter.CRGP.pas',
  FluentInterface.Standard in 'Samples\FluentInterface.Standard.pas',
  FluentInterface.CRGP in 'Samples\FluentInterface.CRGP.pas';

begin
  try
    TestLinkedListStandard;
    TestLinkedListCRGP;
    TestCounterCRGP;
    TestFluentInterfaceStandard;
    TestFluentInterfaceCRGP;

    if (DebugHook <> 0) then
    begin
      StartSection;
      Write('Press [Enter] to continue...');
      ReadLn;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
