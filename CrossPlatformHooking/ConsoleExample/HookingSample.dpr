program HookingSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  InstanceTracker in '..\Source\InstanceTracker.pas',
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  Xml.XmlDoc,
  Xml.XmlDom,
  Xml.XmlIntf,
  Xml.OmniXmlDom,
  Hooking in '..\Source\Hooking.pas',
  InstanceHashMap in '..\Source\InstanceHashMap.pas';

procedure LogInstanceCounts;
var
  InstanceCounts: TArray<TInstanceInfo>;
  I: Integer;
begin
  { Retrieve an array of live instances and sort it by class name. }
  InstanceCounts := GetInstanceCounts;
  TArray.Sort<TInstanceInfo>(InstanceCounts, TComparer<TInstanceInfo>.Construct(
    function(const ALeft, ARight: TInstanceInfo): Integer
    begin
      Result := CompareText(ALeft.Clazz.ClassName, ARight.Clazz.ClassName);
    end));


  WriteLn('----------+--------------------------------------');
  WriteLn('Instances | Class');
  WriteLn('----------+--------------------------------------');
  for I := 0 to Length(InstanceCounts) - 1 do
  begin
    if (InstanceCounts[I].Count <> 0) then
    begin
      WriteLn(Format('    %5d | %s', [
        InstanceCounts[I].Count,
        InstanceCounts[I].Clazz.ClassName]));
    end;
  end;
end;

procedure Run;
var
  Doc: IXMLDocument;
begin
  DefaultDOMVendor := sOmniXmlVendor;

  { Load sample XML to create some objects.
    (Taken from https://www.w3schools.com/xml/note.xml) }
  Doc := LoadXMLData(
    '<note>' +
    '  <to>Tove</to>' +
    '  <from>Jani</from>' +
    '  <heading>Reminder</heading>' +
    '  <body>Don''t forget me this weekend!</body>' +
    '</note>');

  { Log the instances }
  WriteLn('Instance counts after creating XML document: ');
  LogInstanceCounts;

  { Release XML document }
  Doc := nil;

  { Log the instances }
  WriteLn;
  WriteLn('Instance counts after releasing XML document: ');
  LogInstanceCounts;

  WriteLn;
  WriteLn('Press [Enter] to continue...');
  ReadLn;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
