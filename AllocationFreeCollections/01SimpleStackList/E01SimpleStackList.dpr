program E01SimpleStackList;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  USimpleStackList in 'USimpleStackList.pas';

procedure SimpleStackListExample;
var
  List: TStackList<Integer>;
  Error: Boolean;
  I: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;
  List.Initialize;

  { TStackList<Integer> can contain up to 256 bytes of data. An Integer is 4
    bytes in size, meaning the list can contain up to 64 Integers. }
  for I := 0 to 63 do
    List.Add(I);

  { Adding 64'th item should raise an exception. }
  Error := False;
  try
    List.Add(0);
  except
    Error := True;
  end;
  Assert(Error);

  { Check contents }
  Assert(List.Count = 64);
  for I := 0 to List.Count - 1 do
    Assert(List[I] = I);
end;

procedure InvalidItemTypeExample;
type
  TRecordWithString = record
    Value: String;
  end;
var
  { You cannot declare:
      List: TStackList<String>;
    This does not compile because of the record constraint.
    However, you can declare a list of records with a managed type... }
  List: TStackList<TRecordWithString>;
  Error: Boolean;
begin
  { ...However, initializing it should raise an exception. }
  Error := False;
  try
    List.Initialize;
  except
    Error := True;
  end;
  Assert(Error);
end;

begin
  try
    SimpleStackListExample;
    InvalidItemTypeExample;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
