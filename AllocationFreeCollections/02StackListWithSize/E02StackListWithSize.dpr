program E02StackListWithSize;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Types,
  System.SysUtils,
  UStackListWithSize in 'UStackListWithSize.pas';

procedure StackListWithSizeExample;
var
  List: TStackList<TPoint, T128Bytes>;
  Error: Boolean;
  I: Integer;
  P: TPoint;
begin
  ReportMemoryLeaksOnShutdown := True;
  List.Initialize;

  { This list can contain up to 128 bytes of data. A TPoint record is 8 bytes
    in size, meaning the list can contain up to 16 points. }
  for I := 0 to 15 do
    List.Add(Point(I, -I));

  { Adding 16'th point should raise an exception. }
  Error := False;
  try
    List.Add(Point(0, 0));
  except
    Error := True;
  end;
  Assert(Error);

  { Check contents }
  Assert(List.Count = 16);
  for I := 0 to List.Count - 1 do
  begin
    P := List[I];
    Assert(P.X = I);
    Assert(P.Y = -I);
  end;
end;

begin
  try
    StackListWithSizeExample;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
