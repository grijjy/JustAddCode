program E04GrowableStackList;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UGrowableStackList in 'UGrowableStackList.pas';

procedure GrowableStackListExample;
var
  List: TStackList<Integer, T128Bytes>;
  I: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;
  List.Initialize;
  try
    { This list can contain up to 128 bytes of stack data. An Integer is 4 bytes
      in size, meaning the list can contain up to 32 integers before it starts to
      use the heap. }
    for I := 0 to 31 do
    begin
      Assert(List.Count = I);
      Assert(List.HeapCount = 0);

      List.Add(I);
    end;

    { Any additional items will go to the heap }
    for I := 32 to 99 do
    begin
      Assert(List.Count = I);
      Assert(List.HeapCount = I - 32);

      List.Add(I);
    end;

    { Check contents }
    Assert(List.Count = 100);
    for I := 0 to List.Count - 1 do
      Assert(List[I] = I);
  finally
    { Free any heap memory the list may have allocated. }
    List.Finalize;
  end;
end;

begin
  try
    GrowableStackListExample;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


