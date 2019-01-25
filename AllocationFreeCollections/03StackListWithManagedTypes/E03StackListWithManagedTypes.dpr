program E03StackListWithManagedTypes;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UStackListWithManagedTypes in 'UStackListWithManagedTypes.pas';

procedure StackListWithManagedTypesExample;
var
  List: TStackList<String, T512Bytes>;
  I: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;
  List.Initialize;
  try
    { This list can contain up to 512 bytes of data. A String is just a pointer,
      so is 4 or 8 bytes in size, depending on platform. This means that the
      list can hold at most 128 or 64 strings. }
    for I := 0 to 63 do
      List.Add(I.ToString);

    { Check contents }
    Assert(List.Count = 64);
    for I := 0 to List.Count - 1 do
      Assert(List[I] = I.ToString);
  finally
    { When adding Strings (or other managed types) to the list, their reference
      counts get incremented. So we need to finalize the list to release these
      references to prevent a memory leak. }
    List.Finalize;
  end;
end;

procedure StackListWithValueTypesExample;
var
  List: TStackList<Single, T512Bytes>;
  I: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;
  List.Initialize;
  try
    { This list can contain up to 512 bytes of data. A Single is 4 bytes in
      size, so the list can hold at most 128 Singles. }
    for I := 0 to 127 do
      List.Add(I);

    { Check contents }
    Assert(List.Count = 128);
    for I := 0 to List.Count - 1 do
      Assert(List[I] = I);
  finally
    { Even though this list does *not* contain managed types, it is still good
      practice to always finalize it. }
    List.Finalize;
  end;
end;

begin
  try
    StackListWithManagedTypesExample;
    StackListWithValueTypesExample;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
