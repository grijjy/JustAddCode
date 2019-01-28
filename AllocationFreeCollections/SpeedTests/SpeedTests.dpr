program SpeedTests;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Generics.Collections,
  UStackList1 in 'UStackList1.pas',
  UStackList2 in 'UStackList2.pas',
  UStackList3 in 'UStackList3.pas',
  UStackList4 in 'UStackList4.pas';

type
  { Uncomment one of these storage types to test stack lists of that size. }
//  TStorage = record Data: array [0..31] of Byte; end;
//  TStorage = record Data: array [0..63] of Byte; end;
//  TStorage = record Data: array [0..127] of Byte; end;
  TStorage = record Data: array [0..255] of Byte; end;
//  TStorage = record Data: array [0..511] of Byte; end;
//  TStorage = record Data: array [0..1023] of Byte; end;
//  TStorage = record Data: array [0..2047] of Byte; end;

const
  ITEM_COUNT = SizeOf(TStorage) div SizeOf(Integer);
  LOOP_COUNT = 100000000 div ITEM_COUNT;            // So each tests lasts approx. the same amount of time
  ITEM_SUM   = (ITEM_COUNT + 1) * (ITEM_COUNT / 2); // Sum of 1..ITEM_COUNT

var
  Stopwatch: TStopwatch;

procedure StartTimer(const ACaption: String);
begin
  Write(ACaption, '...');
  Stopwatch := TStopwatch.StartNew;
end;

function StopTimer: Double;
begin
  Result := Stopwatch.Elapsed.TotalMilliseconds;
  Write(Format(' %.2f ms', [Result]));
end;

procedure RunSpeedTests;
var
  RefList: TList<Integer>;
  List1: TStackList1<Integer, TStorage>;
  List2: TStackList2<Integer, TStorage>;
  List3: TStackList2<Integer, TStorage>;
  List4: TStackList2<Integer, TStorage>;
  Ms, RefMs: Double;
  I, J, Sum: Integer;
begin
  StartTimer('TList<Integer>');
  for I := 0 to LOOP_COUNT - 1 do
  begin
    RefList := TList<Integer>.Create;

    for J := 1 to ITEM_COUNT do
      RefList.Add(J);

    Sum := 0;
    for J := 0 to RefList.Count - 1 do
      Inc(Sum, RefList[J]);

    if (Sum <> ITEM_SUM) then
      raise Exception.Create('Error!');

    RefList.Free;
  end;
  RefMs := StopTimer;
  WriteLn;

  StartTimer('TStackList1<Integer, TStorage>');
  for I := 0 to LOOP_COUNT - 1 do
  begin
    List1.Initialize;

    for J := 1 to ITEM_COUNT do
      List1.Add(J);

    Sum := 0;
    for J := 0 to List1.Count - 1 do
      Inc(Sum, List1[J]);

    if (Sum <> ITEM_SUM) then
      raise Exception.Create('Error!');
  end;
  Ms := StopTimer;
  WriteLn(Format(' (%.2f x faster)', [RefMs / Ms]));

  StartTimer('TStackList2<Integer, TStorage>');
  for I := 0 to LOOP_COUNT - 1 do
  begin
    List2.Initialize;

    for J := 1 to ITEM_COUNT do
      List2.Add(J);

    Sum := 0;
    for J := 0 to List2.Count - 1 do
      Inc(Sum, List2[J]);

    if (Sum <> ITEM_SUM) then
      raise Exception.Create('Error!');
  end;
  Ms := StopTimer;
  WriteLn(Format(' (%.2f x faster)', [RefMs / Ms]));

  StartTimer('TStackList3<Integer, TStorage>');
  for I := 0 to LOOP_COUNT - 1 do
  begin
    List3.Initialize;

    for J := 1 to ITEM_COUNT do
      List3.Add(J);

    Sum := 0;
    for J := 0 to List3.Count - 1 do
      Inc(Sum, List3[J]);

    if (Sum <> ITEM_SUM) then
      raise Exception.Create('Error!');
  end;
  Ms := StopTimer;
  WriteLn(Format(' (%.2f x faster)', [RefMs / Ms]));

  StartTimer('TStackList4<Integer, TStorage>');
  for I := 0 to LOOP_COUNT - 1 do
  begin
    List4.Initialize;

    for J := 1 to ITEM_COUNT do
      List4.Add(J);

    Sum := 0;
    for J := 0 to List4.Count - 1 do
      Inc(Sum, List4[J]);

    if (Sum <> ITEM_SUM) then
      raise Exception.Create('Error!');
  end;
  Ms := StopTimer;
  WriteLn(Format(' (%.2f x faster)', [RefMs / Ms]));

  WriteLn('Finished!');
  ReadLn;
end;

begin
  try
    RunSpeedTests;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
