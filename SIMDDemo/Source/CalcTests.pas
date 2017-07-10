unit CalcTests;

interface

{ Unit tests }

procedure UnitTestAdd;
procedure UnitTestAddAndSaturate;
procedure UnitTestDistanceSquared;

{ Performance tests }

type
  TTestProc = procedure;

procedure TestAddDelphi;
procedure TestAddSIMD;

procedure TestAddAndSaturateDelphi;
procedure TestAddAndSaturateSIMD;

procedure TestDistanceSquaredDelphi;
procedure TestDistanceSquaredSIMD;

var
  GlobalValue: Single;

implementation

uses
  System.Math,
  CalcProcs;

{ Unit tests }

procedure UnitTestAdd;
var
  A, B, C1, C2: T16Bytes;
  I, Sum: Integer;
begin
  for I := 0 to 15 do
  begin
    A[I] := I * 10;
    B[I] := I * 15;
  end;

  FillChar(C1, SizeOf(C1), 0);
  FillChar(C2, SizeOf(C2), 0);

  AddDelphi(A, B, C1);
  AddSIMD(A, B, C2);

  for I := 0 to 15 do
  begin
    Sum := (I * 25) and 255;
    Assert(C1[I] = Sum);
    Assert(C2[I] = Sum);
  end;
end;

procedure UnitTestAddAndSaturate;
var
  A, B, C1, C2: T16Bytes;
  I, Sum: Integer;
begin
  for I := 0 to 15 do
  begin
    A[I] := I * 10;
    B[I] := I * 15;
  end;

  FillChar(C1, SizeOf(C1), 0);
  FillChar(C2, SizeOf(C2), 0);

  AddAndSaturateDelphi(A, B, C1);
  AddAndSaturateSIMD(A, B, C2);

  for I := 0 to 15 do
  begin
    Sum := EnsureRange(I * 25, 0, 255);
    Assert(C1[I] = Sum);
    Assert(C2[I] = Sum);
  end;
end;

procedure UnitTestDistanceSquared;
var
  A, B: TVector4;
  D1, D2: Single;
begin
  A.Init(1.1, -2.2, 3.3, 4.4);
  B.Init(-9.9, 8.8, -7.7, 6.6);

  D1 := DistanceSquaredDelphi(A, B);
  D2 := DistanceSquaredSIMD(A, B);

  Assert(SameValue(D1, D2));
end;

{ Performance tests }

const
  LOOP_COUNT = 1000000;

{ Add }

procedure TestAddDelphi;
var
  A, B, C: T16Bytes;
  I: Integer;
begin
  for I := 0 to 15 do
  begin
    A[I] := I * 10;
    B[I] := I * 15;
  end;

  for I := 0 to LOOP_COUNT - 1 do
    AddDelphi(A, B, C);
end;

procedure TestAddSIMD;
var
  A, B, C: T16Bytes;
  I: Integer;
begin
  for I := 0 to 15 do
  begin
    A[I] := I * 10;
    B[I] := I * 15;
  end;

  for I := 0 to LOOP_COUNT - 1 do
    AddSIMD(A, B, C);
end;

{ Add and Saturate }

procedure TestAddAndSaturateDelphi;
var
  A, B, C: T16Bytes;
  I: Integer;
begin
  for I := 0 to 15 do
  begin
    A[I] := I * 10;
    B[I] := I * 15;
  end;

  for I := 0 to LOOP_COUNT - 1 do
    AddAndSaturateDelphi(A, B, C);
end;

procedure TestAddAndSaturateSIMD;
var
  A, B, C: T16Bytes;
  I: Integer;
begin
  for I := 0 to 15 do
  begin
    A[I] := I * 10;
    B[I] := I * 15;
  end;

  for I := 0 to LOOP_COUNT - 1 do
    AddAndSaturateSIMD(A, B, C);
end;

{ Distance }

procedure TestDistanceSquaredDelphi;
var
  A, B: TVector4;
  D: Single;
  I: Integer;
begin
  A.Init(1.1, -2.2, 3.3, 4.4);
  B.Init(-9.9, 8.8, -7.7, 6.6);

  for I := 0 to LOOP_COUNT - 1 do
    D := DistanceSquaredDelphi(A, B);

  GlobalValue := D; // To avoid compiler warning
end;

procedure TestDistanceSquaredSIMD;
var
  A, B: TVector4;
  D: Single;
  I: Integer;
begin
  A.Init(1.1, -2.2, 3.3, 4.4);
  B.Init(-9.9, 8.8, -7.7, 6.6);

  for I := 0 to LOOP_COUNT - 1 do
    D := DistanceSquaredSIMD(A, B);

  GlobalValue := D; // To avoid compiler warning
end;

end.
