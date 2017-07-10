unit CalcProcs;

interface

type
  { A simple array of 16 bytes }
  T16Bytes = array [0..15] of Byte;

type
  { A 4-dimensional vector of single-precision floating-point values.
    Very common in 3D applications. }
  TVector4 = record
  public
    X, Y, Z, W: Single;
  public
    procedure Init(const AX, AY, AZ, AW: Single);
  end;

procedure AddDelphi(const A, B: T16Bytes; out C: T16Bytes);
procedure AddSIMD(const A, B: T16Bytes; out C: T16Bytes); {$IFDEF CPUARM}inline;{$ENDIF}

procedure AddAndSaturateDelphi(const A, B: T16Bytes; out C: T16Bytes);
procedure AddAndSaturateSIMD(const A, B: T16Bytes; out C: T16Bytes); {$IFDEF CPUARM}inline;{$ENDIF}

function DistanceSquaredDelphi(const A, B: TVector4): Single;
function DistanceSquaredSIMD(const A, B: TVector4): Single; {$IFDEF CPUARM}inline;{$ENDIF}

{$IFDEF CPUARM}
const
  {$IF Defined(ANDROID)}
  LIB_SIMD = 'libsimd-android.a';
  {$ELSEIF Defined(IOS)}
  LIB_SIMD = 'libsimd-ios.a';
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

{ Import ARM versions from static library. }
procedure add_simd(A, B, C: Pointer); cdecl; external LIB_SIMD;
procedure add_and_saturate_simd(A, B, C: Pointer); cdecl; external LIB_SIMD;
function distance_squared_simd(A, B: Pointer): Single; cdecl; external LIB_SIMD;
{$ENDIF}

implementation

procedure AddDelphi(const A, B: T16Bytes; out C: T16Bytes);
var
  I: Integer;
begin
  for I := 0 to 15 do
    C[I] := A[I] + B[I];
end;

procedure AddAndSaturateDelphi(const A, B: T16Bytes; out C: T16Bytes);
var
  I, Sum: Integer;
begin
  for I := 0 to 15 do
  begin
    Sum := A[I] + B[I];
    if (Sum > 255) then
      C[I] := 255
    else
      C[I] := Sum;
  end;
end;

function DistanceSquaredDelphi(const A, B: TVector4): Single;
var
  C: TVector4;
begin
  { Subtract the two vectors }
  C.X := A.X - B.X;
  C.Y := A.Y - B.Y;
  C.Z := A.Z - B.Z;
  C.W := A.W - B.W;

  { Calculate the dot product of C x C }
  Result := (C.X * C.X) + (C.Y * C.Y) + (C.Z * C.Z) + (C.W * C.W);
end;

{$IF Defined(CPUX86)}

// Windows 32 bit, macOS, iOS simulator

procedure AddSIMD(const A, B: T16Bytes; out C: T16Bytes);
//                     eax edx             ecx
asm
  movdqu  xmm0, [eax]     // Load A into xmm0
  movdqu  xmm1, [edx]     // Load B into xmm1
  paddb   xmm0, xmm1      // xmm0 := xmm0 + xmm1 (16 times)
  movdqu  [ecx], xmm0     // Store xmm0 into C
end;

procedure AddAndSaturateSIMD(const A, B: T16Bytes; out C: T16Bytes);
//                     eax edx             ecx
asm
  movdqu  xmm0, [eax]     // Load A into xmm0
  movdqu  xmm1, [edx]     // Load B into xmm1
  paddusb xmm0, xmm1      // xmm0 := EnsureRange(xmm0 + xmm1, 0, 255)
  movdqu  [ecx], xmm0     // Store xmm0 into C
end;

function DistanceSquaredSIMD(const A, B: TVector4): Single;
//                                eax edx
asm
  movups  xmm0, [eax]     // Load A into xmm0 (as 4 Singles)
  movups  xmm1, [edx]     // Load B into xmm1

  // Subtract the two vectors
  subps   xmm0, xmm1      // xmm0 := xmm0 - xmm1 (4 times)

  // Calculate dot product
  mulps   xmm0, xmm0      // W*W  Z*Z  Y*Y  X*X
  pshufd  xmm1, xmm0, $0E // --   --   W*W  Z*Z
  addps   xmm0, xmm1      // --  --  (Y*Y + W*W)  (X*X + Z*Z)
  pshufd  xmm1, xmm0, $01 // --  --  --           (Y*Y + W*W)
  addss   xmm0, xmm1      //               (X*X + Z*Z) + (Y*Y + W*W)

  movss   [Result], xmm0  // Store result in return value
end;

{$ELSEIF Defined(CPUX64)}

// Windows 64 bit

procedure AddSIMD(const A, B: T16Bytes; out C: T16Bytes);
//                     rcx rdx              r8
asm
  movdqu  xmm0, [rcx]   // Load A into xmm0
  movdqu  xmm1, [rdx]   // Load B into xmm1
  paddb   xmm0, xmm1    // xmm0 := xmm0 + xmm1 (16 times)
  movdqu  [r8], xmm0    // Store xmm0 into C
end;

procedure AddAndSaturateSIMD(const A, B: T16Bytes; out C: T16Bytes);
//                                rcx rdx              r8
asm
  movdqu  xmm0, [rcx]   // Load A into xmm0
  movdqu  xmm1, [rdx]   // Load B into xmm1
  paddusb xmm0, xmm1    // xmm0 := EnsureRange(xmm0 + xmm1, 0, 255)
  movdqu  [r8], xmm0    // Store xmm0 into C
end;

function DistanceSquaredSIMD(const A, B: TVector4): Single;
//                                rcx rdx
asm
  movups  xmm0, [rcx]     // Load A into xmm0 (as 4 Singles)
  movups  xmm1, [rdx]     // Load B into xmm1

  // Subtract the two vectors
  subps   xmm0, xmm1      // xmm0 := xmm0 - xmm1 (4 times)

  // Calculate dot product
  mulps   xmm0, xmm0      // W*W  Z*Z  Y*Y  X*X
  pshufd  xmm1, xmm0, $0E // --   --   W*W  Z*Z
  addps   xmm0, xmm1      // --  --  (Y*Y + W*W)  (X*X + Z*Z)
  pshufd  xmm1, xmm0, $01 // --  --  --           (Y*Y + W*W)
  addss   xmm0, xmm1      //               (X*X + Z*Z) + (Y*Y + W*W)

  movss   [Result], xmm0  // Store result in return value
end;

{$ELSEIF Defined(CPUARM)}

// iOS (32/64 bit), Android

procedure AddSIMD(const A, B: T16Bytes; out C: T16Bytes); inline;
begin
  add_simd(@A, @B, @C);
end;

procedure AddAndSaturateSIMD(const A, B: T16Bytes; out C: T16Bytes); inline;
begin
  add_and_saturate_simd(@A, @B, @C);
end;

function DistanceSquaredSIMD(const A, B: TVector4): Single; inline;
begin
  Result := distance_squared_simd(@A, @B);
end;

{$ELSE}
  {$MESSAGE Error 'Unsupported platform'}
{$ENDIF}

{ TVector4 }

procedure TVector4.Init(const AX, AY, AZ, AW: Single);
begin
  X := AX;
  Y := AY;
  Z := AZ;
  W := AW;
end;

end.
