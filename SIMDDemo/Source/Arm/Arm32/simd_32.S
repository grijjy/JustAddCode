#if ANDROID
// For Android, we need to tell the compiler to output NEON enabled armv7-a code
    .arch armv7-a
    .fpu neon
#endif

// Tell to compiler that assembly code follows, aligned to a 4-byte boundary
    .text
    .align 4

// Import some macros

#include "../ArmCommon/macros.S"

// Assembly routines follow

PROC add_simd // (const A, B: T16Bytes; out C: T16Bytes);
              //        r0 r1               r2
    vld1.8      {q0}, [r0]     // Load A into q0
    vld1.8      {q1}, [r1]     // Load B into q1
    vadd.u8     q2, q0, q1     // q2 := q0 + q1 (16 times)
    vst1.8      {q2}, [r2]     // Store q2 into C
    bx          lr             // Return
    

PROC add_and_saturate_simd // (const A, B: T16Bytes; out C: T16Bytes);
                           //        r0 r1               r2
    vld1.8      {q0}, [r0]     // Load A into q0
    vld1.8      {q1}, [r1]     // Load B into q1
    vqadd.u8    q2, q0, q1     // q2 := EnsureRange(q0 + q1, 0, 255)
    vst1.8      {q2}, [r2]     // Store q2 into C
    bx          lr             // Return
    
    
PROC distance_squared_simd // (const A, B: TVector4): Single;
                           //        r0 r1            r0
    vld1.32     {q0}, [r0]     // Load A into q0 (as 4 Singles)
    vld1.32     {q1}, [r1]     // Load B into q1
    
    // Subtract the two vectors
    vsub.f32    q0, q0, q1     // q0 := q0 - q1 (4 times)

    // Calculate dot product
    vmul.f32    q0, q0, q0     // W*W  Z*Z  Y*Y  X*X
    vpadd.f32   d0, d0, d1     // (W*W + Z*Z) (Y*Y + X*X)
    vpadd.f32   d0, d0, d0     // -- (W*W + Z*Z) + (Y*Y + X*X)

    vmov.f32    r0, s0         // Store result in r0
    bx          lr             // Return