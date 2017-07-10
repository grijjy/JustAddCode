// Tell to compiler that assembly code follows, aligned to a 4-byte boundary
    .text
    .align 4

// Import some macros

#include "../ArmCommon/macros.S"

// Assembly routines follow

PROC add_simd // (const A, B: T16Bytes; out C: T16Bytes);
              //        x0 x1               x2
    ld1.16b     {v0}, [x0]     // Load A into v0
    ld1.16b     {v1}, [x1]     // Load B into v1
    add.16b     v2, v0, v1     // v2 := v0 + v1 (16 times)
    st1.16b     {v2}, [x2]     // Store v2 into C
    ret                        // Return
    

PROC add_and_saturate_simd // (const A, B: T16Bytes; out C: T16Bytes);
                           //        x0 x1               x2
    ld1.16b     {v0}, [x0]     // Load A into v0
    ld1.16b     {v1}, [x1]     // Load B into v1
    uqadd.16b   v2, v0, v1     // v2 := EnsureRange(v0 + v1, 0, 255)
    st1.16b     {v2}, [x2]     // Store v2 into C
    ret                        // Return
    
    
PROC distance_squared_simd // (const A, B: TVector4): Single;
                           //        x0 x1            v0
    ld1.4s      {v0}, [x0]     // Load A into v0 (as 4 Singles) 
    ld1.4s      {v1}, [x1]     // Load B into v1
    
    // Subtract the two vectors
    fsub.4s     v0, v0, v1     // v0 := v0 - v1 (4 times)
    fmul.4s     v0, v0, v0     // W*W  Z*Z  Y*Y  X*X
    faddp.4s    v0, v0, v0     // --  --  (W*W + Z*Z) (Y*Y + X*X)
    faddp.4s    v0, v0, v0     // --  --  --  (W*W + Z*Z) + (Y*Y + X*X)
    
    // Function result is stored in lowest Single in v0
    ret                        // Return