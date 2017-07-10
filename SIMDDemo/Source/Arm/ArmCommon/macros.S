#if ANDROID

// The macro PROC is used for Android to define a function/procedure. It has a 
// single parameter ProcName with the name of the procedure. The macro makes 
// this name global, so you can link to it from Delphi. Then it starts a label
// with the name of the routine.

.macro PROC ProcName
    .globl \ProcName
    \ProcName:
.endm

#else

// The PROC macro for iOS is slightly different. On iOS, global names must start
// with an underscore.

.macro PROC ProcName
    .globl _\ProcName
    _\ProcName:
.endm

#endif