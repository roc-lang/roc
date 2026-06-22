//! Standalone object root that exports the compiler-rt 128-bit libcalls native
//! LLVM codegen emits (`__divti3`, `__fixdfti`, ...) as real symbols.
//!
//! The eval LLVM backend re-codegens the merged app+builtins module for the
//! host target; that final instruction selection lowers 128-bit multiply/
//! divide/remainder and 128-bit<->float conversions to compiler-rt libcalls
//! that are not defined anywhere in the produced image. On Unix the eval image
//! is loaded by a dynamic loader (or the in-process `eval_loader`, which binds
//! them via `native_runtime_libcalls.resolve`), so the shared object can leave
//! them undefined. Windows loads the image with `LoadLibrary`, which only
//! accepts a fully linked DLL, so the eval linker links this object in to
//! satisfy those references. The implementations are the same decomposed-to-
//! 64-bit ones the in-process loader hands out, kept once in `compiler_rt_128`.

const std = @import("std");
const native_runtime_libcalls = @import("native_runtime_libcalls.zig");

pub const panic = std.debug.no_panic;

comptime {
    native_runtime_libcalls.exportLibcalls();
}
