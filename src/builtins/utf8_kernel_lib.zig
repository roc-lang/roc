//! The UTF-8 validation function the LLVM builtins bitcode links in for its
//! target. The bitcode is compiled for wasm64 or wasm32 and retargeted, so it
//! cannot pick its kernel when it is compiled; build.zig compiles this file
//! once per `utf8.Kernel` and pointer width (for wasm64 with the DFA, for
//! x86_64 with SSSE3, for aarch64 with NEON, for WebAssembly 1.0 with the DFA,
//! and for wasm32 with simd128), and the LLVM backend links the one the
//! target's CPU features select.

const std = @import("std");
const builtin = @import("builtin");
const utf8 = @import("utf8.zig");

/// Builtin payloads must not pull in Zig's panic formatting machinery.
pub const panic = std.debug.no_panic;

/// The kernel this payload carries: the one its compile target's CPU features
/// select. build.zig compiles each payload for exactly the features its
/// kernel needs, so they are a promise about every target that links it.
const kernel = utf8.Kernel.forCpu(builtin.cpu);

fn validate(bytes: [*]const u8, len: usize, index: *usize) callconv(.c) u32 {
    return utf8.validateAbi(kernel, bytes, len, index);
}

comptime {
    const abi: *const utf8.ValidateAbi = &validate;
    @export(abi, .{ .name = utf8.validate_symbol });
}
