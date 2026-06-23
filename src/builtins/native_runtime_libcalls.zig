//! Host implementations for the compiler-rt runtime calls that native LLVM
//! codegen emits but a self-contained merged module does not define.
//!
//! The eval LLVM backend merges the (target-independent) builtins bitcode into
//! the user module and re-codegens the whole thing for the host's native
//! target. That final instruction selection lowers operations with no native
//! instruction — 128-bit multiply/divide/remainder and 128-bit<->float
//! conversions — to compiler-rt libcalls (`__divti3`, `__fixsfti`, ...). Those
//! symbols are not in the builtins bitcode (they are introduced *after* it,
//! during native codegen), so the produced shared object references them as
//! *undefined* symbols.
//!
//! For a normally-linked program the system linker resolves these against
//! compiler-rt. The eval path instead loads the object with a minimal
//! in-process loader (`eval_loader`) in a static, no-libc binary that has no
//! dynamic linker to bind undefined symbols. `resolve` lets that loader bind
//! each such symbol to the matching decomposed-64-bit implementation already
//! maintained for the builtins in `compiler_rt_128`, keeping the loaded image
//! self-contained without depending on the host's own compiler-rt.

const std = @import("std");
const compiler_rt = @import("compiler_rt_128.zig");

// `callconv(.c)` wrappers matching each compiler-rt symbol's ABI. The
// underlying implementations decompose to 64-bit arithmetic only, so
// re-codegen for the native target never re-introduces the same libcall.

fn __multi3(a: i128, b: i128) callconv(.c) i128 {
    return compiler_rt.mul_i128(a, b);
}

fn __muloti4(a: i128, b: i128, overflow: *c_int) callconv(.c) i128 {
    return compiler_rt.mulWithOverflow_i128(a, b, overflow);
}

fn __divti3(a: i128, b: i128) callconv(.c) i128 {
    return compiler_rt.divTrunc_i128(a, b);
}

fn __udivti3(a: u128, b: u128) callconv(.c) u128 {
    return compiler_rt.divTrunc_u128(a, b);
}

fn __modti3(a: i128, b: i128) callconv(.c) i128 {
    return compiler_rt.rem_i128(a, b);
}

fn __umodti3(a: u128, b: u128) callconv(.c) u128 {
    return compiler_rt.rem_u128(a, b);
}

// f32 widens to f64 losslessly, so the f32 conversions route through the f64
// implementation, which already saturates out-of-range inputs like compiler-rt.
fn __fixsfti(a: f32) callconv(.c) i128 {
    return compiler_rt.f64_to_i128(a);
}

fn __fixdfti(a: f64) callconv(.c) i128 {
    return compiler_rt.f64_to_i128(a);
}

fn __fixunssfti(a: f32) callconv(.c) u128 {
    return compiler_rt.f64_to_u128(a);
}

fn __fixunsdfti(a: f64) callconv(.c) u128 {
    return compiler_rt.f64_to_u128(a);
}

fn __floattisf(a: i128) callconv(.c) f32 {
    return compiler_rt.i128_to_f32(a);
}

fn __floattidf(a: i128) callconv(.c) f64 {
    return compiler_rt.i128_to_f64(a);
}

fn __floatuntisf(a: u128) callconv(.c) f32 {
    return compiler_rt.u128_to_f32(a);
}

fn __floatuntidf(a: u128) callconv(.c) f64 {
    return compiler_rt.u128_to_f64(a);
}

// The closed set of compiler-rt symbols x86_64/aarch64 instruction selection
// emits for Roc's 128-bit integer and 128-bit<->float operations, paired with
// their host implementation. (8-, 16-, 32- and 64-bit arithmetic all have
// native instructions and never become libcalls.)
const entries = .{
    .{ "__multi3", &__multi3 },
    .{ "__muloti4", &__muloti4 },
    .{ "__divti3", &__divti3 },
    .{ "__udivti3", &__udivti3 },
    .{ "__modti3", &__modti3 },
    .{ "__umodti3", &__umodti3 },
    .{ "__fixsfti", &__fixsfti },
    .{ "__fixdfti", &__fixdfti },
    .{ "__fixunssfti", &__fixunssfti },
    .{ "__fixunsdfti", &__fixunsdfti },
    .{ "__floattisf", &__floattisf },
    .{ "__floattidf", &__floattidf },
    .{ "__floatuntisf", &__floatuntisf },
    .{ "__floatuntidf", &__floatuntidf },
};

/// Resolve a compiler-rt runtime symbol to its host implementation, or null if
/// it is not one we provide. The signature matches
/// `base.elf_self_relocate.UndefinedSymbolResolver` so it can be handed to the
/// in-process eval loader.
pub fn resolve(name: []const u8) ?usize {
    inline for (entries) |entry| {
        if (std.mem.eql(u8, name, entry[0])) return @intFromPtr(entry[1]);
    }
    return null;
}

/// Emit every libcall in `entries` as an exported symbol under its compiler-rt
/// name. The standalone `eval_compiler_rt_libcalls` object calls this from a
/// `comptime` block so it can be linked into the eval shared library on targets
/// whose loader cannot bind undefined symbols at load time. The in-process
/// `eval_loader` (static-musl Linux) and the OS dynamic loader (other Unixes)
/// bind them via `resolve` / their own compiler-rt instead, but Windows loads
/// the eval image with `LoadLibrary`, which requires a fully linked DLL.
pub fn exportLibcalls() void {
    inline for (entries) |entry| {
        @export(entry[1], .{ .name = entry[0] });
    }
}

test "resolve maps known compiler-rt symbols and rejects others" {
    try std.testing.expect(resolve("__divti3") != null);
    try std.testing.expect(resolve("__fixsfti") != null);
    try std.testing.expect(resolve("__floatuntidf") != null);
    try std.testing.expect(resolve("not_a_runtime_symbol") == null);
    try std.testing.expect(resolve("roc_builtins_float_tan") == null);
}

test "resolved division and remainder match native i128 arithmetic" {
    const divti3: *const fn (i128, i128) callconv(.c) i128 = @ptrFromInt(resolve("__divti3").?);
    const modti3: *const fn (i128, i128) callconv(.c) i128 = @ptrFromInt(resolve("__modti3").?);

    const min = std.math.minInt(i128);
    try std.testing.expectEqual(@as(i128, -3), divti3(7, -2));
    try std.testing.expectEqual(@as(i128, 1), modti3(7, -2));
    // I128.div_try(I128.lowest, -1) overflows to lowest under truncating wrap.
    try std.testing.expectEqual(min, divti3(min, -1));
}
