//! Host implementations for the compiler-rt runtime calls that native LLVM
//! codegen emits but a self-contained merged module does not define.
//!
//! The eval LLVM backend merges the (target-independent) builtins bitcode into
//! the user module and re-codegens the whole thing for the host's native
//! target. That final instruction selection lowers operations with no native
//! instruction (128-bit multiply/divide/remainder and 128-bit<->float
//! conversions) to compiler-rt libcalls (`__divti3`, `__fixsfti`, ...). Those
//! symbols are not in the builtins bitcode (they are introduced after it,
//! during native codegen), so the produced object references them as
//! undefined symbols.
//!
//! For a normally-linked program the system linker resolves these against
//! compiler-rt. The compiler's relocatable loader instead binds each such
//! symbol through `resolve` to the matching decomposed-64-bit implementation
//! already maintained for the builtins in `compiler_rt_128`, keeping the
//! loaded image self-contained without depending on the host's own
//! compiler-rt.

const std = @import("std");
const builtin = @import("builtin");
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

fn __fixsfti(a: f32) callconv(.c) i128 {
    return compiler_rt.f32_to_i128(a);
}

fn __fixdfti(a: f64) callconv(.c) i128 {
    return compiler_rt.f64_to_i128(a);
}

fn __fixunssfti(a: f32) callconv(.c) u128 {
    return compiler_rt.f32_to_u128(a);
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

/// The C memory routines and stack probes native codegen may also emit
/// calls to, bound to the definitions this binary already carries. These
/// are resolved for a loaded object but never exported by `exportLibcalls`,
/// since a linked program gets them from its own C runtime.
const host_routines = struct {
    extern fn memcpy(dest: ?[*]u8, src: ?[*]const u8, len: usize) callconv(.c) ?[*]u8;
    extern fn memmove(dest: ?[*]u8, src: ?[*]const u8, len: usize) callconv(.c) ?[*]u8;
    extern fn memset(dest: ?[*]u8, value: c_int, len: usize) callconv(.c) ?[*]u8;
    extern fn memcmp(a: ?[*]const u8, b: ?[*]const u8, len: usize) callconv(.c) c_int;
    /// LLVM's stack probe for frames past a page on Windows x64.
    extern fn ___chkstk_ms() callconv(.c) void;

    const entries = .{
        .{ "memcpy", &memcpy },
        .{ "memmove", &memmove },
        .{ "memset", &memset },
        .{ "memcmp", &memcmp },
    };
};

/// Resolve a symbol native codegen emits a call to (a compiler-rt libcall, a
/// C memory routine, or a stack probe) to its host implementation, or null
/// if it is not one we provide.
pub fn resolve(name: []const u8) ?usize {
    inline for (entries) |entry| {
        if (std.mem.eql(u8, name, entry[0])) return @intFromPtr(entry[1]);
    }
    inline for (host_routines.entries) |entry| {
        if (std.mem.eql(u8, name, entry[0])) return @intFromPtr(entry[1]);
    }
    if (builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) {
        if (std.mem.eql(u8, name, "___chkstk_ms")) return @intFromPtr(&host_routines.___chkstk_ms);
    }
    return null;
}

/// Emit every libcall in `entries` as an exported symbol under its compiler-rt
/// name, for an object that is linked into a program rather than loaded by
/// the compiler.
///
/// `linkage` is `.weak` for an object that only needs these when nothing else
/// in the link supplies them: a strong definition elsewhere then wins, which
/// COFF requires, since it rejects two strong definitions of `__udivti3`
/// outright where ELF would pick one.
pub fn exportLibcalls(comptime linkage: std.builtin.GlobalLinkage) void {
    inline for (entries) |entry| {
        @export(entry[1], .{ .name = entry[0], .linkage = linkage });
    }
}

test "resolve maps known compiler-rt symbols and rejects others" {
    try std.testing.expect(resolve("__divti3") != null);
    try std.testing.expect(resolve("__fixsfti") != null);
    try std.testing.expect(resolve("__floatuntidf") != null);
    try std.testing.expect(resolve("not_a_runtime_symbol") == null);
    try std.testing.expect(resolve(@import("builtin_registry.zig").BuiltinFn.float_tan.symbolName()) == null);
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
