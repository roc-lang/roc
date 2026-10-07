//! Host bindings for the runtime calls that native LLVM codegen emits but a
//! self-contained merged module does not define.
//!
//! The eval LLVM backend merges the (target-independent) builtins bitcode into
//! the user module and re-codegens the whole thing for the host's native
//! target. That final instruction selection lowers operations with no native
//! instruction to calls: 128-bit multiply/divide/remainder and 128-bit<->float
//! conversions to compiler-rt libcalls (`__divti3`, `__fixsfti`, ...), a float
//! remainder to `fmod`, float rounding on a CPU without the instruction to
//! `floor` and its siblings, and block copies and fills to the C memory
//! routines. Those symbols are not in the builtins bitcode (they are
//! introduced after it, during native codegen), so the produced object
//! references them as undefined symbols.
//!
//! For a normally-linked program the linker resolves these against the
//! platform's C runtime or the default platform's compiler-rt carrier. The
//! compiler's relocatable loader instead binds each such symbol through
//! `resolve`. A compiler-rt arithmetic helper binds to the matching
//! decomposed-64-bit implementation already maintained for the builtins in
//! `compiler_rt_128`, keeping the loaded image independent of the host's own
//! compiler-rt. A C routine binds to the definition this binary carries; the
//! set of those is `shim_symbols.c_memory_set` and `shim_symbols.c_math_set`,
//! the same names a platform's C runtime owes compiled Roc code.

const std = @import("std");
const builtin = @import("builtin");
const compiler_rt = @import("compiler_rt_128.zig");
const shim_symbols = @import("shim_symbols.zig");

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

/// The routines native codegen may also emit calls to that only some targets
/// name, bound to the definitions this binary already carries. Like the C
/// routines `resolve` binds, they are resolved for a loaded object but never
/// exported by `exportLibcalls`, since a linked program gets them from its own
/// C runtime.
const host_routines = struct {
    /// Apple targets lower a zero-filling `memset` to `bzero`.
    extern fn bzero(dest: ?[*]u8, len: usize) callconv(.c) void;
    /// LLVM's stack probe for frames past a page on Windows x64.
    extern fn ___chkstk_ms() callconv(.c) void;
};

/// The C memory and math routines native codegen may emit calls to, exactly
/// the ones the boundary contract names. A loaded object's reference to one
/// binds to the definition this binary already carries.
const c_routines = shim_symbols.c_memory_set ++ shim_symbols.c_math_set;

/// Resolve a symbol native codegen emits a call to (a compiler-rt libcall, a
/// C memory or math routine, or a stack probe) to its host implementation, or
/// null if it is not one we provide.
pub fn resolve(name: []const u8) ?usize {
    inline for (entries) |entry| {
        if (std.mem.eql(u8, name, entry[0])) return @intFromPtr(entry[1]);
    }
    inline for (c_routines) |routine| {
        if (std.mem.eql(u8, name, routine)) return @intFromPtr(@extern(*const anyopaque, .{ .name = routine }));
    }
    if (builtin.os.tag.isDarwin()) {
        // Darwin codegen also emits the libc __bzero entry point.
        if (std.mem.eql(u8, name, "bzero") or std.mem.eql(u8, name, "__bzero")) return @intFromPtr(&host_routines.bzero);
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

test "resolve binds every C routine the boundary contract names" {
    inline for (shim_symbols.c_memory_set ++ shim_symbols.c_math_set) |routine| {
        try std.testing.expect(resolve(routine) != null);
    }
}

test "resolved C math routines compute float remainder and rounding" {
    const Binary64 = *const fn (f64, f64) callconv(.c) f64;
    const Binary32 = *const fn (f32, f32) callconv(.c) f32;
    const Unary64 = *const fn (f64) callconv(.c) f64;
    const Unary32 = *const fn (f32) callconv(.c) f32;

    const fmod: Binary64 = @ptrFromInt(resolve("fmod").?);
    const fmodf: Binary32 = @ptrFromInt(resolve("fmodf").?);
    // The remainder keeps the sign of the dividend.
    try std.testing.expectEqual(@as(f64, 1.5), fmod(7.5, 2.0));
    try std.testing.expectEqual(@as(f64, -1.5), fmod(-7.5, 2.0));
    try std.testing.expectEqual(@as(f32, 1.5), fmodf(7.5, 2.0));
    try std.testing.expectEqual(@as(f32, -1.5), fmodf(-7.5, 2.0));

    inline for (.{
        .{ "floor", "floorf", -8.0 },
        .{ "ceil", "ceilf", -7.0 },
        .{ "trunc", "truncf", -7.0 },
    }) |case| {
        const wide: Unary64 = @ptrFromInt(resolve(case[0]).?);
        const narrow: Unary32 = @ptrFromInt(resolve(case[1]).?);
        try std.testing.expectEqual(@as(f64, case[2]), wide(-7.5));
        try std.testing.expectEqual(@as(f32, case[2]), narrow(-7.5));
    }

    const sqrt: Unary64 = @ptrFromInt(resolve("sqrt").?);
    const sqrtf: Unary32 = @ptrFromInt(resolve("sqrtf").?);
    try std.testing.expectEqual(@as(f64, 1.5), sqrt(2.25));
    try std.testing.expectEqual(@as(f32, 1.5), sqrtf(2.25));
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

test "Darwin zero-fill libcalls clear exactly the requested bytes" {
    if (!builtin.os.tag.isDarwin()) return error.SkipZigTest;
    for ([_][]const u8{ "bzero", "__bzero" }) |name| {
        const zero: *const fn (?[*]u8, usize) callconv(.c) void = @ptrFromInt(resolve(name).?);
        var bytes = [_]u8{0xaa} ** 8;
        zero(bytes[2..].ptr, 4);
        try std.testing.expectEqualSlices(u8, &.{ 0xaa, 0xaa, 0, 0, 0, 0, 0xaa, 0xaa }, &bytes);
    }
}
