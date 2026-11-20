//! Private libcall support for Linux and macOS machine-code shims.
//! Integer, shift and EABI helpers are unmodified Zig toolchain copies; integer/float
//! conversion cores are an attributed repository copy without registration.
//! Explicit helpers have local binding and cannot be supplied or interposed
//! by the platform.
const builtin = @import("builtin");
const std = @import("std");
const int = @import("compiler_rt/int.zig");
const arm = @import("compiler_rt/arm.zig");
/// Unmodified upstream entries, also exercised by the dedicated arithmetic test root.
pub const shift = @import("compiler_rt/shift.zig");
const float_from_int = @import("float_int_conversion.zig");
const int_from_float = @import("float_int_conversion.zig");

/// Use the toolchain's ARM EABI implementations on ARM Linux.
pub const want_aeabi = builtin.cpu.arch.isArm();
/// This private runtime is not used by Windows shims.
pub const want_windows_arm_abi = false;
/// Match the toolchain's runtime arithmetic rather than its test instrumentation.
pub const test_safety = false;

/// Upstream modules must not register their public compiler-rt exports.
pub fn symbol(comptime _: *const anyopaque, comptime _: []const u8) void {}

const integer_helpers = .{
    .{ "__udivdi3", &int.__udivdi3 },
    .{ "__umoddi3", &int.__umoddi3 },
    .{ "__divdi3", &int.__divdi3 },
    .{ "__moddi3", &int.__moddi3 },
    .{ "__udivmoddi4", &int.__udivmoddi4 },
    .{ "__divmoddi4", &int.__divmoddi4 },
};
const arm_helpers = .{
    .{ "__aeabi_idiv", &int.__divsi3 },
    .{ "__aeabi_uidiv", &int.__udivsi3 },
    .{ "__aeabi_idivmod", &arm.__aeabi_idivmod },
    .{ "__aeabi_uidivmod", &arm.__aeabi_uidivmod },
    .{ "__aeabi_ldivmod", &arm.__aeabi_ldivmod },
    .{ "__aeabi_uldivmod", &arm.__aeabi_uldivmod },
    .{ "__aeabi_memcpy", &arm.__aeabi_memcpy },
    .{ "__aeabi_memcpy4", &arm.__aeabi_memcpy4 },
    .{ "__aeabi_memcpy8", &arm.__aeabi_memcpy8 },
    .{ "__aeabi_memmove", &arm.__aeabi_memmove },
    .{ "__aeabi_memmove4", &arm.__aeabi_memmove4 },
    .{ "__aeabi_memmove8", &arm.__aeabi_memmove8 },
    .{ "__aeabi_memset", &arm.__aeabi_memset },
    .{ "__aeabi_memset4", &arm.__aeabi_memset4 },
    .{ "__aeabi_memset8", &arm.__aeabi_memset8 },
    .{ "__aeabi_memclr", &arm.__aeabi_memclr },
    .{ "__aeabi_memclr4", &arm.__aeabi_memclr4 },
    .{ "__aeabi_memclr8", &arm.__aeabi_memclr8 },
    .{ "__aeabi_ul2d", &ul2d },
    .{ "__aeabi_ul2f", &ul2f },
    .{ "__aeabi_d2lz", &d2lz },
    .{ "__aeabi_d2ulz", &d2ulz },
    .{ "__aeabi_f2lz", &f2lz },
    .{ "__aeabi_f2ulz", &f2ulz },
};
// The checked Zig 0.17.0 ReleaseSmall matrix needs no shift helpers on
// AArch64 macOS: its archive has no unresolved shift libcalls. Keep that
// target in the verifier matrix so a toolchain change exposes a new requirement.
const shift_helpers = if (want_aeabi) .{
    .{ "__aeabi_llsl", &llsl },
    .{ "__aeabi_lasr", &lasr },
    .{ "__aeabi_llsr", &llsr },
} else if (@bitSizeOf(usize) == 32) .{
    .{ "__ashldi3", &shift.__ashldi3 },
    .{ "__ashrdi3", &shift.__ashrdi3 },
    .{ "__lshrdi3", &shift.__lshrdi3 },
} else if (builtin.os.tag == .linux or builtin.cpu.arch == .x86_64) .{
    .{ "__ashlti3", &shift.__ashlti3 },
    .{ "__ashrti3", &shift.__ashrti3 },
    .{ "__lshrti3", &shift.__lshrti3 },
} else .{};
const helpers = (if (@bitSizeOf(usize) == 32) integer_helpers else .{}) ++
    (if (want_aeabi) arm_helpers else .{}) ++ shift_helpers;

// EABI uses core registers on hard-float targets as well. Upstream keeps its
// EABI entries private; these adapters reuse its public integer C entries.
fn llsl(a: i64, b: i32) callconv(.{ .arm_aapcs = .{} }) i64 {
    return shift.__ashldi3(a, b);
}
fn lasr(a: i64, b: i32) callconv(.{ .arm_aapcs = .{} }) i64 {
    return shift.__ashrdi3(a, b);
}
fn llsr(a: i64, b: i32) callconv(.{ .arm_aapcs = .{} }) i64 {
    return shift.__lshrdi3(a, b);
}

// AAPCS libcalls use core registers even on hard-float targets. The upstream
// public C wrappers use the target C convention, so adapt only the ABI here.
fn ul2d(a: u64) callconv(.{ .arm_aapcs = .{} }) f64 {
    return float_from_int.f64_floatFromInt_u64(a);
}
fn ul2f(a: u64) callconv(.{ .arm_aapcs = .{} }) f32 {
    return float_from_int.f32_floatFromInt_u64(a);
}
fn d2lz(a: f64) callconv(.{ .arm_aapcs = .{} }) i64 {
    return int_from_float.i64_intFromFloat_f64(a);
}
fn d2ulz(a: f64) callconv(.{ .arm_aapcs = .{} }) u64 {
    return int_from_float.u64_intFromFloat_f64(a);
}
fn f2lz(a: f32) callconv(.{ .arm_aapcs = .{} }) i64 {
    return int_from_float.i64_intFromFloat_f32(a);
}
fn f2ulz(a: f32) callconv(.{ .arm_aapcs = .{} }) u64 {
    return int_from_float.u64_intFromFloat_f32(a);
}

comptime {
    const Shift64 = fn (i64, i32) callconv(.c) i64;
    const Shift128 = fn (i128, i32) callconv(.c) i128;
    std.debug.assert(@TypeOf(shift.__ashldi3) == Shift64);
    std.debug.assert(@TypeOf(shift.__ashrdi3) == Shift64);
    std.debug.assert(@TypeOf(shift.__lshrdi3) == Shift64);
    std.debug.assert(@TypeOf(shift.__ashlti3) == Shift128);
    std.debug.assert(@TypeOf(shift.__ashrti3) == Shift128);
    std.debug.assert(@TypeOf(shift.__lshrti3) == Shift128);
    if (want_aeabi) {
        const Shift = fn (i64, i32) callconv(.{ .arm_aapcs = .{} }) i64;
        std.debug.assert(@TypeOf(llsl) == Shift);
        std.debug.assert(@TypeOf(lasr) == Shift);
        std.debug.assert(@TypeOf(llsr) == Shift);
        std.debug.assert(@TypeOf(ul2d) == fn (u64) callconv(.{ .arm_aapcs = .{} }) f64);
        std.debug.assert(@TypeOf(ul2f) == fn (u64) callconv(.{ .arm_aapcs = .{} }) f32);
        std.debug.assert(@TypeOf(d2lz) == fn (f64) callconv(.{ .arm_aapcs = .{} }) i64);
        std.debug.assert(@TypeOf(d2ulz) == fn (f64) callconv(.{ .arm_aapcs = .{} }) u64);
        std.debug.assert(@TypeOf(f2lz) == fn (f32) callconv(.{ .arm_aapcs = .{} }) i64);
        std.debug.assert(@TypeOf(f2ulz) == fn (f32) callconv(.{ .arm_aapcs = .{} }) u64);
    }
}

/// Supply the upstream division implementation's exact integer-halving contract.
pub fn HalveInt(comptime T: type, comptime signed_half: bool) type {
    return extern union {
        pub const bits = @divExact(@typeInfo(T).int.bits, 2);
        pub const HalfTU = @Int(.unsigned, bits);
        pub const HalfTS = @Int(.signed, bits);
        pub const HalfT = if (signed_half) HalfTS else HalfTU;
        all: T,
        s: if (builtin.cpu.arch.endian() == .little)
            extern struct { low: HalfT, high: HalfT }
        else
            extern struct { high: HalfT, low: HalfT },
    };
}

/// Keep compiler-inserted libcalls bound locally even after LLVM optimization.
pub inline fn retain() void {
    inline for (helpers) |helper| {
        // LLVM introduces libcalls after dead-code elimination, which removes
        // unreferenced internal @export aliases. Name the retained definition
        // in the assembler instead, after LLVM's symbol optimization.
        asm volatile ((if (builtin.os.tag == .macos)
                "_" ++ helper[0] ++ " = %[helper:P]"
            else
                ".local " ++ helper[0] ++ "\n.set " ++ helper[0] ++ ", " ++
                    (if (builtin.cpu.arch == .x86 or builtin.cpu.arch == .x86_64) "%[helper:P]" else "%[helper]"))
            :
            : [helper] "X" (helper[1]),
        );
    }
}
