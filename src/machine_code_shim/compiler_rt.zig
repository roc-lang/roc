//! Private 32-bit libcall support for the machine-code shim.
//! The build places unmodified Zig compiler-rt sources beside this root.
//! The explicit helpers below have local binding and cannot be supplied or
//! interposed by the platform. Arithmetic remains owned by the Zig toolchain.
const std = @import("std");
const builtin = @import("builtin");
const int = @import("compiler_rt/int.zig");
const arm = @import("compiler_rt/arm.zig");

/// Use the toolchain's ARM EABI implementations on ARM Linux.
pub const want_aeabi = builtin.cpu.arch.isArm();
/// This private runtime is not used by Windows shims.
pub const want_windows_arm_abi = false;
/// This private runtime is not used by Windows shims.
pub const want_windows_v2u64_abi = false;
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
const helpers = integer_helpers ++ (if (want_aeabi) arm_helpers else .{});

// AAPCS libcalls use core registers even on hard-float targets. The upstream
// public C wrappers use the target C convention, so adapt only the ABI here.
fn ul2d(a: u64) callconv(.{ .arm_aapcs = .{} }) f64 {
    return @import("compiler_rt/floatundidf.zig").__floatundidf(a);
}
fn ul2f(a: u64) callconv(.{ .arm_aapcs = .{} }) f32 {
    return @import("compiler_rt/floatundisf.zig").__floatundisf(a);
}
fn d2lz(a: f64) callconv(.{ .arm_aapcs = .{} }) i64 {
    return @import("compiler_rt/fixdfdi.zig").__fixdfdi(a);
}
fn d2ulz(a: f64) callconv(.{ .arm_aapcs = .{} }) u64 {
    return @import("compiler_rt/fixunsdfdi.zig").__fixunsdfdi(a);
}
fn f2lz(a: f32) callconv(.{ .arm_aapcs = .{} }) i64 {
    return @import("compiler_rt/fixsfdi.zig").__fixsfdi(a);
}
fn f2ulz(a: f32) callconv(.{ .arm_aapcs = .{} }) u64 {
    return @import("compiler_rt/fixunssfdi.zig").__fixunssfdi(a);
}

/// Supply the upstream division implementation's exact integer-halving contract.
pub fn HalveInt(comptime T: type, comptime signed_half: bool) type {
    return extern union {
        pub const bits = @divExact(@typeInfo(T).int.bits, 2);
        pub const HalfTU = std.meta.Int(.unsigned, bits);
        pub const HalfTS = std.meta.Int(.signed, bits);
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
        asm volatile (".local " ++ helper[0] ++ "\n.set " ++ helper[0] ++ ", " ++
                (if (want_aeabi) "%[helper]" else "%[helper:P]")
            :
            : [helper] "X" (helper[1]),
        );
    }
}
