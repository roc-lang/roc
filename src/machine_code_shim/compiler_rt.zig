//! Private 32-bit libcall support for the machine-code shim.
//! The build places unmodified Zig compiler-rt sources beside this root.
//! The explicit helpers below have local binding and cannot be supplied or
//! interposed by the platform. Arithmetic remains owned by the Zig toolchain.
const builtin = @import("builtin");
const std = @import("std");
const int = @import("compiler_rt/int.zig");
const arm = @import("compiler_rt/arm.zig");
const float_from_int = @import("compiler_rt/float_from_int.zig");
const int_from_float = @import("compiler_rt/int_from_float.zig");

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

// The upstream conversion modules resolve the full set of their private
// exports against this root, so it declares the target-ABI facts they read.
// Only the pure `*_floatFromInt_*`/`*_intFromFloat_*` helpers are called.
/// This private runtime does not serve PowerPC.
pub const want_ppc_abi = false;
/// This private runtime does not serve SPARC.
pub const want_sparc64_abi = false;
/// This private runtime does not serve SPARC.
pub const want_sparc32_abi = false;

fn FloatAbi(comptime Float: type) type {
    return switch (std.zig.target.compilerRtFloatAbi(&builtin.target, @bitSizeOf(Float))) {
        .hard => struct {
            pub const Abi = Float;
            pub inline fn toAbi(raw: Float) Abi {
                return raw;
            }
            pub inline fn fromAbi(abi: Abi) Float {
                return abi;
            }
        },
        .soft => struct {
            pub const Abi = @Int(.unsigned, @bitSizeOf(Float));
            pub inline fn toAbi(raw: Float) Abi {
                return @bitCast(raw);
            }
            pub inline fn fromAbi(abi: Abi) Float {
                return @bitCast(abi);
            }
        },
    };
}
/// Calling-convention carriers read by the upstream conversion modules.
pub const @"f16" = FloatAbi(f16);
/// Calling-convention carriers read by the upstream conversion modules.
pub const @"f32" = FloatAbi(f32);
/// Calling-convention carriers read by the upstream conversion modules.
pub const @"f64" = FloatAbi(f64);
/// Calling-convention carriers read by the upstream conversion modules.
pub const @"f80" = switch (std.zig.target.compilerRtFloatAbi(&builtin.target, 80)) {
    .hard => FloatAbi(f80),
    .soft => struct {
        pub const Abi = extern struct { mantissa: u64, exponent: u16 };
        const Repr = packed struct { mantissa: u64, exponent: u16 };
        pub inline fn toAbi(raw: f80) Abi {
            const repr: Repr = @bitCast(raw);
            return .{ .mantissa = repr.mantissa, .exponent = repr.exponent };
        }
        pub inline fn fromAbi(abi: Abi) f80 {
            const repr: Repr = .{ .mantissa = abi.mantissa, .exponent = abi.exponent };
            return @bitCast(repr);
        }
    },
};
/// Calling-convention carriers read by the upstream conversion modules.
pub const @"f128" = switch (std.zig.target.compilerRtFloatAbi(&builtin.target, 128)) {
    .hard => FloatAbi(f128),
    .soft => struct {
        pub const Abi = switch (builtin.cpu.arch.endian()) {
            .big => extern struct { hi: u64, lo: u64 },
            .little => extern struct { lo: u64, hi: u64 },
        };
        const Repr = packed struct { lo: u64, hi: u64 };
        pub inline fn toAbi(raw: f128) Abi {
            const repr: Repr = @bitCast(raw);
            return .{ .lo = repr.lo, .hi = repr.hi };
        }
        pub inline fn fromAbi(abi: Abi) f128 {
            const repr: Repr = .{ .lo = abi.lo, .hi = abi.hi };
            return @bitCast(repr);
        }
    },
};

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
        asm volatile (".local " ++ helper[0] ++ "\n.set " ++ helper[0] ++ ", " ++
                (if (want_aeabi) "%[helper]" else "%[helper:P]")
            :
            : [helper] "X" (helper[1]),
        );
    }
}
