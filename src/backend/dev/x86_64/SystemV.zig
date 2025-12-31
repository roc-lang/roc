//! System V AMD64 ABI calling convention.
//!
//! Used on Linux, macOS, FreeBSD, and other Unix-like systems.
//! See: https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf

const std = @import("std");
const Registers = @import("Registers.zig");
const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;

/// System V AMD64 ABI calling convention
pub const SystemV = @This();

/// Base pointer register (frame pointer)
pub const BASE_PTR_REG: GeneralReg = .RBP;

/// Stack pointer register
pub const STACK_PTR_REG: GeneralReg = .RSP;

/// Registers used for passing integer/pointer arguments (in order)
pub const GENERAL_PARAM_REGS = [_]GeneralReg{
    .RDI, // 1st argument
    .RSI, // 2nd argument
    .RDX, // 3rd argument
    .RCX, // 4th argument
    .R8, // 5th argument
    .R9, // 6th argument
};

/// Registers used for returning integer/pointer values
pub const GENERAL_RETURN_REGS = [_]GeneralReg{
    .RAX, // Primary return value
    .RDX, // Secondary return value (for 128-bit returns)
};

/// Registers used for passing floating-point arguments
pub const FLOAT_PARAM_REGS = [_]FloatReg{
    .XMM0, .XMM1, .XMM2, .XMM3,
    .XMM4, .XMM5, .XMM6, .XMM7,
};

/// Registers used for returning floating-point values
pub const FLOAT_RETURN_REGS = [_]FloatReg{
    .XMM0, .XMM1,
};

/// Caller-saved (volatile) general registers
/// These registers may be modified by a called function
pub const CALLER_SAVED_GENERAL = [_]GeneralReg{
    .RAX, .RCX, .RDX, .RSI, .RDI, .R8, .R9, .R10, .R11,
};

/// Callee-saved (non-volatile) general registers
/// Called functions must preserve these registers
pub const CALLEE_SAVED_GENERAL = [_]GeneralReg{
    .RBX, .R12, .R13, .R14, .R15,
    // RBP is also callee-saved but handled specially as frame pointer
};

/// Caller-saved floating-point registers
pub const CALLER_SAVED_FLOAT = [_]FloatReg{
    .XMM0,  .XMM1,  .XMM2,  .XMM3,  .XMM4,  .XMM5,  .XMM6,  .XMM7,
    .XMM8,  .XMM9,  .XMM10, .XMM11, .XMM12, .XMM13, .XMM14, .XMM15,
};

/// No shadow space required (unlike Windows)
pub const SHADOW_SPACE_SIZE: u8 = 0;

/// Stack alignment requirement (16 bytes)
pub const STACK_ALIGNMENT: u8 = 16;

/// Red zone size (128 bytes below RSP that can be used without adjusting RSP)
pub const RED_ZONE_SIZE: u8 = 128;

/// Check if a general register is callee-saved
pub fn isCalleeSaved(reg: GeneralReg) bool {
    return switch (reg) {
        .RBX, .RBP, .R12, .R13, .R14, .R15 => true,
        else => false,
    };
}

/// Check if a float register is callee-saved
/// Note: In System V, NO XMM registers are callee-saved
pub fn isFloatCalleeSaved(_: FloatReg) bool {
    return false;
}

/// Default free registers for allocation (ordered by preference)
/// Put callee-saved regs last to minimize saves
pub const DEFAULT_FREE_GENERAL_REGS = [_]GeneralReg{
    // Caller-saved first (no save/restore needed)
    .R11, .R10, .R9, .R8,
    .RCX, .RDX, .RSI, .RDI,
    .RAX,
    // Callee-saved last (need save/restore)
    .R15, .R14, .R13, .R12, .RBX,
};

pub const DEFAULT_FREE_FLOAT_REGS = [_]FloatReg{
    .XMM15, .XMM14, .XMM13, .XMM12, .XMM11, .XMM10, .XMM9, .XMM8,
    .XMM7,  .XMM6,  .XMM5,  .XMM4,  .XMM3,  .XMM2,  .XMM1, .XMM0,
};

/// Bitmask of caller-saved general registers (for fast allocation)
/// RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11
pub const CALLER_SAVED_GENERAL_MASK: u32 =
    (1 << @intFromEnum(GeneralReg.RAX)) |
    (1 << @intFromEnum(GeneralReg.RCX)) |
    (1 << @intFromEnum(GeneralReg.RDX)) |
    (1 << @intFromEnum(GeneralReg.RSI)) |
    (1 << @intFromEnum(GeneralReg.RDI)) |
    (1 << @intFromEnum(GeneralReg.R8)) |
    (1 << @intFromEnum(GeneralReg.R9)) |
    (1 << @intFromEnum(GeneralReg.R10)) |
    (1 << @intFromEnum(GeneralReg.R11));

/// Bitmask of caller-saved float registers (all XMM0-XMM15 are caller-saved)
pub const CALLER_SAVED_FLOAT_MASK: u32 = 0xFFFF;

test "calling convention constants" {
    try std.testing.expectEqual(GeneralReg.RBP, BASE_PTR_REG);
    try std.testing.expectEqual(GeneralReg.RSP, STACK_PTR_REG);
    try std.testing.expectEqual(@as(usize, 6), GENERAL_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), FLOAT_PARAM_REGS.len);
}

test "callee-saved detection" {
    try std.testing.expect(isCalleeSaved(.RBX));
    try std.testing.expect(isCalleeSaved(.R12));
    try std.testing.expect(!isCalleeSaved(.RAX));
    try std.testing.expect(!isCalleeSaved(.RDI));
}
