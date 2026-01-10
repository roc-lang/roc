//! AAPCS64 (ARM Architecture Procedure Call Standard for 64-bit) calling convention.
//!
//! Used on Linux, macOS (Apple Silicon), and other aarch64 platforms.
//! See: https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst

const std = @import("std");
const Registers = @import("Registers.zig");
const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;

/// AAPCS64 calling convention
pub const Call = @This();

/// Frame pointer register
pub const BASE_PTR_REG: GeneralReg = .FP; // X29

/// Stack pointer register
pub const STACK_PTR_REG: GeneralReg = .ZRSP; // SP (encoded as 31)

/// Link register (return address)
pub const LINK_REG: GeneralReg = .LR; // X30

/// Registers used for passing integer/pointer arguments (in order)
pub const GENERAL_PARAM_REGS = [_]GeneralReg{
    .X0, // 1st argument
    .X1, // 2nd argument
    .X2, // 3rd argument
    .X3, // 4th argument
    .X4, // 5th argument
    .X5, // 6th argument
    .X6, // 7th argument
    .X7, // 8th argument
};

/// Registers used for returning integer/pointer values
pub const GENERAL_RETURN_REGS = [_]GeneralReg{
    .X0, // Primary return value
    .X1, // Secondary (for pairs)
    .X2, // (for larger returns)
    .X3,
    .X4,
    .X5,
    .X6,
    .X7,
};

/// Registers used for passing floating-point/SIMD arguments
pub const FLOAT_PARAM_REGS = [_]FloatReg{
    .V0, .V1, .V2, .V3, .V4, .V5, .V6, .V7,
};

/// Registers used for returning floating-point/SIMD values
pub const FLOAT_RETURN_REGS = [_]FloatReg{
    .V0, .V1, .V2, .V3, .V4, .V5, .V6, .V7,
};

/// Indirect result location register
/// When a function returns a large aggregate, X8 points to the memory location
pub const INDIRECT_RESULT_REG: GeneralReg = .XR; // X8

/// Caller-saved (volatile) general registers
pub const CALLER_SAVED_GENERAL = [_]GeneralReg{
    .X0, .X1, .X2,  .X3,  .X4,  .X5,  .X6,  .X7,
    .XR, .X9, .X10, .X11, .X12, .X13, .X14, .X15,
    .IP0, .IP1, // Intra-procedure-call scratch registers
};

/// Callee-saved (non-volatile) general registers
pub const CALLEE_SAVED_GENERAL = [_]GeneralReg{
    .X19, .X20, .X21, .X22, .X23, .X24, .X25, .X26, .X27, .X28,
    // FP (X29) and LR (X30) are also callee-saved but handled specially
};

/// Caller-saved floating-point registers
pub const CALLER_SAVED_FLOAT = [_]FloatReg{
    .V0,  .V1,  .V2,  .V3,  .V4,  .V5,  .V6,  .V7,
    .V16, .V17, .V18, .V19, .V20, .V21, .V22, .V23,
    .V24, .V25, .V26, .V27, .V28, .V29, .V30, .V31,
};

/// Callee-saved floating-point registers (lower 64 bits only)
pub const CALLEE_SAVED_FLOAT = [_]FloatReg{
    .V8, .V9, .V10, .V11, .V12, .V13, .V14, .V15,
};

/// No shadow space required
pub const SHADOW_SPACE_SIZE: u8 = 0;

/// Stack alignment requirement (16 bytes)
pub const STACK_ALIGNMENT: u8 = 16;

/// Platform register (reserved, do not use)
pub const PLATFORM_REG: GeneralReg = .PR; // X18

/// Check if a general register is callee-saved
pub fn isCalleeSaved(reg: GeneralReg) bool {
    return switch (reg) {
        .X19, .X20, .X21, .X22, .X23, .X24, .X25, .X26, .X27, .X28 => true,
        .FP, .LR => true, // Frame pointer and link register are also preserved
        else => false,
    };
}

/// Check if a float register is callee-saved
/// Note: Only the lower 64 bits of V8-V15 are callee-saved
pub fn isFloatCalleeSaved(reg: FloatReg) bool {
    return switch (reg) {
        .V8, .V9, .V10, .V11, .V12, .V13, .V14, .V15 => true,
        else => false,
    };
}

/// Default free registers for allocation (ordered by preference)
/// Put callee-saved regs last to minimize saves
pub const DEFAULT_FREE_GENERAL_REGS = [_]GeneralReg{
    // Caller-saved first (no save/restore needed)
    .X15, .X14, .X13, .X12, .X11, .X10, .X9,
    .X7,  .X6,  .X5,  .X4,  .X3,  .X2,  .X1,
    .X0,
    // Callee-saved last (need save/restore)
     .X28, .X27, .X26, .X25, .X24, .X23,
    .X22, .X21, .X20, .X19,
};

/// Default order for allocating float registers (caller-saved first).
pub const DEFAULT_FREE_FLOAT_REGS = [_]FloatReg{
    // Caller-saved first
    .V31, .V30, .V29, .V28, .V27, .V26, .V25, .V24,
    .V23, .V22, .V21, .V20, .V19, .V18, .V17, .V16,
    .V7,  .V6,  .V5,  .V4,  .V3,  .V2,  .V1,  .V0,
    // Callee-saved last
    .V15, .V14, .V13, .V12, .V11, .V10, .V9,  .V8,
};

/// Bitmask of caller-saved general registers (for fast allocation)
/// X0-X15, IP0 (X16), IP1 (X17) - excludes X18 (platform register)
pub const CALLER_SAVED_GENERAL_MASK: u32 =
    (1 << @intFromEnum(GeneralReg.X0)) |
    (1 << @intFromEnum(GeneralReg.X1)) |
    (1 << @intFromEnum(GeneralReg.X2)) |
    (1 << @intFromEnum(GeneralReg.X3)) |
    (1 << @intFromEnum(GeneralReg.X4)) |
    (1 << @intFromEnum(GeneralReg.X5)) |
    (1 << @intFromEnum(GeneralReg.X6)) |
    (1 << @intFromEnum(GeneralReg.X7)) |
    (1 << @intFromEnum(GeneralReg.XR)) |
    (1 << @intFromEnum(GeneralReg.X9)) |
    (1 << @intFromEnum(GeneralReg.X10)) |
    (1 << @intFromEnum(GeneralReg.X11)) |
    (1 << @intFromEnum(GeneralReg.X12)) |
    (1 << @intFromEnum(GeneralReg.X13)) |
    (1 << @intFromEnum(GeneralReg.X14)) |
    (1 << @intFromEnum(GeneralReg.X15)) |
    (1 << @intFromEnum(GeneralReg.IP0)) |
    (1 << @intFromEnum(GeneralReg.IP1));

/// Bitmask of caller-saved float registers
/// V0-V7 and V16-V31 are caller-saved
pub const CALLER_SAVED_FLOAT_MASK: u32 =
    0x00FF | // V0-V7
    0xFFFF0000; // V16-V31

test "calling convention constants" {
    try std.testing.expectEqual(GeneralReg.FP, BASE_PTR_REG);
    try std.testing.expectEqual(GeneralReg.ZRSP, STACK_PTR_REG);
    try std.testing.expectEqual(@as(usize, 8), GENERAL_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), FLOAT_PARAM_REGS.len);
}

test "callee-saved detection" {
    try std.testing.expect(isCalleeSaved(.X19));
    try std.testing.expect(isCalleeSaved(.FP));
    try std.testing.expect(!isCalleeSaved(.X0));
    try std.testing.expect(!isCalleeSaved(.X9));

    try std.testing.expect(isFloatCalleeSaved(.V8));
    try std.testing.expect(!isFloatCalleeSaved(.V0));
    try std.testing.expect(!isFloatCalleeSaved(.V16));
}
