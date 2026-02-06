//! Windows x64 (Microsoft x64) calling convention.
//!
//! Used on Windows for 64-bit code.
//! See: https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention

const std = @import("std");
const Registers = @import("Registers.zig");
const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;

/// Windows x64 calling convention
pub const WindowsFastcall = @This();

/// Base pointer register (frame pointer)
pub const BASE_PTR_REG: GeneralReg = .RBP;

/// Stack pointer register
pub const STACK_PTR_REG: GeneralReg = .RSP;

/// Registers used for passing integer/pointer arguments (in order)
/// Note: Windows uses fewer parameter registers than System V
pub const GENERAL_PARAM_REGS = [_]GeneralReg{
    .RCX, // 1st argument
    .RDX, // 2nd argument
    .R8, // 3rd argument
    .R9, // 4th argument
};

/// Registers used for returning integer/pointer values
pub const GENERAL_RETURN_REGS = [_]GeneralReg{
    .RAX, // Return value
};

/// Registers used for passing floating-point arguments
/// Note: These are the SAME positions as integer regs (not independent)
pub const FLOAT_PARAM_REGS = [_]FloatReg{
    .XMM0, // 1st float arg (or 1st any arg if it's float)
    .XMM1, // 2nd float arg
    .XMM2, // 3rd float arg
    .XMM3, // 4th float arg
};

/// Registers used for returning floating-point values
pub const FLOAT_RETURN_REGS = [_]FloatReg{
    .XMM0,
};

/// Caller-saved (volatile) general registers
pub const CALLER_SAVED_GENERAL = [_]GeneralReg{
    .RAX, .RCX, .RDX, .R8, .R9, .R10, .R11,
};

/// Callee-saved (non-volatile) general registers
/// Windows has MORE callee-saved registers than System V
pub const CALLEE_SAVED_GENERAL = [_]GeneralReg{
    .RBX, .RSI, .RDI, .R12, .R13, .R14, .R15,
};

/// Caller-saved floating-point registers
pub const CALLER_SAVED_FLOAT = [_]FloatReg{
    .XMM0, .XMM1, .XMM2, .XMM3, .XMM4, .XMM5,
};

/// Callee-saved floating-point registers
/// Windows DOES have callee-saved XMM registers (unlike System V!)
///
/// KNOWN LIMITATION: The CodeGen does not currently save/restore these registers
/// in function prologues/epilogues. This is safe because:
/// - CALLER_SAVED_FLOAT_MASK (0x3F) only includes XMM0-5
/// - CodeGen.free_float is initialized from this mask, so allocator never gives out XMM6-15
/// - If we ever expand the allocatable pool to include XMM6-15, we would need to add
///   float_callee_saved_used tracking to CodeGen and emit movdqu save/restore pairs
pub const CALLEE_SAVED_FLOAT = [_]FloatReg{
    .XMM6, .XMM7, .XMM8, .XMM9, .XMM10, .XMM11, .XMM12, .XMM13, .XMM14, .XMM15,
};

/// Shadow space (home space) required on stack for register params
/// Caller must allocate 32 bytes even if fewer than 4 args
pub const SHADOW_SPACE_SIZE: u8 = 32;

/// Stack alignment requirement (16 bytes)
pub const STACK_ALIGNMENT: u8 = 16;

/// No red zone in Windows x64
pub const RED_ZONE_SIZE: u8 = 0;

/// Check if a general register is callee-saved
pub fn isCalleeSaved(reg: GeneralReg) bool {
    return switch (reg) {
        .RBX, .RBP, .RSI, .RDI, .R12, .R13, .R14, .R15 => true,
        else => false,
    };
}

/// Check if a float register is callee-saved
pub fn isFloatCalleeSaved(reg: FloatReg) bool {
    return switch (reg) {
        .XMM6, .XMM7, .XMM8, .XMM9, .XMM10, .XMM11, .XMM12, .XMM13, .XMM14, .XMM15 => true,
        else => false,
    };
}

/// Default free registers for allocation (ordered by preference)
pub const DEFAULT_FREE_GENERAL_REGS = [_]GeneralReg{
    // Caller-saved first
    .R11, .R10, .R9,  .R8,
    .RDX, .RCX, .RAX,
    // Callee-saved last
    .R15,
    .R14, .R13, .R12, .RDI,
    .RSI, .RBX,
};

/// Default order for allocating float registers (caller-saved first).
pub const DEFAULT_FREE_FLOAT_REGS = [_]FloatReg{
    // Caller-saved first
    .XMM5,  .XMM4,  .XMM3,  .XMM2,  .XMM1,  .XMM0,
    // Callee-saved last
    .XMM15, .XMM14, .XMM13, .XMM12, .XMM11, .XMM10,
    .XMM9,  .XMM8,  .XMM7,  .XMM6,
};

/// Bitmask of caller-saved general registers (for fast allocation)
/// RAX, RCX, RDX, R8, R9, R10, R11
pub const CALLER_SAVED_GENERAL_MASK: u32 =
    (1 << @intFromEnum(GeneralReg.RAX)) |
    (1 << @intFromEnum(GeneralReg.RCX)) |
    (1 << @intFromEnum(GeneralReg.RDX)) |
    (1 << @intFromEnum(GeneralReg.R8)) |
    (1 << @intFromEnum(GeneralReg.R9)) |
    (1 << @intFromEnum(GeneralReg.R10)) |
    (1 << @intFromEnum(GeneralReg.R11));

/// Bitmask of caller-saved float registers (XMM0-XMM5 only on Windows)
pub const CALLER_SAVED_FLOAT_MASK: u32 = 0x3F; // XMM0-XMM5

test "calling convention constants" {
    try std.testing.expectEqual(GeneralReg.RBP, BASE_PTR_REG);
    try std.testing.expectEqual(GeneralReg.RSP, STACK_PTR_REG);
    try std.testing.expectEqual(@as(usize, 4), GENERAL_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 4), FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(@as(u8, 32), SHADOW_SPACE_SIZE);
}

test "callee-saved detection" {
    try std.testing.expect(isCalleeSaved(.RBX));
    try std.testing.expect(isCalleeSaved(.RSI)); // Different from System V!
    try std.testing.expect(!isCalleeSaved(.RAX));
    try std.testing.expect(!isCalleeSaved(.R10));

    try std.testing.expect(isFloatCalleeSaved(.XMM6));
    try std.testing.expect(!isFloatCalleeSaved(.XMM0));
}
