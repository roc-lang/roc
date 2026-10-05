//! aarch64 (ARM64) register definitions.
//!
//! Defines the general-purpose and floating-point registers available
//! on aarch64 processors.

const std = @import("std");

/// aarch64 general-purpose registers.
///
/// The aarch64 architecture has 31 general-purpose registers (X0-X30) plus
/// a zero register/stack pointer (XZR/SP) that shares encoding 31.
pub const GeneralReg = enum(u5) {
    // Argument/result registers (caller-saved)
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,

    // Indirect result location register (caller-saved)
    XR = 8,

    // Temporary registers (caller-saved)
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,

    // Intra-procedure-call scratch registers
    IP0 = 16,
    IP1 = 17,

    // Platform register (reserved)
    PR = 18,

    // Callee-saved registers
    X19 = 19,
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,

    // Frame pointer (callee-saved)
    FP = 29,

    // Link register (return address)
    LR = 30,

    /// Zero register when reading, Stack Pointer when writing (context-dependent)
    ZRSP = 31,

    /// Get the 5-bit register encoding
    pub fn enc(self: GeneralReg) u5 {
        return @intFromEnum(self);
    }

    /// Get the 64-bit register name
    pub fn name64(self: GeneralReg) []const u8 {
        return switch (self) {
            .X0 => "x0",
            .X1 => "x1",
            .X2 => "x2",
            .X3 => "x3",
            .X4 => "x4",
            .X5 => "x5",
            .X6 => "x6",
            .X7 => "x7",
            .XR => "x8",
            .X9 => "x9",
            .X10 => "x10",
            .X11 => "x11",
            .X12 => "x12",
            .X13 => "x13",
            .X14 => "x14",
            .X15 => "x15",
            .IP0 => "x16",
            .IP1 => "x17",
            .PR => "x18",
            .X19 => "x19",
            .X20 => "x20",
            .X21 => "x21",
            .X22 => "x22",
            .X23 => "x23",
            .X24 => "x24",
            .X25 => "x25",
            .X26 => "x26",
            .X27 => "x27",
            .X28 => "x28",
            .FP => "x29",
            .LR => "x30",
            .ZRSP => "sp",
        };
    }
};

/// aarch64 SIMD/floating-point registers.
///
/// V0-V31 are 128-bit vector registers. They can be accessed as:
/// - Bn (8-bit), Hn (16-bit), Sn (32-bit float), Dn (64-bit float), Qn (128-bit)
pub const FloatReg = enum(u5) {
    V0 = 0,
    V1 = 1,
    V2 = 2,
    V3 = 3,
    V4 = 4,
    V5 = 5,
    V6 = 6,
    V7 = 7,
    V8 = 8,
    V9 = 9,
    V10 = 10,
    V11 = 11,
    V12 = 12,
    V13 = 13,
    V14 = 14,
    V15 = 15,
    V16 = 16,
    V17 = 17,
    V18 = 18,
    V19 = 19,
    V20 = 20,
    V21 = 21,
    V22 = 22,
    V23 = 23,
    V24 = 24,
    V25 = 25,
    V26 = 26,
    V27 = 27,
    V28 = 28,
    V29 = 29,
    V30 = 30,
    V31 = 31,

    /// Get the 5-bit register encoding
    pub fn enc(self: FloatReg) u5 {
        return @intFromEnum(self);
    }
};

/// Register width for operations
pub const RegisterWidth = enum(u1) {
    /// 32-bit operation (Wn registers)
    w32 = 0,
    /// 64-bit operation (Xn registers)
    w64 = 1,

    /// Get the operand size in bytes
    pub fn bytes(self: RegisterWidth) u8 {
        return switch (self) {
            .w32 => 4,
            .w64 => 8,
        };
    }

    /// Get the sf (size flag) bit for instruction encoding
    pub fn sf(self: RegisterWidth) u1 {
        return @intFromEnum(self);
    }
};

// Tests
test "general register encoding" {
    try std.testing.expectEqual(@as(u5, 0), GeneralReg.X0.enc());
    try std.testing.expectEqual(@as(u5, 29), GeneralReg.FP.enc());
    try std.testing.expectEqual(@as(u5, 30), GeneralReg.LR.enc());
    try std.testing.expectEqual(@as(u5, 31), GeneralReg.ZRSP.enc());
}

test "float register encoding" {
    try std.testing.expectEqual(@as(u5, 0), FloatReg.V0.enc());
    try std.testing.expectEqual(@as(u5, 31), FloatReg.V31.enc());
}

test "register width" {
    try std.testing.expectEqual(@as(u8, 4), RegisterWidth.w32.bytes());
    try std.testing.expectEqual(@as(u8, 8), RegisterWidth.w64.bytes());
    try std.testing.expectEqual(@as(u1, 0), RegisterWidth.w32.sf());
    try std.testing.expectEqual(@as(u1, 1), RegisterWidth.w64.sf());
}
