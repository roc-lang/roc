//! x86_64 register definitions.
//!
//! Defines the general-purpose and floating-point registers available
//! on x86_64 processors.

const std = @import("std");

/// x86_64 general-purpose registers.
///
/// Register encoding matches the x86_64 instruction encoding:
/// - RAX-RDI use 3-bit encoding (0-7)
/// - R8-R15 require REX.B prefix and use 3-bit encoding (0-7)
pub const GeneralReg = enum(u4) {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,

    /// Get the 3-bit register encoding (for ModR/M and SIB bytes)
    pub fn enc(self: GeneralReg) u3 {
        return @truncate(@intFromEnum(self));
    }

    /// Returns true if this register requires a REX prefix (R8-R15)
    pub fn requiresRex(self: GeneralReg) bool {
        return @intFromEnum(self) >= 8;
    }

    /// Get the REX.B bit value for this register
    pub fn rexB(self: GeneralReg) u1 {
        return if (self.requiresRex()) 1 else 0;
    }

    /// Get the REX.R bit value for this register (when used in ModR/M.reg)
    pub fn rexR(self: GeneralReg) u1 {
        return self.rexB();
    }

    /// Get the REX.X bit value for this register (when used in SIB.index)
    pub fn rexX(self: GeneralReg) u1 {
        return self.rexB();
    }

    /// Get the 64-bit register name (e.g., "rax", "r8")
    pub fn name64(self: GeneralReg) []const u8 {
        return switch (self) {
            .RAX => "rax",
            .RCX => "rcx",
            .RDX => "rdx",
            .RBX => "rbx",
            .RSP => "rsp",
            .RBP => "rbp",
            .RSI => "rsi",
            .RDI => "rdi",
            .R8 => "r8",
            .R9 => "r9",
            .R10 => "r10",
            .R11 => "r11",
            .R12 => "r12",
            .R13 => "r13",
            .R14 => "r14",
            .R15 => "r15",
        };
    }

    /// Get the 32-bit register name (e.g., "eax", "r8d")
    pub fn name32(self: GeneralReg) []const u8 {
        return switch (self) {
            .RAX => "eax",
            .RCX => "ecx",
            .RDX => "edx",
            .RBX => "ebx",
            .RSP => "esp",
            .RBP => "ebp",
            .RSI => "esi",
            .RDI => "edi",
            .R8 => "r8d",
            .R9 => "r9d",
            .R10 => "r10d",
            .R11 => "r11d",
            .R12 => "r12d",
            .R13 => "r13d",
            .R14 => "r14d",
            .R15 => "r15d",
        };
    }

    /// Get the 16-bit register name (e.g., "ax", "r8w")
    pub fn name16(self: GeneralReg) []const u8 {
        return switch (self) {
            .RAX => "ax",
            .RCX => "cx",
            .RDX => "dx",
            .RBX => "bx",
            .RSP => "sp",
            .RBP => "bp",
            .RSI => "si",
            .RDI => "di",
            .R8 => "r8w",
            .R9 => "r9w",
            .R10 => "r10w",
            .R11 => "r11w",
            .R12 => "r12w",
            .R13 => "r13w",
            .R14 => "r14w",
            .R15 => "r15w",
        };
    }

    /// Get the 8-bit register name (e.g., "al", "r8b")
    pub fn name8(self: GeneralReg) []const u8 {
        return switch (self) {
            .RAX => "al",
            .RCX => "cl",
            .RDX => "dl",
            .RBX => "bl",
            .RSP => "spl",
            .RBP => "bpl",
            .RSI => "sil",
            .RDI => "dil",
            .R8 => "r8b",
            .R9 => "r9b",
            .R10 => "r10b",
            .R11 => "r11b",
            .R12 => "r12b",
            .R13 => "r13b",
            .R14 => "r14b",
            .R15 => "r15b",
        };
    }
};

/// x86_64 SSE/AVX floating-point registers.
///
/// XMM0-XMM15 are 128-bit registers used for floating-point and SIMD operations.
pub const FloatReg = enum(u4) {
    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7,
    XMM8 = 8,
    XMM9 = 9,
    XMM10 = 10,
    XMM11 = 11,
    XMM12 = 12,
    XMM13 = 13,
    XMM14 = 14,
    XMM15 = 15,

    /// Get the 3-bit register encoding
    pub fn enc(self: FloatReg) u3 {
        return @truncate(@intFromEnum(self));
    }

    /// Returns true if this register requires a REX prefix (XMM8-XMM15)
    pub fn requiresRex(self: FloatReg) bool {
        return @intFromEnum(self) >= 8;
    }

    /// Get the REX.B bit value for this register
    pub fn rexB(self: FloatReg) u1 {
        return if (self.requiresRex()) 1 else 0;
    }

    /// Get the register name (e.g., "xmm0", "xmm8")
    pub fn name(self: FloatReg) []const u8 {
        return switch (self) {
            .XMM0 => "xmm0",
            .XMM1 => "xmm1",
            .XMM2 => "xmm2",
            .XMM3 => "xmm3",
            .XMM4 => "xmm4",
            .XMM5 => "xmm5",
            .XMM6 => "xmm6",
            .XMM7 => "xmm7",
            .XMM8 => "xmm8",
            .XMM9 => "xmm9",
            .XMM10 => "xmm10",
            .XMM11 => "xmm11",
            .XMM12 => "xmm12",
            .XMM13 => "xmm13",
            .XMM14 => "xmm14",
            .XMM15 => "xmm15",
        };
    }
};

/// Register width for operations
pub const RegisterWidth = enum(u2) {
    w8 = 0b00,
    w16 = 0b01,
    w32 = 0b10,
    w64 = 0b11,

    /// Get the operand size in bytes
    pub fn bytes(self: RegisterWidth) u8 {
        return switch (self) {
            .w8 => 1,
            .w16 => 2,
            .w32 => 4,
            .w64 => 8,
        };
    }

    /// Returns true if this width requires a REX.W prefix
    pub fn requiresRexW(self: RegisterWidth) bool {
        return self == .w64;
    }

    /// Returns true if this width requires an operand size override prefix (0x66)
    pub fn requiresSizeOverride(self: RegisterWidth) bool {
        return self == .w16;
    }
};

// Tests
test "general register encoding" {
    try std.testing.expectEqual(@as(u3, 0), GeneralReg.RAX.enc());
    try std.testing.expectEqual(@as(u3, 1), GeneralReg.RCX.enc());
    try std.testing.expectEqual(@as(u3, 0), GeneralReg.R8.enc());
    try std.testing.expectEqual(@as(u3, 7), GeneralReg.R15.enc());
}

test "general register REX requirements" {
    try std.testing.expect(!GeneralReg.RAX.requiresRex());
    try std.testing.expect(!GeneralReg.RDI.requiresRex());
    try std.testing.expect(GeneralReg.R8.requiresRex());
    try std.testing.expect(GeneralReg.R15.requiresRex());
}

test "float register encoding" {
    try std.testing.expectEqual(@as(u3, 0), FloatReg.XMM0.enc());
    try std.testing.expectEqual(@as(u3, 0), FloatReg.XMM8.enc());
    try std.testing.expect(!FloatReg.XMM7.requiresRex());
    try std.testing.expect(FloatReg.XMM8.requiresRex());
}

test "register width" {
    try std.testing.expectEqual(@as(u8, 1), RegisterWidth.w8.bytes());
    try std.testing.expectEqual(@as(u8, 8), RegisterWidth.w64.bytes());
    try std.testing.expect(RegisterWidth.w64.requiresRexW());
    try std.testing.expect(!RegisterWidth.w32.requiresRexW());
}
