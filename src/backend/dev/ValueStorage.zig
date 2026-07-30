//! Value storage kinds for the dev backend.
//!
//! Defines the `ValueLoc` union describing where a computed value lives
//! (register, stack, immediate, or memory address) and the `NumKind` enum of
//! numeric types. The architecture-specific code generators in
//! `x86_64/CodeGen.zig` and `aarch64/CodeGen.zig` consume `ValueLoc` when
//! tracking local variable locations.

const std = @import("std");

/// Value location - where a computed value is stored
pub const ValueLoc = union(enum) {
    /// Value is in a general-purpose register
    general_reg: u8,
    /// Value is in a floating-point register
    float_reg: u8,
    /// Value is on the stack at offset from frame pointer
    stack: i32,
    /// Value is an immediate constant
    immediate: i64,
    /// Value is a floating-point immediate
    float_immediate: f64,
    /// Value is at a memory address (for large values)
    memory: struct {
        base_reg: u8,
        offset: i32,
    },
};

/// Numeric type kinds (matches CIR.NumKind)
pub const NumKind = enum {
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,

    pub fn isFloat(self: NumKind) bool {
        return self == .f32 or self == .f64;
    }

    pub fn isSigned(self: NumKind) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128 => true,
            .u8, .u16, .u32, .u64, .u128 => false,
            .f32, .f64, .dec => true, // floats are signed
        };
    }

    pub fn byteSize(self: NumKind) u8 {
        return switch (self) {
            .u8, .i8 => 1,
            .u16, .i16 => 2,
            .u32, .i32, .f32 => 4,
            .u64, .i64, .f64 => 8,
            .u128, .i128, .dec => 16,
        };
    }
};

// Tests

test "NumKind properties" {
    try std.testing.expect(NumKind.f32.isFloat());
    try std.testing.expect(NumKind.f64.isFloat());
    try std.testing.expect(!NumKind.i32.isFloat());

    try std.testing.expect(NumKind.i32.isSigned());
    try std.testing.expect(!NumKind.u32.isSigned());

    try std.testing.expectEqual(@as(u8, 4), NumKind.i32.byteSize());
    try std.testing.expectEqual(@as(u8, 8), NumKind.i64.byteSize());
    try std.testing.expectEqual(@as(u8, 16), NumKind.i128.byteSize());
}
