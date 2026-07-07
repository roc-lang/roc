//! Shared checked-integer arithmetic metadata for LIR producers and consumers.

const std = @import("std");
const layout = @import("layout");

const LIR = @import("LIR.zig");

/// Runtime failure category for a checked arithmetic operation.
pub const ErrorKind = enum {
    overflow,
    zero_denominator,
};

/// Returns whether a layout is one of Roc's fixed-width integer layouts.
pub fn isIntegerLayout(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .u8,
        .i8,
        .u16,
        .i16,
        .u32,
        .i32,
        .u64,
        .i64,
        .u128,
        .i128,
        => true,
        else => false,
    };
}

/// Returns whether a layout is one of Roc's signed fixed-width integer layouts.
pub fn isSignedIntegerLayout(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .i8,
        .i16,
        .i32,
        .i64,
        .i128,
        => true,
        else => false,
    };
}

/// Returns the integer bit width represented by a fixed-width integer layout.
pub fn intBits(layout_idx: layout.Idx) u16 {
    return switch (layout_idx) {
        .u8, .i8 => 8,
        .u16, .i16 => 16,
        .u32, .i32 => 32,
        .u64, .i64 => 64,
        .u128, .i128 => 128,
        else => unreachable,
    };
}

/// Returns the lowest representable value for a signed integer layout.
pub fn signedLowestValue(layout_idx: layout.Idx) ?i128 {
    return switch (layout_idx) {
        .i8 => std.math.minInt(i8),
        .i16 => std.math.minInt(i16),
        .i32 => std.math.minInt(i32),
        .i64 => std.math.minInt(i64),
        .i128 => std.math.minInt(i128),
        else => null,
    };
}

/// Returns the checked LIR operation for a plain integer arithmetic operation.
pub fn checkedOp(op: LIR.LowLevel, layout_idx: layout.Idx) ?LIR.LowLevel {
    if (!isIntegerLayout(layout_idx)) return null;
    return switch (op) {
        .num_plus => .num_plus_checked,
        .num_minus => .num_minus_checked,
        .num_times => .num_times_checked,
        .num_div_by => .num_div_by_checked,
        .num_div_trunc_by => .num_div_trunc_by_checked,
        .num_rem_by => .num_rem_by_checked,
        .num_mod_by => .num_mod_by_checked,
        .num_negate => if (isSignedIntegerLayout(layout_idx)) .num_negate_checked else null,
        .num_abs => if (isSignedIntegerLayout(layout_idx)) .num_abs_checked else null,
        else => null,
    };
}

/// Returns the plain wrapping LIR operation for a checked arithmetic operation.
pub fn uncheckedOp(op: LIR.LowLevel) ?LIR.LowLevel {
    return switch (op) {
        .num_plus_checked => .num_plus,
        .num_minus_checked => .num_minus,
        .num_times_checked => .num_times,
        .num_div_by_checked => .num_div_by,
        .num_div_trunc_by_checked => .num_div_trunc_by,
        .num_rem_by_checked => .num_rem_by,
        .num_mod_by_checked => .num_mod_by,
        .num_negate_checked => .num_negate,
        .num_abs_checked => .num_abs,
        else => null,
    };
}

/// Returns the canonical crash message for a checked arithmetic overflow.
pub fn overflowMessage(op: LIR.LowLevel) ?[]const u8 {
    return switch (op) {
        .num_plus_checked => "Integer addition overflowed",
        .num_minus_checked => "Integer subtraction overflowed",
        .num_times_checked => "Integer multiplication overflowed",
        .num_negate_checked => "Integer negation overflowed",
        .num_abs_checked => "Integer absolute value overflowed",
        .num_div_by_checked,
        .num_div_trunc_by_checked,
        => "Integer division overflowed",
        else => null,
    };
}

/// Returns the canonical crash message for a checked zero-denominator operation.
pub fn zeroDenominatorMessage(op: LIR.LowLevel, layout_idx: layout.Idx) ?[]const u8 {
    return switch (op) {
        .num_div_by_checked,
        .num_div_trunc_by_checked,
        => divisionByZeroMessage(layout_idx),
        .num_rem_by_checked => remainderByZeroMessage(layout_idx),
        .num_mod_by_checked => moduloByZeroMessage(layout_idx),
        else => null,
    };
}

fn divisionByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
    return switch (layout_idx) {
        .u8 => "U8 division by zero",
        .i8 => "I8 division by zero",
        .u16 => "U16 division by zero",
        .i16 => "I16 division by zero",
        .u32 => "U32 division by zero",
        .i32 => "I32 division by zero",
        .u64 => "U64 division by zero",
        .i64 => "I64 division by zero",
        .u128 => "U128 division by zero",
        .i128 => "I128 division by zero",
        else => null,
    };
}

fn remainderByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
    return switch (layout_idx) {
        .u8 => "U8 remainder by zero",
        .i8 => "I8 remainder by zero",
        .u16 => "U16 remainder by zero",
        .i16 => "I16 remainder by zero",
        .u32 => "U32 remainder by zero",
        .i32 => "I32 remainder by zero",
        .u64 => "U64 remainder by zero",
        .i64 => "I64 remainder by zero",
        .u128 => "U128 remainder by zero",
        .i128 => "I128 remainder by zero",
        else => null,
    };
}

fn moduloByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
    return switch (layout_idx) {
        .u8 => "U8 modulo by zero",
        .i8 => "I8 modulo by zero",
        .u16 => "U16 modulo by zero",
        .i16 => "I16 modulo by zero",
        .u32 => "U32 modulo by zero",
        .i32 => "I32 modulo by zero",
        .u64 => "U64 modulo by zero",
        .i64 => "I64 modulo by zero",
        .u128 => "U128 modulo by zero",
        .i128 => "I128 modulo by zero",
        else => null,
    };
}

test "checkedOp maps every integer arithmetic operation and skips non-integers" {
    try std.testing.expectEqual(LIR.LowLevel.num_plus_checked, checkedOp(.num_plus, .u8).?);
    try std.testing.expectEqual(LIR.LowLevel.num_minus_checked, checkedOp(.num_minus, .i16).?);
    try std.testing.expectEqual(LIR.LowLevel.num_times_checked, checkedOp(.num_times, .u32).?);
    try std.testing.expectEqual(LIR.LowLevel.num_div_by_checked, checkedOp(.num_div_by, .i64).?);
    try std.testing.expectEqual(LIR.LowLevel.num_div_trunc_by_checked, checkedOp(.num_div_trunc_by, .u128).?);
    try std.testing.expectEqual(LIR.LowLevel.num_rem_by_checked, checkedOp(.num_rem_by, .i8).?);
    try std.testing.expectEqual(LIR.LowLevel.num_mod_by_checked, checkedOp(.num_mod_by, .i128).?);
    try std.testing.expectEqual(LIR.LowLevel.num_negate_checked, checkedOp(.num_negate, .i32).?);
    try std.testing.expectEqual(LIR.LowLevel.num_abs_checked, checkedOp(.num_abs, .i64).?);

    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_plus, .f64));
    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_abs, .u64));
    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_negate, .u128));
}

test "checkedOp round trips through uncheckedOp" {
    const checked_ops = [_]LIR.LowLevel{
        .num_plus_checked,
        .num_minus_checked,
        .num_times_checked,
        .num_div_by_checked,
        .num_div_trunc_by_checked,
        .num_rem_by_checked,
        .num_mod_by_checked,
        .num_negate_checked,
        .num_abs_checked,
    };

    for (checked_ops) |checked| {
        const plain = uncheckedOp(checked).?;
        try std.testing.expectEqual(checked, checkedOp(plain, .i64).?);
    }
}

test "checked arithmetic messages are canonical and operation specific" {
    try std.testing.expectEqualStrings("Integer addition overflowed", overflowMessage(.num_plus_checked).?);
    try std.testing.expectEqualStrings("Integer subtraction overflowed", overflowMessage(.num_minus_checked).?);
    try std.testing.expectEqualStrings("Integer multiplication overflowed", overflowMessage(.num_times_checked).?);
    try std.testing.expectEqualStrings("Integer negation overflowed", overflowMessage(.num_negate_checked).?);
    try std.testing.expectEqualStrings("Integer absolute value overflowed", overflowMessage(.num_abs_checked).?);
    try std.testing.expectEqualStrings("Integer division overflowed", overflowMessage(.num_div_by_checked).?);
    try std.testing.expectEqualStrings("Integer division overflowed", overflowMessage(.num_div_trunc_by_checked).?);

    const cases = [_]struct {
        layout_idx: layout.Idx,
        div: []const u8,
        rem: []const u8,
        mod: []const u8,
    }{
        .{ .layout_idx = .u8, .div = "U8 division by zero", .rem = "U8 remainder by zero", .mod = "U8 modulo by zero" },
        .{ .layout_idx = .i8, .div = "I8 division by zero", .rem = "I8 remainder by zero", .mod = "I8 modulo by zero" },
        .{ .layout_idx = .u16, .div = "U16 division by zero", .rem = "U16 remainder by zero", .mod = "U16 modulo by zero" },
        .{ .layout_idx = .i16, .div = "I16 division by zero", .rem = "I16 remainder by zero", .mod = "I16 modulo by zero" },
        .{ .layout_idx = .u32, .div = "U32 division by zero", .rem = "U32 remainder by zero", .mod = "U32 modulo by zero" },
        .{ .layout_idx = .i32, .div = "I32 division by zero", .rem = "I32 remainder by zero", .mod = "I32 modulo by zero" },
        .{ .layout_idx = .u64, .div = "U64 division by zero", .rem = "U64 remainder by zero", .mod = "U64 modulo by zero" },
        .{ .layout_idx = .i64, .div = "I64 division by zero", .rem = "I64 remainder by zero", .mod = "I64 modulo by zero" },
        .{ .layout_idx = .u128, .div = "U128 division by zero", .rem = "U128 remainder by zero", .mod = "U128 modulo by zero" },
        .{ .layout_idx = .i128, .div = "I128 division by zero", .rem = "I128 remainder by zero", .mod = "I128 modulo by zero" },
    };

    for (cases) |case| {
        try std.testing.expectEqualStrings(case.div, zeroDenominatorMessage(.num_div_by_checked, case.layout_idx).?);
        try std.testing.expectEqualStrings(case.div, zeroDenominatorMessage(.num_div_trunc_by_checked, case.layout_idx).?);
        try std.testing.expectEqualStrings(case.rem, zeroDenominatorMessage(.num_rem_by_checked, case.layout_idx).?);
        try std.testing.expectEqualStrings(case.mod, zeroDenominatorMessage(.num_mod_by_checked, case.layout_idx).?);
    }
}
