//! Shared checked-integer arithmetic metadata for LIR producers and consumers.

const std = @import("std");
const layout = @import("layout");

const LIR = @import("LIR.zig");

/// Runtime failure category for a checked arithmetic operation.
pub const ErrorKind = enum {
    overflow,
    zero_denominator,
};

/// The mathematical operation represented by one integer-arithmetic family.
pub const Operation = enum {
    add,
    sub,
    mul,
};

/// The behavior carried explicitly by an integer-arithmetic LIR operation.
pub const Mode = enum {
    wrap,
    crash_on_overflow,
    overflows,
    proven_cannot_overflow,
};

/// Classification of one integer-arithmetic family member.
pub const FamilyEntry = struct {
    op: LIR.LowLevel,
    operation: Operation,
    mode: Mode,
};

const family = [_]FamilyEntry{
    .{ .op = .num_int_add_wrap, .operation = .add, .mode = .wrap },
    .{ .op = .num_int_add_crash_on_overflow, .operation = .add, .mode = .crash_on_overflow },
    .{ .op = .num_int_add_overflows, .operation = .add, .mode = .overflows },
    .{ .op = .num_int_add_proven_cannot_overflow, .operation = .add, .mode = .proven_cannot_overflow },
    .{ .op = .num_int_sub_wrap, .operation = .sub, .mode = .wrap },
    .{ .op = .num_int_sub_crash_on_overflow, .operation = .sub, .mode = .crash_on_overflow },
    .{ .op = .num_int_sub_overflows, .operation = .sub, .mode = .overflows },
    .{ .op = .num_int_sub_proven_cannot_overflow, .operation = .sub, .mode = .proven_cannot_overflow },
    .{ .op = .num_int_mul_wrap, .operation = .mul, .mode = .wrap },
    .{ .op = .num_int_mul_crash_on_overflow, .operation = .mul, .mode = .crash_on_overflow },
    .{ .op = .num_int_mul_overflows, .operation = .mul, .mode = .overflows },
    .{ .op = .num_int_mul_proven_cannot_overflow, .operation = .mul, .mode = .proven_cannot_overflow },
};

const low_level_table_len = blk: {
    var highest: usize = 0;
    for (@typeInfo(LIR.LowLevel).@"enum".fields) |field| {
        highest = @max(highest, field.value);
    }
    break :blk highest + 1;
};

const classify_table: [low_level_table_len]?FamilyEntry = blk: {
    var table = [_]?FamilyEntry{null} ** low_level_table_len;
    for (family) |entry| {
        const index = @intFromEnum(entry.op);
        if (table[index] != null) @compileError("duplicate checked-arithmetic family operation");
        table[index] = entry;
    }
    break :blk table;
};

/// Returns the operation and behavior of an integer-arithmetic family member.
pub fn classify(op: LIR.LowLevel) ?FamilyEntry {
    return classify_table[@intFromEnum(op)];
}

/// Returns whether an operation belongs to an integer-arithmetic family.
pub fn isFamily(op: LIR.LowLevel) bool {
    return classify(op) != null;
}

/// Returns one operation's family member for the requested behavior.
pub fn member(operation: Operation, mode: Mode) LIR.LowLevel {
    inline for (family) |entry| {
        if (entry.operation == operation and entry.mode == mode) return entry.op;
    }
    unreachable;
}

/// Returns whether an operation is a pre-LIR source-policy marker.
pub fn isSourcePolicyOp(op: LIR.LowLevel) bool {
    return op == .num_plus or op == .num_minus or op == .num_times;
}

/// Returns the form that licenses a backend no-wraparound assertion.
pub fn provenForm(op: LIR.LowLevel) ?LIR.LowLevel {
    if (classify(op)) |entry| {
        if (entry.mode == .overflows) return null;
        return member(entry.operation, .proven_cannot_overflow);
    }
    return uncheckedOp(op);
}

/// Returns whether a layout is one of Roc's fixed-width integer layouts.
pub fn isIntegerLayout(layout_idx: layout.Idx) bool {
    return layout_idx == .u8 or layout_idx == .i8 or
        layout_idx == .u16 or layout_idx == .i16 or
        layout_idx == .u32 or layout_idx == .i32 or
        layout_idx == .u64 or layout_idx == .i64 or
        layout_idx == .u128 or layout_idx == .i128;
}

/// Returns whether a layout is one of Roc's signed fixed-width integer layouts.
pub fn isSignedIntegerLayout(layout_idx: layout.Idx) bool {
    return layout_idx == .i8 or layout_idx == .i16 or layout_idx == .i32 or
        layout_idx == .i64 or layout_idx == .i128;
}

/// Returns the integer bit width represented by a fixed-width integer layout.
pub fn intBits(layout_idx: layout.Idx) u16 {
    if (layout_idx == .u8 or layout_idx == .i8) return 8;
    if (layout_idx == .u16 or layout_idx == .i16) return 16;
    if (layout_idx == .u32 or layout_idx == .i32) return 32;
    if (layout_idx == .u64 or layout_idx == .i64) return 64;
    if (layout_idx == .u128 or layout_idx == .i128) return 128;
    unreachable;
}

/// Returns the lowest representable value for a signed numeric layout.
pub fn signedLowestValue(layout_idx: layout.Idx) ?i128 {
    if (layout_idx == .i8) return std.math.minInt(i8);
    if (layout_idx == .i16) return std.math.minInt(i16);
    if (layout_idx == .i32) return std.math.minInt(i32);
    if (layout_idx == .i64) return std.math.minInt(i64);
    if (layout_idx == .i128 or layout_idx == .dec) return std.math.minInt(i128);
    return null;
}

/// Returns the checked LIR operation required by a plain numeric operation.
pub fn checkedOp(op: LIR.LowLevel, layout_idx: layout.Idx) ?LIR.LowLevel {
    if (layout_idx == .dec) {
        if (op == .num_plus) return member(.add, .crash_on_overflow);
        if (op == .num_minus) return member(.sub, .crash_on_overflow);
        return if (op == .num_abs) .num_abs_checked else null;
    }
    if (!isIntegerLayout(layout_idx)) return null;
    if (op == .num_plus) return member(.add, .crash_on_overflow);
    if (op == .num_minus) return member(.sub, .crash_on_overflow);
    if (op == .num_times) return member(.mul, .crash_on_overflow);
    if (op == .num_div_by) return .num_div_by_checked;
    if (op == .num_div_trunc_by) return .num_div_trunc_by_checked;
    if (op == .num_rem_by) return .num_rem_by_checked;
    if (op == .num_mod_by) return .num_mod_by_checked;
    if (op == .num_negate and isSignedIntegerLayout(layout_idx)) return .num_negate_checked;
    if (op == .num_abs and isSignedIntegerLayout(layout_idx)) return .num_abs_checked;
    return null;
}

/// Commits the source arithmetic policy to its LIR operation.
pub fn lowerOp(op: LIR.LowLevel, layout_idx: layout.Idx) LIR.LowLevel {
    if (op == .num_plus) {
        if (layout_idx == .f32 or layout_idx == .f64) return .num_float_add;
        return checkedOp(op, layout_idx) orelse unreachable;
    }
    if (op == .num_minus) {
        if (layout_idx == .f32 or layout_idx == .f64) return .num_float_sub;
        return checkedOp(op, layout_idx) orelse unreachable;
    }
    if (op == .num_times) {
        if (layout_idx == .f32 or layout_idx == .f64) return .num_float_mul;
        if (layout_idx == .dec) return .dec_mul;
        return checkedOp(op, layout_idx) orelse unreachable;
    }
    return checkedOp(op, layout_idx) orelse op;
}

/// Returns the plain wrapping LIR operation for a checked arithmetic operation.
pub fn uncheckedOp(op: LIR.LowLevel) ?LIR.LowLevel {
    if (op == .num_div_by_checked) return .num_div_by;
    if (op == .num_div_trunc_by_checked) return .num_div_trunc_by;
    if (op == .num_rem_by_checked) return .num_rem_by;
    if (op == .num_mod_by_checked) return .num_mod_by;
    if (op == .num_negate_checked) return .num_negate;
    if (op == .num_abs_checked) return .num_abs;
    return null;
}

/// Returns the canonical crash message for a checked arithmetic overflow.
pub fn overflowMessage(op: LIR.LowLevel) ?[]const u8 {
    if (classify(op)) |entry| {
        return switch (entry.operation) {
            .add => "Integer addition overflowed",
            .sub => "Integer subtraction overflowed",
            .mul => "Integer multiplication overflowed",
        };
    }
    if (op == .num_negate_checked) return "Integer negation overflowed";
    if (op == .num_abs_checked) return "Integer absolute value overflowed";
    if (op == .num_div_by_checked or op == .num_div_trunc_by_checked) return "Integer division overflowed";
    return null;
}

/// Returns the overflow message for a checked operation and operand layout.
pub fn overflowMessageForLayout(op: LIR.LowLevel, layout_idx: layout.Idx) ?[]const u8 {
    if (op == .num_abs_checked and layout_idx == .dec) {
        return "Decimal absolute value overflow!";
    }
    return overflowMessage(op);
}

/// Returns the canonical crash message for a checked zero-denominator operation.
pub fn zeroDenominatorMessage(op: LIR.LowLevel, layout_idx: layout.Idx) ?[]const u8 {
    if (op == .num_div_by_checked or op == .num_div_trunc_by_checked) return divisionByZeroMessage(layout_idx);
    if (op == .num_rem_by_checked) return remainderByZeroMessage(layout_idx);
    if (op == .num_mod_by_checked) return moduloByZeroMessage(layout_idx);
    return null;
}

fn divisionByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
    if (layout_idx == .u8) return "U8 division by zero";
    if (layout_idx == .i8) return "I8 division by zero";
    if (layout_idx == .u16) return "U16 division by zero";
    if (layout_idx == .i16) return "I16 division by zero";
    if (layout_idx == .u32) return "U32 division by zero";
    if (layout_idx == .i32) return "I32 division by zero";
    if (layout_idx == .u64) return "U64 division by zero";
    if (layout_idx == .i64) return "I64 division by zero";
    if (layout_idx == .u128) return "U128 division by zero";
    if (layout_idx == .i128) return "I128 division by zero";
    return null;
}

fn remainderByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
    if (layout_idx == .u8) return "U8 remainder by zero";
    if (layout_idx == .i8) return "I8 remainder by zero";
    if (layout_idx == .u16) return "U16 remainder by zero";
    if (layout_idx == .i16) return "I16 remainder by zero";
    if (layout_idx == .u32) return "U32 remainder by zero";
    if (layout_idx == .i32) return "I32 remainder by zero";
    if (layout_idx == .u64) return "U64 remainder by zero";
    if (layout_idx == .i64) return "I64 remainder by zero";
    if (layout_idx == .u128) return "U128 remainder by zero";
    if (layout_idx == .i128) return "I128 remainder by zero";
    return null;
}

fn moduloByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
    if (layout_idx == .u8) return "U8 modulo by zero";
    if (layout_idx == .i8) return "I8 modulo by zero";
    if (layout_idx == .u16) return "U16 modulo by zero";
    if (layout_idx == .i16) return "I16 modulo by zero";
    if (layout_idx == .u32) return "U32 modulo by zero";
    if (layout_idx == .i32) return "I32 modulo by zero";
    if (layout_idx == .u64) return "U64 modulo by zero";
    if (layout_idx == .i64) return "I64 modulo by zero";
    if (layout_idx == .u128) return "U128 modulo by zero";
    if (layout_idx == .i128) return "I128 modulo by zero";
    return null;
}

test "checkedOp maps integer arithmetic and checked Dec absolute value" {
    try std.testing.expectEqual(LIR.LowLevel.num_int_add_crash_on_overflow, checkedOp(.num_plus, .u8).?);
    try std.testing.expectEqual(LIR.LowLevel.num_int_sub_crash_on_overflow, checkedOp(.num_minus, .i16).?);
    try std.testing.expectEqual(LIR.LowLevel.num_int_mul_crash_on_overflow, checkedOp(.num_times, .u32).?);
    try std.testing.expectEqual(LIR.LowLevel.num_div_by_checked, checkedOp(.num_div_by, .i64).?);
    try std.testing.expectEqual(LIR.LowLevel.num_div_trunc_by_checked, checkedOp(.num_div_trunc_by, .u128).?);
    try std.testing.expectEqual(LIR.LowLevel.num_rem_by_checked, checkedOp(.num_rem_by, .i8).?);
    try std.testing.expectEqual(LIR.LowLevel.num_mod_by_checked, checkedOp(.num_mod_by, .i128).?);
    try std.testing.expectEqual(LIR.LowLevel.num_negate_checked, checkedOp(.num_negate, .i32).?);
    try std.testing.expectEqual(LIR.LowLevel.num_abs_checked, checkedOp(.num_abs, .i64).?);
    try std.testing.expectEqual(LIR.LowLevel.num_abs_checked, checkedOp(.num_abs, .dec).?);

    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_plus, .f64));
    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_abs, .u64));
    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_negate, .u128));
    try std.testing.expectEqual(LIR.LowLevel.num_int_add_crash_on_overflow, checkedOp(.num_plus, .dec).?);
    try std.testing.expectEqual(LIR.LowLevel.num_int_sub_crash_on_overflow, checkedOp(.num_minus, .dec).?);
    try std.testing.expectEqual(@as(?LIR.LowLevel, null), checkedOp(.num_times, .dec));
}

test "lowerOp commits every polymorphic arithmetic marker" {
    try std.testing.expectEqual(LIR.LowLevel.num_int_add_crash_on_overflow, lowerOp(.num_plus, .u8));
    try std.testing.expectEqual(LIR.LowLevel.num_int_sub_crash_on_overflow, lowerOp(.num_minus, .i16));
    try std.testing.expectEqual(LIR.LowLevel.num_int_mul_crash_on_overflow, lowerOp(.num_times, .u128));
    try std.testing.expectEqual(LIR.LowLevel.num_float_add, lowerOp(.num_plus, .f64));
    try std.testing.expectEqual(LIR.LowLevel.num_float_sub, lowerOp(.num_minus, .f32));
    try std.testing.expectEqual(LIR.LowLevel.num_float_mul, lowerOp(.num_times, .f64));
    try std.testing.expectEqual(LIR.LowLevel.num_int_add_crash_on_overflow, lowerOp(.num_plus, .dec));
    try std.testing.expectEqual(LIR.LowLevel.num_int_sub_crash_on_overflow, lowerOp(.num_minus, .dec));
    try std.testing.expectEqual(LIR.LowLevel.dec_mul, lowerOp(.num_times, .dec));
    try std.testing.expectEqual(LIR.LowLevel.num_abs_checked, lowerOp(.num_abs, .dec));

    for ([_]LIR.LowLevel{ .num_plus, .num_minus, .num_times }) |source_op| {
        for ([_]layout.Idx{ .u8, .i64, .u128, .i128, .dec, .f32, .f64 }) |layout_idx| {
            try std.testing.expect(!isSourcePolicyOp(lowerOp(source_op, layout_idx)));
            try std.testing.expect(classify(lowerOp(source_op, layout_idx)) == null or
                classify(lowerOp(source_op, layout_idx)).?.mode != .proven_cannot_overflow);
        }
    }
}

test "legacy checked operations round trip through uncheckedOp" {
    const checked_ops = [_]LIR.LowLevel{
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
        try std.testing.expectEqual(plain, provenForm(checked).?);
    }
}

test "integer arithmetic family is total injective and round trips" {
    var seen = std.EnumSet(LIR.LowLevel).initEmpty();
    inline for (std.meta.tags(Operation)) |operation| {
        inline for (std.meta.tags(Mode)) |mode| {
            const op = member(operation, mode);
            try std.testing.expect(!seen.contains(op));
            seen.insert(op);
            const entry = classify(op).?;
            try std.testing.expectEqual(operation, entry.operation);
            try std.testing.expectEqual(mode, entry.mode);
            try std.testing.expectEqual(op, entry.op);
            if (mode == .overflows) {
                try std.testing.expectEqual(@as(?LIR.LowLevel, null), provenForm(op));
            } else {
                try std.testing.expectEqual(member(operation, .proven_cannot_overflow), provenForm(op).?);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 12), seen.count());
    try std.testing.expectEqual(@as(?FamilyEntry, null), classify(.num_div_by));
}

test "checked arithmetic messages are canonical and operation specific" {
    try std.testing.expectEqualStrings("Integer addition overflowed", overflowMessage(.num_int_add_crash_on_overflow).?);
    try std.testing.expectEqualStrings("Integer subtraction overflowed", overflowMessage(.num_int_sub_crash_on_overflow).?);
    try std.testing.expectEqualStrings("Integer multiplication overflowed", overflowMessage(.num_int_mul_crash_on_overflow).?);
    try std.testing.expectEqualStrings("Integer negation overflowed", overflowMessage(.num_negate_checked).?);
    try std.testing.expectEqualStrings("Integer absolute value overflowed", overflowMessage(.num_abs_checked).?);
    try std.testing.expectEqualStrings("Integer division overflowed", overflowMessage(.num_div_by_checked).?);
    try std.testing.expectEqualStrings("Integer division overflowed", overflowMessage(.num_div_trunc_by_checked).?);
    try std.testing.expectEqualStrings("Decimal absolute value overflow!", overflowMessageForLayout(.num_abs_checked, .dec).?);

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
