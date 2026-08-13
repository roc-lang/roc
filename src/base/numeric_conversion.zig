//! This module helps the backends and the interpreter lower each numeric
//! conversion low-level to the right instructions.
//!
//! Given a name such as `f32_to_i8_try_unsafe`, it parses out the source type
//! (`f32`), the destination type (`i8`), and the mode (`try_unsafe`) into a
//! `Conversion`. Every op's `Conversion` is built into a table at comptime, so a
//! consumer reads those fields from the table instead of parsing the name again.
//!
//! The `assert*` functions below run at comptime over every `LowLevel` variant,
//! matching every name containing `_to_`. They keep the table from drifting
//! away from the enum. Every such name must parse into fields or be listed as an
//! exception, and rebuilding the name from the fields must match the original.

const std = @import("std");

const LowLevel = @import("LowLevel.zig").LowLevel;
const NumericClass = @import("LowLevelBuiltins.zig").NumericClass;

/// Comptime branch budget for the passes over `LowLevel` below. Zig's default of
/// 1000 is far short of what parsing every variant name costs: 2^14 still fails
/// and 2^15 clears it. If the enum grows past this, the build says so and the fix
/// is the next power of two.
const eval_branch_quota = 1 << 15;

/// A machine number type that a conversion reads from or writes to.
pub const NumType = enum {
    i8,
    i16,
    i32,
    i64,
    i128,
    u8,
    u16,
    u32,
    u64,
    u128,
    f32,
    f64,
    dec,

    /// Width of the value in bits. Note that a `Dec` is stored as an `i128`.
    pub fn bits(self: NumType) u8 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32, .f32 => 32,
            .i64, .u64, .f64 => 64,
            .i128, .u128, .dec => 128,
        };
    }

    /// Width of the value in bytes.
    pub fn bytes(self: NumType) u8 {
        return self.bits() / 8;
    }

    /// Whether this type is signed.
    pub fn isSigned(self: NumType) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .u128 => false,
            .i8, .i16, .i32, .i64, .i128, .f32, .f64, .dec => true,
        };
    }

    /// The Zig type used to read and write values of this `NumType`.
    pub fn ZigType(comptime self: NumType) type {
        return switch (self) {
            .i8 => i8,
            .i16 => i16,
            .i32 => i32,
            .i64 => i64,
            .i128 => i128,
            .u8 => u8,
            .u16 => u16,
            .u32 => u32,
            .u64 => u64,
            .u128 => u128,
            .f32 => f32,
            .f64 => f64,
            .dec => @compileError("Dec has no Zig type; ask for its .i128 payload"),
        };
    }

    /// Whether this type is an integer, a float, or a `Dec`.
    pub fn class(self: NumType) NumericClass {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128, .u8, .u16, .u32, .u64, .u128 => .int,
            .f32, .f64 => .float,
            .dec => .dec,
        };
    }
};

/// What a conversion does with a value that the destination cannot represent
/// exactly.
pub const Mode = enum {
    /// The conversion is defined for every source value. The result may still
    /// round, as `u128_to_f32` does.
    exact,
    /// Fit the value to the destination. An integer destination takes the value
    /// modulo its range. A float destination rounds to the nearest value that it can
    /// represent, which is an infinity once the magnitude exceeds its range.
    wrap,
    /// Discard the fractional part, then keep the low-order bits that fit the
    /// destination width.
    trunc,
    /// Return a `Try`, tagging values that do not fit.
    @"try",
    /// Return a flat `{ success, payload }` record whose payload is meaningless
    /// when `success` is false. Roc's `out_of_range_try` lifts it into a `Try`.
    try_unsafe,
};

/// What one conversion op does: the exact types that it reads and writes, and
/// how it handles a value that does not fit.
pub const Conversion = struct {
    src: NumType,
    dst: NumType,
    mode: Mode,
};

/// Look up the conversion that `op` performs, or null if it does not convert
/// between number types.
pub fn getConversionSpec(op: LowLevel) ?Conversion {
    return conversion_by_op[@intFromEnum(op)];
}

/// Look up a `NumType` by name. Returns null when no variant has that name.
/// Hand-rolled because `std.meta.stringToEnum` builds a comptime map on every
/// call, which costs most of the branch budget.
fn numTypeFromName(name: []const u8) ?NumType {
    inline for (@typeInfo(NumType).@"enum".fields) |field| {
        if (std.mem.eql(u8, name, field.name)) return @field(NumType, field.name);
    }
    return null;
}

/// Look up a `Mode` by name. Returns null when no variant has that name.
fn modeFromName(name: []const u8) ?Mode {
    inline for (@typeInfo(Mode).@"enum".fields) |field| {
        if (std.mem.eql(u8, name, field.name)) return @field(Mode, field.name);
    }
    return null;
}

/// Split an op name on `_` into `src`, `"to"`, `dst`, and the tail. Each part
/// must name a `NumType`, the literal `to`, and a `Mode`; an empty tail means
/// `exact`. The tail is taken whole, so `try_unsafe` survives the split. This is
/// what separates `dec_to_u64_trunc` from `f32_to_bits`.
fn conversionFromName(name: []const u8) ?Conversion {
    var parts = std.mem.splitScalar(u8, name, '_');
    const src = numTypeFromName(parts.next() orelse return null) orelse return null;
    if (!std.mem.eql(u8, parts.next() orelse return null, "to")) return null;
    const dst = numTypeFromName(parts.next() orelse return null) orelse return null;
    const tail = parts.rest();
    const mode = if (tail.len == 0) Mode.exact else modeFromName(tail) orelse return null;
    return .{ .src = src, .dst = dst, .mode = mode };
}

/// Rebuild a name from its three parts, inverting `conversionFromName`.
fn rebuildName(comptime conversion: Conversion) []const u8 {
    const stem = @tagName(conversion.src) ++ "_to_" ++ @tagName(conversion.dst);
    return if (conversion.mode == .exact) stem else stem ++ "_" ++ @tagName(conversion.mode);
}

/// Every op's `Conversion`, indexed by enum value; null where the op is not a
/// numeric conversion. Built by parsing every name at comptime.
const conversion_by_op = blk: {
    @setEvalBranchQuota(eval_branch_quota);
    const fields = @typeInfo(LowLevel).@"enum".fields;
    var entries: [fields.len]?Conversion = @splat(null);
    for (fields) |field| entries[field.value] = conversionFromName(field.name);
    break :blk entries;
};

/// Check every `LowLevel` variant.
pub fn assertTableConforms() void {
    assertClassificationIsComplete();
    assertClassificationIsLossless();
}

/// Ops whose names contain `_to_` but do not convert between two number types.
const not_conversions = [_]LowLevel{
    .str_to_utf8,
    .u8_to_str,
    .i8_to_str,
    .u16_to_str,
    .i16_to_str,
    .u32_to_str,
    .i32_to_str,
    .u64_to_str,
    .i64_to_str,
    .u128_to_str,
    .i128_to_str,
    .dec_to_str,
    .f32_to_str,
    .f64_to_str,
    .num_to_str,
    .f32_to_bits,
    .f64_to_bits,
    .simd_to_u128_bits,
    .dec_to_attos,
};

/// Every op whose name contains `_to_` is classified as a conversion, or is
/// listed in `not_conversions`.
fn assertClassificationIsComplete() void {
    @setEvalBranchQuota(eval_branch_quota);
    for (@typeInfo(LowLevel).@"enum".fields) |field| {
        if (std.mem.find(u8, field.name, "_to_") == null) continue;
        if (conversion_by_op[field.value] != null) continue;
        for (not_conversions) |excluded| {
            if (field.value == @intFromEnum(excluded)) break;
        } else {
            @compileError("'" ++ field.name ++ "' is named like a numeric conversion but does not" ++
                " classify as one; add it to not_conversions if that is right, or fix `conversionFromName`");
        }
    }
}

/// Rebuilding a name from its three parts reproduces the original.
fn assertClassificationIsLossless() void {
    @setEvalBranchQuota(eval_branch_quota);
    for (@typeInfo(LowLevel).@"enum".fields) |field| {
        const conversion = conversion_by_op[field.value] orelse continue;
        if (!std.mem.eql(u8, rebuildName(conversion), field.name)) {
            @compileError("numeric conversion '" ++ field.name ++ "' parses as '" ++
                rebuildName(conversion) ++ "', so its name and its parts disagree");
        }
    }
}

test "getConversionSpec reads the parts out of the name" {
    try std.testing.expectEqual(
        Conversion{ .src = .dec, .dst = .u64, .mode = .trunc },
        getConversionSpec(.dec_to_u64_trunc).?,
    );
    try std.testing.expectEqual(
        Conversion{ .src = .u8, .dst = .i16, .mode = .exact },
        getConversionSpec(.u8_to_i16).?,
    );
    try std.testing.expectEqual(
        Conversion{ .src = .f64, .dst = .f32, .mode = .try_unsafe },
        getConversionSpec(.f64_to_f32_try_unsafe).?,
    );
}

test "every mode is reachable from a real op" {
    try std.testing.expectEqual(Mode.exact, getConversionSpec(.u8_to_i16).?.mode);
    try std.testing.expectEqual(Mode.wrap, getConversionSpec(.f64_to_f32_wrap).?.mode);
    try std.testing.expectEqual(Mode.trunc, getConversionSpec(.dec_to_u64_trunc).?.mode);
    try std.testing.expectEqual(Mode.@"try", getConversionSpec(.u8_to_i8_try).?.mode);
    try std.testing.expectEqual(Mode.try_unsafe, getConversionSpec(.f64_to_f32_try_unsafe).?.mode);
}

test "ops that share the naming shape but not the meaning classify as null" {
    try std.testing.expectEqual(@as(?Conversion, null), getConversionSpec(.str_to_utf8));
    try std.testing.expectEqual(@as(?Conversion, null), getConversionSpec(.u8_to_str));
    try std.testing.expectEqual(@as(?Conversion, null), getConversionSpec(.dec_to_attos));
    try std.testing.expectEqual(@as(?Conversion, null), getConversionSpec(.simd_to_u128_bits));
}

test "the widths a conversion computes at come from its types" {
    try std.testing.expectEqual(@as(u16, 128), getConversionSpec(.dec_to_i8_trunc).?.src.bits());
    try std.testing.expectEqual(@as(u16, 1), getConversionSpec(.dec_to_i8_trunc).?.dst.bytes());
    try std.testing.expect(getConversionSpec(.dec_to_i8_trunc).?.src.isSigned());
    try std.testing.expect(!getConversionSpec(.f32_to_u32_trunc).?.dst.isSigned());
}

test "classes group the types a consumer dispatches on" {
    try std.testing.expectEqual(NumericClass.dec, getConversionSpec(.dec_to_u128_trunc).?.src.class());
    try std.testing.expectEqual(NumericClass.int, getConversionSpec(.dec_to_u128_trunc).?.dst.class());
    try std.testing.expectEqual(NumericClass.float, getConversionSpec(.f64_to_i128_trunc).?.src.class());
}
