//! The structure every scalar numeric conversion low-level carries in its name.
//!
//! `LowLevel` names each of these ops after the conversion it performs:
//! a source scalar, a destination scalar, and the mode that says what happens
//! to values the destination cannot represent. `dec_to_u64_trunc` is a `Dec`
//! source, a `u64` destination, and truncating semantics.
//!
//! Backends need those three facts, not the name. Recovering them means
//! parsing, and a parse that is wrong—or that quietly accepts an op it does
//! not understand—yields working code for the wrong arithmetic: not a type
//! error and not a test failure, but a silent miscompile. So the parse happens
//! once, here, and `Family` names the sixteen source-class, destination-class,
//! and mode combinations the ops form. A backend switches on the family
//! exhaustively, which makes an unhandled combination a compile error in the
//! compiler rather than a wrong answer in the compiled program.
//!
//! Two comptime rules run over the whole enum:
//!
//! 1. The triple identifies the op. Rebuilding a name from its triple
//!    reproduces the original, so the parse is lossless. Two ops sharing a
//!    triple would rebuild to one name, which an enum cannot have twice.
//! 2. Every classified op has a family. An op whose shape no family covers is
//!    a compile error naming that op, so adding one to `LowLevel` forces the
//!    decision about how it lowers.

const std = @import("std");

const LowLevel = @import("LowLevel.zig").LowLevel;

/// A scalar a conversion can read from or write to.
pub const Scalar = enum {
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

    /// Width of the value in bits. A `Dec` is stored as a 128-bit payload.
    pub fn bits(self: Scalar) u16 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32, .f32 => 32,
            .i64, .u64, .f64 => 64,
            .i128, .u128, .dec => 128,
        };
    }

    /// Whether the representation has a sign. `u8` through `u128` are the only
    /// unsigned scalars: a `Dec` payload is a signed `i128`, and both floats
    /// carry a sign bit.
    pub fn isSigned(self: Scalar) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .u128 => false,
            .i8, .i16, .i32, .i64, .i128, .f32, .f64, .dec => true,
        };
    }

    /// Which of the three representations this scalar uses. Conversions
    /// dispatch on this rather than on the individual scalar, because the
    /// instruction sequence depends on the representation and the width is
    /// then a parameter of it.
    pub fn class(self: Scalar) Class {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128, .u8, .u16, .u32, .u64, .u128 => .int,
            .f32, .f64 => .float,
            .dec => .dec,
        };
    }
};

/// How a scalar is represented, which is what decides the instruction sequence.
pub const Class = enum { int, float, dec };

/// What a conversion does with a value the destination cannot represent.
pub const Mode = enum {
    /// Defined for every source value, so there is nothing to decide. The
    /// result may still round, as `u128_to_f32` does.
    exact,
    /// Keep the low bits of the destination width.
    wrap,
    /// Discard the fractional part, then keep the low bits of the destination
    /// width.
    trunc,
    /// Return a `Try`, tagging values that do not fit.
    @"try",
    /// Return the payload of that `Try` without the tag, for a caller that has
    /// already established the value fits.
    try_unsafe,
};

/// The source, destination, and mode named by a conversion op.
pub const Conversion = struct {
    src: Scalar,
    dst: Scalar,
    mode: Mode,

    /// The combination this conversion belongs to, or null if its shape is one
    /// no family covers. The comptime rules below reject a `LowLevel` op that
    /// lands here, so a null at runtime means the caller built the
    /// `Conversion` itself rather than classifying an op.
    pub fn family(self: Conversion) ?Family {
        return switch (self.src.class()) {
            .int => switch (self.dst.class()) {
                .int => switch (self.mode) {
                    .exact => .int_to_int_exact,
                    .wrap => .int_to_int_wrap,
                    .@"try" => .int_to_int_try,
                    .trunc, .try_unsafe => null,
                },
                .float => switch (self.mode) {
                    .exact => .int_to_float_exact,
                    .wrap, .trunc, .@"try", .try_unsafe => null,
                },
                .dec => switch (self.mode) {
                    .exact => .int_to_dec_exact,
                    .try_unsafe => .int_to_dec_try_unsafe,
                    .wrap, .trunc, .@"try" => null,
                },
            },
            .float => switch (self.dst.class()) {
                .int => switch (self.mode) {
                    .trunc => .float_to_int_trunc,
                    .try_unsafe => .float_to_int_try_unsafe,
                    .exact, .wrap, .@"try" => null,
                },
                .float => switch (self.mode) {
                    .exact => .float_to_float_exact,
                    .wrap => .float_to_float_wrap,
                    .try_unsafe => .float_to_float_try_unsafe,
                    .trunc, .@"try" => null,
                },
                .dec => null,
            },
            .dec => switch (self.dst.class()) {
                .int => switch (self.mode) {
                    .trunc => .dec_to_int_trunc,
                    .try_unsafe => .dec_to_int_try_unsafe,
                    .exact, .wrap, .@"try" => null,
                },
                .float => switch (self.mode) {
                    .exact => .dec_to_float_exact,
                    .wrap => .dec_to_float_wrap,
                    .try_unsafe => .dec_to_float_try_unsafe,
                    .trunc, .@"try" => null,
                },
                .dec => null,
            },
        };
    }
};

/// The source-class, destination-class, and mode combinations the conversion
/// ops form. A backend that switches over this without an `else` prong gets a
/// compile error when a combination is added.
pub const Family = enum {
    int_to_int_exact,
    int_to_int_wrap,
    int_to_int_try,
    int_to_float_exact,
    int_to_dec_exact,
    int_to_dec_try_unsafe,
    float_to_float_exact,
    float_to_float_wrap,
    float_to_float_try_unsafe,
    float_to_int_trunc,
    float_to_int_try_unsafe,
    dec_to_float_exact,
    dec_to_float_wrap,
    dec_to_float_try_unsafe,
    dec_to_int_trunc,
    dec_to_int_try_unsafe,
};

/// The conversion an op performs, or null if the op is not a scalar numeric
/// conversion. `str_to_utf8`, `u8_to_str`, and `dec_to_attos` are named the
/// same way but do not convert between two scalars, so they classify as null.
pub fn classify(op: LowLevel) ?Conversion {
    return table[@intFromEnum(op)];
}

/// Modes ordered so that a suffix which contains another is tested first.
const suffix_order = [_]Mode{ .try_unsafe, .trunc, .wrap, .@"try" };

/// Split a name into its three parts. Returns null unless both ends name a
/// `Scalar`, which is what separates a conversion from an op such as
/// `f32_to_bits` that shares the shape but not the meaning.
fn parse(name: []const u8) ?Conversion {
    const separator = std.mem.find(u8, name, "_to_") orelse return null;
    const src = std.meta.stringToEnum(Scalar, name[0..separator]) orelse return null;

    var rest: []const u8 = name[separator + "_to_".len ..];
    var mode: Mode = .exact;
    for (suffix_order) |candidate| {
        const suffix: []const u8 = "_" ++ @tagName(candidate);
        if (std.mem.endsWith(u8, rest, suffix)) {
            rest = rest[0 .. rest.len - suffix.len];
            mode = candidate;
            break;
        }
    }

    const dst = std.meta.stringToEnum(Scalar, rest) orelse return null;
    return .{ .src = src, .dst = dst, .mode = mode };
}

/// The name an op with this conversion would have, used to check that parsing
/// loses nothing.
fn rebuildName(comptime conversion: Conversion) []const u8 {
    const base = @tagName(conversion.src) ++ "_to_" ++ @tagName(conversion.dst);
    return if (conversion.mode == .exact) base else base ++ "_" ++ @tagName(conversion.mode);
}

const table = blk: {
    @setEvalBranchQuota(200_000);
    const fields = @typeInfo(LowLevel).@"enum".fields;
    var entries: [fields.len]?Conversion = @splat(null);
    for (fields) |field| entries[field.value] = parse(field.name);
    break :blk entries;
};

/// Run both rules over the whole enum. Called from `base`'s comptime block.
pub fn assertTableConforms() void {
    assertClassificationIsLossless();
    assertEveryConversionHasAFamily();
}

/// Rule 1: the triple identifies the op. Rebuilding a name from its parts
/// reproduces the original, so a backend that dispatches on the triple is
/// dispatching on the op. Uniqueness follows: two ops with one triple would
/// rebuild to one name, and enum field names are distinct.
fn assertClassificationIsLossless() void {
    @setEvalBranchQuota(200_000);
    for (@typeInfo(LowLevel).@"enum".fields) |field| {
        const conversion = table[field.value] orelse continue;
        if (!std.mem.eql(u8, rebuildName(conversion), field.name)) {
            @compileError("numeric conversion '" ++ field.name ++ "' parses as '" ++
                rebuildName(conversion) ++ "', so its name and its parts disagree");
        }
    }
}

/// Rule 2: every conversion belongs to a family. An op whose shape no family
/// covers would otherwise reach a backend with nothing to dispatch on, which
/// is the position `dec_to_*_trunc` was in.
fn assertEveryConversionHasAFamily() void {
    @setEvalBranchQuota(200_000);
    for (@typeInfo(LowLevel).@"enum".fields) |field| {
        const conversion = table[field.value] orelse continue;
        if (conversion.family() == null) {
            @compileError("numeric conversion '" ++ field.name ++ "' converts " ++
                @tagName(conversion.src.class()) ++ " to " ++ @tagName(conversion.dst.class()) ++
                " with mode '" ++ @tagName(conversion.mode) ++
                "', which is not one of the families backends handle; add it to Family and to every backend's switch");
        }
    }
}

test "classify reads the parts out of the name" {
    try std.testing.expectEqual(
        Conversion{ .src = .dec, .dst = .u64, .mode = .trunc },
        classify(.dec_to_u64_trunc).?,
    );
    try std.testing.expectEqual(
        Conversion{ .src = .u8, .dst = .i16, .mode = .exact },
        classify(.u8_to_i16).?,
    );
    try std.testing.expectEqual(
        Conversion{ .src = .f64, .dst = .f32, .mode = .try_unsafe },
        classify(.f64_to_f32_try_unsafe).?,
    );
}

test "ops that share the naming shape but not the meaning classify as null" {
    try std.testing.expectEqual(@as(?Conversion, null), classify(.str_to_utf8));
    try std.testing.expectEqual(@as(?Conversion, null), classify(.u8_to_str));
    try std.testing.expectEqual(@as(?Conversion, null), classify(.dec_to_attos));
    try std.testing.expectEqual(@as(?Conversion, null), classify(.f32_to_bits));
}

test "the widths a conversion computes at come from its scalars" {
    try std.testing.expectEqual(@as(u16, 128), classify(.dec_to_i8_trunc).?.src.bits());
    try std.testing.expectEqual(@as(u16, 8), classify(.dec_to_i8_trunc).?.dst.bits());
    try std.testing.expect(classify(.dec_to_i8_trunc).?.src.isSigned());
    try std.testing.expect(!classify(.f32_to_u32_trunc).?.dst.isSigned());
}

test "families group the ops backends lower the same way" {
    try std.testing.expectEqual(Family.dec_to_int_trunc, classify(.dec_to_u128_trunc).?.family().?);
    try std.testing.expectEqual(Family.dec_to_int_trunc, classify(.dec_to_i8_trunc).?.family().?);
    try std.testing.expectEqual(Family.int_to_int_wrap, classify(.u8_to_i8_wrap).?.family().?);
    try std.testing.expectEqual(Family.float_to_int_trunc, classify(.f64_to_i128_trunc).?.family().?);
}
