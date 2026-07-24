//! Target-independent rules for observable and compile-time float bit patterns.

const std = @import("std");

/// Whether an evaluator preserves the target's NaN result bits or rewrites
/// every NaN to Roc's single normalized quiet-NaN representation.
pub const NanMode = enum {
    preserve,
    normalize,
};

/// Roc's canonical F32 quiet-NaN bit pattern.
pub const normalized_f32_nan_bits: u32 = 0x7fc0_0000;
/// Roc's canonical F64 quiet-NaN bit pattern.
pub const normalized_f64_nan_bits: u64 = 0x7ff8_0000_0000_0000;

/// Return whether `bits` encodes any F32 NaN.
pub fn isF32NanBits(bits: u32) bool {
    return bits & 0x7fff_ffff > 0x7f80_0000;
}

/// Return whether `bits` encodes any F64 NaN.
pub fn isF64NanBits(bits: u64) bool {
    return bits & 0x7fff_ffff_ffff_ffff > 0x7ff0_0000_0000_0000;
}

/// Replace any F32 NaN encoding with Roc's canonical quiet NaN.
pub fn normalizeF32NanBits(bits: u32) u32 {
    return if (isF32NanBits(bits)) normalized_f32_nan_bits else bits;
}

/// Replace any F64 NaN encoding with Roc's canonical quiet NaN.
pub fn normalizeF64NanBits(bits: u64) u64 {
    return if (isF64NanBits(bits)) normalized_f64_nan_bits else bits;
}

test "NaN normalization preserves every non-NaN bit pattern" {
    const f32_non_nans = [_]u32{
        0x0000_0000,
        0x8000_0000,
        0x0000_0001,
        0x3f80_0000,
        0x7f7f_ffff,
        0x7f80_0000,
        0xff80_0000,
    };
    for (f32_non_nans) |bits| {
        try std.testing.expectEqual(bits, normalizeF32NanBits(bits));
    }

    const f64_non_nans = [_]u64{
        0x0000_0000_0000_0000,
        0x8000_0000_0000_0000,
        0x0000_0000_0000_0001,
        0x3ff0_0000_0000_0000,
        0x7fef_ffff_ffff_ffff,
        0x7ff0_0000_0000_0000,
        0xfff0_0000_0000_0000,
    };
    for (f64_non_nans) |bits| {
        try std.testing.expectEqual(bits, normalizeF64NanBits(bits));
    }
}

test "NaN normalization collapses signs, payloads, and signaling encodings" {
    const f32_nans = [_]u32{
        0x7f80_0001,
        0x7fa1_2345,
        0x7fc0_0000,
        0x7fff_ffff,
        0xff80_0001,
        0xffc1_2345,
    };
    for (f32_nans) |bits| {
        try std.testing.expectEqual(normalized_f32_nan_bits, normalizeF32NanBits(bits));
    }

    const f64_nans = [_]u64{
        0x7ff0_0000_0000_0001,
        0x7ff1_2345_6789_abcd,
        0x7ff8_0000_0000_0000,
        0x7fff_ffff_ffff_ffff,
        0xfff0_0000_0000_0001,
        0xfff9_2345_6789_abcd,
    };
    for (f64_nans) |bits| {
        try std.testing.expectEqual(normalized_f64_nan_bits, normalizeF64NanBits(bits));
    }
}
