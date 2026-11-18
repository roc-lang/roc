//! Target-independent rules for observable and compile-time float bit patterns.

const std = @import("std");

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

/// Copy the native-endian F32 bit patterns `source` holds, four bytes each,
/// into `out`, with every NaN replaced by Roc's canonical quiet NaN, exactly
/// as `normalizeF32NanBits` replaces each one.
pub fn copyNormalizingF32NanBits(out: []u8, source: []const u8) void {
    copyNormalizingNanBits(f32, u32, 16, normalized_f32_nan_bits, out, source);
}

/// Copy the native-endian F64 bit patterns `source` holds, eight bytes each,
/// into `out`, with every NaN replaced by Roc's canonical quiet NaN, exactly
/// as `normalizeF64NanBits` replaces each one.
pub fn copyNormalizingF64NanBits(out: []u8, source: []const u8) void {
    copyNormalizingNanBits(f64, u64, 8, normalized_f64_nan_bits, out, source);
}

/// One pass over whole vectors of `lanes` patterns: each is loaded, its NaN
/// lanes found by comparing it with itself as floats (one unordered
/// compare), those lanes replaced with `canonical_nan` by a select on the
/// patterns as integers, so every other pattern is stored with its bits
/// unchanged, and the result stored to `out`. The patterns past the last
/// whole vector are copied one at a time.
fn copyNormalizingNanBits(
    comptime Float: type,
    comptime Bits: type,
    comptime lanes: comptime_int,
    comptime canonical_nan: Bits,
    out: []u8,
    source: []const u8,
) void {
    const width = @sizeOf(Bits);
    std.debug.assert(out.len == source.len and source.len % width == 0);
    const Patterns = @Vector(lanes, Bits);
    const Floats = @Vector(lanes, Float);
    const vector_bytes = lanes * width;
    var offset: usize = 0;
    while (offset + vector_bytes <= source.len) : (offset += vector_bytes) {
        // Loaded and stored through vector pointers, which compile to
        // unaligned vector moves.
        const patterns = @as(*align(1) const Patterns, @ptrCast(source[offset..][0..vector_bytes])).*;
        const floats: Floats = @bitCast(patterns);
        const nan = floats != floats;
        @as(*align(1) Patterns, @ptrCast(out[offset..][0..vector_bytes])).* = @select(Bits, nan, @as(Patterns, @splat(canonical_nan)), patterns);
    }
    while (offset < source.len) : (offset += width) {
        const pattern = std.mem.readInt(Bits, source[offset..][0..width], .native);
        const float: Float = @bitCast(pattern);
        std.mem.writeInt(Bits, out[offset..][0..width], if (float != float) canonical_nan else pattern, .native);
    }
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

test "copying while normalizing NaNs matches normalizing each pattern" {
    // Signs and payloads of quiet and signaling NaNs, infinities, zeros,
    // subnormals, the largest finite values and ordinary numbers, repeated
    // to a count that leaves a partial vector at the end.
    const f32_patterns = [_]u32{
        0x7f80_0001, 0x7fa1_2345, 0x7fc0_0000, 0x7fff_ffff, 0xff80_0001, 0xffc1_2345, 0xffff_ffff,
        0x7f80_0000, 0xff80_0000, 0x0000_0000, 0x8000_0000, 0x0000_0001, 0x807f_ffff, 0x7f7f_ffff,
        0xff7f_ffff, 0x3f80_0000, 0xbf80_0000,
    };
    var f32_bits: [f32_patterns.len * 3 + 5]u32 = undefined;
    for (&f32_bits, 0..) |*bits, index| bits.* = f32_patterns[index % f32_patterns.len];
    // One byte past the storage's start, so the patterns are unaligned.
    var f32_source_storage: [f32_bits.len * 4 + 1]u8 = undefined;
    const f32_source = f32_source_storage[1..];
    for (f32_bits, 0..) |bits, index| std.mem.writeInt(u32, f32_source[index * 4 ..][0..4], bits, .native);
    var f32_out_storage: [f32_bits.len * 4 + 3]u8 = undefined;
    const f32_out = f32_out_storage[3..];
    copyNormalizingF32NanBits(f32_out, f32_source);
    for (f32_bits, 0..) |bits, index| {
        try std.testing.expectEqual(normalizeF32NanBits(bits), std.mem.readInt(u32, f32_out[index * 4 ..][0..4], .native));
    }

    const f64_patterns = [_]u64{
        0x7ff0_0000_0000_0001, 0x7ff1_2345_6789_abcd, 0x7ff8_0000_0000_0000, 0x7fff_ffff_ffff_ffff,
        0xfff0_0000_0000_0001, 0xfff9_2345_6789_abcd, 0xffff_ffff_ffff_ffff, 0x7ff0_0000_0000_0000,
        0xfff0_0000_0000_0000, 0x0000_0000_0000_0000, 0x8000_0000_0000_0000, 0x0000_0000_0000_0001,
        0x800f_ffff_ffff_ffff, 0x7fef_ffff_ffff_ffff, 0xffef_ffff_ffff_ffff, 0x3ff0_0000_0000_0000,
        0xbff0_0000_0000_0000,
    };
    var f64_bits: [f64_patterns.len * 3 + 3]u64 = undefined;
    for (&f64_bits, 0..) |*bits, index| bits.* = f64_patterns[index % f64_patterns.len];
    var f64_source_storage: [f64_bits.len * 8 + 1]u8 = undefined;
    const f64_source = f64_source_storage[1..];
    for (f64_bits, 0..) |bits, index| std.mem.writeInt(u64, f64_source[index * 8 ..][0..8], bits, .native);
    var f64_out_storage: [f64_bits.len * 8 + 5]u8 = undefined;
    const f64_out = f64_out_storage[5..];
    copyNormalizingF64NanBits(f64_out, f64_source);
    for (f64_bits, 0..) |bits, index| {
        try std.testing.expectEqual(normalizeF64NanBits(bits), std.mem.readInt(u64, f64_out[index * 8 ..][0..8], .native));
    }
}
