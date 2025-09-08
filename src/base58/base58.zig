//! Base58 encoding and decoding for BLAKE3 hashes
//!
//! This module provides base58 encoding/decoding specifically optimized for 256-bit BLAKE3 hashes.
//! The base58 alphabet excludes visually similar characters (0, O, I, l) to prevent confusion.

const std = @import("std");

// Base58 alphabet (no '0', 'O', 'I', or 'l' to deter visual similarity attacks.)
const base58_alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

// LUT: base58 char -> its index in the alphabet
const base58_alphabet_reverse: [256]?u6 = blk: {
    var b: [256]?u6 = @splat(null);
    for (base58_alphabet, 0..) |c, i| {
        b[c] = @intCast(i);
    }
    break :blk b;
};

/// We use 256-bit BLAKE3 hashes
const hash_bytes = 32;

/// Number of Base58 characters needed to represent a 256-bit hash
///
/// Each base58 character can represent 58 values
/// - So we need ⌈log₅₈(MAX_HASH + 1)⌉ characters
/// - Which is ⌈log₂(MAX_HASH + 1) / log₂(58)⌉
/// - Since MAX_HASH = 2²⁵⁶ - 1, we need ⌈256 ⋅ log₂(2) / log₂(58)⌉
/// - 256 ⋅ log₂(2) / log₂(58) ≈ 43.7
///
/// So we need 44 characters.
pub const base58_hash_bytes = 44;

pub fn encode(src: [hash_bytes]u8, dest: *[base58_hash_bytes]u8) []u8 {
    var value: u256 = std.mem.bytesToValue(u256, &src);
    var write_idx: usize = base58_hash_bytes;

    if (value == 0) {
        dest[write_idx - 1] = '1';
        write_idx -= 1;
    }

    while (value != 0) {
        const remainder: u8 = @intCast(value % 58);
        value /= 58;
        write_idx -= 1;
        dest[write_idx] = base58_alphabet[remainder];
    }
    return dest[write_idx..];
}

pub fn decode(src: []const u8) error{InvalidBase58}![hash_bytes]u8 {
    if (src.len > base58_hash_bytes) return error.InvalidBase58;

    var result: u256 = 0;
    for (src) |c| {
        if (base58_alphabet_reverse[c]) |entry| {
            result, const of1 = @mulWithOverflow(result, 58);
            result, const of2 = @addWithOverflow(result, entry);
            if (of1 == 1 or of2 == 1) return error.InvalidBase58;
        } else {
            return error.InvalidBase58;
        }
    }
    return std.mem.toBytes(result);
}

// Tools used:
// https://learnmeabitcoin.com/technical/keys/base58/
// https://appdevtools.com/base58-encoder-decoder
const known_values = [_]struct { u256, []const u8 }{
    .{ 0x0, "1" },
    .{ 0x0d9e, "237" },
    .{ 0x0d9e0d9e, "MBvnh" },
    .{ 0xdeadbeef, "6h8cQN" },
    .{ 0x12345678, "TzMhH" },
    .{ 0x0f1e6b1421c04a070431265c19c5bbee1992bae8afd1cd078ef8af7047dc11f7, "22222222222222222222222222222222222222222222" },
    .{ 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff, "JEKNVnkbo3jma5nREBBJCDoXFVeKkD56V3xKrvRmWxFG" },
    .{ 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0, "JEKNVnkbo3jma5nREBBJCDoXFVeKkD56V3xKrvRmWxF1" },
};

test "encode known values" {
    for (known_values) |case| {
        const input, const expected_output = case;
        const input_bytes = std.mem.toBytes(input);
        var buffer: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(input_bytes, &buffer);

        try std.testing.expectEqualStrings(expected_output, encoded);
    }
}

test "decode known values" {
    for (known_values) |case| {
        const expected_output, const input = case;
        const decoded_bytes = try decode(input);
        const decoded = std.mem.bytesToValue(u256, &decoded_bytes);

        try std.testing.expectEqual(expected_output, decoded);
    }
}

test "roundtrips" {
    var rng: std.Random.DefaultPrng = .init(0);
    for (0..1024) |_| {
        var src: [32]u8 = undefined;
        rng.fill(&src);

        var buffer: [base58_hash_bytes]u8 = undefined;

        const round = try decode(encode(src, &buffer));
        try std.testing.expectEqual(src, round);
    }
}

test "decode invalid" {
    const invalid = [_][]const u8{
        "0",
        "O",
        "I",
        "l",
        "!",
        "JEKNVnkbo3jma5nREBBJCDoXFVeKkD56V3xKrvRmWxFH", // first that doesn't fit in u256
    };
    for (invalid) |c| {
        _ = try std.testing.expectError(error.InvalidBase58, decode(c));
    }
}
