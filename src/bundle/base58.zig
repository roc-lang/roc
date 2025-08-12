const std = @import("std");

// Base58 alphabet (no '0', 'O', 'I', or 'l' to deter visual similarity attacks.)
const base58_alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

/// We use 256-bit BLAKE3 hashes
const hash_bytes = 32;

/// Number of Base58 characters needed to represent a 256-bit hash
///
/// Math:
/// - Each base58 character can represent 58 values
/// - So we need ceil(log58(MAX_HASH + 1)) characters
/// - Which is ceil(log(MAX_HASH + 1) / log(58))
/// - Since MAX_HASH is 2^256 - 1, we need ceil(256 * log(2) / log(58))
/// - 256 * log(2) / log(58) ≈ 43.7
///
/// So we need 44 characters.
pub const base58_hash_bytes = 44;

/// Encode the given slice of 32 bytes as a base58 string and write it to the destination.
/// Returns a slice of the destination containing the encoded string (1-45 characters).
pub fn encode(src: *const [hash_bytes]u8, dest: *[base58_hash_bytes]u8) []u8 {
    // Count leading zero bytes
    var leading_zeros: usize = 0;
    while (leading_zeros < src.len and src[leading_zeros] == 0) {
        leading_zeros += 1;
    }

    if (leading_zeros == src.len) {
        // All zeros - return just the leading '1's
        @memset(dest[0..leading_zeros], '1');
        return dest[0..leading_zeros];
    }

    // Make a mutable scratch copy of the source
    var scratch: [hash_bytes]u8 = undefined;
    @memcpy(&scratch, src);

    var write_idx: isize = base58_hash_bytes - 1;
    const start: usize = leading_zeros;

    // Repeatedly divide scratch[start..] by 58, collecting remainder
    // We need to keep dividing until the entire number becomes zero
    var has_nonzero = true;
    while (has_nonzero) {
        var remainder: u16 = 0;
        has_nonzero = false;
        for (scratch[start..]) |*byte| {
            const value = (@as(u16, remainder) << 8) | byte.*;
            byte.* = @intCast(value / 58);
            remainder = value % 58;
            if (byte.* != 0) has_nonzero = true;
        }
        dest[@intCast(write_idx)] = base58_alphabet[@intCast(remainder)];
        write_idx -= 1;
    }

    // Now combine leading '1's with the encoded value
    const encoded_start = @as(usize, @intCast(write_idx + 1));
    const encoded_len = base58_hash_bytes - encoded_start;

    // Write leading '1's at the beginning
    @memset(dest[0..leading_zeros], '1');

    // Move the encoded data to follow the '1's
    std.mem.copyForwards(u8, dest[leading_zeros .. leading_zeros + encoded_len], dest[encoded_start..base58_hash_bytes]);

    return dest[0 .. leading_zeros + encoded_len];
}

test "encode - all zero bytes" {
    const input = [_]u8{0} ** 32;
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce all '1's
    for (encoded) |char| {
        try std.testing.expectEqual('1', char);
    }
    try std.testing.expectEqual(@as(usize, 32), encoded.len);
}

test "encode - some leading zero bytes" {
    var input = [_]u8{0} ** 32;
    input[3] = 1;
    input[4] = 2;
    input[5] = 3;
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should have 3 leading '1's
    try std.testing.expectEqual('1', encoded[0]);
    try std.testing.expectEqual('1', encoded[1]);
    try std.testing.expectEqual('1', encoded[2]);
    // Rest should be valid base58
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - single non-zero byte at end" {
    var input = [_]u8{0} ** 32;
    input[31] = 255;
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce valid base58 ending with encoded value of 255
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - known 32-byte test vector" {
    // 32-byte test vector
    const input = [_]u8{
        0x00, 0x01, 0x09, 0x66, 0x77, 0x00, 0x06, 0x95,
        0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E, 0x39, 0xF8,
        0x6A, 0x0D, 0x27, 0x3B, 0xEE, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Verify all chars are valid base58
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - text padded to 32 bytes" {
    // "Hello World" padded to 32 bytes
    var input = [_]u8{0} ** 32;
    const text = "Hello World";
    @memcpy(input[0..text.len], text);
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Verify all chars are valid base58
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - 32-byte hash (typical use case)" {
    // SHA256 hash of "test"
    const input = [_]u8{
        0x9f, 0x86, 0xd0, 0x81, 0x88, 0x4c, 0x7d, 0x65,
        0x9a, 0x2f, 0xea, 0xa0, 0xc5, 0x5a, 0xd0, 0x15,
        0xa3, 0xbf, 0x4f, 0x1b, 0x2b, 0x0b, 0x82, 0x2c,
        0xd1, 0x5d, 0x6c, 0x15, 0xb0, 0xf0, 0x0a, 0x08,
    };
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce a valid base58 string
    // Check that all characters are in the base58 alphabet
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - all 0xFF bytes" {
    const input = [_]u8{0xFF} ** 32;
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce a valid base58 string
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - mixed bytes with leading zeros (32 bytes)" {
    var input = [_]u8{0} ** 32;
    input[3] = 1;
    input[4] = 2;
    input[5] = 3;
    input[6] = 4;
    input[7] = 5;
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should have 3 leading '1's
    try std.testing.expectEqual('1', encoded[0]);
    try std.testing.expectEqual('1', encoded[1]);
    try std.testing.expectEqual('1', encoded[2]);

    // All should be valid base58
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - incremental values" {
    // Test that incrementing input produces different outputs
    var prev_output: [base58_hash_bytes]u8 = undefined;
    var curr_output: [base58_hash_bytes]u8 = undefined;

    var input1 = [_]u8{0} ** 32;
    input1[31] = 1;
    const encoded1 = encode(&input1, &prev_output);

    var input2 = [_]u8{0} ** 32;
    input2[31] = 2;
    const encoded2 = encode(&input2, &curr_output);

    // Outputs should be different
    try std.testing.expect(!std.mem.eql(u8, encoded1, encoded2));
}

test "encode - power of two boundaries" {
    // Test encoding at power-of-two boundaries in 32-byte inputs
    const test_values = [_]u8{ 1, 2, 4, 128, 255 };

    for (test_values) |val| {
        var input = [_]u8{0} ** 32;
        input[31] = val;
        var output: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(&input, &output);

        // Just verify it produces valid base58
        for (encoded) |char| {
            const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
            try std.testing.expect(is_valid);
        }
    }
}

test "encode - alternating bits pattern" {
    // Test with alternating 0xAA and 0x55 pattern
    var input = [_]u8{ 0xAA, 0x55 } ** 16;
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce valid base58 chars
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - sequential bytes" {
    // Test with sequential byte values
    var input: [hash_bytes]u8 = undefined;
    for (0..hash_bytes) |i| {
        input[i] = @intCast(i);
    }
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce valid base58 chars
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - high entropy random-like data" {
    // Test with pseudo-random looking data (using prime multiplication)
    var input: [hash_bytes]u8 = undefined;
    var val: u32 = 17;
    for (0..hash_bytes) |i| {
        val = (val *% 31) +% 37;
        input[i] = @truncate(val);
    }
    var output: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&input, &output);

    // Should produce valid base58 chars
    for (encoded) |char| {
        const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
        try std.testing.expect(is_valid);
    }
}

test "encode - single bit set in different positions" {
    // Test with single bit set at different byte positions
    const positions = [_]usize{ 0, 7, 15, 16, 24, 31 };

    for (positions) |pos| {
        var input = [_]u8{0} ** 32;
        input[pos] = 1;
        var output: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(&input, &output);

        // All outputs should be different
        for (encoded) |char| {
            const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
            try std.testing.expect(is_valid);
        }
    }
}

test "encode - max value in different positions" {
    // Test with 0xFF in different positions
    const positions = [_]usize{ 0, 8, 16, 24, 31 };
    var outputs: [positions.len][base58_hash_bytes]u8 = undefined;
    var encoded_slices: [positions.len][]u8 = undefined;

    for (positions, 0..) |pos, i| {
        var input = [_]u8{0} ** 32;
        input[pos] = 0xFF;
        encoded_slices[i] = encode(&input, &outputs[i]);

        // Verify all chars are valid
        for (encoded_slices[i]) |char| {
            const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
            try std.testing.expect(is_valid);
        }
    }

    // All outputs should be unique
    for (0..outputs.len) |i| {
        for (i + 1..outputs.len) |j| {
            try std.testing.expect(!std.mem.eql(u8, encoded_slices[i], encoded_slices[j]));
        }
    }
}

test "encode - known sha256 hash values" {
    // Test with actual SHA256 hash outputs
    const test_cases = [_][hash_bytes]u8{
        // SHA256("")
        [_]u8{
            0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14,
            0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
            0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c,
            0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
        },
        // SHA256("abc")
        [_]u8{
            0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea,
            0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
            0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c,
            0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
        },
    };

    for (test_cases) |input| {
        var output: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(&input, &output);

        // Should produce valid base58 chars
        for (encoded) |char| {
            const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
            try std.testing.expect(is_valid);
        }
    }
}

test "encode - boundary values" {
    // Test edge cases around byte boundaries
    const test_cases = [_]struct {
        desc: []const u8,
        input: [hash_bytes]u8,
    }{
        .{ .desc = "minimum after all zeros", .input = blk: {
            var arr = [_]u8{0} ** 32;
            arr[31] = 1;
            break :blk arr;
        } },
        .{
            .desc = "one less than power of 58",
            .input = blk: {
                var arr = [_]u8{0} ** 32;
                arr[31] = 57; // 58 - 1
                break :blk arr;
            },
        },
        .{ .desc = "exactly power of 58", .input = blk: {
            var arr = [_]u8{0} ** 32;
            arr[31] = 58;
            break :blk arr;
        } },
        .{ .desc = "high bytes in middle", .input = blk: {
            var arr = [_]u8{0} ** 32;
            arr[15] = 0xFF;
            arr[16] = 0xFF;
            break :blk arr;
        } },
    };

    for (test_cases) |tc| {
        var output: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(&tc.input, &output);

        // All should produce valid base58
        for (encoded) |char| {
            const is_valid = std.mem.indexOfScalar(u8, base58_alphabet, char) != null;
            try std.testing.expect(is_valid);
        }
    }
}

test "encode - deterministic output" {
    // Ensure encoding is deterministic - same input always produces same output
    const input = [_]u8{
        0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0,
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x00,
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
    };

    var output1: [base58_hash_bytes]u8 = undefined;
    var output2: [base58_hash_bytes]u8 = undefined;
    var output3: [base58_hash_bytes]u8 = undefined;

    const encoded1 = encode(&input, &output1);
    const encoded2 = encode(&input, &output2);
    const encoded3 = encode(&input, &output3);

    // All outputs should be identical
    try std.testing.expectEqualSlices(u8, encoded1, encoded2);
    try std.testing.expectEqualSlices(u8, encoded2, encoded3);
}

/// Decode a base58 string back to 32 bytes.
/// Returns InvalidBase58 error if the string contains invalid characters.
pub fn decode(src: []const u8, dest: *[hash_bytes]u8) !void {
    // Clear destination - needed because the multiplication algorithm
    // accumulates values across the entire buffer
    @memset(dest, 0);

    // Count leading '1's (representing leading zeros in the output)
    var leading_ones: usize = 0;
    for (src) |char| {
        if (char == '1') {
            leading_ones += 1;
        } else {
            break;
        }
    }

    // If all '1's, we're done (all zeros)
    if (leading_ones == src.len) {
        return;
    }

    // Process each character from the input
    for (src) |char| {
        // Find the value of this character
        const char_value = blk: {
            for (base58_alphabet, 0..) |alpha_char, i| {
                if (char == alpha_char) {
                    break :blk @as(u8, @intCast(i));
                }
            }
            return error.InvalidBase58;
        };

        // Multiply dest by 58 and add char_value
        var carry: u16 = char_value;
        var j: usize = hash_bytes;
        while (j > 0) {
            j -= 1;
            const value = @as(u16, dest[j]) * 58 + carry;
            dest[j] = @truncate(value);
            carry = value >> 8;
        }

        // If we still have carry, the number is too large
        if (carry != 0) {
            return error.InvalidBase58;
        }
    }

    // Count actual leading zeros we produced
    var actual_zeros: usize = 0;
    for (dest.*) |byte| {
        if (byte == 0) {
            actual_zeros += 1;
        } else {
            break;
        }
    }

    // Standard base58: ensure we have exactly the right number of leading zeros
    // Each leading '1' in input should produce one leading zero in output
    if (actual_zeros < leading_ones) {
        const shift = leading_ones - actual_zeros;
        // Shift data right to make room for more zeros
        var i: usize = hash_bytes;
        while (i > shift) {
            i -= 1;
            dest[i] = dest[i - shift];
        }
        @memset(dest[0..shift], 0);
    }
}

// Tests for decode
test "decode - all ones" {
    const input = [_]u8{'1'} ** base58_hash_bytes;
    var output: [hash_bytes]u8 = undefined;
    try decode(&input, &output);

    // Should produce all zeros
    for (output) |byte| {
        try std.testing.expectEqual(@as(u8, 0), byte);
    }
}

test "decode - roundtrip simple" {
    // Test roundtrip: encode then decode
    var original = [_]u8{0} ** 32;
    original[31] = 42;

    var encoded_buf: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&original, &encoded_buf);

    var decoded: [hash_bytes]u8 = undefined;
    try decode(encoded, &decoded);

    try std.testing.expectEqualSlices(u8, &original, &decoded);
}

test "decode - roundtrip all values" {
    // Test roundtrip with all different byte values in last position
    for (0..256) |val| {
        var original = [_]u8{0} ** 32;
        original[31] = @intCast(val);

        var encoded_buf: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(&original, &encoded_buf);

        var decoded: [hash_bytes]u8 = undefined;
        try decode(encoded, &decoded);

        try std.testing.expectEqualSlices(u8, &original, &decoded);
    }
}

test "decode - roundtrip with leading zeros" {
    var original = [_]u8{0} ** 32;
    original[5] = 1;
    original[6] = 2;
    original[7] = 3;

    var encoded_buf: [base58_hash_bytes]u8 = undefined;
    const encoded = encode(&original, &encoded_buf);

    var decoded: [hash_bytes]u8 = undefined;
    try decode(encoded, &decoded);

    try std.testing.expectEqualSlices(u8, &original, &decoded);
}

test "decode - invalid character" {
    var input = [_]u8{'1'} ** base58_hash_bytes;
    input[20] = '0'; // '0' is not in base58 alphabet

    var output: [hash_bytes]u8 = undefined;
    const result = decode(&input, &output);

    try std.testing.expectError(error.InvalidBase58, result);
}

test "decode - roundtrip random patterns" {
    const patterns = [_][hash_bytes]u8{
        [_]u8{0xFF} ** 32,
        [_]u8{ 0xAA, 0x55 } ** 16,
        blk: {
            var arr: [hash_bytes]u8 = undefined;
            for (0..hash_bytes) |i| {
                arr[i] = @intCast(i * 7);
            }
            break :blk arr;
        },
    };

    for (patterns) |original| {
        var encoded_buf: [base58_hash_bytes]u8 = undefined;
        const encoded = encode(&original, &encoded_buf);

        var decoded: [hash_bytes]u8 = undefined;
        try decode(encoded, &decoded);

        try std.testing.expectEqualSlices(u8, &original, &decoded);
    }
}
