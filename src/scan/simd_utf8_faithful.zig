const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

// Error flags - these are bits that get set for different error types
const TOO_SHORT: u8 = 1 << 0; // 0x01
const TOO_LONG: u8 = 1 << 1; // 0x02
const OVERLONG_3: u8 = 1 << 2; // 0x04
const SURROGATE: u8 = 1 << 4; // 0x10
const OVERLONG_2: u8 = 1 << 5; // 0x20
const TWO_CONTS: u8 = 1 << 7; // 0x80
const TOO_LARGE: u8 = 1 << 3; // 0x08
const TOO_LARGE_1000: u8 = 1 << 6; // 0x40
const OVERLONG_4: u8 = 1 << 6; // 0x40
const CARRY: u8 = TOO_SHORT | TOO_LONG | TWO_CONTS; // 0x83

// Lookup tables for validation
const byte_1_high_table = [16]u8{
    TOO_LONG,               TOO_LONG,  TOO_LONG,                           TOO_LONG,
    TOO_LONG,               TOO_LONG,  TOO_LONG,                           TOO_LONG,
    TWO_CONTS,              TWO_CONTS, TWO_CONTS,                          TWO_CONTS,
    TOO_SHORT | OVERLONG_2, TOO_SHORT, TOO_SHORT | OVERLONG_3 | SURROGATE, TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4,
};

const byte_1_low_table = [16]u8{
    CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
    CARRY | OVERLONG_2,
    CARRY,
    CARRY,
    CARRY | TOO_LARGE,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
    CARRY | TOO_LARGE | TOO_LARGE_1000,
};

const byte_2_high_table = [16]u8{
    TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
    TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
    TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4, TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE,
    TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
};

pub const Utf8State = struct {
    has_error: @Vector(16, u8),
    previous: Processed,
    prev_incomplete: @Vector(16, u8),
};

const Processed = struct {
    rawbytes: @Vector(16, u8),
    high_nibbles: @Vector(16, u8),
    carried_continuations: @Vector(16, u8),
};

pub fn prev1(input: @Vector(16, u8), prev_input: @Vector(16, u8)) @Vector(16, u8) {
    // Shift input right by 1 byte, filling with prev_input's last byte
    // Positive indices select from prev_input, negative from input (-1 = input[0], -2 = input[1], etc)
    const shifted = @shuffle(u8, prev_input, input, @Vector(16, i32){ 15, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15 });
    return shifted;
}

fn lookup_16(vec: @Vector(16, u8), table: [16]u8) @Vector(16, u8) {
    // Use the nibbles as indices to look up values from the table
    var result: @Vector(16, u8) = undefined;
    inline for (0..16) |i| {
        const idx = vec[i] & 0x0F;
        result[i] = table[idx];
    }
    return result;
}

// Maximum values for detecting incomplete sequences at end
const max_array = [32]u8{ 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0b11110000 - 1, 0b11100000 - 1, 0b11000000 - 1 };

pub fn isIncomplete(input: @Vector(16, u8)) @Vector(16, u8) {
    // Check if the last bytes indicate an incomplete multi-byte sequence
    const max_value: @Vector(16, u8) = max_array[16..32].*;
    const is_greater = input > max_value;
    return @select(u8, is_greater, @as(@Vector(16, u8), @splat(0xFF)), @as(@Vector(16, u8), @splat(0)));
}

fn checkSpecialCases(input: @Vector(16, u8), prev_bytes: @Vector(16, u8)) @Vector(16, u8) {
    // Extract nibbles
    const prev_high = prev_bytes >> @as(@Vector(16, u8), @splat(4));
    const prev_low = prev_bytes & @as(@Vector(16, u8), @splat(0x0F));
    const curr_high = input >> @as(@Vector(16, u8), @splat(4));

    // Lookup in tables
    const byte_1_high = lookup_16(prev_high, byte_1_high_table);
    const byte_1_low = lookup_16(prev_low, byte_1_low_table);
    const byte_2_high = lookup_16(curr_high, byte_2_high_table);

    // Combine checks
    return byte_1_high & byte_1_low & byte_2_high;
}

fn checkMultibyteLength(input: @Vector(16, u8), prev_input: @Vector(16, u8), sc: @Vector(16, u8)) @Vector(16, u8) {
    // Create shifted versions
    // Positive indices select from prev_input, negative from input (-1 = input[0], etc)
    const prev2 = @shuffle(u8, prev_input, input, @Vector(16, i32){ 14, 15, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14 });
    const prev3 = @shuffle(u8, prev_input, input, @Vector(16, i32){ 13, 14, 15, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13 });

    // Use saturating subtraction to detect 3-byte and 4-byte UTF-8 sequences
    // For 3-byte sequences (E0-EF), subtract 0xDF and check if result > 0
    // For 4-byte sequences (F0-F7), subtract 0xEF and check if result > 0
    const is_third_byte = prev2 -| @as(@Vector(16, u8), @splat(0xE0 - 1));
    const is_fourth_byte = prev3 -| @as(@Vector(16, u8), @splat(0xF0 - 1));

    // Combine and check if > 0 (which means the position must be a continuation)
    const must23_temp = is_third_byte | is_fourth_byte;
    const must23 = must23_temp > @as(@Vector(16, u8), @splat(0));

    // Get only the TWO_CONTS bit (0x80) for positions that should be continuations
    const must23_80 = @select(u8, must23, @as(@Vector(16, u8), @splat(0x80)), @as(@Vector(16, u8), @splat(0)));

    // XOR to cancel out TWO_CONTS for valid continuations
    return must23_80 ^ sc;
}

pub fn validateUtf8Chunk(input: @Vector(16, u8), state: *Utf8State) @Vector(16, u8) {
    const prev_bytes = prev1(input, state.previous.rawbytes);
    const sc = checkSpecialCases(input, prev_bytes);
    const errors = checkMultibyteLength(input, state.previous.rawbytes, sc);

    // Check for incomplete sequences from previous chunk
    const combined_errors = errors | state.prev_incomplete;

    // Update state
    state.previous.rawbytes = input;
    state.prev_incomplete = isIncomplete(input);
    state.has_error = state.has_error | combined_errors;

    return combined_errors;
}

pub fn initState() Utf8State {
    return .{
        .has_error = @as(@Vector(16, u8), @splat(0)),
        .previous = .{
            .rawbytes = @as(@Vector(16, u8), @splat(0)),
            .high_nibbles = @as(@Vector(16, u8), @splat(0)),
            .carried_continuations = @as(@Vector(16, u8), @splat(0)),
        },
        .prev_incomplete = @as(@Vector(16, u8), @splat(0)),
    };
}

/// Compatibility wrapper that returns a boolean vector for integration with Bitmasks.zig
pub fn validateUtf8ChunkStateful(input: @Vector(16, u8), state: *Utf8State) @Vector(16, bool) {
    const errors = validateUtf8Chunk(input, state);
    // Convert non-zero error bytes to true
    return errors != @as(@Vector(16, u8), @splat(0));
}

// Helper function for testing
fn validate(bytes: []const u8) bool {
    var state = initState();

    var i: usize = 0;
    while (i + 16 <= bytes.len) : (i += 16) {
        const chunk: @Vector(16, u8) = bytes[i..][0..16].*;
        _ = validateUtf8Chunk(chunk, &state);
    }

    // Handle remaining bytes
    if (i < bytes.len) {
        var padded = [_]u8{0} ** 16;
        @memcpy(padded[0 .. bytes.len - i], bytes[i..]);
        const chunk: @Vector(16, u8) = padded;
        _ = validateUtf8Chunk(chunk, &state);
    }

    // Check if any errors were found
    const error_mask = @reduce(.Or, state.has_error);
    return error_mask == 0;
}

// Tests
test "valid ASCII" {
    try testing.expect(validate("Hello, World!"));
}

test "valid multibyte UTF-8" {
    try testing.expect(validate("Hello €!")); // € = 0xE2 0x82 0xAC
}

test "valid 4-byte UTF-8" {
    try testing.expect(validate("𝄞")); // U+1D11E
}

test "invalid orphan continuation" {
    try testing.expect(!validate(&[_]u8{ 'H', 'i', 0x80, '!' }));
}

test "invalid truncated" {
    try testing.expect(!validate(&[_]u8{ 'H', 'i', 0xE2, 0x82 })); // Missing last byte of €
}

test "invalid overlong" {
    try testing.expect(!validate(&[_]u8{ 0xC0, 0x80 })); // Overlong encoding of U+0000
}

test "invalid surrogate" {
    try testing.expect(!validate(&[_]u8{ 0xED, 0xA0, 0x80 })); // U+D800 (surrogate)
}
