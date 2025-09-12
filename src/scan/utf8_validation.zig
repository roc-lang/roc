//! UTF-8 validation using existing SIMD vectors.

const std = @import("std");

/// UTF-8 validation state for cross-chunk processing
pub const Utf8State = struct {
    prev_input_block: @Vector(16, u8) = @as(@Vector(16, u8), @splat(0)),
};

/// Validates a 16-byte chunk of UTF-8 data using SIMD
pub inline fn validateUtf8ChunkStateful(input: @Vector(16, u8), state: *Utf8State) @Vector(16, bool) {
    var error_mask: @Vector(16, u8) = @splat(0);

    // Get previous byte
    var prev1: @Vector(16, u8) = undefined;
    inline for (0..16) |i| {
        if (i == 0) {
            prev1[0] = state.prev_input_block[15];
        } else {
            prev1[i] = input[i - 1];
        }
    }

    // 1. Check for bytes that are NEVER valid
    // 0xC0, 0xC1 (overlong 2-byte)
    error_mask = error_mask | @intFromBool(input == @as(@Vector(16, u8), @splat(0xC0)));
    error_mask = error_mask | @intFromBool(input == @as(@Vector(16, u8), @splat(0xC1)));

    // 0xF5-0xFF (too large)
    error_mask = error_mask | @intFromBool(input >= @as(@Vector(16, u8), @splat(0xF5)));

    // 2. Check for continuation bytes (0x80-0xBF) in invalid positions
    const cont_ge = @intFromBool(input >= @as(@Vector(16, u8), @splat(0x80)));
    const cont_le = @intFromBool(input <= @as(@Vector(16, u8), @splat(0xBF)));
    const is_cont = cont_ge & cont_le;

    // Continuation is invalid only at start of sequence (after ASCII)
    const prev_is_ascii = @intFromBool(prev1 < @as(@Vector(16, u8), @splat(0x80)));
    const invalid_cont_start = is_cont & prev_is_ascii;
    error_mask = error_mask | invalid_cont_start;

    // 3. Check for specific overlong/surrogate patterns

    // 0xE0 followed by < 0xA0 (overlong 3-byte)
    const is_e0_prev = @intFromBool(prev1 == @as(@Vector(16, u8), @splat(0xE0)));
    const lt_a0 = @intFromBool(input < @as(@Vector(16, u8), @splat(0xA0)));
    error_mask = error_mask | (is_e0_prev & lt_a0 & is_cont);

    // 0xED followed by >= 0xA0 (surrogate)
    const is_ed_prev = @intFromBool(prev1 == @as(@Vector(16, u8), @splat(0xED)));
    const ge_a0 = @intFromBool(input >= @as(@Vector(16, u8), @splat(0xA0)));
    error_mask = error_mask | (is_ed_prev & ge_a0 & is_cont);

    // 0xF0 followed by < 0x90 (overlong 4-byte)
    const is_f0_prev = @intFromBool(prev1 == @as(@Vector(16, u8), @splat(0xF0)));
    const lt_90 = @intFromBool(input < @as(@Vector(16, u8), @splat(0x90)));
    error_mask = error_mask | (is_f0_prev & lt_90 & is_cont);

    // 0xF4 followed by >= 0x90 (too large)
    const is_f4_prev = @intFromBool(prev1 == @as(@Vector(16, u8), @splat(0xF4)));
    const ge_90 = @intFromBool(input >= @as(@Vector(16, u8), @splat(0x90)));
    error_mask = error_mask | (is_f4_prev & ge_90 & is_cont);

    // 4. Check for truncated 2-byte sequences
    // 0xC2-0xDF not followed by continuation
    const lead2_ge = @intFromBool(prev1 >= @as(@Vector(16, u8), @splat(0xC2)));
    const lead2_le = @intFromBool(prev1 <= @as(@Vector(16, u8), @splat(0xDF)));
    const is_2byte_lead = lead2_ge & lead2_le;
    const not_cont_lt = @intFromBool(input < @as(@Vector(16, u8), @splat(0x80)));
    const not_cont_gt = @intFromBool(input > @as(@Vector(16, u8), @splat(0xBF)));
    const not_cont = not_cont_lt | not_cont_gt;
    error_mask = error_mask | (is_2byte_lead & not_cont);

    // 5. Check for isolated continuation at start of chunk
    // If first byte is continuation and previous chunk didn't end with a lead byte
    if (state.prev_input_block[15] < 0x80 or state.prev_input_block[15] > 0xF4) {
        if (input[0] >= 0x80 and input[0] <= 0xBF) {
            error_mask[0] = 1;
        }
    }

    // Save current block for next chunk
    state.prev_input_block = input;

    return error_mask != @as(@Vector(16, u8), @splat(0));
}

/// Simple validation wrapper
pub fn validateUtf8Chunk(input: @Vector(16, u8), prev_block: @Vector(16, u8)) @Vector(16, bool) {
    var state = Utf8State{ .prev_input_block = prev_block };
    return validateUtf8ChunkStateful(input, &state);
}
