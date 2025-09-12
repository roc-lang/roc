//! High-performance UTF-8 validation using simdutf algorithm with vectorized lookup tables.
//! Based on "Validating UTF-8 In Less Than One Instruction Per Byte" by Keiser & Lemire.

const std = @import("std");
const builtin = @import("builtin");

/// UTF-8 validation state for cross-chunk processing
pub const Utf8State = struct {
    /// Previous 16-byte input block for cross-chunk validation
    prev_input_block: @Vector(16, u8) = @as(@Vector(16, u8), @splat(0)),
    /// Incomplete sequence state from previous chunk
    prev_incomplete: @Vector(16, u8) = @as(@Vector(16, u8), @splat(0)),
    /// Accumulated error state
    error_vec: @Vector(16, u8) = @as(@Vector(16, u8), @splat(0)),
};

// Error flag bits for validation
const TOO_SHORT: u8 = 0x01; // Lead byte/ASCII followed by lead byte/ASCII
const TOO_LONG: u8 = 0x02; // ASCII followed by continuation
const OVERLONG_3: u8 = 0x04; // Overlong 3-byte sequence
const SURROGATE: u8 = 0x08; // UTF-16 surrogate
const OVERLONG_2: u8 = 0x10; // Overlong 2-byte sequence
const TWO_CONTS: u8 = 0x20; // Two continuations in a row
const TOO_LARGE: u8 = 0x40; // Value > U+10FFFF
const TOO_LARGE_1000: u8 = 0x80;
const OVERLONG_4: u8 = 0x40; // Reuse TOO_LARGE bit
const CARRY: u8 = TOO_SHORT | TOO_LONG | TWO_CONTS;

/// Platform-specific SIMD shuffle operation
pub inline fn shuffle(table: @Vector(16, u8), indices: @Vector(16, u8)) @Vector(16, u8) {
    if (builtin.cpu.arch == .aarch64) {
        // ARM NEON: use tbl instruction
        return asm volatile ("tbl.16b %[out], {%[table]}, %[indices]"
            : [out] "=w" (-> @Vector(16, u8)),
            : [table] "w" (table),
              [indices] "w" (indices),
        );
    } else if (builtin.cpu.arch == .x86_64) {
        // x86-64: use pshufb instruction
        var result = table;
        asm volatile ("pshufb %[indices], %[result]"
            : [result] "+x" (result),
            : [indices] "x" (indices),
        );
        return result;
    } else {
        // Fallback for other architectures
        var result: @Vector(16, u8) = undefined;
        inline for (0..16) |i| {
            const idx = indices[i] & 0x0F; // Mask to 0-15
            result[i] = table[idx];
        }
        return result;
    }
}

/// The three lookup tables for UTF-8 validation (from simdjson)
const lookup_tables = struct {
    /// Table 1: High nibble of first byte
    const byte_1_high = @as(@Vector(16, u8), .{
        // 0x0_ to 0x7_: ASCII range
        TOO_LONG,               TOO_LONG,  TOO_LONG,                           TOO_LONG,
        TOO_LONG,               TOO_LONG,  TOO_LONG,                           TOO_LONG,
        // 0x8_ to 0xB_: Continuation bytes
        TWO_CONTS,              TWO_CONTS, TWO_CONTS,                          TWO_CONTS,
        // 0xC_: 2-byte sequence starts
        TOO_SHORT | OVERLONG_2,
        // 0xD_: 2-byte sequence starts
        TOO_SHORT,
        // 0xE_: 3-byte sequence starts
        TOO_SHORT | OVERLONG_3 | SURROGATE,
        // 0xF_: 4-byte sequence starts
        TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4,
    });

    /// Table 2: Low nibble of first byte
    const byte_1_low = @as(@Vector(16, u8), .{
        // 0x_0 to 0x_7
        CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
        CARRY | OVERLONG_2,
        CARRY,
        CARRY,
        CARRY | TOO_LARGE,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        // 0x_8 to 0x_F
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
        CARRY | TOO_LARGE | TOO_LARGE_1000,
    });

    /// Table 3: High nibble of second byte
    const byte_2_high = @as(@Vector(16, u8), .{
        // 0x0_ to 0x7_: ASCII (not valid as continuation)
        TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
        TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
        // 0x8_ to 0xB_: Valid continuation bytes
        TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4, TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE,
        // 0xC_ to 0xF_: Lead bytes (not valid as continuation)
        TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
    });
};

/// Check for special UTF-8 validation cases using three lookup tables
inline fn check_special_cases(input: @Vector(16, u8), prev_input: @Vector(16, u8)) @Vector(16, u8) {
    // Prepare previous byte vector (shifted by one)
    var prev1: @Vector(16, u8) = undefined;
    prev1[0] = prev_input[15];
    inline for (1..16) |i| {
        prev1[i] = input[i - 1];
    }

    // Extract nibbles for lookups
    const high_nibbles = (prev1 >> @splat(4)) & @as(@Vector(16, u8), @splat(0x0F));
    const low_nibbles = prev1 & @as(@Vector(16, u8), @splat(0x0F));
    const next_high_nibbles = (input >> @splat(4)) & @as(@Vector(16, u8), @splat(0x0F));

    // Perform three vectorized lookups using shuffle
    const lookup1 = shuffle(lookup_tables.byte_1_high, high_nibbles);
    const lookup2 = shuffle(lookup_tables.byte_1_low, low_nibbles);
    const lookup3 = shuffle(lookup_tables.byte_2_high, next_high_nibbles);

    // Combine lookups with bitwise AND (branchless validation)
    return lookup1 & lookup2 & lookup3;
}

/// Check for incomplete UTF-8 sequences at chunk boundaries
inline fn check_incomplete_sequences(input: @Vector(16, u8)) @Vector(16, u8) {
    // Identify different lead byte types
    const lead2_ge = @intFromBool(input >= @as(@Vector(16, u8), @splat(0xC0)));
    const lead2_le = @intFromBool(input <= @as(@Vector(16, u8), @splat(0xDF)));
    const is_2byte_lead = lead2_ge & lead2_le;

    const lead3_ge = @intFromBool(input >= @as(@Vector(16, u8), @splat(0xE0)));
    const lead3_le = @intFromBool(input <= @as(@Vector(16, u8), @splat(0xEF)));
    const is_3byte_lead = lead3_ge & lead3_le;

    const lead4_ge = @intFromBool(input >= @as(@Vector(16, u8), @splat(0xF0)));
    const lead4_le = @intFromBool(input <= @as(@Vector(16, u8), @splat(0xF4)));
    const is_4byte_lead = lead4_ge & lead4_le;

    // Calculate expected continuation bytes for sequences at chunk end
    var incomplete = @as(@Vector(16, u8), @splat(0));

    // Check last 3 positions for incomplete sequences
    // Position 15: can start 2, 3, or 4-byte sequence
    incomplete[15] = @as(u8, is_4byte_lead[15]) * 3 +
        @as(u8, is_3byte_lead[15]) * 2 +
        @as(u8, is_2byte_lead[15]);

    // Position 14: can start 3 or 4-byte sequence
    incomplete[14] = @as(u8, is_4byte_lead[14]) * 2 +
        @as(u8, is_3byte_lead[14]);

    // Position 13: can only start 4-byte sequence
    incomplete[13] = @as(u8, is_4byte_lead[13]);

    return incomplete;
}

/// Main UTF-8 validation function using simdutf algorithm
pub fn validateUtf8ChunkStateful(input: @Vector(16, u8), state: *Utf8State) @Vector(16, bool) {
    // Step 1: Check special cases using three lookup tables
    var special_cases = check_special_cases(input, state.prev_input_block);

    // Step 2: Check for incomplete sequences from previous chunk
    const incomplete_current = check_incomplete_sequences(input);

    // Step 3: Handle continuation bytes for incomplete sequences from previous chunk
    const must_be_continuation = state.prev_incomplete;

    // Check which bytes are valid continuation bytes (0x80-0xBF)
    const cont_ge = @intFromBool(input >= @as(@Vector(16, u8), @splat(0x80)));
    const cont_le = @intFromBool(input <= @as(@Vector(16, u8), @splat(0xBF)));
    const is_continuation = cont_ge & cont_le;

    // Create mask for expected continuation positions and handle them
    var continuation_errors = @as(@Vector(16, u8), @splat(0));

    // For positions that should be continuations from previous chunk:
    // 1. Check if they are actually continuations
    // 2. Suppress TWO_CONTS and other CARRY errors for them

    // Handle incomplete sequences from position 15 of previous chunk
    if (must_be_continuation[15] > 0) {
        const num_expected = must_be_continuation[15];

        // Check first N bytes should be continuations
        inline for (0..3) |i| {
            if (i < num_expected) {
                if (is_continuation[i] == 0) {
                    // Not a continuation when it should be
                    continuation_errors[i] = TOO_SHORT;
                } else {
                    // Is a valid continuation for cross-chunk sequence
                    // Suppress CARRY errors (including TWO_CONTS)
                    special_cases[i] = special_cases[i] & ~@as(u8, CARRY);
                }
            }
        }
    }

    // Handle incomplete sequences from position 14 of previous chunk
    if (must_be_continuation[14] > 0) {
        const num_expected = must_be_continuation[14];
        const start_pos = 1; // These continuations start at position 1

        inline for (0..2) |i| {
            if (i < num_expected) {
                const pos = start_pos + i;
                if (is_continuation[pos] == 0) {
                    continuation_errors[pos] = TOO_SHORT;
                } else {
                    // Suppress CARRY errors for valid continuations
                    special_cases[pos] = special_cases[pos] & ~@as(u8, CARRY);
                }
            }
        }
    }

    // Handle incomplete sequences from position 13 of previous chunk
    if (must_be_continuation[13] > 0) {
        // Only 4-byte sequences can start at position 13
        const pos = 2; // Continuation at position 2
        if (is_continuation[pos] == 0) {
            continuation_errors[pos] = TOO_SHORT;
        } else {
            special_cases[pos] = special_cases[pos] & ~@as(u8, CARRY);
        }
    }

    // Step 4: Combine error conditions
    const errors = special_cases | continuation_errors | state.error_vec;

    // Step 5: Check for structural errors
    // ASCII bytes (< 0x80) and lead bytes must not have CARRY flag set
    const ascii_mask = @intFromBool(input < @as(@Vector(16, u8), @splat(0x80)));
    const lead_mask = @intFromBool(input >= @as(@Vector(16, u8), @splat(0xC0)));
    const ascii_or_lead = ascii_mask | lead_mask;

    // Check if CARRY flag is set where it shouldn't be
    const carry_errors = (errors & @as(@Vector(16, u8), @splat(CARRY))) & ascii_or_lead;

    // Step 6: Final error accumulation
    const total_errors = errors | carry_errors;

    // Update state for next chunk
    state.prev_input_block = input;
    state.prev_incomplete = incomplete_current;
    state.error_vec = @as(@Vector(16, u8), @splat(0)); // Reset error accumulator

    // Return boolean vector indicating errors
    return total_errors != @as(@Vector(16, u8), @splat(0));
}

/// Simplified validation wrapper for compatibility
pub fn validateUtf8Chunk(input: @Vector(16, u8), prev_block: @Vector(16, u8)) @Vector(16, bool) {
    var state = Utf8State{ .prev_input_block = prev_block };
    return validateUtf8ChunkStateful(input, &state);
}

/// Check for errors at end of input
pub fn checkEof(state: *const Utf8State) bool {
    // Any incomplete sequences at EOF are errors
    const has_incomplete = @reduce(.Or, state.prev_incomplete != @as(@Vector(16, u8), @splat(0)));
    const has_errors = @reduce(.Or, state.error_vec != @as(@Vector(16, u8), @splat(0)));
    return has_incomplete or has_errors;
}
