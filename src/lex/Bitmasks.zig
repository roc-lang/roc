//! 64-bit bitmasks for a page of source code bytes. (Each page is up to 64B.)

const std = @import("std");
const builtin = @import("builtin");

pub const Self = @This();
pub const page_size: usize = 64;

/// How many bytes within the 64B page we've already processed.
/// NOTE: we make the *masks* do the padding, by giving them a set of pointers to load their 16B at a time from,
/// and then giving them different pointers if we're on the last page (pointers which point to 16B of newlines).
/// This way we never need to actually load 64B into memory!
bytes_chomped: usize = 0,

/// The 64-bit masks, with each bit representing a byte in a 64B page.
masks: [std.meta.fields(Bitmask).len]u64 = .{0} ** std.meta.fields(Bitmask).len,

pub fn init(src_bytes: []align(16) u8) Self {
    if (src_bytes.len >= page_size) {
        return initExact(src_bytes);
    } else {
        return initPartialPage(src_bytes);
    }
}

/// Only call when there are enough bytes in here that we can just load everything.
fn initExact(src_bytes: []align(16) u8) Self {
    std.debug.assert(src_bytes.len >= page_size);

    var dollar_mask: u64 = 0;
    var brace_mask: u64 = 0;
    var newline_mask: u64 = 0;
    var quote_mask: u64 = 0;
    var backslash_mask: u64 = 0;
    var num_or_ident_mask: u64 = 0;
    var non_whitespace_mask: u64 = 0;

    // Lookup tables for identifier classification
    // These encode which characters are identifiers based on their nibbles
    const high_table = [_]u8{ 0, 0, 0, 1, 2, 4, 8, 16, 32, 32, 32, 32, 32, 32, 32, 32 };
    const low_table = [_]u8{ 53, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 42, 42, 42, 42, 46 };

    // Branchlessly get a mask where the last bit is set iff the byte at the start of the page is '{'.
    // The purpose of this is to handle the edge case where the last byte of this page and the first
    // byte of the next page is '{', and we wouldn't otherwise register this as the start of a string
    // interpolation. This lets us branchlessly do that, while avoiding out-of-bounds reads by pointing
    // to a known zero (so that it will fail the `== '{'` check) if there is no next page.
    const first_in_next_page_ptr = src_bytes.ptr + page_size;
    const first_in_next_page = if (src_bytes.len > page_size) first_in_next_page_ptr[0] else 0;
    const last_byte_starts_interpolation: u64 = @as(u64, @intFromBool(first_in_next_page == '{')) << 63;

    // Use 128-bit SIMD because it's supported on all the targets we build for, including wasm32.
    inline for (0..page_size / 16) |ix| {
        const offset = ix * 16;
        const chunk_ptr: *align(16) const [16]u8 = @ptrCast(src_bytes[offset..].ptr);

        // Load 16 bytes as a vector
        const chunk_vec: @Vector(16, u8) = chunk_ptr.*;

        // Create vectors filled with the target bytes
        const dollar_vec: @Vector(16, u8) = @splat('$');
        const brace_vec: @Vector(16, u8) = @splat('{');
        const newline_vec: @Vector(16, u8) = @splat('\n');
        const quote_vec: @Vector(16, u8) = @splat('"');
        const backslash_vec: @Vector(16, u8) = @splat('\\');

        // Compare each byte in chunk with target bytes (produces 0xff where equal, 0x00 otherwise)
        const dollar_cmp = chunk_vec == dollar_vec;
        const brace_cmp = chunk_vec == brace_vec;
        const newline_cmp = chunk_vec == newline_vec;
        const quote_cmp = chunk_vec == quote_vec;
        const backslash_cmp = chunk_vec == backslash_vec;

        // Convert comparison results to bitmasks
        // Each true comparison produces a bit in the result
        const dollar_bits = vectorToBitmask(dollar_cmp);
        const brace_bits = vectorToBitmask(brace_cmp);
        const newline_bits = vectorToBitmask(newline_cmp);
        const quote_bits = vectorToBitmask(quote_cmp);
        const backslash_bits = vectorToBitmask(backslash_cmp);

        // Shift and OR the 16-bit masks into the appropriate position in the 64-bit masks
        dollar_mask |= @as(u64, dollar_bits) << @intCast(offset);
        brace_mask |= @as(u64, brace_bits) << @intCast(offset);
        newline_mask |= @as(u64, newline_bits) << @intCast(offset);
        quote_mask |= @as(u64, quote_bits) << @intCast(offset);
        backslash_mask |= @as(u64, backslash_bits) << @intCast(offset);

        // Compute identifier mask using nibble-based classification
        // This identifies ASCII alphanumeric, underscore, and non-ASCII bytes
        const low_nibble_mask: @Vector(16, u8) = @splat(0x0F);
        const low_nibbles = chunk_vec & low_nibble_mask;
        const high_nibbles = chunk_vec >> @as(@Vector(16, u8), @splat(4));

        // Perform parallel lookups using shuffle operations
        // Use vectorized lookups instead of shuffle for runtime indices
        var high_result: @Vector(16, u8) = @splat(0);
        var low_result: @Vector(16, u8) = @splat(0);

        inline for (0..16) |i| {
            const idx_vec: @Vector(16, u8) = @splat(@as(u8, i));
            const high_mask = high_nibbles == idx_vec;
            const low_mask = low_nibbles == idx_vec;

            high_result = @select(u8, high_mask, @as(@Vector(16, u8), @splat(high_table[i])), high_result);
            low_result = @select(u8, low_mask, @as(@Vector(16, u8), @splat(low_table[i])), low_result);
        }

        // AND the results - non-zero only if both lookups agree
        const combined_result = high_result & low_result;

        // Check which bytes are non-zero (i.e., are identifiers)
        const zeros: @Vector(16, u8) = @splat(0);
        const is_identifier = combined_result > zeros;

        // Convert to bitmask and add to num_or_ident_mask
        const ident_bits = vectorToBitmask(is_identifier);
        num_or_ident_mask |= @as(u64, ident_bits) << @intCast(offset);

        // Compute non-whitespace mask (we already have the newline mask computed from earlier)
        const space_vec: @Vector(16, u8) = @splat(' ');
        const tab_vec: @Vector(16, u8) = @splat('\t');
        const cr_vec: @Vector(16, u8) = @splat('\r');

        const is_space = chunk_vec == space_vec;
        const is_tab = chunk_vec == tab_vec;
        const is_cr = chunk_vec == cr_vec;

        // Combine whitespace checks - convert to u8 vectors for bitwise OR
        const space_u8 = @intFromBool(is_space);
        const tab_u8 = @intFromBool(is_tab);
        const cr_u8 = @intFromBool(is_cr);
        const newline_u8 = @intFromBool(newline_cmp);
        const whitespace_u8 = space_u8 | tab_u8 | cr_u8 | newline_u8;
        // Invert for non-whitespace
        const non_ws = whitespace_u8 == @as(@Vector(16, u8), @splat(0));
        const non_ws_bits = vectorToBitmask(non_ws);
        non_whitespace_mask |= @as(u64, non_ws_bits) << @intCast(offset);
    }

    const multi_str_seg_ends =
        (dollar_mask & (brace_mask >> 1)) | last_byte_starts_interpolation | newline_mask | backslash_mask;
    const single_str_seg_ends = multi_str_seg_ends | quote_mask; // Quotes end single-line strings but not multiline

    return Self{
        .masks = .{
            single_str_seg_ends,
            multi_str_seg_ends,
            newline_mask,
            non_whitespace_mask,
            num_or_ident_mask,
        },
    };
}

// Convert a vector comparison result to a bitmask
// x86_64 can do this in 1 instruction, and ARM NEON can do slightly better
// than the fallback. (The fallback is basically for wasm32.)
fn vectorToBitmask(vec: @Vector(16, bool)) u16 {
    switch (builtin.cpu.arch) {
        .aarch64 => return @import("aarch64.zig").vectorToBitmask(vec),
        .x86_64 => return @import("x86_64.zig").vectorToBitmask(vec),
        else => return @import("fallback.zig").vectorToBitmask(vec),
    }
}

fn initPartialPage(src_bytes: []align(16) u8) Self {
    // For partial pages (< 64 bytes), pad with spaces and process normally
    var padded_bytes: [page_size]u8 align(16) = [_]u8{' '} ** page_size;
    @memcpy(padded_bytes[0..src_bytes.len], src_bytes);

    return initExact(&padded_bytes);
}

/// Load a new page of bytes and compute bitmasks
/// This is called for subsequent pages after the initial one
pub fn load(self: *Self, src_bytes: []align(16) u8) void {
    const actual_len = @min(src_bytes.len, page_size);

    var dollar_mask: u64 = 0;
    var brace_mask: u64 = 0;
    var newline_mask: u64 = 0;
    var quote_mask: u64 = 0;
    var backslash_mask: u64 = 0;
    var num_or_ident_mask: u64 = 0;
    var non_whitespace_mask: u64 = 0;

    // Lookup tables for identifier classification
    const high_table = [_]u8{ 0, 0, 0, 1, 2, 4, 8, 16, 32, 32, 32, 32, 32, 32, 32, 32 };
    const low_table = [_]u8{ 53, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 42, 42, 42, 42, 46 };
    // Lookup tables are used directly in the inline for loops

    // Handle edge case for string interpolation across page boundary
    const first_in_next_page_ptr = src_bytes.ptr + actual_len;
    const first_in_next_page = if (src_bytes.len > actual_len) first_in_next_page_ptr[0] else 0;
    const last_byte_starts_interpolation: u64 = @as(u64, @intFromBool(first_in_next_page == '{')) << 63;

    // Process in 16-byte chunks
    var chunks_to_process = actual_len / 16;
    if (actual_len % 16 != 0) chunks_to_process += 1;

    inline for (0..4) |ix| {
        if (ix >= chunks_to_process) break;

        const offset = ix * 16;
        const chunk_ptr: *align(16) const [16]u8 = @ptrCast(src_bytes[offset..].ptr);
        const chunk_vec: @Vector(16, u8) = chunk_ptr.*;

        // Character detection (same as in initExact)
        const dollar_vec: @Vector(16, u8) = @splat('$');
        const brace_vec: @Vector(16, u8) = @splat('{');
        const newline_vec: @Vector(16, u8) = @splat('\n');
        const quote_vec: @Vector(16, u8) = @splat('"');
        const backslash_vec: @Vector(16, u8) = @splat('\\');

        const dollar_cmp = chunk_vec == dollar_vec;
        const brace_cmp = chunk_vec == brace_vec;
        const newline_cmp = chunk_vec == newline_vec;
        const quote_cmp = chunk_vec == quote_vec;
        const backslash_cmp = chunk_vec == backslash_vec;

        const dollar_bits = vectorToBitmask(dollar_cmp);
        const brace_bits = vectorToBitmask(brace_cmp);
        const newline_bits = vectorToBitmask(newline_cmp);
        const quote_bits = vectorToBitmask(quote_cmp);
        const backslash_bits = vectorToBitmask(backslash_cmp);

        dollar_mask |= @as(u64, dollar_bits) << @intCast(offset);
        brace_mask |= @as(u64, brace_bits) << @intCast(offset);
        newline_mask |= @as(u64, newline_bits) << @intCast(offset);
        quote_mask |= @as(u64, quote_bits) << @intCast(offset);
        backslash_mask |= @as(u64, backslash_bits) << @intCast(offset);

        // Identifier detection
        const low_nibble_mask: @Vector(16, u8) = @splat(0x0F);
        const low_nibbles = chunk_vec & low_nibble_mask;
        const high_nibbles = chunk_vec >> @as(@Vector(16, u8), @splat(4));

        // Use vectorized lookups instead of shuffle for runtime indices
        var high_result: @Vector(16, u8) = @splat(0);
        var low_result: @Vector(16, u8) = @splat(0);

        inline for (0..16) |j| {
            const idx_vec: @Vector(16, u8) = @splat(@as(u8, j));
            const high_mask = high_nibbles == idx_vec;
            const low_mask = low_nibbles == idx_vec;

            high_result = @select(u8, high_mask, @as(@Vector(16, u8), @splat(high_table[j])), high_result);
            low_result = @select(u8, low_mask, @as(@Vector(16, u8), @splat(low_table[j])), low_result);
        }
        const combined_result = high_result & low_result;

        const zeros: @Vector(16, u8) = @splat(0);
        const is_identifier = combined_result > zeros;
        const ident_bits = vectorToBitmask(is_identifier);
        num_or_ident_mask |= @as(u64, ident_bits) << @intCast(offset);

        // Non-whitespace detection
        const space_vec: @Vector(16, u8) = @splat(' ');
        const tab_vec: @Vector(16, u8) = @splat('\t');
        const cr_vec: @Vector(16, u8) = @splat('\r');

        const is_space = chunk_vec == space_vec;
        const is_tab = chunk_vec == tab_vec;
        const is_cr = chunk_vec == cr_vec;

        // Combine whitespace checks - convert to u8 vectors for bitwise OR
        const space_u8 = @intFromBool(is_space);
        const tab_u8 = @intFromBool(is_tab);
        const cr_u8 = @intFromBool(is_cr);
        const newline_u8 = @intFromBool(newline_cmp);
        const whitespace_u8 = space_u8 | tab_u8 | cr_u8 | newline_u8;
        // Invert for non-whitespace
        const non_ws = whitespace_u8 == @as(@Vector(16, u8), @splat(0));
        const non_ws_bits = vectorToBitmask(non_ws);
        non_whitespace_mask |= @as(u64, non_ws_bits) << @intCast(offset);
    }

    // Compute string segment end masks
    const multi_str_seg_ends =
        (dollar_mask & (brace_mask >> 1)) | last_byte_starts_interpolation | newline_mask | backslash_mask;
    const single_str_seg_ends = multi_str_seg_ends | quote_mask;

    // Update masks
    self.masks = .{
        single_str_seg_ends,
        multi_str_seg_ends,
        newline_mask,
        non_whitespace_mask,
        num_or_ident_mask,
    };

    // Reset byte counter for new page
    self.bytes_chomped = 0;
}

/// We use SIMD to make a bitmask of these, 16B at a time.
/// So for example, using one 128-bit SIMD instruction,
/// we can make a 16-bit bitmask of where all the newlines are.
/// This in turn creates a u64 bitmask representing where all
/// the newlines are in one 64-*byte* page. From there we can use
/// cheap u64 bit shifts to figure out where relevant byte indices are.
///
/// Crucially, none of this will cause any cache misses or branch mispredictions.
/// That doesn't make it *free*, but it makes it shockingly cheap in practice.
pub const Bitmask = enum {
    single_str_seg_ends, // "${" or newline or backslash or double quote
    multi_str_seg_ends, // "${" or newline or backslash (no double quote)
    newlines, // "\n" - for end of comments
    non_ws, // anything other than [" ", "\t", "\r", "\n"]
    num_or_ident, // '0'..'9', 'A'..'Z', 'a'..'z', '_', and UTF-8 multibyte sequences
};

// Tests

test "init with small input" {
    const allocator = std.testing.allocator;
    const small_input = try allocator.alignedAlloc(u8, 16, 32);
    defer allocator.free(small_input);

    // Fill with test data
    for (small_input) |*byte| {
        byte.* = ' '; // Initialize all with spaces
    }
    // Place specific test patterns
    small_input[0] = '"'; // Quote
    small_input[1] = 'h';
    small_input[2] = 'i';
    small_input[3] = '\n'; // Newline
    small_input[4] = '$'; // Dollar
    small_input[5] = '{'; // Brace (should trigger ${)
    small_input[6] = '\\'; // Backslash

    const bitmasks = init(small_input[0..32]);

    // Check that newline is detected at position 3
    const newline_mask = bitmasks.masks[@intFromEnum(Bitmask.newlines)];
    try std.testing.expect((newline_mask >> 3) & 1 == 1);

    // Check that ${ is detected (dollar at position 4)
    const multi_ends = bitmasks.masks[@intFromEnum(Bitmask.multi_str_seg_ends)];
    try std.testing.expect((multi_ends >> 4) & 1 == 1);

    // Check that backslash is detected at position 6
    try std.testing.expect((multi_ends >> 6) & 1 == 1);

    // Check that quote is in single_str_seg_ends
    const single_ends = bitmasks.masks[@intFromEnum(Bitmask.single_str_seg_ends)];
    try std.testing.expect((single_ends >> 0) & 1 == 1);
}

test "init with exact page size" {
    const allocator = std.testing.allocator;
    const exact_input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(exact_input);

    // Fill with test data
    for (exact_input, 0..) |*byte, i| {
        if (i % 10 == 0) {
            byte.* = '\n';
        } else if (i % 15 == 0) {
            byte.* = '"';
        } else {
            byte.* = 'a';
        }
    }

    const bitmasks = init(exact_input);

    // Verify newlines are detected at positions 0, 10, 20, 30, 40, 50, 60
    const newline_mask = bitmasks.masks[@intFromEnum(Bitmask.newlines)];
    try std.testing.expect((newline_mask >> 0) & 1 == 1);
    try std.testing.expect((newline_mask >> 10) & 1 == 1);
    try std.testing.expect((newline_mask >> 20) & 1 == 1);
    try std.testing.expect((newline_mask >> 30) & 1 == 1);
    try std.testing.expect((newline_mask >> 40) & 1 == 1);
    try std.testing.expect((newline_mask >> 50) & 1 == 1);
    try std.testing.expect((newline_mask >> 60) & 1 == 1);

    // Verify quotes are detected at positions 0, 15, 30, 45, 60
    const single_str_ends = bitmasks.masks[@intFromEnum(Bitmask.single_str_seg_ends)];
    try std.testing.expect((single_str_ends >> 15) & 1 == 1);
    try std.testing.expect((single_str_ends >> 30) & 1 == 1);
    try std.testing.expect((single_str_ends >> 45) & 1 == 1);
    try std.testing.expect((single_str_ends >> 60) & 1 == 1);
}

test "vectorToBitmask" {
    // Test with all false
    const all_false: @Vector(16, bool) = @splat(false);
    try std.testing.expectEqual(@as(u16, 0), vectorToBitmask(all_false));

    // Test with all true
    const all_true: @Vector(16, bool) = @splat(true);
    try std.testing.expectEqual(@as(u16, 0xFFFF), vectorToBitmask(all_true));

    // Test with specific pattern
    var pattern: @Vector(16, bool) = @splat(false);
    pattern[0] = true;
    pattern[3] = true;
    pattern[7] = true;
    pattern[15] = true;
    const expected: u16 = (1 << 0) | (1 << 3) | (1 << 7) | (1 << 15);
    try std.testing.expectEqual(expected, vectorToBitmask(pattern));
}

test "identifier detection" {
    const allocator = std.testing.allocator;
    const input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(input);

    // Fill with various identifier and non-identifier characters
    @memcpy(input[0..25], "abc123_XYZ !@#$%^&*()+=[]");
    // Pad the rest with spaces
    for (input[26..]) |*byte| {
        byte.* = ' ';
    }

    const bitmasks = init(input);

    const ident_mask = bitmasks.masks[@intFromEnum(Bitmask.num_or_ident)];

    // Check that letters, numbers, and underscore are detected
    try std.testing.expect((ident_mask >> 0) & 1 == 1); // 'a'
    try std.testing.expect((ident_mask >> 1) & 1 == 1); // 'b'
    try std.testing.expect((ident_mask >> 2) & 1 == 1); // 'c'
    try std.testing.expect((ident_mask >> 3) & 1 == 1); // '1'
    try std.testing.expect((ident_mask >> 4) & 1 == 1); // '2'
    try std.testing.expect((ident_mask >> 5) & 1 == 1); // '3'
    try std.testing.expect((ident_mask >> 6) & 1 == 1); // '_'
    try std.testing.expect((ident_mask >> 7) & 1 == 1); // 'X'
    try std.testing.expect((ident_mask >> 8) & 1 == 1); // 'Y'
    try std.testing.expect((ident_mask >> 9) & 1 == 1); // 'Z'

    // Check that special characters are not detected as identifiers
    try std.testing.expect((ident_mask >> 11) & 1 == 0); // '!'
    try std.testing.expect((ident_mask >> 12) & 1 == 0); // '@'
    try std.testing.expect((ident_mask >> 13) & 1 == 0); // '#'
}

test "whitespace detection" {
    const allocator = std.testing.allocator;
    const input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(input);

    // Mix of whitespace and non-whitespace
    @memcpy(input[0..16], "a b\tc\rd\ne       ");
    // Pad the rest
    for (input[16..]) |*byte| {
        byte.* = ' ';
    }

    const bitmasks = init(input);

    const non_ws_mask = bitmasks.masks[@intFromEnum(Bitmask.non_ws)];

    // Check non-whitespace characters are detected
    try std.testing.expect((non_ws_mask >> 0) & 1 == 1); // 'a'
    try std.testing.expect((non_ws_mask >> 2) & 1 == 1); // 'b'
    try std.testing.expect((non_ws_mask >> 4) & 1 == 1); // 'c'
    try std.testing.expect((non_ws_mask >> 6) & 1 == 1); // 'd'
    try std.testing.expect((non_ws_mask >> 8) & 1 == 1); // 'e'

    // Check whitespace characters are not in non_ws mask
    try std.testing.expect((non_ws_mask >> 1) & 1 == 0); // ' '
    try std.testing.expect((non_ws_mask >> 3) & 1 == 0); // '\t'
    try std.testing.expect((non_ws_mask >> 5) & 1 == 0); // '\r'
    try std.testing.expect((non_ws_mask >> 7) & 1 == 0); // '\n'
}

test "string interpolation detection" {
    const allocator = std.testing.allocator;
    const input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(input);

    // Initialize all bytes to spaces
    for (input) |*byte| {
        byte.* = ' ';
    }

    // Test string with interpolation
    input[0] = '"';
    input[1] = 'h';
    input[2] = 'e';
    input[3] = 'l';
    input[4] = 'l';
    input[5] = 'o';
    input[6] = ' ';
    input[7] = '$'; // Dollar at position 7
    input[8] = '{'; // Brace at position 8 (forms ${)
    input[9] = 'n';
    input[10] = 'a';
    input[11] = 'm';
    input[12] = 'e';
    input[13] = '}';
    input[14] = '"';

    const bitmasks = init(input);

    const multi_ends = bitmasks.masks[@intFromEnum(Bitmask.multi_str_seg_ends)];
    const single_ends = bitmasks.masks[@intFromEnum(Bitmask.single_str_seg_ends)];

    // Check that quote is in single_str_seg_ends
    try std.testing.expect((single_ends >> 0) & 1 == 1); // '"' at position 0

    // Check that ${ is detected (dollar at position 7)
    try std.testing.expect((multi_ends >> 7) & 1 == 1); // '$' followed by '{'
}

test "escape sequence detection" {
    const allocator = std.testing.allocator;
    const input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(input);

    // Test with backslashes
    @memcpy(input[0..10], "a\\nb\\tc\\\\d");
    // Pad the rest
    for (input[10..]) |*byte| {
        byte.* = ' ';
    }

    const bitmasks = init(input);

    const multi_ends = bitmasks.masks[@intFromEnum(Bitmask.multi_str_seg_ends)];
    const single_ends = bitmasks.masks[@intFromEnum(Bitmask.single_str_seg_ends)];

    // Check that backslashes are detected
    try std.testing.expect((multi_ends >> 1) & 1 == 1); // '\\' at position 1
    try std.testing.expect((multi_ends >> 4) & 1 == 1); // '\\' at position 4
    try std.testing.expect((multi_ends >> 7) & 1 == 1); // '\\' at position 7
    try std.testing.expect((multi_ends >> 8) & 1 == 1); // '\\' at position 8

    // Backslashes should also be in single_str_seg_ends
    try std.testing.expect((single_ends >> 1) & 1 == 1);
    try std.testing.expect((single_ends >> 4) & 1 == 1);
    try std.testing.expect((single_ends >> 7) & 1 == 1);
    try std.testing.expect((single_ends >> 8) & 1 == 1);
}

test "UTF-8 multibyte detection" {
    const allocator = std.testing.allocator;
    const input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(input);

    // Test with UTF-8 multibyte sequences
    // "café" = 'c' 'a' 'f' 0xC3 0xA9 (é in UTF-8)
    input[0] = 'c';
    input[1] = 'a';
    input[2] = 'f';
    input[3] = 0xC3; // First byte of é
    input[4] = 0xA9; // Second byte of é

    // Pad the rest
    for (input[5..]) |*byte| {
        byte.* = ' ';
    }

    const bitmasks = init(input);

    const ident_mask = bitmasks.masks[@intFromEnum(Bitmask.num_or_ident)];

    // ASCII letters should be detected as identifiers
    try std.testing.expect((ident_mask >> 0) & 1 == 1); // 'c'
    try std.testing.expect((ident_mask >> 1) & 1 == 1); // 'a'
    try std.testing.expect((ident_mask >> 2) & 1 == 1); // 'f'

    // UTF-8 multibyte sequences should be detected as identifiers
    try std.testing.expect((ident_mask >> 3) & 1 == 1); // 0xC3
    try std.testing.expect((ident_mask >> 4) & 1 == 1); // 0xA9
}

test "load function with new pages" {
    const allocator = std.testing.allocator;
    const input = try allocator.alignedAlloc(u8, 16, page_size);
    defer allocator.free(input);

    // Initialize with first page
    @memcpy(input[0..10], "first page");
    for (input[10..]) |*byte| {
        byte.* = ' ';
    }

    var bitmasks = init(input);

    // Load a second page
    @memcpy(input[0..11], "second\npage");
    for (input[11..]) |*byte| {
        byte.* = ' ';
    }

    bitmasks.load(input);

    // Check that newline in second page is detected
    const newline_mask = bitmasks.masks[@intFromEnum(Bitmask.newlines)];
    try std.testing.expect((newline_mask >> 6) & 1 == 1); // '\n' at position 6
}

test "edge case: page boundary interpolation" {
    const allocator = std.testing.allocator;

    // Test case where '$' is at the end of a page and '{' starts the next page
    const input1 = try allocator.alignedAlloc(u8, 16, page_size + 1);
    defer allocator.free(input1);

    // Fill most of the page with 'a', put '$' at the last position
    for (input1[0 .. page_size - 1]) |*byte| {
        byte.* = 'a';
    }
    input1[page_size - 1] = '$';
    input1[page_size] = '{'; // First byte of next page

    const bitmasks = init(input1[0 .. page_size + 1]);

    // The last bit should be set in multi_str_seg_ends because of ${ spanning the boundary
    const multi_ends = bitmasks.masks[@intFromEnum(Bitmask.multi_str_seg_ends)];
    try std.testing.expect((multi_ends >> 63) & 1 == 1);
}
