//! 64-bit bitmasks for a page of source code bytes. (Each page is up to 64B.)

const std = @import("std");
const builtin = @import("builtin");

pub const Self = @This();
pub const page_size: usize = 64;

/// How many bytes within the 64B page we've already processed.
/// TODO NOTE: we make the *masks* do the padding, by giving them a set of pointers to load their 16B at a time from,
/// and then giving them different pointers if we're on the last page (pointers which point to 16B of newlines).
/// This way we never need to actually load 64B into memory!
bytes_chomped: usize = 0,

/// The 64-bit masks, with each bit representing a byte in a 64B page.
masks: [std.meta.fields(Bitmask).len]u64 = .{0} ** std.meta.fields(Bitmask).len,

/// If any invalid UTF-8 characters are found, they will be marked using the given
/// u64 bitmap (0-bits mean valid UTF-8 bytes, 1-bits mean invalid UTF-8 found there).
pub fn init(src_bytes: []align(16) u8, invalid_utf8_locs: *u64) Self {
    if (src_bytes.len >= page_size) {
        return initExact(src_bytes, invalid_utf8_locs);
    } else {
        return initPartialPage(src_bytes, invalid_utf8_locs);
    }
}

/// Only call when there are enough bytes in here that we can just load everything.
fn initExact(src_bytes: []align(16) u8, _: *u64) Self {
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
    const high_nibble_lookup: @Vector(16, u8) = high_table;
    const low_nibble_lookup: @Vector(16, u8) = low_table;

    // Branchlessly get a mask where the last bit is set iff the byte at the start of the page is '{'.
    // The purpose of this is to handle the edge case where the last byte of this page and the first
    // byte of the next page is '{', and we wouldn't otherwise register this as the start of a string
    // interpolation. This lets us branchlessly do that, while avoiding out-of-bounds reads by pointing
    // to a known zero (so that it will fail the `== '{'` check) if there is no next page.
    const first_in_next_page_ptr = src_bytes.ptr + page_size;
    const first_in_next_page = *if (src_bytes.len > page_size) first_in_next_page_ptr else (&dollar_mask);
    // TODO is this the bit we want? Like will the very last bit of the u64 be set if we do this? Even given endianness?
    const last_byte_starts_interpolation: u64 = first_in_next_page == '{';

    // Use 128-bit SIMD because it's supported on all the targets we build for,
    // including wasm32.
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
        const high_result = @shuffle(u8, high_nibble_lookup, undefined, high_nibbles);
        const low_result = @shuffle(u8, low_nibble_lookup, undefined, low_nibbles);

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

        const is_whitespace = is_space | is_tab | is_cr | newline_cmp;
        const non_ws_bits = vectorToBitmask(!is_whitespace);
        non_whitespace_mask |= @as(u64, non_ws_bits) << @intCast(offset);

        // TODO: do UTF-8 validation on this chunk and update invalid_utf8_locs
    }

    const multi_str_seg_ends =
        (dollar_mask & (brace_mask << 1)) | last_byte_starts_interpolation | newline_mask | backslash_mask;
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

fn initPartialPage(_: []align(16) u8, _: *u64) Self {
    @panic("initPartial is not implemented yet");
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
    newlines, // "\n" - for end of comments, and also formatter cares about blank lines
    non_whitespace, // anything other than [" ", "\t", "\r", "\n"]
    num_or_ident, // '0'..'9', 'A'..'Z', 'a'..'z', '_', and UTF-8 multibyte sequences
};
