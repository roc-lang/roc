//! Utility functions for processing Roc source code.

const std = @import("std");

/// Normalizes line endings in source code by converting CRLF (\r\n) to LF (\n).
///
/// This ensures consistent behavior across different operating systems. On Windows,
/// text files often have CRLF line endings, but Roc source code should be processed
/// with LF-only line endings for consistent parsing and formatting.
///
/// The normalization is done in-place, modifying the input buffer and returning
/// a slice of the normalized content. The returned slice will be the same length
/// or shorter than the input.
///
/// IMPORTANT: This function returns a sub-slice of the input. If the input was
/// allocated, the caller must keep track of the original allocation for freeing.
/// For allocated buffers where proper memory management is needed, use
/// `normalizeLineEndingsRealloc` instead.
///
/// Standalone \r characters (not followed by \n) are preserved as-is - the tokenizer
/// will report these as errors separately via the MisplacedCarriageReturn diagnostic.
pub fn normalizeLineEndings(source: []u8) []u8 {
    if (source.len == 0) return source;

    var write_pos: usize = 0;
    var read_pos: usize = 0;

    while (read_pos < source.len) {
        const c = source[read_pos];
        if (c == '\r' and read_pos + 1 < source.len and source[read_pos + 1] == '\n') {
            // Skip the \r in \r\n sequence, only write the \n
            read_pos += 1;
        } else {
            source[write_pos] = c;
            write_pos += 1;
            read_pos += 1;
        }
    }

    return source[0..write_pos];
}

/// Normalizes line endings and reallocates the buffer to the correct size.
///
/// This function normalizes CRLF to LF and properly handles memory:
/// - If no normalization is needed, returns the original buffer unchanged
/// - If normalization is needed, reallocates to the correct size and frees the original
///
/// The returned buffer is always properly sized for freeing with the allocator.
/// The caller is responsible for freeing the returned buffer.
pub fn normalizeLineEndingsRealloc(allocator: std.mem.Allocator, source: []u8) std.mem.Allocator.Error![]u8 {
    if (source.len == 0) return source;

    // First, check if normalization is needed and count CRLF sequences
    var crlf_count: usize = 0;
    for (0..source.len) |i| {
        if (source[i] == '\r' and i + 1 < source.len and source[i + 1] == '\n') {
            crlf_count += 1;
        }
    }

    // If no CRLF sequences, return original buffer unchanged
    if (crlf_count == 0) {
        return source;
    }

    // Normalize in place first
    var write_pos: usize = 0;
    var read_pos: usize = 0;

    while (read_pos < source.len) {
        const c = source[read_pos];
        if (c == '\r' and read_pos + 1 < source.len and source[read_pos + 1] == '\n') {
            // Skip the \r in \r\n sequence, only write the \n
            read_pos += 1;
        } else {
            source[write_pos] = c;
            write_pos += 1;
            read_pos += 1;
        }
    }

    // Allocate a new properly-sized buffer
    const new_len = write_pos;
    const new_buffer = try allocator.alloc(u8, new_len);
    @memcpy(new_buffer, source[0..new_len]);

    // Free the original oversized buffer
    allocator.free(source);

    return new_buffer;
}

/// Normalizes line endings by allocating a new buffer if needed.
/// Returns the normalized source and a boolean indicating whether a new buffer was allocated.
/// If no normalization was needed, returns the original slice and false.
/// If normalization was performed, returns a new allocated slice and true.
///
/// The caller is responsible for freeing the returned slice if the boolean is true.
pub fn normalizeLineEndingsAlloc(allocator: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!struct { data: []u8, allocated: bool } {
    // First pass: check if normalization is needed
    var needs_normalization = false;
    for (0..source.len) |i| {
        if (source[i] == '\r' and i + 1 < source.len and source[i + 1] == '\n') {
            needs_normalization = true;
            break;
        }
    }

    if (!needs_normalization) {
        // No CRLF sequences found, can use original buffer
        // But we need to return a mutable copy since the caller expects []u8
        const copy = try allocator.alloc(u8, source.len);
        @memcpy(copy, source);
        return .{ .data = copy, .allocated = true };
    }

    // Count how many \r\n sequences there are to calculate exact output size
    var crlf_count: usize = 0;
    for (0..source.len) |i| {
        if (source[i] == '\r' and i + 1 < source.len and source[i + 1] == '\n') {
            crlf_count += 1;
        }
    }

    // Allocate exact size needed
    const new_len = source.len - crlf_count;
    const result = try allocator.alloc(u8, new_len);

    // Copy with normalization
    var write_pos: usize = 0;
    var read_pos: usize = 0;
    while (read_pos < source.len) {
        const c = source[read_pos];
        if (c == '\r' and read_pos + 1 < source.len and source[read_pos + 1] == '\n') {
            // Skip the \r in \r\n sequence
            read_pos += 1;
        } else {
            result[write_pos] = c;
            write_pos += 1;
            read_pos += 1;
        }
    }

    return .{ .data = result, .allocated = true };
}

test "normalizeLineEndings - no changes needed" {
    const allocator = std.testing.allocator;

    // Test with LF-only content
    {
        const source = try allocator.dupe(u8, "hello\nworld\n");
        defer allocator.free(source);
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("hello\nworld\n", result);
    }

    // Test with empty content
    {
        const source: []u8 = &.{};
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("", result);
    }
}

test "normalizeLineEndings - CRLF to LF" {
    const allocator = std.testing.allocator;

    // Test with CRLF content
    {
        const source = try allocator.dupe(u8, "hello\r\nworld\r\n");
        defer allocator.free(source);
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("hello\nworld\n", result);
    }

    // Test with mixed line endings
    {
        const source = try allocator.dupe(u8, "line1\r\nline2\nline3\r\n");
        defer allocator.free(source);
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("line1\nline2\nline3\n", result);
    }
}

test "normalizeLineEndings - standalone CR preserved" {
    const allocator = std.testing.allocator;

    // Standalone \r (not followed by \n) should be preserved
    // The tokenizer will handle these as errors
    {
        const source = try allocator.dupe(u8, "hello\rworld");
        defer allocator.free(source);
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("hello\rworld", result);
    }

    // Mix of standalone \r and \r\n
    {
        const source = try allocator.dupe(u8, "a\rb\r\nc");
        defer allocator.free(source);
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("a\rb\nc", result);
    }
}

test "normalizeLineEndings - multiline strings" {
    const allocator = std.testing.allocator;

    // Test with Roc multiline string syntax
    {
        const source = try allocator.dupe(u8, "lines =\r\n    \\\\first line\r\nOk(lines)\r\n");
        defer allocator.free(source);
        const result = normalizeLineEndings(source);
        try std.testing.expectEqualStrings("lines =\n    \\\\first line\nOk(lines)\n", result);
    }
}

test "normalizeLineEndingsAlloc - allocates new buffer" {
    const allocator = std.testing.allocator;

    // Test with CRLF content - should allocate new buffer
    {
        const source = "hello\r\nworld\r\n";
        const result = try normalizeLineEndingsAlloc(allocator, source);
        defer allocator.free(result.data);
        try std.testing.expect(result.allocated);
        try std.testing.expectEqualStrings("hello\nworld\n", result.data);
    }

    // Test with LF-only content - still allocates a copy
    {
        const source = "hello\nworld\n";
        const result = try normalizeLineEndingsAlloc(allocator, source);
        defer allocator.free(result.data);
        try std.testing.expect(result.allocated);
        try std.testing.expectEqualStrings("hello\nworld\n", result.data);
    }
}

test "normalizeLineEndingsRealloc - proper memory management" {
    const allocator = std.testing.allocator;

    // Test with CRLF content - should reallocate to smaller buffer
    {
        const source = try allocator.dupe(u8, "hello\r\nworld\r\n");
        // source is now owned, normalizeLineEndingsRealloc will free it if needed
        const result = try normalizeLineEndingsRealloc(allocator, source);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("hello\nworld\n", result);
    }

    // Test with LF-only content - should return original buffer unchanged
    {
        const source = try allocator.dupe(u8, "hello\nworld\n");
        const result = try normalizeLineEndingsRealloc(allocator, source);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("hello\nworld\n", result);
        // result should be the same pointer as source (no reallocation)
        try std.testing.expectEqual(source.ptr, result.ptr);
    }

    // Test with empty content
    {
        const source: []u8 = try allocator.alloc(u8, 0);
        const result = try normalizeLineEndingsRealloc(allocator, source);
        defer allocator.free(result);
        try std.testing.expectEqualStrings("", result);
    }
}
