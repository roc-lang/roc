//! Utility functions for processing Roc source code.

const std = @import("std");
const SourceDigest = @import("SourceDigest.zig");

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

/// A module's source as the compiler consumes it, with its digest.
pub const DigestedSource = struct {
    /// The file's bytes with line endings normalized.
    source: []u8,
    /// Digest of `source`.
    digest: SourceDigest,
    /// Digest of the file's bytes exactly as read, when it was requested.
    /// Watch mode compares this against the file on disk.
    file_digest: ?SourceDigest,
};

/// Normalize the line endings of `data` exactly like
/// `normalizeLineEndingsRealloc`, including its ownership rules, and digest
/// the result.
///
/// With `want_file_digest`, the bytes as read are digested too. A file with no
/// CRLF is byte-identical before and after normalization, so both digests are
/// one hash of its bytes; only a file that normalization rewrote is hashed in
/// each form.
pub fn normalizeAndDigest(
    allocator: std.mem.Allocator,
    data: []u8,
    want_file_digest: bool,
) std.mem.Allocator.Error!DigestedSource {
    const file_len = data.len;
    const file_digest: ?SourceDigest = if (want_file_digest) SourceDigest.of(data) else null;
    const source = try normalizeLineEndingsRealloc(allocator, data);
    // Normalization only removes bytes, so an unchanged length means the
    // bytes are unchanged.
    const digest = if (file_digest != null and source.len == file_len)
        file_digest.?
    else
        SourceDigest.of(source);
    return .{ .source = source, .digest = digest, .file_digest = file_digest };
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

test "normalizeAndDigest digests the normalized bytes, and the bytes as read on request" {
    const allocator = std.testing.allocator;

    const lf_only = try normalizeAndDigest(allocator, try allocator.dupe(u8, "a = 1\nb = 2\n"), false);
    defer allocator.free(lf_only.source);
    try std.testing.expect(lf_only.digest.eql(SourceDigest.of("a = 1\nb = 2\n")));
    try std.testing.expect(lf_only.file_digest == null);

    const lf_both = try normalizeAndDigest(allocator, try allocator.dupe(u8, "a = 1\nb = 2\n"), true);
    defer allocator.free(lf_both.source);
    try std.testing.expect(lf_both.digest.eql(SourceDigest.of("a = 1\nb = 2\n")));
    try std.testing.expect(lf_both.file_digest.?.eql(lf_both.digest));

    const crlf_both = try normalizeAndDigest(allocator, try allocator.dupe(u8, "a = 1\r\nb = 2\r\n"), true);
    defer allocator.free(crlf_both.source);
    try std.testing.expectEqualStrings("a = 1\nb = 2\n", crlf_both.source);
    try std.testing.expect(crlf_both.digest.eql(SourceDigest.of("a = 1\nb = 2\n")));
    try std.testing.expect(crlf_both.file_digest.?.eql(SourceDigest.of("a = 1\r\nb = 2\r\n")));
}
