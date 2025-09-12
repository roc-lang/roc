const std = @import("std");
const Bitmasks = @import("Bitmasks.zig");
const utf8_validation = @import("utf8_validation.zig");
const expect = std.testing.expect;

test "UTF-8 validation detects invalid bytes" {
    const allocator = std.testing.allocator;

    // Test with invalid UTF-8
    const invalid_utf8 = "\xFF\xFE"; // Invalid UTF-8 bytes
    const aligned_bytes = try allocator.alignedAlloc(u8, 16, 128);
    defer allocator.free(aligned_bytes);

    @memset(aligned_bytes, ' '); // Fill with spaces
    @memcpy(aligned_bytes[0..invalid_utf8.len], invalid_utf8);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_bytes, &invalid_utf8_locs);

    // Should have detected invalid UTF-8
    try expect(invalid_utf8_locs != 0);
    // 0xFF triggers error at position 0, 0xFE also invalid
    // Following bytes marked as continuation errors
}

test "UTF-8 validation accepts valid ASCII" {
    const allocator = std.testing.allocator;

    // Test with valid ASCII
    const valid_ascii = "Hello World!";
    const aligned_bytes = try allocator.alignedAlloc(u8, 16, 128);
    defer allocator.free(aligned_bytes);

    @memset(aligned_bytes, ' '); // Fill with spaces
    @memcpy(aligned_bytes[0..valid_ascii.len], valid_ascii);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_bytes, &invalid_utf8_locs);

    // Should have no invalid UTF-8 locations
    try expect(invalid_utf8_locs == 0);
}

test "UTF-8 validation with truncated sequence" {
    const allocator = std.testing.allocator;

    // Test with truncated UTF-8
    const truncated = "Hello\xE2\x82"; // Truncated 3-byte sequence
    const aligned_bytes = try allocator.alignedAlloc(u8, 16, 128);
    defer allocator.free(aligned_bytes);

    @memset(aligned_bytes, ' '); // Fill with spaces
    @memcpy(aligned_bytes[0..truncated.len], truncated);
    aligned_bytes[truncated.len] = ' '; // Add a space after to ensure truncation is detected

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_bytes, &invalid_utf8_locs);

    // Should have detected truncated UTF-8
    try expect(invalid_utf8_locs != 0);
}
