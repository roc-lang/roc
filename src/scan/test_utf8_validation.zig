const std = @import("std");
const testing = std.testing;
const Bitmasks = @import("Bitmasks.zig");

test "valid ASCII text" {
    const allocator = testing.allocator;

    // Create aligned buffer
    const text = "Hello, World! This is a test of ASCII text that spans 64 bytes!!";
    const aligned_text = try allocator.alignedAlloc(u8, 16, text.len);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, text);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // All ASCII should be valid UTF-8
    try testing.expectEqual(@as(u64, 0), invalid_utf8_locs);
}

test "valid UTF-8 multibyte sequences" {
    const allocator = testing.allocator;

    // Test various valid UTF-8 sequences
    // 2-byte: ñ (U+00F1), 3-byte: € (U+20AC), 4-byte: 𝄞 (U+1D11E)
    const text = "Hello ñ world € test 𝄞 more text to fill up to sixty-four bytes!";
    const aligned_text = try allocator.alignedAlloc(u8, 16, text.len);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, text);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // All valid UTF-8 should pass
    try testing.expectEqual(@as(u64, 0), invalid_utf8_locs);
}

test "invalid UTF-8 continuation bytes" {
    const allocator = testing.allocator;

    // Create text with invalid continuation byte (0x80 without lead byte)
    var text_bytes = [_]u8{0} ** 64;
    @memcpy(text_bytes[0..5], "Hello");
    text_bytes[5] = 0x80; // Invalid continuation byte
    @memcpy(text_bytes[6..11], "world");

    const aligned_text = try allocator.alignedAlloc(u8, 16, 64);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, &text_bytes);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // Bit 5 should be set (invalid byte at position 5)
    try testing.expect((invalid_utf8_locs & (@as(u64, 1) << 5)) != 0);
}

test "invalid UTF-8 truncated sequence" {
    const allocator = testing.allocator;

    // Create text with truncated 2-byte sequence (0xC3 without continuation)
    var text_bytes = [_]u8{0} ** 64;
    @memcpy(text_bytes[0..5], "Hello");
    text_bytes[5] = 0xC3; // Start of 2-byte sequence
    text_bytes[6] = 'w'; // Should be continuation byte
    @memcpy(text_bytes[7..12], "orld!");

    const aligned_text = try allocator.alignedAlloc(u8, 16, 64);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, &text_bytes);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // Should detect invalid sequence
    try testing.expect(invalid_utf8_locs != 0);
}

test "invalid UTF-8 overlong encoding" {
    const allocator = testing.allocator;

    // Overlong encoding of '/' (U+002F): 0xC0 0xAF instead of just 0x2F
    var text_bytes = [_]u8{0} ** 64;
    @memcpy(text_bytes[0..4], "test");
    text_bytes[4] = 0xC0; // Invalid lead byte for overlong
    text_bytes[5] = 0xAF; // Continuation
    @memcpy(text_bytes[6..10], "test");

    const aligned_text = try allocator.alignedAlloc(u8, 16, 64);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, &text_bytes);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // Should detect overlong encoding
    try testing.expect(invalid_utf8_locs != 0);
}

test "invalid UTF-8 surrogate pairs" {
    const allocator = testing.allocator;

    // UTF-16 surrogate (0xED 0xA0 0x80 = U+D800) - invalid in UTF-8
    var text_bytes = [_]u8{0} ** 64;
    @memcpy(text_bytes[0..4], "test");
    text_bytes[4] = 0xED; // Surrogate lead
    text_bytes[5] = 0xA0; // Continuation
    text_bytes[6] = 0x80; // Continuation
    @memcpy(text_bytes[7..11], "test");

    const aligned_text = try allocator.alignedAlloc(u8, 16, 64);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, &text_bytes);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // Should detect surrogate pair
    try testing.expect(invalid_utf8_locs != 0);
}

test "UTF-8 validation across chunk boundaries" {
    const allocator = testing.allocator;

    // Create 80-byte buffer to force multiple 16-byte chunks
    // This will implicitly test state preservation between chunks
    var text_bytes = [_]u8{' '} ** 80;

    // Test 1: 3-byte sequence split across 16-byte boundary
    // Chunk 1: bytes 0-15, Chunk 2: bytes 16-31
    @memcpy(text_bytes[0..14], "Hello world a ");
    // € = 0xE2 0x82 0xAC - split across boundary at byte 16
    text_bytes[14] = 0xE2; // Byte 14: start of 3-byte sequence
    text_bytes[15] = 0x82; // Byte 15: last byte of chunk 1
    text_bytes[16] = 0xAC; // Byte 16: first byte of chunk 2
    @memcpy(text_bytes[17..25], " test ok");

    // Test 2: 4-byte sequence split across boundary at byte 32
    @memcpy(text_bytes[25..30], " more");
    // 𝄞 (U+1D11E) = 0xF0 0x9D 0x84 0x9E
    text_bytes[30] = 0xF0; // Start 4-byte sequence
    text_bytes[31] = 0x9D; // Last byte of chunk 2
    text_bytes[32] = 0x84; // First byte of chunk 3
    text_bytes[33] = 0x9E; // Complete sequence
    @memcpy(text_bytes[34..40], " done!");

    // Test 3: 2-byte sequence at boundary 48
    @memcpy(text_bytes[40..47], "padding");
    // ñ = 0xC3 0xB1
    text_bytes[47] = 0xC3; // Last byte of chunk 3
    text_bytes[48] = 0xB1; // First byte of chunk 4
    @memcpy(text_bytes[49..60], " final text");

    const aligned_text = try allocator.alignedAlloc(u8, 16, 80);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, &text_bytes);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text[0..64], &invalid_utf8_locs);

    // All sequences are valid UTF-8, properly handled across boundaries
    try testing.expectEqual(@as(u64, 0), invalid_utf8_locs);

    // Also test remaining bytes
    var invalid_utf8_locs2: u64 = 0;
    _ = Bitmasks.init(aligned_text[64..80], &invalid_utf8_locs2);
    try testing.expectEqual(@as(u64, 0), invalid_utf8_locs2);
}

test "mixed valid and invalid UTF-8" {
    const allocator = testing.allocator;

    // Mix of valid and invalid sequences
    var text_bytes = [_]u8{' '} ** 64;
    @memcpy(text_bytes[0..5], "Valid");
    text_bytes[5] = 0xFF; // Invalid byte (never valid in UTF-8)
    @memcpy(text_bytes[6..13], "text€"); // Valid with multibyte (€ is 3 bytes)
    text_bytes[14] = 0x80; // Invalid continuation without lead
    @memcpy(text_bytes[15..20], "more!");

    const aligned_text = try allocator.alignedAlloc(u8, 16, 64);
    defer allocator.free(aligned_text);
    @memcpy(aligned_text, &text_bytes);

    var invalid_utf8_locs: u64 = 0;
    _ = Bitmasks.init(aligned_text, &invalid_utf8_locs);

    // Should mark positions 5 and 14 as invalid
    try testing.expect((invalid_utf8_locs & (@as(u64, 1) << 5)) != 0);
    try testing.expect((invalid_utf8_locs & (@as(u64, 1) << 14)) != 0);
}
