//! This efficiently stores byte slices (like strings or arbitrarily-long number literals)
//! in a single contiguous buffer, with each slice prefixed by its length.
//!
//! The lengths are stored in a simple variable-length encoding that assumes lengths under
//! 250 will be by far the most common. (Some string literals will be more than 250B, but
//! it will be very rare.) As such, the length is usually stored in 1 byte.
//!
//! Length encoding:
//! - up to 252: stored as a single byte
//! - up to 2^16: first byte is 253, followed by 2 bytes (u16 little-endian)
//! - up to 2^24: first byte is 254, followed by 3 bytes (u24 little-endian)
//! - up to 2^32: first byte is 255, followed by 4 bytes (u32 little-endian)

const std = @import("std");
const collections = @import("collections");
const Allocator = std.mem.Allocator;

const Self = @This();

entries: collections.SafeList(u8),

pub const Idx = enum(u32) {
    _,

    fn asUsize(self: Idx) usize {
        return @intCast(@intFromEnum(self));
    }
};

pub fn slice(self: *const Self, idx: Self.Idx) []const u8 {
    const idx_usize = idx.asUsize();
    const items = self.entries.items.items;

    // Read the first byte to determine encoding
    const first_byte = items[idx_usize];

    var slice_len: u32 = undefined;
    var slice_start: usize = undefined;

    if (first_byte < 253) {
        // Length fits in a single byte
        slice_len = first_byte;
        slice_start = idx_usize + 1;
    } else if (first_byte == 253) {
        // Length fits in u16
        const len_bytes = items[idx_usize + 1 .. idx_usize + 3];
        slice_len = std.mem.readInt(u16, len_bytes[0..2], .little);
        slice_start = idx_usize + 3;
    } else if (first_byte == 254) {
        // Length fits in u24
        const len_bytes = items[idx_usize + 1 .. idx_usize + 4];
        var buf: [4]u8 = .{0} ** 4;
        buf[0] = len_bytes[0];
        buf[1] = len_bytes[1];
        buf[2] = len_bytes[2];
        slice_len = std.mem.readInt(u32, &buf, .little);
        slice_start = idx_usize + 4;
    } else { // first_byte == 255
        // Length fits in u32
        const len_bytes = items[idx_usize + 1 .. idx_usize + 5];
        slice_len = std.mem.readInt(u32, len_bytes[0..4], .little);
        slice_start = idx_usize + 5;
    }

    return items[slice_start .. slice_start + @as(usize, @intCast(slice_len))];
}

/// Appends the given slice inline to the bytes, with the length written using
/// variable-length encoding, then returns the index of the length.
pub fn append(self: *Self, allocator: Allocator, bytes: []const u8) Allocator.Error!Self.Idx {
    const current_len = self.entries.items.items.len;
    const len_idx = current_len;

    // Determine how many bytes we need for the length encoding
    var header_size: usize = undefined;
    if (bytes.len < 253) {
        header_size = 1;
    } else if (bytes.len < (1 << 16)) {
        header_size = 3; // 1 byte marker + 2 bytes for u16
    } else if (bytes.len < (1 << 24)) {
        header_size = 4; // 1 byte marker + 3 bytes for u24
    } else {
        header_size = 5; // 1 byte marker + 4 bytes for u32
    }

    // Reserve enough space for the header and the actual bytes
    try self.entries.items.ensureUnusedCapacity(allocator, header_size + bytes.len);

    // Write the length header
    if (bytes.len < 253) {
        // Single byte encoding
        self.entries.items.appendAssumeCapacity(@as(u8, @intCast(bytes.len)));
    } else if (bytes.len < (1 << 16)) {
        // u16 encoding
        self.entries.items.appendAssumeCapacity(253);
        var buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &buf, @as(u16, @intCast(bytes.len)), .little);
        self.entries.items.appendSliceAssumeCapacity(&buf);
    } else if (bytes.len < (1 << 24)) {
        // u24 encoding
        self.entries.items.appendAssumeCapacity(254);
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @as(u32, @intCast(bytes.len)), .little);
        self.entries.items.appendSliceAssumeCapacity(buf[0..3]);
    } else {
        // u32 encoding
        self.entries.items.appendAssumeCapacity(255);
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @as(u32, @intCast(bytes.len)), .little);
        self.entries.items.appendSliceAssumeCapacity(&buf);
    }

    // Append the actual bytes
    self.entries.items.appendSliceAssumeCapacity(bytes);

    // Return the index where the length was written
    return @as(Self.Idx, @enumFromInt(@as(u32, @intCast(len_idx))));
}

test "variable length encoding - single byte (< 253)" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var byte_slices = Self{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Test small lengths
    const test_cases = [_][]const u8{
        "", // 0 bytes
        "a", // 1 byte
        "hello", // 5 bytes
        "a" ** 100, // 100 bytes
        "b" ** 252, // 252 bytes (max single-byte encoding)
    };

    for (test_cases) |test_str| {
        const idx = try byte_slices.append(allocator, test_str);
        const retrieved = byte_slices.slice(idx);
        try testing.expectEqualSlices(u8, test_str, retrieved);
    }
}

test "variable length encoding - u16 (253 to 65535)" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var byte_slices = Self{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Test u16 range
    const test_sizes = [_]usize{
        253, // Minimum u16 encoding
        254,
        255,
        256,
        1000,
        65535, // Maximum u16 value
    };

    for (test_sizes) |size| {
        const test_str = try allocator.alloc(u8, size);
        defer allocator.free(test_str);
        @memset(test_str, 'x');

        const idx = try byte_slices.append(allocator, test_str);
        const retrieved = byte_slices.slice(idx);
        try testing.expectEqual(size, retrieved.len);
        try testing.expectEqualSlices(u8, test_str, retrieved);
    }
}

test "variable length encoding - u24 (65536 to 16777215)" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var byte_slices = Self{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Test u24 range
    const test_sizes = [_]usize{
        65536, // Minimum u24 encoding
        65537,
        100000,
        1000000,
        16777215, // Maximum u24 value (2^24 - 1)
    };

    for (test_sizes) |size| {
        const test_str = try allocator.alloc(u8, size);
        defer allocator.free(test_str);
        @memset(test_str, 'y');

        const idx = try byte_slices.append(allocator, test_str);
        const retrieved = byte_slices.slice(idx);
        try testing.expectEqual(size, retrieved.len);
        // Just check length and first/last bytes for large allocations
        try testing.expectEqual('y', retrieved[0]);
        try testing.expectEqual('y', retrieved[retrieved.len - 1]);
    }
}

test "variable length encoding - u32 (16777216 and up)" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var byte_slices = Self{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Test u32 range - using smaller values to avoid huge allocations in tests
    const test_sizes = [_]usize{
        16777216, // Minimum u32 encoding (2^24)
        16777217,
        20000000,
    };

    for (test_sizes) |size| {
        const test_str = try allocator.alloc(u8, size);
        defer allocator.free(test_str);
        @memset(test_str, 'z');

        const idx = try byte_slices.append(allocator, test_str);
        const retrieved = byte_slices.slice(idx);
        try testing.expectEqual(size, retrieved.len);
        // Just check length and first/last bytes for large allocations
        try testing.expectEqual('z', retrieved[0]);
        try testing.expectEqual('z', retrieved[retrieved.len - 1]);
    }
}

test "variable length encoding - mixed sizes" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var byte_slices = Self{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Add multiple strings of different sizes and verify all can be retrieved
    const str1 = "short"; // 5 bytes (single byte encoding)
    const idx1 = try byte_slices.append(allocator, str1);

    const str2 = "a" ** 300; // 300 bytes (u16 encoding)
    const idx2 = try byte_slices.append(allocator, str2);

    const str3 = ""; // 0 bytes (single byte encoding)
    const idx3 = try byte_slices.append(allocator, str3);

    const str4 = "b" ** 252; // 252 bytes (single byte encoding - boundary)
    const idx4 = try byte_slices.append(allocator, str4);

    const str5 = "c" ** 253; // 253 bytes (u16 encoding - boundary)
    const idx5 = try byte_slices.append(allocator, str5);

    // Verify all strings can be retrieved correctly
    try testing.expectEqualSlices(u8, str1, byte_slices.slice(idx1));
    try testing.expectEqualSlices(u8, str2, byte_slices.slice(idx2));
    try testing.expectEqualSlices(u8, str3, byte_slices.slice(idx3));
    try testing.expectEqualSlices(u8, str4, byte_slices.slice(idx4));
    try testing.expectEqualSlices(u8, str5, byte_slices.slice(idx5));
}

test "variable length encoding - edge cases" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var byte_slices = Self{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Test boundary values specifically
    const boundaries = [_]struct { size: usize, expected_header_size: usize }{
        .{ .size = 0, .expected_header_size = 1 }, // Empty string
        .{ .size = 1, .expected_header_size = 1 },
        .{ .size = 252, .expected_header_size = 1 }, // Max single byte
        .{ .size = 253, .expected_header_size = 3 }, // Min u16
        .{ .size = 65535, .expected_header_size = 3 }, // Max u16
        .{ .size = 65536, .expected_header_size = 4 }, // Min u24
        .{ .size = 16777215, .expected_header_size = 4 }, // Max u24
        .{ .size = 16777216, .expected_header_size = 5 }, // Min u32
    };

    for (boundaries) |boundary| {
        const test_str = try allocator.alloc(u8, boundary.size);
        defer allocator.free(test_str);
        @memset(test_str, 'e');

        const before_len = byte_slices.entries.items.items.len;
        const idx = try byte_slices.append(allocator, test_str);
        const after_len = byte_slices.entries.items.items.len;

        // Check that the total size added is header + data
        try testing.expectEqual(boundary.expected_header_size + boundary.size, after_len - before_len);

        // Verify we can retrieve the data correctly
        const retrieved = byte_slices.slice(idx);
        try testing.expectEqual(boundary.size, retrieved.len);
        if (boundary.size > 0) {
            try testing.expectEqual('e', retrieved[0]);
            try testing.expectEqual('e', retrieved[retrieved.len - 1]);
        }
    }
}
