//! This efficiently stores byte slices for literals (strings, arbitrarily-long numbers, etc.)
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
const SafeList = @import("safe_list.zig").SafeList;
const Allocator = std.mem.Allocator;

const Self = @This();

entries: SafeList(u8),

pub const Idx = enum(u32) {
    _,

    fn asUsize(self: Idx) usize {
        return @intCast(@intFromEnum(self));
    }
};

const ByteMarker = enum {
    u16 = 254,
    u32 = 255,
};

fn lenHeaderSize(self: *const Self, idx: Self.Idx, slice_len: *usize) usize {
    const idx_usize = idx.asUsize();
    const items = self.entries.items.items;

    // Read the first byte to determine encoding
    const first_byte = items[idx_usize];

    if (first_byte < @intFromEnum(ByteMarker.u16)) {
        // Length fits in a single byte
        slice_len.* = @intCast(first_byte);
        return idx_usize + 1;
    } else if (first_byte == @intFromEnum(ByteMarker.u16)) {
        // Length fits in u16
        const len_bytes = items[idx_usize + 1 .. idx_usize + 3];
        slice_len.* = @intCast(std.mem.readInt(u16, len_bytes[0..2], .little));
        return idx_usize + 3;
    } else {
        // Length fits in u32
        const len_bytes = items[idx_usize + 1 .. idx_usize + 5];
        slice_len.* = std.mem.readInt(u32, len_bytes[0..4], .little);
        return idx_usize + 5;
    }
}

pub fn slice(self: *const Self, idx: Self.Idx) []const u8 {
    const idx_usize = idx.asUsize();
    const items = self.entries.items.items;
    var slice_len = undefined;
    const slice_start = self.lenHeaderSize(idx, &slice_len);

    return items[slice_start .. slice_start + @as(usize, @intCast(*slice_len))];
}

/// Appends the given slice inline to the bytes, with the length written using
/// variable-length encoding, then returns the index of the length.
pub fn append(self: *Self, allocator: Allocator, bytes: []const u8) Allocator.Error!Self.Idx {
    return self.appendInternal(allocator, bytes, 0);
}

fn appendInternal(
    self: *Self,
    allocator: Allocator,
    bytes: []const u8,
    existing_len: usize,
) Allocator.Error!Self.Idx {
    const total_bytes_len = bytes.len + existing_len;

    // Append the actual bytes first, then write the header after the bytes.
    const len_idx = if (total_bytes_len < @intFromEnum(ByteMarker.u16)) blk: {
        const len_size = 1; // u8 length
        try self.entries.items.ensureUnusedCapacity(allocator, len_size + bytes.len);
        self.entries.items.appendSliceAssumeCapacity(bytes);
        const items_len = self.entries.items.items.len;
        self.entries.items.appendAssumeCapacity(@as(u8, @intCast(total_bytes_len)));
        break :blk items_len;
    } else if (total_bytes_len < std.math.maxInt(u16)) blk: {
        const len_size = 3; // 1 byte marker + 2 bytes for u16 length
        try self.entries.items.ensureUnusedCapacity(allocator, len_size + bytes.len);
        self.entries.items.appendSliceAssumeCapacity(bytes);
        const items_len = self.entries.items.items.len;
        self.entries.items.appendAssumeCapacity(@intFromEnum(ByteMarker.u16));
        var buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &buf, @as(u16, @intCast(total_bytes_len)), .little);
        self.entries.items.appendSliceAssumeCapacity(&buf);
        break :blk items_len;
    } else blk: {
        const len_size = 5; // 1 byte marker + 4 bytes for u32 length
        try self.entries.items.ensureUnusedCapacity(allocator, len_size + bytes.len);
        self.entries.items.appendSliceAssumeCapacity(bytes);
        const items_len = self.entries.items.items.len;
        self.entries.items.appendAssumeCapacity(@intFromEnum(ByteMarker.u16));
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @as(u32, @intCast(total_bytes_len)), .little);
        self.entries.items.appendSliceAssumeCapacity(&buf);
        break :blk items_len;
    };

    const idx = @as(Self.Idx, @enumFromInt(@as(u32, @intCast(len_idx))));

    self.last_idx = @enumFromInt(idx);

    return idx;
}

/// Appends the given bytes (just like append()) while also validating that they are
/// valid UTF-8. Returns an error if they are not valid UTF-8.
pub fn appendUtf8(
    self: *Self,
    allocator: Allocator,
    bytes: []const u8,
) (error{InvalidUtf8} || Allocator.Error)!Self.Idx {
    if (std.unicode.utf8ValidateSlice(bytes)) {
        // TODO in the future, we can use SIMD to validate UTF-8
        // while also copying from the vectors we already have loaded.
        return try self.append(allocator, bytes);
    } else {
        return .InvalidUtf8;
    }
}

/// We ask for old_last_idx as a pointer, and then write to that pointer,
/// just to make this harder to misuse - to make it really clear that we
/// are changing that Idx!
pub fn extendLast(
    self: *Self,
    allocator: Allocator,
    bytes: []const u8,
    old_last_idx: *Idx,
) Allocator.Error!void {
    std.debug.assert(bytes.len > 0);

    var old_len: usize = undefined;
    const header_size = self.lenHeaderSize(old_last_idx, &old_len);

    // This should only ever be passed the last item in the list!
    std.debug.assert(old_last_idx.asUsize() + header_size ==
        @as(usize, @intCast(self.entries.len())));

    // Write the bytes on top of the previous length entry, with the new
    // length at the end as usual. This is why we have the length at the
    // end of the slice instead of at the beginning: if it were at the beginning,
    // and extending the bytes resulted in needing to jump up a size category,
    // then we'd have to re-copy all of those bytes. This way we always just
    // overwrite and write a fresh length at the end, where there's always
    // enough space for it no matter what size it ended up being.
    self.entries.items.items.len = @intCast(self.last_idx);

    old_last_idx.* = try self.appendInternal(allocator, bytes, old_len);
}

pub fn extendLastUtf8(
    self: *Self,
    allocator: Allocator,
    bytes: []const u8,
    old_last_idx: *Idx,
) (error{InvalidUtf8} || Allocator.Error)!void {
    if (std.unicode.utf8ValidateSlice(bytes)) {
        // TODO in the future, we can use SIMD to validate UTF-8
        // while also copying from the vectors we already have loaded.
        return try self.extendLast(allocator, bytes, old_last_idx);
    } else {
        return .InvalidUtf8;
    }
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

test "ByteSlices: store and retrieve single slice" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var slices = Self{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    const data = "Hello, World!";
    const idx = try slices.append(allocator, data);

    const retrieved = slices.slice(idx);
    try testing.expectEqualSlices(u8, data, retrieved);
}

test "ByteSlices: store and retrieve multiple slices" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var slices = Self{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    const data1 = "First";
    const data2 = "Second string";
    const data3 = "Third!";

    const idx1 = try slices.append(allocator, data1);
    const idx2 = try slices.append(allocator, data2);
    const idx3 = try slices.append(allocator, data3);

    try testing.expectEqualSlices(u8, data1, slices.slice(idx1));
    try testing.expectEqualSlices(u8, data2, slices.slice(idx2));
    try testing.expectEqualSlices(u8, data3, slices.slice(idx3));
}

test "ByteSlices: various lengths" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var slices = Self{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Test strings of various lengths
    const test_cases = [_][]const u8{
        "", // Empty string
        "a", // 1 byte
        "ab", // 2 bytes
        "abc", // 3 bytes
        "abcd", // 4 bytes
        "abcde", // 5 bytes
        "Hello!", // 6 bytes
        "1234567", // 7 bytes
        "12345678", // 8 bytes
        "123456789", // 9 bytes
    };

    var indices: [test_cases.len]Self.Idx = undefined;

    // Store all strings
    for (test_cases, 0..) |data, i| {
        indices[i] = try slices.append(allocator, data);
    }

    // Verify all can be retrieved correctly
    for (test_cases, 0..) |expected, i| {
        const retrieved = slices.slice(indices[i]);
        try testing.expectEqualSlices(u8, expected, retrieved);
    }
}

test "ByteSlices: variable length encoding layout" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var slices = Self{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Start with a known state
    const first = "x"; // 1 byte
    const idx1 = try slices.append(allocator, first);

    // After storing 1 byte length + 1 byte data, we have 2 bytes total
    const second = "yy"; // 2 bytes
    const idx2 = try slices.append(allocator, second);

    // Verify both can still be retrieved
    try testing.expectEqualSlices(u8, first, slices.slice(idx1));
    try testing.expectEqualSlices(u8, second, slices.slice(idx2));

    // Check the actual buffer layout with variable-length encoding
    const buffer = slices.entries.items.items;

    // First entry: 1 byte for length (value 1) + 1 byte data = 2 bytes
    try testing.expectEqual(@as(u8, 1), buffer[0]); // Length encoded as single byte
    try testing.expectEqual(@as(u8, 'x'), buffer[1]);

    // Second entry starts at index 2: 1 byte for length (value 2) + 2 bytes data
    try testing.expectEqual(@as(u8, 2), buffer[2]); // Length encoded as single byte
    try testing.expectEqual(@as(u8, 'y'), buffer[3]);
    try testing.expectEqual(@as(u8, 'y'), buffer[4]);
}

test "ByteSlices: large strings" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var slices = Self{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Create a large string
    var large_data: [1000]u8 = undefined;
    for (&large_data, 0..) |*byte, i| {
        byte.* = @as(u8, @intCast(i % 256));
    }

    const idx = try slices.append(allocator, &large_data);
    const retrieved = slices.slice(idx);

    try testing.expectEqualSlices(u8, &large_data, retrieved);
}

test "ByteSlices: interleaved small and large" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var slices = Self{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    const small1 = "small";
    var large: [500]u8 = undefined;
    @memset(&large, 'L');
    const small2 = "tiny";

    const idx1 = try slices.append(allocator, small1);
    const idx2 = try slices.append(allocator, &large);
    const idx3 = try slices.append(allocator, small2);

    try testing.expectEqualSlices(u8, small1, slices.slice(idx1));
    try testing.expectEqualSlices(u8, &large, slices.slice(idx2));
    try testing.expectEqualSlices(u8, small2, slices.slice(idx3));
}
