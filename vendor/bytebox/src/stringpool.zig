const std = @import("std");
const StableArray = @import("stable-array").StableArray;

const hashString = std.hash_map.hashString;
const StringHashLookupTable = std.hash_map.AutoHashMap(u64, usize);

const StringPool = @This();

buffer: StableArray(u8),
lookup: StringHashLookupTable,

const StringLenType = u16;

pub const PutError = error{StringLengthTooLong};

pub fn init(max_stringpool_bytes: usize, allocator: std.mem.Allocator) StringPool {
    return StringPool{
        .buffer = StableArray(u8).init(max_stringpool_bytes),
        .lookup = StringHashLookupTable.init(allocator),
    };
}

pub fn deinit(self: *StringPool) void {
    self.buffer.deinit();
    self.lookup.deinit();
}

pub fn put(self: *StringPool, str: []const u8) ![]const u8 {
    if (str.len > std.math.maxInt(StringLenType)) {
        return error.StringLengthTooLong;
    }

    const hash: u64 = hashString(str);

    // alignment requirements for StringLenType may require the buffer to be 1 byte larger than string size + sizeOf(StringLenType)
    // so take care not to include the final byte in the string + size byte buffer
    const string_and_size_num_bytes: usize = str.len + @sizeOf(StringLenType);
    const alloc_size = std.mem.alignForward(usize, string_and_size_num_bytes, @alignOf(StringLenType));
    const str_offset_begin: usize = self.buffer.items.len;
    const str_offset_end: usize = str_offset_begin + string_and_size_num_bytes;
    const aligned_buffer_end: usize = str_offset_begin + alloc_size;

    try self.buffer.resize(aligned_buffer_end);
    try self.lookup.put(hash, str_offset_begin);

    var bytes: []u8 = self.buffer.items[str_offset_begin..str_offset_end];
    const str_len: *StringLenType = @alignCast(@ptrCast(bytes.ptr));
    str_len.* = @as(StringLenType, @intCast(str.len));
    const str_bytes: []u8 = bytes[@sizeOf(StringLenType)..];
    @memcpy(str_bytes, str);

    return str_bytes;
}

pub fn find(self: *StringPool, str: []const u8) ?[]const u8 {
    const hash: u64 = hashString(str);

    if (self.lookup.get(hash)) |string_bytes_begin| {
        var str_bytes: [*]u8 = self.buffer.items[string_bytes_begin..].ptr;
        const str_len: *StringLenType = @alignCast(@ptrCast(str_bytes));
        const pooled_str: []u8 = str_bytes[@sizeOf(StringLenType) .. @sizeOf(StringLenType) + str_len.*];
        return pooled_str;
    }

    return null;
}

pub fn findOrPut(self: *StringPool, str: []const u8) ![]const u8 {
    if (self.find(str)) |found| {
        return found;
    }

    return try self.put(str);
}

test "basic" {
    const test_str: []const u8 = "test";
    const test1_str: []const u8 = "test";
    const test2_str: []const u8 = "test2";
    const long_str: []const u8 = "a very long string that has no end repeated many times! a very long string that has no end repeated many times! a very long string that has no end repeated many times!";

    var pool = StringPool.init(4096, std.testing.allocator);
    defer pool.deinit();

    const test_str_added = try pool.put(test_str);
    const test1_str_added = try pool.put(test1_str);
    const test2_str_added = try pool.put(test2_str);
    const long_str_added = try pool.put(long_str);

    try std.testing.expect(test_str_added.ptr != test_str.ptr);
    try std.testing.expect(test1_str_added.ptr != test1_str.ptr);
    try std.testing.expect(test2_str_added.ptr != test2_str.ptr);
    try std.testing.expect(long_str_added.ptr != long_str.ptr);

    const test_str_found = pool.find(test_str);
    const test1_str_found = pool.find(test1_str);
    const test2_str_found = pool.find(test2_str);
    const long_str_found = pool.find(long_str);

    try std.testing.expect(test_str_found != null);
    try std.testing.expect(test1_str_found != null);
    try std.testing.expect(test2_str_found != null);
    try std.testing.expect(long_str_found != null);

    try std.testing.expect(test_str_found.?.ptr != test_str.ptr);
    try std.testing.expect(test1_str_found.?.ptr != test1_str.ptr);
    try std.testing.expect(test2_str_found.?.ptr != test2_str.ptr);
    try std.testing.expect(long_str_found.?.ptr != long_str.ptr);

    std.debug.print("found: {s}, existing: {s}\n", .{ test_str_found.?, test_str });

    try std.testing.expect(std.mem.eql(u8, test_str_found.?, test_str));
    try std.testing.expect(std.mem.eql(u8, test1_str_found.?, test1_str));
    try std.testing.expect(std.mem.eql(u8, test2_str_found.?, test2_str));
    try std.testing.expect(std.mem.eql(u8, long_str_found.?, long_str));

    const lazyadd_str1 = try pool.findOrPut("lazy put");
    const lazyadd_str2 = try pool.findOrPut("lazy put");
    try std.testing.expect(lazyadd_str1.ptr == lazyadd_str2.ptr);
}
