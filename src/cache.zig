const std = @import("std");
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const assert = std.debug.assert;

const Package = base.Package;

const file_ext = ".rcir";

pub const CacheError = error{
    PartialRead,
    InvalidChecksum,
};

pub const CacheHeader = struct {
    total_cached_bytes: u32,
    checksum: u32,

    pub fn initFromBytes(buf: []align(@alignOf(CacheHeader)) u8) CacheError!*CacheHeader {
        if (buf.len < @sizeOf(CacheHeader)) {
            return CacheError.PartialRead;
        }

        const header = @as(*CacheHeader, @ptrCast(buf.ptr));

        if (buf.len < @sizeOf(CacheHeader) + header.total_cached_bytes) {
            return CacheError.PartialRead;
        }

        // Validate checksum on the data portion (excluding the header)
        const data_start = @sizeOf(CacheHeader);
        const data_bytes = buf[data_start .. data_start + header.total_cached_bytes];
        const computed_checksum = computeChecksum(data_bytes);

        if (computed_checksum != header.checksum) {
            return CacheError.InvalidChecksum;
        }

        return header;
    }

    // Simple Adler-32 checksum calculation
    fn computeChecksum(data: []const u8) u32 {
        var a: u32 = 1;
        var b: u32 = 0;
        const MOD: u32 = 65521; // Largest prime number less than 2^16

        for (data) |byte| {
            a = (a + byte) % MOD;
            b = (b + a) % MOD;
        }

        return (b << 16) | a;
    }
};

/// Reads the canonical IR for a given file hash and Roc version into the given buffer.
///
/// If this succeeds, then it's the caller's responsibility to:
/// - Verify that there are bytes left over in the buffer. (If the buffer is now full,
///   then this was a partial read and the caller needs to call this again with a bigger buffer).
/// - Cast the bytes to a CacheHeader
/// - Truncate the buffer's length based on the total_cached_bytes field of the CacheHeader.
///
/// Returns the number of bytes read or an error if file operations fail.
pub fn readCacheInto(buf: []align(@alignOf(CacheHeader)) u8, abs_cache_dir: []const u8, roc_version: []const u8, file_hash: []const u8) !usize {
    // Get the full path, e.g. "/path/to/roc/cache/0.1.0/abc12345.rcir"
    assert(std.fs.path.isAbsolute(abs_cache_dir));
    const sep = std.fs.path.sep;
    const extra_len = file_ext.len + (2 * std.mem.len(&[_]u8{sep}));
    var path_buf: [std.mem.len(abs_cache_dir) + std.mem.len(roc_version) + std.mem.len(file_hash) + extra_len]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}{c}{s}{c}{s}{s}", .{ abs_cache_dir, sep, roc_version, sep, file_hash, file_ext }) catch unreachable;

    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    return try file.readAll(buf);
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator) []const u8 {
    _ = url_data;
    _ = gpa;

    @panic("not implemented");
}

test "CacheHeader.initFromBytes - valid data" {
    // Create a buffer with a valid header and data
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    var buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));
    header.total_cached_bytes = test_data_len;

    // Copy test data after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(buffer[data_start .. data_start + test_data_len], test_data);

    // Calculate and set the checksum
    header.checksum = CacheHeader.computeChecksum(buffer[data_start .. data_start + test_data_len]);

    // Test initFromBytes
    const parsed_header = try CacheHeader.initFromBytes(&buffer);

    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);
    try std.testing.expectEqual(header.checksum, parsed_header.checksum);
}

test "CacheHeader.initFromBytes - buffer too small" {
    // Create a buffer smaller than CacheHeader size
    var small_buffer: [4]u8 align(@alignOf(CacheHeader)) = undefined;

    // Test that it returns PartialRead error
    const result = CacheHeader.initFromBytes(&small_buffer);
    try std.testing.expectError(CacheError.PartialRead, result);
}

test "CacheHeader.initFromBytes - insufficient data bytes" {
    // Create a buffer with a header but insufficient data bytes
    var buffer: [128]u8 align(@alignOf(CacheHeader)) = undefined;
    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));
    header.total_cached_bytes = 100; // Claim more bytes than are available

    // Test that it returns PartialRead error
    const result = CacheHeader.initFromBytes(&buffer);
    try std.testing.expectError(CacheError.PartialRead, result);
}

test "CacheHeader.initFromBytes - invalid checksum" {
    // Create a buffer with a valid header but invalid checksum
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    var buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));
    header.total_cached_bytes = test_data_len;

    // Copy test data after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(buffer[data_start .. data_start + test_data_len], test_data);

    // Set an incorrect checksum
    header.checksum = 0xDEADBEEF;

    // Test that it returns InvalidChecksum error
    const result = CacheHeader.initFromBytes(&buffer);
    try std.testing.expectError(CacheError.InvalidChecksum, result);
}

test "readCacheInto and initFromBytes integration" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    const roc_version = "0.1.0";
    const file_hash = "abc123456";

    // Create the version subdirectory
    try tmp_dir.dir.makePath(roc_version);

    // Create test data
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    // Create a buffer for header and data
    var write_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    var header = @as(*CacheHeader, @ptrCast(&write_buffer[0]));
    header.total_cached_bytes = test_data_len;

    // Copy test data after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(write_buffer[data_start .. data_start + test_data_len], test_data);

    // Calculate and set the checksum
    header.checksum = CacheHeader.computeChecksum(write_buffer[data_start .. data_start + test_data_len]);

    // Construct the cache file path
    const sep = std.fs.path.sep;
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try std.fmt.bufPrint(&path_buf, "{s}{c}{s}{c}{s}{s}", .{ abs_cache_dir, sep, roc_version, sep, file_hash, file_ext });

    // Create the cache file and write the data
    const file = try std.fs.createFileAbsolute(path, .{});
    defer file.close();
    try file.writeAll(write_buffer[0 .. data_start + test_data_len]);

    // Test readCacheInto
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, abs_cache_dir, roc_version, file_hash);

    try std.testing.expect(bytes_read >= @sizeOf(CacheHeader));

    // Test initFromBytes with the read data
    const parsed_header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);

    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);
    try std.testing.expectEqual(header.checksum, parsed_header.checksum);
}

test "readCacheInto - file not found" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    const roc_version = "0.1.0";
    const file_hash = "nonexistent";

    // Test readCacheInto with a nonexistent file
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const result = readCacheInto(&read_buffer, abs_cache_dir, roc_version, file_hash);

    try std.testing.expectError(error.FileNotFound, result);
}
