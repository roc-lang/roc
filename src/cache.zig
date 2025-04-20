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

const hash_encoder = std.base64.url_safe_no_pad.Encoder;

/// Takes the given hash bytes, base64-url encodes them,
/// and writes them to the given buffer with a directory
/// separator in the middle (to avoid one giant cache dir.)
///
/// Returns the number of bytes written to `out`.
fn writeHashToPath(hash: []const u8, out: []u8) usize {
    const half_hash_len = hash.len / 2;
    const half_encoded_len = hash_encoder.calcSize(half_hash_len);

    assert(out.len >= bytesNeededToHashPath(hash));

    // Encode the bytes with a path separator in the middle.
    _ = hash_encoder.encode(out[0..half_encoded_len], hash[0..half_hash_len]);
    out[half_encoded_len] = std.fs.path.sep;
    _ = hash_encoder.encode(out[half_encoded_len + 1 ..], hash[half_hash_len..hash.len]);

    return (half_encoded_len * 2) + 1;
}

fn bytesNeededToHashPath(hash: []const u8) usize {
    // We split the input bytes in half and write a path separator,
    // so we need +1 to account for the separator.
    return hash_encoder.calcSize(hash.len) + 1;
}

pub const CacheHeader = struct {
    total_cached_bytes: u32,
    data_checksum: u32,

    pub fn initFromBytes(buf: []align(@alignOf(CacheHeader)) u8) CacheError!*CacheHeader {
        if (buf.len == 0) {
            return CacheError.PartialRead;
        }

        // The buffer might not contain a complete header.
        if (buf.len < @sizeOf(CacheHeader)) {
            return CacheError.PartialRead;
        }

        const header = @as(*CacheHeader, @ptrCast(buf.ptr));
        const data_start = @sizeOf(CacheHeader);
        const data_end = data_start + header.total_cached_bytes;

        // The buffer might not contain complete data after the header.
        if (buf.len < data_end) {
            return CacheError.PartialRead;
        }

        // The data in the buffer might be corrupted (as in, doesn't pass checksum)
        if (checksum(buf[data_start..data_end]) != header.data_checksum) {
            return CacheError.InvalidChecksum;
        }

        return header;
    }

    // Simple Adler-32 checksum calculation
    fn checksum(data: []const u8) u32 {
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
pub fn readCacheInto(
    buf: []align(@alignOf(CacheHeader)) u8,
    abs_cache_dir: []const u8,
    file_hash: []const u8,
) !usize {
    // Get the full path, e.g. "/path/to/roc/cache/0.1.0/abc12345.rcir"
    assert(std.fs.path.isAbsolute(abs_cache_dir));

    // +1 for the separator between the cache dir and the hash.
    const path_len = abs_cache_dir.len + bytesNeededToHashPath(file_hash) + file_ext.len + 1;
    var path: [path_len:0]u8 = undefined;

    // abs_cache_dir + "/" + first_half_of_hash + "/" + second_half_of_hash + file_ext
    @memcpy(path[0..abs_cache_dir.len], abs_cache_dir);
    path[abs_cache_dir.len] = std.fs.path.sep;
    const hash_start = abs_cache_dir.len + 1; // +1 for the path separator.
    const hash_len = writeHashToPath(file_hash, path[hash_start..]);
    @memcpy(path[hash_start + hash_len ..], file_ext);
    path[path_len - 1] = 0; // Null-terminate

    const file = try std.fs.openFileAbsoluteZ(path, .{});
    defer file.close();

    return try file.readAll(buf);
}

/// Joins path components with separators for the cache file path
/// Writes the path to the provided buffer and returns a slice of the result
fn joinPath(buf: []u8, abs_dir: []const u8, version: []const u8, hash: []const u8, ext: []const u8) ![]const u8 {
    const sep = std.fs.path.sep;

    // Ensure we don't overflow the buffer
    const needed_len = abs_dir.len + 1 + version.len + 1 + hash.len + ext.len;
    if (needed_len > buf.len) {
        return error.PathTooLong;
    }

    // Copy each component with separators
    var index: usize = 0;

    @memcpy(buf[index..][0..abs_dir.len], abs_dir);
    index += abs_dir.len;

    buf[index] = sep;
    index += 1;

    @memcpy(buf[index..][0..version.len], version);
    index += version.len;

    buf[index] = sep;
    index += 1;

    @memcpy(buf[index..][0..hash.len], hash);
    index += hash.len;

    @memcpy(buf[index..][0..ext.len], ext);
    index += ext.len;

    return buf[0..index]; // Return a slice of exactly the right length
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator) []const u8 {
    _ = url_data;
    _ = gpa;

    @panic("not implemented");
}

test "CacheHeader memory layout" {
    std.debug.print("\nCacheHeader size: {}\n", .{@sizeOf(CacheHeader)});
    std.debug.print("CacheHeader alignment: {}\n", .{@alignOf(CacheHeader)});

    std.debug.print("total_cached_bytes offset: {}\n", .{@offsetOf(CacheHeader, "total_cached_bytes")});
    std.debug.print("data_checksum offset: {}\n", .{@offsetOf(CacheHeader, "data_checksum")});
}

test "CacheHeader.initFromBytes - valid data" {
    // Create a buffer with a valid header and data
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    var buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    // Zero out the buffer to ensure predictable content
    @memset(buffer[0..], 0);

    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));
    header.total_cached_bytes = test_data_len;

    // Copy test data after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(buffer[data_start .. data_start + test_data_len], test_data);

    // Calculate and set the checksum
    header.data_checksum = CacheHeader.checksum(buffer[data_start .. data_start + test_data_len]);

    // Test initFromBytes
    const parsed_header = try CacheHeader.initFromBytes(&buffer);

    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);
    try std.testing.expectEqual(header.data_checksum, parsed_header.data_checksum);
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
    // Zero out the buffer to ensure predictable content
    @memset(buffer[0..], 0);

    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));

    // The buffer size is 128 bytes, so any cached_bytes value larger than (128 - @sizeOf(CacheHeader))
    // should trigger the PartialRead error
    const available_data_space = buffer.len - @sizeOf(CacheHeader);
    header.total_cached_bytes = available_data_space + 1; // One byte more than available

    // Test that it returns PartialRead error
    const result = CacheHeader.initFromBytes(&buffer);
    try std.testing.expectError(CacheError.PartialRead, result);
}

test "CacheHeader.initFromBytes - invalid checksum" {
    // Create a buffer with a valid header but invalid checksum
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    var buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    // Zero out the buffer to ensure predictable content
    @memset(buffer[0..], 0);

    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));
    header.total_cached_bytes = test_data_len;

    // Copy test data after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(buffer[data_start .. data_start + test_data_len], test_data);

    // Set an incorrect checksum
    header.data_checksum = 0xDEADBEEF;

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
    // Zero out the buffer to ensure predictable content
    @memset(write_buffer[0..], 0);

    var header = @as(*CacheHeader, @ptrCast(&write_buffer[0]));
    header.total_cached_bytes = test_data_len;

    // Copy test data after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(write_buffer[data_start .. data_start + test_data_len], test_data);

    // Calculate and set the checksum
    header.data_checksum = CacheHeader.checksum(write_buffer[data_start .. data_start + test_data_len]);

    // Construct the cache file path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try joinPath(&path_buf, abs_cache_dir, roc_version, file_hash, file_ext);

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
    try std.testing.expectEqual(header.data_checksum, parsed_header.data_checksum);
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
