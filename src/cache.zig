//! Exposes the readCacheInto and writeToCache functions for
//! serializing IR to and from disk. The caller is responsible for:
//! - Determining the base directory where the cache files should go.
//! - Determining what hash should be used as the cache key.
//! - Providing either the data to write to disk, or a buffer to read into.
const std = @import("std");
const builtin = @import("builtin");
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const assert = std.debug.assert;
const Filesystem = @import("coordinate/Filesystem.zig");
const Package = base.Package;
const Allocator = std.mem.Allocator;

const hash_encoder = std.base64.url_safe_no_pad.Encoder;
const file_ext = ".rcir";

/// The header that gets written to disk right before the cached data.
/// Having this header makes it possible to read the entire cached file
/// into a buffer in one syscall, because the header provides all the
/// information necessary to process the remainder of the information
/// (e.g. rehydrating pointers).
pub const CacheHeader = struct {
    total_cached_bytes: u32,

    /// Error specific to initializing a CacheHeader from bytes.
    /// Returned when the buffer is too small to contain a complete header
    /// or the complete data that the header specifies.
    pub const InitError = error{
        PartialRead,
    };

    /// Verify that the given buffer begins with a valid CacheHeader,
    /// and also that it has a valid number of bytes in it. Returns
    /// a pointer to the CacheHeader within the buffer.
    pub fn initFromBytes(buf: []align(@alignOf(CacheHeader)) u8) InitError!*CacheHeader {
        if (buf.len == 0) {
            return InitError.PartialRead;
        }

        // The buffer might not contain a complete header.
        if (buf.len < @sizeOf(CacheHeader)) {
            return InitError.PartialRead;
        }

        const header = @as(*CacheHeader, @ptrCast(buf.ptr));
        const data_start = @sizeOf(CacheHeader);
        const data_end = data_start + header.total_cached_bytes;

        // The buffer might not contain complete data after the header.
        if (buf.len < data_end) {
            return InitError.PartialRead;
        }

        return header;
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
    dest: []align(@alignOf(CacheHeader)) u8,
    abs_cache_dir: []const u8,
    hash: []const u8,
    fs: Filesystem,
    allocator: Allocator,
) (Filesystem.ReadError || Allocator.Error)!usize {
    const path_result = try createCachePath(allocator, abs_cache_dir, hash);
    defer allocator.free(path_result.path);
    return try fs.readFileInto(path_result.path, dest);
}

/// Writes the given content to a cache file for the specified hash.
/// Creates any missing intermediate directories as necessary.
pub fn writeToCache(
    cache_dir_path: []const u8,
    hash: []const u8,
    header: *const CacheHeader, // Must be followed in memory by the contents of the header
    fs: Filesystem,
    allocator: Allocator,
) (Filesystem.WriteError || Filesystem.MakePathError || Allocator.Error)!void {
    const cache_path = try createCachePath(allocator, cache_dir_path, hash);
    defer allocator.free(cache_path.path);

    // Create enclosing directories as needed.
    const hash_start = cache_dir_path.len + 1; // +1 for path separator
    const hash_sep_pos = hash_start + cache_path.half_encoded_len;
    try fs.makePath(cache_path.path[0..hash_sep_pos]);

    // Write to the file both the header and the cache data immediately following it in memory.
    const total_bytes = @sizeOf(CacheHeader) + header.total_cached_bytes;
    const header_and_content = @as([*]const u8, @ptrCast(header))[0..total_bytes];
    try fs.writeFile(cache_path.path, header_and_content);
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: Allocator, fs: Filesystem) []const u8 {
    _ = url_data;
    _ = gpa;
    _ = fs;

    @panic("not implemented");
}

/// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []const u8, roc_version: []const u8, fs: Filesystem, allocator: Allocator) ?canonicalize.IR {
    _ = file_hash;
    _ = roc_version;
    _ = fs;
    _ = allocator;
    return null;
}

/// Allocates and returns the full path to the cache file for the given hash.
/// Also returns the length of the hash path part.
///
/// The path format is: abs_cache_dir + "/" + first_half_of_hash + "/" + second_half_of_hash + file_ext
///
/// All other path-related values can be derived from the returned values.
///
/// Returns a tuple containing:
/// - The full path as a null-terminated string
/// - The hash path length
fn createCachePath(allocator: Allocator, abs_cache_dir: []const u8, hash: []const u8) Allocator.Error!struct { path: [:0]u8, half_encoded_len: usize } {
    // Calculate required space: abs_cache_dir + "/" + hash_path + file_ext + null terminator
    // We need hash_encoder.calcSize(hash.len) + 1 bytes for the hash path (+1 for the separator)
    const required_bytes = abs_cache_dir.len + 1 + hash_encoder.calcSize(hash.len) + 1 + file_ext.len + 1;

    var path_buf = try allocator.allocSentinel(u8, required_bytes - 1, 0);
    errdefer allocator.free(path_buf);

    // abs_cache_dir + "/" + first_half_of_hash + "/" + second_half_of_hash + file_ext
    @memcpy(path_buf[0..abs_cache_dir.len], abs_cache_dir);
    path_buf[abs_cache_dir.len] = std.fs.path.sep;
    const hash_start = abs_cache_dir.len + 1; // +1 for the path separator

    // Inline the writeHashToPath function here with the hash bytes split in half
    const half_hash_len = hash.len / 2;
    const half_encoded_len = hash_encoder.calcSize(half_hash_len);

    // Encode the first half of the hash
    _ = hash_encoder.encode(path_buf[hash_start .. hash_start + half_encoded_len], hash[0..half_hash_len]);

    // Add path separator
    path_buf[hash_start + half_encoded_len] = std.fs.path.sep;

    // Encode the second half of the hash
    _ = hash_encoder.encode(path_buf[hash_start + half_encoded_len + 1 ..], hash[half_hash_len..hash.len]);

    const hash_path_len = (half_encoded_len * 2) + 1;

    const ext_start = hash_start + hash_path_len;
    const ext_end = ext_start + file_ext.len;
    @memcpy(path_buf[ext_start..ext_end], file_ext);

    return .{ .path = path_buf, .half_encoded_len = half_encoded_len };
}

test "CacheHeader.initFromBytes - valid data" {
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    var buffer: [1024]u8 align(@alignOf(CacheHeader)) = .{0} ** 1024;

    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));
    header.total_cached_bytes = test_data_len;

    const data_start = @sizeOf(CacheHeader);
    @memcpy(buffer[data_start .. data_start + test_data_len], test_data);

    const parsed_header = try CacheHeader.initFromBytes(&buffer);
    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);
}

test "CacheHeader.initFromBytes - buffer too small" {
    // Create a buffer smaller than CacheHeader size
    var small_buffer: [4]u8 align(@alignOf(CacheHeader)) = undefined;

    // Test that it returns PartialRead error
    const result = CacheHeader.initFromBytes(&small_buffer);
    try std.testing.expectError(CacheHeader.InitError.PartialRead, result);
}

test "CacheHeader.initFromBytes - insufficient data bytes" {
    var buffer: [128]u8 align(@alignOf(CacheHeader)) = .{0} ** 128;

    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));

    // Set header to request more data than is available in the buffer
    const available_data_space = buffer.len - @sizeOf(CacheHeader);
    header.total_cached_bytes = available_data_space + 1;

    const result = CacheHeader.initFromBytes(&buffer);
    try std.testing.expectError(CacheHeader.InitError.PartialRead, result);
}

test "readCacheInto - file too big" {
    var mock_fs = Filesystem.testing();
    const err = error.FileTooBig;

    mock_fs.readFileInto = struct {
        fn readFileInto(path: []const u8, buf: []u8) Filesystem.ReadError!usize {
            _ = path;
            _ = buf;
            return err;
        }
    }.readFileInto;

    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const result = readCacheInto(&read_buffer, "/fake/cache/dir", "not-a-hash", mock_fs, std.testing.allocator);

    try std.testing.expectError(err, result);
}

test "readCacheInto after writeToCache" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get absolute path of tmp_dir to use as cache directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    const fs = Filesystem.default();
    const hash = "0123456789abcdef";
    const test_data = "Test data for caching!";
    const test_data_len = test_data.len;

    // Create buffer with header and data
    const buffer_size = @sizeOf(CacheHeader) + test_data_len;
    var write_buffer: []align(@alignOf(CacheHeader)) u8 = try std.testing.allocator.alignedAlloc(u8, @alignOf(CacheHeader), buffer_size);
    defer std.testing.allocator.free(write_buffer);
    var header = @as(*CacheHeader, @ptrCast(write_buffer.ptr));
    header.total_cached_bytes = test_data_len;
    const data_start = @sizeOf(CacheHeader);
    @memcpy(write_buffer[data_start .. data_start + test_data_len], test_data);

    // Write to cache
    try writeToCache(abs_cache_dir, hash, header, fs, std.testing.allocator);

    // Read it back
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, abs_cache_dir, hash, fs, std.testing.allocator);

    // Verify header was read correctly
    try std.testing.expect(bytes_read >= @sizeOf(CacheHeader));
    const parsed_header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);
    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);

    // Verify data was read correctly
    const expected_total_bytes = @sizeOf(CacheHeader) + parsed_header.total_cached_bytes;
    try std.testing.expectEqual(expected_total_bytes, bytes_read);

    const data_bytes = read_buffer[@sizeOf(CacheHeader)..expected_total_bytes];
    try std.testing.expectEqualStrings(test_data, data_bytes);
}
