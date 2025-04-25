//! Exposes the readCacheInto and writeToCache functions for
//! serializing IR to and from disk. The caller is responsible for:
//! - Determining the base directory where the cache files should go.
//! - Determining what hash should be used as the cache key.
//! - Providing either the data to write to disk, or a buffer to read into.
const std = @import("std");
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const assert = std.debug.assert;
const Filesystem = @import("coordinate/Filesystem.zig");
const Package = base.Package;

const hash_encoder = std.base64.url_safe_no_pad.Encoder;
const file_ext = ".rcir";

/// An error when trying to read a file from cache.
/// The path (after appending the cache filename to the base dir)
/// can be too long for the OS, or the provided buffer to read
/// into can be too small, resuling in a partial read.
pub const CacheError = error{
    PartialRead,
    PathTooLong,
    OutOfMemory,
};

/// The header that gets written to disk right before the cached data.
/// Having this header makes it possible to read the entire cached file
/// into a buffer in one syscall, because the header provides all the
/// information necessary to process the remainder of the information
/// (e.g. rehydrating pointers).
pub const CacheHeader = struct {
    total_cached_bytes: u32,

    /// Verify that the given buffer begins with a valid CacheHeader,
    /// and also that it has a valid number of bytes in it. Returns
    /// a pointer to the CacheHeader within the buffer.
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
    allocator: std.mem.Allocator,
) (CacheError || Filesystem.OpenError || Filesystem.ReadError)!usize {
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
    allocator: std.mem.Allocator,
) (CacheError || Filesystem.WriteError || Filesystem.MakePathError)!void {
    // Create cache path using an allocator
    const cache_path = try createCachePath(allocator, cache_dir_path, hash);
    defer allocator.free(cache_path.path);

    // Create enclosing directories as needed.
    const hash_start = cache_dir_path.len + 1; // +1 for path separator
    const hash_sep_pos = hash_start + cache_path.half_encoded_len;
    try fs.makePath(cache_path.path[0..hash_sep_pos]);

    // Write both the header and the cached data to the file
    const header_size = @sizeOf(CacheHeader);
    const total_bytes = header_size + header.total_cached_bytes;

    // Create a slice that covers both the header and the data after it
    const bytes_to_write = @as([*]const u8, @ptrCast(header))[0..total_bytes];
    try fs.writeFile(cache_path.path, bytes_to_write);
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator, fs: Filesystem) []const u8 {
    _ = url_data;
    _ = gpa;
    _ = fs;

    @panic("not implemented");
}

/// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []const u8, roc_version: []const u8, fs: Filesystem, allocator: std.mem.Allocator) ?canonicalize.IR {
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
fn createCachePath(allocator: std.mem.Allocator, abs_cache_dir: []const u8, hash: []const u8) CacheError!struct { path: [:0]u8, half_encoded_len: usize } {
    // Calculate required space: abs_cache_dir + "/" + hash_path + file_ext + null terminator
    // We need hash_encoder.calcSize(hash.len) + 1 bytes for the hash path (+1 for the separator)
    const required_bytes = abs_cache_dir.len + 1 + hash_encoder.calcSize(hash.len) + 1 + file_ext.len + 1;

    // Allocate buffer with null terminator
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
    // The buffer already has a null terminator from allocSentinel

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
    try std.testing.expectError(CacheError.PartialRead, result);
}

test "CacheHeader.initFromBytes - insufficient data bytes" {
    var buffer: [128]u8 align(@alignOf(CacheHeader)) = .{0} ** 128;

    var header = @as(*CacheHeader, @ptrCast(&buffer[0]));

    // Set header to request more data than is available in the buffer
    const available_data_space = buffer.len - @sizeOf(CacheHeader);
    header.total_cached_bytes = available_data_space + 1;

    const result = CacheHeader.initFromBytes(&buffer);
    try std.testing.expectError(CacheError.PartialRead, result);
}

test "readCacheInto and initFromBytes integration" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Setup a mock filesystem for testing
    const mock_fs = Filesystem.default();

    const file_hash = "abc123456";
    const test_data = "This is test data for our cache!";
    const test_data_len = test_data.len;

    var write_buffer: [1024]u8 align(@alignOf(CacheHeader)) = .{0} ** 1024;

    var header = @as(*CacheHeader, @ptrCast(&write_buffer[0]));
    header.total_cached_bytes = test_data_len;

    const data_start = @sizeOf(CacheHeader);
    @memcpy(write_buffer[data_start .. data_start + test_data_len], test_data);

    // Create the path for the hash
    const path_result = try createCachePath(allocator, abs_cache_dir, file_hash);
    defer allocator.free(path_result.path);

    // Extract just the hash path portion
    const hash_start = abs_cache_dir.len + 1;
    const hash_path = path_result.path[hash_start .. hash_start + (path_result.half_encoded_len * 2) + 1];

    // Create parent directory for the hash path
    var sep_pos: usize = 0;
    while (sep_pos < hash_path.len) : (sep_pos += 1) {
        if (hash_path[sep_pos] == std.fs.path.sep) {
            break;
        }
    }
    try tmp_dir.dir.makePath(hash_path[0..sep_pos]);

    const file_path = try std.fs.path.join(allocator, &.{
        abs_cache_dir,
        hash_path,
    });
    const full_path = try std.fmt.allocPrint(allocator, "{s}{s}", .{ file_path, file_ext });

    const file = try std.fs.createFileAbsolute(full_path, .{});
    defer file.close();
    try file.writeAll(write_buffer[0 .. data_start + test_data_len]);

    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, abs_cache_dir, file_hash, mock_fs, allocator);

    try std.testing.expect(bytes_read >= @sizeOf(CacheHeader));

    const parsed_header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);
    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);

    const expected_total_bytes = @sizeOf(CacheHeader) + parsed_header.total_cached_bytes;
    try std.testing.expectEqual(expected_total_bytes, bytes_read);
}

test "readCacheInto - file not found" {
    // Create a simple mock filesystem for testing that will always return FileNotFound
    const mock_fs = Filesystem{
        .fileExists = struct {
            fn fileExists(absolute_path: []const u8) Filesystem.OpenError!bool {
                _ = absolute_path;
                return false;
            }
        }.fileExists,
        .readFile = struct {
            fn readFile(path: []const u8, alloc: std.mem.Allocator) Filesystem.ReadError![]const u8 {
                _ = path;
                _ = alloc;
                return error.FileNotFound;
            }
        }.readFile,
        .readFileInto = struct {
            fn readFileInto(path: []const u8, buf: []u8) Filesystem.ReadError!usize {
                _ = path;
                _ = buf;
                return error.FileNotFound;
            }
        }.readFileInto,
        .writeFile = struct {
            fn writeFile(path: []const u8, contents: []const u8) Filesystem.WriteError!void {
                _ = path;
                _ = contents;
                return;
            }
        }.writeFile,
        .openDir = Filesystem.default().openDir,
        .dirName = Filesystem.default().dirName,
        .baseName = Filesystem.default().baseName,
        .canonicalize = Filesystem.default().canonicalize,
        .makePath = struct {
            fn makePath(path: []const u8) Filesystem.MakePathError!void {
                _ = path;
                return;
            }
        }.makePath,
    };

    const abs_cache_dir = "/mock/cache/dir";
    const file_hash = "nonexistent";

    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const result = readCacheInto(&read_buffer, abs_cache_dir, file_hash, mock_fs, std.testing.allocator);

    try std.testing.expectError(error.FileNotFound, result);
}

test "writeToCache and readCacheInto integration" {
    // Use the allocator for all our memory needs
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create storage for data that will be shared between test functions
    const TestContext = struct {
        // Pointer to the data, static for this test case
        var data: ?[]u8 = null;
        var path: ?[]u8 = null;
        var alloc: ?std.mem.Allocator = null;

        pub fn init(a: std.mem.Allocator) void {
            alloc = a;
        }

        pub fn deinit() void {
            if (data) |d| alloc.?.free(d);
            if (path) |p| alloc.?.free(p);
            data = null;
            path = null;
        }
    };

    TestContext.init(allocator);
    defer TestContext.deinit();

    // Mock filesystem implementation
    const test_fs = Filesystem{
        .fileExists = struct {
            fn fileExists(path: []const u8) Filesystem.OpenError!bool {
                _ = path;
                return true;
            }
        }.fileExists,

        .readFile = struct {
            fn readFile(path: []const u8, alloc: std.mem.Allocator) Filesystem.ReadError![]const u8 {
                _ = path;
                if (TestContext.data) |data| {
                    return alloc.dupe(u8, data);
                } else {
                    return error.FileNotFound;
                }
            }
        }.readFile,

        .readFileInto = struct {
            fn readFileInto(path: []const u8, buffer: []u8) Filesystem.ReadError!usize {
                _ = path;
                if (TestContext.data) |data| {
                    const bytes_to_copy = @min(buffer.len, data.len);
                    @memcpy(buffer[0..bytes_to_copy], data[0..bytes_to_copy]);
                    return bytes_to_copy;
                } else {
                    return error.FileNotFound;
                }
            }
        }.readFileInto,

        .writeFile = struct {
            fn writeFile(path: []const u8, contents: []const u8) Filesystem.WriteError!void {
                // Free existing data
                if (TestContext.path != null) TestContext.alloc.?.free(TestContext.path.?);
                if (TestContext.data != null) TestContext.alloc.?.free(TestContext.data.?);

                // Store new copies
                TestContext.path = TestContext.alloc.?.dupe(u8, path) catch unreachable;
                TestContext.data = TestContext.alloc.?.dupe(u8, contents) catch unreachable;
            }
        }.writeFile,

        .openDir = Filesystem.default().openDir,
        .dirName = Filesystem.default().dirName,
        .baseName = Filesystem.default().baseName,
        .canonicalize = Filesystem.default().canonicalize,

        .makePath = struct {
            fn makePath(path: []const u8) Filesystem.MakePathError!void {
                _ = path;
                // Just pretend we successfully made the directory
            }
        }.makePath,
    };

    // Create a hash for our test file
    const file_hash = "testHash12345";
    const abs_cache_dir = "/mock/cache/dir";

    // Create test data and header
    const test_data = "This is test data to write to and read from cache!";
    const test_data_len = test_data.len;

    // Allocate buffer large enough for header + data
    const buffer_size = @sizeOf(CacheHeader) + test_data_len;
    var write_buffer: []align(@alignOf(CacheHeader)) u8 = try allocator.alignedAlloc(u8, @alignOf(CacheHeader), buffer_size);
    defer allocator.free(write_buffer);

    // Setup the header at the beginning of the buffer
    var header = @as(*CacheHeader, @ptrCast(write_buffer.ptr));
    header.total_cached_bytes = test_data_len;

    // Copy the test data right after the header
    const data_start = @sizeOf(CacheHeader);
    @memcpy(write_buffer[data_start .. data_start + test_data_len], test_data);

    // Use the writeToCache function
    try writeToCache(abs_cache_dir, file_hash, header, test_fs, allocator);

    // Verify a file path was written
    try std.testing.expect(TestContext.path != null);
    try std.testing.expect(TestContext.data != null);

    // Verify the path contains our cache directory and hash
    try std.testing.expect(std.mem.indexOf(u8, TestContext.path.?, abs_cache_dir) != null);

    // Now read the data back using readCacheInto
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, abs_cache_dir, file_hash, test_fs, allocator);

    // Verify header was read correctly
    try std.testing.expect(bytes_read >= @sizeOf(CacheHeader));
    const parsed_header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);
    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);

    // Calculate the expected number of bytes we should have read
    const expected_total_bytes = @sizeOf(CacheHeader) + parsed_header.total_cached_bytes;
    try std.testing.expectEqual(expected_total_bytes, bytes_read);

    // Verify the data content
    const data_bytes = read_buffer[@sizeOf(CacheHeader)..expected_total_bytes];
    try std.testing.expectEqualStrings(test_data, data_bytes);

    // Now test with different data - making sure the file gets updated
    const test_data2 = "This is updated test data for writeToCache!";
    const test_data_len2 = test_data2.len;

    var write_buffer2: []align(@alignOf(CacheHeader)) u8 = try allocator.alignedAlloc(u8, @alignOf(CacheHeader), @sizeOf(CacheHeader) + test_data_len2);
    defer allocator.free(write_buffer2);

    var header2 = @as(*CacheHeader, @ptrCast(write_buffer2.ptr));
    header2.total_cached_bytes = test_data_len2;

    @memcpy(write_buffer2[@sizeOf(CacheHeader) .. @sizeOf(CacheHeader) + test_data_len2], test_data2);

    // Use a different hash for the second test
    const file_hash2 = "testWriteToCache";

    // Use writeToCache this time
    try writeToCache(abs_cache_dir, file_hash2, header2, test_fs, allocator);

    // Read it back
    var read_buffer2: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read2 = try readCacheInto(&read_buffer2, abs_cache_dir, file_hash2, test_fs, allocator);

    // Verify header
    try std.testing.expect(bytes_read2 >= @sizeOf(CacheHeader));
    const parsed_header2 = try CacheHeader.initFromBytes(read_buffer2[0..bytes_read2]);
    try std.testing.expectEqual(header2.total_cached_bytes, parsed_header2.total_cached_bytes);

    // Verify data
    const expected_total_bytes2 = @sizeOf(CacheHeader) + parsed_header2.total_cached_bytes;
    try std.testing.expectEqual(expected_total_bytes2, bytes_read2);

    const data_bytes2 = read_buffer2[@sizeOf(CacheHeader)..expected_total_bytes2];
    try std.testing.expectEqualStrings(test_data2, data_bytes2);
}
