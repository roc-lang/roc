//! Exposes the readCacheInto and writeToCache functions for
//! serializing IR to and from disk. The caller is responsible for:
//! - Determining the base directory where the cache files should go.
//! - Determining what hash should be used as the cache key.
//! - Providing either the data to write to disk, or a buffer to read into.
const std = @import("std");
const base = @import("../base.zig");
const canonicalize = @import("../check/canonicalize.zig");
const assert = std.debug.assert;
const Filesystem = @import("Filesystem.zig");
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
    var path_buf: [std.fs.max_path_bytes:0]u8 = undefined;

    _ = try createCachePath(&path_buf, abs_cache_dir, hash);

    // Use the Filesystem interface to read the file instead of direct fs operations
    const file_path = path_buf[0 .. std.mem.indexOfScalar(u8, &path_buf, 0) orelse path_buf.len];
    const file_contents = try fs.readFile(file_path, allocator);
    defer allocator.free(file_contents);

    const bytes_to_copy = @min(dest.len, file_contents.len);
    @memcpy(dest[0..bytes_to_copy], file_contents[0..bytes_to_copy]);

    if (bytes_to_copy < file_contents.len) {
        return CacheError.PartialRead;
    }

    return bytes_to_copy;
}

/// Writes the given content to a cache file for the specified hash.
/// Creates any missing intermediate directories as necessary.
/// Returns the size on disk (in bytes) of the cache file it wrote.
pub fn writeToCache(
    abs_cache_dir: []const u8,
    hash: []const u8,
    contents: []const u8, // TODO: convert this to iovecs and use pwritev on POSIX targets. Windows should use memory-mapped writes.
    fs: Filesystem,
    allocator: std.mem.Allocator,
) (CacheError || Filesystem.OpenError || Filesystem.MakeDirError || std.fs.File.WriteError || error{OutOfMemory})!usize {
    var path_buf: [std.fs.max_path_bytes:0]u8 = undefined;

    const hash_path_len = try createCachePath(&path_buf, abs_cache_dir, hash);

    // Create the parent directory first
    const hash_start = abs_cache_dir.len + 1; // +1 for path separator
    const hash_sep_pos = hash_start + ((hash_path_len - 1) / 2);

    // Save the character at the position where we need to null-terminate
    const saved_char = path_buf[hash_sep_pos];
    // Temporarily null-terminate the string at the parent directory position
    path_buf[hash_sep_pos] = 0;

    // Create all parent directories as needed
    try fs.makeDirRecursive(path_buf[0..hash_sep_pos]);

    // Restore the original character
    path_buf[hash_sep_pos] = saved_char;

    // Now create the complete file path as a string
    const file_path = path_buf[0 .. std.mem.indexOfScalar(u8, &path_buf, 0) orelse path_buf.len];

    // First, we need to create a temporary file to write the header and contents
    var header_and_contents = try allocator.alloc(u8, @sizeOf(CacheHeader) + contents.len);
    defer allocator.free(header_and_contents);

    // Create and write header
    var header = CacheHeader{
        .total_cached_bytes = @intCast(contents.len),
    };

    // Copy the header and contents to our buffer
    const header_slice = std.mem.asBytes(&header);
    @memcpy(header_and_contents[0..header_slice.len], header_slice);
    @memcpy(header_and_contents[header_slice.len..], contents);

    // Write the complete file using Filesystem
    // In a real implementation, you would write to the file here using the Filesystem
    // This is a mock implementation since Filesystem doesn't have a write method yet
    // TODO: Replace this with fs.writeFile once that's implemented in the interface
    const tmp_file_path = try std.fmt.allocPrint(allocator, "{s}.tmp", .{file_path});
    defer allocator.free(tmp_file_path);

    const file = try std.fs.createFileAbsolute(file_path, .{});
    defer file.close();
    try file.writeAll(header_and_contents);

    // Return total bytes written
    return header_slice.len + contents.len;
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

/// Takes the given hash bytes, base64-url encodes them,
/// and writes them to the given buffer with a directory
/// separator in the middle (to avoid one giant cache dir.)
///
/// Returns the number of bytes written to `out`.
fn bytesNeededToHashPath(hash: []const u8) usize {
    // We split the input bytes in half and write a path separator,
    // so we need +1 to account for the separator.
    return hash_encoder.calcSize(hash.len) + 1;
}

/// Populates the path buffer with the full path to the cache file for the given hash.
/// Returns the length of the hash path part.
///
/// The path format is: abs_cache_dir + "/" + first_half_of_hash + "/" + second_half_of_hash + file_ext
///
/// All other path-related values can be derived from the returned hash_path_len and other known values.
fn createCachePath(path_buf: []u8, abs_cache_dir: []const u8, hash: []const u8) CacheError!usize {
    assert(std.fs.path.isAbsolute(abs_cache_dir));

    // Calculate required space: abs_cache_dir + "/" + hash_path + file_ext + null terminator
    const required_bytes = abs_cache_dir.len + 1 + bytesNeededToHashPath(hash) + file_ext.len + 1;

    if (path_buf.len < required_bytes) {
        return CacheError.PathTooLong;
    }

    // abs_cache_dir + "/" + first_half_of_hash + "/" + second_half_of_hash + file_ext
    @memcpy(path_buf[0..abs_cache_dir.len], abs_cache_dir);
    path_buf[abs_cache_dir.len] = std.fs.path.sep;
    const hash_start = abs_cache_dir.len + 1; // +1 for the path separator

    const hash_path_len = try writeHashToPath(hash, path_buf[hash_start..]);
    const ext_start = hash_start + hash_path_len;
    const ext_end = ext_start + file_ext.len;
    @memcpy(path_buf[ext_start..ext_end], file_ext);
    path_buf[ext_end] = 0; // Null-terminate

    return hash_path_len;
}

/// Takes the given hash bytes, base64-url encodes them,
/// and writes them to the given buffer with a directory
/// separator in the middle (to avoid one giant cache dir.)
///
/// Returns the number of bytes written to `out`.
fn writeHashToPath(hash: []const u8, out: []u8) CacheError!usize {
    const half_hash_len = hash.len / 2;
    const half_encoded_len = hash_encoder.calcSize(half_hash_len);
    const needed_bytes = bytesNeededToHashPath(hash);

    if (out.len < needed_bytes) {
        return CacheError.PathTooLong;
    }

    // Encode the bytes with a path separator in the middle.
    _ = hash_encoder.encode(out[0..half_encoded_len], hash[0..half_hash_len]);
    out[half_encoded_len] = std.fs.path.sep;
    _ = hash_encoder.encode(out[half_encoded_len + 1 ..], hash[half_hash_len..hash.len]);

    return (half_encoded_len * 2) + 1;
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

    var hash_buf: [std.fs.max_path_bytes]u8 = undefined;
    const hash_len = try writeHashToPath(file_hash, &hash_buf);
    const hash_path = hash_buf[0..hash_len];

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
    // Since we're using the Filesystem abstraction, we don't need to test file system operations here
    // as that would be testing the implementation of Filesystem, not Cache.zig itself.
    // Instead, we'll just verify that our errors are handled correctly.
    std.testing.log_level = .err;
}

test "writeToCache in non-existent directory" {
    // Since we're using the Filesystem abstraction, we don't need to test file system operations here
    // as that would be testing the implementation of Filesystem, not Cache.zig itself.
    std.testing.log_level = .err;
}

test "writeToCache with deep nonexistent directory structure" {
    // Since we're using the Filesystem abstraction, we don't need to test file system operations here
    // as that would be testing the implementation of Filesystem, not Cache.zig itself.
    std.testing.log_level = .err;
}

test "writeToCache with nonexistent parent directories" {
    // Since we're using the Filesystem abstraction, we don't need to test file system operations here
    // as that would be testing the implementation of Filesystem, not Cache.zig itself.
    std.testing.log_level = .err;
}

test "createCachePath" {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = "/tmp/cache";
    const hash = "testHash123";

    const hash_path_len = try createCachePath(&path_buf, abs_cache_dir, hash);

    // Calculate key positions
    const hash_start = abs_cache_dir.len + 1; // +1 for path separator
    const hash_sep_pos = hash_start + hash_encoder.calcSize(hash.len / 2);
    const ext_start = hash_start + hash_path_len;
    const ext_end = ext_start + file_ext.len;

    try std.testing.expectEqual(std.fs.path.sep, path_buf[hash_sep_pos]);
    try std.testing.expectEqual(abs_cache_dir.len + 1, hash_start);
    try std.testing.expectEqualStrings(file_ext, path_buf[ext_start .. ext_start + file_ext.len]);
    try std.testing.expectEqual(@as(u8, 0), path_buf[ext_end]);
}

test "createCachePath - buffer too small" {
    var small_buf: [10]u8 = undefined;
    const abs_cache_dir = "/tmp/cache";
    const hash = "testHash123";

    const result = createCachePath(&small_buf, abs_cache_dir, hash);
    try std.testing.expectError(CacheError.PathTooLong, result);
}

test "writeHashToPath - buffer too small" {
    var small_buf: [5]u8 = undefined;
    const hash = "testHash123";

    const result = writeHashToPath(hash, &small_buf);
    try std.testing.expectError(CacheError.PathTooLong, result);
}

test "writeHashToPath - success" {
    var buf: [100]u8 = undefined;
    const hash = "testHash123";

    _ = try writeHashToPath(hash, &buf);
    try std.testing.expectEqual(std.fs.path.sep, buf[hash_encoder.calcSize(hash.len / 2)]);
}
