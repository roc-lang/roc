const std = @import("std");
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const assert = std.debug.assert;
const path_utils = @import("path.zig");
const Package = base.Package;

const hash_encoder = std.base64.url_safe_no_pad.Encoder;
const file_ext = ".rcir";

pub const CacheError = error{
    PartialRead,
    PathTooLong,
};

pub const CacheHeader = struct {
    total_cached_bytes: u32,

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
    buf: []align(@alignOf(CacheHeader)) u8,
    abs_cache_dir: []const u8,
    hash: []const u8,
) !usize {
    var path_buf: [std.fs.max_path_bytes:0]u8 = undefined;

    // Create the full cache path
    _ = try createCachePath(&path_buf, abs_cache_dir, hash);

    // Open and read the file
    const file = try std.fs.openFileAbsoluteZ(&path_buf, .{});
    defer file.close();

    return try file.readAll(buf);
}

/// Writes the given content to a cache file for the specified hash.
/// Creates any missing intermediate directories as necessary.
/// Returns the size on disk (in bytes) of the cache file it wrote.
pub fn writeToCache(
    abs_cache_dir: []const u8,
    hash: []const u8,
    contents: []const u8,
) !usize {
    var path_buf: [std.fs.max_path_bytes:0]u8 = undefined;

    // Create the full cache path
    const hash_path_len = try createCachePath(&path_buf, abs_cache_dir, hash);

    // First, try to create the file without creating directories
    const file = std.fs.createFileAbsoluteZ(&path_buf, .{}) catch |err| {
        // If the error indicates missing directories, try creating just the parent directory first
        if (err == error.FileNotFound) {
            const hash_start = abs_cache_dir.len + 1; // +1 for path separator
            const hash_sep_pos = hash_start + ((hash_path_len - 1) / 2);
            const parent_dir_path = path_buf[0..hash_sep_pos];

            std.fs.makeDirAbsolute(parent_dir_path) catch |parent_err| {
                // If the parent directory couldn't be created due to missing parent directories,
                // fall back to the recursive approach
                if (parent_err == error.FileNotFound) {
                    // If the parent directory couldn't be created, then we might be missing
                    // the entire cache dir. Recursively create all of them.
                    try path_utils.makeDirRecursive(parent_dir_path);
                } else if (parent_err != error.PathAlreadyExists) {
                    // If the parent directory already exists, that's fine.
                    // (It must have been created concurrently.)
                    //
                    // For any other error, propagate it.
                    return parent_err;
                }
            };

            // Try creating the file again now that directories exist
            const retry_file = try std.fs.createFileAbsoluteZ(&path_buf, .{});
            defer retry_file.close();

            return try writeCacheContents(retry_file, contents);
        } else {
            // For any other error besides missing directories, propagate it.
            return err;
        }
    };
    defer file.close();

    // Write the header and contents
    return try writeCacheContents(file, contents);
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator) []const u8 {
    _ = url_data;
    _ = gpa;

    @panic("not implemented");
}

/// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []const u8, roc_version: []const u8) ?canonicalize.IR {
    _ = file_hash;
    _ = roc_version;
    return null;
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

/// Writes the header and content to the provided file.
/// Returns the total number of bytes written (header + content).
fn writeCacheContents(
    file: std.fs.File,
    contents: []const u8,
) !usize {
    // Create and write header
    var header = CacheHeader{
        .total_cached_bytes = @intCast(contents.len),
    };

    // Write the header
    const header_slice = std.mem.asBytes(&header);
    try file.writeAll(header_slice);

    // Write the contents
    try file.writeAll(contents);

    // Return total bytes written
    return header_slice.len + contents.len;
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

    // Test initFromBytes
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

test "readCacheInto and initFromBytes integration" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Use a test allocator for path operations in the test
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file_hash = "abc123456";

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

    // Create the hash directory structure
    var hash_buf: [std.fs.max_path_bytes]u8 = undefined;
    const hash_len = try writeHashToPath(file_hash, &hash_buf);
    const hash_path = hash_buf[0..hash_len];

    // Create the directory structure for the hash
    // Find the position of the separator in the hash path
    var sep_pos: usize = 0;
    while (sep_pos < hash_path.len) : (sep_pos += 1) {
        if (hash_path[sep_pos] == std.fs.path.sep) {
            break;
        }
    }
    try tmp_dir.dir.makePath(hash_path[0..sep_pos]);

    // Create the full file path
    const file_path = try std.fs.path.join(allocator, &.{
        abs_cache_dir,
        hash_path,
    });
    const full_path = try std.fmt.allocPrint(allocator, "{s}{s}", .{ file_path, file_ext });

    // Create the cache file and write the data
    const file = try std.fs.createFileAbsolute(full_path, .{});
    defer file.close();
    try file.writeAll(write_buffer[0 .. data_start + test_data_len]);

    // Test readCacheInto
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, abs_cache_dir, file_hash);

    try std.testing.expect(bytes_read >= @sizeOf(CacheHeader));

    // Test initFromBytes with the read data
    const parsed_header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);

    try std.testing.expectEqual(header.total_cached_bytes, parsed_header.total_cached_bytes);

    // Verify that bytes read match expected total (header + data)
    const expected_total_bytes = @sizeOf(CacheHeader) + parsed_header.total_cached_bytes;
    try std.testing.expectEqual(expected_total_bytes, bytes_read);
}

test "readCacheInto - file not found" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    const file_hash = "nonexistent";

    // Test readCacheInto with a nonexistent file
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const result = readCacheInto(&read_buffer, abs_cache_dir, file_hash);

    try std.testing.expectError(error.FileNotFound, result);
}

test "writeToCache and readCacheInto integration" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    const file_hash = "def789012";
    const test_data = "This is data written by writeToCache!";

    // Write data to cache
    const bytes_written = try writeToCache(abs_cache_dir, file_hash, test_data);
    const expected_bytes = @sizeOf(CacheHeader) + test_data.len;
    try std.testing.expectEqual(expected_bytes, bytes_written);

    // Read data back from cache
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, abs_cache_dir, file_hash);

    // Verify bytes read matches bytes written
    try std.testing.expectEqual(bytes_written, bytes_read);

    // Parse the header from read buffer
    const header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);
    try std.testing.expectEqual(@as(u32, test_data.len), header.total_cached_bytes);

    // Verify the data content
    const data_start = @sizeOf(CacheHeader);
    const read_data = read_buffer[data_start..bytes_read];
    try std.testing.expectEqualStrings(test_data, read_data);
}

test "writeToCache in non-existent directory" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create a deeper path that doesn't exist yet
    const nested_dir = try std.fs.path.join(std.testing.allocator, &.{
        abs_cache_dir, "nested", "directories", "for", "testing",
    });
    defer std.testing.allocator.free(nested_dir);

    const file_hash = "ghi345678";
    const test_data = "This should create all parent directories!";

    // Write data to cache (this should create all necessary directories)
    const bytes_written = try writeToCache(nested_dir, file_hash, test_data);
    const expected_bytes = @sizeOf(CacheHeader) + test_data.len;
    try std.testing.expectEqual(expected_bytes, bytes_written);

    // Try to read back the data to confirm it worked
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, nested_dir, file_hash);

    // Verify bytes read matches bytes written
    try std.testing.expectEqual(bytes_written, bytes_read);

    // Verify the data content
    const data_start = @sizeOf(CacheHeader);
    const read_data = read_buffer[data_start..bytes_read];
    try std.testing.expectEqualStrings(test_data, read_data);
}

test "writeToCache with deep nonexistent directory structure" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_cache_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create a deeply nested path where none of the directories exist
    const deep_cache_dir = try std.fs.path.join(
        std.testing.allocator,
        &.{
            abs_cache_dir,
            "level1",
            "level2",
            "level3",
            "level4",
            "level5",
            "level6",
        },
    );
    defer std.testing.allocator.free(deep_cache_dir);

    const file_hash = "deepnested123";
    const test_data = "Testing a deeply nested directory structure that doesn't exist yet";

    // Write data to cache (this should create all necessary directories)
    const bytes_written = try writeToCache(deep_cache_dir, file_hash, test_data);
    const expected_bytes = @sizeOf(CacheHeader) + test_data.len;
    try std.testing.expectEqual(expected_bytes, bytes_written);

    // Try to read back the data to confirm it worked
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, deep_cache_dir, file_hash);

    // Verify bytes read matches bytes written
    try std.testing.expectEqual(bytes_written, bytes_read);

    // Parse the header from read buffer
    const header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);
    try std.testing.expectEqual(@as(u32, test_data.len), header.total_cached_bytes);

    // Verify the data content
    const data_start = @sizeOf(CacheHeader);
    const read_data = read_buffer[data_start..bytes_read];
    try std.testing.expectEqualStrings(test_data, read_data);

    // Verify that all the directories were created
    // Check level1 directory exists
    const level1_path = try std.fs.path.join(std.testing.allocator, &.{ abs_cache_dir, "level1" });
    defer std.testing.allocator.free(level1_path);
    const level1_dir = std.fs.openDirAbsolute(level1_path, .{}) catch {
        try std.testing.expect(false); // Should not reach here
        unreachable;
    };
    var level1_dir_mutable = level1_dir;
    level1_dir_mutable.close();

    // Check level3 directory exists (middle level)
    const level3_path = try std.fs.path.join(std.testing.allocator, &.{ abs_cache_dir, "level1", "level2", "level3" });
    defer std.testing.allocator.free(level3_path);
    const level3_dir = std.fs.openDirAbsolute(level3_path, .{}) catch {
        try std.testing.expect(false); // Should not reach here
        unreachable;
    };
    var level3_dir_mutable = level3_dir;
    level3_dir_mutable.close();
}

test "writeToCache with nonexistent parent directories" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create paths for multiple levels of nested directories
    const cache_dir = try std.fs.path.join(std.testing.allocator, &.{ base_dir, "cache_dir" });
    defer std.testing.allocator.free(cache_dir);

    // Create a hash that will require multiple levels of directories
    // The hash directories are created based on the hash value (split in two parts)
    const file_hash = "abcdefghijklmnopqrstuvwxyz123456";
    const test_data = "Testing directory creation with hash-based directory structure";

    // This will test that all parent directories in the hash-based path are created
    // automatically, even when none of them exist yet
    const bytes_written = try writeToCache(cache_dir, file_hash, test_data);
    const expected_bytes = @sizeOf(CacheHeader) + test_data.len;
    try std.testing.expectEqual(expected_bytes, bytes_written);

    // Read data back to verify it was written correctly
    var read_buffer: [1024]u8 align(@alignOf(CacheHeader)) = undefined;
    const bytes_read = try readCacheInto(&read_buffer, cache_dir, file_hash);

    // Verify bytes read matches bytes written
    try std.testing.expectEqual(bytes_written, bytes_read);

    // Parse the header from read buffer
    const header = try CacheHeader.initFromBytes(read_buffer[0..bytes_read]);
    try std.testing.expectEqual(@as(u32, test_data.len), header.total_cached_bytes);

    // Verify the data content
    const data_start = @sizeOf(CacheHeader);
    const read_data = read_buffer[data_start..bytes_read];
    try std.testing.expectEqualStrings(test_data, read_data);
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

    // Make sure the separator position is correct
    try std.testing.expectEqual(std.fs.path.sep, path_buf[hash_sep_pos]);

    // Check that the hash starts where expected
    try std.testing.expectEqual(abs_cache_dir.len + 1, hash_start);

    // Verify path ends with file extension
    try std.testing.expectEqualStrings(file_ext, path_buf[ext_start .. ext_start + file_ext.len]);

    // Verify null termination
    try std.testing.expectEqual(@as(u8, 0), path_buf[ext_end]);
}

test "createCachePath - buffer too small" {
    // Create a small buffer that can't fit the path
    var small_buf: [10]u8 = undefined;
    const abs_cache_dir = "/tmp/cache";
    const hash = "testHash123";

    // Test that it returns PathTooLong error
    const result = createCachePath(&small_buf, abs_cache_dir, hash);
    try std.testing.expectError(CacheError.PathTooLong, result);
}

test "writeHashToPath - buffer too small" {
    // Create a small buffer that can't fit the encoded hash
    var small_buf: [5]u8 = undefined;
    const hash = "testHash123";

    // Test that it returns PathTooLong error
    const result = writeHashToPath(hash, &small_buf);
    try std.testing.expectError(CacheError.PathTooLong, result);
}

test "writeHashToPath - success" {
    var buf: [100]u8 = undefined;
    const hash = "testHash123";

    _ = try writeHashToPath(hash, &buf);

    // Verify that the separator is at the expected position
    try std.testing.expectEqual(std.fs.path.sep, buf[hash_encoder.calcSize(hash.len / 2)]);
}
