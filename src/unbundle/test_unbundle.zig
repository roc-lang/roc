//! Tests for unbundle functionality
//!
//! These tests verify:
//! - Path validation and security
//! - Base58 hash validation
//! - Error handling

const std = @import("std");
const testing = std.testing;
const unbundle = @import("unbundle.zig");
const base58 = @import("base58");

test "pathHasUnbundleErr - various invalid paths" {
    // Test path traversal
    const err1 = unbundle.pathHasUnbundleErr("../etc/passwd");
    try testing.expect(err1 != null);
    try testing.expect(err1.?.reason == .path_traversal);

    // Test absolute path
    const err2 = unbundle.pathHasUnbundleErr("/etc/passwd");
    try testing.expect(err2 != null);
    try testing.expect(err2.?.reason == .absolute_path);

    // Test current directory reference
    const err3 = unbundle.pathHasUnbundleErr("./file.txt");
    try testing.expect(err3 != null);
    try testing.expect(err3.?.reason == .current_directory_reference);

    // Test Windows reserved name
    const err4 = unbundle.pathHasUnbundleErr("CON");
    try testing.expect(err4 != null);
    try testing.expect(err4.?.reason == .windows_reserved_name);

    // Test path with null byte
    const err5 = unbundle.pathHasUnbundleErr("file\x00name.txt");
    try testing.expect(err5 != null);
    try testing.expect(err5.?.reason == .windows_reserved_char);

    // Test component ending with space
    const err6 = unbundle.pathHasUnbundleErr("file /test.txt");
    try testing.expect(err6 != null);
    try testing.expect(err6.?.reason == .component_ends_with_space);

    // Test component ending with period
    const err7 = unbundle.pathHasUnbundleErr("file./test.txt");
    try testing.expect(err7 != null);
    try testing.expect(err7.?.reason == .component_ends_with_period);

    // Test valid paths
    try testing.expect(unbundle.pathHasUnbundleErr("normal/file.txt") == null);
    try testing.expect(unbundle.pathHasUnbundleErr("file.txt") == null);
    try testing.expect(unbundle.pathHasUnbundleErr("deeply/nested/dir/file.md") == null);
}

test "pathHasUnbundleErr - Windows reserved characters" {
    // Test various Windows reserved characters
    const reserved_chars = [_]u8{ '<', '>', ':', '"', '|', '?', '*' };

    for (reserved_chars) |char| {
        var path_buf: [20]u8 = undefined;
        const path = try std.fmt.bufPrint(&path_buf, "file{c}name.txt", .{char});
        const err = unbundle.pathHasUnbundleErr(path);
        try testing.expect(err != null);
        try testing.expect(err.?.reason == .windows_reserved_char);
        try testing.expectEqual(char, err.?.reason.windows_reserved_char);
    }
}

test "pathHasUnbundleErr - Windows reserved names" {
    const reserved_names = [_][]const u8{
        "CON",  "PRN",  "AUX",  "NUL",
        "COM1", "COM2", "COM3", "COM4",
        "COM5", "COM6", "COM7", "COM8",
        "COM9", "LPT1", "LPT2", "LPT3",
        "LPT4", "LPT5", "LPT6", "LPT7",
        "LPT8", "LPT9",
    };

    for (reserved_names) |name| {
        // Test exact match
        const err1 = unbundle.pathHasUnbundleErr(name);
        try testing.expect(err1 != null);
        try testing.expect(err1.?.reason == .windows_reserved_name);

        // Test with extension (still reserved)
        var path_buf: [20]u8 = undefined;
        const path_with_ext = try std.fmt.bufPrint(&path_buf, "{s}.txt", .{name});
        const err2 = unbundle.pathHasUnbundleErr(path_with_ext);
        try testing.expect(err2 != null);
        try testing.expectEqual(unbundle.PathValidationReason.windows_reserved_name, err2.?.reason);
    }
}

test "pathHasUnbundleErr - backslash handling" {
    // On non-Windows, backslash should be rejected
    if (@import("builtin").os.tag != .windows) {
        const err = unbundle.pathHasUnbundleErr("path\\with\\backslash");
        try testing.expect(err != null);
        try testing.expect(err.?.reason == .contained_backslash_on_unix);
    }
}

test "validateBase58Hash - valid and invalid hashes" {
    // Generate a real hash and encode it
    const data = "test data";
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(data);
    var hash: [32]u8 = undefined;
    hasher.final(&hash);

    var base58_buf: [44]u8 = undefined;
    const base58_hash = base58.encode(&hash, &base58_buf);

    // Valid base58 hash should decode correctly
    const decoded = try unbundle.validateBase58Hash(base58_hash);
    try testing.expect(decoded != null);
    try testing.expectEqualSlices(u8, &hash, &decoded.?);

    // Invalid base58 should return null
    const invalid1 = try unbundle.validateBase58Hash("not-valid-base58!@#");
    try testing.expect(invalid1 == null);

    // Short strings should return null (less than typical hash length)
    // "abc" is only 3 characters, but base58 decode might succeed with garbage
    // We need to check for minimum expected length for a 32-byte hash
    const invalid2 = try unbundle.validateBase58Hash("abc");
    try testing.expect(invalid2 == null);

    // Empty string should return null
    const invalid3 = try unbundle.validateBase58Hash("");
    try testing.expect(invalid3 == null);
}

test "BufferExtractWriter - basic functionality" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var writer = unbundle.BufferExtractWriter.init(alloc);
    defer writer.deinit();

    // Create a file
    const file_writer = try writer.extractWriter().createFile("test.txt");
    try file_writer.writeAll("Hello, World!");
    writer.extractWriter().finishFile(file_writer);

    // Create a directory (should be no-op for buffer writer)
    try writer.extractWriter().makeDir("test_dir");

    // Create another file in a subdirectory
    const file_writer2 = try writer.extractWriter().createFile("subdir/test2.txt");
    try file_writer2.writeAll("Second file");
    writer.extractWriter().finishFile(file_writer2);

    // Verify files were stored
    try testing.expectEqual(@as(usize, 2), writer.files.count());

    // Check first file
    const file1 = writer.files.get("test.txt");
    try testing.expect(file1 != null);
    try testing.expectEqualStrings("Hello, World!", file1.?.items);

    // Check second file
    const file2 = writer.files.get("subdir/test2.txt");
    try testing.expect(file2 != null);
    try testing.expectEqualStrings("Second file", file2.?.items);
}

test "DirExtractWriter - basic functionality" {
    // Create temp directory for extraction
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    var writer = unbundle.DirExtractWriter.init(tmp.dir, testing.allocator);
    defer writer.deinit();

    // Create a directory
    try writer.extractWriter().makeDir("test_dir");

    // Verify directory was created
    const dir_stat = try tmp.dir.statFile("test_dir");
    try testing.expect(dir_stat.kind == .directory);

    // Create a file
    const file_writer = try writer.extractWriter().createFile("test.txt");
    try file_writer.writeAll("Test content");
    writer.extractWriter().finishFile(file_writer);

    // Verify file was created
    const content = try tmp.dir.readFileAlloc(testing.allocator, "test.txt", 1024);
    defer testing.allocator.free(content);
    try testing.expectEqualStrings("Test content", content);

    // Create a file in a subdirectory (should create parent dirs)
    const file_writer2 = try writer.extractWriter().createFile("deep/nested/file.txt");
    try file_writer2.writeAll("Nested content");
    writer.extractWriter().finishFile(file_writer2);

    // Verify nested file was created
    const nested_content = try tmp.dir.readFileAlloc(testing.allocator, "deep/nested/file.txt", 1024);
    defer testing.allocator.free(nested_content);
    try testing.expectEqualStrings("Nested content", nested_content);
}

test "unbundle filename validation" {
    // Use a dummy reader and directory that won't actually be used
    const dummy_data = "";
    var stream = std.io.fixedBufferStream(dummy_data);
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    // Test with invalid filename (no .tar.zst extension)
    try testing.expectError(error.InvalidFilename, unbundle.unbundle(testing.allocator, stream.reader(), tmp.dir, "invalid.txt", null));

    // Reset stream position
    stream.pos = 0;

    // Test with invalid base58 hash
    try testing.expectError(error.InvalidFilename, unbundle.unbundle(testing.allocator, stream.reader(), tmp.dir, "not-valid-base58!@#.tar.zst", null));

    // Reset stream position
    stream.pos = 0;

    // Test with empty hash
    try testing.expectError(error.InvalidFilename, unbundle.unbundle(testing.allocator, stream.reader(), tmp.dir, ".tar.zst", null));
}

test "pathHasUnbundleErr - long paths" {
    var long_path: [300]u8 = undefined;
    @memset(&long_path, 'a');

    const err = unbundle.pathHasUnbundleErr(&long_path);
    try testing.expect(err != null);
    try testing.expect(err.?.reason == .path_too_long);
}

test "pathHasUnbundleErr - empty path" {
    const err = unbundle.pathHasUnbundleErr("");
    try testing.expect(err != null);
    try testing.expect(err.?.reason == .empty_path);
}

test "pathHasUnbundleErr - mixed valid and invalid components" {
    // Path with valid components but one .. in the middle
    const err1 = unbundle.pathHasUnbundleErr("valid/path/../file.txt");
    try testing.expect(err1 != null);
    try testing.expect(err1.?.reason == .path_traversal);

    // Path with valid components but one . in the middle
    const err2 = unbundle.pathHasUnbundleErr("valid/./path/file.txt");
    try testing.expect(err2 != null);
    try testing.expect(err2.?.reason == .current_directory_reference);
}

test "pathHasUnbundleErr - Windows drive letters" {
    const paths = [_][]const u8{
        "C:/file.txt",
        "D:\\file.txt",
        "Z:file.txt",
    };

    for (paths) |path| {
        const err = unbundle.pathHasUnbundleErr(path);
        try testing.expect(err != null);
        try testing.expect(err.?.reason == .absolute_path);
    }
}

test "pathHasUnbundleErr - special characters in filenames" {
    // Test null byte
    const err1 = unbundle.pathHasUnbundleErr("file\x00name.txt");
    try testing.expect(err1 != null);
    try testing.expect(err1.?.reason == .windows_reserved_char);
    try testing.expectEqual(@as(u8, 0), err1.?.reason.windows_reserved_char);

    // Test various control characters are allowed (except null)
    try testing.expect(unbundle.pathHasUnbundleErr("file\x01name.txt") == null);
    try testing.expect(unbundle.pathHasUnbundleErr("file\x1fname.txt") == null);
}

test "validateBase58Hash - edge cases" {
    // Exactly 32 characters (minimum valid)
    const short_valid = "11111111111111111111111111111111";
    const result1 = try unbundle.validateBase58Hash(short_valid);
    try testing.expect(result1 != null);

    // 31 characters (too short)
    const too_short = "1111111111111111111111111111111";
    const result2 = try unbundle.validateBase58Hash(too_short);
    try testing.expect(result2 == null);

    // 45 characters (too long)
    const too_long = "111111111111111111111111111111111111111111111";
    const result3 = try unbundle.validateBase58Hash(too_long);
    try testing.expect(result3 == null);
}

test "BufferExtractWriter - overwrite existing file" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var writer = unbundle.BufferExtractWriter.init(alloc);
    defer writer.deinit();

    // Create a file with initial content
    const file_writer1 = try writer.extractWriter().createFile("test.txt");
    try file_writer1.writeAll("Initial content");
    writer.extractWriter().finishFile(file_writer1);

    // Overwrite the same file
    const file_writer2 = try writer.extractWriter().createFile("test.txt");
    try file_writer2.writeAll("New content");
    writer.extractWriter().finishFile(file_writer2);

    // Verify it was overwritten
    const file = writer.files.get("test.txt");
    try testing.expect(file != null);
    try testing.expectEqualStrings("New content", file.?.items);
}

test "DirExtractWriter - nested directory creation" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    var writer = unbundle.DirExtractWriter.init(tmp.dir, testing.allocator);
    defer writer.deinit();

    // Create a file in a deeply nested path
    const file_writer = try writer.extractWriter().createFile("a/b/c/d/e/file.txt");
    try file_writer.writeAll("Nested content");
    writer.extractWriter().finishFile(file_writer);

    // Verify the file was created
    const content = try tmp.dir.readFileAlloc(testing.allocator, "a/b/c/d/e/file.txt", 1024);
    defer testing.allocator.free(content);
    try testing.expectEqualStrings("Nested content", content);
}

test "ErrorContext population" {
    var error_context: unbundle.ErrorContext = undefined;

    // Test that error context is populated correctly
    if (unbundle.pathHasUnbundleErr("../etc/passwd")) |validation_error| {
        error_context.path = validation_error.path;
        error_context.reason = validation_error.reason;

        try testing.expectEqualStrings("../etc/passwd", error_context.path);
        try testing.expect(error_context.reason == .path_traversal);
    } else {
        try testing.expect(false); // Should have failed
    }
}
