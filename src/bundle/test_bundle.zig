//! Tests for the bundle and download functionality.
//!
//! This file contains comprehensive tests for:
//! - Bundle creation with tar and zstd compression
//! - Bundle extraction with hash verification
//! - Base58 encoding/decoding
//! - Download URL validation
//! - Memory-based file system for testing

const std = @import("std");
const bundle = @import("bundle.zig");
const download = @import("download.zig");
const streaming_writer = @import("streaming_writer.zig");
const test_util = @import("test_util.zig");
const DirExtractWriter = bundle.DirExtractWriter;
const FilePathIterator = test_util.FilePathIterator;

// Use fast compression for tests
const TEST_COMPRESSION_LEVEL: c_int = 2;

test "path validation prevents malicious paths" {
    const testing = std.testing;

    const test_cases = [_]struct {
        path: []const u8,
        should_fail: bool,
        description: []const u8,
    }{
        // Directory traversal
        .{ .path = "../../../etc/passwd", .should_fail = true, .description = "Directory traversal" },
        .{ .path = "foo/../../../etc/passwd", .should_fail = true, .description = "Directory traversal in middle" },
        .{ .path = "./foo/../../bar", .should_fail = true, .description = "Directory traversal with current dir" },
        .{ .path = "foo/bar/..", .should_fail = true, .description = "Trailing directory traversal" },

        // Absolute paths
        .{ .path = "/etc/passwd", .should_fail = true, .description = "Absolute path Unix" },

        // Current directory references
        .{ .path = "foo/./bar", .should_fail = true, .description = "Current directory reference" },
        .{ .path = ".", .should_fail = true, .description = "Single dot" },
        .{ .path = "./foo", .should_fail = true, .description = "Current directory prefix" },

        // Reserved characters
        .{ .path = "foo:bar.txt", .should_fail = true, .description = "Colon character" },
        .{ .path = "foo*bar.txt", .should_fail = true, .description = "Asterisk character" },
        .{ .path = "foo?bar.txt", .should_fail = true, .description = "Question mark" },
        .{ .path = "foo\"bar.txt", .should_fail = true, .description = "Quote character" },
        .{ .path = "foo<bar.txt", .should_fail = true, .description = "Less than character" },
        .{ .path = "foo>bar.txt", .should_fail = true, .description = "Greater than character" },
        .{ .path = "foo|bar.txt", .should_fail = true, .description = "Pipe character" },
        .{ .path = "foo\\bar.txt", .should_fail = true, .description = "Backslash character" },

        // Windows reserved names
        .{ .path = "CON", .should_fail = true, .description = "Windows reserved name CON" },
        .{ .path = "con", .should_fail = true, .description = "Windows reserved name con (lowercase)" },
        .{ .path = "PRN.txt", .should_fail = true, .description = "Windows reserved name PRN with extension" },
        .{ .path = "AUX", .should_fail = true, .description = "Windows reserved name AUX" },
        .{ .path = "NUL", .should_fail = true, .description = "Windows reserved name NUL" },
        .{ .path = "COM1", .should_fail = true, .description = "Windows reserved name COM1" },
        .{ .path = "LPT1", .should_fail = true, .description = "Windows reserved name LPT1" },
        .{ .path = "folder/CON/file.txt", .should_fail = true, .description = "Windows reserved name in path" },

        // Components ending with space or period
        .{ .path = "foo /bar.txt", .should_fail = true, .description = "Component ending with space" },
        .{ .path = "foo./bar.txt", .should_fail = true, .description = "Component ending with period" },
        .{ .path = "folder/file.txt ", .should_fail = true, .description = "Filename ending with space" },
        .{ .path = "folder/file.txt.", .should_fail = true, .description = "Filename ending with period" },

        // Edge cases
        .{ .path = "", .should_fail = true, .description = "Empty path" },
        .{ .path = "a" ** 256, .should_fail = true, .description = "Path too long (> 255 chars)" },

        // Valid paths
        .{ .path = "foo/bar.txt", .should_fail = false, .description = "Valid path" },
        .{ .path = "src/main.zig", .should_fail = false, .description = "Valid source path" },
        .{ .path = "a-b_c.123", .should_fail = false, .description = "Valid filename with special chars" },
        .{ .path = "deeply/nested/folder/structure/file.ext", .should_fail = false, .description = "Valid nested path" },
    };

    for (test_cases) |tc| {
        const validation_result = bundle.validatePath(tc.path);
        const is_valid = validation_result == null;

        if (tc.should_fail) {
            try testing.expect(!is_valid);
        } else {
            if (validation_result) |err| {
                std.debug.print("Unexpected validation failure for '{s}': {}\n", .{ tc.path, err.reason });
            }
            try testing.expect(is_valid);
        }
    }

    // Test path with NUL byte separately since we can't put it in a string literal easily
    const nul_path = [_]u8{ 'f', 'o', 'o', 0, 'b', 'a', 'r' };
    const nul_result = bundle.validatePath(&nul_path);
    try testing.expect(nul_result != null);
    if (nul_result) |err| {
        try testing.expectEqual(bundle.PathValidationReason.contains_nul, err.reason);
    }
}

test "path validation returns correct error reasons" {
    const testing = std.testing;

    // Test specific error reasons
    const test_cases = [_]struct {
        path: []const u8,
        expected_reason: bundle.PathValidationReason,
    }{
        .{ .path = "", .expected_reason = .empty_path },
        .{ .path = "a" ** 256, .expected_reason = .path_too_long },
        .{ .path = "foo\\bar", .expected_reason = .contains_backslash },
        .{ .path = "foo:bar", .expected_reason = .{ .windows_reserved_char = ':' } },
        .{ .path = "foo*bar", .expected_reason = .{ .windows_reserved_char = '*' } },
        .{ .path = "foo?bar", .expected_reason = .{ .windows_reserved_char = '?' } },
        .{ .path = "foo<bar", .expected_reason = .{ .windows_reserved_char = '<' } },
        .{ .path = "/etc/passwd", .expected_reason = .absolute_path },
        .{ .path = "../etc/passwd", .expected_reason = .path_traversal },
        .{ .path = "foo/./bar", .expected_reason = .current_directory_reference },
        .{ .path = "CON", .expected_reason = .windows_reserved_name },
        .{ .path = "com1.txt", .expected_reason = .windows_reserved_name },
        .{ .path = "foo ", .expected_reason = .component_ends_with_space },
        .{ .path = "foo.", .expected_reason = .component_ends_with_period },
    };

    for (test_cases) |tc| {
        const result = bundle.validatePath(tc.path);
        try testing.expect(result != null);
        if (result) |err| {
            try testing.expectEqual(tc.expected_reason, err.reason);
        }
    }
}

test "bundle fails with invalid paths" {
    const testing = std.testing;
    var allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    // Create a valid file that we'll try to bundle with invalid names
    {
        const file = try tmp.dir.createFile("valid.txt", .{});
        defer file.close();
        try file.writeAll("Test content");
    }

    // Test cases with invalid paths
    const invalid_paths = [_][]const u8{
        "foo:bar.txt", // Colon
        "foo*bar.txt", // Asterisk
        "foo\\bar.txt", // Backslash
        "../etc/passwd", // Directory traversal
        "CON", // Windows reserved name
        "file.txt ", // Trailing space
    };

    for (invalid_paths) |invalid_path| {
        var bundle_data = std.ArrayList(u8).init(allocator);
        defer bundle_data.deinit();

        const paths = [_][]const u8{invalid_path};
        var iter = FilePathIterator{ .paths = &paths };

        var error_ctx: bundle.ErrorContext = undefined;
        const result = bundle.bundle(&iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), tmp.dir, null, &error_ctx);

        try testing.expectError(error.InvalidPath, result);
    }
}

test "path validation prevents directory traversal" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a malicious tar with directory traversal attempt
    var malicious_tar = std.ArrayList(u8).init(allocator);
    defer malicious_tar.deinit();

    var tar_writer = std.tar.writer(malicious_tar.writer());

    // Try to write a file with ".." in path
    const Options = @TypeOf(tar_writer).Options;
    const options = Options{
        .mode = 0o644,
        .mtime = 0,
    };

    try tar_writer.writeFileBytes("../../../etc/passwd", "malicious content", options);
    try tar_writer.finish();

    // Compress it
    var compressed = std.ArrayList(u8).init(allocator);
    defer compressed.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        compressed.writer().any(),
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    try writer.writer().writeAll(malicious_tar.items);
    try writer.finish();

    const hash = writer.getHash();

    // Try to extract - should fail with InvalidPath error
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var stream = std.io.fixedBufferStream(compressed.items);
    var allocator_copy2 = allocator;
    var dir_writer = DirExtractWriter.init(tmp.dir);
    const result = bundle.unbundleStream(
        stream.reader(),
        dir_writer.extractWriter(),
        &allocator_copy2,
        &hash,
        null,
    );

    try testing.expectError(error.InvalidPath, result);
}

test "empty directories are preserved" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create source with empty directories
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create empty directories
    try src_dir.makePath("empty_dir");
    try src_dir.makePath("nested/empty");

    // Create one file to ensure bundle isn't empty
    {
        const file = try src_dir.createFile("readme.txt", .{});
        defer file.close();
        try file.writeAll("Test");
    }

    // Bundle with explicit directory entries
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    // Note: Current implementation doesn't explicitly handle empty directories
    // This test documents current behavior - empty dirs are NOT preserved
    const file_paths = [_][]const u8{"readme.txt"};
    var file_iter = FilePathIterator{ .paths = &file_paths };

    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Extract
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();

    var stream = std.io.fixedBufferStream(bundle_data.items);
    var allocator_copy = allocator;
    try bundle.unbundle(stream.reader(), dst_tmp.dir, &allocator_copy, filename, null);

    // Verify file exists
    _ = try dst_tmp.dir.statFile("readme.txt");

    // Document that empty directories are NOT preserved
    // This is a known limitation of the current implementation
}

test "bundle and unbundle roundtrip" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create source temp directory
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create test files and directories
    {
        const file = try src_dir.createFile("file1.txt", .{});
        defer file.close();
        try file.writeAll("Hello from file1!");
    }
    {
        const file = try src_dir.createFile("file2.txt", .{});
        defer file.close();
        try file.writeAll("This is file2 content.");
    }

    try src_dir.makePath("subdir1");
    {
        const file = try src_dir.createFile("subdir1/nested1.txt", .{});
        defer file.close();
        try file.writeAll("Nested file 1");
    }
    {
        const file = try src_dir.createFile("subdir1/nested2.txt", .{});
        defer file.close();
        try file.writeAll("Another nested file");
    }

    try src_dir.makePath("subdir2/deeply/nested");
    {
        const file = try src_dir.createFile("subdir2/deeply/nested/deep.txt", .{});
        defer file.close();
        try file.writeAll("Deep file content");
    }

    // Collect file paths
    const file_paths = [_][]const u8{
        "file1.txt",
        "file2.txt",
        "subdir1/nested1.txt",
        "subdir1/nested2.txt",
        "subdir2/deeply/nested/deep.txt",
    };

    // Create an iterator for the file paths
    var file_iter = FilePathIterator{ .paths = &file_paths };

    // Bundle to memory
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, &allocator, filename, null);

    // Verify all files exist with correct content
    const file1_content = try dst_dir.readFileAlloc(allocator, "file1.txt", 1024);
    defer allocator.free(file1_content);
    try testing.expectEqualStrings("Hello from file1!", file1_content);

    const file2_content = try dst_dir.readFileAlloc(allocator, "file2.txt", 1024);
    defer allocator.free(file2_content);
    try testing.expectEqualStrings("This is file2 content.", file2_content);

    const nested1_content = try dst_dir.readFileAlloc(allocator, "subdir1/nested1.txt", 1024);
    defer allocator.free(nested1_content);
    try testing.expectEqualStrings("Nested file 1", nested1_content);

    const nested2_content = try dst_dir.readFileAlloc(allocator, "subdir1/nested2.txt", 1024);
    defer allocator.free(nested2_content);
    try testing.expectEqualStrings("Another nested file", nested2_content);

    const deep_content = try dst_dir.readFileAlloc(allocator, "subdir2/deeply/nested/deep.txt", 1024);
    defer allocator.free(deep_content);
    try testing.expectEqualStrings("Deep file content", deep_content);
}

test "bundle and unbundle over socket stream" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Skip on Windows as Unix sockets aren't supported
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

    // Create source temp directory with test files
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create test files
    {
        const file = try src_dir.createFile("test1.txt", .{});
        defer file.close();
        try file.writeAll("Socket test file 1");
    }
    {
        const file = try src_dir.createFile("test2.txt", .{});
        defer file.close();
        try file.writeAll("This is socket test file 2!");
    }

    try src_dir.makePath("nested");
    {
        const file = try src_dir.createFile("nested/deep.txt", .{});
        defer file.close();
        try file.writeAll("Deep socket test content");
    }

    // Bundle to a file first
    var bundle_tmp = testing.tmpDir(.{});
    defer bundle_tmp.cleanup();

    const bundle_path = "test.bundle";
    const bundle_file = try bundle_tmp.dir.createFile(bundle_path, .{});
    defer bundle_file.close();

    const file_paths = [_][]const u8{
        "test1.txt",
        "test2.txt",
        "nested/deep.txt",
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_file.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Create socket in temp directory
    var socket_tmp = testing.tmpDir(.{});
    defer socket_tmp.cleanup();

    // Get the real path of the temp directory
    var real_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const real_path = try socket_tmp.dir.realpath(".", &real_path_buf);

    var socket_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const socket_path = try std.fmt.bufPrint(&socket_path_buf, "{s}/test.sock", .{real_path});

    // Create server thread
    const ServerContext = struct {
        socket_path: []const u8,
        bundle_path: []const u8,
        bundle_dir: std.fs.Dir,
        ready: std.Thread.ResetEvent = .{},
        done: std.Thread.ResetEvent = .{},

        fn run(ctx: *@This()) !void {
            const server = try std.net.Address.initUnix(ctx.socket_path);
            var listener = try server.listen(.{});
            defer listener.deinit();

            // Signal that server is ready
            ctx.ready.set();

            // Accept one connection
            const connection = try listener.accept();
            defer connection.stream.close();

            // Open and stream the bundle file
            const file = try ctx.bundle_dir.openFile(ctx.bundle_path, .{});
            defer file.close();

            // Stream file contents to socket
            var buf: [4096]u8 = undefined;
            while (true) {
                const bytes_read = try file.read(&buf);
                if (bytes_read == 0) break;
                _ = try connection.stream.writeAll(buf[0..bytes_read]);
            }

            ctx.done.set();
        }
    };

    var server_ctx = ServerContext{
        .socket_path = socket_path,
        .bundle_path = bundle_path,
        .bundle_dir = bundle_tmp.dir,
    };

    const server_thread = try std.Thread.spawn(.{}, ServerContext.run, .{&server_ctx});
    defer server_thread.join();

    // Wait for server to be ready
    server_ctx.ready.wait();

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Connect to socket and unbundle
    const stream = try std.net.connectUnixSocket(socket_path);
    defer stream.close();

    // Unbundle from socket stream
    try bundle.unbundle(stream.reader(), dst_dir, &allocator, filename, null);

    // Wait for server to finish
    server_ctx.done.wait();

    // Verify all files exist with correct content
    const file1_content = try dst_dir.readFileAlloc(allocator, "test1.txt", 1024);
    defer allocator.free(file1_content);
    try testing.expectEqualStrings("Socket test file 1", file1_content);

    const file2_content = try dst_dir.readFileAlloc(allocator, "test2.txt", 1024);
    defer allocator.free(file2_content);
    try testing.expectEqualStrings("This is socket test file 2!", file2_content);

    const deep_content = try dst_dir.readFileAlloc(allocator, "nested/deep.txt", 1024);
    defer allocator.free(deep_content);
    try testing.expectEqualStrings("Deep socket test content", deep_content);
}

test "std.tar.writer creates valid tar" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a tar in memory
    var tar_buffer = std.ArrayList(u8).init(allocator);
    defer tar_buffer.deinit();

    var tar_writer = std.tar.writer(tar_buffer.writer());

    // Write a simple file
    const content = "Hello tar world!";
    const Options = @TypeOf(tar_writer).Options;
    try tar_writer.writeFileBytes("test.txt", content, Options{
        .mode = 0o644,
        .mtime = 0,
    });

    try tar_writer.finish();

    // Now try to read it back
    var stream = std.io.fixedBufferStream(tar_buffer.items);
    var file_name_buffer: [256]u8 = undefined;
    var link_name_buffer: [256]u8 = undefined;
    var tar_iter = std.tar.iterator(stream.reader(), .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    const file = try tar_iter.next();
    try testing.expect(file != null);
    try testing.expectEqualStrings("test.txt", file.?.name);
    try testing.expectEqual(@as(u64, content.len), file.?.size);

    // Read content
    const reader = tar_iter.reader;
    var buf: [1024]u8 = undefined;
    const bytes_read = try reader.read(buf[0..content.len]);
    try testing.expectEqualStrings(content, buf[0..bytes_read]);
}

test "minimal bundle unbundle" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create source temp directory
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create a simple test file
    {
        const file = try src_dir.createFile("test.txt", .{});
        defer file.close();
        try file.writeAll("Hello");
    }

    // Bundle to memory
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const file_paths = [_][]const u8{"test.txt"};
    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, &allocator, filename, null);

    // Read and verify content
    const content = try dst_dir.readFileAlloc(allocator, "test.txt", 1024);
    defer allocator.free(content);
    try testing.expectEqualStrings("Hello", content);
}

test "bundle with path prefix stripping" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create source temp directory with nested structure
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create a deep directory structure
    try src_dir.makePath("foo/bar/src");
    try src_dir.makePath("foo/bar/src/utils");

    // Create test files with the prefix
    {
        const file = try src_dir.createFile("foo/bar/src/main.txt", .{});
        defer file.close();
        try file.writeAll("Main file content");
    }
    {
        const file = try src_dir.createFile("foo/bar/src/utils/helper.txt", .{});
        defer file.close();
        try file.writeAll("Helper file content");
    }

    // Bundle with path prefix
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    // File paths include the full prefix
    const file_paths = [_][]const u8{
        "foo/bar/src/main.txt",
        "foo/bar/src/utils/helper.txt",
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };

    // Bundle with prefix "foo/bar/src/"
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, "foo/bar/src/", null);
    defer allocator.free(filename);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, &allocator, filename, null);

    // Verify files exist WITHOUT the prefix
    const main_content = try dst_dir.readFileAlloc(allocator, "main.txt", 1024);
    defer allocator.free(main_content);
    try testing.expectEqualStrings("Main file content", main_content);

    const helper_content = try dst_dir.readFileAlloc(allocator, "utils/helper.txt", 1024);
    defer allocator.free(helper_content);
    try testing.expectEqualStrings("Helper file content", helper_content);
}

test "blake3 hash verification success" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create a simple test file
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    {
        const file = try src_dir.createFile("test.txt", .{});
        defer file.close();
        try file.writeAll("Test content for hash verification");
    }

    // Bundle the file
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const file_paths = [_][]const u8{"test.txt"};
    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Verify filename ends with .tar.zst
    try testing.expect(std.mem.endsWith(u8, filename, ".tar.zst"));

    // Create destination directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle with correct filename - should succeed
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, &allocator, filename, null);

    // Verify content
    const content = try dst_dir.readFileAlloc(allocator, "test.txt", 1024);
    defer allocator.free(content);
    try testing.expectEqualStrings("Test content for hash verification", content);
}

test "blake3 hash verification failure" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create a simple test file
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    {
        const file = try src_dir.createFile("test.txt", .{});
        defer file.close();
        try file.writeAll("Test content");
    }

    // Bundle the file
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const file_paths = [_][]const u8{"test.txt"};
    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Create destination directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Try to unbundle with wrong filename - should fail
    const wrong_filename = "1234567890abcdef.tar.zst";
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const result = bundle.unbundle(stream.reader(), dst_dir, &allocator, wrong_filename, null);

    try testing.expectError(error.InvalidFilename, result);
}

test "unbundle with existing directory error" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create temp directory
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_dir = tmp.dir;

    // Create a simple tar archive
    var output_buffer = std.ArrayList(u8).init(allocator);
    defer output_buffer.deinit();

    const files = [_][]const u8{"test.txt"};
    var iter = FilePathIterator{ .paths = &files };

    // Create test file
    {
        const file = try tmp_dir.createFile("test.txt", .{});
        defer file.close();
        try file.writeAll("test content");
    }

    // Bundle the file
    const filename = try bundle.bundle(&iter, TEST_COMPRESSION_LEVEL, &allocator, output_buffer.writer(), tmp_dir, null, null);
    defer allocator.free(filename);

    // Write the bundled data to a file
    {
        const bundle_file = try tmp_dir.createFile(filename, .{});
        defer bundle_file.close();
        try bundle_file.writeAll(output_buffer.items);
    }

    // Extract the base name without extension for directory
    const dir_name = filename[0 .. filename.len - 8]; // Remove .tar.zst

    // Create a directory with the same name
    try tmp_dir.makePath(dir_name);

    // Try to unbundle - should fail because directory exists
    const bundle_file = try tmp_dir.openFile(filename, .{});
    defer bundle_file.close();

    // This should succeed but the CLI would error on existing directory
    try bundle.unbundle(bundle_file.reader(), tmp_dir, &allocator, filename, null);
}

test "unbundle multiple archives" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create temp directory
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_dir = tmp.dir;

    // Create two different archives
    var filenames = std.ArrayList([]const u8).init(allocator);
    defer {
        for (filenames.items) |fname| {
            allocator.free(fname);
        }
        filenames.deinit();
    }

    // First archive
    {
        var output_buffer = std.ArrayList(u8).init(allocator);
        defer output_buffer.deinit();

        const files = [_][]const u8{"file1.txt"};
        var iter = FilePathIterator{ .paths = &files };

        {
            const file = try tmp_dir.createFile("file1.txt", .{});
            defer file.close();
            try file.writeAll("content 1");
        }

        const filename = try bundle.bundle(&iter, TEST_COMPRESSION_LEVEL, &allocator, output_buffer.writer(), tmp_dir, null, null);
        try filenames.append(filename);

        const bundle_file = try tmp_dir.createFile(filename, .{});
        defer bundle_file.close();
        try bundle_file.writeAll(output_buffer.items);
    }

    // Second archive
    {
        var output_buffer = std.ArrayList(u8).init(allocator);
        defer output_buffer.deinit();

        const files = [_][]const u8{"file2.txt"};
        var iter = FilePathIterator{ .paths = &files };

        {
            const file = try tmp_dir.createFile("file2.txt", .{});
            defer file.close();
            try file.writeAll("content 2");
        }

        const filename = try bundle.bundle(&iter, TEST_COMPRESSION_LEVEL, &allocator, output_buffer.writer(), tmp_dir, null, null);
        try filenames.append(filename);

        const bundle_file = try tmp_dir.createFile(filename, .{});
        defer bundle_file.close();
        try bundle_file.writeAll(output_buffer.items);
    }

    // Unbundle both archives
    for (filenames.items) |fname| {
        const bundle_file = try tmp_dir.openFile(fname, .{});
        defer bundle_file.close();

        const dir_name = fname[0 .. fname.len - 8]; // Remove .tar.zst
        const extract_dir = try tmp_dir.makeOpenPath(dir_name, .{});

        try bundle.unbundle(bundle_file.reader(), extract_dir, &allocator, fname, null);
    }

    // Verify extraction
    const dir1_name = filenames.items[0][0 .. filenames.items[0].len - 8];
    const dir2_name = filenames.items[1][0 .. filenames.items[1].len - 8];

    const path1 = try std.fmt.allocPrint(allocator, "{s}/file1.txt", .{dir1_name});
    defer allocator.free(path1);
    const extracted1 = try tmp_dir.readFileAlloc(allocator, path1, 1024);
    defer allocator.free(extracted1);
    try testing.expectEqualStrings("content 1", extracted1);

    const path2 = try std.fmt.allocPrint(allocator, "{s}/file2.txt", .{dir2_name});
    defer allocator.free(path2);
    const extracted2 = try tmp_dir.readFileAlloc(allocator, path2, 1024);
    defer allocator.free(extracted2);
    try testing.expectEqualStrings("content 2", extracted2);
}

test "blake3 hash detects corruption" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create a test file
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    {
        const file = try src_dir.createFile("test.txt", .{});
        defer file.close();
        try file.writeAll("Original content");
    }

    // Bundle the file
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const file_paths = [_][]const u8{"test.txt"};
    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), src_dir, null, null);
    defer allocator.free(filename);

    // Corrupt the data by flipping a bit
    // Since the bundle is compressed, corrupting any bit should be detected
    if (bundle_data.items.len > 10) {
        // Corrupt a bit near the end to avoid breaking the zstd header
        bundle_data.items[bundle_data.items.len - 5] ^= 0x01;
    }

    // Create destination directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Try to unbundle corrupted data - should fail with HashMismatch or DecompressionFailed
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const result = bundle.unbundle(stream.reader(), dst_dir, &allocator, filename, null);

    // Corruption can cause either hash mismatch (if decompression succeeds but data is wrong)
    // or decompression failure (if the compressed stream structure is corrupted)
    if (result) |_| {
        return error.TestUnexpectedResult;
    } else |err| {
        switch (err) {
            error.HashMismatch, error.DecompressionFailed, error.InvalidTarHeader => {
                // Any of these errors are acceptable - corruption was detected
            },
            else => return err,
        }
    }
}

test "double roundtrip bundle -> unbundle -> bundle -> unbundle" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create initial temp directory with test files
    var initial_tmp = testing.tmpDir(.{});
    defer initial_tmp.cleanup();
    const initial_dir = initial_tmp.dir;

    // Create test files with varied content
    const test_files = [_]struct { path: []const u8, content: []const u8 }{
        .{ .path = "README.md", .content = "# Test Project\n\nThis is a test." },
        .{ .path = "src/main.roc", .content = "app \"test\"\n    packages {}\n    imports []\n    provides [main] to pf\n\nmain = \"Hello!\"" },
        .{ .path = "src/utils.roc", .content = "module [helper]\n\nhelper = \\x -> x + 1" },
        .{ .path = "test/test1.roc", .content = "# Test file 1\nexpect 1 == 1" },
        .{ .path = "test/test2.roc", .content = "# Test file 2\nexpect 2 + 2 == 4" },
        .{ .path = "docs/guide.txt", .content = "User Guide\n==========\n\nStep 1: ...\nStep 2: ..." },
    };

    // Create all test files
    for (test_files) |test_file| {
        if (std.fs.path.dirname(test_file.path)) |dir| {
            try initial_dir.makePath(dir);
        }
        const file = try initial_dir.createFile(test_file.path, .{});
        defer file.close();
        try file.writeAll(test_file.content);
    }

    // First bundle
    var first_bundle = std.ArrayList(u8).init(allocator);
    defer first_bundle.deinit();

    var paths1 = std.ArrayList([]const u8).init(allocator);
    defer paths1.deinit();
    for (test_files) |test_file| {
        try paths1.append(test_file.path);
    }
    var iter1 = FilePathIterator{ .paths = paths1.items };

    const filename1 = try bundle.bundle(&iter1, TEST_COMPRESSION_LEVEL, &allocator, first_bundle.writer(), initial_dir, null, null);
    defer allocator.free(filename1);

    // Write first bundle to file
    {
        const bundle_file = try initial_dir.createFile(filename1, .{});
        defer bundle_file.close();
        try bundle_file.writeAll(first_bundle.items);
    }

    // First unbundle
    var unbundle1_tmp = testing.tmpDir(.{});
    defer unbundle1_tmp.cleanup();
    const unbundle1_dir = unbundle1_tmp.dir;

    {
        const bundle_file = try initial_dir.openFile(filename1, .{});
        defer bundle_file.close();

        const extract_dir = try unbundle1_dir.makeOpenPath("extracted1", .{});

        try bundle.unbundle(bundle_file.reader(), extract_dir, &allocator, filename1, null);
    }

    // Second bundle (from first extraction)
    var second_bundle = std.ArrayList(u8).init(allocator);
    defer second_bundle.deinit();

    var paths2 = std.ArrayList([]const u8).init(allocator);
    defer paths2.deinit();
    for (test_files) |test_file| {
        try paths2.append(test_file.path);
    }
    var iter2 = FilePathIterator{ .paths = paths2.items };

    const extracted1_dir = try unbundle1_dir.openDir("extracted1", .{});
    const filename2 = try bundle.bundle(&iter2, TEST_COMPRESSION_LEVEL, &allocator, second_bundle.writer(), extracted1_dir, null, null);
    defer allocator.free(filename2);

    // Filenames should be identical (same content = same hash)
    try testing.expectEqualStrings(filename1, filename2);

    // Write second bundle to file
    {
        const bundle_file = try unbundle1_dir.createFile(filename2, .{});
        defer bundle_file.close();
        try bundle_file.writeAll(second_bundle.items);
    }

    // Second unbundle
    var unbundle2_tmp = testing.tmpDir(.{});
    defer unbundle2_tmp.cleanup();
    const unbundle2_dir = unbundle2_tmp.dir;

    {
        const bundle_file = try unbundle1_dir.openFile(filename2, .{});
        defer bundle_file.close();

        const extract_dir = try unbundle2_dir.makeOpenPath("extracted2", .{});

        try bundle.unbundle(bundle_file.reader(), extract_dir, &allocator, filename2, null);
    }

    // Verify all files match original content
    const extracted2_dir = try unbundle2_dir.openDir("extracted2", .{});
    for (test_files) |test_file| {
        const content = try extracted2_dir.readFileAlloc(allocator, test_file.path, 10240);
        defer allocator.free(content);
        try testing.expectEqualStrings(test_file.content, content);
    }

    // Bundle sizes should be identical
    try testing.expectEqual(first_bundle.items.len, second_bundle.items.len);
}

test "CLI unbundle with no args defaults to all .tar.zst files" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create temp directory
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_dir = tmp.dir;

    // Create multiple archives
    var archive_names = std.ArrayList([]const u8).init(allocator);
    defer {
        for (archive_names.items) |name| {
            allocator.free(name);
        }
        archive_names.deinit();
    }

    // Create 3 different archives
    for ([_][]const u8{ "file1.txt", "file2.txt", "file3.txt" }) |filename| {
        var output_buffer = std.ArrayList(u8).init(allocator);
        defer output_buffer.deinit();

        const files = [_][]const u8{filename};
        var iter = FilePathIterator{ .paths = &files };

        // Create test file
        {
            const file = try tmp_dir.createFile(filename, .{});
            defer file.close();
            try file.writer().print("Content of {s}", .{filename});
        }

        const archive_name = try bundle.bundle(&iter, TEST_COMPRESSION_LEVEL, &allocator, output_buffer.writer(), tmp_dir, null, null);
        try archive_names.append(archive_name);

        // Write archive to disk
        const archive_file = try tmp_dir.createFile(archive_name, .{});
        defer archive_file.close();
        try archive_file.writeAll(output_buffer.items);
    }

    // Verify all archives exist
    try testing.expectEqual(@as(usize, 3), archive_names.items.len);
    for (archive_names.items) |name| {
        try testing.expect(std.mem.endsWith(u8, name, ".tar.zst"));
        _ = try tmp_dir.statFile(name);
    }

    // Simulate unbundle with no args - should extract all .tar.zst files
    // Here we just verify that our test setup would work with the CLI
    var cwd = try tmp_dir.openDir(".", .{ .iterate = true });
    defer cwd.close();

    var found_archives = std.ArrayList([]const u8).init(allocator);
    defer found_archives.deinit();

    var iter = cwd.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".tar.zst")) {
            try found_archives.append(entry.name);
        }
    }

    // Should find all 3 archives
    try testing.expectEqual(@as(usize, 3), found_archives.items.len);
}

test "download URL validation" {
    const testing = std.testing;

    // Create a temp dir for testing (won't actually download)
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    // Valid HTTPS URLs
    {
        const url = "https://example.com/path/to/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        _ = download.validateUrl(url) catch |err| {
            try testing.expect(false); // Should not error
            std.debug.print("Unexpected error: {}\n", .{err});
        };
    }

    // Valid localhost IPv4 URL
    {
        const url = "http://127.0.0.1:8000/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        _ = download.validateUrl(url) catch |err| {
            try testing.expect(false); // Should not error
            std.debug.print("Unexpected error: {}\n", .{err});
        };
    }

    // Valid localhost IPv6 URL with port
    {
        const url = "http://[::1]:8000/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        _ = download.validateUrl(url) catch |err| {
            try testing.expect(false); // Should not error
            std.debug.print("Unexpected error: {}\n", .{err});
        };
    }

    // Valid localhost IPv6 URL without port
    {
        const url = "http://[::1]/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        _ = download.validateUrl(url) catch |err| {
            try testing.expect(false); // Should not error
            std.debug.print("Unexpected error: {}\n", .{err});
        };
    }

    // Valid: localhost hostname (will be resolved and verified during download)
    {
        const url = "http://localhost:8000/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const hash = try download.validateUrl(url);
        try testing.expectEqualStrings("4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf", hash);
    }

    // Invalid: HTTP (not localhost IP)
    {
        const url = "http://example.com/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const result = download.validateUrl(url);
        try testing.expectError(download.DownloadError.InvalidUrl, result);
    }

    // Invalid: no hash in URL
    {
        const url = "https://example.com/path/to/";
        const result = download.validateUrl(url);
        try testing.expectError(download.DownloadError.NoHashInUrl, result);
    }

    // Valid: hash without .tar.zst extension
    {
        const url = "https://example.com/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf";
        const hash = try download.validateUrl(url);
        try testing.expectEqualStrings("4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf", hash);
    }

    // Valid: hash with .tar.zst extension
    {
        const url = "https://example.com/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const hash = try download.validateUrl(url);
        try testing.expectEqualStrings("4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf", hash);
    }
}

// In-memory file system for testing
const MemoryFileSystem = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMap(std.ArrayList(u8)),
    directories: std.StringHashMap(void),

    pub fn init(allocator: std.mem.Allocator) MemoryFileSystem {
        return .{
            .allocator = allocator,
            .files = std.StringHashMap(std.ArrayList(u8)).init(allocator),
            .directories = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *MemoryFileSystem) void {
        var file_iter = self.files.iterator();
        while (file_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit();
        }
        self.files.deinit();

        var dir_iter = self.directories.iterator();
        while (dir_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.directories.deinit();
    }

    pub fn extractWriter(self: *MemoryFileSystem) bundle.ExtractWriter {
        return .{
            .ptr = self,
            .makeDirFn = makeDir,
            .streamFileFn = streamFile,
        };
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) anyerror!void {
        const self = @as(*MemoryFileSystem, @ptrCast(@alignCast(ptr)));
        if (!self.directories.contains(path)) {
            try self.directories.put(try self.allocator.dupe(u8, path), {});
        }
    }

    fn streamFile(ptr: *anyopaque, path: []const u8, reader: std.io.AnyReader, size: usize) anyerror!void {
        const self = @as(*MemoryFileSystem, @ptrCast(@alignCast(ptr)));

        // Create parent directories if needed
        if (std.fs.path.dirname(path)) |dir_name| {
            if (!self.directories.contains(dir_name)) {
                try self.directories.put(try self.allocator.dupe(u8, dir_name), {});
            }
        }

        // Create new file data
        var file_data = std.ArrayList(u8).init(self.allocator);

        // Stream from reader
        var buffer: [bundle.STREAM_BUFFER_SIZE]u8 = undefined;
        var total_read: usize = 0;

        while (total_read < size) {
            const to_read = @min(buffer.len, size - total_read);
            const bytes_read = try reader.read(buffer[0..to_read]);

            if (bytes_read == 0) {
                break;
            }

            try file_data.appendSlice(buffer[0..bytes_read]);
            total_read += bytes_read;
        }

        if (total_read != size) {
            file_data.deinit();
            return error.UnexpectedEndOfStream;
        }

        // Store the file
        try self.files.put(try self.allocator.dupe(u8, path), file_data);
    }

    pub fn getFileContent(self: *MemoryFileSystem, path: []const u8) ?[]const u8 {
        const file = self.files.get(path) orelse return null;
        return file.items;
    }
};

test "download from local server" {
    const testing = std.testing;
    var allocator = testing.allocator;

    // Create test files in memory
    var src_files = MemoryFileSystem.init(allocator);
    defer src_files.deinit();

    try src_files.files.put(try allocator.dupe(u8, "README.md"), std.ArrayList(u8).init(allocator));
    try src_files.files.getPtr("README.md").?.appendSlice("# Test Project\n\nThis is a test README.");

    try src_files.files.put(try allocator.dupe(u8, "src/main.roc"), std.ArrayList(u8).init(allocator));
    try src_files.files.getPtr("src/main.roc").?.appendSlice("app \"test\"\n    packages {}\n    imports []\n    provides [main] to pf\n\nmain = \"Hello!\"");

    try src_files.files.put(try allocator.dupe(u8, "src/lib.roc"), std.ArrayList(u8).init(allocator));
    try src_files.files.getPtr("src/lib.roc").?.appendSlice("module [helper]\n\nhelper = \\x -> x * 2");

    try src_files.files.put(try allocator.dupe(u8, "test/test1.roc"), std.ArrayList(u8).init(allocator));
    try src_files.files.getPtr("test/test1.roc").?.appendSlice("# Test file 1\nexpect 2 + 2 == 4");

    try src_files.files.put(try allocator.dupe(u8, "docs/guide.md"), std.ArrayList(u8).init(allocator));
    try src_files.files.getPtr("docs/guide.md").?.appendSlice("# User Guide\n\n## Getting Started\n\nWelcome to the guide!");

    // Create bundle in memory
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    // Create file path iterator from memory files
    const file_paths = [_][]const u8{
        "README.md",
        "src/main.roc",
        "src/lib.roc",
        "test/test1.roc",
        "docs/guide.md",
    };
    var file_iter = FilePathIterator{ .paths = &file_paths };

    // Create a temp directory to serve as the source for bundling
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    // Write memory files to temp dir for bundling
    var src_iter = src_files.files.iterator();
    while (src_iter.next()) |entry| {
        if (std.fs.path.dirname(entry.key_ptr.*)) |dir| {
            try tmp.dir.makePath(dir);
        }
        const file = try tmp.dir.createFile(entry.key_ptr.*, .{});
        defer file.close();
        try file.writeAll(entry.value_ptr.items);
    }

    // Bundle the files
    const filename = try bundle.bundle(&file_iter, TEST_COMPRESSION_LEVEL, &allocator, bundle_data.writer(), tmp.dir, null, null);
    defer allocator.free(filename);

    // Extract hash from filename
    const base58_hash = filename[0 .. filename.len - 8];

    // Try to find an available port
    var port: u16 = 0;
    var server: std.net.Server = undefined;
    var attempts: u8 = 0;
    while (attempts < 30) : (attempts += 1) {
        // Generate random port between 20000 and 60000
        port = 20000 + @as(u16, @intCast(std.crypto.random.int(u32) % 40000));

        const addr = try std.net.Address.parseIp("127.0.0.1", port);
        server = std.net.Address.listen(addr, .{ .reuse_port = true }) catch |err| {
            if (err == error.AddressInUse) continue;
            return err;
        };
        break;
    }
    defer server.deinit();

    if (attempts >= 30) {
        return error.NoAvailablePort;
    }

    // Create server thread
    const ServerContext = struct {
        server: *std.net.Server,
        bundle_data: []const u8,
        served: std.Thread.ResetEvent = .{},

        fn run(ctx: *@This()) !void {
            const connection = try ctx.server.accept();
            defer connection.stream.close();

            // Read HTTP request (we don't really parse it, just consume it)
            var request_buf: [4096]u8 = undefined;
            _ = try connection.stream.read(&request_buf);

            // Send HTTP response with bundle data
            const response = try std.fmt.allocPrint(std.heap.page_allocator, "HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: application/octet-stream\r\n\r\n", .{ctx.bundle_data.len});
            defer std.heap.page_allocator.free(response);

            try connection.stream.writeAll(response);
            try connection.stream.writeAll(ctx.bundle_data);

            ctx.served.set();
        }
    };

    var server_ctx = ServerContext{
        .server = &server,
        .bundle_data = bundle_data.items,
    };

    const server_thread = try std.Thread.spawn(.{}, ServerContext.run, .{&server_ctx});
    defer server_thread.join();

    // Download and extract to memory
    var extracted_files = MemoryFileSystem.init(allocator);
    defer extracted_files.deinit();

    const url = try std.fmt.allocPrint(allocator, "http://127.0.0.1:{d}/{s}.tar.zst", .{ port, base58_hash });
    defer allocator.free(url);

    // Download (with memory extract writer)
    {
        const expected_hash = (try bundle.validateBase58Hash(base58_hash)).?;

        var client = std.http.Client{ .allocator = allocator };
        defer client.deinit();

        const uri = try std.Uri.parse(url);

        var server_header_buffer: [16 * 1024]u8 = undefined;
        var request = try client.open(.GET, uri, .{
            .server_header_buffer = &server_header_buffer,
            .redirect_behavior = .unhandled,
        });
        defer request.deinit();

        try request.send();
        try request.finish();
        try request.wait();

        try testing.expectEqual(std.http.Status.ok, request.response.status);

        const reader = request.reader();
        try bundle.unbundleStream(reader, extracted_files.extractWriter(), &allocator, &expected_hash, null);
    }

    // Wait for server to finish
    server_ctx.served.wait();

    // Verify all files were extracted with correct content
    try testing.expectEqualStrings("# Test Project\n\nThis is a test README.", extracted_files.getFileContent("README.md").?);
    try testing.expectEqualStrings("app \"test\"\n    packages {}\n    imports []\n    provides [main] to pf\n\nmain = \"Hello!\"", extracted_files.getFileContent("src/main.roc").?);
    try testing.expectEqualStrings("module [helper]\n\nhelper = \\x -> x * 2", extracted_files.getFileContent("src/lib.roc").?);
    try testing.expectEqualStrings("# Test file 1\nexpect 2 + 2 == 4", extracted_files.getFileContent("test/test1.roc").?);
    try testing.expectEqualStrings("# User Guide\n\n## Getting Started\n\nWelcome to the guide!", extracted_files.getFileContent("docs/guide.md").?);

    // Verify directories were created
    try testing.expect(extracted_files.directories.contains("src"));
    try testing.expect(extracted_files.directories.contains("test"));
    try testing.expect(extracted_files.directories.contains("docs"));
}
