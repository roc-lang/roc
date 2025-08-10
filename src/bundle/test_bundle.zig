const std = @import("std");
const bundle = @import("bundle.zig");

test "bundle and unbundle roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

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
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };

    // Bundle to memory
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir, null);
    defer allocator.free(filename);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, allocator, filename);

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
    const allocator = testing.allocator;

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

    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_file.writer(), src_dir, null);
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
    try bundle.unbundle(stream.reader(), dst_dir, allocator, filename);

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
    const allocator = testing.allocator;

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
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir, null);
    defer allocator.free(filename);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, allocator, filename);

    // Read and verify content
    const content = try dst_dir.readFileAlloc(allocator, "test.txt", 1024);
    defer allocator.free(content);
    try testing.expectEqualStrings("Hello", content);
}

test "bundle with path prefix stripping" {
    const testing = std.testing;
    const allocator = testing.allocator;

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

    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };

    // Bundle with prefix "foo/bar/src/"
    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir, "foo/bar/src/");
    defer allocator.free(filename);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, allocator, filename);

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
    const allocator = testing.allocator;

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
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir, null);
    defer allocator.free(filename);

    // Verify filename ends with .tar.zst
    try testing.expect(std.mem.endsWith(u8, filename, ".tar.zst"));

    // Create destination directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle with correct filename - should succeed
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try bundle.unbundle(stream.reader(), dst_dir, allocator, filename);

    // Verify content
    const content = try dst_dir.readFileAlloc(allocator, "test.txt", 1024);
    defer allocator.free(content);
    try testing.expectEqualStrings("Test content for hash verification", content);
}

test "blake3 hash verification failure" {
    const testing = std.testing;
    const allocator = testing.allocator;

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
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir, null);
    defer allocator.free(filename);

    // Create destination directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Try to unbundle with wrong filename - should fail
    const wrong_filename = "1234567890abcdef.tar.zst";
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const result = bundle.unbundle(stream.reader(), dst_dir, allocator, wrong_filename);

    try testing.expectError(error.InvalidFilename, result);
}

test "base58 encode/decode roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Test various inputs
    const test_cases = [_][]const u8{
        "",
        "\x00",
        "\x00\x00",
        "Hello",
        "The quick brown fox jumps over the lazy dog",
        "\xFF\xFF\xFF\xFF",
    };

    for (test_cases) |input| {
        const encoded = try bundle.base58Encode(allocator, input);
        defer allocator.free(encoded);

        const decoded = try bundle.base58Decode(allocator, encoded);
        defer allocator.free(decoded);

        try testing.expectEqualSlices(u8, input, decoded);
    }
}

test "blake3 hash detects corruption" {
    const testing = std.testing;
    const allocator = testing.allocator;

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
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const filename = try bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir, null);
    defer allocator.free(filename);

    // Corrupt the data by flipping a bit in the middle
    if (bundle_data.items.len > 100) {
        bundle_data.items[bundle_data.items.len / 2] ^= 0x01;
    }

    // Create destination directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Try to unbundle corrupted data - should fail with HashMismatch
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const result = bundle.unbundle(stream.reader(), dst_dir, allocator, filename);

    try testing.expectError(error.HashMismatch, result);
}
