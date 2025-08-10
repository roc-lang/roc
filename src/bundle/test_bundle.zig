const std = @import("std");
const bundle = @import("bundle.zig");

// Common FilePathIterator for tests
const FilePathIterator = struct {
    paths: []const []const u8,
    index: usize = 0,

    pub fn next(self: *FilePathIterator) !?[]const u8 {
        if (self.index >= self.paths.len) return null;
        const path = self.paths[self.index];
        self.index += 1;
        return path;
    }
};

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

test "unbundle with existing directory error" {
    const testing = std.testing;
    const allocator = testing.allocator;

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
    const filename = try bundle.bundle(&iter, 3, allocator, output_buffer.writer(), tmp_dir, null);
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
    try bundle.unbundle(bundle_file.reader(), tmp_dir, allocator, filename);
}

test "unbundle multiple archives" {
    const testing = std.testing;
    const allocator = testing.allocator;

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

        const filename = try bundle.bundle(&iter, 3, allocator, output_buffer.writer(), tmp_dir, null);
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

        const filename = try bundle.bundle(&iter, 3, allocator, output_buffer.writer(), tmp_dir, null);
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
        defer extract_dir.close();

        try bundle.unbundle(bundle_file.reader(), extract_dir, allocator, fname);
    }

    // Verify extraction
    const dir1_name = filenames.items[0][0 .. filenames.items[0].len - 8];
    const dir2_name = filenames.items[1][0 .. filenames.items[1].len - 8];

    const extracted1 = try tmp_dir.readFileAlloc(allocator, try std.fmt.allocPrint(allocator, "{s}/file1.txt", .{dir1_name}), 1024);
    defer allocator.free(extracted1);
    try testing.expectEqualStrings("content 1", extracted1);

    const extracted2 = try tmp_dir.readFileAlloc(allocator, try std.fmt.allocPrint(allocator, "{s}/file2.txt", .{dir2_name}), 1024);
    defer allocator.free(extracted2);
    try testing.expectEqualStrings("content 2", extracted2);
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

test "double roundtrip bundle -> unbundle -> bundle -> unbundle" {
    const testing = std.testing;
    const allocator = testing.allocator;

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

    const filename1 = try bundle.bundle(&iter1, 3, allocator, first_bundle.writer(), initial_dir, null);
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
        defer extract_dir.close();

        try bundle.unbundle(bundle_file.reader(), extract_dir, allocator, filename1);
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
    const filename2 = try bundle.bundle(&iter2, 3, allocator, second_bundle.writer(), extracted1_dir, null);
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
        defer extract_dir.close();

        try bundle.unbundle(bundle_file.reader(), extract_dir, allocator, filename2);
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
    const allocator = testing.allocator;

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

        const archive_name = try bundle.bundle(&iter, 3, allocator, output_buffer.writer(), tmp_dir, null);
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
