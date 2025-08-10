const std = @import("std");
const bundle = @import("bundle.zig");

test "verify bundle creates valid tar format using std.tar" {
    return error.SkipZigTest; // Skip this test for now
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create source temp directory
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create test files
    {
        const file = try src_dir.createFile("test1.txt", .{});
        defer file.close();
        try file.writeAll("Hello from test1!");
    }
    {
        const file = try src_dir.createFile("test2.txt", .{});
        defer file.close();
        try file.writeAll("This is test2 content.");
    }

    try src_dir.makePath("subdir");
    {
        const file = try src_dir.createFile("subdir/nested.txt", .{});
        defer file.close();
        try file.writeAll("Nested file content");
    }

    // Bundle to memory (without compression first, to test tar format)
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const file_paths = [_][]const u8{
        "test1.txt",
        "test2.txt",
        "subdir/nested.txt",
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

    // First, let's create an uncompressed tar to verify format
    // We'll need to modify bundle.zig to support this, or create a separate test helper
    // For now, let's test the compressed version and decompress it first

    const bundle_result = bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir);
    switch (bundle_result) {
        .success => {},
        .err => |e| {
            std.debug.print("Bundle failed: {}\n", .{e});
            return error.BundleFailed;
        },
    }

    // Decompress the data first since our bundle creates compressed tar
    const c = @cImport({
        @cInclude("zstd.h");
    });

    const dctx = c.ZSTD_createDCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeDCtx(dctx);

    // Get decompressed size (if available) or use a large buffer
    const decompressed_size = c.ZSTD_getFrameContentSize(bundle_data.items.ptr, bundle_data.items.len);
    const CONTENTSIZE_UNKNOWN = @as(c_ulonglong, @bitCast(@as(c_longlong, -1)));
    const CONTENTSIZE_ERROR = @as(c_ulonglong, @bitCast(@as(c_longlong, -2)));
    const buffer_size = if (decompressed_size == CONTENTSIZE_UNKNOWN or decompressed_size == CONTENTSIZE_ERROR)
        1024 * 1024 * 10 // 10MB should be enough for tests
    else
        decompressed_size;

    var decompressed_data = try allocator.alloc(u8, buffer_size);
    defer allocator.free(decompressed_data);

    const actual_size = c.ZSTD_decompressDCtx(
        dctx,
        decompressed_data.ptr,
        decompressed_data.len,
        bundle_data.items.ptr,
        bundle_data.items.len,
    );

    if (c.ZSTD_isError(actual_size) != 0) {
        return error.DecompressionFailed;
    }

    const tar_data = decompressed_data[0..actual_size];

    // Now use std.tar to parse and verify
    var tar_stream = std.io.fixedBufferStream(tar_data);
    var file_name_buffer: [256]u8 = undefined;
    var link_name_buffer: [256]u8 = undefined;
    var tar_iter = std.tar.iterator(tar_stream.reader(), .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    var files_found = std.StringHashMap(bool).init(allocator);
    defer files_found.deinit();

    while (try tar_iter.next()) |file| {
        const file_size = @as(usize, @intCast(file.size));
        const file_content = try allocator.alloc(u8, file_size);
        defer allocator.free(file_content);

        _ = try tar_iter.reader().readAll(file_content);

        // Verify file metadata
        try testing.expect(file.kind == .file);
        try testing.expect(file.mode == 0o644);

        // Track that we found this file
        try files_found.put(file.name, true);

        // Verify content matches what we wrote
        if (std.mem.eql(u8, file.name, "test1.txt")) {
            try testing.expectEqualStrings("Hello from test1!", file_content);
        } else if (std.mem.eql(u8, file.name, "test2.txt")) {
            try testing.expectEqualStrings("This is test2 content.", file_content);
        } else if (std.mem.eql(u8, file.name, "subdir/nested.txt")) {
            try testing.expectEqualStrings("Nested file content", file_content);
        } else {
            std.debug.print("Unexpected file: {s}\n", .{file.name});
            return error.UnexpectedFile;
        }
    }

    // Verify all expected files were found
    try testing.expect(files_found.contains("test1.txt"));
    try testing.expect(files_found.contains("test2.txt"));
    try testing.expect(files_found.contains("subdir/nested.txt"));
    try testing.expectEqual(@as(usize, 3), files_found.count());
}
