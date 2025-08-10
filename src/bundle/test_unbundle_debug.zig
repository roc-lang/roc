const std = @import("std");
const bundle = @import("bundle.zig");

test "debug unbundle with std.tar" {
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
        try file.writeAll("Hello world");
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
    const bundle_result = bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir);
    switch (bundle_result) {
        .success => {},
        .err => |e| {
            std.debug.print("Bundle failed: {}\n", .{e});
            return error.BundleFailed;
        },
    }

    std.debug.print("Bundle size: {} bytes\n", .{bundle_data.items.len});

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const unbundle_result = bundle.unbundle(stream.reader(), dst_dir, allocator);
    switch (unbundle_result) {
        .success => {},
        .err => |e| {
            std.debug.print("Unbundle failed: {}\n", .{e});
            return error.UnbundleFailed;
        },
    }

    // Check what files were created
    var dir_iter = dst_dir.iterate();
    while (try dir_iter.next()) |entry| {
        std.debug.print("Created: {s} (type: {})\n", .{ entry.name, entry.kind });
        if (entry.kind == .file) {
            const stat = try dst_dir.statFile(entry.name);
            std.debug.print("  Size: {} bytes\n", .{stat.size});
        }
    }

    // Try to read the file with a larger buffer
    const content = try dst_dir.readFileAlloc(allocator, "test.txt", 100_000);
    defer allocator.free(content);
    std.debug.print("File content length: {} bytes\n", .{content.len});
    try testing.expectEqualStrings("Hello world", content);
}
