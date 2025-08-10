const std = @import("std");
const bundle = @import("bundle.zig");

test "minimal bundle unbundle test" {
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
    const bundle_result = bundle.bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir);
    try testing.expect(bundle_result == .success);

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const unbundle_result = bundle.unbundle(stream.reader(), dst_dir, allocator);
    try testing.expect(unbundle_result == .success);

    // Read and verify content
    const content = try dst_dir.readFileAlloc(allocator, "test.txt", 1024);
    defer allocator.free(content);
    try testing.expectEqualStrings("Hello", content);
}
