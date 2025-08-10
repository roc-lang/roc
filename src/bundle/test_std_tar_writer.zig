const std = @import("std");

test "std.tar.writer basic test" {
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
