const std = @import("std");
const bundle = @import("bundle.zig");
const streaming_writer = @import("streaming_writer.zig");
const streaming_reader = @import("streaming_reader.zig");
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

// Use fast compression for tests
const TEST_COMPRESSION_LEVEL: c_int = 2;

test "simple streaming write" {
    var allocator = std.testing.allocator;

    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        output.writer().any(),
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    try writer.writer().writeAll("Hello, world!");
    try writer.finish();

    // Just check we got some output
    try std.testing.expect(output.items.len > 0);
}

test "simple streaming read" {
    var allocator = std.testing.allocator;

    // First compress some data
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

    const test_data = "Hello, world! This is a test.";
    try writer.writer().writeAll(test_data);
    try writer.finish();

    const hash = writer.getHash();

    // Now decompress it
    var stream = std.io.fixedBufferStream(compressed.items);
    var allocator_copy2 = allocator;
    var reader = try streaming_reader.DecompressingHashReader.init(
        &allocator_copy2,
        stream.reader().any(),
        hash,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer reader.deinit();

    var decompressed = std.ArrayList(u8).init(allocator);
    defer decompressed.deinit();

    var buffer: [1024]u8 = undefined;
    while (true) {
        const n = try reader.reader().read(&buffer);
        if (n == 0) break;
        try decompressed.appendSlice(buffer[0..n]);
    }

    try std.testing.expectEqualStrings(test_data, decompressed.items);
}

test "streaming write with exact buffer boundary" {
    var allocator = std.testing.allocator;

    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        output.writer().any(),
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    // Write data that exactly fills the input buffer
    const buffer_size = @as(usize, @intCast(c.ZSTD_CStreamInSize()));
    const exact_data = try allocator.alloc(u8, buffer_size);
    defer allocator.free(exact_data);
    @memset(exact_data, 'X');

    try writer.writer().writeAll(exact_data);
    try writer.finish();

    // Just verify we got output
    try std.testing.expect(output.items.len > 0);
}

test "streaming read with hash mismatch" {
    var allocator = std.testing.allocator;

    // First compress some data
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

    try writer.writer().writeAll("Test data");
    try writer.finish();

    // Use wrong hash
    var wrong_hash: [32]u8 = undefined;
    @memset(&wrong_hash, 0xFF);

    // Try to decompress with wrong hash
    var stream = std.io.fixedBufferStream(compressed.items);
    var allocator_copy2 = allocator;
    var reader = try streaming_reader.DecompressingHashReader.init(
        &allocator_copy2,
        stream.reader().any(),
        wrong_hash,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer reader.deinit();

    var buffer: [1024]u8 = undefined;
    while (true) {
        const n = reader.reader().read(&buffer) catch |err| {
            try std.testing.expectEqual(err, error.HashMismatch);
            return;
        };
        if (n == 0) break;
    }

    // Should have gotten hash mismatch error
    try std.testing.expect(false);
}

test "different compression levels" {
    var allocator = std.testing.allocator;

    const test_data = "This is test data that will be compressed at different levels!";

    // Test compression levels 1 (fastest) to 22 (max compression)
    const levels = [_]c_int{ 1, 6, 12, 22 };
    var sizes: [levels.len]usize = undefined;

    for (levels, 0..) |level, i| {
        var output = std.ArrayList(u8).init(allocator);
        defer output.deinit();

        var allocator_copy = allocator;
        var writer = try streaming_writer.CompressingHashWriter.init(
            &allocator_copy,
            level,
            output.writer().any(),
            bundle.allocForZstd,
            bundle.freeForZstd,
        );
        defer writer.deinit();

        try writer.writer().writeAll(test_data);
        try writer.finish();

        sizes[i] = output.items.len;

        // Verify we can decompress
        var stream = std.io.fixedBufferStream(output.items);
        var allocator_copy2 = allocator;
        var reader = try streaming_reader.DecompressingHashReader.init(
            &allocator_copy2,
            stream.reader().any(),
            writer.getHash(),
            bundle.allocForZstd,
            bundle.freeForZstd,
        );
        defer reader.deinit();

        var decompressed = std.ArrayList(u8).init(allocator);
        defer decompressed.deinit();

        var buffer: [1024]u8 = undefined;
        while (true) {
            const n = try reader.reader().read(&buffer);
            if (n == 0) break;
            try decompressed.appendSlice(buffer[0..n]);
        }

        try std.testing.expectEqualStrings(test_data, decompressed.items);
    }

    // Higher compression levels should generally produce smaller output
    // (though not always guaranteed for small data)
    try std.testing.expect(sizes[0] >= sizes[3] or sizes[0] - sizes[3] < 10);
}

test "large file streaming extraction" {
    var allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // Create a large file (2MB)
    const large_size = 2 * 1024 * 1024;
    {
        const file = try tmp.dir.createFile("large.bin", .{});
        defer file.close();

        // Write recognizable pattern
        var buffer: [1024]u8 = undefined;
        for (&buffer, 0..) |*b, i| {
            b.* = @intCast(i % 256);
        }

        var written: usize = 0;
        while (written < large_size) : (written += buffer.len) {
            try file.writeAll(&buffer);
        }
    }

    // Bundle it
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const test_util = @import("test_util.zig");
    const paths = [_][]const u8{"large.bin"};
    var iter = test_util.FilePathIterator{ .paths = &paths };

    var allocator_copy = allocator;
    const filename = try bundle.bundle(
        &iter,
        3,
        &allocator_copy,
        bundle_data.writer(),
        tmp.dir,
        null,
        null,
    );
    defer allocator.free(filename);

    // Extract to new directory
    try tmp.dir.makeDir("extracted");
    var extract_dir = try tmp.dir.openDir("extracted", .{});

    // Unbundle - this should use streaming for the 2MB file
    var stream = std.io.fixedBufferStream(bundle_data.items);
    var allocator_copy2 = allocator;
    try bundle.unbundle(stream.reader(), extract_dir, &allocator_copy2, filename, null);

    // Verify file was extracted correctly
    const stat = try extract_dir.statFile("large.bin");
    try std.testing.expectEqual(@as(u64, large_size), stat.size);

    // Verify content pattern
    const verify_file = try extract_dir.openFile("large.bin", .{});
    defer verify_file.close();

    var verify_buffer: [1024]u8 = undefined;
    const bytes_read = try verify_file.read(&verify_buffer);

    // Check first 1KB has the expected pattern
    for (verify_buffer[0..bytes_read], 0..) |b, i| {
        try std.testing.expectEqual(@as(u8, @intCast(i % 256)), b);
    }
}
