//! Tests for streaming compression and decompression functionality
//!
//! This module contains tests that verify the correct operation of the streaming
//! compression/decompression with hash verification functionality.

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
    const allocator = std.testing.allocator;

    var output_writer: std.Io.Writer.Allocating = .init(allocator);
    defer output_writer.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        &output_writer.writer,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    try writer.interface.writeAll("Hello, world!");
    try writer.finish();
    try writer.interface.flush();

    // Just check we got some output
    var list = output_writer.toArrayList();
    defer list.deinit(allocator);
    try std.testing.expect(list.items.len > 0);
}

test "simple streaming read" {
    const allocator = std.testing.allocator;

    // First compress some data
    var compressed_writer: std.Io.Writer.Allocating = .init(allocator);
    defer compressed_writer.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        &compressed_writer.writer,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    const test_data = "Hello, world! This is a test.";
    try writer.interface.writeAll(test_data);
    try writer.finish();
    try writer.interface.flush();

    const hash = writer.getHash();
    var compressed_list = compressed_writer.toArrayList();
    defer compressed_list.deinit(allocator);

    // Now decompress it
    var stream = std.Io.Reader.fixed(compressed_list.items);
    var allocator_copy2 = allocator;
    var reader = try streaming_reader.DecompressingHashReader.init(
        &allocator_copy2,
        &stream,
        hash,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer reader.deinit();

    var decompressed_writer: std.Io.Writer.Allocating = .init(allocator);
    defer decompressed_writer.deinit();

    // Stream the data from reader to writer
    _ = try reader.interface.streamRemaining(&decompressed_writer.writer);
    try decompressed_writer.writer.flush();

    var decompressed_list = decompressed_writer.toArrayList();
    defer decompressed_list.deinit(allocator);
    try std.testing.expectEqualStrings(test_data, decompressed_list.items);
}

test "streaming write with exact buffer boundary" {
    const allocator = std.testing.allocator;

    var output_writer: std.Io.Writer.Allocating = .init(allocator);
    defer output_writer.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        &output_writer.writer,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    // Write data that exactly fills the input buffer
    const buffer_size = @as(usize, @intCast(c.ZSTD_CStreamInSize()));
    const exact_data = try allocator.alloc(u8, buffer_size);
    defer allocator.free(exact_data);
    @memset(exact_data, 'X');

    try writer.interface.writeAll(exact_data);
    try writer.finish();
    try writer.interface.flush();

    // Just verify we got output
    var list = output_writer.toArrayList();
    defer list.deinit(allocator);
    try std.testing.expect(list.items.len > 0);
}

test "streaming read with hash mismatch" {
    const allocator = std.testing.allocator;

    // First compress some data
    var compressed_writer: std.Io.Writer.Allocating = .init(allocator);
    defer compressed_writer.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        3,
        &compressed_writer.writer,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    try writer.interface.writeAll("Test data");
    try writer.finish();
    try writer.interface.flush();

    // Use wrong hash
    var wrong_hash: [32]u8 = undefined;
    @memset(&wrong_hash, 0xFF);

    // Try to decompress with wrong hash
    var compressed_list = compressed_writer.toArrayList();
    defer compressed_list.deinit(allocator);
    var stream_reader = std.Io.Reader.fixed(compressed_list.items);
    var allocator_copy2 = allocator;
    var reader = try streaming_reader.DecompressingHashReader.init(
        &allocator_copy2,
        &stream_reader,
        wrong_hash,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer reader.deinit();

    var buffer: [1024]u8 = undefined;
    while (true) {
        const n = reader.read(&buffer) catch |err| {
            try std.testing.expectEqual(err, error.HashMismatch);
            return;
        };
        if (n == 0) break;
    }

    // Should have gotten hash mismatch error
    try std.testing.expect(false);
}

test "different compression levels" {
    const allocator = std.testing.allocator;

    const test_data = "This is test data that will be compressed at different levels!";

    // Test compression levels 1 (fastest) to 22 (max compression)
    const levels = [_]c_int{ 1, 6, 12, 22 };
    var sizes: [levels.len]usize = undefined;

    for (levels, 0..) |level, i| {
        var output_writer: std.Io.Writer.Allocating = .init(allocator);
        defer output_writer.deinit();

        var allocator_copy = allocator;
        var writer = try streaming_writer.CompressingHashWriter.init(
            &allocator_copy,
            level,
            &output_writer.writer,
            bundle.allocForZstd,
            bundle.freeForZstd,
        );
        defer writer.deinit();

        try writer.interface.writeAll(test_data);
        try writer.finish();
        try writer.interface.flush();

        var output_list = output_writer.toArrayList();
        defer output_list.deinit(allocator);
        sizes[i] = output_list.items.len;

        // Verify we can decompress
        var stream_reader = std.Io.Reader.fixed(output_list.items);
        var allocator_copy2 = allocator;
        var reader = try streaming_reader.DecompressingHashReader.init(
            &allocator_copy2,
            &stream_reader,
            writer.getHash(),
            bundle.allocForZstd,
            bundle.freeForZstd,
        );
        defer reader.deinit();

        var decompressed = std.array_list.Managed(u8).init(allocator);
        defer decompressed.deinit();

        var buffer: [1024]u8 = undefined;
        while (true) {
            const n = try reader.read(&buffer);
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
    const allocator = std.testing.allocator;
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
    var bundle_writer: std.Io.Writer.Allocating = .init(allocator);
    defer bundle_writer.deinit();

    const test_util = @import("test_util.zig");
    const paths = [_][]const u8{"large.bin"};
    var iter = test_util.FilePathIterator{ .paths = &paths };

    var allocator_copy = allocator;
    const filename = try bundle.bundle(
        &iter,
        3,
        &allocator_copy,
        &bundle_writer.writer,
        tmp.dir,
        null,
        null,
    );
    defer allocator.free(filename);

    // Just verify we successfully bundled a large file
    var bundle_list = bundle_writer.toArrayList();
    defer bundle_list.deinit(allocator);
    try std.testing.expect(bundle_list.items.len > 512); // Should include header and compressed data
    // Note: Full round-trip testing with unbundle is done in integration tests
}
