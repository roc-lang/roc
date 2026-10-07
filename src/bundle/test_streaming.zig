//! Tests for streaming compression and decompression functionality
//!
//! This module contains tests that verify the correct operation of the streaming
//! compression/decompression with hash verification functionality.

const std = @import("std");
const bundle = @import("bundle.zig");
const streaming_writer = @import("streaming_writer.zig");
const c = @import("zstd");

// Use fast compression for tests
const TEST_COMPRESSION_LEVEL: c_int = 2;

/// Decompress `compressed` with Zig's zstd, the decoder the shipped unbundler
/// uses, after checking that `hash` is the BLAKE3 hash of the compressed bytes.
fn decompressVerified(allocator: std.mem.Allocator, compressed: []const u8, hash: [32]u8) error{ OutOfMemory, ReadFailed, WriteFailed, TestExpectedEqual }![]u8 {
    var actual: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(compressed, &actual, .{});
    try std.testing.expectEqualSlices(u8, &hash, &actual);

    var input = std.Io.Reader.fixed(compressed);
    const window = try allocator.alloc(u8, std.compress.zstd.default_window_len + std.compress.zstd.block_size_max);
    defer allocator.free(window);
    var decompressor = std.compress.zstd.Decompress.init(&input, window, .{});
    var decompressed: std.Io.Writer.Allocating = .init(allocator);
    errdefer decompressed.deinit();
    _ = try decompressor.reader.streamRemaining(&decompressed.writer);
    return decompressed.toOwnedSlice();
}

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
    const decompressed = try decompressVerified(allocator, compressed_list.items, hash);
    defer allocator.free(decompressed);
    try std.testing.expectEqualStrings(test_data, decompressed);
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
        const decompressed = try decompressVerified(allocator, output_list.items, writer.getHash());
        defer allocator.free(decompressed);
        try std.testing.expectEqualStrings(test_data, decompressed);
    }

    // Higher compression levels should generally produce smaller output
    // (though not always guaranteed for small data)
    try std.testing.expect(sizes[0] >= sizes[3] or sizes[0] - sizes[3] < 10);
}

test "large data roundtrip" {
    const allocator = std.testing.allocator;

    // Generate test data larger than the buffer sizes
    const large_size = 1024 * 1024;
    const large_data = try allocator.alloc(u8, large_size);
    defer allocator.free(large_data);
    for (large_data, 0..) |*b, i| {
        b.* = @intCast(i % 256);
    }

    // Compress
    var compressed_writer: std.Io.Writer.Allocating = .init(allocator);
    defer compressed_writer.deinit();

    var allocator_copy = allocator;
    var writer = try streaming_writer.CompressingHashWriter.init(
        &allocator_copy,
        TEST_COMPRESSION_LEVEL,
        &compressed_writer.writer,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer writer.deinit();

    try writer.interface.writeAll(large_data);
    try writer.finish();
    try writer.interface.flush();

    const hash = writer.getHash();
    const compressed_list = compressed_writer.written();

    // Decompress
    const decompressed = try decompressVerified(allocator, compressed_list, hash);
    defer allocator.free(decompressed);
    try std.testing.expectEqual(large_size, decompressed.len);
    try std.testing.expectEqualSlices(u8, large_data, decompressed);
}

test "large file streaming extraction" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // Create a large file (2MB)
    const large_size = 2 * 1024 * 1024;
    {
        const file = try tmp.dir.createFile(io, "large.bin", .{});
        defer file.close(io);

        // Write recognizable pattern
        var buffer: [1024]u8 = undefined;
        for (&buffer, 0..) |*b, i| {
            b.* = @intCast(i % 256);
        }

        var written: usize = 0;
        while (written < large_size) : (written += buffer.len) {
            try file.writeStreamingAll(io, &buffer);
        }
    }

    // Bundle it
    var bundle_writer: std.Io.Writer.Allocating = .init(allocator);
    defer bundle_writer.deinit();

    const test_util = @import("test_util.zig");
    const paths = [_][]const u8{"large.bin"};
    var iter = test_util.FilePathIterator{ .paths = &paths };

    var allocator_copy = allocator;
    const filename = (try bundle.bundle(
        &iter,
        3,
        &allocator_copy,
        io,
        &bundle_writer.writer,
        tmp.dir,
        null,
    )).filename;
    defer allocator.free(filename);

    // Just verify we successfully bundled a large file
    const bundle_list = bundle_writer.written();
    try std.testing.expect(bundle_list.len > 512); // Should include header and compressed data
    // Note: Full round-trip testing with unbundle is done in integration tests
}
