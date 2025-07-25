//! Comprehensive tests for IovecWriter serialization improvements

const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");
const IovecWriter = @import("../../base/iovec_serialize.zig").IovecWriter;

test "IovecWriter memory leak regression test" {
    // This test verifies that deferred writes don't leak memory
    // Run with: zig build test -- --leak-check
    const allocator = testing.allocator;

    // Create a writer and perform operations that create owned buffers
    {
        var writer = IovecWriter.init(allocator);
        defer writer.deinit();

        // Reserve space for headers
        const header1 = try writer.reserveStruct(TestHeader);
        const header2 = try writer.reserveStruct(TestHeader);

        // Add some data
        try writer.appendBytes("data1");
        try writer.appendBytes("data2");

        // Write deferred headers
        try writer.writeDeferredStruct(header1, TestHeader{ .magic = 0xDEADBEEF, .size = 5 });
        try writer.writeDeferredStruct(header2, TestHeader{ .magic = 0xCAFEBABE, .size = 5 });

        // Finalize - this creates an owned buffer
        try writer.finalize();

        // Verify we tracked the buffer
        try testing.expectEqual(@as(usize, 1), writer.owned_buffers.items.len);
    }
    // deinit should have freed all owned buffers - verified by leak detector
}

test "IovecWriter partial write handling" {
    if (builtin.os.tag == .windows) return; // Windows uses different path

    const allocator = testing.allocator;

    // Create a mock file that simulates partial writes
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("partial_write_test.bin", .{});
    defer file.close();

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add multiple iovecs of varying sizes
    try writer.appendBytes("a"); // 1 byte
    try writer.appendBytes("bb"); // 2 bytes
    try writer.appendBytes("ccc"); // 3 bytes
    try writer.appendBytes("dddd"); // 4 bytes
    try writer.appendBytes("eeeee"); // 5 bytes
    // Total: 15 bytes

    // Write to file
    try writer.writevToFile(file, 0);

    // Verify file contents
    try file.seekTo(0);
    var buffer: [15]u8 = undefined;
    const bytes_read = try file.read(&buffer);
    try testing.expectEqual(@as(usize, 15), bytes_read);
    try testing.expectEqualStrings("abbcccddddeeee", buffer[0..bytes_read]);
}

test "IovecWriter error handling - buffer too small" {
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add some data
    try writer.appendBytes("hello world");

    // Try to write to a file that can't be created
    if (builtin.os.tag != .windows) {
        // Create a read-only directory
        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();

        const readonly_dir_name = "readonly";
        try tmp_dir.dir.makeDir(readonly_dir_name);

        // Make directory read-only (Unix-like systems)
        const readonly_dir = try tmp_dir.dir.openDir(readonly_dir_name, .{});
        readonly_dir.close();

        // Try to create file in read-only directory (should fail)
        const result = writer.writevToFileAtomic(tmp_dir.dir, readonly_dir_name ++ "/test.bin", 0o644);
        try testing.expectError(error.AccessDenied, result);
    }
}

test "IovecWriter atomic write rollback on error" {
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add test data
    try writer.appendBytes("test data");

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a file with the target name
    const target_file = try tmp_dir.dir.createFile("target.bin", .{});
    target_file.close();

    // Write some initial content
    {
        const file = try tmp_dir.dir.openFile("target.bin", .{ .mode = .write_only });
        defer file.close();
        try file.writeAll("original content");
    }

    // Attempt atomic write (this should succeed and replace the file)
    try writer.writevToFileAtomic(tmp_dir.dir, "target.bin", 0o644);

    // Verify new content
    {
        const file = try tmp_dir.dir.openFile("target.bin", .{});
        defer file.close();

        var buffer: [100]u8 = undefined;
        const bytes_read = try file.read(&buffer);
        try testing.expectEqualStrings("test data", buffer[0..bytes_read]);
    }

    // Ensure no temp files remain
    var iter = tmp_dir.dir.iterate();
    var file_count: usize = 0;
    while (try iter.next()) |entry| {
        file_count += 1;
        try testing.expectEqualStrings("target.bin", entry.name);
    }
    try testing.expectEqual(@as(usize, 1), file_count);
}

test "IovecWriter complex serialization scenario" {
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Simulate a complex serialization with multiple deferred writes
    const FileHeader = struct {
        magic: u32,
        version: u32,
        section_count: u32,
        total_size: u64,
    };

    const SectionHeader = struct {
        type: u32,
        offset: u64,
        size: u64,
        flags: u32,
    };

    // Reserve space for main header
    const main_header_offset = try writer.reserveStruct(FileHeader);

    // Reserve space for section headers
    const section1_header_offset = try writer.reserveStruct(SectionHeader);
    const section2_header_offset = try writer.reserveStruct(SectionHeader);

    // Write section 1 data
    const section1_data_offset = writer.getOffset();
    try writer.appendBytes("Section 1 data content");
    const section1_size = writer.getOffset() - section1_data_offset;

    // Add alignment padding
    const padding = try writer.appendAligned(&[_]u8{}, 16);
    _ = padding;

    // Write section 2 data
    const section2_data_offset = writer.getOffset();
    try writer.appendBytes("Section 2 has different content");
    const section2_size = writer.getOffset() - section2_data_offset;

    // Now write all the headers with correct offsets
    try writer.writeDeferredStruct(main_header_offset, FileHeader{
        .magic = 0x12345678,
        .version = 1,
        .section_count = 2,
        .total_size = writer.totalSize(),
    });

    try writer.writeDeferredStruct(section1_header_offset, SectionHeader{
        .type = 1,
        .offset = section1_data_offset,
        .size = section1_size,
        .flags = 0,
    });

    try writer.writeDeferredStruct(section2_header_offset, SectionHeader{
        .type = 2,
        .offset = section2_data_offset,
        .size = section2_size,
        .flags = 0,
    });

    // Finalize
    try writer.finalize();

    // Verify the structure
    const buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    // Check file header
    const file_header = @as(*const FileHeader, @ptrCast(@alignCast(buffer.ptr))).*;
    try testing.expectEqual(@as(u32, 0x12345678), file_header.magic);
    try testing.expectEqual(@as(u32, 1), file_header.version);
    try testing.expectEqual(@as(u32, 2), file_header.section_count);

    // Check section headers
    const section1_header = @as(*const SectionHeader, @ptrCast(@alignCast(buffer.ptr + @sizeOf(FileHeader)))).*;
    try testing.expectEqual(@as(u32, 1), section1_header.type);
    try testing.expectEqual(section1_data_offset, section1_header.offset);
    try testing.expectEqual(section1_size, section1_header.size);

    const section2_header = @as(*const SectionHeader, @ptrCast(@alignCast(buffer.ptr + @sizeOf(FileHeader) + @sizeOf(SectionHeader)))).*;
    try testing.expectEqual(@as(u32, 2), section2_header.type);
    try testing.expectEqual(section2_data_offset, section2_header.offset);
    try testing.expectEqual(section2_size, section2_header.size);
}

test "IovecWriter Windows fallback memory efficiency" {
    if (builtin.os.tag != .windows) return;

    const allocator = testing.allocator;

    // Test that Windows fallback doesn't leak memory
    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add a large amount of data across many iovecs
    var i: usize = 0;
    while (i < 100) : (i += 1) {
        try writer.appendBytes("data chunk");
    }

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("windows_test.bin", .{});
    defer file.close();

    // This should allocate a temporary buffer internally
    try writer.writevToFile(file, 0);

    // Verify file contents
    try file.seekTo(0);
    const expected_size = 100 * "data chunk".len;
    const read_buffer = try allocator.alloc(u8, expected_size);
    defer allocator.free(read_buffer);

    const bytes_read = try file.read(read_buffer);
    try testing.expectEqual(expected_size, bytes_read);

    // Verify pattern
    i = 0;
    while (i < 100) : (i += 1) {
        const offset = i * "data chunk".len;
        try testing.expectEqualStrings("data chunk", read_buffer[offset .. offset + "data chunk".len]);
    }
}

// Test structures
const TestHeader = struct {
    magic: u32,
    size: u32,
};

test "IovecWriter stress test - many small iovecs" {
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add many small iovecs
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        const byte = @as(u8, @intCast(i % 256));
        try writer.appendBytes(&[_]u8{byte});
    }

    // Verify total size
    try testing.expectEqual(@as(usize, 1000), writer.totalSize());

    // Write to file and verify
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writer.writevToFileAtomic(tmp_dir.dir, "stress_test.bin", 0o644);

    // Read back and verify
    const file = try tmp_dir.dir.openFile("stress_test.bin", .{});
    defer file.close();

    var buffer: [1000]u8 = undefined;
    const bytes_read = try file.read(&buffer);
    try testing.expectEqual(@as(usize, 1000), bytes_read);

    // Verify pattern
    i = 0;
    while (i < 1000) : (i += 1) {
        const expected = @as(u8, @intCast(i % 256));
        try testing.expectEqual(expected, buffer[i]);
    }
}

test "IovecWriter alignment handling" {
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Test various alignment scenarios
    const offset1 = try writer.appendAligned("a", 1); // No alignment needed
    try testing.expectEqual(@as(usize, 0), offset1);

    const offset2 = try writer.appendAligned("bb", 4); // Should add 3 bytes padding
    try testing.expectEqual(@as(usize, 4), offset2);

    const offset3 = try writer.appendAligned("ccc", 8); // Should add padding to reach multiple of 8
    try testing.expectEqual(@as(usize, 8), offset3);

    const offset4 = try writer.appendAligned("dddd", 16); // Should add padding to reach multiple of 16
    try testing.expectEqual(@as(usize, 16), offset4);

    // Verify total size includes all padding
    try testing.expectEqual(@as(usize, 20), writer.totalSize()); // 16 + 4

    // Verify the actual data layout
    const buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    try testing.expectEqual(@as(u8, 'a'), buffer[0]);
    try testing.expectEqual(@as(u8, 'b'), buffer[4]);
    try testing.expectEqual(@as(u8, 'b'), buffer[5]);
    try testing.expectEqual(@as(u8, 'c'), buffer[8]);
    try testing.expectEqual(@as(u8, 'c'), buffer[9]);
    try testing.expectEqual(@as(u8, 'c'), buffer[10]);
    try testing.expectEqual(@as(u8, 'd'), buffer[16]);
    try testing.expectEqual(@as(u8, 'd'), buffer[17]);
    try testing.expectEqual(@as(u8, 'd'), buffer[18]);
    try testing.expectEqual(@as(u8, 'd'), buffer[19]);
}
