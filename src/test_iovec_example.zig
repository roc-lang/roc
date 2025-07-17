//! Example program demonstrating pwritev-based serialization

const std = @import("std");
const base = @import("base.zig");
const ModuleEnv = base.ModuleEnv;
const Ident = base.Ident;
const Region = base.Region;
const iovec_serialize = base.iovec_serialize;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create a test ModuleEnv with some data
    var env = try ModuleEnv.init(allocator, "example source code\nwith multiple lines\n");
    defer env.deinit();

    // Add some identifiers
    const ident1 = try env.idents.insert(allocator, Ident.for_text("myFunction"), Region.zero());
    const ident2 = try env.idents.insert(allocator, Ident.for_text("myVariable"), Region.zero());
    const ident3 = try env.idents.insert(allocator, Ident.for_text("MyType"), Region.zero());
    _ = ident1;
    _ = ident2;
    _ = ident3;

    // Add some strings
    _ = try env.strings.insert(allocator, "Hello, World!");
    _ = try env.strings.insert(allocator, "This is a test string");
    _ = try env.strings.insert(allocator, "Another string for testing");

    // Create output file
    const file = try std.fs.cwd().createFile("test_iovec_output.bin", .{});
    defer file.close();

    // Method 1: Traditional buffer serialization
    {
        const start_time = std.time.milliTimestamp();

        const size = env.serializedSize();
        const buffer = try allocator.alloc(u8, size);
        defer allocator.free(buffer);

        const written = try env.serializeInto(buffer);
        try file.writeAll(buffer[0..written]);

        const end_time = std.time.milliTimestamp();
        std.debug.print("Traditional serialization: {} bytes in {} ms\n", .{ written, end_time - start_time });
    }

    // Reset file position
    try file.seekTo(0);

    // Method 2: IoVec-based serialization
    {
        const start_time = std.time.milliTimestamp();

        var writer = iovec_serialize.IovecWriter.init(allocator);
        defer writer.deinit();

        _ = try env.appendToIovecs(&writer);
        try writer.finalize();

        // Write using pwritev (or fallback on Windows)
        try writer.writevToFile(file, 0);

        const end_time = std.time.milliTimestamp();
        const total_size = writer.totalSize();
        std.debug.print("IoVec serialization: {} bytes in {} ms\n", .{ total_size, end_time - start_time });
        std.debug.print("Number of iovecs: {}\n", .{writer.iovecs.items.len});
    }

    // Verify both methods produce identical output
    {
        try file.seekTo(0);
        const file_size = try file.getEndPos();

        const buffer1 = try allocator.alloc(u8, file_size);
        defer allocator.free(buffer1);
        _ = try file.read(buffer1);

        // Create second file using traditional method for comparison
        const file2 = try std.fs.cwd().createFile("test_traditional_output.bin", .{});
        defer file2.close();

        const size = env.serializedSize();
        const buffer2 = try allocator.alloc(u8, size);
        defer allocator.free(buffer2);

        const written = try env.serializeInto(buffer2);
        try file2.writeAll(buffer2[0..written]);

        // Compare files
        if (std.mem.eql(u8, buffer1, buffer2[0..written])) {
            std.debug.print("✓ Both serialization methods produce identical output!\n", .{});
        } else {
            std.debug.print("✗ Serialization methods produced different output!\n", .{});
        }
    }

    // Clean up test files
    try std.fs.cwd().deleteFile("test_iovec_output.bin");
    try std.fs.cwd().deleteFile("test_traditional_output.bin");
}
