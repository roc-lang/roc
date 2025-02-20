const std = @import("std");

// TODO: constrain this to the actual errors
pub const FileReadError = anyerror;

pub fn readFile(relative_path: []u8, allocator: std.mem.Allocator) FileReadError![]u8 {
    const file = try std.fs.cwd().openFile(relative_path, .{ .read = true });
    defer file.close();

    const max_allowed_file_length = std.math.maxInt(usize);
    const contents = try file.reader().readAllAlloc(allocator, max_allowed_file_length);

    return contents;
}

pub fn blake3Hash(data: []u8) [32]u8 {
    const digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(data, digest, .{});

    return digest;
}
