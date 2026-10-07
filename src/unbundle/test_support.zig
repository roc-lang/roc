//! Small, deterministic tar.zst archives for extraction regression tests.

const std = @import("std");

pub const Entry = struct {
    name: []const u8,
    kind: enum { file, symlink, hard_link } = .file,
    data: []const u8,
};

/// The caller owns the returned bytes. Link entries use `data` as their target.
pub fn archive(allocator: std.mem.Allocator, entries: []const Entry) ![]u8 {
    var tar: std.Io.Writer.Allocating = .init(allocator);
    defer tar.deinit();
    var writer = std.tar.Writer{ .underlying_writer = &tar.writer };
    for (entries) |entry| {
        switch (entry.kind) {
            .file => try writer.writeFileBytes(entry.name, entry.data, .{ .mtime = 0 }),
            .symlink => try writer.writeLink(entry.name, entry.data, .{ .mtime = 0 }),
            .hard_link => {
                // std.tar.Writer only exposes symbolic links. Change that one
                // header's typeflag to the POSIX hard-link flag and recheck it.
                const offset = tar.written().len;
                try writer.writeLink(entry.name, entry.data, .{ .mtime = 0 });
                const header = tar.written()[offset..][0..512];
                header[156] = '1';
                @memset(header[148..156], ' ');
                var checksum: u32 = 0;
                for (header) |byte| checksum += byte;
                _ = try std.fmt.bufPrint(header[148..154], "{o:0>6}", .{checksum});
                header[154] = 0;
                header[155] = ' ';
            },
        }
    }
    try writer.finishPedantically();

    // A valid Zstandard single-segment frame with one final raw block avoids
    // requiring a compression library in these decompressor-only test suites.
    const payload = tar.written();
    std.debug.assert(payload.len <= 128 * 1024);
    const frame = try allocator.alloc(u8, 12 + payload.len);
    @memcpy(frame[0..4], "\x28\xb5\x2f\xfd");
    frame[4] = 0xa0; // Single segment, four-byte frame content size, no checksum.
    std.mem.writeInt(u32, frame[5..9], @intCast(payload.len), .little);
    std.mem.writeInt(u24, frame[9..12], (@as(u24, @intCast(payload.len)) << 3) | 1, .little);
    @memcpy(frame[12..], payload);
    return frame;
}

pub fn hash(bytes: []const u8) [32]u8 {
    var result: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(bytes, &result, .{});
    return result;
}

pub fn hashName(bytes: []const u8, buffer: *[44]u8) []const u8 {
    return @import("base58").encode(hash(bytes), buffer);
}
