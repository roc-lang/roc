//! Reject literal bidirectional controls in tracked repository text and paths.
const std = @import("std");
const bidi = @import("bidi");

const binary_extensions = [_][]const u8{
    ".ico", ".png", ".webp", ".jpg", ".jpeg", ".gif",   ".bin",   ".o",    ".obj",
    ".bc",  ".a",   ".lib",  ".dll", ".so",   ".dylib", ".wasm",  ".rlib", ".rmeta",
    ".pdf", ".gz",  ".zip",  ".zst", ".xz",   ".woff",  ".woff2", ".ttf",
};

/// Run the source gate, or inspect explicitly supplied files for local testing.
pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();
    const io = init.io;
    var buffer: [4096]u8 = undefined;
    var output = std.Io.File.stderr().writer(io, &buffer);
    const writer = &output.interface;
    var args = try std.process.Args.Iterator.initAllocator(init.minimal.args, gpa);
    defer args.deinit();
    _ = args.next();
    var failures: usize = 0;
    if (args.next()) |first| {
        failures += try checkFile(gpa, io, writer, first);
        while (args.next()) |path| failures += try checkFile(gpa, io, writer, path);
    } else {
        const listing = try std.process.run(gpa, io, .{ .argv = &.{ "git", "ls-files", "-z" } });
        if (listing.term != .exited or listing.term.exited != 0) return error.GitFailed;
        var paths = std.mem.splitScalar(u8, listing.stdout, 0);
        while (paths.next()) |path| {
            if (path.len != 0) failures += try checkFile(gpa, io, writer, path);
        }
    }
    try writer.print("Source bidi check: {d} forbidden controls.\n", .{failures});
    try writer.flush();
    if (failures != 0) std.process.exit(1);
}

fn checkFile(gpa: std.mem.Allocator, io: std.Io, writer: *std.Io.Writer, path: []const u8) !usize {
    var count = try scan(writer, path, path, false);
    const dir = std.Io.Dir.cwd();
    const stat = try dir.statFile(io, path, .{ .follow_symlinks = false });
    if (stat.kind == .sym_link) {
        var target: [std.fs.max_path_bytes]u8 = undefined;
        const length = try dir.readLink(io, path, &target);
        return count + try scan(writer, path, target[0..length], false);
    }
    if (stat.kind != .file) return error.ExpectedTrackedFile;
    for (binary_extensions) |extension| {
        if (std.mem.endsWith(u8, path, extension)) return count;
    }
    const bytes = try dir.readFileAlloc(io, path, gpa, .unlimited);
    defer gpa.free(bytes);
    count += try scan(writer, path, bytes, true);
    return count;
}

fn emit(writer: *std.Io.Writer, path: []const u8, offset: usize, line: usize, column: usize, control: bidi.Control) !void {
    try writer.print("{f}:{d}:{d}: forbidden U+{X:0>4} ({s}), byte {d}\n", .{ bidi.Display{ .bytes = path }, line, column, control.codepoint, control.name, offset });
}

fn scan(writer: *std.Io.Writer, path: []const u8, bytes: []const u8, comptime recognize_bom: bool) !usize {
    var width: usize = 1;
    var endian: std.builtin.Endian = .little;
    var offset: usize = 0;
    if (recognize_bom) {
        if (std.mem.startsWith(u8, bytes, "\xff\xfe\x00\x00")) {
            width = 4;
            offset = 4;
        } else if (std.mem.startsWith(u8, bytes, "\x00\x00\xfe\xff")) {
            width = 4;
            endian = .big;
            offset = 4;
        } else if (std.mem.startsWith(u8, bytes, "\xff\xfe")) {
            width = 2;
            offset = 2;
        } else if (std.mem.startsWith(u8, bytes, "\xfe\xff")) {
            width = 2;
            endian = .big;
            offset = 2;
        }
    }
    var count: usize = 0;
    var line: usize = 1;
    var column: usize = 1;
    while (offset + width <= bytes.len) : (offset += width) {
        const cp: u32 = switch (width) {
            1 => bytes[offset],
            2 => std.mem.readInt(u16, bytes[offset..][0..2], endian),
            4 => std.mem.readInt(u32, bytes[offset..][0..4], endian),
            else => unreachable,
        };
        if (width == 1) {
            if (bidi.at(bytes[offset..])) |control| {
                try emit(writer, path, offset, line, column, control);
                count += 1;
            }
        } else {
            for (bidi.controls) |control| {
                if (cp == control.codepoint) {
                    try emit(writer, path, offset, line, column, control);
                    count += 1;
                }
            }
        }
        if (cp == '\n') {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    return count;
}

test "bidi gate scans malformed and NUL containing text without truncation" {
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    var bytes: [8192]u8 = @splat('a');
    bytes[0] = 0;
    bytes[1] = 0xff;
    @memcpy(bytes[4095..][0..3], "\u{202e}");
    try std.testing.expectEqual(@as(usize, 1), try scan(&output.writer, "file\u{2066}.txt", &bytes, true));
    var iter = bidi.Iterator{ .bytes = output.written() };
    try std.testing.expect(iter.next() == null);
}

test "bidi gate handles UTF16 and UTF32 byte order marks" {
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    for (bidi.controls) |control| {
        inline for (.{ std.builtin.Endian.little, std.builtin.Endian.big }) |endian| {
            var utf16: [4]u8 = undefined;
            std.mem.writeInt(u16, utf16[0..2], 0xfeff, endian);
            std.mem.writeInt(u16, utf16[2..4], @intCast(control.codepoint), endian);
            try std.testing.expectEqual(@as(usize, 1), try scan(&output.writer, "utf16.txt", &utf16, true));
            var utf32: [8]u8 = undefined;
            std.mem.writeInt(u32, utf32[0..4], 0xfeff, endian);
            std.mem.writeInt(u32, utf32[4..8], control.codepoint, endian);
            try std.testing.expectEqual(@as(usize, 1), try scan(&output.writer, "utf32.txt", &utf32, true));
        }
    }
    try std.testing.expectEqual(@as(usize, 0), try scan(&output.writer, "safe.txt", "שלום مرحبا \\u(202E)", true));
}

test "bidi gate never interprets a path prefix as an encoding declaration" {
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    const path = "\xff\xfe\u{202e}.txt";
    try std.testing.expectEqual(@as(usize, 1), try scan(&output.writer, path, path, false));
    var iter = bidi.Iterator{ .bytes = output.written() };
    try std.testing.expect(iter.next() == null);
}
