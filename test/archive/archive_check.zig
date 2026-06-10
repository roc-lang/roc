//! Verifies that a file produced by `roc build` for an Archive target is a
//! well-formed static archive: it starts with the ar magic and contains the
//! expected member content. Used for targets whose archives we can't link and
//! run on the build host (e.g. wasm32 and cross-compiled targets).
//!
//! Usage: archive_check <path-to-archive> <expected-substring>...

const std = @import("std");

const ar_magic = "!<arch>\n";

pub fn main(init: std.process.Init) anyerror!void {
    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var arg_iter = try std.process.Args.Iterator.initAllocator(init.minimal.args, arena);
    defer arg_iter.deinit();
    _ = arg_iter.skip();
    const archive_path = arg_iter.next() orelse {
        std.debug.print("Usage: archive_check <path-to-archive> <expected-substring>...\n", .{});
        return error.MissingArchivePath;
    };

    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, archive_path, arena, .unlimited);

    if (bytes.len < ar_magic.len or !std.mem.eql(u8, bytes[0..ar_magic.len], ar_magic)) {
        std.debug.print("FAILED: {s} does not start with the ar archive magic\n", .{archive_path});
        return error.NotAnArchive;
    }

    while (arg_iter.next()) |expected| {
        if (std.mem.find(u8, bytes, expected) == null) {
            std.debug.print("FAILED: {s} does not contain \"{s}\"\n", .{ archive_path, expected });
            return error.MissingExpectedContent;
        }
    }

    std.debug.print("SUCCESS: {s} is a static archive with the expected contents\n", .{archive_path});
}
