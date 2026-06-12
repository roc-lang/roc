//! Verifies byte-level properties of build outputs: substrings that must be
//! present (e.g. archive members) and substrings that must be absent (e.g.
//! dead-code-elimination canaries that symbol-ABI links must strip).
//!
//! Usage: archive_check [--archive] <path> [--absent <substring>] <expected-substring>...

const std = @import("std");

const ar_magic = "!<arch>\n";

pub fn main(init: std.process.Init) anyerror!void {
    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var arg_iter = try std.process.Args.Iterator.initAllocator(init.minimal.args, arena);
    defer arg_iter.deinit();
    _ = arg_iter.skip();

    var require_archive = false;
    var first = arg_iter.next() orelse {
        std.debug.print("Usage: archive_check [--archive] <path> [--absent <substring>] <expected-substring>...\n", .{});
        return error.MissingPath;
    };
    if (std.mem.eql(u8, first, "--archive")) {
        require_archive = true;
        first = arg_iter.next() orelse {
            std.debug.print("Usage: archive_check [--archive] <path> [--absent <substring>] <expected-substring>...\n", .{});
            return error.MissingPath;
        };
    }
    const path = first;

    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, path, arena, .unlimited);

    if (require_archive and (bytes.len < ar_magic.len or !std.mem.eql(u8, bytes[0..ar_magic.len], ar_magic))) {
        std.debug.print("FAILED: {s} does not start with the ar archive magic\n", .{path});
        return error.NotAnArchive;
    }

    while (arg_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--absent")) {
            const pattern = arg_iter.next() orelse {
                std.debug.print("Error: --absent requires an argument\n", .{});
                return error.MissingPattern;
            };
            if (std.mem.find(u8, bytes, pattern) != null) {
                std.debug.print("FAILED: {s} contains \"{s}\", which should have been dead-code-eliminated\n", .{ path, pattern });
                return error.UnexpectedContent;
            }
            continue;
        }
        if (std.mem.find(u8, bytes, arg) == null) {
            std.debug.print("FAILED: {s} does not contain \"{s}\"\n", .{ path, arg });
            return error.MissingExpectedContent;
        }
    }

    std.debug.print("SUCCESS: {s} has the expected contents\n", .{path});
}
