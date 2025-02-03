const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;

const usage =
    \\Usage:
    \\
    \\  roc [options] [roc_file] [args]
    \\  roc [command] [options]
    \\
    \\Commands:
    \\
    \\  build           Build a binary from the given .roc file, but don't run it
    \\  test            Run all top-level `expect`s in a main module and any modules it imports
    \\  repl            Launch the interactive Read Eval Print Loop (REPL)
    \\  format          Format a .roc file or the .roc files contained in a directory using standard Roc formatting
    \\  version         Print the Roc compiler’s version, which is currently built from commit 90db3b2db0, committed at 2025-01-28 18:26:51 UTC
    \\  check           Check the code for problems, but don’t build or run it
    \\  docs            Generate documentation for a Roc package
    \\  glue            Generate glue code between a platform's Roc API and its host language
    \\
    \\General Options:
    \\
    \\ -h, --help       Print command-specific usage
;

pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    std.process.exit(1);
}

pub fn log(
    comptime level: std.log.Level,
    comptime format: []const u8,
    args: anytype,
) void {
    const prefix = comptime level.asText();

    // Print the message to stderr, silently ignoring any errors
    std.debug.print(prefix ++ ": " ++ format ++ "\n", args);
}

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = general_purpose_allocator.deinit();
    }
    const gpa = general_purpose_allocator.allocator();

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);

    return mainArgs(gpa, arena, args);
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    _ = gpa;
    _ = arena;

    if (args.len <= 1) {
        std.log.info("{s}", .{usage});
        fatal("expected command argument", .{});
    }

    const cmd = args[1];
    // const cmd_args = args[2..];
    if (mem.eql(u8, cmd, "build")) {
        log(.info, "TODO roc build", .{});
    } else if (mem.eql(u8, cmd, "test")) {
        log(.info, "TODO roc test", .{});
    } else if (mem.eql(u8, cmd, "repl")) {
        log(.info, "TODO roc repl", .{});
    } else if (mem.eql(u8, cmd, "format")) {
        log(.info, "TODO roc format", .{});
    } else if (mem.eql(u8, cmd, "version")) {
        log(.info, "TODO roc version", .{});
    } else if (mem.eql(u8, cmd, "check")) {
        log(.info, "TODO roc check", .{});
    } else if (mem.eql(u8, cmd, "docs")) {
        log(.info, "TODO roc docs", .{});
    } else if (mem.eql(u8, cmd, "glue")) {
        log(.info, "TODO roc glue", .{});
    } else if (mem.eql(u8, cmd, "help") or mem.eql(u8, cmd, "-h") or mem.eql(u8, cmd, "--help")) {
        return std.io.getStdOut().writeAll(usage);
    }

    fatal("subcommand not yet implemented", .{});
}
