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

const RocCmd = enum {
    roc_build,
    roc_test,
    roc_repl,
    roc_format,
    roc_version,
    roc_check,
    roc_docs,
    roc_glue,
    roc_help,

    // Parse from string, return null if not found
    pub fn fromString(str: []const u8) ?RocCmd {
        inline for (std.meta.fields(RocCmd)) |field| {
            if (mem.eql(u8, str, field.name)) {
                return @enumFromInt(field.value);
            }
        }
        return null;
    }

    // Define the function type for command handlers
    const CommandFn = *const fn (allocator: Allocator, args: []const []const u8) anyerror!void;

    const table = std.static_string_map.StaticStringMap(CommandFn).initComptime(.{
        .{ "build", rocBuild },
        .{ "test", rocTest },
        .{ "repl", rocRepl },
        .{ "format", rocFormat },
        .{ "version", rocVersion },
        .{ "check", rocCheck },
        .{ "docs", rocDocs },
        .{ "glue", rocGlue },
        .{ "help", rocHelp },
    });
};

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

    if (args.len <= 1) {
        std.log.info("{s}", .{usage});
        fatal("expected command argument", .{});
    }

    const cmd = args[1];
    const cmd_args = args[2..];

    if (RocCmd.table.get(cmd)) |handler| {
        try handler(arena, cmd_args);
    } else if (std.fs.path.extension(cmd).len > 0) {
        if (!mem.eql(u8, std.fs.path.extension(cmd), ".roc")) {
            fatal("expected .roc file, got: {s}", .{cmd});
        }
        // Handle .roc file execution
        fatal("TODO: run .roc file: {s}", .{cmd});
    } else if (mem.eql(u8, cmd, "-h") or mem.eql(u8, cmd, "--help")) {
        try rocHelp(arena, cmd_args);
    } else {
        std.log.info("{s}", .{usage});
        fatal("unknown command: {s}", .{cmd});
    }
}

fn print_help() !void {
    try std.io.getStdOut().writeAll(usage);
    std.process.exit(0);
}

fn rocBuild(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc build", .{});
}

fn rocTest(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc test", .{});
}

fn rocRepl(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc repl", .{});
}

fn rocFormat(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc format", .{});
}

fn rocVersion(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc version", .{});
}

fn rocCheck(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc check", .{});
}

fn rocDocs(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc docs", .{});
}

fn rocGlue(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;
    fatal("TODO roc glue", .{});
}

fn rocHelp(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;
    _ = args;

    try std.io.getStdOut().writeAll(usage);
}
