const std = @import("std");
const fmt = @import("fmt.zig");
const parse = @import("check/parse.zig");
const base = @import("base.zig");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const RocCmd = @import("cli.zig").RocCmd;
const RocOpt = @import("cli.zig").RocOpt;

const coordinate = @import("coordinate.zig");

const usage =
    \\Usage:
    \\
    \\  roc [command] [options] [roc_file] [args]
    \\
    \\Commands:
    \\
    \\  run             Run a roc app
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
    \\ -h, --help       Print usage
;

pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    std.process.exit(1);
}

pub fn main() !void {
    const gpa = std.heap.c_allocator;

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

    if (RocCmd.parse(cmd)) |roc_command| {
        const parsed_opt = try RocOpt.parse(args[2..]);
        const opt = parsed_opt.opt;
        const cmd_args = args[(2 + parsed_opt.next_index)..];

        switch (roc_command) {
            .roc_run => try rocRun(arena, opt, cmd_args),
            .roc_build => try rocBuild(arena, opt, cmd_args),
            .roc_test => try rocTest(arena, opt, cmd_args),
            .roc_repl => try rocRepl(arena, opt, cmd_args),
            .roc_format => try rocFormat(arena, opt, cmd_args),
            .roc_version => try rocVersion(arena, cmd_args),
            .roc_check => try rocCheck(arena, opt, cmd_args),
            .roc_docs => try rocDocs(arena, opt, cmd_args),
            .roc_glue => try rocGlue(arena, opt, cmd_args),
            .roc_help => try rocHelp(),
        }
    } else if (mem.eql(u8, cmd, "-h") or mem.eql(u8, cmd, "--help")) {
        try rocHelp();
    } else {
        std.log.info("{s}", .{usage});
        fatal("unknown command: {s}", .{cmd});
    }
}

fn print_help() !void {
    try std.io.getStdOut().writeAll(usage);
    std.process.exit(0);
}

fn rocRun(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc run\n{}\n{s}\n\n", .{ opt, args });

    // TODO - check if file exists

    fatal("not implemented", .{});
}

fn rocBuild(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc build\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocTest(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc test\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocRepl(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc repl\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocFormat(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = opt;
    const path = args[0];
    const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var env = base.ModuleEnv.init(&arena);

    var parse_ast = parse.parse(&env, allocator, contents);
    defer parse_ast.deinit();

    var formatter = fmt.init(parse_ast, allocator);

    const formatted_output = formatter.formatFile();

    const new_file = try std.fs.cwd().openFile(path, .{ .mode = .write_only });
    defer new_file.close();
    _ = try new_file.write(formatted_output);
}

fn rocVersion(allocator: Allocator, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc version\n{s}\n\n", .{args});

    fatal("not implemented", .{});
}

fn rocCheck(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc check\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocDocs(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc docs\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocGlue(allocator: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = allocator;

    std.debug.print("TODO roc glue\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocHelp() !void {
    try std.io.getStdOut().writeAll(usage);
}
