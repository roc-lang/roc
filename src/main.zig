//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dllvm -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const fmt = @import("fmt.zig");
const base = @import("base.zig");
const cli = @import("cli.zig");
const collections = @import("collections.zig");
const coordinate = @import("coordinate.zig");
const problem_mod = @import("problem.zig");
const tracy = @import("tracy.zig");
const Filesystem = @import("coordinate/Filesystem.zig");
const eval = @import("eval/eval.zig").eval;

const _ = @import("eval/stack.zig"); // TODO this is just so stack.zig gets tested; remove it once it's actually used!

const RocCmd = cli.RocCmd;
const RocOpt = cli.RocOpt;
const Problem = problem_mod.Problem;
const Allocator = std.mem.Allocator;
const exitOnOom = collections.utils.exitOnOom;
const fatal = collections.utils.fatal;

const legalDetailsFileContent = @embedFile("legal_details");

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
    \\  licenses        Prints license info for Roc as well as attributions to other projects used by Roc.
    \\
    \\General Options:
    \\
    \\ -h, --help       Print usage
;

/// The CLI entrypoint for the Roc compiler.
pub fn main() !void {
    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa = std.heap.c_allocator;

    if (tracy.enable_allocation) {
        gpa_tracy = tracy.tracyAllocator(gpa);
        gpa = gpa_tracy.allocator();
    }

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try std.process.argsAlloc(arena);

    const result = mainArgs(gpa, arena, args);
    if (tracy.enable) {
        try tracy.waitForShutdown();
    }
    return result;
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stderr = std.io.getStdErr().writer();
    if (args.len <= 1) {
        try stderr.print("{s}", .{usage});
        fatal("expected command argument", .{});
    }

    const cmd = args[1];

    if (RocCmd.parse(cmd)) |roc_command| {
        const parsed_opt = try RocOpt.parse(args[2..]);
        const opt = parsed_opt.opt;
        const cmd_args = args[(2 + parsed_opt.next_index)..];

        switch (roc_command) {
            .roc_run => try rocRun(gpa, opt, cmd_args),
            .roc_build => try rocBuild(gpa, opt, cmd_args),
            .roc_test => try rocTest(gpa, opt, cmd_args),
            .roc_repl => try rocRepl(gpa, opt, cmd_args),
            .roc_format => try rocFormat(gpa, arena, cmd_args),
            .roc_version => try rocVersion(gpa, cmd_args),
            .roc_check => rocCheck(gpa, opt, cmd_args),
            .roc_docs => try rocDocs(gpa, opt, cmd_args),
            .roc_glue => try rocGlue(gpa, opt, cmd_args),
            .roc_help => try rocHelp(),
            .roc_licenses => try rocLicenses(),
        }
    } else if (std.mem.eql(u8, cmd, "-h") or std.mem.eql(u8, cmd, "--help")) {
        try rocHelp();
    } else {
        try stderr.print("{s}", .{usage});
        fatal("unknown command: {s}", .{cmd});
    }
}

fn rocRun(gpa: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = gpa;

    std.debug.print("TODO roc run\n{}\n{s}\n\n", .{ opt, args });

    // TODO - check if file exists

    fatal("not implemented", .{});
}

fn rocBuild(gpa: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = gpa;

    std.debug.print("TODO roc build\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocTest(gpa: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = gpa;

    std.debug.print("TODO roc test\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

fn rocRepl(gpa: Allocator, opt: RocOpt, args: []const []const u8) !void {
    _ = eval;
    _ = gpa;

    std.debug.print("TODO roc repl\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    var timer = try std.time.Timer.start();
    var count = fmt.SuccessFailCount{ .success = 0, .failure = 0 };
    if (args.len > 0) {
        for (args) |arg| {
            const inner_count = try fmt.formatPath(gpa, arena, std.fs.cwd(), arg);
            count.success += inner_count.success;
            count.failure += inner_count.failure;
        }
    } else {
        count = try fmt.formatPath(gpa, arena, std.fs.cwd(), "main.roc");
    }
    const elapsed = timer.read() / std.time.ns_per_ms;
    try std.io.getStdOut().writer().print("Successfully formatted {} files\n", .{count.success});
    if (count.failure > 0) {
        try std.io.getStdOut().writer().print("Failed to format {} files.\n", .{count.failure});
    }
    try std.io.getStdOut().writer().print("Took {} ms.\n", .{elapsed});
}

fn rocVersion(gpa: Allocator, args: []const []const u8) !void {
    _ = gpa;

    std.debug.print("TODO roc version\n{s}\n\n", .{args});

    fatal("not implemented", .{});
}

fn rocCheck(gpa: Allocator, opt: RocOpt, args: []const []const u8) void {
    _ = opt;

    const filename = if (args.len == 1) args[0] else {
        fatal("The check command expects a single filename as an argument.\n", .{});
    };

    switch (coordinate.typecheckModule(gpa, Filesystem.default(), filename)) {
        .success => |data| {
            var problems = std.ArrayList(Problem).init(gpa);
            var index_iter = data.can_irs.iterIndices();
            while (index_iter.next()) |idx| {
                const env = &data.can_irs.getWork(idx).env;
                problems.appendSlice(env.problems.items.items) catch |err| exitOnOom(err);
            }

            for (problems.items) |problem| {
                std.debug.print("{}\n", .{problem});
            }

            std.debug.print("{} problems found.\n", .{problems.items.len});
        },
        .err => |err| {
            std.debug.print("Failed to check {s}:\n{}\n", .{ filename, err });
        },
    }
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

fn rocLicenses() !void {
    try std.io.getStdOut().writeAll(legalDetailsFileContent);
    std.process.exit(0);
}

fn rocHelp() !void {
    try std.io.getStdOut().writeAll(usage);
    std.process.exit(0);
}
