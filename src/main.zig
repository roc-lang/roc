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
const parse_args = @import("args.zig");

const RocCmd = cli.RocCmd;
const RocOpt = cli.RocOpt;
const Problem = problem_mod.Problem;
const Allocator = std.mem.Allocator;
const exitOnOom = collections.utils.exitOnOom;
const fatal = collections.utils.fatal;

const legalDetailsFileContent = @embedFile("legal_details");

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

    std.debug.print("Args: {s}\n", .{args});
    const stdout = std.io.getStdOut();
    try switch (parse_args.parse(args[1..])) {
        .run => |run_args| roc_run(gpa, run_args),
        .check => |check_args| roc_check(gpa, check_args),
        .build => |build_args| roc_build(gpa, build_args),
        .format => |format_args| roc_format(gpa, arena, format_args),
        .test_cmd => |test_args| roc_test(gpa, test_args),
        .repl => roc_repl(gpa),
        .version => roc_version(gpa),
        .docs => |docs_args| roc_docs(gpa, docs_args),
        .help => |help_message| stdout.writeAll(help_message),
        .licenses => stdout.writeAll(legalDetailsFileContent),
        .invalid => |error_message| stdout.writeAll(error_message),
    };
}

fn roc_run(gpa: Allocator, args: parse_args.RunArgs) void {
    _ = gpa;
    _ = args;
    fatal("run not implemented", .{});
}

fn roc_build(gpa: Allocator, args: parse_args.BuildArgs) void {
    _ = gpa;
    _ = args;

    fatal("build not implemented", .{});
}

fn roc_test(gpa: Allocator, args: parse_args.TestArgs) !void {
    _ = gpa;
    _ = args;
    fatal("test not implemented", .{});
}

fn roc_repl(gpa: Allocator) !void {
    _ = gpa;
    fatal("repl not implemented", .{});
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn roc_format(gpa: Allocator, arena: Allocator, args: parse_args.FormatArgs) !void {
    var timer = try std.time.Timer.start();
    var count = fmt.SuccessFailCount{ .success = 0, .failure = 0 };
    for (args.paths) |path| {
        const inner_count = try fmt.formatPath(gpa, arena, std.fs.cwd(), path);
        count.success += inner_count.success;
        count.failure += inner_count.failure;
    }
    const elapsed = timer.read() / std.time.ns_per_ms;
    try std.io.getStdOut().writer().print("Successfully formatted {} files\n", .{count.success});
    if (count.failure > 0) {
        try std.io.getStdOut().writer().print("Failed to format {} files.\n", .{count.failure});
    }
    try std.io.getStdOut().writer().print("Took {} ms.\n", .{elapsed});
}

fn roc_version(gpa: Allocator) !void {
    _ = gpa;
    fatal("version not implemented", .{});
}

fn roc_check(gpa: Allocator, args: parse_args.CheckArgs) void {
    switch (coordinate.typecheckModule(gpa, Filesystem.default(), args.path)) {
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
            std.debug.print("Failed to check {s}:\n{}\n", .{ args.path, err });
        },
    }
}

fn roc_docs(gpa: Allocator, args: parse_args.DocsArgs) !void {
    _ = gpa;
    _ = args;
    fatal("docs not implemented", .{});
}
