const std = @import("std");
const fmt = @import("fmt.zig");
const parse = @import("check/parse.zig");
const base = @import("base.zig");
const cli = @import("cli.zig");
const collections = @import("collections.zig");
const coordinate = @import("coordinate.zig");
const problem_mod = @import("problem.zig");
const Filesystem = @import("coordinate/Filesystem.zig");

const RocCmd = cli.RocCmd;
const RocOpt = cli.RocOpt;
const Problem = problem_mod.Problem;
const Allocator = std.mem.Allocator;
const exitOnOom = collections.utils.exitOnOom;

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

/// Log a fatal error and exit the process with a non-zero code.
pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.io.getStdErr().writer().print(format, args) catch unreachable;
    std.process.exit(1);
}

/// The CLI entrypoint for the Roc compiler.
pub fn main() !void {
    const gpa = std.heap.c_allocator;

    const args = try std.process.argsAlloc(gpa);
    defer gpa.free(args);

    return mainArgs(gpa, args);
}

fn mainArgs(gpa: Allocator, args: []const []const u8) !void {
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
            .roc_format => try rocFormat(gpa, cmd_args),
            .roc_version => try rocVersion(gpa, cmd_args),
            .roc_check => rocCheck(gpa, opt, cmd_args),
            .roc_docs => try rocDocs(gpa, opt, cmd_args),
            .roc_glue => try rocGlue(gpa, opt, cmd_args),
            .roc_help => try rocHelp(),
        }
    } else if (std.mem.eql(u8, cmd, "-h") or std.mem.eql(u8, cmd, "--help")) {
        try rocHelp();
    } else {
        try stderr.print("{s}", .{usage});
        fatal("unknown command: {s}", .{cmd});
    }
}

fn printHelp() !void {
    try std.io.getStdOut().writeAll(usage);
    std.process.exit(0);
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
    _ = gpa;

    std.debug.print("TODO roc repl\n{}\n{s}\n\n", .{ opt, args });

    fatal("not implemented", .{});
}

/// Reads, parses, formats, and overwrites a Roc file.
fn rocFormat(gpa: Allocator, args: []const []const u8) !void {
    const roc_file_path = if (args.len > 0) args[0] else "main.roc";

    const input_file = try std.fs.cwd().openFile(roc_file_path, .{ .mode = .read_only });
    defer input_file.close();

    const contents = try input_file.reader().readAllAlloc(gpa, std.math.maxInt(usize));
    defer gpa.free(contents);

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var parse_ast = parse.parse(&module_env, contents);
    defer parse_ast.deinit();
    if (parse_ast.errors.len > 0) {
        // TODO: pretty print the parse failures.
        fatal("Failed to parse '{s}' for formatting.\nErrors:\n{any}\n", .{ roc_file_path, parse_ast.errors });
    }

    var formatter = fmt.init(parse_ast);
    defer formatter.deinit();

    const formatted_output = formatter.formatFile();
    defer gpa.free(formatted_output);

    const output_file = try std.fs.cwd().createFile(roc_file_path, .{});
    defer output_file.close();
    try output_file.writeAll(formatted_output);
}

test "format single file" {
    const gpa = std.testing.allocator;
    const roc_filename = "test.roc";

    const roc_file = try std.fs.cwd().createFile(roc_filename, .{ .read = true });
    defer roc_file.close();
    try roc_file.writeAll(
        \\module []
        \\
        \\foo =      "bar"
    );
    defer std.fs.cwd().deleteFile(roc_filename) catch std.debug.panic("Failed to clean up test.roc", .{});

    try rocFormat(gpa, &.{roc_filename});

    // Reset file position to read formatted roc code
    try roc_file.seekTo(0);
    const formatted_code = try roc_file.reader().readAllAlloc(gpa, std.math.maxInt(usize));
    defer gpa.free(formatted_code);

    try std.testing.expectEqualStrings(
        \\module []
        \\
        \\foo = "bar"
    , formatted_code);
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

fn rocHelp() !void {
    try std.io.getStdOut().writeAll(usage);
}
