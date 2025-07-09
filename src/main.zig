//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dllvm -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const fmt = @import("fmt.zig");
const base = @import("base.zig");
const collections = @import("collections.zig");
const reporting = @import("reporting.zig");
// const coordinate = @import("coordinate.zig");
const coordinate_simple = @import("coordinate_simple.zig");

const tracy = @import("tracy.zig");
const Filesystem = @import("coordinate/Filesystem.zig");
const cli_args = @import("cli_args.zig");

const Allocator = std.mem.Allocator;
const exitOnOom = collections.utils.exitOnOom;
const fatal = collections.utils.fatal;
const ColorPalette = reporting.ColorPalette;

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

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const parsed_args = cli_args.parse(gpa, args[1..]);
    defer parsed_args.deinit(gpa);
    try switch (parsed_args) {
        .run => |run_args| rocRun(gpa, run_args),
        .check => |check_args| rocCheck(gpa, check_args),
        .build => |build_args| rocBuild(gpa, build_args),
        .format => |format_args| rocFormat(gpa, arena, format_args),
        .test_cmd => |test_args| rocTest(gpa, test_args),
        .repl => rocRepl(gpa),
        .version => rocVersion(gpa),
        .docs => |docs_args| rocDocs(gpa, docs_args),
        .help => |help_message| stdout.writeAll(help_message),
        .licenses => stdout.writeAll(legalDetailsFileContent),
        .problem => |problem| {
            try switch (problem) {
                .missing_flag_value => |details| stderr.print("Error: no value was supplied for {s}\n", .{details.flag}),
                .unexpected_argument => |details| stderr.print("Error: roc {s} received an unexpected argument: `{s}`\n", .{ details.cmd, details.arg }),
                .invalid_flag_value => |details| stderr.print("Error: `{s}` is not a valid value for {s}. The valid options are {s}\n", .{ details.value, details.flag, details.valid_options }),
            };
            std.process.exit(1);
        },
    };
}

fn rocRun(gpa: Allocator, args: cli_args.RunArgs) void {
    _ = gpa;
    _ = args;
    fatal("run not implemented", .{});
}

fn rocBuild(gpa: Allocator, args: cli_args.BuildArgs) void {
    _ = gpa;
    _ = args;

    fatal("build not implemented", .{});
}

fn rocTest(gpa: Allocator, args: cli_args.TestArgs) !void {
    _ = gpa;
    _ = args;
    fatal("test not implemented", .{});
}

fn rocRepl(gpa: Allocator) !void {
    _ = gpa;
    fatal("repl not implemented", .{});
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(gpa: Allocator, arena: Allocator, args: cli_args.FormatArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var timer = try std.time.Timer.start();
    var count = fmt.SuccessFailCount{ .success = 0, .failure = 0 };
    for (args.paths) |path| {
        const inner_count = try fmt.formatPath(gpa, arena, std.fs.cwd(), path);
        count.success += inner_count.success;
        count.failure += inner_count.failure;
    }
    const elapsed = timer.read();
    try std.io.getStdOut().writer().print("Successfully formatted {} files\n", .{count.success});
    if (count.failure > 0) {
        try std.io.getStdOut().writer().print("Failed to format {} files.\n", .{count.failure});
    }
    try std.io.getStdOut().writer().print("Took ", .{});
    try formatElapsedTime(std.io.getStdOut().writer(), elapsed);
    try std.io.getStdOut().writer().print(".\n", .{});
}

fn rocVersion(gpa: Allocator) !void {
    _ = gpa;
    fatal("version not implemented", .{});
}

/// Helper function to format elapsed time, showing decimal milliseconds
fn formatElapsedTime(writer: anytype, elapsed_ns: u64) !void {
    const elapsed_ms_float = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    try writer.print("{d:.1} ms", .{elapsed_ms_float});
}

fn rocCheck(gpa: Allocator, args: cli_args.CheckArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const stderr_writer = stderr.any();

    var timer = try std.time.Timer.start();

    // Process the file and get Reports
    var result = (if (args.time)
        coordinate_simple.processFileWithTiming(gpa, Filesystem.default(), args.path)
    else
        coordinate_simple.processFile(gpa, Filesystem.default(), args.path)
    ) catch |err| {
        stderr.print("Failed to check {s}: ", .{args.path}) catch {};
        switch (err) {
            error.FileNotFound => stderr.print("File not found\n", .{}) catch {},
            error.AccessDenied => stderr.print("Access denied\n", .{}) catch {},
            error.FileReadError => stderr.print("Could not read file\n", .{}) catch {},
            else => stderr.print("{}\n", .{err}) catch {},
        }
        std.process.exit(1);
    };
    defer result.deinit(gpa);

    const elapsed = timer.read();

    // Process reports and render them using the reporting system
    if (result.reports.len > 0) {
        var fatal_errors: usize = 0;
        var runtime_errors: usize = 0;
        var warnings: usize = 0;

        // Render each report
        for (result.reports) |*report| {
            // Render the diagnostic report to stderr
            reporting.renderReportToTerminal(report, stderr_writer, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
                stderr.print("Error rendering diagnostic report: {}\n", .{render_err}) catch {};
                // Fallback to just printing the title
                stderr.print("  {s}\n", .{report.title}) catch {};
            };

            switch (report.severity) {
                .runtime_error => {
                    runtime_errors += 1;
                },
                .fatal => {
                    fatal_errors += 1;
                },
                .warning => {
                    warnings += 1;
                },
            }
        }
        stderr.writeAll("\n") catch {};

        stderr.print("Found {} error(s) and {} warning(s) in ", .{
            (fatal_errors + runtime_errors),
            warnings,
        }) catch {};
        formatElapsedTime(stderr, elapsed) catch {};
        stderr.print(" for {s}.\n", .{args.path}) catch {};
        
        // Print timing breakdown if requested
        if (args.time and result.timing != null) {
            const timing = result.timing.?;
            printTimingBreakdown(stderr, timing);
        }
        std.process.exit(1);
    } else {
        stdout.print("No errors found in ", .{}) catch {};
        formatElapsedTime(stdout, elapsed) catch {};
        stdout.print(" for {s}\n", .{args.path}) catch {};
        
        // Print timing breakdown if requested
        if (args.time and result.timing != null) {
            const timing = result.timing.?;
            printTimingBreakdown(stdout, timing);
        }
    }
}

fn printTimingBreakdown(writer: anytype, timing: anytype) void {
    writer.print("\nTiming breakdown:\n", .{}) catch {};
    writer.print("  tokenize + parse:     ", .{}) catch {};
    formatElapsedTime(writer, timing.tokenize_parse_ns) catch {};
    writer.print("\n", .{}) catch {};
    writer.print("  canonicalize:         ", .{}) catch {};
    formatElapsedTime(writer, timing.canonicalize_ns) catch {};
    writer.print("\n", .{}) catch {};
    writer.print("  can diagnostics:      ", .{}) catch {};
    formatElapsedTime(writer, timing.canonicalize_diagnostics_ns) catch {};
    writer.print("\n", .{}) catch {};
    writer.print("  checkDefs:            ", .{}) catch {};
    formatElapsedTime(writer, timing.check_defs_ns) catch {};
    writer.print("\n", .{}) catch {};
    writer.print("  check diagnostics:    ", .{}) catch {};
    formatElapsedTime(writer, timing.check_diagnostics_ns) catch {};
    writer.print("\n", .{}) catch {};
}

fn rocDocs(gpa: Allocator, args: cli_args.DocsArgs) !void {
    _ = gpa;
    _ = args;
    fatal("docs not implemented", .{});
}
