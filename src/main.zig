//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dllvm -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const fmt = @import("fmt.zig");
const base = @import("base.zig");
const collections = @import("collections.zig");
const reporting = @import("reporting.zig");
const build_options = @import("build_options");
// const coordinate = @import("coordinate.zig");
const coordinate_simple = @import("coordinate_simple.zig");

const tracy = @import("tracy.zig");
const Filesystem = @import("fs/Filesystem.zig");
const cli_args = @import("cli_args.zig");
const cache_mod = @import("cache/mod.zig");
const CacheManager = cache_mod.CacheManager;
const CacheConfig = cache_mod.CacheConfig;
const tokenize = @import("check/parse/tokenize.zig");
const parse = @import("check/parse.zig");

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
        .version => try stdout.print("Roc compiler version {s}\n", .{build_options.compiler_version}),
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

fn rocBuild(gpa: Allocator, args: cli_args.BuildArgs) !void {
    // Handle the --z-bench-tokenize flag
    if (args.z_bench_tokenize) |file_path| {
        try benchTokenizer(gpa, file_path);
        return;
    }

    // Handle the --z-bench-parse flag
    if (args.z_bench_parse) |directory_path| {
        try benchParse(gpa, directory_path);
        return;
    }

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

    const stdout = std.io.getStdOut();
    if (args.stdin) {
        fmt.formatStdin(gpa) catch std.process.exit(1);
        return;
    }

    var timer = try std.time.Timer.start();
    var elapsed: u64 = undefined;
    var failure_count: usize = 0;
    var exit_code: u8 = 0;

    if (args.check) {
        var unformatted_files = std.ArrayList([]const u8).init(gpa);
        defer unformatted_files.deinit();

        for (args.paths) |path| {
            var result = try fmt.formatPath(gpa, arena, std.fs.cwd(), path, true);
            defer result.deinit();
            if (result.unformatted_files) |files| {
                try unformatted_files.appendSlice(files.items);
            }
            failure_count += result.failure;
        }

        elapsed = timer.read();
        if (unformatted_files.items.len > 0) {
            try stdout.writer().print("The following file(s) failed `roc format --check`:\n", .{});
            for (unformatted_files.items) |file_name| {
                try stdout.writer().print("    {s}\n", .{file_name});
            }
            try stdout.writer().print("You can fix this with `roc format FILENAME.roc`.\n", .{});
            exit_code = 1;
        } else {
            try stdout.writer().print("All formatting valid\n", .{});
        }
        if (failure_count > 0) {
            try stdout.writer().print("Failed to check {} files.\n", .{failure_count});
            exit_code = 1;
        }
    } else {
        var success_count: usize = 0;
        for (args.paths) |path| {
            const result = try fmt.formatPath(gpa, arena, std.fs.cwd(), path, false);
            success_count += result.success;
            failure_count += result.failure;
        }
        elapsed = timer.read();
        try stdout.writer().print("Successfully formatted {} files\n", .{success_count});
        if (failure_count > 0) {
            try stdout.writer().print("Failed to format {} files.\n", .{failure_count});
            exit_code = 1;
        }
    }

    try stdout.writer().print("Took ", .{});
    try formatElapsedTime(stdout.writer(), elapsed);
    try stdout.writer().print(".\n", .{});

    std.process.exit(exit_code);
}

/// Helper function to format elapsed time, showing decimal milliseconds
fn formatElapsedTime(writer: anytype, elapsed_ns: u64) !void {
    const elapsed_ms_float = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    try writer.print("{d:.1} ms", .{elapsed_ms_float});
}

fn handleProcessFileError(err: anytype, stderr: anytype, path: []const u8) noreturn {
    stderr.print("Failed to check {s}: ", .{path}) catch {};
    switch (err) {
        error.FileNotFound => stderr.print("File not found\n", .{}) catch {},
        error.AccessDenied => stderr.print("Access denied\n", .{}) catch {},
        error.FileReadError => stderr.print("Could not read file\n", .{}) catch {},
        else => stderr.print("{}\n", .{err}) catch {},
    }
    std.process.exit(1);
}

fn rocCheck(gpa: Allocator, args: cli_args.CheckArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const stderr_writer = stderr.any();

    var timer = try std.time.Timer.start();

    // Initialize cache if enabled
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    var cache_manager = if (cache_config.enabled) blk: {
        const manager = CacheManager.init(gpa, cache_config, Filesystem.default());
        break :blk manager;
    } else null;

    // Process the file and get Reports
    var process_result = coordinate_simple.processFile(gpa, Filesystem.default(), args.path, if (cache_manager) |*cm| cm else null, args.time) catch |err| handleProcessFileError(err, stderr, args.path);

    defer process_result.deinit(gpa);

    const elapsed = timer.read();

    // Print cache statistics if verbose
    if (cache_manager) |*cm| {
        if (args.verbose) {
            cm.printStats(gpa);
        }
    }

    // Handle cached results vs fresh compilation results differently
    if (process_result.was_cached) {
        // For cached results, use the stored diagnostic counts
        const total_errors = process_result.error_count;
        const total_warnings = process_result.warning_count;

        if (total_errors > 0 or total_warnings > 0) {
            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                total_errors,
                total_warnings,
            }) catch {};
            formatElapsedTime(stderr, elapsed) catch {};
            stderr.print(" for {s} (note module loaded from cache, use --no-cache to display Errors and Warnings.).\n", .{args.path}) catch {};
            std.process.exit(1);
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s} (loaded from cache)\n", .{args.path}) catch {};
        }
    } else {
        // For fresh compilation, process and display reports normally
        if (process_result.reports.len > 0) {
            var fatal_errors: usize = 0;
            var runtime_errors: usize = 0;
            var warnings: usize = 0;

            // Render each report
            for (process_result.reports) |*report| {

                // Render the diagnostic report to stderr
                reporting.renderReportToTerminal(report, stderr_writer, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
                    stderr.print("Error rendering diagnostic report: {}\n", .{render_err}) catch {};
                    // Fallback to just printing the title
                    stderr.print("  {s}\n", .{report.title}) catch {};
                };

                switch (report.severity) {
                    .info => {}, // Informational messages don't affect error/warning counts
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

            std.process.exit(1);
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s}\n", .{args.path}) catch {};
        }
    }

    printTimingBreakdown(stderr, process_result.timing);
}

fn printTimingBreakdown(writer: anytype, timing: ?coordinate_simple.TimingInfo) void {
    if (timing) |t| {
        writer.print("\nTiming breakdown:\n", .{}) catch {};
        writer.print("  tokenize + parse:             ", .{}) catch {};
        formatElapsedTime(writer, t.tokenize_parse_ns) catch {};
        writer.print("\n", .{}) catch {};
        writer.print("  canonicalize:                 ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_ns) catch {};
        writer.print("\n", .{}) catch {};
        writer.print("  can diagnostics:              ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_diagnostics_ns) catch {};
        writer.print("\n", .{}) catch {};
        writer.print("  type checking:                ", .{}) catch {};
        formatElapsedTime(writer, t.type_checking_ns) catch {};
        writer.print("\n", .{}) catch {};
        writer.print("  type checking diagnostics:    ", .{}) catch {};
        formatElapsedTime(writer, t.check_diagnostics_ns) catch {};
        writer.print("\n", .{}) catch {};
    }
}

fn rocDocs(gpa: Allocator, args: cli_args.DocsArgs) !void {
    _ = gpa;
    _ = args;
    fatal("docs not implemented", .{});
}

const RocFile = struct {
    path: []const u8,
    content: []const u8,
};

const FileMetrics = struct {
    total_bytes: usize,
    total_lines: usize,
};

const BenchmarkResults = struct {
    path: []const u8,
    num_files: usize,
    total_bytes: usize,
    total_lines: usize,
    total_tokens: u64,
    num_iterations: usize,
    total_time: u64,
};

fn benchParseOrTokenize(comptime is_parse: bool, gpa: Allocator, path: []const u8) !void {
    const operation_name = if (is_parse) "parse" else "tokenizer";
    std.debug.print("Benchmarking {s} on '{s}'\n", .{ operation_name, path });

    // Find all .roc files (from file or directory)
    var roc_files = std.ArrayList(RocFile).init(gpa);
    defer {
        for (roc_files.items) |roc_file| {
            gpa.free(roc_file.path);
            gpa.free(roc_file.content);
        }
        roc_files.deinit();
    }

    try collectRocFiles(gpa, path, &roc_files);

    if (roc_files.items.len == 0) {
        std.debug.print("No .roc files found in '{s}'\n", .{path});
        return;
    }

    std.debug.print("Found {} .roc files\n", .{roc_files.items.len});

    // Calculate total metrics
    const metrics = calculateMetrics(roc_files.items);

    std.debug.print("Total: {} bytes, {} lines\n", .{ metrics.total_bytes, metrics.total_lines });

    // Create a module environment for tokenization (reused for tokenizer, created per-iteration for parser)
    var env: ?base.ModuleEnv = if (!is_parse) base.ModuleEnv.init(gpa) else null;
    defer if (env) |*e| e.deinit();

    // Benchmark parameters
    const num_iterations = 100;
    var total_time: u64 = 0;
    var total_tokens: u64 = 0;

    std.debug.print("Running {} iterations...\n", .{num_iterations});

    // Benchmark loop
    for (0..num_iterations) |_| {
        const start_time = std.time.nanoTimestamp();

        var iteration_tokens: u64 = 0;

        // Process all files in this iteration
        for (roc_files.items) |roc_file| {
            if (is_parse) {
                // Parse mode
                var parse_env = base.ModuleEnv.init(gpa);
                var ir = parse.parse(&parse_env, roc_file.content);
                iteration_tokens += ir.tokens.tokens.len;
                ir.deinit(gpa);
                parse_env.deinit();
            } else {
                // Tokenize mode
                var messages: [128]tokenize.Diagnostic = undefined;
                const msg_slice = messages[0..];

                var tokenizer = tokenize.Tokenizer.init(&env.?, roc_file.content, msg_slice);
                tokenizer.tokenize();
                var result = tokenizer.finishAndDeinit();
                iteration_tokens += result.tokens.tokens.len;
                result.tokens.deinit();
            }
        }

        const end_time = std.time.nanoTimestamp();

        total_time += @intCast(end_time - start_time);
        total_tokens = iteration_tokens;

        std.debug.print(".", .{});
    }
    std.debug.print("\n", .{});

    const results = BenchmarkResults{
        .path = path,
        .num_files = roc_files.items.len,
        .total_bytes = metrics.total_bytes,
        .total_lines = metrics.total_lines,
        .total_tokens = total_tokens,
        .num_iterations = num_iterations,
        .total_time = total_time,
    };

    const result_name = if (is_parse) "Parse" else "Tokenizer";
    printBenchmarkResults(result_name, results);
}

fn benchParse(gpa: Allocator, path: []const u8) !void {
    try benchParseOrTokenize(true, gpa, path);
}

fn collectRocFiles(gpa: Allocator, path: []const u8, roc_files: *std.ArrayList(RocFile)) !void {
    // Check if path is a file or directory
    const stat = std.fs.cwd().statFile(path) catch |err| {
        fatal("Failed to access '{s}': {}", .{ path, err });
    };

    switch (stat.kind) {
        .file => {
            if (std.mem.endsWith(u8, path, ".roc")) {
                try addRocFile(gpa, path, roc_files);
            } else {
                fatal("File '{s}' is not a .roc file", .{path});
            }
        },
        .directory => {
            try findRocFiles(gpa, path, roc_files);
        },
        else => {
            fatal("Path '{s}' is not a file or directory", .{path});
        },
    }
}

fn addRocFile(gpa: Allocator, file_path: []const u8, roc_files: *std.ArrayList(RocFile)) !void {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        std.debug.print("Warning: Failed to open file '{s}': {}\n", .{ file_path, err });
        return;
    };
    defer file.close();

    const file_size = try file.getEndPos();
    if (file_size > 0xffff_ffff) {
        std.debug.print("Warning: File '{s}' is too large to process ({} bytes), skipping\n", .{ file_path, file_size });
        return;
    }
    const file_size_usize: usize = @intCast(file_size);
    const content = try file.readToEndAlloc(gpa, file_size_usize);
    const owned_path = try gpa.dupe(u8, file_path);

    try roc_files.append(.{
        .path = owned_path,
        .content = content,
    });
}

fn findRocFiles(gpa: Allocator, dir_path: []const u8, roc_files: *std.ArrayList(RocFile)) !void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        fatal("Failed to open directory '{s}': {}", .{ dir_path, err });
    };
    defer dir.close();

    var iterator = dir.iterate();
    while (try iterator.next()) |entry| {
        const full_path = try std.fs.path.join(gpa, &[_][]const u8{ dir_path, entry.name });
        defer gpa.free(full_path);

        switch (entry.kind) {
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".roc")) {
                    try addRocFile(gpa, full_path, roc_files);
                }
            },
            .directory => {
                // Recursively search subdirectories
                try findRocFiles(gpa, full_path, roc_files);
            },
            else => {
                // Ignore other file types
            },
        }
    }
}

fn benchTokenizer(gpa: Allocator, path: []const u8) !void {
    try benchParseOrTokenize(false, gpa, path);
}

fn calculateMetrics(roc_files: []const RocFile) FileMetrics {
    var total_bytes: usize = 0;
    var total_lines: usize = 0;

    for (roc_files) |roc_file| {
        total_bytes += roc_file.content.len;
        var line_count: usize = 1;
        for (roc_file.content) |byte| {
            if (byte == '\n') {
                line_count += 1;
            }
        }
        total_lines += line_count;
    }

    return FileMetrics{
        .total_bytes = total_bytes,
        .total_lines = total_lines,
    };
}

fn printBenchmarkResults(benchmark_name: []const u8, results: BenchmarkResults) void {
    // Calculate timing statistics
    const avg_time_ns = results.total_time / results.num_iterations;
    const avg_time_us = avg_time_ns / 1000;
    const avg_time_ms = avg_time_us / 1000;

    // Calculate throughput
    const tokens_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.total_tokens)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    const bytes_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.total_bytes)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    const lines_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.total_lines)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    const files_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.num_files)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    std.debug.print("\n=== {s} Benchmark Results ===\n", .{benchmark_name});
    std.debug.print("Path: {s}\n", .{results.path});
    std.debug.print("Files: {}\n", .{results.num_files});
    std.debug.print("Total size: {} bytes\n", .{results.total_bytes});
    std.debug.print("Total lines: {}\n", .{results.total_lines});
    std.debug.print("Tokens generated: {}\n", .{results.total_tokens});
    std.debug.print("Iterations: {}\n", .{results.num_iterations});
    std.debug.print("\nTiming:\n", .{});
    std.debug.print("  Average time: {} ms ({} μs, {} ns)\n", .{ avg_time_ms, avg_time_us, avg_time_ns });
    std.debug.print("  Total time: {d:.2} ms\n", .{@as(f64, @floatFromInt(results.total_time)) / 1_000_000.0});
    std.debug.print("\nThroughput:\n", .{});
    std.debug.print("  {d:.0} files/second\n", .{files_per_second});
    std.debug.print("  {d:.0} tokens/second\n", .{tokens_per_second});
    std.debug.print("  {d:.0} lines/second\n", .{lines_per_second});
    std.debug.print("  {d:.2} MB/second\n", .{bytes_per_second / (1024.0 * 1024.0)});
    std.debug.print("  {d:.2} bytes/token\n", .{@as(f64, @floatFromInt(results.total_bytes)) / @as(f64, @floatFromInt(results.total_tokens))});
}
