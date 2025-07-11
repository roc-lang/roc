//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dllvm -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const fmt = @import("fmt.zig");
const base = @import("base.zig");
const collections = @import("collections.zig");
const reporting = @import("reporting.zig");
const load = @import("load/mod.zig");
const cache = @import("cache/mod.zig");
const canonicalize = @import("check/canonicalize.zig");

const tracy = @import("tracy.zig");
const Filesystem = @import("fs/Filesystem.zig");
const cli_args = @import("cli_args.zig");
const cache_mod = @import("cache/mod.zig");
const CacheManager = cache_mod.CacheManager;
const CacheConfig = cache_mod.CacheConfig;

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

    // Initialize builder configuration
    const cache_config = cache.CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    const builder_config = load.Builder.Config{
        .allocator = gpa,
        .filesystem = Filesystem.default(),
        .mode = .single_threaded,
        .cache_config = cache_config,
    };

    // Create and initialize the builder
    var builder = try load.Builder.init(builder_config);

    // Build the module
    builder.build(args.path) catch |err| {
        stderr.print("Failed to check {s}: {}\n", .{ args.path, err }) catch {};
        builder.deinit();
        std.process.exit(1);
    };

    const elapsed = timer.read();

    // Collect all diagnostics from all modules
    var all_reports = std.ArrayList(reporting.Report).init(gpa);
    defer {
        for (all_reports.items) |*report| {
            report.deinit();
        }
        all_reports.deinit();
    }

    var total_errors: u32 = 0;
    var total_warnings: u32 = 0;
    var was_cached = false;

    // Process diagnostics while builder is still alive
    {
        // Check if we have a module
        if (builder.getModule(0)) |module| {
            switch (module.phase_data) {
                .created, .parsed => {
                    // Module didn't make it past parsing
                },
                .canonicalized => |canon_data| {
                    // Count errors and warnings from canonicalization
                    total_errors += canon_data.error_count;
                    total_warnings += canon_data.warning_count;
                    was_cached = canon_data.was_cached;
                },
                .type_checked => |tc_data| {
                    // Count errors and warnings from both phases
                    total_errors += tc_data.canonicalize_error_count;
                    total_warnings += tc_data.canonicalize_warning_count;
                    total_errors += tc_data.type_error_count;
                    was_cached = false; // Type checked means fresh compilation
                },
            }

            // If loaded from cache, we're done with counting
            if (was_cached) {
                // For cached results, we don't generate detailed reports
                // The user can use --no-cache to see detailed errors
            } else {
                // For fresh compilation, get AST for detailed error reporting
                const ast = switch (module.phase_data) {
                    .created => unreachable,
                    .parsed => |data| data.ast,
                    .canonicalized => |data| data.ast,
                    .type_checked => |data| data.ast,
                };

                // Make a copy of the filename to ensure it's not freed memory
                const filename = try gpa.dupe(u8, module.module_path);
                defer gpa.free(filename);

                // Count parse errors
                const tokenize_error_count = ast.tokenize_diagnostics.items.len;
                const parse_error_count = ast.parse_diagnostics.items.len;
                total_errors += @intCast(tokenize_error_count + parse_error_count);

                // Convert tokenize diagnostics to reports
                if (tokenize_error_count > 0) {
                    for (ast.tokenize_diagnostics.items) |diagnostic| {
                        const report = ast.tokenizeDiagnosticToReport(diagnostic, gpa) catch |err| {
                            stderr.print("Error converting tokenize diagnostic to report: {}\n", .{err}) catch {};
                            continue;
                        };
                        try all_reports.append(report);
                    }
                }

                // Convert parse diagnostics to reports
                if (parse_error_count > 0) {
                    for (ast.parse_diagnostics.items) |diagnostic| {
                        const report = ast.parseDiagnosticToReport(module.env, diagnostic, gpa, filename) catch |err| {
                            stderr.print("Error converting parse diagnostic to report: {}\n", .{err}) catch {};
                            continue;
                        };
                        try all_reports.append(report);
                    }
                }

                // If there were no parse errors, convert CIR diagnostics
                if (tokenize_error_count == 0 and parse_error_count == 0 and (total_errors > 0 or total_warnings > 0)) {
                    // Get diagnostics from canonicalized or type checked phase
                    const diagnostics = switch (module.phase_data) {
                        .created, .parsed => unreachable,
                        .canonicalized => |data| data.diagnostics,
                        .type_checked => blk: {
                            // For type checked, we might want both canon and type diagnostics
                            // For now, just use empty since type errors aren't converted to reports yet
                            break :blk &[_]canonicalize.CIR.Diagnostic{};
                        },
                    };

                    const cir = switch (module.phase_data) {
                        .created, .parsed => unreachable,
                        .canonicalized => |data| data.cir,
                        .type_checked => |data| data.cir,
                    };

                    for (diagnostics) |diagnostic| {
                        // Create report with owned data to avoid dangling references
                        const report = @constCast(cir).diagnosticToReport(diagnostic, gpa, module.env.source, filename) catch |err| {
                                stderr.print("Error converting diagnostic to report: {}\n", .{err}) catch {};
                                continue;
                            };
                            try all_reports.append(report);
                    }
                }
            }
        } else {
            // No module found - file not found or other error
            stderr.print("Error: Failed to load {s}\n", .{args.path}) catch {};
            builder.deinit();
            std.process.exit(1);
        }
    }

    // If we only got to parse phase, handle parse errors
    if (builder.getModule(0)) |module| {
        if (module.phase_data == .parsed) {
            const ast = module.phase_data.parsed.ast;
            // Make a copy of the filename to ensure it's not freed memory
            const filename = try gpa.dupe(u8, module.module_path);
            defer gpa.free(filename);

            // Count parse errors
            const tokenize_error_count = ast.tokenize_diagnostics.items.len;
            const parse_error_count = ast.parse_diagnostics.items.len;
            total_errors += @intCast(tokenize_error_count + parse_error_count);

            // Convert tokenize diagnostics to reports
            if (tokenize_error_count > 0) {
                for (ast.tokenize_diagnostics.items) |diagnostic| {
                    const report = ast.tokenizeDiagnosticToReport(diagnostic, gpa) catch |err| {
                        stderr.print("Error converting tokenize diagnostic to report: {}\n", .{err}) catch {};
                        continue;
                    };
                    try all_reports.append(report);
                }
            }

            // Convert parse diagnostics to reports
            if (parse_error_count > 0) {
                for (ast.parse_diagnostics.items) |diagnostic| {
                    const report = ast.parseDiagnosticToReport(module.env, diagnostic, gpa, filename) catch |err| {
                        stderr.print("Error converting parse diagnostic to report: {}\n", .{err}) catch {};
                        continue;
                    };
                    try all_reports.append(report);
                }
            }
        }
    }

    // Print cache statistics if verbose
    if (args.verbose and cache_config.enabled) {
        builder.cache_manager.printStats(gpa);
    }

    // Display results
    if (was_cached and (total_errors > 0 or total_warnings > 0)) {
        // For cached results with errors, just show the count
        stderr.print("Found {} error(s) and {} warning(s) in ", .{
            total_errors,
            total_warnings,
        }) catch {};
        formatElapsedTime(stderr, elapsed) catch {};
        stderr.print(" for {s} (note module loaded from cache, use --no-cache to display Errors and Warnings.).\n", .{args.path}) catch {};
        std.process.exit(1);
    } else if (all_reports.items.len > 0) {
        // For fresh compilation, display the reports
        for (all_reports.items) |*report| {
            // Render the diagnostic report to stderr
            reporting.renderReportToTerminal(report, stderr_writer, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
                stderr.print("Error rendering diagnostic report: {}\n", .{render_err}) catch {};
                // Fallback to just printing the title
                stderr.print("  {s}\n", .{report.title}) catch {};
            };
        }
        stderr.writeAll("\n") catch {};

        stderr.print("Found {} error(s) and {} warning(s) in ", .{
            total_errors,
            total_warnings,
        }) catch {};
        formatElapsedTime(stderr, elapsed) catch {};
        stderr.print(" for {s}.\n", .{args.path}) catch {};
        std.process.exit(1);
    } else {
        // No errors found
        stdout.print("No errors found in ", .{}) catch {};
        formatElapsedTime(stdout, elapsed) catch {};
        const cache_status = if (was_cached) " (loaded from cache)" else "";
        stdout.print(" for {s}{s}\n", .{ args.path, cache_status }) catch {};
    }

    // Clean up the builder after we're done rendering reports
    // This must be done after rendering because reports contain references to source text
    builder.deinit();
}

fn rocDocs(gpa: Allocator, args: cli_args.DocsArgs) !void {
    _ = gpa;
    _ = args;
    fatal("docs not implemented", .{});
}
