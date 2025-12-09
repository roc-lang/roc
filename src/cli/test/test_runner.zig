//! Unified test platform runner.
//!
//! This tool tests Roc test platforms with various modes:
//! - Cross-compilation to different targets
//! - Native build and execution
//! - Valgrind memory testing (Linux x86_64 only)
//! - IO spec verification (for fx platform)
//!
//! Usage:
//!   test_runner <roc_binary> <platform> [options]
//!
//! Platforms:
//!   int       - Integer operations platform
//!   str       - String processing platform
//!   fx        - Effectful platform (stdout/stderr/stdin)
//!   fx-open   - Effectful with open union errors
//!
//! Options:
//!   --target=<name>   Target to test (default: all for platform)
//!                     Values: x64musl, arm64musl, x64glibc, arm64glibc, native
//!   --mode=<mode>     Test mode (default: all applicable)
//!                     Values: cross, native, valgrind
//!   --verbose         Show detailed output
//!
//! Examples:
//!   test_runner ./zig-out/bin/roc int                    # All int tests
//!   test_runner ./zig-out/bin/roc fx --target=x64musl    # fx cross-compile to x64musl
//!   test_runner ./zig-out/bin/roc str --mode=valgrind    # str under valgrind
//!   test_runner ./zig-out/bin/roc int --mode=native      # int native only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const platform_config = @import("platform_config.zig");
const runner_core = @import("runner_core.zig");
const fx_test_specs = @import("fx_test_specs.zig");

const PlatformConfig = platform_config.PlatformConfig;
const TestStats = runner_core.TestStats;
const TestResult = runner_core.TestResult;

/// Test mode
const TestMode = enum {
    cross,
    native,
    valgrind,
    all,
};

/// Parsed command line arguments
const Args = struct {
    roc_binary: []const u8,
    platform_name: []const u8,
    target_filter: ?[]const u8,
    mode: TestMode,
    verbose: bool,
    /// Raw args buffer - caller must free via std.process.argsFree
    raw_args: [][:0]u8,
};

/// Entry point for the unified test platform runner.
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try parseArgs(allocator);
    defer std.process.argsFree(allocator, args.raw_args);

    // Look up the platform
    const platform = platform_config.findPlatform(args.platform_name) orelse {
        std.debug.print("Error: Unknown platform '{s}'\n", .{args.platform_name});
        std.debug.print("Available platforms: int, str, fx, fx-open\n", .{});
        std.process.exit(1);
    };

    // Validate target if specified
    if (args.target_filter) |target_name| {
        if (!std.mem.eql(u8, target_name, "native")) {
            if (platform_config.findTarget(platform, target_name) == null) {
                std.debug.print("Error: Target '{s}' not supported by platform '{s}'\n", .{ target_name, platform.name });
                std.debug.print("Available targets: ", .{});
                for (platform.targets, 0..) |t, i| {
                    if (i > 0) std.debug.print(", ", .{});
                    std.debug.print("{s}", .{t.name});
                }
                std.debug.print(", native\n", .{});
                std.process.exit(1);
            }
        }
    }

    // Print banner
    std.debug.print("=== Test Platform Runner ===\n", .{});
    std.debug.print("Roc binary: {s}\n", .{args.roc_binary});
    std.debug.print("Platform: {s}\n", .{platform.name});
    if (args.target_filter) |t| {
        std.debug.print("Target filter: {s}\n", .{t});
    }
    std.debug.print("Mode: {s}\n", .{@tagName(args.mode)});
    std.debug.print("\n", .{});

    var stats = TestStats{};

    // Run tests based on mode
    switch (args.mode) {
        .cross => {
            try runCrossCompileTests(allocator, args, platform, &stats);
        },
        .native => {
            try runNativeTests(allocator, args, platform, &stats);
        },
        .valgrind => {
            try runValgrindTests(allocator, args, platform, &stats);
        },
        .all => {
            // Run cross-compilation tests
            try runCrossCompileTests(allocator, args, platform, &stats);

            // Run native tests
            try runNativeTests(allocator, args, platform, &stats);
        },
    }

    // Print summary
    runner_core.printSummary(stats);

    if (stats.failed > 0) {
        std.process.exit(1);
    }
}

fn runCrossCompileTests(
    allocator: Allocator,
    args: Args,
    platform: PlatformConfig,
    stats: *TestStats,
) !void {
    runner_core.printHeader("Cross-compilation tests", .{});

    // First verify platform files exist
    std.debug.print("Verifying platform target files...\n", .{});
    var verify_failed = false;

    for (platform.targets) |target| {
        // Apply target filter
        if (args.target_filter) |filter| {
            if (!std.mem.eql(u8, target.name, filter)) continue;
        }

        // Skip glibc on non-Linux
        if (runner_core.shouldSkipTarget(target.name)) {
            runner_core.printResultLine("SKIP", target.name, "glibc requires Linux host");
            continue;
        }

        const exists = try runner_core.verifyPlatformFiles(allocator, platform.base_dir, target.name);
        if (exists) {
            runner_core.printResultLine("OK", target.name, "libhost.a exists");
        } else {
            runner_core.printResultLine("FAIL", target.name, "libhost.a missing");
            verify_failed = true;
        }
    }

    if (verify_failed) {
        std.debug.print("\nPlatform verification failed. Aborting.\n", .{});
        std.process.exit(1);
    }

    // Now run cross-compilation tests
    std.debug.print("\n", .{});

    switch (platform.test_apps) {
        .single => |app_name| {
            const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
            defer allocator.free(roc_file);

            for (platform.targets) |target| {
                // Apply target filter
                if (args.target_filter) |filter| {
                    if (!std.mem.eql(u8, target.name, filter)) continue;
                }

                // Skip glibc on non-Linux
                if (runner_core.shouldSkipTarget(target.name)) {
                    stats.record(.skipped);
                    continue;
                }

                std.debug.print("Building {s} for {s}... ", .{ app_name, target.name });

                const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ platform.name, target.name });
                defer allocator.free(output_name);

                const result = try runner_core.crossCompile(allocator, args.roc_binary, roc_file, target.name, output_name);
                stats.record(result);
            }
        },

        .spec_list => |specs| {
            for (platform.targets) |target| {
                // Apply target filter
                if (args.target_filter) |filter| {
                    if (!std.mem.eql(u8, target.name, filter)) continue;
                }

                // Skip glibc on non-Linux
                if (runner_core.shouldSkipTarget(target.name)) {
                    stats.record(.skipped);
                    continue;
                }

                std.debug.print("Cross-compiling {d} tests for {s}...\n", .{ specs.len, target.name });

                for (specs, 0..) |spec, i| {
                    const test_num = i + 1;
                    std.debug.print("[{d}/{d}] {s}... ", .{ test_num, specs.len, spec.roc_file });

                    const basename = std.fs.path.stem(spec.roc_file);
                    const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ basename, target.name });
                    defer allocator.free(output_name);

                    const result = try runner_core.crossCompile(allocator, args.roc_binary, spec.roc_file, target.name, output_name);
                    stats.record(result);
                }
            }
        },
    }
}

fn runNativeTests(
    allocator: Allocator,
    args: Args,
    platform: PlatformConfig,
    stats: *TestStats,
) !void {
    // Check if native target is filtered out
    if (args.target_filter) |filter| {
        if (!std.mem.eql(u8, filter, "native")) {
            return; // Skip native tests if a specific cross target is requested
        }
    }

    if (!platform.supports_native_exec) {
        return;
    }

    runner_core.printHeader("Native build and execution tests", .{});

    switch (platform.test_apps) {
        .single => |app_name| {
            const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
            defer allocator.free(roc_file);

            const output_name = try std.fmt.allocPrint(allocator, "{s}_native", .{platform.name});
            defer allocator.free(output_name);

            // Build
            std.debug.print("Building {s} native... ", .{app_name});
            const build_result = try runner_core.buildNative(allocator, args.roc_binary, roc_file, output_name);
            stats.record(build_result);

            if (build_result != .passed) {
                return;
            }

            // Run
            std.debug.print("Running native executable... ", .{});
            const exe_path = try std.fmt.allocPrint(allocator, "./{s}", .{output_name});
            defer allocator.free(exe_path);

            const run_result = try runner_core.runNative(allocator, exe_path);
            stats.record(run_result);

            // Cleanup
            runner_core.cleanup(output_name);
        },

        .spec_list => |specs| {
            if (platform.supports_io_specs) {
                // Use IO spec verification
                std.debug.print("Running {d} IO spec tests...\n", .{specs.len});

                for (specs, 0..) |spec, i| {
                    const test_num = i + 1;
                    std.debug.print("[{d}/{d}] {s}... ", .{ test_num, specs.len, spec.roc_file });

                    const result = try runner_core.runWithIoSpec(allocator, args.roc_binary, spec.roc_file, spec.io_spec);
                    stats.record(result);
                }
            } else {
                // Just build and run each test
                for (specs, 0..) |spec, i| {
                    const test_num = i + 1;
                    const basename = std.fs.path.stem(spec.roc_file);
                    const output_name = try std.fmt.allocPrint(allocator, "{s}_native", .{basename});
                    defer allocator.free(output_name);

                    std.debug.print("[{d}/{d}] Building {s}... ", .{ test_num, specs.len, spec.roc_file });
                    const build_result = try runner_core.buildNative(allocator, args.roc_binary, spec.roc_file, output_name);
                    stats.record(build_result);

                    if (build_result == .passed) {
                        const exe_path = try std.fmt.allocPrint(allocator, "./{s}", .{output_name});
                        defer allocator.free(exe_path);

                        std.debug.print("         Running... ", .{});
                        const run_result = try runner_core.runNative(allocator, exe_path);
                        stats.record(run_result);

                        runner_core.cleanup(output_name);
                    }
                }
            }
        },
    }
}

fn runValgrindTests(
    allocator: Allocator,
    args: Args,
    platform: PlatformConfig,
    stats: *TestStats,
) !void {
    // Valgrind only works on Linux x86_64
    if (builtin.os.tag != .linux or builtin.cpu.arch != .x86_64) {
        std.debug.print("Skipping valgrind tests (requires Linux x86_64)\n", .{});
        return;
    }

    if (!platform.valgrind_safe) {
        std.debug.print("Skipping valgrind tests for {s} (has stdin tests)\n", .{platform.name});
        return;
    }

    runner_core.printHeader("Valgrind memory tests", .{});

    switch (platform.test_apps) {
        .single => |app_name| {
            const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
            defer allocator.free(roc_file);

            std.debug.print("Running {s} under valgrind... ", .{app_name});
            const result = try runner_core.runWithValgrind(allocator, args.roc_binary, roc_file);
            stats.record(result);
        },

        .spec_list => |specs| {
            // For valgrind, only run tests that don't use stdin
            var valgrind_safe_count: usize = 0;
            for (specs) |spec| {
                if (std.mem.indexOf(u8, spec.io_spec, "0<") == null) {
                    valgrind_safe_count += 1;
                }
            }

            std.debug.print("Running {d} valgrind-safe tests...\n", .{valgrind_safe_count});

            var test_num: usize = 0;
            for (specs) |spec| {
                // Skip tests that use stdin
                if (std.mem.indexOf(u8, spec.io_spec, "0<") != null) {
                    continue;
                }

                test_num += 1;
                std.debug.print("[{d}/{d}] {s}... ", .{ test_num, valgrind_safe_count, spec.roc_file });
                const result = try runner_core.runWithValgrind(allocator, args.roc_binary, spec.roc_file);
                stats.record(result);
            }
        },
    }
}

fn parseArgs(allocator: Allocator) !Args {
    const raw_args = try std.process.argsAlloc(allocator);

    if (raw_args.len < 3) {
        printUsage();
        std.process.exit(1);
    }

    var args = Args{
        .roc_binary = raw_args[1],
        .platform_name = raw_args[2],
        .target_filter = null,
        .mode = .all,
        .verbose = false,
        .raw_args = raw_args,
    };

    // Parse options
    var i: usize = 3;
    while (i < raw_args.len) : (i += 1) {
        const arg = raw_args[i];

        if (std.mem.startsWith(u8, arg, "--target=")) {
            args.target_filter = arg["--target=".len..];
        } else if (std.mem.startsWith(u8, arg, "--mode=")) {
            const mode_str = arg["--mode=".len..];
            if (std.mem.eql(u8, mode_str, "cross")) {
                args.mode = .cross;
            } else if (std.mem.eql(u8, mode_str, "native")) {
                args.mode = .native;
            } else if (std.mem.eql(u8, mode_str, "valgrind")) {
                args.mode = .valgrind;
            } else if (std.mem.eql(u8, mode_str, "all")) {
                args.mode = .all;
            } else {
                std.debug.print("Error: Unknown mode '{s}'\n", .{mode_str});
                std.debug.print("Available modes: cross, native, valgrind, all\n", .{});
                std.process.exit(1);
            }
        } else if (std.mem.eql(u8, arg, "--verbose")) {
            args.verbose = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            printUsage();
            std.process.exit(1);
        }
    }

    return args;
}

fn printUsage() void {
    std.debug.print(
        \\Usage: test_runner <roc_binary> <platform> [options]
        \\
        \\Platforms:
        \\  int       - Integer operations platform
        \\  str       - String processing platform
        \\  fx        - Effectful platform (stdout/stderr/stdin)
        \\  fx-open   - Effectful with open union errors
        \\
        \\Options:
        \\  --target=<name>   Target to test (default: all for platform)
        \\                    Values: x64musl, arm64musl, x64glibc, arm64glibc, native
        \\  --mode=<mode>     Test mode (default: all applicable)
        \\                    Values: cross, native, valgrind, all
        \\  --verbose         Show detailed output
        \\
        \\Examples:
        \\  test_runner ./zig-out/bin/roc int                    # All int tests
        \\  test_runner ./zig-out/bin/roc fx --target=x64musl    # fx cross-compile to x64musl
        \\  test_runner ./zig-out/bin/roc str --mode=valgrind    # str under valgrind
        \\  test_runner ./zig-out/bin/roc int --mode=native      # int native only
        \\
    , .{});
}
