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
//!   --target=<name>     Target to test (default: all for platform)
//!                       Values: x64musl, arm64musl, x64glibc, arm64glibc, native
//!   --mode=<mode>       Test mode (default: all applicable)
//!                       Values: cross, native, valgrind
//!   --app=<path>        Run only one app within the selected platform
//!   --opt=<name>        Roc optimization level (default: dev)
//!                       Values: dev, interpreter
//!   --verbose           Show detailed output
//!
//! Examples:
//!   test_runner ./zig-out/bin/roc int                    # All int tests
//!   test_runner ./zig-out/bin/roc fx --target=x64musl    # fx cross-compile to x64musl
//!   test_runner ./zig-out/bin/roc str --mode=valgrind    # str under valgrind
//!   test_runner ./zig-out/bin/roc str --mode=valgrind --app=test/str/app.roc
//!   test_runner ./zig-out/bin/roc int --mode=native      # int native only
//!   test_runner ./zig-out/bin/roc fx --opt=dev           # fx with dev backend

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const platform_config = @import("platform_config.zig");
const runner_core = @import("runner_core.zig");

const PlatformConfig = platform_config.PlatformConfig;
const TestStats = runner_core.TestStats;

var debug_threaded_io_instance: std.Io.Threaded = .init_single_threaded;
/// Override the default debug IO so that `std.Options.debug_io` uses a properly
/// initialized Threaded instance with a real allocator for process spawning.
pub const std_options_debug_threaded_io: *std.Io.Threaded = &debug_threaded_io_instance;

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
    app_filter: ?[]const u8,
    opt: ?[]const u8,
    verbose: bool,
};

/// Entry point for the unified test platform runner.
pub fn main(init: std.process.Init) !void {
    // Initialize the debug IO with a real allocator for process spawning
    debug_threaded_io_instance = .init(init.gpa, .{
        .argv0 = .init(init.minimal.args),
        .environ = init.minimal.environ,
    });
    defer debug_threaded_io_instance.deinit();

    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try parseArgs(init.minimal.args);

    // Look up the platform
    const platform = platform_config.findPlatform(args.platform_name) orelse {
        std.debug.print("Error: Unknown platform '{s}'\n", .{args.platform_name});
        std.debug.print("Available platforms: int, str, fx, fx-open\n", .{});
        std.process.exit(1);
    };

    if (args.app_filter) |app_filter| {
        if (!platformContainsApp(platform, app_filter)) {
            std.debug.print("Error: App filter '{s}' does not match any app in platform '{s}'\n", .{ app_filter, platform.name });
            std.process.exit(1);
        }
    }

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
    if (args.app_filter) |app_filter| {
        std.debug.print("App filter: {s}\n", .{app_filter});
    }
    if (args.opt) |opt| {
        std.debug.print("Opt: {s}\n", .{opt});
    }
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
        std.debug.print("\nPlatform verification failed. Aborting.\n" ++
            "To regenerate host libraries, run: zig build test-platforms\n", .{});
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

                if (!appMatchesFilter(roc_file, args.app_filter)) continue;

                std.debug.print("Building {s} for {s}... ", .{ app_name, target.name });

                const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ platform.name, target.name });
                defer allocator.free(output_name);

                const result = try runner_core.crossCompile(allocator, args.roc_binary, roc_file, target.name, output_name, args.opt);
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

                var matching_count: usize = 0;
                for (specs) |spec| {
                    if (appMatchesFilter(spec.roc_file, args.app_filter)) matching_count += 1;
                }

                std.debug.print("Cross-compiling {d} tests for {s}...\n", .{ matching_count, target.name });

                var test_num: usize = 0;
                for (specs) |spec| {
                    if (!appMatchesFilter(spec.roc_file, args.app_filter)) continue;

                    test_num += 1;
                    std.debug.print("[{d}/{d}] {s}... ", .{ test_num, matching_count, spec.roc_file });

                    const basename = std.fs.path.stem(spec.roc_file);
                    const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ basename, target.name });
                    defer allocator.free(output_name);

                    const result = try runner_core.crossCompile(allocator, args.roc_binary, spec.roc_file, target.name, output_name, args.opt);
                    stats.record(result);
                }
            }
        },

        .simple_list => |specs| {
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

                var matching_count: usize = 0;
                for (specs) |spec| {
                    if (appMatchesFilter(spec.roc_file, args.app_filter)) matching_count += 1;
                }

                std.debug.print("Cross-compiling {d} tests for {s}...\n", .{ matching_count, target.name });

                var test_num: usize = 0;
                for (specs) |spec| {
                    if (!appMatchesFilter(spec.roc_file, args.app_filter)) continue;

                    test_num += 1;
                    std.debug.print("[{d}/{d}] {s}... ", .{ test_num, matching_count, spec.roc_file });

                    const basename = std.fs.path.stem(spec.roc_file);
                    const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ basename, target.name });
                    defer allocator.free(output_name);

                    const result = try runner_core.crossCompile(allocator, args.roc_binary, spec.roc_file, target.name, output_name, args.opt);
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

            if (!appMatchesFilter(roc_file, args.app_filter)) return;

            const output_name = try std.fmt.allocPrint(allocator, "{s}_native", .{platform.name});
            defer allocator.free(output_name);

            // Build
            std.debug.print("Building {s} native... ", .{app_name});
            const build_result = try runner_core.buildNative(allocator, args.roc_binary, roc_file, output_name, args.opt);
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

                    const result = try runner_core.runWithIoSpec(allocator, args.roc_binary, spec.roc_file, spec.io_spec, args.opt);
                    stats.record(result);
                }
            } else {
                // Just build and run each test
                var matching_count: usize = 0;
                for (specs) |spec| {
                    if (appMatchesFilter(spec.roc_file, args.app_filter)) matching_count += 1;
                }

                var test_num: usize = 0;
                for (specs) |spec| {
                    if (!appMatchesFilter(spec.roc_file, args.app_filter)) continue;

                    test_num += 1;
                    const basename = std.fs.path.stem(spec.roc_file);
                    const output_name = try std.fmt.allocPrint(allocator, "{s}_native", .{basename});
                    defer allocator.free(output_name);

                    std.debug.print("[{d}/{d}] Building {s}... ", .{ test_num, matching_count, spec.roc_file });
                    const build_result = try runner_core.buildNative(allocator, args.roc_binary, spec.roc_file, output_name, args.opt);
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

        .simple_list => |specs| {
            // Build and run each test (no IO spec verification)
            var matching_count: usize = 0;
            for (specs) |spec| {
                if (appMatchesFilter(spec.roc_file, args.app_filter)) matching_count += 1;
            }

            std.debug.print("Running {d} native tests...\n", .{matching_count});

            var test_num: usize = 0;
            for (specs) |spec| {
                if (!appMatchesFilter(spec.roc_file, args.app_filter)) continue;

                test_num += 1;
                const basename = std.fs.path.stem(spec.roc_file);
                const output_name = try std.fmt.allocPrint(allocator, "{s}_native", .{basename});
                defer allocator.free(output_name);

                std.debug.print("[{d}/{d}] Building {s}... ", .{ test_num, matching_count, spec.roc_file });
                const build_result = try runner_core.buildNative(allocator, args.roc_binary, spec.roc_file, output_name, args.opt);
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

            if (!appMatchesFilter(roc_file, args.app_filter)) return;

            std.debug.print("Running {s} under valgrind... ", .{app_name});
            const result = try runner_core.runWithValgrind(allocator, args.roc_binary, roc_file);
            stats.record(result);
        },

        .spec_list => |specs| {
            // For valgrind, only run tests that don't use stdin
            var valgrind_safe_count: usize = 0;
            for (specs) |spec| {
                if (std.mem.indexOf(u8, spec.io_spec, "0<") == null and appMatchesFilter(spec.roc_file, args.app_filter)) {
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

                if (!appMatchesFilter(spec.roc_file, args.app_filter)) continue;

                test_num += 1;
                std.debug.print("[{d}/{d}] {s}... ", .{ test_num, valgrind_safe_count, spec.roc_file });
                const result = try runner_core.runWithValgrind(allocator, args.roc_binary, spec.roc_file);
                stats.record(result);
            }
        },

        .simple_list => |specs| {
            // All simple tests are valgrind-safe (no stdin)
            var matching_count: usize = 0;
            for (specs) |spec| {
                if (appMatchesFilter(spec.roc_file, args.app_filter)) matching_count += 1;
            }

            std.debug.print("Running {d} valgrind tests...\n", .{matching_count});

            var test_num: usize = 0;
            for (specs) |spec| {
                if (!appMatchesFilter(spec.roc_file, args.app_filter)) continue;

                test_num += 1;
                std.debug.print("[{d}/{d}] {s}... ", .{ test_num, matching_count, spec.roc_file });
                const result = try runner_core.runWithValgrind(allocator, args.roc_binary, spec.roc_file);
                stats.record(result);
            }
        },
    }
}

fn parseArgs(process_args: std.process.Args) !Args {
    var iter = std.process.Args.Iterator.init(process_args);

    // Skip argv[0] (program name)
    _ = iter.next();

    const roc_binary = iter.next() orelse {
        printUsage();
        std.process.exit(1);
    };

    const platform_name = iter.next() orelse {
        printUsage();
        std.process.exit(1);
    };

    var args = Args{
        .roc_binary = roc_binary,
        .platform_name = platform_name,
        .target_filter = null,
        .mode = .all,
        .app_filter = null,
        .opt = null,
        .verbose = false,
    };

    // Parse options
    while (iter.next()) |arg| {
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
        } else if (std.mem.startsWith(u8, arg, "--app=")) {
            args.app_filter = arg["--app=".len..];
        } else if (std.mem.startsWith(u8, arg, "--opt=")) {
            args.opt = arg["--opt=".len..];
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

fn appMatchesFilter(roc_file: []const u8, app_filter: ?[]const u8) bool {
    const filter = app_filter orelse return true;

    return std.mem.eql(u8, roc_file, filter) or
        std.mem.eql(u8, std.fs.path.basename(roc_file), filter);
}

fn platformContainsApp(platform: PlatformConfig, app_filter: []const u8) bool {
    switch (platform.test_apps) {
        .single => |app_name| {
            var buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
            const full_path = std.fmt.bufPrint(&buf, "{s}/{s}", .{ platform.base_dir, app_name }) catch return false;
            return appMatchesFilter(full_path, app_filter);
        },
        .spec_list => |specs| {
            for (specs) |spec| {
                if (appMatchesFilter(spec.roc_file, app_filter)) return true;
            }
        },
        .simple_list => |specs| {
            for (specs) |spec| {
                if (appMatchesFilter(spec.roc_file, app_filter)) return true;
            }
        },
    }

    return false;
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
        \\  --target=<name>     Target to test (default: all for platform)
        \\                      Values: x64musl, arm64musl, x64glibc, arm64glibc, native
        \\  --mode=<mode>       Test mode (default: all applicable)
        \\                      Values: cross, native, valgrind, all
        \\  --app=<path>        Run only one app within the selected platform
        \\  --opt=<name>        Roc optimization level (default: dev)
        \\                      Values: dev, interpreter
        \\  --verbose           Show detailed output
        \\
        \\Examples:
        \\  test_runner ./zig-out/bin/roc int                    # All int tests
        \\  test_runner ./zig-out/bin/roc fx --target=x64musl    # fx cross-compile to x64musl
        \\  test_runner ./zig-out/bin/roc str --mode=valgrind    # str under valgrind
        \\  test_runner ./zig-out/bin/roc str --mode=valgrind --app=test/str/app.roc
        \\  test_runner ./zig-out/bin/roc int --mode=native      # int native only
        \\  test_runner ./zig-out/bin/roc fx --opt=dev           # fx with dev backend
        \\
    , .{});
}
