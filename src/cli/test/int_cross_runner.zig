//! Cross-compilation test runner for int platform tests.
//!
//! This standalone binary tests cross-compilation of the int platform test app
//! to various targets. It:
//! 1. Verifies target directories have required files (libhost.a, etc.)
//! 2. Cross-compiles test/int/app.roc using `roc build --target=<target>`
//! 3. Reports success/failure for each compilation
//! 4. Optionally runs the native build and verifies output
//!
//! Usage:
//!   int_cross_runner <roc_binary_path> [target]
//!
//! Examples:
//!   int_cross_runner ./zig-out/bin/roc           # Test all targets
//!   int_cross_runner ./zig-out/bin/roc x64musl   # Test specific target

const std = @import("std");
const builtin = @import("builtin");

const INT_APP = "test/int/app.roc";
const PLATFORM_DIR = "test/int/platform/targets";

/// All supported cross-compilation targets
const all_targets = [_][]const u8{
    "x64musl",
    "arm64musl",
    "x64glibc",
    "arm64glibc",
};

/// Entry point for the cross-compilation test runner.
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: int_cross_runner <roc_binary_path> [target]\n", .{});
        std.debug.print("Examples:\n", .{});
        std.debug.print("  int_cross_runner ./zig-out/bin/roc           # Test all targets\n", .{});
        std.debug.print("  int_cross_runner ./zig-out/bin/roc x64musl   # Test specific target\n", .{});
        std.process.exit(1);
    }

    const roc_binary = args[1];
    const target_filter: ?[]const u8 = if (args.len > 2) args[2] else null;

    std.debug.print("=== Int Platform Cross-Compilation Test Runner ===\n", .{});
    std.debug.print("Roc binary: {s}\n", .{roc_binary});
    std.debug.print("Test app: {s}\n", .{INT_APP});
    if (target_filter) |filter| {
        std.debug.print("Target filter: {s}\n", .{filter});
    }
    std.debug.print("\n", .{});

    // First verify platform files exist
    std.debug.print(">>> Verifying platform target files\n", .{});
    var verified: usize = 0;
    var verify_failed: usize = 0;

    for (all_targets) |target| {
        if (target_filter) |filter| {
            if (!std.mem.eql(u8, target, filter)) continue;
        }

        // Skip glibc on non-Linux
        if (std.mem.indexOf(u8, target, "glibc") != null and builtin.os.tag != .linux) {
            std.debug.print("[SKIP] {s} (glibc requires Linux host)\n", .{target});
            continue;
        }

        const target_dir = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ PLATFORM_DIR, target });
        defer allocator.free(target_dir);

        const libhost_path = try std.fmt.allocPrint(allocator, "{s}/libhost.a", .{target_dir});
        defer allocator.free(libhost_path);

        if (std.fs.cwd().access(libhost_path, .{})) |_| {
            std.debug.print("[OK] {s}/libhost.a exists\n", .{target});
            verified += 1;
        } else |_| {
            std.debug.print("[FAIL] {s}/libhost.a missing\n", .{target});
            verify_failed += 1;
        }
    }

    if (verify_failed > 0) {
        std.debug.print("\nPlatform verification failed. Aborting.\n", .{});
        std.process.exit(1);
    }

    // Now run cross-compilation tests
    std.debug.print("\n>>> Testing cross-compilation\n", .{});
    var passed: usize = 0;
    var failed: usize = 0;
    var skipped: usize = 0;

    for (all_targets) |target| {
        if (target_filter) |filter| {
            if (!std.mem.eql(u8, target, filter)) continue;
        }

        // Skip glibc on non-Linux
        if (std.mem.indexOf(u8, target, "glibc") != null and builtin.os.tag != .linux) {
            skipped += 1;
            continue; // Already printed skip message above
        }

        std.debug.print("Building {s}... ", .{target});

        const output_name = try std.fmt.allocPrint(allocator, "int_app_{s}", .{target});
        defer allocator.free(output_name);

        const target_arg = try std.fmt.allocPrint(allocator, "--target={s}", .{target});
        defer allocator.free(target_arg);

        const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_name});
        defer allocator.free(output_arg);

        // Run roc build --target=<target> --output=<output> test/int/app.roc
        const result = std.process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{
                roc_binary,
                "build",
                target_arg,
                output_arg,
                INT_APP,
            },
        }) catch |err| {
            std.debug.print("FAIL (spawn error: {})\n", .{err});
            failed += 1;
            continue;
        };
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        switch (result.term) {
            .Exited => |code| {
                if (code == 0) {
                    // Verify executable was created
                    if (std.fs.cwd().access(output_name, .{})) |_| {
                        std.debug.print("OK\n", .{});
                        passed += 1;
                        // Clean up
                        std.fs.cwd().deleteFile(output_name) catch {};
                    } else |_| {
                        std.debug.print("FAIL (executable not created)\n", .{});
                        failed += 1;
                    }
                } else {
                    std.debug.print("FAIL (exit code {d})\n", .{code});
                    if (result.stderr.len > 0) {
                        var lines = std.mem.splitScalar(u8, result.stderr, '\n');
                        var line_count: usize = 0;
                        while (lines.next()) |line| {
                            if (line_count >= 5) {
                                std.debug.print("       ... (truncated)\n", .{});
                                break;
                            }
                            if (line.len > 0) {
                                std.debug.print("       {s}\n", .{line});
                                line_count += 1;
                            }
                        }
                    }
                    failed += 1;
                }
            },
            .Signal => |sig| {
                std.debug.print("FAIL (signal {d})\n", .{sig});
                failed += 1;
            },
            else => {
                std.debug.print("FAIL (abnormal termination)\n", .{});
                failed += 1;
            },
        }
    }

    // Test native build and execution
    std.debug.print("\n>>> Testing native build and execution\n", .{});

    const native_output = "int_app_native";
    const native_output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{native_output});
    defer allocator.free(native_output_arg);

    std.debug.print("Building native... ", .{});
    const native_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary,
            "build",
            native_output_arg,
            INT_APP,
        },
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        failed += 1;
        printSummary(passed, failed, skipped);
        std.process.exit(1);
    };
    defer allocator.free(native_result.stdout);
    defer allocator.free(native_result.stderr);

    if (native_result.term != .Exited or native_result.term.Exited != 0) {
        std.debug.print("FAIL\n", .{});
        if (native_result.stderr.len > 0) {
            std.debug.print("       {s}\n", .{native_result.stderr[0..@min(native_result.stderr.len, 200)]});
        }
        failed += 1;
        printSummary(passed, failed, skipped);
        std.process.exit(1);
    }
    std.debug.print("OK\n", .{});
    passed += 1;

    // Run the native executable
    std.debug.print("Running native executable... ", .{});
    const native_exe_path = try std.fmt.allocPrint(allocator, "./{s}", .{native_output});
    defer allocator.free(native_exe_path);
    const run_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{native_exe_path},
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        std.fs.cwd().deleteFile(native_output) catch {};
        failed += 1;
        printSummary(passed, failed, skipped);
        std.process.exit(1);
    };
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    if (run_result.term == .Exited and run_result.term.Exited == 0) {
        std.debug.print("OK\n", .{});
        passed += 1;
        // Print first few lines of output
        if (run_result.stdout.len > 0) {
            var lines = std.mem.splitScalar(u8, run_result.stdout, '\n');
            var line_count: usize = 0;
            while (lines.next()) |line| {
                if (line_count >= 3) break;
                if (line.len > 0) {
                    std.debug.print("       {s}\n", .{line});
                    line_count += 1;
                }
            }
        }
    } else {
        std.debug.print("FAIL\n", .{});
        failed += 1;
    }

    // Clean up native executable
    std.fs.cwd().deleteFile(native_output) catch {};

    printSummary(passed, failed, skipped);

    if (failed > 0) {
        std.process.exit(1);
    }
}

fn printSummary(passed: usize, failed: usize, skipped: usize) void {
    std.debug.print("\n=== Summary ===\n", .{});
    std.debug.print("Passed: {d}\n", .{passed});
    std.debug.print("Failed: {d}\n", .{failed});
    std.debug.print("Skipped: {d}\n", .{skipped});

    if (failed > 0) {
        std.debug.print("\nSome tests failed!\n", .{});
    } else {
        std.debug.print("\nAll tests passed!\n", .{});
    }
}
