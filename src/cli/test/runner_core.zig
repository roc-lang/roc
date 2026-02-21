//! Shared execution logic for the test platform runner.
//!
//! This module provides common functions for:
//! - Cross-compilation of Roc apps
//! - Native build and execution
//! - Valgrind memory testing
//! - Result formatting and summary printing

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

/// Result of a test execution
pub const TestResult = enum {
    passed,
    failed,
    skipped,
};

/// Statistics for test run
pub const TestStats = struct {
    passed: usize = 0,
    failed: usize = 0,
    skipped: usize = 0,

    pub fn total(self: TestStats) usize {
        return self.passed + self.failed + self.skipped;
    }

    pub fn record(self: *TestStats, result: TestResult) void {
        switch (result) {
            .passed => self.passed += 1,
            .failed => self.failed += 1,
            .skipped => self.skipped += 1,
        }
    }
};

/// Cross-compile a Roc app to a specific target.
/// Returns true if compilation succeeded.
pub fn crossCompile(
    allocator: Allocator,
    roc_binary: []const u8,
    roc_file: []const u8,
    target: []const u8,
    output_name: []const u8,
    backend: ?[]const u8,
) !TestResult {
    const target_arg = try std.fmt.allocPrint(allocator, "--target={s}", .{target});
    defer allocator.free(target_arg);

    const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_name});
    defer allocator.free(output_arg);

    const backend_arg = if (backend) |b| try std.fmt.allocPrint(allocator, "--backend={s}", .{b}) else null;
    defer if (backend_arg) |b| allocator.free(b);

    var argv_buf: [6][]const u8 = undefined;
    var argc: usize = 0;
    argv_buf[argc] = roc_binary;
    argc += 1;
    argv_buf[argc] = "build";
    argc += 1;
    argv_buf[argc] = target_arg;
    argc += 1;
    argv_buf[argc] = output_arg;
    argc += 1;
    if (backend_arg) |b| {
        argv_buf[argc] = b;
        argc += 1;
    }
    argv_buf[argc] = roc_file;
    argc += 1;

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv_buf[0..argc],
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        return .failed;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    return handleProcessResult(result, output_name);
}

/// Build a Roc app natively (no cross-compilation).
/// Does NOT clean up the output file - caller is responsible for cleanup.
pub fn buildNative(
    allocator: Allocator,
    roc_binary: []const u8,
    roc_file: []const u8,
    output_name: []const u8,
    backend: ?[]const u8,
) !TestResult {
    const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_name});
    defer allocator.free(output_arg);

    const backend_arg = if (backend) |b| try std.fmt.allocPrint(allocator, "--backend={s}", .{b}) else null;
    defer if (backend_arg) |b| allocator.free(b);

    var argv_buf: [5][]const u8 = undefined;
    var argc: usize = 0;
    argv_buf[argc] = roc_binary;
    argc += 1;
    argv_buf[argc] = "build";
    argc += 1;
    argv_buf[argc] = output_arg;
    argc += 1;
    if (backend_arg) |b| {
        argv_buf[argc] = b;
        argc += 1;
    }
    argv_buf[argc] = roc_file;
    argc += 1;

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv_buf[0..argc],
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        return .failed;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Don't cleanup - caller will run and then cleanup
    return handleProcessResultNoCleanup(result, output_name);
}

/// Run a native executable and check for successful execution.
pub fn runNative(
    allocator: Allocator,
    exe_path: []const u8,
) !TestResult {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{exe_path},
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        return .failed;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Check for memory errors in stderr (GPA errors or Roc runtime leak detection)
    if (hasMemoryErrors(result.stderr)) |msg| {
        std.debug.print("FAIL ({s})\n", .{msg});
        printTruncatedOutput(result.stderr, 10, "       ");
        return .failed;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("OK\n", .{});
                // Print first few lines of output
                if (result.stdout.len > 0) {
                    printTruncatedOutput(result.stdout, 3, "       ");
                }
                return .passed;
            } else {
                std.debug.print("FAIL (exit code {d})\n", .{code});
                if (result.stderr.len > 0) {
                    printTruncatedOutput(result.stderr, 5, "       ");
                }
                return .failed;
            }
        },
        .Signal => |sig| {
            std.debug.print("FAIL (signal {d})\n", .{sig});
            return .failed;
        },
        else => {
            std.debug.print("FAIL (abnormal termination)\n", .{});
            return .failed;
        },
    }
}

/// Run a Roc app with --test mode and IO spec verification.
/// When backend is set, builds the executable first with `roc build --backend=<name>`
/// then runs the resulting binary with `--test <spec>`.
/// When backend is null, uses `roc run --test=<spec>` (interpreter).
pub fn runWithIoSpec(
    allocator: Allocator,
    roc_binary: []const u8,
    roc_file: []const u8,
    io_spec: []const u8,
    backend: ?[]const u8,
) !TestResult {
    if (backend) |b| {
        return runWithIoSpecBuildAndExec(allocator, roc_binary, roc_file, io_spec, b);
    }

    const test_arg = try std.fmt.allocPrint(allocator, "--test={s}", .{io_spec});
    defer allocator.free(test_arg);

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary,
            "run",
            test_arg,
            roc_file,
        },
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        return .failed;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Check for memory errors in stderr (GPA errors or Roc runtime leak detection)
    if (hasMemoryErrors(result.stderr)) |msg| {
        std.debug.print("FAIL ({s})\n", .{msg});
        printTruncatedOutput(result.stderr, 10, "       ");
        return .failed;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("OK\n", .{});
                return .passed;
            } else {
                std.debug.print("FAIL (exit code {d})\n", .{code});
                if (result.stderr.len > 0) {
                    printTruncatedOutput(result.stderr, 5, "       ");
                }
                return .failed;
            }
        },
        .Signal => |sig| {
            std.debug.print("FAIL (signal {d})\n", .{sig});
            return .failed;
        },
        else => {
            std.debug.print("FAIL (abnormal termination)\n", .{});
            return .failed;
        },
    }
}

/// Build a Roc app with a specific backend, then run the resulting executable
/// with `--test <spec>` for IO spec verification.
fn runWithIoSpecBuildAndExec(
    allocator: Allocator,
    roc_binary: []const u8,
    roc_file: []const u8,
    io_spec: []const u8,
    backend: []const u8,
) !TestResult {
    // Generate a temp output name from the roc file basename
    const basename = std.fs.path.stem(std.fs.path.basename(roc_file));
    const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}_test", .{ basename, backend });
    defer allocator.free(output_name);

    // Step 1: Build with the specified backend
    const build_result = try buildNative(allocator, roc_binary, roc_file, output_name, backend);
    if (build_result != .passed) {
        return .failed;
    }

    // Step 2: Run the built executable with --test <spec>
    const exe_path = try std.fmt.allocPrint(allocator, "./{s}", .{output_name});
    defer allocator.free(exe_path);

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            exe_path,
            "--test",
            io_spec,
        },
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        cleanup(output_name);
        return .failed;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Clean up the built executable
    cleanup(output_name);

    // Check for memory errors in stderr (GPA errors or Roc runtime leak detection)
    if (hasMemoryErrors(result.stderr)) |msg| {
        std.debug.print("FAIL ({s})\n", .{msg});
        printTruncatedOutput(result.stderr, 10, "       ");
        return .failed;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("OK\n", .{});
                return .passed;
            } else {
                std.debug.print("FAIL (exit code {d})\n", .{code});
                if (result.stderr.len > 0) {
                    printTruncatedOutput(result.stderr, 5, "       ");
                }
                return .failed;
            }
        },
        .Signal => |sig| {
            std.debug.print("FAIL (signal {d})\n", .{sig});
            return .failed;
        },
        else => {
            std.debug.print("FAIL (abnormal termination)\n", .{});
            return .failed;
        },
    }
}

/// Run a Roc app under valgrind.
/// Only works on Linux x86_64.
pub fn runWithValgrind(
    allocator: Allocator,
    roc_binary: []const u8,
    roc_file: []const u8,
) !TestResult {
    // Valgrind only works on Linux x86_64
    if (builtin.os.tag != .linux or builtin.cpu.arch != .x86_64) {
        std.debug.print("SKIP (valgrind requires Linux x86_64)\n", .{});
        return .skipped;
    }

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./ci/custom_valgrind.sh",
            roc_binary,
            "--no-cache",
            roc_file,
        },
    }) catch |err| {
        std.debug.print("FAIL (spawn error: {})\n", .{err});
        return .failed;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("OK\n", .{});
                return .passed;
            } else {
                std.debug.print("FAIL (valgrind exit code {d})\n", .{code});
                if (result.stderr.len > 0) {
                    printTruncatedOutput(result.stderr, 5, "       ");
                }
                return .failed;
            }
        },
        .Signal => |sig| {
            std.debug.print("FAIL (signal {d})\n", .{sig});
            return .failed;
        },
        else => {
            std.debug.print("FAIL (abnormal termination)\n", .{});
            return .failed;
        },
    }
}

/// Verify that required platform target files exist.
pub fn verifyPlatformFiles(
    allocator: Allocator,
    platform_dir: []const u8,
    target: []const u8,
) !bool {
    const libhost_path = try std.fmt.allocPrint(allocator, "{s}/platform/targets/{s}/libhost.a", .{ platform_dir, target });
    defer allocator.free(libhost_path);

    if (std.fs.cwd().access(libhost_path, .{})) |_| {
        return true;
    } else |_| {
        return false;
    }
}

/// Check if a target requires Linux host (glibc targets).
pub fn requiresLinuxHost(target: []const u8) bool {
    return std.mem.indexOf(u8, target, "glibc") != null;
}

/// Check if we should skip this target on current host.
pub fn shouldSkipTarget(target: []const u8) bool {
    if (requiresLinuxHost(target) and builtin.os.tag != .linux) {
        return true;
    }
    return false;
}

/// Clean up a generated file.
pub fn cleanup(path: []const u8) void {
    std.fs.cwd().deleteFile(path) catch {};
}

/// Print a section header.
pub fn printHeader(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("\n>>> " ++ fmt ++ "\n", args);
}

/// Print test summary.
pub fn printSummary(stats: TestStats) void {
    std.debug.print("\n=== Summary ===\n", .{});
    std.debug.print("Passed: {d}\n", .{stats.passed});
    std.debug.print("Failed: {d}\n", .{stats.failed});
    std.debug.print("Skipped: {d}\n", .{stats.skipped});

    if (stats.failed > 0) {
        std.debug.print("\nSome tests failed!\n", .{});
    } else {
        std.debug.print("\nAll tests passed!\n", .{});
    }
}

/// Print a result line.
pub fn printResultLine(status: []const u8, target: []const u8, message: []const u8) void {
    if (message.len > 0) {
        std.debug.print("[{s}] {s} ({s})\n", .{ status, target, message });
    } else {
        std.debug.print("[{s}] {s}\n", .{ status, target });
    }
}

// --- Internal helpers ---

/// Check stderr for memory-related errors:
/// - GPA (General Purpose Allocator) errors: alignment mismatches, double frees, etc.
/// - Roc runtime leak detection: allocations not freed
/// Returns a description string if an error is found, null otherwise.
fn hasMemoryErrors(stderr: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, stderr, "error(gpa):") != null) {
        return "memory error detected";
    }
    if (std.mem.indexOf(u8, stderr, "allocation(s) not freed") != null) {
        return "memory leak detected";
    }
    return null;
}

fn handleProcessResult(result: std.process.Child.RunResult, output_name: []const u8) TestResult {
    // Check for memory errors in stderr (GPA errors or Roc runtime leak detection)
    if (hasMemoryErrors(result.stderr)) |msg| {
        std.debug.print("FAIL ({s})\n", .{msg});
        printTruncatedOutput(result.stderr, 10, "       ");
        cleanup(output_name);
        return .failed;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                // Verify executable was created
                if (std.fs.cwd().access(output_name, .{})) |_| {
                    std.debug.print("OK\n", .{});
                    // Clean up
                    cleanup(output_name);
                    return .passed;
                } else |_| {
                    std.debug.print("FAIL (executable not created)\n", .{});
                    return .failed;
                }
            } else {
                std.debug.print("FAIL (exit code {d})\n", .{code});
                if (result.stderr.len > 0) {
                    printTruncatedOutput(result.stderr, 5, "       ");
                }
                return .failed;
            }
        },
        .Signal => |sig| {
            std.debug.print("FAIL (signal {d})\n", .{sig});
            return .failed;
        },
        else => {
            std.debug.print("FAIL (abnormal termination)\n", .{});
            return .failed;
        },
    }
}

fn handleProcessResultNoCleanup(result: std.process.Child.RunResult, output_name: []const u8) TestResult {
    // Check for memory errors in stderr (GPA errors or Roc runtime leak detection)
    if (hasMemoryErrors(result.stderr)) |msg| {
        std.debug.print("FAIL ({s})\n", .{msg});
        printTruncatedOutput(result.stderr, 10, "       ");
        return .failed;
    }

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                // Verify executable was created
                if (std.fs.cwd().access(output_name, .{})) |_| {
                    std.debug.print("OK\n", .{});
                    // Don't clean up - caller will handle
                    return .passed;
                } else |_| {
                    std.debug.print("FAIL (executable not created)\n", .{});
                    return .failed;
                }
            } else {
                std.debug.print("FAIL (exit code {d})\n", .{code});
                if (result.stderr.len > 0) {
                    printTruncatedOutput(result.stderr, 5, "       ");
                }
                return .failed;
            }
        },
        .Signal => |sig| {
            std.debug.print("FAIL (signal {d})\n", .{sig});
            return .failed;
        },
        else => {
            std.debug.print("FAIL (abnormal termination)\n", .{});
            return .failed;
        },
    }
}

fn printTruncatedOutput(output: []const u8, max_lines: usize, prefix: []const u8) void {
    var lines = std.mem.splitScalar(u8, output, '\n');
    var line_count: usize = 0;
    while (lines.next()) |line| {
        if (line_count >= max_lines) {
            std.debug.print("{s}... (truncated)\n", .{prefix});
            break;
        }
        if (line.len > 0) {
            std.debug.print("{s}{s}\n", .{ prefix, line });
            line_count += 1;
        }
    }
}
