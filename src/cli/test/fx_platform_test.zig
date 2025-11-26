//! Integration tests for the fx platform with effectful functions.
//!
//! Tests that platform-provided hosted functions (like Stdout.line! and Stderr.line!)
//! can be properly invoked from Roc applications.

const std = @import("std");
const testing = std.testing;

const roc_binary_path = "./zig-out/bin/roc";

/// Ensures the roc binary exists, building it if necessary.
/// This is needed because these tests spawn the roc CLI as a child process.
fn ensureRocBinary(allocator: std.mem.Allocator) !void {
    // Check if binary exists
    std.fs.cwd().access(roc_binary_path, .{}) catch {
        // Binary doesn't exist, build it
        std.debug.print("roc binary not found, building with 'zig build roc'...\n", .{});
        const build_result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{ "zig", "build", "roc" },
        });
        defer allocator.free(build_result.stdout);
        defer allocator.free(build_result.stderr);

        if (build_result.term != .Exited or build_result.term.Exited != 0) {
            std.debug.print("Failed to build roc binary:\n{s}\n", .{build_result.stderr});
            return error.RocBuildFailed;
        }
        std.debug.print("roc binary built successfully.\n", .{});
    };
}

fn runRocWithStdin(allocator: std.mem.Allocator, roc_file: []const u8, stdin_input: []const u8) !std.process.Child.RunResult {
    try ensureRocBinary(allocator);
    var child = std.process.Child.init(&[_][]const u8{ "./zig-out/bin/roc", roc_file }, allocator);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Write stdin and close
    if (child.stdin) |stdin| {
        try stdin.writeAll(stdin_input);
        stdin.close();
        child.stdin = null;
    }

    // Collect stdout
    const stdout = if (child.stdout) |stdout_pipe|
        try stdout_pipe.readToEndAlloc(allocator, std.math.maxInt(usize))
    else
        try allocator.dupe(u8, "");

    // Collect stderr
    const stderr = if (child.stderr) |stderr_pipe|
        try stderr_pipe.readToEndAlloc(allocator, std.math.maxInt(usize))
    else
        try allocator.dupe(u8, "");

    const term = try child.wait();

    return .{
        .term = term,
        .stdout = stdout,
        .stderr = stderr,
    };
}

test "fx platform effectful functions" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run the app directly with the roc CLI (not build, just run)
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/app.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // Verify stdout contains expected messages
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello from stdout!") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 1 to stdout") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 3 to stdout") != null);

    // Verify stderr contains expected messages
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Error from stderr!") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Line 2 to stderr") != null);

    // Verify stderr messages are NOT in stdout
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Error from stderr!") == null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 2 to stderr") == null);

    // Verify stdout messages are NOT in stderr
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Hello from stdout!") == null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Line 1 to stdout") == null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Line 3 to stdout") == null);
}

test "fx platform stdin to stdout" {
    const allocator = testing.allocator;

    const result = try runRocWithStdin(allocator, "test/fx/stdin_to_stdout.roc", "test input\n");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("Test failed with term: {}\n", .{result.term});
        std.debug.print("STDOUT:\n{s}\n", .{result.stdout});
        std.debug.print("STDERR:\n{s}\n", .{result.stderr});
        return error.TestFailed;
    }
    try testing.expect(std.mem.indexOf(u8, result.stdout, "test input") != null);
}

test "fx platform stdin echo" {
    const allocator = testing.allocator;

    const result = try runRocWithStdin(allocator, "test/fx/stdin_echo.roc", "hello world\n");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("Test failed with term: {}\n", .{result.term});
        std.debug.print("STDOUT:\n{s}\n", .{result.stdout});
        std.debug.print("STDERR:\n{s}\n", .{result.stderr});
        return error.TestFailed;
    }
    try testing.expect(std.mem.indexOf(u8, result.stdout, "hello world") != null);
}

test "fx platform stdin test with output" {
    const allocator = testing.allocator;

    const result = try runRocWithStdin(allocator, "test/fx/stdin_test.roc", "user input\n");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("Test failed with term: {}\n", .{result.term});
        std.debug.print("STDOUT:\n{s}\n", .{result.stdout});
        std.debug.print("STDERR:\n{s}\n", .{result.stderr});
        return error.TestFailed;
    }
    try testing.expect(std.mem.indexOf(u8, result.stdout, "Before stdin") != null);
    try testing.expect(std.mem.indexOf(u8, result.stdout, "After stdin") != null);
}

test "fx platform stdin simple" {
    const allocator = testing.allocator;

    const result = try runRocWithStdin(allocator, "test/fx/stdin_simple.roc", "simple test\n");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("Test failed with term: {}\n", .{result.term});
        std.debug.print("STDOUT:\n{s}\n", .{result.stdout});
        std.debug.print("STDERR:\n{s}\n", .{result.stderr});
        return error.TestFailed;
    }
    // stdin_simple reads from stdin and prints to stderr
    try testing.expect(std.mem.indexOf(u8, result.stderr, "simple test") != null);
}

test "fx platform expect with main" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run `roc test` on the app that has both main! and an expect
    // Note: `roc test` only evaluates expect statements, it does not run main!
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test",
            "test/fx/expect_with_main.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // When all tests pass without --verbose, roc test produces no output
    try testing.expectEqualStrings("", run_result.stdout);
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform expect with numeric literal" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run `roc test` on an app that compares a typed variable with a numeric literal
    // This tests that numeric literals in top-level expects are properly typed
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test",
            "test/fx/expect_with_literal.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // When all tests pass without --verbose, roc test produces no output
    try testing.expectEqualStrings("", run_result.stdout);
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform match returning string" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run the app that has a match expression returning a string
    // This tests that match expressions with string returns work correctly
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/match_str_return.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // The app should run successfully and exit with code 0
    // It outputs "0" from the match expression
    try testing.expectEqualStrings("0\n", run_result.stdout);
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform match with wildcard" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run an app that uses a match expression with a wildcard pattern
    // This tests that wildcard patterns in match expressions work correctly
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/match_with_wildcard.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }
}

test "fx platform check unused state var reports correct errors" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run `roc check` on an app with unused variables and type annotations
    // This test checks that the compiler reports the correct errors and doesn't
    // produce extraneous unrelated errors
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "check",
            "test/fx/unused_state_var.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The check should fail with errors
    switch (run_result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("ERROR: roc check succeeded but we expected it to fail with errors\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            return error.RunFailed;
        },
    }

    // Check that we get exactly 2 UNUSED VARIABLE errors and nothing else
    const stderr = run_result.stderr;

    // Count occurrences of each error type
    var unused_variable_count: usize = 0;
    var type_mismatch_count: usize = 0;
    var module_not_found_count: usize = 0;
    var undefined_variable_count: usize = 0;
    var exposed_but_not_defined_count: usize = 0;
    var comptime_crash_count: usize = 0;

    var line_iter = std.mem.splitScalar(u8, stderr, '\n');
    while (line_iter.next()) |line| {
        if (std.mem.indexOf(u8, line, "UNUSED VARIABLE") != null) {
            unused_variable_count += 1;
        } else if (std.mem.indexOf(u8, line, "TYPE MISMATCH") != null) {
            type_mismatch_count += 1;
        } else if (std.mem.indexOf(u8, line, "MODULE NOT FOUND") != null) {
            module_not_found_count += 1;
        } else if (std.mem.indexOf(u8, line, "UNDEFINED VARIABLE") != null) {
            undefined_variable_count += 1;
        } else if (std.mem.indexOf(u8, line, "EXPOSED BUT NOT DEFINED") != null) {
            exposed_but_not_defined_count += 1;
        } else if (std.mem.indexOf(u8, line, "COMPTIME CRASH") != null) {
            comptime_crash_count += 1;
        }
    }

    // We expect exactly 2 UNUSED VARIABLE errors and 0 of everything else
    const expected_unused_variable: usize = 2;
    const expected_other_errors: usize = 0;

    var test_passed = true;

    if (unused_variable_count != expected_unused_variable) {
        std.debug.print("\n❌ UNUSED VARIABLE count mismatch: expected {d}, got {d}\n", .{ expected_unused_variable, unused_variable_count });
        test_passed = false;
    } else {
        std.debug.print("\n✅ UNUSED VARIABLE count correct: {d}\n", .{unused_variable_count});
    }

    if (type_mismatch_count != expected_other_errors) {
        std.debug.print("❌ TYPE MISMATCH (extraneous): expected {d}, got {d}\n", .{ expected_other_errors, type_mismatch_count });
        test_passed = false;
    }

    if (module_not_found_count != expected_other_errors) {
        std.debug.print("❌ MODULE NOT FOUND (extraneous): expected {d}, got {d}\n", .{ expected_other_errors, module_not_found_count });
        test_passed = false;
    }

    if (undefined_variable_count != expected_other_errors) {
        std.debug.print("❌ UNDEFINED VARIABLE (extraneous): expected {d}, got {d}\n", .{ expected_other_errors, undefined_variable_count });
        test_passed = false;
    }

    if (exposed_but_not_defined_count != expected_other_errors) {
        std.debug.print("❌ EXPOSED BUT NOT DEFINED (extraneous): expected {d}, got {d}\n", .{ expected_other_errors, exposed_but_not_defined_count });
        test_passed = false;
    }

    if (comptime_crash_count != expected_other_errors) {
        std.debug.print("❌ COMPTIME CRASH (extraneous): expected {d}, got {d}\n", .{ expected_other_errors, comptime_crash_count });
        test_passed = false;
    }

    if (!test_passed) {
        std.debug.print("\n========== FULL ROC CHECK OUTPUT ==========\n", .{});
        std.debug.print("STDERR:\n{s}\n", .{stderr});
        std.debug.print("==========================================\n\n", .{});
        return error.ExtraneousErrorsFound;
    }
}
