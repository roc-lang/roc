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

test "fx platform dbg missing return value" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run an app that uses dbg without providing a return value.
    // This has a type error (returns Str instead of {}) which should be caught by the type checker.
    // When run, it should fail gracefully with a TypeMismatch error rather than panicking.
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/dbg_missing_return.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The run should fail with a non-zero exit code due to the type mismatch
    switch (run_result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("Run should have failed but succeeded\n", .{});
                return error.TestFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // Verify that the dbg output was printed before the error
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "this will break") != null);

    // Verify that it crashes with TypeMismatch error rather than a panic
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "TypeMismatch") != null);
}
