//! Integration tests for the fx platform with effectful functions.
//!
//! Tests that platform-provided hosted functions (like Stdout.line! and Stderr.line!)
//! can be properly invoked from Roc applications.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const roc_binary_path = if (builtin.os.tag == .windows) ".\\zig-out\\bin\\roc.exe" else "./zig-out/bin/roc";

/// Ensures the roc binary is up-to-date by always rebuilding it.
/// This is needed because these tests spawn the roc CLI as a child process,
/// and a stale binary will cause test failures even if the test code is correct.
fn ensureRocBinary(allocator: std.mem.Allocator) !void {
    // Always rebuild to ensure the binary is up-to-date with the latest source changes.
    // This prevents confusing test failures when the binary exists but is stale.
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
}

/// Options for running roc commands
const RunOptions = struct {
    /// Additional command line arguments (e.g., "test", "check")
    extra_args: []const []const u8 = &[_][]const u8{},
    /// Optional current working directory
    cwd: ?[]const u8 = null,
};

/// Runs a roc command and returns the result.
/// Automatically adds --no-cache for non-test/non-check commands to ensure fresh builds.
fn runRoc(allocator: std.mem.Allocator, roc_file: []const u8, options: RunOptions) !std.process.Child.RunResult {
    try ensureRocBinary(allocator);

    var args = std.ArrayList([]const u8){};
    defer args.deinit(allocator);

    try args.append(allocator, roc_binary_path);

    // Determine if this is a test or check command
    const is_test_or_check = blk: {
        for (options.extra_args) |arg| {
            if (std.mem.eql(u8, arg, "test") or std.mem.eql(u8, arg, "check")) {
                break :blk true;
            }
        }
        break :blk false;
    };

    // Add --no-cache before other args for non-test/non-check commands
    if (!is_test_or_check) {
        try args.append(allocator, "--no-cache");
    }

    try args.appendSlice(allocator, options.extra_args);
    try args.append(allocator, roc_file);

    return try std.process.Child.run(.{
        .allocator = allocator,
        .argv = args.items,
        .cwd = options.cwd,
    });
}

/// Helper to check if a run result indicates success (exit code 0)
fn checkSuccess(result: std.process.Child.RunResult) !void {
    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{result.stdout});
                std.debug.print("STDERR: {s}\n", .{result.stderr});
                return error.RunFailed;
            }
        },
        .Signal => |sig| {
            std.debug.print("Process terminated by signal: {}\n", .{sig});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{result.term});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.RunFailed;
        },
    }
}

/// Helper to check if a run result indicates failure (non-zero exit code)
fn checkFailure(result: std.process.Child.RunResult) !void {
    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("ERROR: roc succeeded but we expected it to fail\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            // Non-zero exit is expected
        },
    }
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

test "fx platform with dotdot starting path" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run the app from a subdirectory that uses ../ at the START of its platform path
    // This tests that relative paths starting with .. are handled correctly
    // Bug: paths starting with ../ fail with TypeMismatch, while ./path/../ works
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/subdir/app.roc",
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

    // Run `roc test` on the app that has both main! and an expect
    // Note: `roc test` only evaluates expect statements, it does not run main!
    const run_result = try runRoc(allocator, "test/fx/expect_with_main.roc", .{ .extra_args = &[_][]const u8{"test"} });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // When all tests pass without --verbose, roc test produces no output
    try testing.expectEqualStrings("", run_result.stdout);
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform expect with numeric literal" {
    const allocator = testing.allocator;

    // Run `roc test` on an app that compares a typed variable with a numeric literal
    // This tests that numeric literals in top-level expects are properly typed
    const run_result = try runRoc(allocator, "test/fx/expect_with_literal.roc", .{ .extra_args = &[_][]const u8{"test"} });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // When all tests pass without --verbose, roc test produces no output
    try testing.expectEqualStrings("", run_result.stdout);
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform match returning string" {
    const allocator = testing.allocator;

    // Run the app that has a match expression returning a string
    // This tests that match expressions with string returns work correctly
    const run_result = try runRoc(allocator, "test/fx/match_str_return.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // The app should run successfully and exit with code 0
    // It outputs "0" from the match expression
    try testing.expectEqualStrings("0\n", run_result.stdout);
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform match with wildcard" {
    const allocator = testing.allocator;

    // Run an app that uses a match expression with a wildcard pattern
    // This tests that wildcard patterns in match expressions work correctly
    const run_result = try runRoc(allocator, "test/fx/match_with_wildcard.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
}

test "fx platform dbg missing return value" {
    const allocator = testing.allocator;

    // Run an app that uses dbg as the last expression in main!.
    // dbg is treated as a statement (side-effect only) when it's the final
    // expression in a block, so the block returns {} as expected by main!.
    const run_result = try runRoc(allocator, "test/fx/dbg_missing_return.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify that the dbg output was printed
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "this should work now") != null);
}

test "fx platform check unused state var reports correct errors" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run `roc check` on an app with unused variables and type annotations
    // This test checks that the compiler reports the correct errors and doesn't
    // produce extraneous unrelated errors from platform module resolution
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

    const stderr = run_result.stderr;

    // Count occurrences of each error type
    var unused_variable_count: usize = 0;
    var module_not_found_count: usize = 0;
    var exposed_but_not_defined_count: usize = 0;

    var line_iter = std.mem.splitScalar(u8, stderr, '\n');
    while (line_iter.next()) |line| {
        if (std.mem.indexOf(u8, line, "UNUSED VARIABLE") != null) {
            unused_variable_count += 1;
        } else if (std.mem.indexOf(u8, line, "MODULE NOT FOUND") != null) {
            module_not_found_count += 1;
        } else if (std.mem.indexOf(u8, line, "EXPOSED BUT NOT DEFINED") != null) {
            exposed_but_not_defined_count += 1;
        }
    }

    // We expect exactly 2 UNUSED VARIABLE errors
    // We should NOT get MODULE NOT FOUND or EXPOSED BUT NOT DEFINED errors
    // (those were the extraneous errors this test was created to catch)
    //
    // Note: There are other errors (TYPE MISMATCH, UNDEFINED VARIABLE for main!,
    // COMPTIME CRASH) that are pre-existing bugs related to platform/app interaction
    // and should be fixed separately.
    var test_passed = true;

    if (unused_variable_count != 2) {
        std.debug.print("\n❌ UNUSED VARIABLE count mismatch: expected 2, got {d}\n", .{unused_variable_count});
        test_passed = false;
    }

    if (module_not_found_count != 0) {
        std.debug.print("❌ MODULE NOT FOUND (extraneous): expected 0, got {d}\n", .{module_not_found_count});
        test_passed = false;
    }

    if (exposed_but_not_defined_count != 0) {
        std.debug.print("❌ EXPOSED BUT NOT DEFINED (extraneous): expected 0, got {d}\n", .{exposed_but_not_defined_count});
        test_passed = false;
    }

    if (!test_passed) {
        std.debug.print("\n========== FULL ROC CHECK OUTPUT ==========\n", .{});
        std.debug.print("STDERR:\n{s}\n", .{stderr});
        std.debug.print("==========================================\n\n", .{});
        return error.ExtraneousErrorsFound;
    }
}

test "fx platform checked directly finds sibling modules" {
    // When checking a platform module directly (not through an app), sibling .roc
    // files in the same directory should be discovered automatically. This means
    // we should NOT get MODULE NOT FOUND errors for Stdout/Stderr/Stdin since
    // those files exist in the same directory as main.roc.
    const allocator = std.testing.allocator;

    // Check the platform module directly (not through an app)
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "check",
            "test/fx/platform/main.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    const stderr = run_result.stderr;

    // Count MODULE NOT FOUND errors - we should get 0 since sibling modules are discovered
    var module_not_found_count: usize = 0;

    var line_iter = std.mem.splitScalar(u8, stderr, '\n');
    while (line_iter.next()) |line| {
        if (std.mem.indexOf(u8, line, "MODULE NOT FOUND") != null) {
            module_not_found_count += 1;
        }
    }

    // When checking a platform directly, sibling modules should be discovered,
    // so we should NOT get MODULE NOT FOUND errors for valid imports.
    if (module_not_found_count != 0) {
        std.debug.print("\n❌ Expected 0 MODULE NOT FOUND errors (siblings should be discovered), got {d}\n", .{module_not_found_count});
        std.debug.print("\n========== FULL ROC CHECK OUTPUT ==========\n", .{});
        std.debug.print("STDERR:\n{s}\n", .{stderr});
        std.debug.print("==========================================\n\n", .{});
        return error.UnexpectedModuleNotFoundErrors;
    }
}

test "fx platform opaque type with method" {
    // Regression test: An opaque type with a method attached causes a segfault
    // when running the app. This test will pass once the bug is fixed.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/opaque_with_method.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify the output contains the expected string
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "My favourite color is Red") != null);
}

test "fx platform string interpolation type mismatch" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run an app that tries to interpolate a U8 (non-Str) type in a string.
    // This should fail with a type error because string interpolation only accepts Str.
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/num_method_call.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The program should run (exit 0) even with type errors - errors are just warnings
    switch (run_result.term) {
        .Exited => |code| {
            try testing.expectEqual(@as(u8, 0), code);
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // Verify the error output contains proper diagnostic info
    // Should show TYPE MISMATCH error with the type information
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "U8") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Str") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Found 1 error") != null);

    // The program should still produce output (it runs despite errors)
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "two:") != null);
}

test "fx platform run from different cwd" {
    // Regression test: Running roc from a different current working directory
    // than the project root should still work. Previously this failed with
    // "error.InvalidAppPath" because the path resolution didn't handle
    // running from a subdirectory correctly.
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Get absolute path to roc binary since we'll change cwd
    const roc_abs_path = try std.fs.cwd().realpathAlloc(allocator, roc_binary_path);
    defer allocator.free(roc_abs_path);

    // Run roc from the test/fx directory with a relative path to app.roc
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_abs_path,
            "app.roc",
        },
        .cwd = "test/fx",
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
}

test "question mark operator" {
    // Tests the `?` operator for error propagation.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/question_mark_operator.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    // The ? operator should unwrap Ok values and return "hello"
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "hello") != null);
}

test "numeric fold" {
    // Tests List.fold with numeric accumulators.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/numeric_fold.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    // Verify we get the correct sum: 1+2+3+4+5 = 15
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Sum: 15") != null);
}

test "List.for_each! with effectful callback" {
    // Tests List.for_each! which iterates over a list and calls an effectful callback
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/list_for_each.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify each item is printed
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Item: apple") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Item: banana") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Item: cherry") != null);
}

test "string literal pattern matching" {
    // Tests pattern matching on string literals in match expressions.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/string_pattern_matching.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify string patterns match correctly
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello Alice!") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hey Bob!") != null);
}

test "drop_prefix segfault regression" {
    // Regression test: Calling drop_prefix on a string literal and assigning
    // the result to an unused variable causes a segfault.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/drop_prefix_segfault.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
}

test "drop_prefix match use-after-free regression" {
    // Regression test: Calling drop_prefix on a string literal and using the
    // result in a match expression causes a use-after-free panic.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/drop_prefix_match_uaf.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Also check for panic messages in stderr that indicate use-after-free
    if (std.mem.indexOf(u8, run_result.stderr, "panic") != null or
        std.mem.indexOf(u8, run_result.stderr, "use-after-free") != null or
        std.mem.indexOf(u8, run_result.stderr, "Invalid pointer") != null)
    {
        std.debug.print("Detected memory safety panic in stderr:\n{s}\n", .{run_result.stderr});
        return error.UseAfterFree;
    }
}

test "multiline string split_on" {
    // Tests splitting a multiline string and iterating over the lines.
    // This is a regression test to ensure split_on works correctly with
    // multiline strings and doesn't cause memory issues.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/multiline_split_leak.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify the output contains lines from the multiline string
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "This is a longer line number one") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "This is a longer line number two") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "L68") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "The last line is here") != null);
}

test "big string equality regression" {
    // Regression test: String literals of length >= 24 (big strings) must work
    // correctly in expect expressions. This tests the single-segment string
    // fast path in str_collect which previously caused use-after-free.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/big_string_equality.roc", .{ .extra_args = &[_][]const u8{"test"} });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Check for panic messages in stderr that indicate use-after-free
    if (std.mem.indexOf(u8, run_result.stderr, "panic") != null or
        std.mem.indexOf(u8, run_result.stderr, "use-after-free") != null or
        std.mem.indexOf(u8, run_result.stderr, "Use-after-free") != null)
    {
        std.debug.print("Detected memory safety panic in stderr:\n{s}\n", .{run_result.stderr});
        return error.UseAfterFree;
    }
}

test "fx platform hello world" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/hello_world.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello, world!") != null);
}

test "fx platform function wrapper stdout" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/function_wrapper_stdout.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello from stdout!") != null);
}

test "fx platform function wrapper multiline" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/function_wrapper_multiline.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello from stdout!") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 2") != null);
}

test "fx platform multiline stdout" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/multiline_stdout.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "World") != null);
}

test "fx platform empty_list_get" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/empty_list_get.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "is err") != null);
}

test "fx platform str_interp_valid" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/str_interp_valid.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello, World!") != null);
}

test "fx platform expect with toplevel numeric" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    // Run the app
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/expect_with_toplevel_numeric.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "hello") != null);

    // Run `roc test` since this file has a top-level expect
    const test_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test",
            "test/fx/expect_with_toplevel_numeric.roc",
        },
    });
    defer allocator.free(test_result.stdout);
    defer allocator.free(test_result.stderr);

    switch (test_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Test failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{test_result.stdout});
                std.debug.print("STDERR: {s}\n", .{test_result.stderr});
                return error.TestFailed;
            }
        },
        else => {
            std.debug.print("Test terminated abnormally: {}\n", .{test_result.term});
            std.debug.print("STDOUT: {s}\n", .{test_result.stdout});
            std.debug.print("STDERR: {s}\n", .{test_result.stderr});
            return error.TestFailed;
        },
    }
}

// TODO: Fix test7.roc - currently fails with "UNRECOGNIZED SYNTAX" for `_ = x` pattern
// test "fx platform test7" {
//     const allocator = testing.allocator;

//     try ensureRocBinary(allocator);

//     const run_result = try std.process.Child.run(.{
//         .allocator = allocator,
//         .argv = &[_][]const u8{
//             "./zig-out/bin/roc",
//             "test/fx/test7.roc",
//         },
//     });
//     defer allocator.free(run_result.stdout);
//     defer allocator.free(run_result.stderr);

//     switch (run_result.term) {
//         .Exited => |code| {
//             if (code != 0) {
//                 std.debug.print("Run failed with exit code {}\n", .{code});
//                 std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
//                 std.debug.print("STDERR: {s}\n", .{run_result.stderr});
//                 return error.RunFailed;
//             }
//         },
//         else => {
//             std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
//             std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
//             std.debug.print("STDERR: {s}\n", .{run_result.stderr});
//             return error.RunFailed;
//         },
//     }

//     try testing.expect(std.mem.indexOf(u8, run_result.stdout, "done") != null);
// }

// TODO: Fix test8.roc - currently fails with "UNRECOGNIZED SYNTAX" for `_ = x` pattern
// test "fx platform test8" {
//     const allocator = testing.allocator;

//     try ensureRocBinary(allocator);

//     const run_result = try std.process.Child.run(.{
//         .allocator = allocator,
//         .argv = &[_][]const u8{
//             "./zig-out/bin/roc",
//             "test/fx/test8.roc",
//         },
//     });
//     defer allocator.free(run_result.stdout);
//     defer allocator.free(run_result.stderr);

//     switch (run_result.term) {
//         .Exited => |code| {
//             if (code != 0) {
//                 std.debug.print("Run failed with exit code {}\n", .{code});
//                 std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
//                 std.debug.print("STDERR: {s}\n", .{run_result.stderr});
//                 return error.RunFailed;
//             }
//         },
//         else => {
//             std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
//             std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
//             std.debug.print("STDERR: {s}\n", .{run_result.stderr});
//             return error.RunFailed;
//         },
//     }

//     try testing.expect(std.mem.indexOf(u8, run_result.stdout, "done") != null);
// }

// TODO: Fix test9.roc - currently fails with "UNRECOGNIZED SYNTAX" for `_ = y` pattern
// test "fx platform test9" {
//     const allocator = testing.allocator;

//     try ensureRocBinary(allocator);

//     const run_result = try std.process.Child.run(.{
//         .allocator = allocator,
//         .argv = &[_][]const u8{
//             "./zig-out/bin/roc",
//             "test/fx/test9.roc",
//         },
//     });
//     defer allocator.free(run_result.stdout);
//     defer allocator.free(run_result.stderr);

//     switch (run_result.term) {
//         .Exited => |code| {
//             if (code != 0) {
//                 std.debug.print("Run failed with exit code {}\n", .{code});
//                 std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
//                 std.debug.print("STDERR: {s}\n", .{run_result.stderr});
//                 return error.RunFailed;
//             }
//         },
//         else => {
//             std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
//             std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
//             std.debug.print("STDERR: {s}\n", .{run_result.stderr});
//             return error.RunFailed;
//         },
//     }

//     try testing.expect(std.mem.indexOf(u8, run_result.stdout, "done") != null);
// }

test "fx platform numeric_lookup_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/numeric_lookup_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "done") != null);
}

test "fx platform string_lookup_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/string_lookup_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "hello") != null);
}

test "fx platform test_direct_string" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/test_direct_string.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello") != null);
}

test "fx platform test_one_call" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/test_one_call.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello") != null);
}

test "fx platform test_type_mismatch" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/test_type_mismatch.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail compilation with a type mismatch error
    // The to_inspect method returns I64 instead of Str
    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                // Expected to fail - check for type mismatch error message
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null);
            } else {
                std.debug.print("Expected compilation error but succeeded\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            // Abnormal termination should also indicate error
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            try testing.expect(std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null);
        },
    }
}

test "fx platform test_with_wrapper" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/test_with_wrapper.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello") != null);
}

test "fx platform inspect_compare_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/inspect_compare_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "With to_inspect: Custom::Red") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Without to_inspect: ColorWithoutInspect.Red") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Primitive: 42") != null);
}

test "fx platform inspect_custom_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/inspect_custom_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Color::Red") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Expected: Color::Red") != null);
}

test "fx platform inspect_nested_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/inspect_nested_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "{ color: Color::Red, count: 42, name: \"test\" }") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Expected: { color: Color::Red, count: 42, name: \"test\" }") != null);
}

test "fx platform inspect_no_method_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/inspect_no_method_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Result: Color.Red") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "(Default rendering)") != null);
}

test "fx platform inspect_record_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/inspect_record_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "{ count: 42, name: \"test\" }") != null);
}

test "fx platform inspect_wrong_sig_test" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/inspect_wrong_sig_test.roc",
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

    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Result: 1") != null);
}

test "fx platform issue8433" {
    const allocator = testing.allocator;

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/issue8433.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail compilation with a MISSING METHOD error
    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                // Expected to fail - check for missing method error message
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "MISSING METHOD") != null);
            } else {
                std.debug.print("Expected compilation error but succeeded\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            // Abnormal termination should also indicate error
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            try testing.expect(std.mem.indexOf(u8, run_result.stderr, "MISSING METHOD") != null);
        },
    }
}
