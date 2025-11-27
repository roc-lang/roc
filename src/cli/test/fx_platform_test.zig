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

    // Run an app that uses dbg as the last expression in main!.
    // dbg is treated as a statement (side-effect only) when it's the final
    // expression in a block, so the block returns {} as expected by main!.
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "--no-cache",
            "test/fx/dbg_missing_return.roc",
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

    try ensureRocBinary(allocator);

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/opaque_with_method.roc",
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
