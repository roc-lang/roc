//! Integration tests for the fx platform with effectful functions.
//!
//! Tests that platform-provided hosted functions (like Stdout.line! and Stderr.line!)
//! can be properly invoked from Roc applications.
//!
//! NOTE: These tests depend on the roc binary being built via build.zig. The test step
//! has a dependency on roc_step, so the binary will be built automatically before tests run.
//!
//! IMPORTANT: Do NOT use --no-cache when running roc. The interpreted host doesn't change between
//! tests (we're testing app behaviour, not the platform), so using --no-cache would force unnecessary
//! re-linking on every test, making the test run much slower than is necessary.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const roc_binary_path = if (builtin.os.tag == .windows) ".\\zig-out\\bin\\roc.exe" else "./zig-out/bin/roc";

/// Options for running roc commands
const RunOptions = struct {
    /// Additional command line arguments (e.g., "test", "check")
    extra_args: []const []const u8 = &[_][]const u8{},
    /// Optional current working directory
    cwd: ?[]const u8 = null,
};

/// Runs a roc command and returns the result.
fn runRoc(allocator: std.mem.Allocator, roc_file: []const u8, options: RunOptions) !std.process.Child.RunResult {
    var args = std.ArrayList([]const u8){};
    defer args.deinit(allocator);

    try args.append(allocator, roc_binary_path);
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
/// This verifies the process exited cleanly with a non-zero code, NOT that it crashed.
fn checkFailure(result: std.process.Child.RunResult) !void {
    switch (result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("ERROR: roc succeeded but we expected it to fail\n", .{});
                return error.UnexpectedSuccess;
            }
            // Non-zero exit code is expected - this is a clean failure
        },
        .Signal => |sig| {
            // A crash is NOT the same as a clean failure - report it as an error
            std.debug.print("ERROR: Process crashed with signal {} (expected clean failure with non-zero exit code)\n", .{sig});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("ERROR: Process terminated abnormally: {} (expected clean failure with non-zero exit code)\n", .{result.term});
            std.debug.print("STDOUT: {s}\n", .{result.stdout});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.RunFailed;
        },
    }
}

/// Runs a roc app with --test mode using the given IO spec.
/// Spec format: "0<stdin|1>stdout|2>stderr" (pipe-separated)
/// Returns success if the app's IO matches the spec exactly.
fn runRocTest(allocator: std.mem.Allocator, roc_file: []const u8, spec: []const u8) !std.process.Child.RunResult {
    return try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
            roc_file,
            "--",
            "--test",
            spec,
        },
    });
}

/// Helper to check if a test mode run succeeded (exit code 0, empty output)
fn checkTestSuccess(result: std.process.Child.RunResult) !void {
    switch (result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Test failed with exit code {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{result.stderr});
                return error.TestFailed;
            }
        },
        .Signal => |sig| {
            std.debug.print("Process terminated by signal: {}\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("Test terminated abnormally: {}\n", .{result.term});
            std.debug.print("STDERR: {s}\n", .{result.stderr});
            return error.TestFailed;
        },
    }
}

test "fx platform effectful functions" {
    const allocator = testing.allocator;

    const result = try runRocTest(
        allocator,
        "test/fx/app.roc",
        "1>Hello from stdout!|1>Line 1 to stdout|2>Line 2 to stderr|1>Line 3 to stdout|2>Error from stderr!",
    );
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform with dotdot starting path" {
    // Tests that relative paths starting with .. are handled correctly
    const allocator = testing.allocator;

    const result = try runRocTest(
        allocator,
        "test/fx/subdir/app.roc",
        "1>Hello from stdout!|1>Line 1 to stdout|2>Line 2 to stderr|1>Line 3 to stdout|2>Error from stderr!",
    );
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform stdin to stdout" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/stdin_to_stdout.roc", "0<test input|1>test input");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform stdin echo" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/stdin_echo.roc", "0<hello world|1>hello world");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform stdin test with output" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/stdin_test.roc", "1>Before stdin|0<user input|1>After stdin");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform stdin simple" {
    // stdin_simple reads from stdin and prints to stderr
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/stdin_simple.roc", "0<simple test|2>simple test");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
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
    // Tests that match expressions with string returns work correctly
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/match_str_return.roc", "1>0");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform match with wildcard" {
    // Tests that wildcard patterns in match expressions work correctly
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/match_with_wildcard.roc", "1>0");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
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

    // Run `roc check` on an app with unused variables and type annotations
    // This test checks that the compiler reports the correct errors and doesn't
    // produce extraneous unrelated errors from platform module resolution
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
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
            roc_binary_path,
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
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/opaque_with_method.roc", "1>My favourite color is Red");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform string interpolation type mismatch" {
    const allocator = testing.allocator;

    // Run an app that tries to interpolate a U8 (non-Str) type in a string.
    // This should fail with a type error because string interpolation only accepts Str.
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
            "test/fx/num_method_call.roc",
            "--allow-errors",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The program should run (exit 0) with --allow-errors despite type errors
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

    const result = try runRocTest(allocator, "test/fx/question_mark_operator.roc", "1>hello");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "numeric fold" {
    // Tests List.fold with numeric accumulators.
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/numeric_fold.roc", "1>Sum: 15.0");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "List.for_each! with effectful callback" {
    // Tests List.for_each! which iterates over a list and calls an effectful callback
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/list_for_each.roc", "1>Item: apple|1>Item: banana|1>Item: cherry");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "string literal pattern matching" {
    // Tests pattern matching on string literals in match expressions.
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/string_pattern_matching.roc", "1>Hello Alice!|1>Hey Bob!");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
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

    const result = try runRocTest(allocator, "test/fx/hello_world.roc", "1>Hello, world!");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform function wrapper stdout" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/function_wrapper_stdout.roc", "1>Hello from stdout!");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform function wrapper multiline" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/function_wrapper_multiline.roc", "1>Hello from stdout!|1>Line 2");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform multiline stdout" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/multiline_stdout.roc", "1>Hello|1>World");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform empty_list_get" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/empty_list_get.roc", "1>is err");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform str_interp_valid" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/str_interp_valid.roc", "1>Hello, World!");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform expect with toplevel numeric" {
    const allocator = testing.allocator;

    // Run the app
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
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
            roc_binary_path,
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
//
//     const run_result = try std.process.Child.run(.{
//         .allocator = allocator,
//         .argv = &[_][]const u8{
//             "roc_binary_path",
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
//
//     const run_result = try std.process.Child.run(.{
//         .allocator = allocator,
//         .argv = &[_][]const u8{
//             "roc_binary_path",
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
//
//     const run_result = try std.process.Child.run(.{
//         .allocator = allocator,
//         .argv = &[_][]const u8{
//             "roc_binary_path",
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

    const result = try runRocTest(allocator, "test/fx/numeric_lookup_test.roc", "1>done");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform string_lookup_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/string_lookup_test.roc", "1>hello");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform test_direct_string" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/test_direct_string.roc", "1>Hello");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform test_one_call" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/test_one_call.roc", "1>Hello");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform test_type_mismatch" {
    const allocator = testing.allocator;

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
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

    const result = try runRocTest(allocator, "test/fx/test_with_wrapper.roc", "1>Hello");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform inspect_compare_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(
        allocator,
        "test/fx/inspect_compare_test.roc",
        "1>With to_inspect: Custom::Red|1>Without to_inspect: ColorWithoutInspect.Red|1>Primitive: 42",
    );
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform inspect_custom_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/inspect_custom_test.roc", "1>Color::Red|1>Expected: Color::Red");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform inspect_nested_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(
        allocator,
        "test/fx/inspect_nested_test.roc",
        "1>{ color: Color::Red, count: 42, name: \"test\" }|1>Expected: { color: Color::Red, count: 42, name: \"test\" }",
    );
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform inspect_no_method_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/inspect_no_method_test.roc", "1>Result: Color.Red|1>(Default rendering)");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform inspect_record_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/inspect_record_test.roc", "1>{ count: 42, name: \"test\" }");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform inspect_wrong_sig_test" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/inspect_wrong_sig_test.roc", "1>Result: 1");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform issue8433" {
    const allocator = testing.allocator;

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
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

test "run aborts on type errors by default" {
    // Tests that roc run aborts when there are type errors (without --allow-errors)
    const allocator = testing.allocator;

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
            "test/fx/run_allow_errors.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should fail with type errors
    try checkFailure(run_result);

    // Should show the errors
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "UNDEFINED VARIABLE") != null);
}

test "run aborts on parse errors by default" {
    // Tests that roc run aborts when there are parse errors (without --allow-errors)
    const allocator = testing.allocator;

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "test/fx/parse_error.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should fail with type errors
    try checkFailure(run_result);

    // Should show the errors
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "PARSE ERROR") != null);
}

test "run with --allow-errors attempts execution despite type errors" {
    // Tests that roc run --allow-errors attempts to execute even with type errors
    const allocator = testing.allocator;

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
            "test/fx/run_allow_errors.roc",
            "--allow-errors",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should still show the errors
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "UNDEFINED VARIABLE") != null);

    // The program will attempt to run and likely crash, which is expected behavior
    // We just verify it didn't abort during type checking
}

test "run allows warnings without blocking execution" {
    // Tests that warnings don't block execution (they never should)
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/run_warning_only.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Should show the warning
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "UNUSED VARIABLE") != null);

    // Should produce output (runs successfully)
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello, World!") != null);
}

test "fx platform method inspect on string" {
    // Tests that calling .inspect() on a Str correctly reports MISSING METHOD
    // (Str doesn't have an inspect method, unlike custom opaque types)
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/test_method_inspect.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This should fail because Str doesn't have an inspect method
    try checkFailure(run_result);

    // Should show MISSING METHOD error
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "MISSING METHOD") != null);
}

test "fx platform if-expression closure capture regression" {
    // Regression test: Variables bound inside an if-expression's block were
    // incorrectly being captured as free variables by the enclosing lambda,
    // causing a crash with "e_closure: failed to resolve capture value".
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/if-closure-capture.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
}

test "fx platform var with string interpolation segfault" {
    // Regression test: Using `var` variables with string interpolation causes segfault.
    // The code calls fnA! multiple times, each using var state variables, and
    // interpolates the results into strings.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/var_interp_segfault.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify the expected output
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "A1: 1") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "A2: 1") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "A3: 1") != null);
}

test "fx platform sublist method on inferred type" {
    // Regression test: Calling .sublist() method on a List(U8) from "".to_utf8()
    // causes a segfault when the variable doesn't have an explicit type annotation.
    // Error was: "Roc crashed: Error evaluating from shared memory: InvalidMethodReceiver"
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/sublist_method_segfault.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
}

test "fx platform repeating pattern segfault" {
    // Regression test: This test exposed a compiler bug where variables used multiple times
    // in consuming positions didn't get proper refcount handling. Specifically,
    // in `repeat_helper(acc.concat(list), list, n-1)`, the variable `list` is
    // passed to both concat (consuming) and to the recursive call (consuming).
    // The compiler must insert a copy/incref for the second use to avoid use-after-free.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/repeating_pattern_segfault.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);
}

test "fx platform runtime stack overflow" {
    // Tests that stack overflow in a running Roc program is caught and reported
    // with a helpful error message instead of crashing with a raw signal.
    //
    // The Roc program contains an infinitely recursive function that will
    // overflow the stack at runtime. Once proper stack overflow handling is
    // implemented in the host/platform, this test will pass.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/stack_overflow_runtime.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Stack overflow can be caught by either:
    // 1. The Roc interpreter (exit code 1, "overflowed its stack memory" message) - most common
    // 2. The SIGABRT signal handler (exit code 134) - if native stack overflow handling is used
    switch (run_result.term) {
        .Exited => |code| {
            if (code == 134) {
                // Stack overflow was caught by native signal handler
                // Verify the helpful error message was printed
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "overflowed its stack memory") != null);
            } else if (code == 1) {
                // Stack overflow was caught by the interpreter - this is the expected case
                // The interpreter detects excessive work stack depth and reports the error
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "overflowed its stack memory") != null);
            } else if (code == 139) {
                // Exit code 139 = 128 + 11 (SIGSEGV) - stack overflow was NOT handled
                // The Roc program crashed with a segfault that wasn't caught
                std.debug.print("\n", .{});
                std.debug.print("Stack overflow handling NOT YET IMPLEMENTED for Roc programs.\n", .{});
                std.debug.print("Process crashed with SIGSEGV (exit code 139).\n", .{});
                std.debug.print("Expected: exit code 1 or 134 with stack overflow message\n", .{});
                return error.StackOverflowNotHandled;
            } else {
                std.debug.print("Unexpected exit code: {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedExitCode;
            }
        },
        .Signal => |sig| {
            // Process was killed directly by a signal (likely SIGSEGV = 11).
            std.debug.print("\n", .{});
            std.debug.print("Stack overflow handling NOT YET IMPLEMENTED for Roc programs.\n", .{});
            std.debug.print("Process was killed by signal: {}\n", .{sig});
            std.debug.print("Expected: exit code 1 or 134 with stack overflow message\n", .{});
            return error.StackOverflowNotHandled;
        },
        else => {
            std.debug.print("Unexpected termination: {}\n", .{run_result.term});
            return error.UnexpectedTermination;
        },
    }
}

test "fx platform runtime division by zero" {
    // Tests that division by zero in a running Roc program is caught and reported
    // with a helpful error message instead of crashing with a raw signal.
    //
    // The error can be caught by either:
    // 1. The Roc interpreter (exit code 1, "DivisionByZero" message) - most common
    // 2. The SIGFPE signal handler (exit code 136, "divided by zero" message) - native code
    const allocator = testing.allocator;

    // The Roc program uses a var to prevent compile-time constant folding
    const run_result = try runRoc(allocator, "test/fx/division_by_zero.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .Exited => |code| {
            if (code == 136) {
                // Division by zero was caught by the SIGFPE handler (native code)
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "divided by zero") != null);
            } else if (code == 1) {
                // Division by zero was caught by the interpreter - this is the expected case
                // The interpreter catches it and reports "DivisionByZero"
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "DivisionByZero") != null);
            } else {
                std.debug.print("Unexpected exit code: {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedExitCode;
            }
        },
        .Signal => |sig| {
            // Process was killed directly by a signal without being caught
            std.debug.print("\n", .{});
            std.debug.print("Division by zero was not caught!\n", .{});
            std.debug.print("Process was killed by signal: {}\n", .{sig});
            return error.DivisionByZeroNotHandled;
        },
        else => {
            std.debug.print("Unexpected termination: {}\n", .{run_result.term});
            return error.UnexpectedTermination;
        },
    }
}
