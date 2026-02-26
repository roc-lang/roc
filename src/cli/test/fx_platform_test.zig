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
//!
//! Test specs for IO-based tests are defined in fx_test_specs.zig and shared with
//! the cross-compilation test runner.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const fx_test_specs = @import("fx_test_specs.zig");

// Wire up tests from fx_test_specs module
comptime {
    std.testing.refAllDecls(fx_test_specs);
}

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
    // Check for GPA (General Purpose Allocator) errors in stderr
    // These indicate memory bugs like alignment mismatches, double frees, etc.
    if (std.mem.indexOf(u8, result.stderr, "error(gpa):") != null) {
        std.debug.print("Memory error detected (GPA)\n", .{});
        std.debug.print("STDOUT: {s}\n", .{result.stdout});
        std.debug.print("STDERR: {s}\n", .{result.stderr});
        return error.MemoryError;
    }

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
    // Check for GPA (General Purpose Allocator) errors in stderr
    // These indicate memory bugs like alignment mismatches, double frees, etc.
    if (std.mem.indexOf(u8, result.stderr, "error(gpa):") != null) {
        std.debug.print("Memory error detected (GPA)\n", .{});
        std.debug.print("STDERR: {s}\n", .{result.stderr});
        return error.MemoryError;
    }

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

// IO Spec Tests (using shared specs from fx_test_specs.zig)
// These tests use the --test mode with IO specifications to verify that
// roc applications produce the expected stdout/stderr output for given stdin.
// The specs are defined in fx_test_specs.zig and shared with the cross-compile
// test runner.

test "fx platform IO spec tests" {
    const allocator = testing.allocator;

    var passed: usize = 0;
    var failed: usize = 0;
    var skipped: usize = 0;

    for (fx_test_specs.io_spec_tests) |spec| {
        if (spec.skip.len > 0) {
            skipped += 1;
            continue;
        }
        const result = runRocTest(allocator, spec.roc_file, spec.io_spec) catch |err| {
            std.debug.print("\n[FAIL] {s}: failed to run: {}\n", .{ spec.roc_file, err });
            failed += 1;
            continue;
        };
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        checkTestSuccess(result) catch |err| {
            std.debug.print("\n[FAIL] {s}: {}\n", .{ spec.roc_file, err });
            if (spec.description.len > 0) {
                std.debug.print("       Description: {s}\n", .{spec.description});
            }
            failed += 1;
            continue;
        };

        passed += 1;
    }

    // Print summary
    const total = passed + failed;
    if (failed > 0) {
        std.debug.print("\n{}/{} IO spec tests passed ({} failed, {} skipped)\n", .{ passed, total, failed, skipped });
        return error.SomeTestsFailed;
    }
}

test "fx platform expect with main" {
    const allocator = testing.allocator;

    // Run `roc test` on the app that has both main! and an expect
    // Note: `roc test` only evaluates expect statements, it does not run main!
    const run_result = try runRoc(allocator, "test/fx/expect_with_main.roc", .{ .extra_args = &[_][]const u8{"test"} });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // When all tests pass produce short output message
    try testing.expectStringStartsWith(run_result.stdout, "All (1) tests passed in ");
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

    // When all tests pass produce short output message
    try testing.expectStringStartsWith(run_result.stdout, "All (1) tests passed in ");
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform all_syntax_test.roc prints expected output" {
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/all_syntax_test.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    const expected_stdout =
        "Hello, world!\n" ++
        "Hello, world! (using alias)\n" ++
        "{ diff: 5, div: 2, div_trunc: 2, eq: False, gt: True, gteq: True, lt: False, lteq: False, neg: -10, neq: True, prod: 50, rem: 0, sum: 15 }\n" ++
        "{ bool_and_keyword: False, bool_or_keyword: True, not_a: False }\n" ++
        "\"One Two\"\n" ++
        "\"Three Four\"\n" ++
        "The color is red.\n" ++
        "78\n" ++
        "Success\n" ++
        "Line 1\n" ++
        "Line 2\n" ++
        "Line 3\n" ++
        "Unicode escape sequence: \u{00A0}\n" ++
        "This is an effectful function!\n" ++
        "Try.Ok(1)\n" ++
        "15.0\n" ++
        "False\n" ++
        "10.0\n" ++
        "42.0\n" ++
        "NotOneTwoNotFive\n" ++
        "(\"Roc\", 1.0)\n" ++
        "Builtin.List.[\"a\", \"b\"]\n" ++
        "(\"Roc\", 1.0, 1.0, \"Roc\")\n" ++
        "10.0\n" ++
        "{ age: 31, name: \"Alice\" }\n" ++
        "{ binary: 5.0, explicit_i128: 5, explicit_i16: 5, explicit_i32: 5, explicit_i64: 5, explicit_i8: 5, explicit_u128: 5, explicit_u16: 5, explicit_u32: 5, explicit_u64: 5, explicit_u8: 5, hex: 5.0, octal: 5.0, usage_based: 5.0 }\n" ++
        "<opaque>\n" ++
        "\"The secret key is: my_secret_key\"\n" ++
        "False\n" ++
        "99\n" ++
        "\"12345.0\"\n" ++
        "\"Foo with 42 and hello\"\n" ++
        "\"other color\"\n" ++
        "\"Names: Alice, Bob, Charlie\"\n" ++
        "\"A\"\n" ++
        "\"other letter\"\n";

    try testing.expectEqualStrings(expected_stdout, run_result.stdout);
    try testing.expectEqualStrings("ROC DBG: 42.0\n", run_result.stderr);
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

test "fx platform wildcard match on open union" {
    // Tests that wildcard patterns on open tag unions work correctly.
    // Bug: When error propagates through open tag unions [Exit(I64), ..],
    // Err(_) wildcard match was returning 0 instead of the expected value 42.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/wildcard_match_open_union_bug.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify that the wildcard match worked correctly
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "PASS: Wildcard match worked correctly") != null);
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

test "custom platform and package qualifiers work in roc run" {
    // Regression test for package qualifier extraction and execution.
    // Apps can use any identifier for their platform/package qualifiers (e.g., "fx" instead of "pf").
    // The qualifier must be correctly extracted from the app header for module resolution.
    //
    // This test uses an app with:
    // - Platform qualifier "fx" (instead of default "pf") importing fx.Stdout
    // - Package qualifier "hlp" importing hlp.Helper from a sibling package
    //
    // Two bugs were fixed:
    // 1. setupSharedMemoryWithModuleEnv hardcoded "pf." when registering platform modules
    // 2. Non-platform packages weren't loaded at all during IPC mode execution
    //
    // The test verifies the app runs correctly and produces expected output.
    //
    // See: https://github.com/roc-lang/roc/issues/9030
    const allocator = std.testing.allocator;

    // Run an app that uses custom qualifiers for both platform and package
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
            "test/fx/multi_qualifier.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Check for undefined variable errors which would indicate qualifier mismatch
    if (std.mem.indexOf(u8, run_result.stderr, "UNDEFINED VARIABLE") != null) {
        std.debug.print("\n❌ Custom qualifiers not recognized\n", .{});
        std.debug.print("This indicates the qualifiers were not correctly extracted from the app header.\n", .{});
        std.debug.print("\n========== FULL OUTPUT ==========\n", .{});
        std.debug.print("STDOUT:\n{s}\n", .{run_result.stdout});
        std.debug.print("STDERR:\n{s}\n", .{run_result.stderr});
        std.debug.print("==================================\n\n", .{});
        return error.PackageQualifierNotRecognized;
    }

    // Check that roc run succeeded
    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("\n❌ Run with custom qualifiers failed with exit code {}\n", .{code});
                std.debug.print("STDOUT:\n{s}\n", .{run_result.stdout});
                std.debug.print("STDERR:\n{s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("\n❌ Run terminated abnormally: {}\n", .{run_result.term});
            return error.AbnormalTermination;
        },
    }

    // Verify the expected output
    const expected_output = "Hello, World!";
    if (std.mem.indexOf(u8, run_result.stdout, expected_output) == null) {
        std.debug.print("\n❌ Expected output not found\n", .{});
        std.debug.print("Expected: {s}\n", .{expected_output});
        std.debug.print("Got:\n{s}\n", .{run_result.stdout});
        return error.UnexpectedOutput;
    }
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
    // The coordinator now detects additional errors (COMPTIME EVAL ERROR) beyond TYPE MISMATCH
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Found 2 error") != null);

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

// The platform requires `main! : () => {}` but test_type_mismatch.roc returns Str.
// This should be a type error.
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

    // This file is expected to fail compilation with a TYPE MISMATCH error
    // (number literal used where Str is expected in string interpolation)
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

    try checkFailure(run_result);

    // Should show the warning
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "UNUSED VARIABLE") != null);

    // Should produce output (runs successfully)
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello, World!") != null);
}

test "fx platform method inspect on string" {
    // Tests that Str.inspect works correctly on a string value
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/test_method_inspect.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Str.inspect now exists - this should succeed and output the inspected string
    try checkSuccess(run_result);

    // Should output the inspected string value
    try testing.expectEqualStrings("\"hello\"\n", run_result.stdout);
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

test "fx platform inline expect fails as expected" {
    // Regression test: inline expect inside main! should fail via the
    // normal crash handler (Roc crashed: ...) instead of overflowing
    // the stack and triggering the stack overflow handler.
    const allocator = testing.allocator;
    const run_result = try runRoc(allocator, "test/fx/issue8517.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Expect a clean failure (non-zero exit code, no signal)
    try checkFailure(run_result);

    const stderr = run_result.stderr;

    // Should report a crash with the expect expression snippet
    try testing.expect(std.mem.indexOf(u8, stderr, "1 == 2") != null);
}

test "fx platform inline expect succeeds as expected" {
    const allocator = testing.allocator;

    const result = try runRocTest(allocator, "test/fx/inline_expect_pass.roc", "1>All good.");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkTestSuccess(result);
}

test "fx platform index out of bounds in instantiate regression" {
    // Regression test: A specific combination of features causes an index out of bounds
    // panic in the type instantiation code (instantiate.zig:344). The panic occurs during
    // type checking when instantiating a tag union type.
    //
    // The crash requires:
    // - A value alias (day_input = demo_input)
    // - A print! function using split_on().for_each!()
    // - Two similar effectful functions (part1!, part2!) with:
    //   - for loop over input.trim().split_on()
    //   - print! call inside the for loop
    //   - parse_range call with ? operator
    //   - while loop calling a function with sublist()
    // - has_repeating_pattern using slice->repeat(n // $d) with mutable var $d
    // - String interpolation calling part2!
    //
    // The bug manifests as: panic: index out of bounds: index 2863311530, len 1035
    // The index 0xAAAAAAAA suggests uninitialized/corrupted memory.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/index_oob_instantiate.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The compiler should not panic/crash. Once the bug is fixed, this test will pass.
    // Currently it fails with a panic in instantiate.zig.
    try checkSuccess(run_result);
}

test "fx platform fold_rev static dispatch regression" {
    // Regression test: Calling fold_rev with static dispatch (method syntax) panics,
    // but calling it qualified as List.fold_rev(...) works fine.
    //
    // The panic occurs with: [1].fold_rev([], |elem, acc| acc.append(elem))
    // But this works: List.fold_rev([1], [], |elem, acc| acc.append(elem))
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/fold_rev_static_dispatch.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try checkSuccess(run_result);

    // Verify the expected output
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Start reverse") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Reversed: 3 elements") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Done") != null);
}

test "external platform memory alignment regression" {
    // SKIPPED: This test is currently failing due to an interpreter bug where an opaque_ptr
    // (closure/function pointer) is incorrectly passed to extractNumericValue().
    // See https://github.com/roc-lang/roc/issues/8946
    // TODO: Re-enable this test once the interpreter bug is fixed.
    return error.SkipZigTest;

    // This test verifies that external platforms with the memory alignment fix work correctly.
    // The bug was in roc-platform-template-zig < 0.6 where rocDeallocFn used
    // `roc_dealloc.alignment` directly instead of `@max(roc_dealloc.alignment, @alignOf(usize))`.
    // Fixed in https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/tag/0.6
    // const allocator = testing.allocator;
    //
    // const run_result = try runRoc(allocator, "test/fx/aoc_day2.roc", .{});
    // defer allocator.free(run_result.stdout);
    // defer allocator.free(run_result.stderr);
    //
    // try checkSuccess(run_result);
}

test "fx platform issue8826 app vs platform type mismatch" {
    // Regression test for https://github.com/roc-lang/roc/issues/8826
    // The bug was that `roc check` reported "No errors found" when the app's main!
    // signature didn't match the platform's requires. This happened because
    // getRootEnv() was returning the wrong module (the first one added rather
    // than the actual root module set in buildRoot).
    const allocator = testing.allocator;

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            roc_binary_path,
            "check",
            "test/fx/issue8826_minimal.roc",
        },
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail with a TYPE MISMATCH error because:
    // - App has: main! : List(Str) => Try({}, [Exit(I32)])
    // - Platform requires: main! : () => {}
    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                // Expected to fail - check for type mismatch error message
                try testing.expect(std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null);
            } else {
                std.debug.print("Expected type mismatch error but roc check succeeded\n", .{});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunTerminatedAbnormally;
        },
    }
}

test "fx platform issue8826 large file type checking" {
    // Regression test for https://github.com/roc-lang/roc/issues/8826
    // The bug was that running `roc <file>` on a large parser/tokenizer file
    // would silently exit with code 1 and no output. This was caused by:
    // 1. The type checker using SharedMemoryAllocator for working memory, which
    //    is a bump allocator that can't free/resize, causing OOM on large files
    // 2. The error handler not printing any message before exiting
    //
    // The fix uses ctx.gpa (real allocator) for type checker working memory,
    // and increased shared memory size to handle worst-case fragmentation.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/issue8826_full.roc", .{});
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file has type errors, so it should fail with a non-zero exit code
    // The important thing is that it should NOT silently exit - it should
    // print error messages to stderr
    try checkFailure(run_result);

    // Verify error messages are printed (not silent exit)
    // The file has mutually recursive type aliases, type mismatches, etc.
    // On Windows, we may hit OOM due to shared memory limits, which should
    // still print an error message (just not the type error message).
    const has_type_error = std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null or
        std.mem.indexOf(u8, run_result.stderr, "MUTUALLY RECURSIVE TYPE ALIASES") != null or
        std.mem.indexOf(u8, run_result.stderr, "UNDECLARED TYPE") != null;
    const has_oom_error = std.mem.indexOf(u8, run_result.stderr, "Out of memory") != null;

    if (!has_type_error and !has_oom_error) {
        std.debug.print("Expected type error or OOM output but got:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.ExpectedTypeErrors;
    }
}

test "fx platform issue8943 error message memory corruption" {
    // Regression test for https://github.com/roc-lang/roc/issues/8943
    // The bug was that error messages would display corrupted/garbled text
    // for both the filename and crash message when using the ? operator on
    // a non-Try type.
    //
    // The root cause was:
    // 1. ComptimeEvaluator.deinit() freed crash messages before reports were built
    // 2. addSourceCodeWithUnderlines didn't dupe the filename from SourceCodeDisplayRegion
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/issue8943.roc", .{
        .extra_args = &[_][]const u8{"check"},
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail with TYPE MISMATCH and COMPTIME CRASH errors
    try checkFailure(run_result);

    // Check that the TYPE MISMATCH error is present
    const has_try_type_error = std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null;
    if (!has_try_type_error) {
        std.debug.print("Expected 'TYPE MISMATCH' error but got:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.ExpectedTryTypeError;
    }

    // Check that the COMPTIME CRASH error is present
    const has_comptime_crash = std.mem.indexOf(u8, run_result.stderr, "COMPTIME CRASH") != null;
    if (!has_comptime_crash) {
        std.debug.print("Expected 'COMPTIME CRASH' error but got:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.ExpectedComptimeCrash;
    }

    // The key check: verify no memory corruption in error messages
    // Count how many times the filename appears - it should appear at least twice
    // (once in each error's source region). The bug causes the first one to be garbled.
    var filename_count: usize = 0;
    var search_start: usize = 0;
    while (std.mem.indexOfPos(u8, run_result.stderr, search_start, "issue8943.roc")) |pos| {
        filename_count += 1;
        search_start = pos + 1;
    }

    // We expect at least 2 occurrences: one in TYPE MISMATCH and one in COMPTIME CRASH
    // Plus one more at the end in "Found X error(s)..."
    if (filename_count < 3) {
        std.debug.print("Error output appears corrupted - filename 'issue8943.roc' found only {d} times (expected at least 3):\n", .{filename_count});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.CorruptedErrorOutput;
    }

    // Check for garbled/non-printable characters that indicate memory corruption
    // The replacement character (U+FFFD = 0xEF 0xBF 0xBD in UTF-8) or high bytes often appear in corruption
    for (run_result.stderr) |byte| {
        // Check for bytes that shouldn't appear in normal error output
        // Valid output should be ASCII printable characters, newlines, tabs, or ANSI escape sequences
        if (byte >= 0x80) {
            // This could be valid UTF-8 or ANSI escapes, skip for now
            continue;
        }
        if (byte < 0x20 and byte != '\n' and byte != '\r' and byte != '\t' and byte != 0x1B) {
            std.debug.print("Error output contains corrupted byte 0x{x:0>2} at position in stderr:\n", .{byte});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.CorruptedErrorOutput;
        }
    }

    // Also check that the crash message contains readable text, not garbled bytes
    // A valid crash message should contain "Try" since that's what the error is about
    const has_readable_crash_msg = std.mem.indexOf(u8, run_result.stderr, "Try") != null;
    if (!has_readable_crash_msg) {
        std.debug.print("Crash message appears corrupted - expected 'Try' not found:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.CorruptedCrashMessage;
    }
}

test "fx platform issue9118 try operator on tuple in type method" {
    // Regression test for https://github.com/roc-lang/roc/issues/9118
    // The bug was that using the ? operator on a tuple (instead of a Try type)
    // inside a type method would cause a segfault in the interpreter.
    // The ? operator expects a Try type [Ok(a), Err(e)] but was given a tuple.
    const allocator = testing.allocator;

    const run_result = try runRoc(allocator, "test/fx/for_var_in_type_method.roc", .{
        .extra_args = &[_][]const u8{"test"},
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail with a TYPE MISMATCH error because
    // the ? operator is used on a tuple (Value, [Ok, Err(Str)]) instead of
    // a Try type [Ok(Value), Err(Str)].
    // The important thing is that it should NOT segfault - it should report
    // the type error gracefully.

    switch (run_result.term) {
        .Exited => |code| {
            if (code == 0) {
                std.debug.print("Expected type error but test succeeded\n", .{});
                return error.UnexpectedSuccess;
            }
            // Expected to fail - check for type mismatch error message
            const has_type_error = std.mem.indexOf(u8, run_result.stderr, "TYPE MISMATCH") != null;
            if (!has_type_error) {
                std.debug.print("Expected 'TYPE MISMATCH' error but got:\n", .{});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.ExpectedTypeError;
            }
            // Verify it mentions the ? operator and Try type
            const mentions_try = std.mem.indexOf(u8, run_result.stderr, "Try") != null;
            if (!mentions_try) {
                std.debug.print("Expected error to mention 'Try' type but got:\n", .{});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.ExpectedTryMention;
            }
        },
        .Signal => |sig| {
            // This is the bug we're testing for - it should NOT crash with a signal
            std.debug.print("CRITICAL: Test crashed with signal {} (this is the bug we're testing for)\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.Segfault;
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunTerminatedAbnormally;
        },
    }
}
