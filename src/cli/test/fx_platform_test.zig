//! Integration tests for the fx platform with effectful functions.
//!
//! Tests that platform-provided hosted functions (like Stdout.line! and Stderr.line!)
//! can be properly invoked from Roc applications.
//!
//! NOTE: These tests depend on the roc binary being built via build.zig. The test step
//! has a dependency on roc, so the binary will be built automatically before tests run.
//!
//! IMPORTANT: Do NOT use --no-cache when running roc. The interpreted host doesn't change between
//! tests (we're testing app behaviour, not the platform), so using --no-cache would force unnecessary
//! re-linking on every test, making the test run much slower than is necessary.
//!
//! The shared IO-spec matrix is owned by the parallel CLI platform runner.
//! This file keeps fx-specific tests that are not covered by that matrix.

const std = @import("std");
const testing = std.testing;
const util = @import("util.zig");
const fx_test_specs = @import("fx_test_specs.zig");

const FxPlatformTestError = util.RocRunError || util.ChildTimeoutError || util.ResultCheckError || std.mem.Allocator.Error || std.Io.Dir.RealPathFileAllocError || std.Io.Dir.CreateDirPathError || std.Io.Dir.ReadFileAllocError || error{
    DevBackendBuildFailed,
    DivisionByZeroNotHandled,
    StackOverflowNotHandled,
    StaticDataHostBinaryContainsComptimeOnlyString,
    StaticDataHostBinaryMissingString,
    StaticDataHostTestFailed,
    TestExpectedEqual,
    TestExpectedStartsWith,
    TestUnexpectedResult,
    UnexpectedExitCode,
    UnexpectedTermination,
};

// Wire up tests from fx_test_specs module
comptime {
    std.testing.refAllDecls(fx_test_specs);
}

fn runDevBackendHostSelfTest(
    allocator: std.mem.Allocator,
    roc_file: []const u8,
    self_test_flag: []const u8,
) FxPlatformTestError!std.process.RunResult {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    const output_path = try std.fs.path.join(allocator, &.{ tmp_path, "fx_dev_host_test" });
    defer allocator.free(output_path);

    const cache_path = try std.fs.path.join(allocator, &.{ tmp_path, "roc-cache" });
    defer allocator.free(cache_path);
    try tmp_dir.dir.createDirPath(std.testing.io, "roc-cache");

    const zig_local_cache_path = try std.fs.path.join(allocator, &.{ tmp_path, "zig-local-cache" });
    defer allocator.free(zig_local_cache_path);
    try tmp_dir.dir.createDirPath(std.testing.io, "zig-local-cache");

    const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_path});
    defer allocator.free(output_arg);

    // In Zig 0.16, Environ.Block is GlobalBlock on Windows (PEB-backed) vs PosixBlock on POSIX.
    const environ: std.process.Environ = if (@import("builtin").os.tag == .windows) .{
        .block = .global,
    } else blk: {
        const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
        break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    };
    var env_map = try environ.createMap(allocator);
    defer env_map.deinit();
    try env_map.put("ROC_CACHE_DIR", cache_path);
    try env_map.put("ZIG_LOCAL_CACHE_DIR", zig_local_cache_path);

    const build_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        util.roc_binary_path,
        "build",
        "--opt=dev",
        output_arg,
        roc_file,
    }, .{
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    switch (build_result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("roc build --opt=dev failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{build_result.stdout});
                std.debug.print("STDERR: {s}\n", .{build_result.stderr});
                return error.DevBackendBuildFailed;
            }
        },
        else => {
            std.debug.print("roc build --opt=dev terminated abnormally: {}\n", .{build_result.term});
            std.debug.print("STDOUT: {s}\n", .{build_result.stdout});
            std.debug.print("STDERR: {s}\n", .{build_result.stderr});
            return error.DevBackendBuildFailed;
        },
    }

    return try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        output_path,
        self_test_flag,
    }, .{
        .max_output_bytes = 10 * 1024 * 1024,
    });
}

fn buildAndRunDevBackendApp(
    allocator: std.mem.Allocator,
    roc_file: []const u8,
    output_basename: []const u8,
    inspect_output: ?*const fn (std.mem.Allocator, []const u8) FxPlatformTestError!void,
) FxPlatformTestError!std.process.RunResult {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    const output_path = try std.fs.path.join(allocator, &.{ tmp_path, output_basename });
    defer allocator.free(output_path);

    const cache_path = try std.fs.path.join(allocator, &.{ tmp_path, "roc-cache" });
    defer allocator.free(cache_path);
    try tmp_dir.dir.createDirPath(std.testing.io, "roc-cache");

    const zig_local_cache_path = try std.fs.path.join(allocator, &.{ tmp_path, "zig-local-cache" });
    defer allocator.free(zig_local_cache_path);
    try tmp_dir.dir.createDirPath(std.testing.io, "zig-local-cache");

    const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_path});
    defer allocator.free(output_arg);

    // In Zig 0.16, Environ.Block is GlobalBlock on Windows (PEB-backed) vs PosixBlock on POSIX.
    const environ: std.process.Environ = if (@import("builtin").os.tag == .windows) .{
        .block = .global,
    } else blk: {
        const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
        break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    };
    var env_map = try environ.createMap(allocator);
    defer env_map.deinit();
    try env_map.put("ROC_CACHE_DIR", cache_path);
    try env_map.put("ZIG_LOCAL_CACHE_DIR", zig_local_cache_path);

    const build_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        util.roc_binary_path,
        "build",
        "--opt=dev",
        output_arg,
        roc_file,
    }, .{
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    switch (build_result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("roc build --opt=dev failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{build_result.stdout});
                std.debug.print("STDERR: {s}\n", .{build_result.stderr});
                return error.DevBackendBuildFailed;
            }
        },
        else => {
            std.debug.print("roc build --opt=dev terminated abnormally: {}\n", .{build_result.term});
            std.debug.print("STDOUT: {s}\n", .{build_result.stdout});
            std.debug.print("STDERR: {s}\n", .{build_result.stderr});
            return error.DevBackendBuildFailed;
        },
    }

    if (inspect_output) |inspect| {
        try inspect(allocator, output_path);
    }

    return try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{output_path}, .{
        .max_output_bytes = 10 * 1024 * 1024,
    });
}

fn expectInterpreterRuntimeStackOverflow() FxPlatformTestError!void {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/stack_overflow_runtime.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .exited => |code| {
            if (code != 1) {
                std.debug.print("Unexpected interpreter exit code: {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedExitCode;
            }
            try testing.expect(std.mem.find(u8, run_result.stderr, "Roc crashed:") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "This Roc program overflowed its stack memory") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "divided by zero") == null);
        },
        else => {
            std.debug.print("Unexpected interpreter termination: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.UnexpectedTermination;
        },
    }
}

fn expectDevRuntimeStackOverflow() FxPlatformTestError!void {
    const allocator = testing.allocator;

    const run_result = try runDevBackendHostSelfTest(
        allocator,
        "test/fx/hello_world.roc",
        "--host-test-stack-overflow",
    );
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .exited => |code| {
            if (code != 134) {
                std.debug.print("Unexpected dev exit code: {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedExitCode;
            }
            try testing.expect(std.mem.find(u8, run_result.stderr, "This Roc application overflowed its stack memory and crashed.") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "divided by zero") == null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "Roc crashed:") == null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "panic:") == null);
        },
        .signal => |sig| {
            std.debug.print("Host self-test crashed with signal {}\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.StackOverflowNotHandled;
        },
        else => {
            std.debug.print("Unexpected dev termination: {}\n", .{run_result.term});
            return error.UnexpectedTermination;
        },
    }
}

fn expectInterpreterRuntimeDivisionByZero() FxPlatformTestError!void {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/division_by_zero.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .exited => |code| {
            if (code != 1) {
                std.debug.print("Unexpected interpreter exit code: {}\n", .{code});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedExitCode;
            }
            try testing.expect(std.mem.find(u8, run_result.stderr, "Roc crashed:") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "I64 division by zero") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "overflowed its stack memory") == null);
        },
        else => {
            std.debug.print("Unexpected interpreter termination: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.UnexpectedTermination;
        },
    }
}

fn expectDevRuntimeDivisionByZero() FxPlatformTestError!void {
    const allocator = testing.allocator;

    const run_result = try buildAndRunDevBackendApp(
        allocator,
        "test/fx/division_by_zero.roc",
        "fx_dev_division_by_zero",
        null,
    );
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .exited => |code| {
            if (code != 1) {
                std.debug.print("Unexpected dev exit code: {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedExitCode;
            }
            try testing.expect(std.mem.find(u8, run_result.stderr, "Roc crashed:") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "I64 division by zero") != null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "overflowed its stack memory") == null);
            try testing.expect(std.mem.find(u8, run_result.stderr, "panic:") == null);
        },
        .signal => |sig| {
            std.debug.print("Dev runtime division by zero crashed with signal {}\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.DivisionByZeroNotHandled;
        },
        else => {
            std.debug.print("Unexpected dev termination: {}\n", .{run_result.term});
            return error.UnexpectedTermination;
        },
    }
}

// IO spec helper for narrow fx-only Zig tests. The broad shared IO-spec matrix
// runs through the parallel CLI platform runner.
fn runIoSpecTest(comptime opt_flag: []const u8, spec: fx_test_specs.TestSpec) FxPlatformTestError!void {
    const allocator = testing.allocator;

    const result = util.runRocCommand(std.testing.io, allocator, &.{ opt_flag, spec.roc_file, "--", "--test", spec.io_spec }) catch |err| {
        std.debug.print("\n[FAIL] {s} ({s}): failed to run: {}\n", .{ spec.roc_file, opt_flag, err });
        return err;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    util.checkTestSuccess(result) catch |err| {
        std.debug.print("\n[FAIL] {s} ({s}): {}\n", .{ spec.roc_file, opt_flag, err });
        if (spec.description.len > 0) {
            std.debug.print("       Description: {s}\n", .{spec.description});
        }
        return err;
    };
}

test "fx platform boxed erased callable host boundary (interpreter)" {
    try runIoSpecTest("--opt=interpreter", fx_test_specs.host_boxed_fn_boundary_test);
}

test "fx platform boxed erased callable host boundary (dev backend)" {
    try runIoSpecTest("--opt=dev", fx_test_specs.host_boxed_fn_boundary_test);
}

test "fx platform direct run preserves RocOps after F32.abs before list allocation" {
    const allocator = testing.allocator;

    // Repro for https://github.com/roc-lang/roc/issues/9846:
    // direct-run should preserve RocOps across F32.abs and the later list allocation.
    const result = try util.runRocCommand(std.testing.io, allocator, &.{
        "test/fx/issue_9846_direct_abs_list.roc",
        "--",
        "--test",
        "1>count 1",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try util.checkTestSuccess(result);
}

test "provided static data exports are host-linkable readonly constants" {
    const allocator = testing.allocator;

    const run_result = try buildAndRunDevBackendApp(
        allocator,
        "test/static-data-host/app.roc",
        "static_data_host_test",
        inspectStaticDataHostBinary,
    );
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("static data host test exited with code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.StaticDataHostTestFailed;
            }
        },
        else => {
            std.debug.print("static data host test terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.StaticDataHostTestFailed;
        },
    }

    try testing.expectEqualStrings("", run_result.stdout);
    try testing.expectEqualStrings("static data host constants ok\n", run_result.stderr);
}

fn inspectStaticDataHostBinary(allocator: std.mem.Allocator, output_path: []const u8) FxPlatformTestError!void {
    const bytes = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, output_path, allocator, .limited(256 * 1024 * 1024));
    defer allocator.free(bytes);

    const required = [_][]const u8{
        "literal readonly string longer than thirty bytes",
        "assembled readonly first string from comptime concat",
        "assembled readonly second string from comptime concat",
        "assembled readonly first string from comptime concat + assembled readonly second string from comptime concat",
        "final readonly string after comptime branch",
        "STATIC_SLICE_SOURCE:abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    };

    for (required) |needle| {
        if (std.mem.find(u8, bytes, needle) == null) {
            std.debug.print("compiled static-data-host binary is missing expected static string: {s}\n", .{needle});
            return error.StaticDataHostBinaryMissingString;
        }
    }

    const forbidden = [_][]const u8{
        "INTERMEDIATE_ONLY_LEFT_SHOULD_NOT_BE_EMITTED",
        "INTERMEDIATE_ONLY_RIGHT_SHOULD_NOT_BE_EMITTED",
        "unreachable readonly string after comptime branch",
    };

    for (forbidden) |needle| {
        if (std.mem.find(u8, bytes, needle) != null) {
            std.debug.print("compiled static-data-host binary contains comptime-only string: {s}\n", .{needle});
            return error.StaticDataHostBinaryContainsComptimeOnlyString;
        }
    }
}

/// Shared body for "roc test" tests that expect exactly 1 passing test.
fn testRocTestSinglePass(opt: []const u8, roc_file: []const u8) FxPlatformTestError!void {
    const allocator = testing.allocator;
    const run_result = try util.runRoc(std.testing.io, allocator, &.{ "test", opt }, roc_file);
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);
    try util.checkSuccess(run_result);
    try testing.expectStringStartsWith(run_result.stdout, "All (1) tests passed in ");
    try testing.expectEqualStrings("", run_result.stderr);
}

test "fx platform expect with main (interpreter)" {
    try testRocTestSinglePass("--opt=interpreter", "test/fx/expect_with_main.roc");
}
test "fx platform expect with main (dev backend)" {
    try testRocTestSinglePass("--opt=dev", "test/fx/expect_with_main.roc");
}

test "fx platform expect with numeric literal (interpreter)" {
    try testRocTestSinglePass("--opt=interpreter", "test/fx/expect_with_literal.roc");
}
test "fx platform expect with numeric literal (dev backend)" {
    try testRocTestSinglePass("--opt=dev", "test/fx/expect_with_literal.roc");
}

test "fx platform byte-list file import finalizes bytes correctly" {
    // Repro for https://github.com/roc-lang/roc/issues/9866: importing a
    // source-relative file as List(U8) should produce a byte list with the
    // imported file length during compile-time finalization.
    try testRocTestSinglePass("--opt=dev", "test/fx/file_import_bytes_check_crash.roc");
}

test "fx platform match returning string" {
    // Tests that match expressions with string returns work correctly
    const allocator = testing.allocator;

    const result = try util.runRocTest(std.testing.io, allocator, "test/fx/match_str_return.roc", "1>0");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try util.checkTestSuccess(result);
}

test "fx platform match with wildcard" {
    // Tests that wildcard patterns in match expressions work correctly
    const allocator = testing.allocator;

    const result = try util.runRocTest(std.testing.io, allocator, "test/fx/match_with_wildcard.roc", "1>0");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try util.checkTestSuccess(result);
}

test "fx platform zst nested singleton shapes" {
    const allocator = testing.allocator;

    const spec = fx_test_specs.findByPath("test/fx/zst_nested_singleton_shapes.roc").?;
    const result = try util.runRocTest(std.testing.io, allocator, spec.roc_file, spec.io_spec);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try util.checkTestSuccess(result);
}

test "fx platform wildcard match on open union (interpreter)" {
    // Tests that wildcard patterns on open tag unions work correctly.
    // Bug: When error propagates through open tag unions [Exit(I64), ..],
    // Err(_) wildcard match was returning 0 instead of the expected value 42.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/wildcard_match_open_union_bug.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify that the wildcard match worked correctly
    try testing.expect(std.mem.find(u8, run_result.stdout, "PASS: Wildcard match worked correctly") != null);
}

test "fx platform wildcard match on open union (dev backend)" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=dev"}, "test/fx/wildcard_match_open_union_bug.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify that the wildcard match worked correctly
    try testing.expect(std.mem.find(u8, run_result.stdout, "PASS: Wildcard match worked correctly") != null);
}

test "fx platform nested tag match in statement position (dev backend)" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=dev"}, "test/fx/nested_tag_match_stmt.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    try testing.expect(std.mem.find(u8, run_result.stdout, "PASS: statement-position nested tag match works") != null);
}

test "fx platform dbg missing return value (interpreter)" {
    const allocator = testing.allocator;

    // Run an app that uses dbg as the last expression in main!.
    // dbg is treated as a statement (side-effect only) when it's the final
    // expression in a block, so the block returns {} as expected by main!.
    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/dbg_missing_return.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify that the dbg output was printed
    try testing.expect(std.mem.find(u8, run_result.stderr, "this should work now") != null);
}

test "fx platform dbg missing return value (dev backend)" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=dev"}, "test/fx/dbg_missing_return.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify that the dbg output was printed
    try testing.expect(std.mem.find(u8, run_result.stderr, "this should work now") != null);
}

test "fx platform check unused state var reports correct errors" {
    const allocator = testing.allocator;

    // Run `roc check` on an app with unused variables and type annotations
    // This test checks that the compiler reports the correct errors and doesn't
    // produce extraneous unrelated errors from platform module resolution
    const run_result = try util.runRoc(std.testing.io, allocator, &.{"check"}, "test/fx/unused_state_var.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The check should fail with errors
    switch (run_result.term) {
        .exited => |code| {
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
        if (std.mem.find(u8, line, "UNUSED VARIABLE") != null) {
            unused_variable_count += 1;
        } else if (std.mem.find(u8, line, "MODULE NOT FOUND") != null) {
            module_not_found_count += 1;
        } else if (std.mem.find(u8, line, "EXPOSED BUT NOT DEFINED") != null) {
            exposed_but_not_defined_count += 1;
        }
    }

    // We expect exactly 2 UNUSED VARIABLE errors
    // We should NOT get MODULE NOT FOUND or EXPOSED BUT NOT DEFINED errors
    // (those were the extraneous errors this test was created to catch)
    //
    // Note: There are other errors (TYPE MISMATCH, NAME NOT IN SCOPE for main!,
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
    const run_result = try util.runRoc(std.testing.io, allocator, &.{"check"}, "test/fx/platform/main.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    const stderr = run_result.stderr;

    // Count MODULE NOT FOUND errors - we should get 0 since sibling modules are discovered
    var module_not_found_count: usize = 0;

    var line_iter = std.mem.splitScalar(u8, stderr, '\n');
    while (line_iter.next()) |line| {
        if (std.mem.find(u8, line, "MODULE NOT FOUND") != null) {
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

test "custom platform and package qualifiers work in default roc command" {
    // Regression test for package qualifier extraction and execution.
    // Apps can use any identifier for their platform/package qualifiers (e.g., "fx" instead of "pf").
    // The qualifier must be correctly extracted from the app header for module resolution.
    //
    // This test uses an app with:
    // - Platform qualifier "fx" (instead of default "pf") importing fx.Stdout
    // - Package qualifier "hlp" importing hlp.Helper from a sibling package
    //
    // Two bugs were fixed:
    // 1. Runtime-image publication hardcoded "pf." when registering platform modules
    // 2. Non-platform packages weren't loaded at all during IPC mode execution
    //
    // The test verifies the app runs correctly and produces expected output.
    //
    // See: https://github.com/roc-lang/roc/issues/9030
    const allocator = std.testing.allocator;

    // Run an app that uses custom qualifiers for both platform and package
    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/multi_qualifier.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Check for name-not-in-scope errors which would indicate qualifier mismatch
    if (std.mem.find(u8, run_result.stderr, "NAME NOT IN SCOPE") != null) {
        std.debug.print("\n❌ Custom qualifiers not recognized\n", .{});
        std.debug.print("This indicates the qualifiers were not correctly extracted from the app header.\n", .{});
        std.debug.print("\n========== FULL OUTPUT ==========\n", .{});
        std.debug.print("STDOUT:\n{s}\n", .{run_result.stdout});
        std.debug.print("STDERR:\n{s}\n", .{run_result.stderr});
        std.debug.print("==================================\n\n", .{});
        return error.PackageQualifierNotRecognized;
    }

    // Check that the default roc command succeeded
    switch (run_result.term) {
        .exited => |code| {
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
    if (std.mem.find(u8, run_result.stdout, expected_output) == null) {
        std.debug.print("\n❌ Expected output not found\n", .{});
        std.debug.print("Expected: {s}\n", .{expected_output});
        std.debug.print("Got:\n{s}\n", .{run_result.stdout});
        return error.UnexpectedOutput;
    }
}

test "fx platform string interpolation type mismatch (interpreter)" {
    const allocator = testing.allocator;

    // Run an app that tries to interpolate a U8 (non-Str) type in a string.
    // This should fail with a type error because string interpolation only accepts Str.
    const run_result = try util.runRocCommand(std.testing.io, allocator, &.{
        "--opt=interpreter",
        "test/fx/num_method_call.roc",
        "--allow-errors",
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // `--allow-errors` may exit successfully after reporting diagnostics, but
    // it must not publish checked artifacts or run LIR for an erroneous graph.
    switch (run_result.term) {
        .exited => |code| {
            try testing.expectEqual(@as(u8, 0), code);
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    try testing.expectEqualStrings("", run_result.stdout);

    // Verify the error output contains proper diagnostic info
    // Should show TYPE MISMATCH error with the type information
    try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
    try testing.expect(std.mem.find(u8, run_result.stderr, "U8") != null);
    try testing.expect(std.mem.find(u8, run_result.stderr, "Str") != null);
    try testing.expect(std.mem.find(u8, run_result.stderr, "Found 1 error") != null);
}

test "fx platform string interpolation type mismatch (dev backend)" {
    // TODO: dev backend exits with code 134
    return error.SkipZigTest;
}

test "fx platform run from different cwd" {
    // Regression test: Running roc from a different current working directory
    // than the project root should still work. Previously this failed with
    // "error.InvalidAppPath" because the path resolution didn't handle
    // running from a subdirectory correctly.
    const allocator = testing.allocator;

    // Get absolute path to roc binary since we'll change cwd
    const roc_abs_path = try std.Io.Dir.cwd().realPathFileAlloc(std.testing.io, util.roc_binary_path, allocator);
    defer allocator.free(roc_abs_path);
    var env_map = try util.buildIsolatedTestEnvMap(std.testing.io, allocator, null);
    defer env_map.deinit();

    // Run roc from the test/fx directory with a relative path to app.roc
    const run_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        roc_abs_path,
        "app.roc",
    }, .{
        .cwd = "test/fx",
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    switch (run_result.term) {
        .exited => |code| {
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
    try testing.expect(std.mem.find(u8, run_result.stdout, "Hello from stdout!") != null);
}

test "drop_prefix segfault regression" {
    // Regression test: Calling drop_prefix on a string literal and assigning
    // the result to an unused variable causes a segfault.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/drop_prefix_segfault.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
    try testing.expectEqualStrings("000000000000000000000000\n", run_result.stdout);
}

test "drop_prefix match use-after-free regression" {
    // Regression test: Calling drop_prefix on a string literal and using the
    // result in a match expression causes a use-after-free panic.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/drop_prefix_match_uaf.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Also check for panic messages in stderr that indicate use-after-free
    if (std.mem.find(u8, run_result.stderr, "panic") != null or
        std.mem.find(u8, run_result.stderr, "use-after-free") != null or
        std.mem.find(u8, run_result.stderr, "Invalid pointer") != null)
    {
        std.debug.print("Detected memory safety panic in stderr:\n{s}\n", .{run_result.stderr});
        return error.UseAfterFree;
    }
}

test "str seamless slice rc uses original allocation pointer" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=dev"}, "test/fx/str_seamless_slice_rc.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
    try testing.expectEqualStrings("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\n", run_result.stdout);
    if (std.mem.find(u8, run_result.stderr, "[Roc Memory Info]") != null) {
        std.debug.print("Detected leaked allocation in seamless-slice RC regression:\n{s}\n", .{run_result.stderr});
        return error.SeamlessSliceRcLeak;
    }
}

test "multiline string split_on" {
    // Tests splitting a multiline string and iterating over the lines.
    // This is a regression test to ensure split_on works correctly with
    // multiline strings and doesn't cause memory issues.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/multiline_split_leak.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify the output contains lines from the multiline string
    try testing.expect(std.mem.find(u8, run_result.stdout, "This is a longer line number one") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "This is a longer line number two") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "L68") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "The last line is here") != null);
}

test "big string equality regression (interpreter)" {
    // Regression test: String literals of length >= 24 (big strings) must work
    // correctly in expect expressions. This tests the single-segment string
    // fast path in str_collect which previously caused use-after-free.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{ "test", "--opt=interpreter" }, "test/fx/big_string_equality.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Check for panic messages in stderr that indicate use-after-free
    if (std.mem.find(u8, run_result.stderr, "panic") != null or
        std.mem.find(u8, run_result.stderr, "use-after-free") != null or
        std.mem.find(u8, run_result.stderr, "Use-after-free") != null)
    {
        std.debug.print("Detected memory safety panic in stderr:\n{s}\n", .{run_result.stderr});
        return error.UseAfterFree;
    }
}

test "big string equality regression (dev backend)" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{ "test", "--opt=dev" }, "test/fx/big_string_equality.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    if (std.mem.find(u8, run_result.stderr, "panic") != null or
        std.mem.find(u8, run_result.stderr, "use-after-free") != null or
        std.mem.find(u8, run_result.stderr, "Use-after-free") != null)
    {
        std.debug.print("Detected memory safety panic in stderr:\n{s}\n", .{run_result.stderr});
        return error.UseAfterFree;
    }
}

test "fx platform expect with toplevel numeric (interpreter)" {
    const allocator = testing.allocator;

    // Run the app
    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/expect_with_toplevel_numeric.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    try testing.expect(std.mem.find(u8, run_result.stdout, "hello") != null);

    // Run `roc test` since this file has a top-level expect
    const test_result = try util.runRoc(std.testing.io, allocator, &.{ "test", "--opt=interpreter" }, "test/fx/expect_with_toplevel_numeric.roc");
    defer allocator.free(test_result.stdout);
    defer allocator.free(test_result.stderr);

    try util.checkSuccess(test_result);
}

test "fx platform expect with toplevel numeric (dev backend)" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=dev"}, "test/fx/expect_with_toplevel_numeric.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
    try testing.expect(std.mem.find(u8, run_result.stdout, "hello") != null);

    const test_result = try util.runRoc(std.testing.io, allocator, &.{ "test", "--opt=dev" }, "test/fx/expect_with_toplevel_numeric.roc");
    defer allocator.free(test_result.stdout);
    defer allocator.free(test_result.stderr);

    try util.checkSuccess(test_result);
}

// The platform requires `main! : () => {}` but test_type_mismatch.roc returns Str.
// This should be a type error.
test "fx platform test_type_mismatch" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/test_type_mismatch.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail compilation with a type mismatch error.
    switch (run_result.term) {
        .exited => |code| {
            if (code != 0) {
                // Expected to fail - check for type mismatch error message
                try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
            } else {
                std.debug.print("Expected compilation error but succeeded\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            // Abnormal termination should also indicate error
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
        },
    }
}

test "fx platform inspect_wrong_sig reports type mismatch" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/inspect_wrong_sig_test.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The file declares a BadColor type whose to_inspect returns I64 instead of Str,
    // which is rejected by the type checker. We only need a TYPE MISMATCH report;
    // exact exit semantics aren't asserted because the dev path may bail differently.
    try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
}

test "fx platform issue8433" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/issue8433.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail compilation with a TYPE MISMATCH error
    // (number literal used where Str is expected in string interpolation)
    switch (run_result.term) {
        .exited => |code| {
            if (code != 0) {
                // Expected to fail - check for type mismatch error message
                try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
            } else {
                std.debug.print("Expected compilation error but succeeded\n", .{});
                return error.UnexpectedSuccess;
            }
        },
        else => {
            // Abnormal termination should also indicate error
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
        },
    }
}

test "run aborts on type errors by default" {
    // Tests that the default roc command aborts when there are type errors (without --allow-errors)
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/run_allow_errors.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should fail with type errors
    try util.checkFailure(run_result);

    // Should show the errors
    try testing.expect(std.mem.find(u8, run_result.stderr, "NAME NOT IN SCOPE") != null);
}

test "run aborts on parse errors by default" {
    // Tests that the default roc command aborts when there are parse errors (without --allow-errors)
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/parse_error.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should fail with parse errors
    try util.checkFailure(run_result);

    // Should show the errors
    try testing.expect(std.mem.find(u8, run_result.stderr, "PARSE ERROR") != null);
}

test "run with --allow-errors attempts execution despite type errors" {
    // Tests that `roc --allow-errors` attempts to execute even with type errors.
    // TODO: remove Windows workaround once the shared LIR image path
    // handles crash-on-type-error consistently on Windows.
    const opt_flag: []const u8 = if (@import("builtin").os.tag == .windows) "--opt=interpreter" else "--opt=dev";
    const allocator = testing.allocator;

    const run_result = try util.runRocCommand(std.testing.io, allocator, &.{
        opt_flag,
        "test/fx/run_allow_errors.roc",
        "--allow-errors",
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should still show the errors
    try testing.expect(std.mem.find(u8, run_result.stderr, "NAME NOT IN SCOPE") != null);

    // The program will attempt to run and likely crash, which is expected behavior
    // We just verify it didn't abort during type checking
}

test "run with --allow-errors handles type mismatch in function args" {
    // Regression test for https://github.com/roc-lang/roc/issues/9263
    // The dev backend crashed (SIGABRT) when --allow-errors was used on code
    // where a type mismatch in a function argument caused the return type to
    // become an error type variable nested inside a function type.
    const allocator = testing.allocator;

    const run_result = try util.runRocCommand(std.testing.io, allocator, &.{
        "--opt=dev",
        "test/fx/allow_errors_type_mismatch.roc",
        "--allow-errors",
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should report the type mismatch
    try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);

    // Must not crash with SIGABRT — the process should exit cleanly (or with
    // a runtime error exit code), not be killed by a signal.
    switch (run_result.term) {
        .exited => {},
        .signal => |sig| {
            std.debug.print("CRASH: process killed by signal {}\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.TestUnexpectedResult;
        },
        else => |term| {
            std.debug.print("Unexpected termination: {}\n", .{term});
            return error.TestUnexpectedResult;
        },
    }
}

test "run allows warnings without blocking execution" {
    // Tests that warnings don't block execution (they never should)
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/run_warning_only.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkFailure(run_result);

    // Should show the warning
    try testing.expect(std.mem.find(u8, run_result.stderr, "UNUSED VARIABLE") != null);

    // Should produce output (runs successfully)
    try testing.expect(std.mem.find(u8, run_result.stdout, "Hello, World!") != null);
}

test "fx platform check warns for adjacent string pattern captures" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{ "check", "--no-cache" }, "test/fx/string_pattern_adjacent_capture_warning.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkFailure(run_result);
    try testing.expect(std.mem.find(u8, run_result.stderr, "UNREACHABLE PATTERN CAPTURE") != null);
    try testing.expect(std.mem.find(u8, run_result.stderr, "0 error") != null);
}

test "fx platform run warns for adjacent string pattern captures without crashing" {
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--no-cache"}, "test/fx/string_pattern_adjacent_capture_warning.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkFailure(run_result);
    try testing.expect(std.mem.find(u8, run_result.stderr, "UNREACHABLE PATTERN CAPTURE") != null);
    try testing.expect(std.mem.find(u8, run_result.stderr, "panic") == null);
    try testing.expect(std.mem.find(u8, run_result.stderr, "Segmentation fault") == null);
}

test "fx platform method inspect on string" {
    // Tests that Str.inspect works correctly on a string value
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/test_method_inspect.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Str.inspect now exists - this should succeed and output the inspected string
    try util.checkSuccess(run_result);

    // Should output the inspected string value
    try testing.expectEqualStrings("\"hello\"\n", run_result.stdout);
}

test "fx platform if-expression closure capture regression" {
    // Regression test: Variables bound inside an if-expression's block were
    // incorrectly being captured as free variables by the enclosing lambda,
    // causing a crash with "e_closure: failed to resolve capture value".
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/if-closure-capture.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
}

test "fx platform var with string interpolation segfault (interpreter)" {
    // Regression test: Using `var` variables with string interpolation causes segfault.
    // The code calls fnA! multiple times, each using var state variables, and
    // interpolates the results into strings.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/var_interp_segfault.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify the expected output
    try testing.expect(std.mem.find(u8, run_result.stdout, "A1: 1") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "A2: 1") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "A3: 1") != null);
}

test "fx platform var with string interpolation segfault (dev backend)" {
    // Regression test: same as the interpreter variant but using the dev backend.
    // Previously crashed with unresolved symbol panic in LirCodeGen.zig because
    // Stdout.line! (a hosted function) was passed directly to for_each!.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/var_interp_segfault.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify the expected output
    try testing.expect(std.mem.find(u8, run_result.stdout, "A1: 1") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "A2: 1") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "A3: 1") != null);
}

test "fx platform sublist method on inferred type" {
    // Regression test: Calling .sublist() method on a List(U8) from "".to_utf8()
    // causes a segfault when the variable doesn't have an explicit type annotation.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/sublist_method_segfault.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
}

test "fx platform repeating pattern segfault (interpreter)" {
    // Regression test: This test exposed a compiler bug where variables used multiple times
    // in consuming positions didn't get proper refcount handling. Specifically,
    // in `repeat_helper(acc.concat(list), list, n-1)`, the variable `list` is
    // passed to both concat (consuming) and to the recursive call (consuming).
    // The compiler must insert a copy/incref for the second use to avoid use-after-free.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/repeating_pattern_segfault.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
}

test "fx platform hosted function passed as argument to higher-order function" {
    // Regression test for https://github.com/roc-lang/roc/issues/9264
    // The dev backend panics with "unresolved symbol" when a hosted effectful
    // function (e.g. Stdout.line!) is passed directly as an argument to a
    // higher-order function (e.g. for_each!), instead of being wrapped in a lambda.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/hosted_fn_as_arg.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    try testing.expectEqualStrings("hello\n", run_result.stdout);
}

test "fx platform runtime stack overflow" {
    // Keep coverage for both paths:
    // 1. The normal interpreter path on the real runtime sample.
    // 2. The compiled dev-backend host path via the FX host self-test hook.
    try expectInterpreterRuntimeStackOverflow();
    try expectDevRuntimeStackOverflow();
}

test "fx platform runtime division by zero" {
    // The divisor is mutable in the Roc app, so this covers runtime execution
    // rather than compile-time finalization.
    try expectInterpreterRuntimeDivisionByZero();
    try expectDevRuntimeDivisionByZero();
}

test "fx platform inline expect fails as expected (interpreter)" {
    // Regression test: inline expect inside main! should fail via the
    // normal crash handler (Roc crashed: ...) instead of overflowing
    // the stack and triggering the stack overflow handler.
    const allocator = testing.allocator;
    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/issue8517.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Expect a clean failure (non-zero exit code, no signal)
    try util.checkFailure(run_result);

    const stderr = run_result.stderr;

    // The platform receives failed expectations through the expect-failed host
    // callback, not through the crash callback.
    try testing.expect(std.mem.find(u8, stderr, "Expect failed: expect failed") != null);
}

test "fx platform inline expect fails as expected (dev backend)" {
    // TODO: dev backend succeeds when it should fail (inline expect not evaluated)
    return error.SkipZigTest;
}

test "fx platform inline expect succeeds as expected" {
    const allocator = testing.allocator;

    const result = try util.runRocTest(std.testing.io, allocator, "test/fx/inline_expect_pass.roc", "1>All good.");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try util.checkTestSuccess(result);
}

test "fx platform inline expect fails in dev backend binary" {
    // Regression test for #9261: the dev backend (object file compilation) must
    // evaluate inline expect expressions. Previously, lowered `s_expect`
    // statements did not wrap the condition in an .expect node, causing the dev
    // backend to silently skip the assertion.
    const allocator = testing.allocator;

    // Build with dev backend to produce a native binary
    const build_result = try util.runRoc(std.testing.io, allocator, &.{ "build", "--opt=dev" }, "test/fx/issue8517.roc");
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);
    try util.checkSuccess(build_result);

    // Run the built binary
    const run_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{"./issue8517"}, .{
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Should exit with non-zero code (expect failure)
    switch (run_result.term) {
        .exited => |code| {
            if (code == 0) {
                std.debug.print("ERROR: dev backend binary exited with 0 but expect 1 == 2 should fail\n", .{});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.UnexpectedSuccess;
            }
        },
        .signal => |sig| {
            std.debug.print("ERROR: dev backend binary crashed with signal {} instead of clean expect failure\n", .{sig});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.SegFault;
        },
        else => {
            std.debug.print("ERROR: dev backend binary terminated abnormally: {}\n", .{run_result.term});
            return error.RunFailed;
        },
    }

    // Should report the failing inline expect via roc_expect_failed
    try testing.expect(std.mem.find(u8, run_result.stderr, "Expect failed") != null);
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

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"--opt=interpreter"}, "test/fx/index_oob_instantiate.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // The compiler should not panic/crash. Once the bug is fixed, this test will pass.
    // Currently it fails with a panic in instantiate.zig.
    try util.checkSuccess(run_result);
}

test "fx platform index out of bounds in instantiate regression (dev backend)" {
    // Regression test: same scenario as the interpreter variant above, but using
    // the dev (native) backend. Previously panicked with:
    //   generateCall: unexpected lookup callee after backend peeling deletion
    // because hosted functions (e.g. Stdout.line!) were not tracked in lambda
    // sets, so passing them as callbacks to higher-order functions like
    // for_each! produced unresolvable lookup callees in the code generator.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/index_oob_instantiate.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);
}

test "fx platform fold_rev static dispatch regression" {
    // Regression test: Calling fold_rev with static dispatch (method syntax) panics,
    // but calling it qualified as List.fold_rev(...) works fine.
    //
    // The panic occurs with: [1].fold_rev([], |elem, acc| acc.append(elem))
    // But this works: List.fold_rev([1], [], |elem, acc| acc.append(elem))
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/fold_rev_static_dispatch.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    // Verify the expected output
    try testing.expect(std.mem.find(u8, run_result.stdout, "Start reverse") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "Reversed: 3 elements") != null);
    try testing.expect(std.mem.find(u8, run_result.stdout, "Done") != null);
}

test "fx platform invalid nested where-clause static dispatch fails in check" {
    // Regression test for #9657: the original repro's top-level decode_i64 and
    // encode_i64 declarations are not I64.decode/I64.encode methods, so check
    // must reject the where-clause contract before post-check lowering.
    const allocator = testing.allocator;

    var env_map = try util.buildIsolatedTestEnvMap(std.testing.io, allocator, null);
    defer env_map.deinit();

    const check_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        util.roc_binary_path,
        "check",
        "--no-cache",
        "test/fx/nested_static_dispatch_where_repro.roc",
    }, .{
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(check_result.stdout);
    defer allocator.free(check_result.stderr);

    try util.checkFailure(.{
        .stdout = check_result.stdout,
        .stderr = check_result.stderr,
        .term = check_result.term,
    });
    try testing.expect(std.mem.find(u8, check_result.stderr, "MISSING METHOD") != null);
    try testing.expect(std.mem.find(u8, check_result.stderr, "postcheck invariant violated") == null);
}

test "fx platform valid nested where-clause static dispatch builds" {
    // A generic function returning a nested local function must carry enough
    // checked where-clause dispatch evidence to build when the contract is valid.
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    const output_path = try std.fs.path.join(allocator, &.{ tmp_path, "nested-static-dispatch" });
    defer allocator.free(output_path);

    const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_path});
    defer allocator.free(output_arg);

    var env_map = try util.buildIsolatedTestEnvMap(std.testing.io, allocator, null);
    defer env_map.deinit();

    const build_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        util.roc_binary_path,
        "build",
        "--opt=dev",
        "--debug",
        "--no-cache",
        output_arg,
        "test/fx/nested_static_dispatch_where_valid.roc",
    }, .{
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    try util.checkSuccess(.{
        .stdout = build_result.stdout,
        .stderr = build_result.stderr,
        .term = build_result.term,
    });
    try testing.expect(std.mem.find(u8, build_result.stderr, "postcheck invariant violated") == null);
}

test "fx platform divergent if with all crash branches does not hit postcheck invariant" {
    const allocator = testing.allocator;

    var env_map = try util.buildIsolatedTestEnvMap(std.testing.io, allocator, null);
    defer env_map.deinit();

    const build_result = try util.runChildWithTimeout(std.testing.io, allocator, &[_][]const u8{
        util.roc_binary_path,
        "build",
        "--no-cache",
        "test/fx/divergent_if_all_branches_crash_repro.roc",
    }, .{
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    const did_abort = switch (build_result.term) {
        .exited => |code| code == 134,
        .signal => true,
        else => true,
    };
    try testing.expect(!did_abort);
    try testing.expect(std.mem.find(u8, build_result.stderr, "postcheck invariant violated") == null);
    try testing.expect(std.mem.find(u8, build_result.stderr, "panic") == null);
}

test "external platform memory alignment regression" {
    // SKIPPED: aoc_day2.roc crashes at runtime due to a dev backend bug with
    // mutable variables + for loops + closures (.contains/.append).
    // See https://github.com/roc-lang/roc/issues/8946
    return error.SkipZigTest;

    // This test verifies that external platforms with the memory alignment fix work correctly.
    // The bug was in roc-platform-template-zig < 0.6 where rocDeallocFn used
    // `roc_dealloc.alignment` directly instead of `@max(roc_dealloc.alignment, @alignOf(usize))`.
    // Fixed in https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/tag/0.6
    // const allocator = testing.allocator;

    // const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/aoc_day2.roc");
    // defer allocator.free(run_result.stdout);
    // defer allocator.free(run_result.stderr);

    // try util.checkSuccess(run_result);
}

test "fx platform issue8826 app vs platform type mismatch" {
    // Regression test for https://github.com/roc-lang/roc/issues/8826
    // The bug was that `roc check` reported "No errors found" when the app's main!
    // signature didn't match the platform's requires. This happened because
    // The root semantic-data lookup used to return the wrong module (the first one added rather
    // than the actual root module set in buildRoot).
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"check"}, "test/fx/issue8826_minimal.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail with a TYPE MISMATCH error because:
    // - App has: main! : List(Str) => Try({}, [Exit(I32)])
    // - Platform requires: main! : () => {}
    switch (run_result.term) {
        .exited => |code| {
            if (code != 0) {
                // Expected to fail - check for type mismatch error message
                try testing.expect(std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null);
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

    const run_result = try util.runRoc(std.testing.io, allocator, &.{}, "test/fx/issue8826_full.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file has type errors, so it should fail with a non-zero exit code
    // The important thing is that it should NOT silently exit - it should
    // print error messages to stderr
    try util.checkFailure(run_result);

    // Verify error messages are printed (not silent exit)
    // The file has mutually recursive type aliases, type mismatches, etc.
    // On Windows, we may hit OOM due to shared memory limits, which should
    // still print an error message (just not the type error message).
    const has_type_error = std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null or
        std.mem.find(u8, run_result.stderr, "MUTUALLY RECURSIVE TYPE ALIASES") != null or
        std.mem.find(u8, run_result.stderr, "UNDECLARED TYPE") != null;
    const has_oom_error = std.mem.find(u8, run_result.stderr, "Out of memory") != null;

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
    // 1. Compile-time evaluation cleanup freed crash messages before reports were built
    // 2. addSourceCodeWithUnderlines didn't dupe the filename from SourceCodeDisplayRegion
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{"check"}, "test/fx/issue8943.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail during checking before post-check
    // compile-time evaluation can run. Regression coverage here is about error
    // report memory ownership, not preserving an obsolete later-stage crash.
    try util.checkFailure(run_result);

    // Check that the TYPE MISMATCH error is present
    const has_try_type_error = std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null;
    if (!has_try_type_error) {
        std.debug.print("Expected 'TYPE MISMATCH' error but got:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.ExpectedTryTypeError;
    }

    // The invalid top-level `?` must not escape checking and become a
    // post-check compile-time crash.
    const has_comptime_crash = std.mem.find(u8, run_result.stderr, "COMPILE TIME CRASH") != null;
    if (has_comptime_crash) {
        std.debug.print("Unexpected 'COMPTIME CRASH' after checking reported the invalid `?` expression:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.UnexpectedComptimeCrash;
    }

    // The key check: verify no memory corruption in error messages
    // Count how many times the filename appears - it should appear at least twice
    // (once in each error's source region). The bug causes the first one to be garbled.
    var filename_count: usize = 0;
    var search_start: usize = 0;
    while (std.mem.findPos(u8, run_result.stderr, search_start, "issue8943.roc")) |pos| {
        filename_count += 1;
        search_start = pos + 1;
    }

    // We expect at least 2 occurrences from source regions plus one more at
    // the end in "Found X error(s)..."
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
    const has_readable_crash_msg = std.mem.find(u8, run_result.stderr, "Try") != null;
    if (!has_readable_crash_msg) {
        std.debug.print("Crash message appears corrupted - expected 'Try' not found:\n", .{});
        std.debug.print("STDERR: {s}\n", .{run_result.stderr});
        return error.CorruptedCrashMessage;
    }
}

test "fx platform issue9118 try operator on tuple in type method (interpreter)" {
    // Regression test for https://github.com/roc-lang/roc/issues/9118
    // The bug was that using the ? operator on a tuple (instead of a Try type)
    // inside a type method would cause a segfault in the interpreter.
    // The ? operator expects a Try type [Ok(a), Err(e)] but was given a tuple.
    const allocator = testing.allocator;

    const run_result = try util.runRoc(std.testing.io, allocator, &.{ "test", "--opt=interpreter" }, "test/fx/for_var_in_type_method.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // This file is expected to fail with a TYPE MISMATCH error because
    // the ? operator is used on a tuple (Value, [Ok, Err(Str)]) instead of
    // a Try type [Ok(Value), Err(Str)].
    // The important thing is that it should NOT segfault - it should report
    // the type error gracefully.

    switch (run_result.term) {
        .exited => |code| {
            if (code == 0) {
                std.debug.print("Expected type error but test succeeded\n", .{});
                return error.UnexpectedSuccess;
            }
            // Expected to fail - check for type mismatch error message
            const has_type_error = std.mem.find(u8, run_result.stderr, "TYPE MISMATCH") != null;
            if (!has_type_error) {
                std.debug.print("Expected 'TYPE MISMATCH' error but got:\n", .{});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.ExpectedTypeError;
            }
            // Verify it mentions the ? operator and Try type
            const mentions_try = std.mem.find(u8, run_result.stderr, "Try") != null;
            if (!mentions_try) {
                std.debug.print("Expected error to mention 'Try' type but got:\n", .{});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.ExpectedTryMention;
            }
        },
        .signal => |sig| {
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

test "fx platform issue9118 try operator on tuple in type method (dev backend)" {
    // TODO: dev backend crashes with signal 6 (SIGABRT)
    return error.SkipZigTest;
}

test "default app resolves a sibling type module imported with exposing" {
    const allocator = std.testing.allocator;

    // A headerless file with `main!` runs as a "default app": its source is
    // staged into a temp dir and compiled with a synthetic default platform,
    // while sibling imports resolve against the file's original directory. Here
    // the sibling `FooBar.roc` is a type module whose associated value `square`
    // is brought into scope via `exposing` and then called.
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\import FooBar exposing [square]
        \\
        \\main! = |args| {
        \\    arg_count = List.len(args)
        \\    n = 12 + arg_count - arg_count
        \\    if square(n) == 144 {
        \\        Ok({})
        \\    } else {
        \\        Err(Exit(1))
        \\    }
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "FooBar.roc",
        .data =
        \\FooBar :: {}.{
        \\    square : U64 -> U64
        \\    square = |x| x * x
        \\}
        ,
    });

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);
    const main_path = try std.fs.path.join(allocator, &.{ tmp_path, "main.roc" });
    defer allocator.free(main_path);

    const result = try util.runRoc(std.testing.io, allocator, &.{"--opt=dev"}, main_path);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("roc exited with code {}\nSTDOUT: {s}\nSTDERR: {s}\n", .{ code, result.stdout, result.stderr });
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("roc terminated abnormally: {}\nSTDERR: {s}\n", .{ result.term, result.stderr });
            return error.RunFailed;
        },
    }

    try testing.expectEqualStrings("", result.stdout);
}
