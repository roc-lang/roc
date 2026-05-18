//! End-to-end tests for the echo platform (headerless app) path,
//! exercised through the actual roc CLI binary with both the
//! interpreter and the dev backend.

const std = @import("std");
const util = @import("util.zig");

fn runEchoExpectOutput(opt_args: []const []const u8, roc_file: []const u8, expected_stdout: []const u8) !void {
    const gpa = std.testing.allocator;
    const result = try util.runRoc(gpa, opt_args, roc_file);
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("Echo app failed with exit code: {}\nstdout: {s}\nstderr: {s}\n", .{
            result.term, result.stdout, result.stderr,
        });
    }
    try std.testing.expect(result.term == .Exited and result.term.Exited == 0);
    try std.testing.expectEqualStrings(expected_stdout, result.stdout);
}

fn runEchoExpectExitCode(opt_args: []const []const u8, roc_file: []const u8, expected_code: u32) !void {
    const gpa = std.testing.allocator;
    const result = try util.runRoc(gpa, opt_args, roc_file);
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);
    if (result.term != .Exited or result.term.Exited != expected_code) {
        std.debug.print("Echo app exited with code {} (expected {})\nstdout: {s}\nstderr: {s}\n", .{
            result.term, expected_code, result.stdout, result.stderr,
        });
    }
    try std.testing.expect(result.term == .Exited and result.term.Exited == expected_code);
}

test "echo platform: hello (interpreter)" {
    try runEchoExpectOutput(&.{}, "test/echo/hello.roc", "Hello, World!\n");
}
test "echo platform: hello (dev backend)" {
    try runEchoExpectOutput(&.{"--opt=dev"}, "test/echo/hello.roc", "Hello, World!\n");
}

test "echo platform: multiple echo calls (interpreter)" {
    try runEchoExpectOutput(&.{}, "test/echo/multi.roc", "Hello, \nWorld!\n");
}
test "echo platform: multiple echo calls (dev backend)" {
    try runEchoExpectOutput(&.{"--opt=dev"}, "test/echo/multi.roc", "Hello, \nWorld!\n");
}

test "echo platform: exit ok (interpreter)" {
    try runEchoExpectOutput(&.{}, "test/echo/exit_ok.roc", "success\n");
}
test "echo platform: exit ok (dev backend)" {
    try runEchoExpectOutput(&.{"--opt=dev"}, "test/echo/exit_ok.roc", "success\n");
}

test "echo platform: exit code (interpreter)" {
    try runEchoExpectExitCode(&.{}, "test/echo/exit_code.roc", 255);
}
test "echo platform: exit code (dev backend)" {
    try runEchoExpectExitCode(&.{"--opt=dev"}, "test/echo/exit_code.roc", 255);
}

test "echo platform: custom error issue 9255 repro (dev backend)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{"--opt=dev"}, "test/echo/exit_custom_error.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Expected behavior for issue #9255: the echo platform should preserve the
    // app's custom error tag when matching the open union catch-all.
    try testing.expect(result.term == .Exited and result.term.Exited == 1);
    try testing.expectEqualStrings("Program exited with error: SomeCustomError(41.0)\n", result.stdout);
}

fn runEchoExpectFailure(opt_args: []const []const u8, roc_file: []const u8) !void {
    const gpa = std.testing.allocator;
    const result = try util.runRoc(gpa, opt_args, roc_file);
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);
    try std.testing.expect(result.term == .Exited and result.term.Exited != 0);
}

test "echo platform: list concat with refcounted elements issue 9316 (interpreter)" {
    try runEchoExpectOutput(&.{}, "test/echo/issue_9316.roc", "[\"BAZ\", \"DUCK\", \"XYZ\", \"ABC\"]\n");
}
test "echo platform: list concat with refcounted elements issue 9316 (dev backend)" {
    try runEchoExpectOutput(&.{"--opt=dev"}, "test/echo/issue_9316.roc", "[\"BAZ\", \"DUCK\", \"XYZ\", \"ABC\"]\n");
}

test "echo platform: no main is not a default app (interpreter)" {
    try runEchoExpectFailure(&.{"--opt=interpreter"}, "test/echo/no_main.roc");
}
test "echo platform: no main is not a default app (dev)" {
    // TODO: dev backend crashes test runner
    return error.SkipZigTest;
}

// Lines shared between interpreter and dev backend expected output.
const all_syntax_common_prefix =
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
    "Ok(1)\n" ++
    "Err(NoFirstError(ListWasEmpty))\n" ++
    "Err(NoFirstError(ListWasEmpty))\n" ++
    "15.0\n" ++
    "False\n" ++
    "10.0\n" ++
    "42.0\n" ++
    "NotOneTwoNotFive\n" ++
    "(\"Roc\", 1.0)\n" ++
    "[\"a\", \"b\"]\n" ++
    "(\"Roc\", 1.0, 1.0, 1.0)\n" ++
    "10.0\n" ++
    "{ age: 31, name: \"Alice\" }\n";

const all_syntax_common_suffix =
    "\"The secret key is: my_secret_key\"\n" ++
    "False\n" ++
    "99\n" ++
    "\"12345.0\"\n" ++
    "\"Foo with 42 and hello\"\n" ++
    "\"other color\"\n" ++
    "\"Names: Alice, Bob, Charlie\"\n" ++
    "\"A\"\n" ++
    "\"other letter\"\n" ++
    "True\n";

const all_syntax_expected_stdout =
    all_syntax_common_prefix ++
    "{ binary: 5.0, explicit_i128: 5, explicit_i16: 5, explicit_i32: 5, explicit_i64: 5, explicit_i8: 5, explicit_u128: 5, explicit_u16: 5, explicit_u32: 5, explicit_u64: 5, explicit_u8: 5, hex: 5.0, octal: 5.0, usage_based: 5.0 }\n" ++
    "<opaque>\n" ++
    all_syntax_common_suffix;

const all_syntax_expected_stderr = "[dbg] 42.0\n";

test "echo platform: all_syntax_test.roc prints expected output (interpreter)" {
    const allocator = std.testing.allocator;

    const run_result = try util.runRoc(allocator, &.{"--opt=interpreter"}, "test/echo/all_syntax_test.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    try std.testing.expectEqualStrings(all_syntax_expected_stdout, run_result.stdout);
    try std.testing.expectEqualStrings(all_syntax_expected_stderr, run_result.stderr);
}

test "echo platform: all_syntax_test.roc prints expected output (dev backend)" {
    const allocator = std.testing.allocator;

    const run_result = try util.runRoc(allocator, &.{"--opt=dev"}, "test/echo/all_syntax_test.roc");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    try util.checkSuccess(run_result);

    try std.testing.expectEqualStrings(all_syntax_expected_stdout, run_result.stdout);
    try std.testing.expectEqualStrings(all_syntax_expected_stderr, run_result.stderr);
}

test "echo platform: roc test all_syntax_test.roc passes" {
    const allocator = std.testing.allocator;

    const result = try util.runRoc(allocator, &.{ "test", "--no-cache" }, "test/echo/all_syntax_test.roc");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try util.checkSuccess(result);

    const has_passed = std.mem.indexOf(u8, result.stdout, "passed") != null;
    try std.testing.expect(has_passed);
}
