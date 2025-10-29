//! `roc check` tests that use the actual roc CLI binary.

const std = @import("std");

const CheckResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

/// Helper to set up and run roc check on a test file
fn runRocCheck(allocator: std.mem.Allocator, test_file_path: []const u8) !CheckResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer allocator.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
        return error.SkipZigTest;
    };

    const test_file = try std.fs.path.join(allocator, &.{ cwd_path, test_file_path });
    defer allocator.free(test_file);

    // Run roc check and capture output
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ roc_path, "check", "--no-cache", test_file },
        .cwd = cwd_path,
        .max_output_bytes = 10 * 1024 * 1024, // 10MB
    });

    return CheckResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .term = result.term,
    };
}

test "roc check writes parse errors to stderr" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try runRocCheck(gpa, "test/cli/has_parse_error.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code)
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains error information (THIS IS THE KEY TEST - without flush, this will be empty)
    try testing.expect(result.stderr.len > 0);

    // 3. Stderr contains error reporting
    const has_error = std.mem.indexOf(u8, result.stderr, "Failed to check") != null or
        std.mem.indexOf(u8, result.stderr, "error") != null or
        std.mem.indexOf(u8, result.stderr, "Unsupported") != null;
    try testing.expect(has_error);
}

test "roc check succeeds on valid file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try runRocCheck(gpa, "test/cli/simple_success.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Stderr should be empty or minimal for success
    // (No errors should be reported)
    const has_error = std.mem.indexOf(u8, result.stderr, "Failed to check") != null or
        std.mem.indexOf(u8, result.stderr, "error") != null;
    try testing.expect(!has_error);
}
