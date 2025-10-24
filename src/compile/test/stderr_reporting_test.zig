//! Test that verifies error reports are properly written to stderr
//! This prevents regressions where stderr buffering causes errors to be lost

const std = @import("std");

test "roc check writes parse errors to stderr" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(gpa, ".");
    defer gpa.free(cwd_path);
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer gpa.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
        return error.SkipZigTest;
    };

    // Use a test file that intentionally has a parse error
    const test_file = try std.fs.path.join(gpa, &.{ cwd_path, "test", "compile", "has_parse_error.roc" });
    defer gpa.free(test_file);

    // Run roc check and capture stderr
    const result = try std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{ roc_path, "check", "--no-cache", test_file },
        .cwd = cwd_path,
        .max_output_bytes = 10 * 1024 * 1024, // 10MB
    });
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
