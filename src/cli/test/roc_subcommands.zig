//! End-to-end integration tests for roc subcommands using the actual roc CLI binary.

const std = @import("std");
const util = @import("util.zig");

test "roc check writes parse errors to stderr" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/has_parse_error.roc");
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

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/simple_success.roc");
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

test "roc version outputs at least 5 chars to stdout" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRocCommand(gpa, &.{"version"});
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Stdout contains at least 5 characters
    try testing.expect(result.stdout.len >= 5);
}

// Once repl is implemented, this test should be updated to check for the expected output.
test "roc repl outputs at least 5 chars to stderr" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRocCommand(gpa, &.{"repl"});
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Output (stderr) contains at least 5 characters
    try testing.expect(result.stderr.len >= 5);
}

test "roc help contains Usage:" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRocCommand(gpa, &.{"help"});
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Stdout contains "Usage:"
    const has_usage = std.mem.indexOf(u8, result.stdout, "Usage:") != null;
    try testing.expect(has_usage);
}

test "roc licenses contains =====" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRocCommand(gpa, &.{"licenses"});
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Stdout contains "====="
    const has_usage = std.mem.indexOf(u8, result.stdout, "=====") != null;
    try testing.expect(has_usage);
}

test "roc fmt --check fails on unformatted file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "fmt", "--check" }, "test/cli/needs_formatting.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code) because file needs formatting
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr or stdout contains formatting-related message
    const has_format_msg = std.mem.indexOf(u8, result.stderr, "needs_formatting.roc") != null or
        std.mem.indexOf(u8, result.stdout, "needs_formatting.roc") != null or
        std.mem.indexOf(u8, result.stderr, "formatted") != null or
        std.mem.indexOf(u8, result.stdout, "formatted") != null;
    try testing.expect(has_format_msg);
}

test "roc fmt --check succeeds on well-formatted file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "fmt", "--check" }, "test/cli/well_formatted.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code) because file is well-formatted
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc fmt reformats file in place" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a temporary copy of the unformatted file
    var tmp_dir = testing.tmpDir(.{});
    var tmp = tmp_dir.dir;
    defer tmp_dir.cleanup();

    // Read the source file
    const cwd = std.fs.cwd();
    const source_content = try cwd.readFileAlloc(gpa, "test/cli/needs_formatting.roc", 10 * 1024);
    defer gpa.free(source_content);
    const original_size = source_content.len;

    // Write to temp file
    try tmp.writeFile(.{ .sub_path = "temp_format.roc", .data = source_content });

    // Get absolute path to temp file
    const tmp_path = try tmp.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);
    const temp_file_path = try std.fs.path.join(gpa, &.{ tmp_path, "temp_format.roc" });
    defer gpa.free(temp_file_path);

    // Get absolute path to roc binary
    const cwd_path = try cwd.realpathAlloc(gpa, ".");
    defer gpa.free(cwd_path);
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer gpa.free(roc_path);

    // Run roc fmt on the temp file
    const result = try std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{ roc_path, "fmt", temp_file_path },
        .cwd = cwd_path,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Read the formatted file
    const formatted_content = try tmp.readFileAlloc(gpa, "temp_format.roc", 10 * 1024);
    defer gpa.free(formatted_content);
    const formatted_size = formatted_content.len;

    // Verify that:
    // 1. The file size changed (formatting occurred)
    try testing.expect(formatted_size != original_size);

    // 2. The formatted file is not empty
    try testing.expect(formatted_size > 0);
}

test "roc fmt does not change well-formatted file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Read the well-formatted file before formatting
    const cwd = std.fs.cwd();
    const before_content = try cwd.readFileAlloc(gpa, "test/cli/well_formatted.roc", 10 * 1024);
    defer gpa.free(before_content);

    // Run roc fmt on the well-formatted file
    const result = try util.runRoc(gpa, &.{"fmt"}, "test/cli/well_formatted.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Read the file after formatting
    const after_content = try cwd.readFileAlloc(gpa, "test/cli/well_formatted.roc", 10 * 1024);
    defer gpa.free(after_content);

    // Verify that the content is identical (file was not modified)
    try testing.expectEqualStrings(before_content, after_content);
}

test "roc fmt --stdin formats unformatted input" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Read the unformatted file to use as stdin
    const cwd = std.fs.cwd();
    const input_content = try cwd.readFileAlloc(gpa, "test/cli/needs_formatting.roc", 10 * 1024);
    defer gpa.free(input_content);

    // Get absolute path to roc binary
    const cwd_path = try cwd.realpathAlloc(gpa, ".");
    defer gpa.free(cwd_path);
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer gpa.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
        return error.SkipZigTest;
    };

    // Run roc fmt --stdin with input piped in
    var child = std.process.Child.init(&.{ roc_path, "fmt", "--stdin" }, gpa);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.cwd = cwd_path;

    try child.spawn();

    // Write input to stdin and close it
    try child.stdin.?.writeAll(input_content);
    child.stdin.?.close();
    child.stdin = null;

    // Collect output before waiting
    const stdout = try child.stdout.?.readToEndAlloc(gpa, 10 * 1024 * 1024);
    defer gpa.free(stdout);
    const stderr = try child.stderr.?.readToEndAlloc(gpa, 10 * 1024 * 1024);
    defer gpa.free(stderr);

    // Wait for completion
    const result = try child.wait();

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result == .Exited and result.Exited == 0);

    // 2. Stdout contains formatted output (different from input)
    try testing.expect(!std.mem.eql(u8, stdout, input_content));

    // 3. Output is not empty
    try testing.expect(stdout.len > 0);
}

test "roc fmt --stdin does not change well-formatted input" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Read the well-formatted file to use as stdin
    const cwd = std.fs.cwd();
    const input_content = try cwd.readFileAlloc(gpa, "test/cli/well_formatted.roc", 10 * 1024);
    defer gpa.free(input_content);

    // Get absolute path to roc binary
    const cwd_path = try cwd.realpathAlloc(gpa, ".");
    defer gpa.free(cwd_path);
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer gpa.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
        return error.SkipZigTest;
    };

    // Run roc fmt --stdin with input piped in
    var child = std.process.Child.init(&.{ roc_path, "fmt", "--stdin" }, gpa);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.cwd = cwd_path;

    try child.spawn();

    // Write input to stdin and close it
    try child.stdin.?.writeAll(input_content);
    child.stdin.?.close();
    child.stdin = null;

    // Collect output before waiting
    const stdout = try child.stdout.?.readToEndAlloc(gpa, 10 * 1024 * 1024);
    defer gpa.free(stdout);
    const stderr = try child.stderr.?.readToEndAlloc(gpa, 10 * 1024 * 1024);
    defer gpa.free(stderr);

    // Wait for completion
    const result = try child.wait();

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result == .Exited and result.Exited == 0);

    // 2. Stdout contains the same content as input (no changes)
    try testing.expectEqualStrings(input_content, stdout);
}
