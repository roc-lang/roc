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

test "roc check displays correct file path in parse error messages" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/has_parse_error.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code) due to parse error
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains error information
    try testing.expect(result.stderr.len > 0);

    // 3. Stderr contains the actual file path, not mangled bytes
    // The error message should include "has_parse_error.roc" in the location indicator
    const has_file_path = std.mem.indexOf(u8, result.stderr, "has_parse_error.roc") != null;
    try testing.expect(has_file_path);

    // 4. Stderr should NOT contain sequences of 0xaa bytes (indicates path encoding issue)
    // When paths are mangled, they appear as repeated 0xaa bytes in the output
    const mangled_path_pattern = [_]u8{ 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa };
    const has_mangled_path = std.mem.indexOf(u8, result.stderr, &mangled_path_pattern) != null;
    try testing.expect(!has_mangled_path);
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
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
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
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer gpa.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
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
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer gpa.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
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

test "roc check reports type error - annotation mismatch" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/has_type_error_annotation.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code) due to type error
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains type error information
    try testing.expect(result.stderr.len > 0);

    // 3. Error message mentions type mismatch or error
    const has_type_error = std.mem.indexOf(u8, result.stderr, "TYPE MISMATCH") != null or
        std.mem.indexOf(u8, result.stderr, "error") != null or
        std.mem.indexOf(u8, result.stderr, "Found") != null;
    try testing.expect(has_type_error);
}

test "roc check reports type error - plus operator with incompatible types" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/has_type_error_plus_operator.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code) due to type error
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains type error information
    try testing.expect(result.stderr.len > 0);

    // 3. Error message mentions missing method or type error
    const has_type_error = std.mem.indexOf(u8, result.stderr, "MISSING METHOD") != null or
        std.mem.indexOf(u8, result.stderr, "TYPE MISMATCH") != null or
        std.mem.indexOf(u8, result.stderr, "error") != null or
        std.mem.indexOf(u8, result.stderr, "Found") != null;
    try testing.expect(has_type_error);
}

test "roc test/int/app.roc runs successfully" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{"--no-cache"}, "test/int/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc test/str/app.roc runs successfully" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{"--no-cache"}, "test/str/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

// =============================================================================
// roc build tests
// =============================================================================

test "roc build creates executable from test/int/app.roc" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a temp directory for the output
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    const output_path = try std.fs.path.join(gpa, &.{ tmp_path, "test_app" });
    defer gpa.free(output_path);

    const output_arg = try std.fmt.allocPrint(gpa, "--output={s}", .{output_path});
    defer gpa.free(output_arg);

    const result = try util.runRoc(gpa, &.{ "build", output_arg }, "test/int/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Output file was created
    const stat = tmp_dir.dir.statFile("test_app") catch |err| {
        std.debug.print("Failed to stat output file: {}\nstderr: {s}\n", .{ err, result.stderr });
        return err;
    };

    // 3. Output file is executable (non-zero size)
    try testing.expect(stat.size > 0);
}

test "roc build executable runs correctly" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a temp directory for the output
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    const output_path = try std.fs.path.join(gpa, &.{ tmp_path, "test_app" });
    defer gpa.free(output_path);

    const output_arg = try std.fmt.allocPrint(gpa, "--output={s}", .{output_path});
    defer gpa.free(output_arg);

    // Build the app
    const build_result = try util.runRoc(gpa, &.{ "build", output_arg }, "test/int/app.roc");
    defer gpa.free(build_result.stdout);
    defer gpa.free(build_result.stderr);

    try testing.expect(build_result.term == .Exited and build_result.term.Exited == 0);

    // Run the built executable
    const run_result = try std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{output_path},
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer gpa.free(run_result.stdout);
    defer gpa.free(run_result.stderr);

    // Verify that:
    // 1. Executable ran successfully
    try testing.expect(run_result.term == .Exited and run_result.term.Exited == 0);

    // 2. Output contains expected success message
    const has_success = std.mem.indexOf(u8, run_result.stdout, "SUCCESS") != null or
        std.mem.indexOf(u8, run_result.stdout, "PASSED") != null;
    try testing.expect(has_success);
}

test "roc build fails with file not found error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{"build"}, "nonexistent_file.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code)
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains file not found error
    const has_error = std.mem.indexOf(u8, result.stderr, "FileNotFound") != null or
        std.mem.indexOf(u8, result.stderr, "not found") != null or
        std.mem.indexOf(u8, result.stderr, "Failed") != null;
    try testing.expect(has_error);
}

test "roc build fails with invalid target error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "build", "--target=invalid_target_name" }, "test/int/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code)
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains invalid target error
    const has_error = std.mem.indexOf(u8, result.stderr, "Invalid target") != null or
        std.mem.indexOf(u8, result.stderr, "invalid") != null;
    try testing.expect(has_error);
}
