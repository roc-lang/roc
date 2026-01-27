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

test "roc repl shows welcome banner" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Send empty input (just EOF) to exit the REPL
    const result = try util.runRocWithStdin(gpa, &.{"repl"}, "");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Command exits successfully (EOF closes REPL gracefully)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Stdout contains the welcome banner
    const has_welcome = std.mem.indexOf(u8, result.stdout, "Roc REPL") != null;
    try testing.expect(has_welcome);

    // Stdout mentions help
    const has_help_hint = std.mem.indexOf(u8, result.stdout, ":help") != null;
    try testing.expect(has_help_hint);
}

test "roc repl evaluates simple expression" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Evaluate a simple expression
    const result = try util.runRocWithStdin(gpa, &.{"repl"}, "1 + 1\n");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Command exits successfully
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Output contains the result "2"
    const has_result = std.mem.indexOf(u8, result.stdout, "2") != null;
    try testing.expect(has_result);
}

test "roc repl :help command works" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Send :help command
    const result = try util.runRocWithStdin(gpa, &.{"repl"}, ":help\n");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Command exits successfully
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Output contains help text (mentions commands)
    const has_help_output = std.mem.indexOf(u8, result.stdout, ":exit") != null or
        std.mem.indexOf(u8, result.stdout, ":quit") != null;
    try testing.expect(has_help_output);
}

test "roc repl :exit command exits cleanly" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Send :exit command
    const result = try util.runRocWithStdin(gpa, &.{"repl"}, ":exit\n");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Command exits successfully
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Output contains goodbye message
    const has_goodbye = std.mem.indexOf(u8, result.stdout, "Goodbye") != null;
    try testing.expect(has_goodbye);
}

test "roc repl variable definition and usage" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Define a variable and use it
    const result = try util.runRocWithStdin(gpa, &.{"repl"}, "x = 5\nx + 3\n");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Command exits successfully
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Output contains the result "8"
    const has_result = std.mem.indexOf(u8, result.stdout, "8") != null;
    try testing.expect(has_result);
}

test "roc repl string expression" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Evaluate a string expression
    const result = try util.runRocWithStdin(gpa, &.{"repl"}, "\"hello\"\n");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Command exits successfully
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Output contains the string (with quotes in output)
    const has_string = std.mem.indexOf(u8, result.stdout, "hello") != null;
    try testing.expect(has_string);
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

test "roc check test/int/app.roc does not panic" {
    // Skip on Windows - test/int platform doesn't have Windows host libraries
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/int/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that roc check does not panic on test/int/app.roc.
    // Prior to fix for issue #8947, this would panic with:
    // "trying unifyWith unexpected ranks 1 & 0"
    // Now it should fail gracefully (exit code 1) with type errors, not panic (abort).

    // 1. Should not abort (panic would cause exit code 134 on macOS/Linux)
    const did_panic = result.term == .Signal or (result.term == .Exited and result.term.Exited == 134);
    try testing.expect(!did_panic);

    // 2. Should not contain "panic" in output
    const has_panic_text = std.mem.indexOf(u8, result.stderr, "panic") != null;
    try testing.expect(!has_panic_text);
}

test "roc test/int/app.roc runs successfully" {
    // Skip on Windows - test/int platform doesn't have Windows host libraries
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

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
    // Skip on Windows - test/str platform doesn't have Windows host libraries
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{"--no-cache"}, "test/str/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

// roc build tests

test "roc build creates executable from test/int/app.roc" {
    // Skip on Windows - test/int platform doesn't have Windows host libraries
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

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
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("roc build failed with exit code: {}\nstdout: {s}\nstderr: {s}\n", .{ result.term, result.stdout, result.stderr });
    }
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Output file was created
    const stat = tmp_dir.dir.statFile("test_app") catch |err| {
        std.debug.print("Failed to stat output file: {}\nstderr: {s}\n", .{ err, result.stderr });
        return err;
    };

    // 3. Output file is executable (non-zero size)
    try testing.expect(stat.size > 0);

    // 4. Stdout contains success message
    try testing.expect(result.stdout.len > 5);
    try testing.expect(std.mem.indexOf(u8, result.stdout, "Successfully built") != null);
}

test "roc build executable runs correctly" {
    // Skip on Windows - test/int platform doesn't have Windows host libraries
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

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

    if (build_result.term != .Exited or build_result.term.Exited != 0) {
        std.debug.print("roc build failed with exit code: {}\nstdout: {s}\nstderr: {s}\n", .{ build_result.term, build_result.stdout, build_result.stderr });
    }
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
        std.mem.indexOf(u8, result.stderr, "NOT FOUND") != null or
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

test "roc build glibc target gives helpful error on non-Linux" {
    const testing = std.testing;
    const builtin = @import("builtin");
    const gpa = testing.allocator;

    // This test only applies on non-Linux platforms
    if (builtin.os.tag == .linux) {
        return; // Skip on Linux where glibc cross-compilation is supported
    }

    const result = try util.runRoc(gpa, &.{ "build", "--target=x64glibc" }, "test/int/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed (non-zero exit code)
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 2. Stderr contains helpful error message about glibc not being supported
    const has_glibc_error = std.mem.indexOf(u8, result.stderr, "glibc") != null;
    try testing.expect(has_glibc_error);

    // 3. Stderr suggests using musl instead
    const suggests_musl = std.mem.indexOf(u8, result.stderr, "musl") != null;
    try testing.expect(suggests_musl);
}

test "roc test with nested list chunks does not panic on layout upgrade" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // This test verifies that nested list operations with layout upgrades
    // (from list_of_zst to concrete list types) don't cause integer overflow panics.
    // The expect in the test file is designed to fail, but execution should not panic.
    const result = try util.runRoc(gpa, &.{"test"}, "test/cli/issue8699.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command failed with exit code 1 (test failure, not panic)
    try testing.expect(result.term == .Exited and result.term.Exited == 1);

    // 2. Stderr contains "FAIL" indicating a test failure (not a panic/crash)
    const has_fail = std.mem.indexOf(u8, result.stderr, "FAIL") != null;
    try testing.expect(has_fail);

    // 3. Stderr should not contain "panic" or "overflow" (no crash occurred)
    const has_panic = std.mem.indexOf(u8, result.stderr, "panic") != null or
        std.mem.indexOf(u8, result.stderr, "overflow") != null;
    try testing.expect(!has_panic);
}

// Exit code tests for warnings

test "roc check returns exit code 2 for warnings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/fx/run_warning_only.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command exits with code 2 (warnings present, no errors)
    try testing.expect(result.term == .Exited and result.term.Exited == 2);

    // 2. Stderr contains warning information
    const has_warning = std.mem.indexOf(u8, result.stderr, "UNUSED VARIABLE") != null or
        std.mem.indexOf(u8, result.stderr, "warning") != null;
    try testing.expect(has_warning);

    // 3. Output shows 0 errors and at least 1 warning
    const has_zero_errors = std.mem.indexOf(u8, result.stderr, "0 error") != null;
    try testing.expect(has_zero_errors);
}

test "roc check returns exit code 0 for no warnings or errors" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/simple_success.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that command exits with code 0 (no warnings, no errors)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc check returns exit code 1 for errors" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/has_type_error_annotation.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that command exits with code 1 (errors present)
    try testing.expect(result.term == .Exited and result.term.Exited == 1);
}

test "roc run returns exit code 2 for warnings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{"--no-cache"}, "test/fx/run_warning_only.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command exits with code 2 (warnings present, no errors)
    try testing.expect(result.term == .Exited and result.term.Exited == 2);

    // 2. Stderr contains warning information
    const has_warning = std.mem.indexOf(u8, result.stderr, "UNUSED VARIABLE") != null or
        std.mem.indexOf(u8, result.stderr, "warning") != null;
    try testing.expect(has_warning);
}

test "roc build returns exit code 2 for warnings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a temp directory for the output
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    const output_path = try std.fs.path.join(gpa, &.{ tmp_path, "test_app_warning" });
    defer gpa.free(output_path);

    const output_arg = try std.fmt.allocPrint(gpa, "--output={s}", .{output_path});
    defer gpa.free(output_arg);

    const result = try util.runRoc(gpa, &.{ "build", output_arg }, "test/fx/run_warning_only.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command exits with code 2 (warnings present, no errors)
    try testing.expect(result.term == .Exited and result.term.Exited == 2);

    // 2. Stderr contains warning information
    const has_warning = std.mem.indexOf(u8, result.stderr, "UNUSED VARIABLE") != null or
        std.mem.indexOf(u8, result.stderr, "warning") != null;
    try testing.expect(has_warning);

    // 3. Binary was still created successfully
    const stat = tmp_dir.dir.statFile("test_app_warning") catch |err| {
        std.debug.print("Failed to stat output file: {}\nstderr: {s}\n", .{ err, result.stderr });
        return err;
    };
    try testing.expect(stat.size > 0);

    // 4. Success message was printed
    try testing.expect(std.mem.indexOf(u8, result.stdout, "Successfully built") != null);
}

// Tests for --jobs flag
test "roc check with -j1 succeeds on valid file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache", "-j1" }, "test/cli/simple_success.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that command succeeded
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc check with --jobs=1 succeeds on valid file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache", "--jobs=1" }, "test/cli/simple_success.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that command succeeded
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc check with --jobs=2 succeeds on valid file" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache", "--jobs=2" }, "test/cli/simple_success.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that command succeeded
    try testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc check with invalid --jobs value returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--jobs=abc" }, "test/cli/simple_success.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that command failed with error
    try testing.expect(result.term == .Exited and result.term.Exited == 1);

    // Verify error message mentions invalid value
    const has_error = std.mem.indexOf(u8, result.stderr, "not a valid value") != null;
    try testing.expect(has_error);
}

test "roc check does not panic on invalid package shorthand import (issue 9084)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // This test verifies that importing from a non-existent package shorthand
    // (e.g., "import f.S" where "f" is not defined) produces an error message
    // instead of causing the coordinator to panic with "Coordinator stuck in infinite loop".
    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/cli/invalid_package_shorthand.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command did not abort/panic (exit code 134 on macOS/Linux indicates SIGABRT)
    const did_panic = result.term == .Signal or (result.term == .Exited and result.term.Exited == 134);
    try testing.expect(!did_panic);

    // 2. Stderr should not contain "panic" or "Coordinator stuck"
    const has_panic_text = std.mem.indexOf(u8, result.stderr, "panic") != null or
        std.mem.indexOf(u8, result.stderr, "Coordinator stuck") != null;
    try testing.expect(!has_panic_text);

    // 3. Command should fail with a non-zero exit code (error, not success)
    try testing.expect(result.term != .Exited or result.term.Exited != 0);

    // 4. Stderr should contain some error information
    try testing.expect(result.stderr.len > 0);
}
