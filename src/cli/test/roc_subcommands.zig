//! End-to-end integration tests for roc subcommands using the actual roc CLI binary.

const std = @import("std");
const util = @import("util.zig");

fn createPerTestCacheEnv(allocator: std.mem.Allocator) !std.process.EnvMap {
    return util.buildIsolatedTestEnvMap(allocator, null);
}

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

fn testRocRunsSuccessfully(opt: []const u8, roc_file: []const u8) !void {
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    const result = try util.runRoc(gpa, &.{ opt, "--no-cache" }, roc_file);
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);
    try std.testing.expect(result.term == .Exited and result.term.Exited == 0);
}

test "roc test/int/app.roc runs successfully (interpreter)" {
    try testRocRunsSuccessfully("--opt=interpreter", "test/int/app.roc");
}

test "roc test/int/app.roc runs successfully (dev)" {
    // TODO: dev backend compilation fails for test/int/app.roc
    return error.SkipZigTest;
}

test "roc test/str/app.roc runs successfully (interpreter)" {
    try testRocRunsSuccessfully("--opt=interpreter", "test/str/app.roc");
}

test "roc test/str/app.roc runs successfully (dev)" {
    try testRocRunsSuccessfully("--opt=dev", "test/str/app.roc");
}

// roc build tests

test "roc build creates executable from test/int/app.roc (interpreter)" {
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

    const result = try util.runRoc(gpa, &.{ "build", "--opt=interpreter", output_arg }, "test/int/app.roc");
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

test "roc build creates executable from test/int/app.roc (dev)" {
    // TODO: dev backend compilation fails for test/int/app.roc
    return error.SkipZigTest;
}

test "roc build executable runs correctly (interpreter)" {
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
    const build_result = try util.runRoc(gpa, &.{ "build", "--opt=interpreter", output_arg }, "test/int/app.roc");
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

test "roc build --opt=dev executable runs correctly for test/int/app.roc" {
    // Skip on Windows - test/int platform doesn't have Windows host libraries
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

    const testing = std.testing;
    const gpa = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    const output_path = try std.fs.path.join(gpa, &.{ tmp_path, "test_app_dev" });
    defer gpa.free(output_path);

    const cache_path = try std.fs.path.join(gpa, &.{ tmp_path, "xdg-cache" });
    defer gpa.free(cache_path);
    try tmp_dir.dir.makePath("xdg-cache");

    const output_arg = try std.fmt.allocPrint(gpa, "--output={s}", .{output_path});
    defer gpa.free(output_arg);

    var env_map = try std.process.getEnvMap(gpa);
    defer env_map.deinit();
    try env_map.put("ROC_CACHE_DIR", cache_path);

    const build_result = try util.runRocWithEnv(
        gpa,
        &.{ "build", "--opt=dev", "--no-cache", output_arg },
        "test/int/app.roc",
        &env_map,
    );
    defer gpa.free(build_result.stdout);
    defer gpa.free(build_result.stderr);

    if (build_result.term != .Exited or build_result.term.Exited != 0) {
        std.debug.print("roc build --opt=dev failed with exit code: {}\nstdout: {s}\nstderr: {s}\n", .{
            build_result.term,
            build_result.stdout,
            build_result.stderr,
        });
    }
    try testing.expect(build_result.term == .Exited and build_result.term.Exited == 0);

    const stat = tmp_dir.dir.statFile("test_app_dev") catch |err| {
        std.debug.print("Failed to stat dev backend output file: {}\nstderr: {s}\n", .{ err, build_result.stderr });
        return err;
    };
    try testing.expect(stat.size > 0);

    const run_result = try std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{output_path},
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer gpa.free(run_result.stdout);
    defer gpa.free(run_result.stderr);

    try testing.expect(run_result.term == .Exited and run_result.term.Exited == 0);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "ALL TESTS PASSED") != null);
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

fn testCachesPassingResults(opt: []const u8) !void {
    const gpa = std.testing.allocator;
    var env_map = try createPerTestCacheEnv(gpa);
    defer env_map.deinit();

    const result1 = try util.runRocWithEnv(gpa, &.{ "test", opt }, "test/cli/AllPassTests.roc", &env_map);
    defer gpa.free(result1.stdout);
    defer gpa.free(result1.stderr);
    try std.testing.expect(result1.term == .Exited and result1.term.Exited == 0);

    const result2 = try util.runRocWithEnv(gpa, &.{ "test", opt }, "test/cli/AllPassTests.roc", &env_map);
    defer gpa.free(result2.stdout);
    defer gpa.free(result2.stderr);
    try std.testing.expect(result2.term == .Exited and result2.term.Exited == 0);
    try std.testing.expect(std.mem.indexOf(u8, result2.stdout, "(cached)") != null);
}

test "roc test caches passing results (interpreter)" {
    try testCachesPassingResults("--opt=interpreter");
}
test "roc test caches passing results (dev)" {
    // TODO: dev backend compilation fails for test/cli/AllPassTests.roc
    return error.SkipZigTest;
}

fn testCachesFailingResults(opt: []const u8) !void {
    const gpa = std.testing.allocator;
    var env_map = try createPerTestCacheEnv(gpa);
    defer env_map.deinit();

    const result1 = try util.runRocWithEnv(gpa, &.{ "test", opt }, "test/cli/SomeFailTests.roc", &env_map);
    defer gpa.free(result1.stdout);
    defer gpa.free(result1.stderr);
    try std.testing.expect(result1.term == .Exited and result1.term.Exited == 1);

    const result2 = try util.runRocWithEnv(gpa, &.{ "test", opt }, "test/cli/SomeFailTests.roc", &env_map);
    defer gpa.free(result2.stdout);
    defer gpa.free(result2.stderr);
    try std.testing.expect(result2.term == .Exited and result2.term.Exited == 1);
    try std.testing.expect(std.mem.indexOf(u8, result2.stderr, "(cached)") != null);
}

test "roc test caches failing results (interpreter)" {
    try testCachesFailingResults("--opt=interpreter");
}
test "roc test caches failing results (dev)" {
    // TODO: dev backend compilation fails for test/cli/SomeFailTests.roc
    return error.SkipZigTest;
}

test "roc test cache invalidated by source change (interpreter)" {
    const testing = std.testing;
    const gpa = testing.allocator;
    var env_map = try createPerTestCacheEnv(gpa);
    defer env_map.deinit();

    // Create a temporary copy of the test file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const cwd = std.fs.cwd();

    // Write a type module to temp dir (type name must match filename)
    const source_content = "CacheTest := {}\nadd = |a, b| a + b\nexpect { add(1, 2) == 3 }\n";
    try tmp_dir.dir.writeFile(.{ .sub_path = "CacheTest.roc", .data = source_content });

    const tmp_path = try tmp_dir.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);
    const temp_file_path = try std.fs.path.join(gpa, &.{ tmp_path, "CacheTest.roc" });
    defer gpa.free(temp_file_path);

    // Get absolute path to roc binary
    const cwd_path = try cwd.realpathAlloc(gpa, ".");
    defer gpa.free(cwd_path);
    const roc_binary_name = if (@import("builtin").os.tag == .windows) "roc.exe" else "roc";
    const roc_path = try std.fs.path.join(gpa, &.{ cwd_path, "zig-out", "bin", roc_binary_name });
    defer gpa.free(roc_path);

    // First run - populates cache
    const result1 = try std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{ roc_path, "test", "--opt=interpreter", temp_file_path },
        .cwd = cwd_path,
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer gpa.free(result1.stdout);
    defer gpa.free(result1.stderr);

    try testing.expect(result1.term == .Exited and result1.term.Exited == 0);

    // Modify the source (change the expect body)
    const modified_content = "CacheTest := {}\nadd = |a, b| a + b\nexpect { add(2, 3) == 5 }\n";
    try tmp_dir.dir.writeFile(.{ .sub_path = "CacheTest.roc", .data = modified_content });

    // Second run - should NOT be cached (source changed)
    const result2 = try std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{ roc_path, "test", "--opt=interpreter", temp_file_path },
        .cwd = cwd_path,
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer gpa.free(result2.stdout);
    defer gpa.free(result2.stderr);

    try testing.expect(result2.term == .Exited and result2.term.Exited == 0);

    // Second run should NOT contain "(cached)" since source changed
    try testing.expect(std.mem.indexOf(u8, result2.stdout, "(cached)") == null);
}

test "roc test cache invalidated by source change (dev)" {
    // TODO: dev backend compilation fails for CacheTest.roc
    return error.SkipZigTest;
}

fn testVerboseWorksFromCache(opt: []const u8) !void {
    const gpa = std.testing.allocator;
    var env_map = try createPerTestCacheEnv(gpa);
    defer env_map.deinit();

    const result1 = try util.runRocWithEnv(gpa, &.{ "test", opt }, "test/cli/AllPassTests.roc", &env_map);
    defer gpa.free(result1.stdout);
    defer gpa.free(result1.stderr);
    try std.testing.expect(result1.term == .Exited and result1.term.Exited == 0);

    const result2 = try util.runRocWithEnv(gpa, &.{ "test", opt, "--verbose" }, "test/cli/AllPassTests.roc", &env_map);
    defer gpa.free(result2.stdout);
    defer gpa.free(result2.stderr);
    try std.testing.expect(result2.term == .Exited and result2.term.Exited == 0);
    try std.testing.expect(std.mem.indexOf(u8, result2.stdout, "(cached)") != null);
    try std.testing.expect(std.mem.indexOf(u8, result2.stdout, "PASS") != null);
}

test "roc test --verbose works from cache (interpreter)" {
    try testVerboseWorksFromCache("--opt=interpreter");
}
test "roc test --verbose works from cache (dev)" {
    try testVerboseWorksFromCache("--opt=dev");
}

fn testVerboseCachesFailureReports(opt: []const u8) !void {
    const gpa = std.testing.allocator;
    var env_map = try createPerTestCacheEnv(gpa);
    defer env_map.deinit();

    const result1 = try util.runRocWithEnv(gpa, &.{ "test", opt, "--verbose" }, "test/cli/SomeFailTests.roc", &env_map);
    defer gpa.free(result1.stdout);
    defer gpa.free(result1.stderr);
    try std.testing.expect(result1.term == .Exited and result1.term.Exited == 1);

    const result2 = try util.runRocWithEnv(gpa, &.{ "test", opt, "--verbose" }, "test/cli/SomeFailTests.roc", &env_map);
    defer gpa.free(result2.stdout);
    defer gpa.free(result2.stderr);
    try std.testing.expect(result2.term == .Exited and result2.term.Exited == 1);
    try std.testing.expect(std.mem.indexOf(u8, result2.stderr, "(cached)") != null);
    try std.testing.expect(std.mem.indexOf(u8, result1.stderr, "FAIL") != null);
    try std.testing.expect(std.mem.indexOf(u8, result2.stderr, "FAIL") != null);
}

test "roc test --verbose caches failure reports (interpreter)" {
    try testVerboseCachesFailureReports("--opt=interpreter");
}
test "roc test --verbose caches failure reports (dev)" {
    // TODO: dev backend compilation fails for test/cli/SomeFailTests.roc
    return error.SkipZigTest;
}

fn testNonVerboseCachesVerboseReports(opt: []const u8) !void {
    const gpa = std.testing.allocator;
    var env_map = try createPerTestCacheEnv(gpa);
    defer env_map.deinit();

    const result1 = try util.runRocWithEnv(gpa, &.{ "test", opt }, "test/cli/SomeFailTests.roc", &env_map);
    defer gpa.free(result1.stdout);
    defer gpa.free(result1.stderr);
    try std.testing.expect(result1.term == .Exited and result1.term.Exited == 1);
    try std.testing.expect(std.mem.indexOf(u8, result1.stderr, "expect failed") == null);

    const result2 = try util.runRocWithEnv(gpa, &.{ "test", opt, "--verbose" }, "test/cli/SomeFailTests.roc", &env_map);
    defer gpa.free(result2.stdout);
    defer gpa.free(result2.stderr);
    try std.testing.expect(result2.term == .Exited and result2.term.Exited == 1);
    try std.testing.expect(std.mem.indexOf(u8, result2.stderr, "(cached)") != null);
    try std.testing.expect(std.mem.indexOf(u8, result2.stderr, "expect") != null);
    try std.testing.expect(std.mem.indexOf(u8, result2.stderr, "TEST FAILURE") != null);
}

test "roc test non-verbose run caches verbose failure reports for later verbose run (interpreter)" {
    try testNonVerboseCachesVerboseReports("--opt=interpreter");
}
test "roc test non-verbose run caches verbose failure reports for later verbose run (dev)" {
    // TODO: dev backend compilation fails for test/cli/SomeFailTests.roc
    return error.SkipZigTest;
}

test "roc test with nested list chunks does not panic on layout upgrade (interpreter)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // This test verifies that nested list operations with layout upgrades
    // (from list_of_zst to concrete list types) don't cause integer overflow panics.
    // The expect in the test file is designed to fail, but execution should not panic.
    const result = try util.runRoc(gpa, &.{ "test", "--opt=interpreter" }, "test/cli/issue8699.roc");
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

test "roc test with nested list chunks does not panic on layout upgrade (dev)" {
    // TODO: dev backend compilation fails for test/cli/issue8699.roc
    return error.SkipZigTest;
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

    // Print diagnostic info on failure
    if (!(result.term == .Exited and result.term.Exited == 0)) {
        std.debug.print("\n=== Test Failure Diagnostics ===\n", .{});
        std.debug.print("Expected: exit code 0\n", .{});
        switch (result.term) {
            .Exited => |code| std.debug.print("Actual: exit code {}\n", .{code}),
            .Signal => |sig| std.debug.print("Actual: killed by signal {}\n", .{sig}),
            else => std.debug.print("Actual: {}\n", .{result.term}),
        }
        std.debug.print("stdout: {s}\n", .{result.stdout});
        std.debug.print("stderr: {s}\n", .{result.stderr});
        std.debug.print("================================\n", .{});
    }

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

test "roc run returns exit code 2 for warnings (interpreter)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "--opt=interpreter", "--no-cache" }, "test/fx/run_warning_only.roc");
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

test "roc run --opt=dev returns exit code 2 for warnings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "--opt=dev", "--no-cache" }, "test/fx/run_warning_only.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    try testing.expect(result.term == .Exited and result.term.Exited == 2);

    const has_warning = std.mem.indexOf(u8, result.stderr, "UNUSED VARIABLE") != null or
        std.mem.indexOf(u8, result.stderr, "warning") != null;
    try testing.expect(has_warning);
}

test "roc run --opt=dev rejects non executable targets" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "--opt=dev", "--target=wasm32" }, "test/wasm/app.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    try testing.expect(result.term == .Exited and result.term.Exited != 0);

    const has_expected_error = std.mem.indexOf(u8, result.stderr, "only produces static libraries") != null or
        std.mem.indexOf(u8, result.stderr, "TARGET NOT SUPPORTED") != null or
        std.mem.indexOf(u8, result.stderr, "unsupported target") != null;
    try testing.expect(has_expected_error);
}

test "roc build returns exit code 2 for warnings (interpreter)" {
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

    const result = try util.runRoc(gpa, &.{ "build", "--opt=interpreter", output_arg }, "test/fx/run_warning_only.roc");
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

test "roc build returns exit code 2 for warnings (dev)" {
    // TODO: dev backend compilation fails for test/fx/run_warning_only.roc
    return error.SkipZigTest;
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

test "roc check succeeds on Parser type module" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "check", "--no-cache" }, "test/package_simple_parser/Parser.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. No errors should be reported
    const has_error = std.mem.indexOf(u8, result.stderr, "error") != null;
    try testing.expect(!has_error);
}

test "roc test runs expects in Parser type module (interpreter)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const result = try util.runRoc(gpa, &.{ "test", "--opt=interpreter", "--no-cache" }, "test/package_simple_parser/Parser.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Verify that:
    // 1. Command succeeded (zero exit code)
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // 2. Output indicates tests passed
    const has_passed = std.mem.indexOf(u8, result.stdout, "passed") != null;
    try testing.expect(has_passed);

    // 3. Should have run 2 tests (extract count from "(N)" in output)
    const count = blk: {
        const open = std.mem.indexOf(u8, result.stdout, "(") orelse break :blk @as(usize, 0);
        const close = std.mem.indexOfPos(u8, result.stdout, open, ")") orelse break :blk @as(usize, 0);
        break :blk std.fmt.parseInt(usize, result.stdout[open + 1 .. close], 10) catch 0;
    };
    try testing.expect(count == 2);
}

test "roc test runs expects in Parser type module (dev)" {
    // TODO: dev backend compilation fails for test/package_simple_parser/Parser.roc
    return error.SkipZigTest;
}

test "roc test polymorphic list reverse with numeric literal does not overflow (interpreter)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Calling a polymorphic function (List(a) -> List(a)) from another module
    // with a numeric literal argument caused an integer overflow in
    // from_numeral_flex_count during runtime unification.
    const result = try util.runRoc(gpa, &.{ "test", "--opt=interpreter" }, "test/cli/polymorphic_list_reverse.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Should succeed (exit code 0), not panic
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Stderr should not contain "panic" or "overflow"
    const has_panic = std.mem.indexOf(u8, result.stderr, "panic") != null or
        std.mem.indexOf(u8, result.stderr, "overflow") != null;
    try testing.expect(!has_panic);

    // Should report 1 passing test
    const has_passed = std.mem.indexOf(u8, result.stdout, "passed") != null;
    try testing.expect(has_passed);
}

test "roc test polymorphic list reverse with numeric literal does not overflow (dev)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Calling a polymorphic function (List(a) -> List(a)) from another module
    // with a numeric literal argument caused an integer overflow in
    // from_numeral_flex_count during runtime unification.
    const result = try util.runRoc(gpa, &.{ "test", "--opt=dev" }, "test/cli/polymorphic_list_reverse.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    // Should succeed (exit code 0), not panic
    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    // Stderr should not contain "panic" or "overflow"
    const has_panic = std.mem.indexOf(u8, result.stderr, "panic") != null or
        std.mem.indexOf(u8, result.stderr, "overflow") != null;
    try testing.expect(!has_panic);

    // Should report 1 passing test
    const has_passed = std.mem.indexOf(u8, result.stdout, "passed") != null;
    try testing.expect(has_passed);
}

test "roc test polymorphic list reverse within same module" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // A polymorphic function with a nested lambda (e.g. fold_rev callback)
    // must also type-check correctly when tested from within the same module,
    // not only from an external importer.
    const result = try util.runRoc(gpa, &.{"test"}, "test/cli/PolymorphicListReverseMod.roc");
    defer gpa.free(result.stdout);
    defer gpa.free(result.stderr);

    try testing.expect(result.term == .Exited and result.term.Exited == 0);

    const has_passed = std.mem.indexOf(u8, result.stdout, "passed") != null;
    try testing.expect(has_passed);
}

// --- Echo platform (headerless app) tests ---
// These test the echo platform path (rocRunDefaultApp) with both backends.

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
    "(5, 5, 5.0, 5.0, 5, 5.0, 5.0, 5, 5.0, 5.0, 5, 5.0, 5.0, 5.0)\n" ++
    "<opaque>\n" ++
    all_syntax_common_suffix;

// TODO: dev backend displays module-level records with field names (record
// format) while the interpreter displays them as tuples. This is because
// module-level records are stored as e_tuple in the CIR, and the interpreter
// falls back to tuple format at runtime while the dev backend uses the
// monotype which preserves field names. Once this format difference is
// resolved, use all_syntax_expected_stdout.
const all_syntax_dev_expected_stdout =
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

    try std.testing.expectEqualStrings(all_syntax_dev_expected_stdout, run_result.stdout);
    // TODO: dev backend doesn't produce dbg output
    try std.testing.expectEqualStrings("", run_result.stderr);
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
