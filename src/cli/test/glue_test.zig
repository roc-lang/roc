//! Integration tests for the roc glue command.

const std = @import("std");
const glue = @import("glue");
const util = @import("util.zig");

const InProcessGlueResult = struct {
    stdout: []u8,
    stderr: []u8,
    success: bool,

    fn deinit(self: InProcessGlueResult, allocator: std.mem.Allocator) void {
        allocator.free(self.stdout);
        allocator.free(self.stderr);
    }
};

fn runGlueCommand(
    allocator: std.mem.Allocator,
    opt: []const u8,
    glue_spec: []const u8,
    tmp_path: []const u8,
) anyerror!util.RocResult {
    const result = try util.runRocCommand(allocator, &.{
        "glue",
        opt,
        glue_spec,
        tmp_path,
        "test/fx/platform/main.roc",
    });
    try std.testing.expect(std.mem.find(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.find(u8, result.stderr, "unreachable") == null);
    return result;
}

fn runGlueInProcess(
    allocator: std.mem.Allocator,
    glue_spec: []const u8,
    tmp_path: []const u8,
) anyerror!InProcessGlueResult {
    var stdout_buffer = std.Io.Writer.Allocating.init(allocator);
    defer stdout_buffer.deinit();
    var stderr_buffer = std.Io.Writer.Allocating.init(allocator);
    defer stderr_buffer.deinit();

    const success = if (glue.rocGlue(allocator, &stderr_buffer.writer, &stdout_buffer.writer, .{
        .glue_spec = glue_spec,
        .output_dir = tmp_path,
        .platform_path = "test/fx/platform/main.roc",
        .backend = .interpreter,
    }, tmp_path, std.testing.io)) |_| true else |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => false,
    };

    const stdout = try stdout_buffer.toOwnedSlice();
    errdefer allocator.free(stdout);
    const stderr = try stderr_buffer.toOwnedSlice();
    return .{
        .stdout = stdout,
        .stderr = stderr,
        .success = success,
    };
}

fn checkGlueSuccess(result: util.RocResult, label: []const u8) anyerror!void {
    if (result.term != .exited or result.term.exited != 0) {
        std.debug.print("\n{s} command failed!\nstderr:\n{s}\nstdout:\n{s}\nExit term: {}\n", .{
            label, result.stderr, result.stdout, result.term,
        });
        try std.testing.expect(false);
    }
}

fn checkGlueInProcessSuccess(result: InProcessGlueResult, label: []const u8) anyerror!void {
    if (!result.success) {
        std.debug.print("\n{s} command failed!\nstderr:\n{s}\nstdout:\n{s}\n", .{
            label, result.stderr, result.stdout,
        });
        try std.testing.expect(false);
    }
}

fn expectGeneratedCHeaderCompiles(allocator: std.mem.Allocator, tmp_path: []const u8) anyerror!void {
    const test_c_content =
        \\#include "roc_platform_abi.h"
        \\
        \\void test_types(void) {
        \\    RocStr str = {0};
        \\    RocList list = {0};
        \\    HostedFunctions funcs = {0};
        \\    (void)str;
        \\    (void)list;
        \\    (void)funcs;
        \\}
    ;

    const test_c_path = try std.fs.path.join(allocator, &.{ tmp_path, "test_header.c" });
    defer allocator.free(test_c_path);

    std.Io.Dir.cwd().writeFile(std.testing.io, .{
        .sub_path = test_c_path,
        .data = test_c_content,
    }) catch |err| {
        std.debug.print("\nFailed to write test C file: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };

    const test_o_path = try std.fs.path.join(allocator, &.{ tmp_path, "test_header.o" });
    defer allocator.free(test_o_path);

    const include_flag = try std.fmt.allocPrint(allocator, "-I{s}", .{tmp_path});
    defer allocator.free(include_flag);

    const cc_result = std.process.run(allocator, std.testing.io, .{
        .argv = &.{
            "zig",
            "cc",
            "-c",
            "-std=c11",
            "-Wall",
            "-Werror",
            include_flag,
            test_c_path,
            "-o",
            test_o_path,
        },
    }) catch |err| {
        std.debug.print("\nFailed to run zig cc: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(cc_result.stdout);
    defer allocator.free(cc_result.stderr);

    if (cc_result.term != .exited or cc_result.term.exited != 0) {
        const header_path = try std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.h" });
        defer allocator.free(header_path);

        const header_content = std.Io.Dir.cwd().readFileAlloc(std.testing.io, header_path, allocator, .limited(1024 * 1024)) catch "<failed to read header>";
        defer if (header_content.ptr != "<failed to read header>".ptr) allocator.free(header_content);

        std.debug.print("\nzig cc compilation failed!\n", .{});
        std.debug.print("\n--- Compiler stderr ---\n{s}\n", .{cc_result.stderr});
        std.debug.print("\n--- Generated header ---\n{s}\n", .{header_content});

        try std.testing.expect(false);
    }
}

test "glue command with DebugGlue succeeds (interpreter)" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const result = try runGlueCommand(allocator, "--opt=interpreter", "src/glue/src/DebugGlue.roc", tmp_path);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkGlueSuccess(result, "DebugGlue");

    // Empty string would indicate an encoding bug with the small string optimization
    try std.testing.expect(std.mem.find(u8, result.stderr, "name: \"\"") == null);

    // Should show the actual entry point name from the platform header
    try std.testing.expect(std.mem.find(u8, result.stderr, "name: \"main!\"") != null);
}

test "glue command with DebugGlue succeeds (dev backend)" {
    // TODO: dev backend fails with compilation failure
    return error.SkipZigTest;
}

test "glue command with CGlue generates expected C header and header compiles (interpreter)" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const result = try runGlueInProcess(allocator, "src/glue/src/CGlue.roc", tmp_path);
    defer result.deinit(allocator);

    try checkGlueInProcessSuccess(result, "CGlue");

    // Read the generated header file
    const generated_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.h" }) catch unreachable;
    defer allocator.free(generated_path);

    const generated_content = std.Io.Dir.cwd().readFileAlloc(std.testing.io, generated_path, allocator, .limited(1024 * 1024)) catch |err| {
        std.debug.print("\nFailed to read generated file '{s}': {}\n", .{ generated_path, err });
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(generated_content);

    // Read the expected header file
    const expected_content = std.Io.Dir.cwd().readFileAlloc(std.testing.io, "test/glue/fx_platform_cglue_expected.h", allocator, .limited(1024 * 1024)) catch |err| {
        std.debug.print("\nFailed to read expected file: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(expected_content);

    // Compare generated vs expected
    if (!std.mem.eql(u8, generated_content, expected_content)) {
        std.debug.print("\n\nGenerated C header does not match expected!\n", .{});
        std.debug.print("\n--- Expected ({d} bytes) ---\n{s}\n", .{ expected_content.len, expected_content });
        std.debug.print("\n--- Generated ({d} bytes) ---\n{s}\n", .{ generated_content.len, generated_content });

        // Find first difference for easier debugging
        var i: usize = 0;
        while (i < @min(expected_content.len, generated_content.len)) : (i += 1) {
            if (expected_content[i] != generated_content[i]) {
                const start = if (i > 20) i - 20 else 0;
                const end_expected = @min(i + 20, expected_content.len);
                const end_generated = @min(i + 20, generated_content.len);
                std.debug.print("\nFirst difference at byte {d}:\n", .{i});
                std.debug.print("  Expected:  ...{s}...\n", .{expected_content[start..end_expected]});
                std.debug.print("  Generated: ...{s}...\n", .{generated_content[start..end_generated]});
                break;
            }
        }

        try std.testing.expect(false);
    }

    try expectGeneratedCHeaderCompiles(allocator, tmp_path);
}

test "glue command with CGlue generates expected C header (dev backend)" {
    // TODO: dev backend fails with compilation failure
    return error.SkipZigTest;
}

test "glue command generated C header compiles with zig cc (dev backend)" {
    // TODO: dev backend fails with compilation failure
    return error.SkipZigTest;
}

test "glue regression: ZigGlue interpreter succeeds on fx platform" {
    // This is the normal CLI-level repro for:
    //   roc glue --opt=interpreter src/glue/src/ZigGlue.roc <tmp> test/fx/platform/main.roc
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const result = try runGlueInProcess(allocator, "src/glue/src/ZigGlue.roc", tmp_path);
    defer result.deinit(allocator);

    try std.testing.expect(std.mem.find(u8, result.stderr, "misaligned") == null);
    try checkGlueInProcessSuccess(result, "ZigGlue");

    // Should produce a Zig output file
    const generated_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.zig" }) catch unreachable;
    defer allocator.free(generated_path);

    const generated_content = std.Io.Dir.cwd().readFileAlloc(std.testing.io, generated_path, allocator, .limited(1024 * 1024)) catch |err| {
        std.debug.print("\nFailed to read generated file '{s}': {}\n", .{ generated_path, err });
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(generated_content);

    // Generated file should contain key Zig constructs
    try std.testing.expect(std.mem.find(u8, generated_content, "pub const RocStr") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "pub const RocOps") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "Entrypoint") != null);
}

test "glue command with ZigGlue succeeds (dev backend)" {
    // TODO: dev backend hangs during roc glue execution
    return error.SkipZigTest;
}

test "CGlue.roc expect tests pass (interpreter)" {
    const allocator = std.testing.allocator;

    const result = try util.runRocCommand(allocator, &.{
        "test",
        "--opt=interpreter",
        "src/glue/src/CGlue.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic
    try std.testing.expect(std.mem.find(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.find(u8, result.stderr, "unreachable") == null);

    // Should complete successfully
    if (result.term != .exited or result.term.exited != 0) {
        std.debug.print("\nroc test CGlue.roc failed!\nstderr:\n{s}\nstdout:\n{s}\n", .{ result.stderr, result.stdout });
        std.debug.print("Exit term: {}\n", .{result.term});
        try std.testing.expect(false);
    }
}

test "CGlue.roc expect tests pass (dev backend)" {
    // TODO: dev backend fails with SIGSEGV
    return error.SkipZigTest;
}
