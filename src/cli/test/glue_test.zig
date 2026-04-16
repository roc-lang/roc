//! Integration tests for the roc glue command.

const std = @import("std");
const util = @import("util.zig");

/// Run `roc glue` with given opt level and glue spec, returning result and tmp_dir.
/// Caller must free result.stdout, result.stderr, and tmp_path.
/// tmp_dir is returned for further inspection of generated files.
fn runGlueCommand(
    allocator: std.mem.Allocator,
    opt: []const u8,
    glue_spec: []const u8,
    tmp_path: []const u8,
) !util.RocResult {
    const result = try util.runRocCommand(allocator, &.{
        "glue",
        opt,
        glue_spec,
        tmp_path,
        "test/fx/platform/main.roc",
    });
    // Common checks: should not panic
    try std.testing.expect(std.mem.find(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.find(u8, result.stderr, "unreachable") == null);
    return result;
}

fn checkGlueSuccess(result: util.RocResult, label: []const u8) !void {
    if (result.term != .exited or result.term.exited != 0) {
        std.debug.print("\n{s} command failed!\nstderr:\n{s}\nstdout:\n{s}\nExit term: {}\n", .{
            label, result.stderr, result.stdout, result.term,
        });
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

test "glue command with CGlue generates expected C header (interpreter)" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const result = try runGlueCommand(allocator, "--opt=interpreter", "src/glue/src/CGlue.roc", tmp_path);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkGlueSuccess(result, "CGlue");

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
}

test "glue command with CGlue generates expected C header (dev backend)" {
    // TODO: dev backend fails with compilation failure
    return error.SkipZigTest;
}

test "glue command generated C header compiles with zig cc (interpreter)" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const glue_result = try runGlueCommand(allocator, "--opt=interpreter", "src/glue/src/CGlue.roc", tmp_path);
    defer allocator.free(glue_result.stdout);
    defer allocator.free(glue_result.stderr);

    try checkGlueSuccess(glue_result, "CGlue");

    // Write a minimal C file that includes the header and instantiates the types
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

    const test_c_path = std.fs.path.join(allocator, &.{ tmp_path, "test_header.c" }) catch unreachable;
    defer allocator.free(test_c_path);

    std.Io.Dir.cwd().writeFile(std.testing.io, .{
        .sub_path = test_c_path,
        .data = test_c_content,
    }) catch |err| {
        std.debug.print("\nFailed to write test C file: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };

    const test_o_path = std.fs.path.join(allocator, &.{ tmp_path, "test_header.o" }) catch unreachable;
    defer allocator.free(test_o_path);

    const include_flag = std.fmt.allocPrint(allocator, "-I{s}", .{tmp_path}) catch unreachable;
    defer allocator.free(include_flag);

    // Run: zig cc -c -std=c11 -Wall -Werror -I<tmp_path> test_header.c -o test_header.o
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

    // Check compilation succeeded
    if (cc_result.term != .exited or cc_result.term.exited != 0) {
        // Read the generated header for debugging
        const header_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.h" }) catch unreachable;
        defer allocator.free(header_path);

        const header_content = std.Io.Dir.cwd().readFileAlloc(std.testing.io, header_path, allocator, .limited(1024 * 1024)) catch "<failed to read header>";
        defer if (header_content.ptr != "<failed to read header>".ptr) allocator.free(header_content);

        std.debug.print("\nzig cc compilation failed!\n", .{});
        std.debug.print("\n--- Compiler stderr ---\n{s}\n", .{cc_result.stderr});
        std.debug.print("\n--- Generated header ---\n{s}\n", .{header_content});

        try std.testing.expect(false);
    }
}

test "glue command generated C header compiles with zig cc (dev backend)" {
    // TODO: dev backend fails with compilation failure
    return error.SkipZigTest;
}

test "glue command with ZigGlue succeeds (interpreter)" {
    // Regression test for nominal_translate_cache fix in interpreter.zig.
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const result = try runGlueCommand(allocator, "--opt=interpreter", "src/glue/src/ZigGlue.roc", tmp_path);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try std.testing.expect(std.mem.find(u8, result.stderr, "misaligned") == null);
    try checkGlueSuccess(result, "ZigGlue");

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

    // Run: roc test --opt=interpreter src/glue/src/CGlue.roc
    // --no-cache avoids a cache interaction bug where the module cache
    // populated by earlier glue tests (roc build) is incompatible with
    // what roc test's interpreter expects, causing a .ty_tag_union panic.
    const result = try util.runRocCommand(allocator, &.{
        "test",
        "--opt=interpreter",
        "--no-cache",
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
