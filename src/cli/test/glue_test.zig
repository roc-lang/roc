//! Integration tests for the roc glue command.

const std = @import("std");
const util = @import("util.zig");

test "glue command with DebugGlue succeeds" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc experimental-glue src/glue/src/DebugGlue.roc <tmp_path> test/fx/platform/main.roc
    const result = try util.runRocCommand(allocator, &.{
        "experimental-glue",
        "src/glue/src/DebugGlue.roc",
        tmp_path,
        "test/fx/platform/main.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "unreachable") == null);

    // Should complete successfully
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("\nDebugGlue command failed!\nstderr:\n{s}\nstdout:\n{s}\n", .{ result.stderr, result.stdout });
        std.debug.print("Exit term: {}\n", .{result.term});
        try std.testing.expect(false);
    }

    // DBG output goes to stderr - should show the actual name, not empty string
    // If the small string encoding issue is present, we'd see name: ""
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "name: \"\"") == null);

    // Should show the actual entry point name from the platform header
    // For fx platform, the requires entry is "main!" with type "() => {}"
    // ROC DBG output goes to stderr
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "name: \"main!\"") != null);
}

test "glue command with CGlue generates expected C header" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc experimental-glue src/glue/src/CGlue.roc <tmp_path> test/fx/platform/main.roc
    const result = try util.runRocCommand(allocator, &.{
        "experimental-glue",
        "src/glue/src/CGlue.roc",
        tmp_path,
        "test/fx/platform/main.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "unreachable") == null);

    // Should complete successfully
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("\nCGlue command failed!\nstderr:\n{s}\nstdout:\n{s}\n", .{ result.stderr, result.stdout });
        try std.testing.expect(false);
    }

    // Read the generated header file
    const generated_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.h" }) catch unreachable;
    defer allocator.free(generated_path);

    const generated_content = std.fs.cwd().readFileAlloc(allocator, generated_path, 1024 * 1024) catch |err| {
        std.debug.print("\nFailed to read generated file '{s}': {}\n", .{ generated_path, err });
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(generated_content);

    // Read the expected header file
    const expected_content = std.fs.cwd().readFileAlloc(allocator, "test/glue/fx_platform_cglue_expected.h", 1024 * 1024) catch |err| {
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

// Test for Windows-specific crash when returning non-empty List(OpaqueType) in Ok()
// See PLAN.md for detailed investigation of this bug.
// This test documents the expected behavior - it should succeed on all platforms.
// Currently crashes on Windows with stack overflow (exit code 134).
test "glue command with MinimalGlue returns non-empty file list" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc experimental-glue src/glue/src/MinimalGlue.roc <tmp_path> test/fx/platform/main.roc
    // MinimalGlue returns Ok([{ name: "test.txt", content: "hello" }])
    const result = try util.runRocCommand(allocator, &.{
        "experimental-glue",
        "src/glue/src/MinimalGlue.roc",
        tmp_path,
        "test/fx/platform/main.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic or hit unreachable
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "unreachable") == null);

    // Should complete successfully (exit code 0)
    // On Windows, this currently fails with stack overflow (exit code 134)
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("\nMinimalGlue command failed!\n", .{});
        std.debug.print("Exit term: {}\n", .{result.term});
        std.debug.print("stderr:\n{s}\n", .{result.stderr});
        std.debug.print("stdout:\n{s}\n", .{result.stdout});
        try std.testing.expect(false);
    }

    // Should have written the output file
    const output_path = std.fs.path.join(allocator, &.{ tmp_path, "test.txt" }) catch unreachable;
    defer allocator.free(output_path);

    const content = std.fs.cwd().readFileAlloc(allocator, output_path, 1024) catch |err| {
        std.debug.print("\nFailed to read output file '{s}': {}\n", .{ output_path, err });
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(content);

    try std.testing.expectEqualStrings("hello", content);
}

// Contrast test: Empty list works fine on all platforms including Windows
test "glue command with EmptyListGlue returns empty file list" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc experimental-glue src/glue/src/EmptyListGlue.roc <tmp_path> test/fx/platform/main.roc
    // EmptyListGlue returns Ok([])
    const result = try util.runRocCommand(allocator, &.{
        "experimental-glue",
        "src/glue/src/EmptyListGlue.roc",
        tmp_path,
        "test/fx/platform/main.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "unreachable") == null);

    // Should complete successfully on all platforms
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("\nEmptyListGlue command failed!\n", .{});
        std.debug.print("Exit term: {}\n", .{result.term});
        std.debug.print("stderr:\n{s}\n", .{result.stderr});
        std.debug.print("stdout:\n{s}\n", .{result.stdout});
        try std.testing.expect(false);
    }

    // Output should indicate 0 files were generated
    try std.testing.expect(std.mem.indexOf(u8, result.stdout, "0 file") != null);
}

test "glue command generated C header compiles with zig cc" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc experimental-glue src/glue/src/CGlue.roc <tmp_path> test/fx/platform/main.roc
    const glue_result = try util.runRocCommand(allocator, &.{
        "experimental-glue",
        "src/glue/src/CGlue.roc",
        tmp_path,
        "test/fx/platform/main.roc",
    });
    defer allocator.free(glue_result.stdout);
    defer allocator.free(glue_result.stderr);

    // Should complete successfully
    if (glue_result.term != .Exited or glue_result.term.Exited != 0) {
        std.debug.print("\nCGlue command failed!\nstderr:\n{s}\nstdout:\n{s}\n", .{ glue_result.stderr, glue_result.stdout });
        try std.testing.expect(false);
    }

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

    std.fs.cwd().writeFile(.{
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
    const cc_result = std.process.Child.run(.{
        .allocator = allocator,
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
    if (cc_result.term != .Exited or cc_result.term.Exited != 0) {
        // Read the generated header for debugging
        const header_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.h" }) catch unreachable;
        defer allocator.free(header_path);

        const header_content = std.fs.cwd().readFileAlloc(allocator, header_path, 1024 * 1024) catch "<failed to read header>";
        defer if (header_content.ptr != "<failed to read header>".ptr) allocator.free(header_content);

        std.debug.print("\nzig cc compilation failed!\n", .{});
        std.debug.print("\n--- Compiler stderr ---\n{s}\n", .{cc_result.stderr});
        std.debug.print("\n--- Generated header ---\n{s}\n", .{header_content});

        try std.testing.expect(false);
    }
}
