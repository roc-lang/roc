//! Integration tests for the roc glue command.

const std = @import("std");
const util = @import("util.zig");

/// Run `roc glue` with given opt level, glue spec, and platform file.
/// Caller must free result.stdout and result.stderr.
fn runGlueCommandForPlatform(
    allocator: std.mem.Allocator,
    opt: []const u8,
    glue_spec: []const u8,
    tmp_path: []const u8,
    roc_file: []const u8,
) !util.RocResult {
    const result = try util.runRocCommand(allocator, &.{
        "glue",
        opt,
        glue_spec,
        tmp_path,
        roc_file,
    });
    // Common checks: should not panic
    try std.testing.expect(std.mem.find(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.find(u8, result.stderr, "unreachable") == null);
    return result;
}

/// Run `roc glue` against the default fx platform.
/// Caller must free result.stdout and result.stderr.
fn runGlueCommand(
    allocator: std.mem.Allocator,
    opt: []const u8,
    glue_spec: []const u8,
    tmp_path: []const u8,
) !util.RocResult {
    return runGlueCommandForPlatform(allocator, opt, glue_spec, tmp_path, "test/fx/platform/main.roc");
}

fn checkGlueSuccess(result: util.RocResult, label: []const u8) !void {
    if (result.term != .exited or result.term.exited != 0) {
        std.debug.print("\n{s} command failed!\nstderr:\n{s}\nstdout:\n{s}\nExit term: {}\n", .{
            label, result.stderr, result.stdout, result.term,
        });
        try std.testing.expect(false);
    }
}

fn runGeneratedZigBoxHelperTest(allocator: std.mem.Allocator, tmp_path: []const u8) !void {
    const test_source =
        \\const std = @import("std");
        \\const abi = @import("roc_platform_abi.zig");
        \\
        \\const Env = struct {
        \\    callback_count: usize = 0,
        \\    callback_rc: isize = -1,
        \\    dealloc_count: usize = 0,
        \\    dealloc_ptr: usize = 0,
        \\    dealloc_alignment: usize = 0,
        \\};
        \\
        \\fn dummyHostedFn(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
        \\
        \\var hosted_fns = [_]abi.HostedFn{&dummyHostedFn};
        \\
        \\fn rocAlloc(_: *abi.RocAlloc, _: *anyopaque) callconv(.c) void {
        \\    unreachable;
        \\}
        \\
        \\fn rocDealloc(dealloc_args: *abi.RocDealloc, env_ptr: *anyopaque) callconv(.c) void {
        \\    const env: *Env = @ptrCast(@alignCast(env_ptr));
        \\    env.dealloc_count += 1;
        \\    env.dealloc_ptr = @intFromPtr(dealloc_args.ptr);
        \\    env.dealloc_alignment = dealloc_args.alignment;
        \\}
        \\
        \\fn rocRealloc(_: *abi.RocRealloc, _: *anyopaque) callconv(.c) void {
        \\    unreachable;
        \\}
        \\
        \\fn rocDbg(_: *const abi.RocDbg, _: *anyopaque) callconv(.c) void {}
        \\fn rocExpectFailed(_: *const abi.RocExpectFailed, _: *anyopaque) callconv(.c) void {}
        \\fn rocCrashed(_: *const abi.RocCrashed, _: *anyopaque) callconv(.c) void {}
        \\
        \\fn makeOps(env: *Env) abi.RocOps {
        \\    return .{
        \\        .env = @ptrCast(env),
        \\        .roc_alloc = &rocAlloc,
        \\        .roc_dealloc = &rocDealloc,
        \\        .roc_realloc = &rocRealloc,
        \\        .roc_dbg = &rocDbg,
        \\        .roc_expect_failed = &rocExpectFailed,
        \\        .roc_crashed = &rocCrashed,
        \\        .hosted_fns = .{ .count = 0, .fns = &hosted_fns },
        \\    };
        \\}
        \\
        \\fn dataPtr(comptime payload_contains_refcounted: bool, backing: *align(16) [64]u8) *anyopaque {
        \\    const header_bytes = if (payload_contains_refcounted) 2 * @sizeOf(usize) else @sizeOf(usize);
        \\    const base: [*]u8 = @ptrCast(backing);
        \\    return @ptrCast(base + header_bytes);
        \\}
        \\
        \\fn refcountPtr(data_ptr: *anyopaque) *isize {
        \\    return @ptrFromInt(@intFromPtr(data_ptr) - @sizeOf(isize));
        \\}
        \\
        \\fn payloadDrop(data_ptr: ?*anyopaque, roc_ops: *abi.RocOps) callconv(.c) void {
        \\    const env: *Env = @ptrCast(@alignCast(roc_ops.env));
        \\    env.callback_count += 1;
        \\    env.callback_rc = refcountPtr(data_ptr orelse unreachable).*;
        \\}
        \\
        \\test "decrefBoxWith runs payload callback after final atomic decrement" {
        \\    var env = Env{};
        \\    var ops = makeOps(&env);
        \\    var backing: [64]u8 align(16) = undefined;
        \\    const ptr = dataPtr(true, &backing);
        \\
        \\    refcountPtr(ptr).* = 1;
        \\    abi.decrefBoxWith(ptr, @alignOf(usize), &payloadDrop, &ops);
        \\
        \\    try std.testing.expectEqual(@as(usize, 1), env.callback_count);
        \\    try std.testing.expectEqual(@as(isize, 0), env.callback_rc);
        \\    try std.testing.expectEqual(@as(usize, 1), env.dealloc_count);
        \\    try std.testing.expectEqual(@intFromPtr(&backing), env.dealloc_ptr);
        \\    try std.testing.expectEqual(@as(usize, @alignOf(usize)), env.dealloc_alignment);
        \\}
        \\
        \\test "isUniqueBox returns false for static refcount" {
        \\    var env = Env{};
        \\    var ops = makeOps(&env);
        \\    var backing: [64]u8 align(16) = undefined;
        \\    const ptr = dataPtr(false, &backing);
        \\
        \\    refcountPtr(ptr).* = 0;
        \\
        \\    try std.testing.expect(!abi.isUniqueBox(ptr));
        \\    abi.decrefBox(ptr, &ops);
        \\    try std.testing.expectEqual(@as(usize, 0), env.dealloc_count);
        \\}
    ;

    const test_path = std.fs.path.join(allocator, &.{ tmp_path, "box_helper_test.zig" }) catch unreachable;
    defer allocator.free(test_path);

    std.Io.Dir.cwd().writeFile(std.testing.io, .{
        .sub_path = test_path,
        .data = test_source,
    }) catch |err| {
        std.debug.print("\nFailed to write Zig box helper test file: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };

    const zig_test_result = std.process.run(allocator, std.testing.io, .{
        .argv = &.{ "zig", "test", test_path },
    }) catch |err| {
        std.debug.print("\nFailed to run zig test for generated box helpers: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(zig_test_result.stdout);
    defer allocator.free(zig_test_result.stderr);

    if (zig_test_result.term != .exited or zig_test_result.term.exited != 0) {
        std.debug.print("\ngenerated Zig box helper test failed!\n", .{});
        std.debug.print("\n--- Compiler stderr ---\n{s}\n", .{zig_test_result.stderr});
        std.debug.print("\n--- Test source ---\n{s}\n", .{test_source});
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

test "glue regression: ZigGlue interpreter succeeds on fx platform" {
    // This is the normal CLI-level repro for:
    //   roc glue --opt=interpreter src/glue/src/ZigGlue.roc <tmp> test/fx/platform/main.roc
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
    try std.testing.expect(std.mem.find(u8, generated_content, "pub fn increfBox") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "pub fn decrefBox") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "pub fn decrefBoxWith") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "Entrypoint") != null);

    try runGeneratedZigBoxHelperTest(allocator, tmp_path);
}

test "glue regression: ZigGlue quotes bang record fields" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator) catch unreachable;
    defer allocator.free(tmp_path);

    const result = try runGlueCommandForPlatform(
        allocator,
        "--opt=interpreter",
        "src/glue/src/ZigGlue.roc",
        tmp_path,
        "test/postcheck/platform_required_init/platform/main.roc",
    );
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try checkGlueSuccess(result, "ZigGlue");

    const generated_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.zig" }) catch unreachable;
    defer allocator.free(generated_path);

    const generated_content = std.Io.Dir.cwd().readFileAlloc(std.testing.io, generated_path, allocator, .limited(1024 * 1024)) catch |err| {
        std.debug.print("\nFailed to read generated file '{s}': {}\n", .{ generated_path, err });
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(generated_content);

    try std.testing.expect(std.mem.find(u8, generated_content, "@\"init!\": *anyopaque") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "@\"render!\": *anyopaque") != null);
    try std.testing.expect(std.mem.find(u8, generated_content, "    init!:") == null);
    try std.testing.expect(std.mem.find(u8, generated_content, "    render!:") == null);

    const ast_check_result = std.process.run(allocator, std.testing.io, .{
        .argv = &.{ "zig", "ast-check", generated_path },
    }) catch |err| {
        std.debug.print("\nFailed to run zig ast-check: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(ast_check_result.stdout);
    defer allocator.free(ast_check_result.stderr);

    if (ast_check_result.term != .exited or ast_check_result.term.exited != 0) {
        std.debug.print("\nzig ast-check failed!\n", .{});
        std.debug.print("\n--- Compiler stderr ---\n{s}\n", .{ast_check_result.stderr});
        std.debug.print("\n--- Generated Zig ---\n{s}\n", .{generated_content});
        try std.testing.expect(false);
    }
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
