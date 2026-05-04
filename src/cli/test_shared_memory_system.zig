//! Tests for CLI platform resolution that do not cross the post-check lowering boundary

const std = @import("std");
const testing = std.testing;
const main = @import("main.zig");
const base = @import("base");
const Allocators = base.Allocators;
const cli_context = @import("CliContext.zig");
const CliContext = cli_context.CliContext;
const Io = cli_context.Io;

test "platform resolution - basic cli platform" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file with cli platform
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\app "test"
        \\    packages { pf: platform "cli" }
        \\    imports [pf.Task]
        \\    provides [main] to pf
        \\
        \\main = "Hello, World!"
    ;

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocs.gpa, "test.roc");
    defer allocs.gpa.free(roc_path);

    // This should return CliError since we don't have the actual CLI platform installed
    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

test "platform resolution - no platform in file" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file without platform specification
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\# Just a simple expression
        \\42 + 58
    ;

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocs.gpa, "test.roc");
    defer allocs.gpa.free(roc_path);

    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

test "platform resolution - file not found" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    const result = main.resolvePlatformPaths(&ctx, "nonexistent.roc");
    try testing.expectError(error.CliError, result);
}

test "platform resolution - insecure HTTP URL rejected" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file with insecure HTTP URL (not localhost)
    // This should be rejected for security - only HTTPS or localhost HTTP allowed
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\app [main] { pf: platform "http://example.com/abc123.tar.zst" }
        \\
        \\main = "Hello, World!"
    ;

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocs.gpa, "test.roc");
    defer allocs.gpa.free(roc_path);

    // Insecure HTTP URLs (not localhost) should fail validation
    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}
