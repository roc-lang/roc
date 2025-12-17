//! Tests for the shared memory ModuleEnv system

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const main = @import("main.zig");
const base = @import("base");
const Allocators = base.Allocators;
const cli_error = @import("cli_error.zig");
const CliContext = cli_error.CliContext;
const Io = cli_error.Io;

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

// Integration tests that test the full shared memory pipeline

test "integration - shared memory setup and parsing" {
    if (builtin.os.tag == .windows) {
        // Skip on Windows for now since shared memory implementation differs
        return;
    }

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

    // Use the real int test platform
    const roc_path = "test/int/app.roc";

    // Test that we can set up shared memory with ModuleEnv
    const shm_result = try main.setupSharedMemoryWithModuleEnv(&ctx, roc_path, true);
    const shm_handle = shm_result.handle;

    // Clean up shared memory resources
    defer {
        if (comptime builtin.os.tag == .windows) {
            _ = @import("ipc").platform.windows.UnmapViewOfFile(shm_handle.ptr);
            _ = @import("ipc").platform.windows.CloseHandle(@ptrCast(shm_handle.fd));
        } else {
            const posix = struct {
                extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
                extern "c" fn close(fd: c_int) c_int;
            };
            _ = posix.munmap(shm_handle.ptr, shm_handle.size);
            _ = posix.close(shm_handle.fd);
        }
    }

    // Verify that shared memory was set up correctly
    try testing.expect(shm_handle.size > 0);
    try testing.expect(@intFromPtr(shm_handle.ptr) != 0);

    std.log.debug("Integration test: Successfully set up shared memory with size: {} bytes\n", .{shm_handle.size});
}

test "integration - compilation pipeline for different platforms" {
    if (builtin.os.tag == .windows) {
        return;
    }

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

    // Test with our real test platforms
    const test_apps = [_][]const u8{
        "test/int/app.roc",
        "test/str/app.roc",
        "test/fx/app.roc",
    };

    for (test_apps) |roc_path| {
        // Test the full compilation pipeline (parse -> canonicalize -> typecheck)
        const shm_result = main.setupSharedMemoryWithModuleEnv(&ctx, roc_path, true) catch |err| {
            std.log.warn("Failed to set up shared memory for {s}: {}\n", .{ roc_path, err });
            continue;
        };
        const shm_handle = shm_result.handle;

        // Clean up shared memory resources
        defer {
            if (comptime builtin.os.tag == .windows) {
                _ = @import("ipc").platform.windows.UnmapViewOfFile(shm_handle.ptr);
                _ = @import("ipc").platform.windows.CloseHandle(@ptrCast(shm_handle.fd));
            } else {
                const posix = struct {
                    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
                    extern "c" fn close(fd: c_int) c_int;
                };
                _ = posix.munmap(shm_handle.ptr, shm_handle.size);
                _ = posix.close(shm_handle.fd);
            }
        }

        // Verify shared memory was set up successfully
        try testing.expect(shm_handle.size > 0);
        std.log.debug("Successfully compiled {s} (shared memory size: {} bytes)\n", .{ roc_path, shm_handle.size });
    }
}

test "integration - error handling for non-existent file" {
    if (builtin.os.tag == .windows) {
        return;
    }

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

    // Test with a non-existent file path
    const roc_path = "test/nonexistent/app.roc";

    // This should fail because the file doesn't exist
    const result = main.setupSharedMemoryWithModuleEnv(&ctx, roc_path, true);

    // We expect this to fail - the important thing is that it doesn't crash
    if (result) |shm_result| {
        const shm_handle = shm_result.handle;
        // Clean up shared memory resources if somehow successful
        defer {
            if (comptime builtin.os.tag == .windows) {
                _ = @import("ipc").platform.windows.UnmapViewOfFile(shm_handle.ptr);
                _ = @import("ipc").platform.windows.CloseHandle(@ptrCast(shm_handle.fd));
            } else {
                const posix = struct {
                    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
                    extern "c" fn close(fd: c_int) c_int;
                };
                _ = posix.munmap(shm_handle.ptr, shm_handle.size);
                _ = posix.close(shm_handle.fd);
            }
        }
        // This shouldn't happen with a non-existent file
        return error.UnexpectedSuccess;
    } else |err| {
        // Expected to fail
        std.log.debug("Compilation failed as expected with error: {}\n", .{err});
    }
}
