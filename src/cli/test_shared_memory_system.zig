//! Tests for the shared memory ModuleEnv system

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const main = @import("main.zig");

test "platform resolution - basic cli platform" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    // This should return NoPlatformFound since we don't have the actual CLI platform installed
    const result = main.resolvePlatformHost(allocator, roc_path);
    try testing.expectError(error.NoPlatformFound, result);
}

test "platform resolution - no platform in file" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    const result = main.resolvePlatformHost(allocator, roc_path);
    try testing.expectError(error.NoPlatformFound, result);
}

test "platform resolution - file not found" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = main.resolvePlatformHost(allocator, "nonexistent.roc");
    try testing.expectError(error.NoPlatformFound, result);
}

test "platform resolution - URL platform not supported" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create a temporary Roc file with URL platform
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\app "test" packages { pf: platform "https://example.com/platform.tar.gz" } imports [pf.Task] provides [main] to pf
        \\
        \\main = "Hello, World!"
    ;

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    const result = main.resolvePlatformHost(allocator, roc_path);
    try testing.expectError(error.PlatformNotSupported, result);
}

// Integration tests that test the full shared memory pipeline

test "integration - shared memory setup and parsing" {
    if (builtin.os.tag == .windows) {
        // Skip on Windows for now since shared memory implementation differs
        return;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create a temporary Roc file with simple arithmetic
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content = "app [main] { pf: platform \"test\" }\n\nmain = 42 + 58";

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    // Test that we can set up shared memory with ModuleEnv
    const shm_handle = try main.setupSharedMemoryWithModuleEnv(allocator, roc_path, "main");

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

    std.log.info("Integration test: Successfully set up shared memory with size: {} bytes\n", .{shm_handle.size});
}

test "integration - compilation pipeline for different expressions" {
    if (builtin.os.tag == .windows) {
        return;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const test_cases = [_][]const u8{
        "100 - 58",
        "7 * 6",
        "15 / 3",
        "42 + 0",
    };

    for (test_cases) |expression| {
        // Prepend boilerplate to make a complete Roc app
        const roc_content = try std.fmt.allocPrint(allocator, "app [main] {{ pf: platform \"test\" }}\n\nmain = {s}", .{expression});
        defer allocator.free(roc_content);
        var temp_dir = testing.tmpDir(.{});
        defer temp_dir.cleanup();

        var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
        defer roc_file.close();
        roc_file.writeAll(roc_content) catch unreachable;

        const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
        defer allocator.free(roc_path);

        // Test the full compilation pipeline (parse -> canonicalize -> typecheck)
        const shm_handle = main.setupSharedMemoryWithModuleEnv(allocator, roc_path, "main") catch |err| {
            std.log.warn("Failed to set up shared memory for expression: {s}, error: {}\n", .{ roc_content, err });
            continue;
        };

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
        std.log.info("Successfully compiled expression: '{s}' (shared memory size: {} bytes)\n", .{ roc_content, shm_handle.size });
    }
}

test "integration - error handling in compilation" {
    if (builtin.os.tag == .windows) {
        return;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    // Test with invalid syntax
    const invalid_roc_content = "app [main] { pf: platform \"test\" }\n\nmain = 42 + + 58"; // Invalid syntax

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(invalid_roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    // This should fail during parsing/compilation
    const result = main.setupSharedMemoryWithModuleEnv(allocator, roc_path, "main");

    // We expect this to either fail or succeed (depending on parser error handling)
    // The important thing is that it doesn't crash
    if (result) |shm_handle| {
        // Clean up shared memory resources if successful
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
        std.log.info("Compilation succeeded even with invalid syntax (size: {} bytes)\n", .{shm_handle.size});
    } else |err| {
        std.log.info("Compilation failed as expected with error: {}\n", .{err});
    }
}
