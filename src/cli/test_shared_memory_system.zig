//! Tests for the shared memory ModuleEnv system

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const main = @import("main.zig");
const base = @import("base");
const Allocators = base.Allocators;

test "platform resolution - basic cli platform" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

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

    // This should return NoPlatformFound since we don't have the actual CLI platform installed
    const result = main.resolvePlatformPaths(&allocs, roc_path);
    try testing.expectError(error.NoPlatformFound, result);
}

test "platform resolution - no platform in file" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

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

    const result = main.resolvePlatformPaths(&allocs, roc_path);
    try testing.expectError(error.NoPlatformFound, result);
}

test "platform resolution - file not found" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    const result = main.resolvePlatformPaths(&allocs, "nonexistent.roc");
    try testing.expectError(error.NoPlatformFound, result);
}

test "platform resolution - insecure HTTP URL rejected" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

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
    const result = main.resolvePlatformPaths(&allocs, roc_path);
    try testing.expectError(error.PlatformNotSupported, result);
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

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Use the real int test platform
    const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, "test/int/app.roc" }) catch return;
    defer allocs.gpa.free(roc_path);

    // Test that we can set up shared memory with ModuleEnv
    const shm_result = try main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true);
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

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Test with our real test platforms
    const test_apps = [_][]const u8{
        "test/int/app.roc",
        "test/str/app.roc",
        "test/fx/app.roc",
    };

    for (test_apps) |relative_path| {
        const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, relative_path }) catch continue;
        defer allocs.gpa.free(roc_path);
        // Test the full compilation pipeline (parse -> canonicalize -> typecheck)
        const shm_result = main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true) catch |err| {
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

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Test with a non-existent file path
    const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, "test/nonexistent/app.roc" }) catch return;
    defer allocs.gpa.free(roc_path);

    // This should fail because the file doesn't exist
    const result = main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true);

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

test "integration - automatic module dependency ordering" {
    // This test verifies that platform modules are automatically sorted by their
    // import dependencies, regardless of the order they appear in the exposes list.
    //
    // The test platform at test/str/platform/main.roc has:
    //   exposes [Helper, Core]  -- WRONG order! Helper imports Core
    //
    // Without automatic dependency ordering, this would fail because Helper would
    // be compiled before Core, and Helper's import of Core would fail.
    //
    // With automatic ordering (topological sort), we detect that Helper imports Core
    // and automatically compile Core first, making the compilation succeed.
    if (builtin.os.tag == .windows) {
        return;
    }

    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Test app_transitive.roc which uses the platform with wrong-order exposes
    const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, "test/str/app_transitive.roc" }) catch return;
    defer allocs.gpa.free(roc_path);

    // This should compile successfully because modules are automatically sorted
    const shm_result = main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true) catch |err| {
        std.log.err("Failed to compile with automatic dependency ordering: {}\n", .{err});
        return err;
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
    std.log.debug("Successfully compiled with automatic dependency ordering (shared memory size: {} bytes)\n", .{shm_handle.size});
}

test "integration - transitive module imports (module A imports module B)" {
    // This test verifies that platform modules can import other platform modules.
    // For example, if Helper imports Core, and the app calls Helper.wrap_fancy which
    // internally calls Core.wrap, the compilation should succeed without errors.
    //
    // Without proper sibling module passing during compilation, transitive module
    // calls would cause "TypeMismatch in body evaluation" panic at compile time.
    if (builtin.os.tag == .windows) {
        return;
    }

    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Test app_transitive.roc which uses Helper -> Core transitive import
    const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, "test/str/app_transitive.roc" }) catch return;
    defer allocs.gpa.free(roc_path);

    // This should compile successfully now that we pass sibling modules during compilation
    const shm_result = main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true) catch |err| {
        std.log.err("Failed to compile transitive import test: {}\n", .{err});
        return err;
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
    std.log.debug("Successfully compiled transitive import test (shared memory size: {} bytes)\n", .{shm_handle.size});
}

test "integration - diamond dependency pattern (A imports B and C, both import D)" {
    // This test verifies that diamond dependencies are handled correctly.
    // Diamond pattern:
    //   Helper imports Core AND Utils
    //   Core imports Utils
    //   So: Helper→Core→Utils AND Helper→Utils (diamond with Utils at bottom)
    //
    // The platform exposes [Helper, Core, Utils] (WRONG order)
    // Correct compilation order should be: Utils, Core, Helper
    //
    // This tests that:
    // 1. Multiple modules can import the same dependency (Utils)
    // 2. Topological sort produces valid order for diamond graphs
    // 3. Runtime module resolution works for shared dependencies
    if (builtin.os.tag == .windows) {
        return;
    }

    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Test app_diamond.roc which uses Helper.wrap_quoted (calls both Core and Utils)
    const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, "test/str/app_diamond.roc" }) catch return;
    defer allocs.gpa.free(roc_path);

    // This should compile successfully with correct dependency ordering
    const shm_result = main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true) catch |err| {
        std.log.err("Failed to compile diamond dependency test: {}\n", .{err});
        return err;
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
    std.log.debug("Successfully compiled diamond dependency test (shared memory size: {} bytes)\n", .{shm_handle.size});
}

test "integration - direct Core and Utils calls from app" {
    // This test verifies that an app can directly call platform modules
    // that have their own inter-module dependencies.
    // Core.wrap_tagged internally calls Utils.tag (Core→Utils dependency)
    if (builtin.os.tag == .windows) {
        return;
    }

    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Get absolute path from current working directory
    const cwd_path = std.fs.cwd().realpathAlloc(allocs.gpa, ".") catch return;
    defer allocs.gpa.free(cwd_path);

    // Test app_direct_core.roc which calls Core.wrap directly
    const roc_path = std.fs.path.join(allocs.gpa, &.{ cwd_path, "test/str/app_direct_core.roc" }) catch return;
    defer allocs.gpa.free(roc_path);

    const shm_result = main.setupSharedMemoryWithModuleEnv(&allocs, roc_path, true) catch |err| {
        std.log.err("Failed to compile direct Core call test: {}\n", .{err});
        return err;
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
    std.log.debug("Successfully compiled direct Core call test (shared memory size: {} bytes)\n", .{shm_handle.size});
}
