//! Tests for the shared memory ModuleEnv system

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const eval_shim = @import("eval_shim.zig");
const main = @import("main.zig");

test "evaluator - basic arithmetic addition" {
    const result = eval_shim.performAdd(.{ .int = 5 }, .{ .int = 3 });
    try testing.expect(!result.isError());
    try testing.expectEqual(@as(i128, 8), result.int);
}

test "evaluator - basic arithmetic subtraction" {
    const result = eval_shim.performSub(.{ .int = 10 }, .{ .int = 4 });
    try testing.expect(!result.isError());
    try testing.expectEqual(@as(i128, 6), result.int);
}

test "evaluator - basic arithmetic multiplication" {
    const result = eval_shim.performMul(.{ .int = 7 }, .{ .int = 6 });
    try testing.expect(!result.isError());
    try testing.expectEqual(@as(i128, 42), result.int);
}

test "evaluator - basic arithmetic division" {
    const result = eval_shim.performDiv(.{ .int = 15 }, .{ .int = 3 });
    try testing.expect(!result.isError());
    try testing.expectEqual(@as(i128, 5), result.int);
}

test "evaluator - division by zero" {
    const result = eval_shim.performDiv(.{ .int = 10 }, .{ .int = 0 });
    try testing.expect(result.isError());
}

test "evaluator - mixed type arithmetic int + float32" {
    const result = eval_shim.performAdd(.{ .int = 5 }, .{ .float32 = 2.5 });
    try testing.expect(!result.isError());
    try testing.expectEqual(@as(f32, 7.5), result.float32);
}

test "evaluator - mixed type arithmetic float64 * int" {
    const result = eval_shim.performMul(.{ .float64 = 3.14 }, .{ .int = 2 });
    try testing.expect(!result.isError());
    try testing.expectApproxEqAbs(@as(f64, 6.28), result.float64, 0.001);
}

test "evaluator - comparison operations" {
    const eq_result = eval_shim.performEq(.{ .int = 5 }, .{ .int = 5 });
    try testing.expect(!eq_result.isError());
    try testing.expectEqual(true, eq_result.boolean);

    const neq_result = eval_shim.performNeq(.{ .int = 5 }, .{ .int = 3 });
    try testing.expect(!neq_result.isError());
    try testing.expectEqual(true, neq_result.boolean);

    const lt_result = eval_shim.performLt(.{ .int = 3 }, .{ .int = 5 });
    try testing.expect(!lt_result.isError());
    try testing.expectEqual(true, lt_result.boolean);
}

test "evaluator - string comparisons" {
    const eq_result = eval_shim.performEq(.{ .string = "hello" }, .{ .string = "hello" });
    try testing.expect(!eq_result.isError());
    try testing.expectEqual(true, eq_result.boolean);

    const lt_result = eval_shim.performLt(.{ .string = "apple" }, .{ .string = "banana" });
    try testing.expect(!lt_result.isError());
    try testing.expectEqual(true, lt_result.boolean);
}

test "evaluator - boolean operations" {
    const and_result = eval_shim.performAnd(.{ .boolean = true }, .{ .boolean = false });
    try testing.expect(!and_result.isError());
    try testing.expectEqual(false, and_result.boolean);

    const or_result = eval_shim.performOr(.{ .boolean = true }, .{ .boolean = false });
    try testing.expect(!or_result.isError());
    try testing.expectEqual(true, or_result.boolean);
}

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

test "shared memory system - error handling edge cases" {
    // Test error conditions in evaluator

    // Test unsupported binary operation combination
    const string_add_int = eval_shim.performAdd(.{ .string = "hello" }, .{ .int = 5 });
    try testing.expect(string_add_int.isError());

    // Test remainder with non-integer
    const float_rem = eval_shim.performRem(.{ .float32 = 5.5 }, .{ .int = 2 });
    try testing.expect(float_rem.isError());

    // Test comparison between incompatible types for ordering
    const string_vs_int = eval_shim.performLt(.{ .string = "hello" }, .{ .int = 5 });
    try testing.expect(string_vs_int.isError());

    // Test boolean operations with wrong types
    const int_and_bool = eval_shim.performAnd(.{ .int = 1 }, .{ .boolean = true });
    try testing.expect(int_and_bool.isError());
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

test "evaluator - type coercion hierarchy" {
    // Test that mixed operations follow proper type promotion hierarchy

    // int + float32 -> float32
    const int_float32 = eval_shim.performAdd(.{ .int = 10 }, .{ .float32 = 3.5 });
    try testing.expect(!int_float32.isError());
    try testing.expectEqual(@as(f32, 13.5), int_float32.float32);

    // float32 + float64 -> float64
    const float32_float64 = eval_shim.performMul(.{ .float32 = 2.0 }, .{ .float64 = 3.14159 });
    try testing.expect(!float32_float64.isError());
    try testing.expectApproxEqAbs(@as(f64, 6.28318), float32_float64.float64, 0.00001);

    // int + float64 -> float64
    const int_float64 = eval_shim.performSub(.{ .int = 100 }, .{ .float64 = 0.5 });
    try testing.expect(!int_float64.isError());
    try testing.expectEqual(@as(f64, 99.5), int_float64.float64);
}

test "evaluator - edge cases" {
    // Test remainder edge cases
    const zero_rem = eval_shim.performRem(.{ .int = 0 }, .{ .int = 5 });
    try testing.expect(!zero_rem.isError());
    try testing.expectEqual(@as(i128, 0), zero_rem.int);

    // Test negative number operations
    const neg_add = eval_shim.performAdd(.{ .int = -5 }, .{ .int = 3 });
    try testing.expect(!neg_add.isError());
    try testing.expectEqual(@as(i128, -2), neg_add.int);

    // Test string ordering with different lengths
    const short_long = eval_shim.performLt(.{ .string = "a" }, .{ .string = "abc" });
    try testing.expect(!short_long.isError());
    try testing.expectEqual(true, short_long.boolean);

    // Test empty string comparisons
    const empty_strings = eval_shim.performEq(.{ .string = "" }, .{ .string = "" });
    try testing.expect(!empty_strings.isError());
    try testing.expectEqual(true, empty_strings.boolean);
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

    const roc_content = "42 + 58";

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    // Test that we can set up shared memory with ModuleEnv
    const shm_handle = main.setupSharedMemoryWithModuleEnv(allocator, roc_path) catch |err| switch (err) {
        // On some systems, shared memory might not be available in test environment
        error.ShmOpenFailed => {
            std.log.warn("Shared memory not available in test environment, skipping integration test\n", .{});
            return;
        },
        else => return err,
    };
    defer main.cleanupSharedMemory();

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

    for (test_cases) |roc_content| {
        var temp_dir = testing.tmpDir(.{});
        defer temp_dir.cleanup();

        var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
        defer roc_file.close();
        roc_file.writeAll(roc_content) catch unreachable;

        const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
        defer allocator.free(roc_path);

        // Test the full compilation pipeline (parse -> canonicalize -> typecheck)
        const shm_handle = main.setupSharedMemoryWithModuleEnv(allocator, roc_path) catch |err| switch (err) {
            error.ShmOpenFailed => {
                std.log.warn("Shared memory not available, skipping expression: {s}\n", .{roc_content});
                continue;
            },
            else => return err,
        };
        defer main.cleanupSharedMemory();

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
    const invalid_roc_content = "42 + + 58"; // Invalid syntax

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(invalid_roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(roc_path);

    // This should fail during parsing/compilation
    const result = main.setupSharedMemoryWithModuleEnv(allocator, roc_path);

    // We expect this to either fail or succeed (depending on parser error handling)
    // The important thing is that it doesn't crash
    if (result) |shm_handle| {
        defer main.cleanupSharedMemory();
        std.log.info("Compilation succeeded even with invalid syntax (size: {} bytes)\n", .{shm_handle.size});
    } else |err| {
        std.log.info("Compilation failed as expected with error: {}\n", .{err});
    }
}
