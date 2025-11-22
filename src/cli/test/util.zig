//! Utilities for CLI tests using the actual roc binary.

const std = @import("std");

/// Result of executing a Roc command during testing.
/// Contains the captured output streams and process termination status.
pub const RocResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

/// Helper to run roc with arguments that don't require a test file
pub fn runRocCommand(allocator: std.mem.Allocator, args: []const []const u8) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer allocator.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
    };

    // Build argv: [roc_path, ...args]
    const argv = try std.mem.concat(allocator, []const u8, &.{
        &.{roc_path},
        args,
    });
    defer allocator.free(argv);

    // Run roc and capture output
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
        .cwd = cwd_path,
        .max_output_bytes = 10 * 1024 * 1024, // 10MB
    });

    return RocResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .term = result.term,
    };
}

/// Helper to set up and run roc with arbitrary arguments
pub fn runRoc(allocator: std.mem.Allocator, args: []const []const u8, test_file_path: []const u8) !RocResult {
    // Get absolute path to roc binary from current working directory
    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);
    const roc_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-out", "bin", "roc" });
    defer allocator.free(roc_path);

    // Skip test if roc binary doesn't exist
    std.fs.accessAbsolute(roc_path, .{}) catch {
        std.debug.print("Skipping test: roc binary not found at {s}\n", .{roc_path});
    };

    const test_file = try std.fs.path.join(allocator, &.{ cwd_path, test_file_path });
    defer allocator.free(test_file);

    // Build argv: [roc_path, ...args, test_file]
    const argv = try std.mem.concat(allocator, []const u8, &.{
        &.{roc_path},
        args,
        &.{test_file},
    });
    defer allocator.free(argv);

    // Run roc and capture output
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
        .cwd = cwd_path,
        .max_output_bytes = 10 * 1024 * 1024, // 10MB
    });

    return RocResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .term = result.term,
    };
}
