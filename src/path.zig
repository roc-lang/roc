const std = @import("std");

/// Thread-local buffer for constructing file paths.
/// This is a scratch buffer that can be used by any module that needs to
/// build up a path without allocating memory on the heap.
///
/// IMPORTANT: This buffer is not thread-safe. Each thread gets its own buffer,
/// but within a thread, only one function can safely use this buffer at a time.
/// If you need to use multiple paths simultaneously, make a local copy or allocate
/// memory as needed.
pub threadlocal var scratch_path: [std.fs.max_path_bytes:0]u8 = undefined;

/// Recursive helper function to create a directory and all of its parent directories,
/// similar to `mkdir -p`.
///
/// This function takes a path (not null-terminated) and attempts to create directories
/// up the hierarchy until successful. It starts by attempting to create the parent directory,
/// and if that fails with FileNotFound, it recursively creates parent directories.
pub fn makeDirRecursive(path: []const u8) !void {
    // Try to create the directory
    std.fs.makeDirAbsolute(path) catch |err| {
        // If the directory already exists, that's fine
        if (err == error.PathAlreadyExists) return;

        // If the parent directory doesn't exist, recursively create it
        if (err == error.FileNotFound) {
            // Find the parent directory path
            const parent_path = std.fs.path.dirname(path) orelse return err;

            // Recursively create the parent directory
            try makeDirRecursive(parent_path);

            // Now try creating the original directory again
            return std.fs.makeDirAbsolute(path);
        }

        // If any other error occurs, propagate it
        return err;
    };
}

test "makeDirRecursive - basic functionality" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create a path for a deeply nested directory structure
    const nested_path = try std.fs.path.join(std.testing.allocator, &.{ base_dir, "a", "b", "c", "d", "e" });
    defer std.testing.allocator.free(nested_path);

    // Create the deeply nested directory using our function
    try makeDirRecursive(nested_path);

    // Verify that all directories were created by checking if the deepest one exists
    const deepest_dir = try std.fs.openDirAbsolute(nested_path, .{});
    var deepest_dir_mutable = deepest_dir;
    deepest_dir_mutable.close();
}

test "makeDirRecursive - already existing directory" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create a path for a nested directory
    const nested_path = try std.fs.path.join(std.testing.allocator, &.{ base_dir, "existing_dir" });
    defer std.testing.allocator.free(nested_path);

    // Create the directory first
    try std.fs.makeDirAbsolute(nested_path);

    // Try creating it again with our function - should not error
    try makeDirRecursive(nested_path);

    // Verify that the directory still exists
    const dir = try std.fs.openDirAbsolute(nested_path, .{});
    var dir_mutable = dir;
    dir_mutable.close();
}

test "makeDirRecursive - partial existing path" {
    // Create a temporary directory for testing
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Get the absolute path of the temp directory
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create a path for a middle directory
    const mid_path = try std.fs.path.join(std.testing.allocator, &.{ base_dir, "partial", "path" });
    defer std.testing.allocator.free(mid_path);

    // Create the middle directory
    try makeDirRecursive(mid_path);

    // Create a path for a deeper directory
    const deep_path = try std.fs.path.join(std.testing.allocator, &.{ base_dir, "partial", "path", "deeper", "directory" });
    defer std.testing.allocator.free(deep_path);

    // Create the deeper directory - should only need to create the new parts
    try makeDirRecursive(deep_path);

    // Verify that all directories were created
    const deepest_dir = try std.fs.openDirAbsolute(deep_path, .{});
    var deepest_dir_mutable = deepest_dir;
    deepest_dir_mutable.close();
}
