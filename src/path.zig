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
pub fn createDirAndParents(path: []const u8) !void {
    // Try to create the directory
    std.fs.makeDirAbsolute(path) catch |err| {
        // If the directory already exists, that's fine
        if (err == error.PathAlreadyExists) return;

        // If the parent directory doesn't exist, recursively create it
        if (err == error.FileNotFound) {
            // Find the parent directory path
            const parent_path = std.fs.path.dirname(path) orelse return err;

            // Recursively create the parent directory
            try createDirAndParents(parent_path);

            // Now try creating the original directory again
            return std.fs.makeDirAbsolute(path);
        }

        // If any other error occurs, propagate it
        return err;
    };
}