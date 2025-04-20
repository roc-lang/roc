const std = @import("std");

/// Helper function to create a directory and all of its parent directories,
/// similar to `mkdir -p`.
///
/// The argument isn't const because this temporarily writes zeros into it to
/// null-terminate intermediate directories within the path, then restores the
/// original characters when done. This avoids the need for a separate allocation,
/// but does mean this function isn't thread-safe because it temporarily modifies
/// the given path. (To get thread-safety, clone the path before passing it in.)
pub fn makeDirRecursive(path: [:0]u8) !void {
    const path_len = path.len;

    // First, try to create the directory directly
    std.fs.makeDirAbsoluteZ(path) catch |err| {
        // If it already exists, we're done
        if (err == error.PathAlreadyExists) return;

        // If parent directory doesn't exist, we'll need to create it first
        if (err != error.FileNotFound) return err;

        // Create an array to store the indices where we need to split the path
        var separator_indices: [std.fs.max_path_bytes]usize = undefined;
        var separator_count: usize = 0;

        // Find all path separators working backwards
        var i = path_len - 1;
        while (i > 0) : (i -= 1) {
            if (path[i] == std.fs.path.sep) {
                separator_indices[separator_count] = i;
                separator_count += 1;
            }
        }

        // Try to create parent directories, starting from the highest level
        // (e.g., try /a before /a/b before /a/b/c)
        var created_index: usize = 0;
        while (created_index < separator_count) : (created_index += 1) {
            const separator_pos = separator_indices[separator_count - created_index - 1];

            // Save the character at the split position to restore later
            const save_char = path[separator_pos];
            // Null-terminate at the separator
            path[separator_pos] = 0;

            // Try to create this segment of the path
            std.fs.makeDirAbsoluteZ(path) catch |mk_err| {
                // If the directory already exists, that's fine, continue to the next segment
                if (mk_err != error.PathAlreadyExists) {
                    // Restore the character and propagate any other error
                    path[separator_pos] = save_char;
                    return mk_err;
                }
            };

            // Restore the character
            path[separator_pos] = save_char;
        }

        // Finally try to create the original target directory again.
        // This time, it should succeed, because all the necessary
        // ancestor directories exist.
        try std.fs.makeDirAbsoluteZ(path);
    };
}

test "makeDirRecursive - basic functionality" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    var nested_path_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    _ = try std.fmt.bufPrintZ(&nested_path_buf, "{s}{c}a{c}b{c}c{c}d{c}e", .{
        base_dir,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
    });

    try makeDirRecursive(&nested_path_buf);

    var deepest_dir = try std.fs.openDirAbsoluteZ(&nested_path_buf, .{});
    defer deepest_dir.close();
}

test "makeDirRecursive - already existing directory" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    var nested_path_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    _ = try std.fmt.bufPrintZ(&nested_path_buf, "{s}{c}existing_dir", .{
        base_dir,
        std.fs.path.sep,
    });

    // Create the directory first
    try std.fs.makeDirAbsoluteZ(&nested_path_buf);

    // Try creating it again with our function - should not error
    try makeDirRecursive(&nested_path_buf);

    var dir = try std.fs.openDirAbsoluteZ(&nested_path_buf, .{});
    defer dir.close();
}

test "makeDirRecursive - partial existing path" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Create a middle directory path
    var mid_path_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    _ = try std.fmt.bufPrintZ(&mid_path_buf, "{s}{c}partial{c}path", .{
        base_dir,
        std.fs.path.sep,
        std.fs.path.sep,
    });
    try makeDirRecursive(&mid_path_buf);

    // Create a deeper directory path that builds on the existing structure
    var deep_path_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    _ = try std.fmt.bufPrintZ(&deep_path_buf, "{s}{c}partial{c}path{c}deeper{c}directory", .{
        base_dir,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
    });
    try makeDirRecursive(&deep_path_buf);

    // Verify the deepest directory was created
    var deepest_dir = try std.fs.openDirAbsoluteZ(&deep_path_buf, .{});
    defer deepest_dir.close();
}
