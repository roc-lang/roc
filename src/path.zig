const std = @import("std");

/// Create a directory and all of its parent directories, similar to `mkdir -p`.
/// Takes a null-terminated POSIX path. (Use makeDirRecursiveW for Windows.)
///
/// The argument isn't const because this temporarily writes zeros into it to
/// null-terminate intermediate directories within the path, then restores the
/// original characters when done. This avoids the need for a separate allocation,
/// but does mean this function isn't thread-safe because it temporarily modifies
/// the given path. (To get thread-safety, clone the path before passing it in.)
pub fn makeDirRecursiveZ(path: [:0]u8) std.fs.Dir.MakeError!void {
    std.debug.assert(std.fs.path.isAbsolute(path));

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

test "makeDirRecursiveZ - basic functionality" {
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

    try makeDirRecursiveZ(&nested_path_buf);

    var deepest_dir = try std.fs.openDirAbsoluteZ(&nested_path_buf, .{});
    defer deepest_dir.close();
}

test "makeDirRecursiveZ - already existing directory" {
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
    try makeDirRecursiveZ(&nested_path_buf);

    var dir = try std.fs.openDirAbsoluteZ(&nested_path_buf, .{});
    defer dir.close();
}

test "makeDirRecursiveZ - partial existing path" {
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
    try makeDirRecursiveZ(&mid_path_buf);

    // Create a deeper directory path that builds on the existing structure
    var deep_path_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    _ = try std.fmt.bufPrintZ(&deep_path_buf, "{s}{c}partial{c}path{c}deeper{c}directory", .{
        base_dir,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
        std.fs.path.sep,
    });
    try makeDirRecursiveZ(&deep_path_buf);

    // Verify the deepest directory was created
    var deepest_dir = try std.fs.openDirAbsoluteZ(&deep_path_buf, .{});
    defer deepest_dir.close();
}

/// Create a directory and all of its parent directories, similar to `mkdir -p`.
/// Takes a null-terminated Windows path. (Use makeDirRecursiveZ for POSIX.)
///
/// This function works with UTF-16 encoded paths on Windows.
/// The argument isn't const because this temporarily writes zeros into it to
/// null-terminate intermediate directories within the path, then restores the
/// original characters when done. This avoids the need for a separate allocation,
/// but does mean this function isn't thread-safe because it temporarily modifies
/// the given path. (To get thread-safety, clone the path before passing it in.)
pub fn makeDirRecursiveW(path: [:0]u16) std.fs.Dir.MakeError!void {
    std.debug.assert(std.fs.path.isAbsoluteW(path));

    const path_len = path.len;

    // First, try to create the directory directly
    std.fs.makeDirAbsoluteW(path) catch |err| {
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
            if (path[i] == std.fs.path.sep_windows) {
                separator_indices[separator_count] = i;
                separator_count += 1;
            }
        }

        // Try to create parent directories, starting from the highest level
        // (e.g., try C: before C:\a before C:\a\b)
        var created_index: usize = 0;
        while (created_index < separator_count) : (created_index += 1) {
            const separator_pos = separator_indices[separator_count - created_index - 1];

            // Save the character at the split position to restore later
            const save_char = path[separator_pos];
            // Null-terminate at the separator
            path[separator_pos] = 0;

            // Try to create this segment of the path
            std.fs.makeDirAbsoluteW(path) catch |mk_err| {
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
        try std.fs.makeDirAbsoluteW(path);
    };
}

test "makeDirRecursiveW - basic functionality" {
    // Skip this test on non-Windows platforms
    if (@import("builtin").os.tag != .windows) return error.SkipZigTest;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Convert the base directory path to UTF-16
    var utf16_buf: [std.fs.max_path_bytes:0]u16 = undefined;
    var utf16_len: usize = 0;

    for (base_dir) |byte| {
        utf16_buf[utf16_len] = byte;
        utf16_len += 1;
    }

    // Add nested path components
    utf16_buf[utf16_len] = std.fs.path.sep_windows;
    utf16_len += 1;
    utf16_buf[utf16_len] = 'a';
    utf16_len += 1;
    utf16_buf[utf16_len] = std.fs.path.sep_windows;
    utf16_len += 1;
    utf16_buf[utf16_len] = 'b';
    utf16_len += 1;
    utf16_buf[utf16_len] = std.fs.path.sep_windows;
    utf16_len += 1;
    utf16_buf[utf16_len] = 'c';
    utf16_len += 1;
    utf16_buf[utf16_len] = 0; // Null-terminate

    // Create the nested directories
    try makeDirRecursiveW(utf16_buf[0..utf16_len :0]);

    // Convert back to UTF-8 for verification
    var utf8_verify_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    var i: usize = 0;
    while (i < utf16_len) : (i += 1) {
        utf8_verify_buf[i] = @intCast(utf16_buf[i]);
    }
    utf8_verify_buf[utf16_len] = 0;

    // Verify the deepest directory was created
    var deepest_dir = try std.fs.openDirAbsoluteZ(utf8_verify_buf[0..utf16_len :0], .{});
    defer deepest_dir.close();
}

test "makeDirRecursiveW - already existing directory" {
    // Skip this test on non-Windows platforms
    if (@import("builtin").os.tag != .windows) return error.SkipZigTest;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Convert the base directory path to UTF-16
    var utf16_buf: [std.fs.max_path_bytes:0]u16 = undefined;
    var utf16_len: usize = 0;

    for (base_dir) |byte| {
        utf16_buf[utf16_len] = byte;
        utf16_len += 1;
    }

    // Add a single subdirectory
    utf16_buf[utf16_len] = std.fs.path.sep_windows;
    utf16_len += 1;
    utf16_buf[utf16_len] = 'e';
    utf16_len += 1;
    utf16_buf[utf16_len] = 'x';
    utf16_len += 1;
    utf16_buf[utf16_len] = 'i';
    utf16_len += 1;
    utf16_buf[utf16_len] = 's';
    utf16_len += 1;
    utf16_buf[utf16_len] = 't';
    utf16_len += 1;
    utf16_buf[utf16_len] = 0; // Null-terminate

    // Create the directory first using makeDirAbsoluteW
    try std.fs.makeDirAbsoluteW(utf16_buf[0..utf16_len :0]);

    // Try creating it again with our function - should not error
    try makeDirRecursiveW(utf16_buf[0..utf16_len :0]);

    // Convert back to UTF-8 for verification
    var utf8_verify_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    var i: usize = 0;
    while (i < utf16_len) : (i += 1) {
        utf8_verify_buf[i] = @intCast(utf16_buf[i]);
    }
    utf8_verify_buf[utf16_len] = 0;

    // Verify the directory was created
    var dir = try std.fs.openDirAbsoluteZ(utf8_verify_buf[0..utf16_len :0], .{});
    defer dir.close();
}

test "makeDirRecursiveW - partial existing path" {
    // Skip this test on non-Windows platforms
    if (@import("builtin").os.tag != .windows) return error.SkipZigTest;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_dir = try tmp_dir.dir.realpath(".", &abs_path_buf);

    // Convert the base directory path to UTF-16
    var utf16_buf: [std.fs.max_path_bytes:0]u16 = undefined;
    var utf16_len: usize = 0;

    for (base_dir) |byte| {
        utf16_buf[utf16_len] = byte;
        utf16_len += 1;
    }

    // Create the middle path: base_dir\partial\path
    var mid_path_len = utf16_len;
    utf16_buf[mid_path_len] = std.fs.path.sep_windows;
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'p';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'a';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'r';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 't';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'i';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'a';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'l';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = std.fs.path.sep_windows;
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'p';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'a';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 't';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 'h';
    mid_path_len += 1;
    utf16_buf[mid_path_len] = 0; // Null-terminate

    try makeDirRecursiveW(utf16_buf[0..mid_path_len :0]);

    // Create a deeper path: base_dir\partial\path\deeper\directory
    var deep_path_len = mid_path_len;
    utf16_buf[deep_path_len - 1] = std.fs.path.sep_windows; // Replace the null terminator with a separator
    deep_path_len += 0; // No need to increment since we're replacing the null
    utf16_buf[deep_path_len] = 'd';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'e';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'e';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'p';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'e';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'r';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = std.fs.path.sep_windows;
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'd';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'i';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'r';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'e';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'c';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 't';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'o';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'r';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 'y';
    deep_path_len += 1;
    utf16_buf[deep_path_len] = 0; // Null-terminate

    try makeDirRecursiveW(utf16_buf[0..deep_path_len :0]);

    // Convert back to UTF-8 for verification
    var utf8_verify_buf: [std.fs.max_path_bytes:0]u8 = undefined;
    var i: usize = 0;
    while (i < deep_path_len) : (i += 1) {
        utf8_verify_buf[i] = @intCast(utf16_buf[i]);
    }
    utf8_verify_buf[deep_path_len] = 0;

    // Verify the deepest directory was created
    var deepest_dir = try std.fs.openDirAbsoluteZ(utf8_verify_buf[0..deep_path_len :0], .{});
    defer deepest_dir.close();
}
