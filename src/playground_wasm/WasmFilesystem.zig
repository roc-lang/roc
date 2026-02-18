//! WASM-specific filesystem implementation for the playground.
//! This provides a minimal filesystem interface where source code
//! can be provided from JavaScript and most other operations return errors.

const std = @import("std");
const fs_mod = @import("fs");
const Filesystem = fs_mod.Filesystem;

const Allocator = std.mem.Allocator;
// Helper function to handle OOM errors
fn handleOom() noreturn {
    @panic("Out of memory");
}

/// Global storage for source code provided from JavaScript
pub var global_source: ?[]const u8 = null;
/// Global allocator provided from JavaScript
pub var global_allocator: ?Allocator = null;
/// Global filename for the source file (defaults to "main.roc")
pub var global_filename: []const u8 = "main.roc";

/// Set the source code that will be returned by readFile.
/// This is called from JavaScript via WASM exports.
pub fn setSource(allocator: Allocator, source: []const u8) void {
    // Free previous source if it exists
    if (global_source) |prev_source| {
        if (global_allocator) |prev_allocator| {
            prev_allocator.free(prev_source);
        }
    }

    // Store new source
    global_source = allocator.dupe(u8, source) catch handleOom();
    global_allocator = allocator;
}

/// Set the filename for the source file.
/// This affects which file paths are recognized as the source file.
pub fn setFilename(allocator: Allocator, filename: []const u8) void {
    // Free previous filename if it was allocated (not the default)
    if (global_allocator) |prev_allocator| {
        if (global_filename.ptr != "main.roc".ptr) {
            prev_allocator.free(global_filename);
        }
    }

    // Store new filename
    global_filename = allocator.dupe(u8, filename) catch handleOom();
    global_allocator = allocator;
}

/// Get a WASM filesystem implementation
pub fn wasm() Filesystem {
    return Filesystem{
        .fileExists = &fileExistsWasm,
        .readFile = &readFileWasm,
        .readFileInto = &readFileIntoWasm,
        .writeFile = &writeFileWasm,
        .openDir = &openDirWasm,
        .dirName = &dirNameWasm,
        .baseName = &baseNameWasm,
        .canonicalize = &canonicalizeWasm,
        .makePath = &makePathWasm,
        .rename = &renameWasm,
        .getFileInfo = &getFileInfoWasm,
    };
}

/// Check if the given path matches the current source filename
fn matchesSourceFile(path: []const u8) bool {
    // Check exact match
    if (std.mem.eql(u8, path, global_filename)) return true;
    // Check with leading slash
    if (path.len > 0 and path[0] == '/') {
        if (std.mem.eql(u8, path[1..], global_filename)) return true;
    }
    // Check if path ends with /filename
    if (std.mem.endsWith(u8, path, global_filename)) {
        const prefix_len = path.len - global_filename.len;
        if (prefix_len > 0 and path[prefix_len - 1] == '/') return true;
    }
    return false;
}

fn fileExistsWasm(absolute_path: []const u8) Filesystem.OpenError!bool {
    // For the playground, we assume the source file always exists
    // and all other files don't exist
    return matchesSourceFile(absolute_path);
}

fn readFileWasm(relative_path: []const u8, allocator: Allocator) Filesystem.ReadError![]const u8 {
    // Only support reading the source file
    if (matchesSourceFile(relative_path)) {
        if (global_source) |source| {
            return allocator.dupe(u8, source) catch handleOom();
        } else {
            return error.FileNotFound;
        }
    }

    return error.FileNotFound;
}

fn readFileIntoWasm(path: []const u8, buffer: []u8) Filesystem.ReadError!usize {
    // Only support reading the source file
    if (matchesSourceFile(path)) {
        if (global_source) |source| {
            if (buffer.len < source.len) {
                return error.StreamTooLong;
            }
            @memcpy(buffer[0..source.len], source);
            return source.len;
        } else {
            return error.FileNotFound;
        }
    }

    return error.FileNotFound;
}

fn writeFileWasm(_: []const u8, _: []const u8) Filesystem.WriteError!void {
    // Writing files is not supported in WASM playground
    return error.AccessDenied;
}

fn openDirWasm(_: []const u8) Filesystem.OpenError!Filesystem.Dir {
    // Directory operations are not supported in WASM playground
    return error.FileNotFound;
}

fn dirNameWasm(absolute_path: []const u8) ?[]const u8 {
    // Simple implementation without std.fs.path to avoid posix dependencies
    if (std.mem.lastIndexOfScalar(u8, absolute_path, '/')) |last_slash| {
        if (last_slash == 0) {
            return "/";
        }
        return absolute_path[0..last_slash];
    }
    return null;
}

fn baseNameWasm(absolute_path: []const u8) ?[]const u8 {
    // Simple implementation without std.fs.path to avoid posix dependencies
    if (std.mem.lastIndexOfScalar(u8, absolute_path, '/')) |last_slash| {
        return absolute_path[last_slash + 1 ..];
    }
    return absolute_path;
}

fn canonicalizeWasm(root_relative_path: []const u8, allocator: Allocator) Filesystem.CanonicalizeError![]const u8 {
    // For WASM, just return a clean version of the path
    return allocator.dupe(u8, root_relative_path) catch handleOom();
}

fn makePathWasm(_: []const u8) Filesystem.MakePathError!void {
    // Directory creation is not supported in WASM playground
    return error.AccessDenied;
}

fn renameWasm(_: []const u8, _: []const u8) Filesystem.RenameError!void {
    // File operations are not supported in WASM playground
    return error.AccessDenied;
}

fn getFileInfoWasm(path: []const u8) Filesystem.GetFileInfoError!Filesystem.FileInfo {
    // Only support getting info for the source file
    if (matchesSourceFile(path)) {
        if (global_source) |source| {
            return Filesystem.FileInfo{
                .mtime_ns = 0, // No real timestamps in WASM
                .size = source.len,
            };
        } else {
            return error.FileNotFound;
        }
    }

    return error.FileNotFound;
}
