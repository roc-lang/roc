//! WASM-specific filesystem implementation for the playground.
//! This provides a minimal filesystem interface where source code
//! can be provided from JavaScript and most other operations return errors.

const std = @import("std");
const fs_mod = @import("fs");
const Filesystem = fs_mod.Filesystem;
const collections = @import("collections");

const Allocator = std.mem.Allocator;
// Note: OOM errors are now propagated to callers

/// Global storage for source code provided from JavaScript
pub var global_source: ?[]const u8 = null;
/// Global allocator provided from JavaScript
pub var global_allocator: ?Allocator = null;

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
    // Note: In WASM context, we can't easily propagate errors from this function
    // If allocation fails, we keep the previous source
    global_source = allocator.dupe(u8, source) catch {
        // Keep previous source on allocation failure
        return;
    };
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

fn fileExistsWasm(absolute_path: []const u8) Filesystem.OpenError!bool {
    // For the playground, we assume the main source file always exists
    // and all other files don't exist
    return std.mem.eql(u8, absolute_path, "main.roc") or
        std.mem.eql(u8, absolute_path, "/main.roc") or
        std.mem.endsWith(u8, absolute_path, "/main.roc");
}

fn readFileWasm(relative_path: []const u8, allocator: Allocator) Filesystem.ReadError![]const u8 {
    // Only support reading the main source file
    if (std.mem.eql(u8, relative_path, "main.roc") or
        std.mem.eql(u8, relative_path, "/main.roc") or
        std.mem.endsWith(u8, relative_path, "/main.roc"))
    {
        if (global_source) |source| {
            return allocator.dupe(u8, source) catch error.OutOfMemory;
        } else {
            return error.FileNotFound;
        }
    }

    return error.FileNotFound;
}

fn readFileIntoWasm(path: []const u8, buffer: []u8) Filesystem.ReadError!usize {
    // Only support reading the main source file
    if (std.mem.eql(u8, path, "main.roc") or
        std.mem.eql(u8, path, "/main.roc") or
        std.mem.endsWith(u8, path, "/main.roc"))
    {
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

fn writeFileWasm(path: []const u8, contents: []const u8) Filesystem.WriteError!void {
    _ = path;
    _ = contents;
    // Writing files is not supported in WASM playground
    return error.AccessDenied;
}

fn openDirWasm(absolute_path: []const u8) Filesystem.OpenError!Filesystem.Dir {
    _ = absolute_path;
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
    return allocator.dupe(u8, root_relative_path) catch error.OutOfMemory;
}

fn makePathWasm(path: []const u8) Filesystem.MakePathError!void {
    _ = path;
    // Directory creation is not supported in WASM playground
    return error.AccessDenied;
}

fn renameWasm(old_path: []const u8, new_path: []const u8) Filesystem.RenameError!void {
    _ = old_path;
    _ = new_path;
    // File operations are not supported in WASM playground
    return error.AccessDenied;
}

fn getFileInfoWasm(path: []const u8) Filesystem.GetFileInfoError!Filesystem.FileInfo {
    // Only support getting info for the main source file
    if (std.mem.eql(u8, path, "main.roc") or
        std.mem.eql(u8, path, "/main.roc") or
        std.mem.endsWith(u8, path, "/main.roc"))
    {
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
