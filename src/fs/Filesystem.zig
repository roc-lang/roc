//! Abstract filesystem functions so we can mock them out for testing
//! and also provide an alternative implementation for WASM (webREPL, playground).

const std = @import("std");
const collections = @import("../collections.zig");

const Allocator = std.mem.Allocator;

const Self = @This();

fileExists: *const fn (absolute_path: []const u8) OpenError!bool,
readFile: *const fn (relative_path: []const u8, allocator: Allocator) ReadError![]const u8,
readFileInto: *const fn (path: []const u8, buffer: []u8) ReadError!usize,
writeFile: *const fn (path: []const u8, contents: []const u8) WriteError!void,
openDir: *const fn (absolute_path: []const u8) OpenError!Dir,
dirName: *const fn (absolute_path: []const u8) ?[]const u8,
baseName: *const fn (absolute_path: []const u8) ?[]const u8,
canonicalize: *const fn (relative_path: []const u8, allocator: Allocator) CanonicalizeError![]const u8,
makePath: *const fn (path: []const u8) MakePathError!void,
rename: *const fn (old_path: []const u8, new_path: []const u8) RenameError!void,
getFileInfo: *const fn (path: []const u8) GetFileInfoError!FileInfo,

// TODO: replace this with a method that gets the right
// filesystem manager for the current context.
//
/// Get the default filesystem manager.
pub fn default() Self {
    return Self{
        .fileExists = &fileExistsDefault,
        .readFile = &readFileDefault,
        .readFileInto = &readFileIntoDefault,
        .writeFile = &writeFileDefault,
        .openDir = &openDirDefault,
        .dirName = &dirNameDefault,
        .baseName = &baseNameDefault,
        .canonicalize = &canonicalizeDefault,
        .makePath = &makePathDefault,
        .rename = &renameDefault,
        .getFileInfo = &getFileInfoDefault,
    };
}

/// Get a testing filesystem manager where all functions will
/// cause the test to fail if called. Can be used to create test-specific
/// mocks by overriding only the functions you expect to be called.
pub fn testing() Self {
    return Self{
        .fileExists = &fileExistsTesting,
        .readFile = &readFileTesting,
        .readFileInto = &readFileIntoTesting,
        .writeFile = &writeFileTesting,
        .openDir = &openDirTesting,
        .dirName = &dirNameTesting,
        .baseName = &baseNameTesting,
        .canonicalize = &canonicalizeTesting,
        .makePath = &makePathTesting,
        .rename = &renameTesting,
        .getFileInfo = &getFileInfoTesting,
    };
}

/// The max valid file size.
/// Anything larger will fail due to us using u32 offsets.
pub const max_file_size = std.math.maxInt(u32);

/// All errors that can occur when recursively creating directories.
pub const MakePathError = std.fs.Dir.MakeError || std.fs.Dir.StatFileError;

/// All errors that can occur when reading a file.
pub const ReadError = std.fs.File.OpenError || std.posix.ReadError || Allocator.Error || error{StreamTooLong};

/// All errors that can occur when writing a file.
pub const WriteError = std.fs.File.OpenError || std.fs.File.WriteError || error{SystemResources};

/// All errors that can occur when opening a file or directory.
pub const OpenError = std.fs.File.OpenError || std.fs.Dir.AccessError;

/// All errors that can occur when canonicalizing a filepath.
pub const CanonicalizeError = error{ FileNotFound, Unknown, OutOfMemory } || std.posix.RealPathError;

/// All errors that can occur when renaming a file.
pub const RenameError = std.fs.Dir.RenameError || std.posix.RenameError;

/// All errors that can occur when getting file information.
pub const GetFileInfoError = std.fs.File.OpenError || std.fs.File.StatError;

/// File information structure containing metadata.
pub const FileInfo = struct {
    /// File modification time in nanoseconds since Unix epoch
    mtime_ns: i128,
    /// File size in bytes
    size: u64,
};

/// An abstracted directory handle.
pub const Dir = struct {
    dir: std.fs.Dir,

    const openOptions = std.fs.Dir.OpenDirOptions{
        .access_sub_paths = true,
        .iterate = true,
        // packages should have no symlinks, so don't follow them for better security!
        // (prevents reading files outside the package's tree)
        .no_follow = true,
    };

    /// Attempt to open the parent directory of this directory.
    pub fn openParent(dir: *Dir) OpenError!?Dir {
        return dir.dir.openDir("..", openOptions) catch |err| {
            switch (err) {
                error.FileNotFound => return false,
                else => return err,
            }
        };
    }

    /// Check if this directory has a file with the given name.
    pub fn hasFile(dir: *Dir, filename: []const u8) !bool {
        dir.dir.access(filename, .{}) catch |err| {
            switch (err) {
                error.FileNotFound => return false,
                else => return err,
            }
        };

        return true;
    }

    /// Canonicalize the given filepath relative to this dir's path.
    pub fn canonicalize(dir: *Dir, filename: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
        if (comptime @import("builtin").target.os.tag == .wasi) {
            // WASI doesn't support realpath, so we'll just resolve the path
            // without following symlinks
            return std.fs.path.resolve(allocator, &.{filename}) catch |err| {
                switch (err) {
                    error.OutOfMemory => error.OutOfMemory,
                    else => return err,
                }
            };
        } else {
            return dir.dir.realpathAlloc(allocator, filename) catch |err| {
                switch (err) {
                    error.OutOfMemory => error.OutOfMemory,
                    else => return err,
                }
            };
        }
    }

    /// Close this directory.
    pub fn close(dir: *Dir) void {
        dir.dir.close();
    }

    /// Find all filepaths in this directory recursively.
    ///
    /// The text of the relative paths are stored in the `string_arena`
    /// and the slices over said paths are returned in an `ArrayListUnmanaged`
    /// that must be `deinit`ed by the caller.
    pub fn findAllFilesRecursively(
        dir: *Dir,
        gpa: std.mem.Allocator,
        string_arena: *std.heap.ArenaAllocator,
    ) !std.ArrayListUnmanaged([]const u8) {
        var files = std.ArrayListUnmanaged([]const u8){};
        errdefer files.deinit(gpa);

        var walker = try dir.dir.walk(gpa);
        while (try walker.next()) |entry| {
            switch (entry.kind) {
                .file => {
                    const path = std.mem.sliceTo(entry.path, 0);
                    const relative_path = try string_arena.allocator().dupe(u8, path);

                    try files.append(gpa, relative_path);
                },
                else => {
                    // do nothing
                },
            }
        }

        return files;
    }
};

// Default implementations

fn fileExistsDefault(absolute_path: []const u8) OpenError!bool {
    std.fs.accessAbsolute(absolute_path, .{}) catch |err| {
        switch (err) {
            error.FileNotFound => return false,
            else => return err,
        }
    };

    return true;
}

/// Reads the contents of a file at the given relative path.
fn readFileDefault(relative_path: []const u8, allocator: std.mem.Allocator) ReadError![]const u8 {
    const file = try std.fs.cwd().openFile(relative_path, .{});
    defer file.close();

    const contents = try file.reader().readAllAlloc(allocator, max_file_size);

    return contents;
}

/// Reads the contents of a file at the given path into a pre-allocated buffer.
/// Returns the number of bytes read.
fn readFileIntoDefault(path: []const u8, buffer: []u8) ReadError!usize {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.reader().readAll(buffer);
}

fn openDirDefault(absolute_path: []const u8) OpenError!Dir {
    const dir = std.fs.openDirAbsolute(absolute_path, Dir.openOptions) catch |err| {
        return switch (err) {
            error.FileNotFound => error.FileNotFound,
            else => return err,
        };
    };

    return Dir{ .dir = dir };
}

fn dirNameDefault(absolute_path: []const u8) ?[]const u8 {
    return std.fs.path.dirname(absolute_path);
}

fn baseNameDefault(absolute_path: []const u8) ?[]const u8 {
    return std.fs.path.basename(absolute_path);
}

fn canonicalizeDefault(root_relative_path: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
    if (comptime @import("builtin").target.os.tag == .wasi) {
        // WASI doesn't support realpath, so we'll just resolve the path
        // without following symlinks
        return std.fs.path.resolve(allocator, &.{root_relative_path}) catch |err| {
            return switch (err) {
                error.OutOfMemory => error.OutOfMemory,
            };
        };
    } else {
        return std.fs.realpathAlloc(allocator, root_relative_path) catch |err| {
            return switch (err) {
                error.FileNotFound => error.FileNotFound,
                else => error.Unknown,
            };
        };
    }
}

/// Creates a directory and all its parent directories recursively, similar to `mkdir -p`
fn makePathDefault(path: []const u8) MakePathError!void {
    try std.fs.cwd().makePath(path);
}

/// Renames a file or directory from old_path to new_path.
fn renameDefault(old_path: []const u8, new_path: []const u8) RenameError!void {
    try std.fs.cwd().rename(old_path, new_path);
}

/// Gets file information including modification time and size.
fn getFileInfoDefault(path: []const u8) GetFileInfoError!FileInfo {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const stat = try file.stat();

    return FileInfo{
        .mtime_ns = stat.mtime,
        .size = stat.size,
    };
}

/// Writes contents to a file at the given path.
/// Creates the file if it doesn't exist or truncates it if it does.
fn writeFileDefault(path: []const u8, contents: []const u8) WriteError!void {
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    try file.writeAll(contents);
}

// Testing implementations that fail tests if called

fn fileExistsTesting(absolute_path: []const u8) OpenError!bool {
    _ = absolute_path;
    @panic("fileExists should not be called in this test");
}

fn readFileTesting(relative_path: []const u8, allocator: Allocator) ReadError![]const u8 {
    _ = relative_path;
    _ = allocator;
    @panic("readFile should not be called in this test");
}

fn readFileIntoTesting(path: []const u8, buffer: []u8) ReadError!usize {
    _ = path;
    _ = buffer;
    @panic("readFileInto should not be called in this test");
}

fn writeFileTesting(path: []const u8, contents: []const u8) WriteError!void {
    _ = path;
    _ = contents;
    @panic("writeFile should not be called in this test");
}

fn openDirTesting(absolute_path: []const u8) OpenError!Dir {
    _ = absolute_path;
    @panic("openDir should not be called in this test");
}

fn dirNameTesting(absolute_path: []const u8) ?[]const u8 {
    _ = absolute_path;
    @panic("dirName should not be called in this test");
}

fn baseNameTesting(absolute_path: []const u8) ?[]const u8 {
    _ = absolute_path;
    @panic("baseName should not be called in this test");
}

fn canonicalizeTesting(root_relative_path: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
    _ = root_relative_path;
    _ = allocator;
    @panic("canonicalize should not be called in this test");
}

fn makePathTesting(path: []const u8) MakePathError!void {
    _ = path;
    @panic("makePath should not be called in this test");
}

fn renameTesting(old_path: []const u8, new_path: []const u8) RenameError!void {
    _ = old_path;
    _ = new_path;
    @panic("rename should not be called in this test");
}

fn getFileInfoTesting(path: []const u8) GetFileInfoError!FileInfo {
    // Return deterministic file info for testing
    // Hash the path to get consistent results
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(path);
    const hash = hasher.finalResult();

    const mtime_ns = @as(i128, @bitCast(@as(u128, @bitCast(hash[0..16].*)) & 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF));
    const size = @as(u64, @bitCast(hash[16..24].*)) & 0xFFFFFF; // Limit size for testing

    return FileInfo{
        .mtime_ns = mtime_ns,
        .size = size,
    };
}
