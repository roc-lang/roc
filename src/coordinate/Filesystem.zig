//! Abstract filesystem functions so we can mock them out for testing
//! and also provide an alternative implementation for WASM (webREPL, playground).

const std = @import("std");
const collections = @import("../collections.zig");

const Allocator = std.mem.Allocator;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

fileExists: *const fn (absolute_path: []const u8) OpenError!bool,
readFile: *const fn (relative_path: []const u8, allocator: Allocator) ReadError![]const u8,
openDir: *const fn (absolute_path: []const u8) OpenError!Dir,
dirName: *const fn (absolute_path: []const u8) ?[]const u8,
baseName: *const fn (absolute_path: []const u8) ?[]const u8,
canonicalize: *const fn (relative_path: []const u8, allocator: Allocator) CanonicalizeError![]const u8,

/// todo
pub fn default() Self {
    return Self{
        .fileExists = &fileExists,
        .readFile = &readFile,
        .openDir = &openDir,
        .dirName = &dirName,
        .baseName = &baseName,
        .canonicalize = &canonicalize,
    };
}

/// todo
pub const ReadError = std.fs.File.OpenError || std.posix.ReadError || Allocator.Error || error{StreamTooLong};

/// todo
pub const OpenError = std.fs.File.OpenError || std.fs.Dir.AccessError;

/// todo
pub const CanonicalizeError = error{ FileNotFound, Unknown, OutOfMemory } || std.posix.RealPathError;

/// todo
pub const Dir = struct {
    dir: std.fs.Dir,

    /// todo
    pub const Entry = std.fs.Dir.Entry;

    const openOptions = std.fs.Dir.OpenDirOptions{
        .access_sub_paths = true,
        .iterate = true,
        // packages should have no symlinks, so don't follow them for better security!
        // (prevents reading files outside the package's tree)
        .no_follow = true,
    };

    /// todo
    pub fn openParent(dir: *Dir) OpenError!?Dir {
        return dir.dir.openDir("..", openOptions) catch |err| {
            switch (err) {
                error.FileNotFound => return false,
                else => return err,
            }
        };
    }

    /// todo
    pub fn hasFile(dir: *Dir, filename: []const u8) !bool {
        dir.dir.access(filename, .{}) catch |err| {
            switch (err) {
                error.FileNotFound => return false,
                else => return err,
            }
        };

        return true;
    }

    /// todo
    pub fn canonicalize(dir: *Dir, filename: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
        return dir.dir.realpathAlloc(allocator, filename) catch |err| {
            switch (err) {
                error.OutOfMemory => exitOnOom(error.OutOfMemory),
                else => return err,
            }
        };
    }

    /// todo
    pub fn close(dir: *Dir) void {
        dir.dir.close();
    }

    /// todo
    pub fn findAllFilesRecursively(
        dir: *Dir,
        gpa: std.mem.Allocator,
        string_arena: *std.heap.ArenaAllocator,
    ) !std.ArrayListUnmanaged([]const u8) {
        var files = std.ArrayListUnmanaged([]const u8){};
        errdefer files.deinit(gpa);

        var walker = dir.dir.walk(gpa) catch |err| exitOnOom(err);
        while (try walker.next()) |entry| {
            switch (entry.kind) {
                .file => {
                    const path = std.mem.sliceTo(entry.path, 0);
                    const relative_path = string_arena.allocator().dupe(u8, path) catch |err| exitOnOom(err);

                    files.append(gpa, relative_path) catch |err| exitOnOom(err);
                },
                else => {
                    // do nothing
                },
            }
        }

        return files;
    }
};

fn fileExists(absolute_path: []const u8) OpenError!bool {
    std.fs.accessAbsolute(absolute_path, .{}) catch |err| {
        switch (err) {
            error.FileNotFound => return false,
            else => return err,
        }
    };

    return true;
}

/// Reads the contents of a file at the given relative path.
fn readFile(relative_path: []const u8, allocator: std.mem.Allocator) ReadError![]const u8 {
    const file = try std.fs.cwd().openFile(relative_path, .{});
    defer file.close();

    const max_allowed_file_length = std.math.maxInt(usize);
    const contents = try file.reader().readAllAlloc(allocator, max_allowed_file_length);

    return contents;
}

fn openDir(absolute_path: []const u8) OpenError!Dir {
    const dir = std.fs.openDirAbsolute(absolute_path, Dir.openOptions) catch |err| {
        return switch (err) {
            error.FileNotFound => error.FileNotFound,
            else => return err,
        };
    };

    return Dir{ .dir = dir };
}

fn dirName(absolute_path: []const u8) ?[]const u8 {
    return std.fs.path.dirname(absolute_path);
}

fn baseName(absolute_path: []const u8) ?[]const u8 {
    return std.fs.path.basename(absolute_path);
}

fn canonicalize(root_relative_path: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
    return std.fs.realpathAlloc(allocator, root_relative_path) catch |err| {
        return switch (err) {
            error.FileNotFound => error.FileNotFound,
            else => error.Unknown,
        };
    };
}
