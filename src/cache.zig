// Generally Questions
// How will the compiler/roc symlink be set?
// Spec here https://github.com/roc-lang/roc/issues/7517
const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");

const Package = base.Package;

/// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []const u8, roc_version: []const u8) ?canonicalize.IR {
    _ = file_hash;
    _ = roc_version;

    return null;
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator) []const u8 {
    _ = url_data;
    _ = gpa;

    @panic("not implemented");
}

pub const CacheFileQuery = union(enum) {
    compiler: Compiler,
    build,
    packages_src,
    packages_build,

    pub const Compiler = struct {
        version_name: []const u8,
    };
};

pub const CacheFileData = union(enum) {
    compiler: Compiler,
    build,
    packages_src,
    packages_build,

    pub const Compiler = struct {
        version_name: []const u8,
        binary: []const u8,
    };
};

const target_os = builtin.target.os.tag;
fn setEnvVar(allocator: Allocator, name: []const u8, value: []const u8) !void {
    // SKILL_ISSUE: it's required to use -lc to run the tests because of the
    // c library calls. Is there a better way to run a single test file?
    // zig test ./src/cache.zig -lc
    const c_str = try allocator.dupeZ(u8, value);
    defer allocator.free(c_str);

    return {
        switch (target_os) {
            .macos, .linux => {
                const c = @cImport({
                    @cInclude("stdlib.h");
                });
                if (c.setenv(name.ptr, c_str.ptr, 1) != 0) {
                    return error.SetEnvFailed;
                }
            },
            //.windows => {
            // TODO: figure out windows implementation
            //const c = @cImport({
            //@cInclude("windows.h");
            //});
            //if (c.SetEnvironmentVariableA(name.ptr, value.ptr) == 0) {
            //return error.SetEnvFailed;
            //}
            //},
            else => {
                @compileError("\"" ++ @tagName(target_os) ++ "\" is not supported in cache.zig");
            },
        }
    };
}

pub fn unsetEnvVar(name: []const u8) !void {
    // SKILL_ISSUE: it's required to use -lc to run the tests because of the
    // c library calls. Is there a better way to run a single test file?
    // zig test ./src/cache.zig -lc
    return {
        switch (target_os) {
            .macos, .linux => {
                const c = @cImport({
                    @cInclude("stdlib.h");
                });
                if (c.unsetenv(name.ptr) != 0) {
                    return error.UnsetEnvFailed;
                }
            },
            // FIX_FOR_WINDOWS:
            //.windows => {
            // TODO: figure out windows implementation
            //const c = @cImport({
            //@cInclude("windows.h");
            //});
            //if (c.SetEnvironmentVariableA(name.ptr, value.ptr) == 0) {
            //return error.SetEnvFailed;
            //}
            //},
            else => {
                @compileError("\"" ++ @tagName(target_os) ++ "\" is not supported in cache.zig");
            },
        }
    };
}

fn getDefaultCacheFolder(allocator: Allocator) ![]u8 {
    return {
        switch (target_os) {
            .macos, .linux => {
                const home_dir = try std.process.getEnvVarOwned(allocator, "HOME");
                defer allocator.free(home_dir);
                const default_cache_folder_parts = [_][]const u8{
                    home_dir,
                    ".cache",
                };
                const default_cache_folder = try std.fs.path.join(allocator, &default_cache_folder_parts);
                return default_cache_folder;
            },
            .windows => {
                const default_cache_folder_parts = [_][]const u8{"%APPDATA%"};
                const default_cache_folder = try std.fs.path.join(allocator, &default_cache_folder_parts);
                return default_cache_folder;
            },
            else => {
                @compileError("\"" ++ @tagName(target_os) ++ "\" is not supported in cache.zig");
            },
        }
    };
}

const xdg_cache_home_var_name = "XDG_CACHE_HOME";
fn getBaseRocCacheFolder(allocator: Allocator, sub_path: []const u8) ![]u8 {
    const xdg_cache_home = std.process.getEnvVarOwned(allocator, xdg_cache_home_var_name) catch null;
    defer {
        if (xdg_cache_home) |x| {
            allocator.free(x);
        }
    }
    const base_dir = if (xdg_cache_home) |x|
        try allocator.dupe(u8, x)
    else
        try getDefaultCacheFolder(allocator);

    defer allocator.free(base_dir);
    const roc_cache_parts = [_][]const u8{
        base_dir,
        "roc",
        sub_path,
    };
    return try std.fs.path.join(allocator, &roc_cache_parts);
}

fn getCompilerPath(allocator: Allocator, version_name: []const u8) ![]u8 {
    const sub_path_parts = [_][]const u8{ "compiler", version_name };
    const sub_path = try std.fs.path.join(allocator, &sub_path_parts);
    defer allocator.free(sub_path);
    return try getBaseRocCacheFolder(allocator, sub_path);
}

fn saveCacheFile(file_cache_path: []const u8, file_binary: []const u8) !void {
    const cwd = std.fs.cwd();
    const roc_cache_folder = std.fs.path.dirname(file_cache_path);
    if (roc_cache_folder) |x| {
        cwd.makePath(x) catch {
            return error.FailedToMakeCacheFolder;
        };
    }
    const file = cwd.createFile(file_cache_path, .{}) catch {
        return error.FailedToMakeCacheFile;
    };
    file.writeAll(file_binary) catch {
        return error.FailedToWriteToCacheFile;
    };
}

pub fn saveToCache(allocator: Allocator, roc_cache_file: CacheFileData) !void {
    const file_cache_path = switch (roc_cache_file) {
        .compiler => |value| try getCompilerPath(allocator, value.version_name),
        else => @panic("FIX ME!!!!!!"),
    };

    defer allocator.free(file_cache_path);

    const file_binary = switch (roc_cache_file) {
        .compiler => |value| value.binary,
        else => @panic("FIX ME!!!!!!"),
    };

    try saveCacheFile(file_cache_path, file_binary);
}

fn getCacheFile(allocator: Allocator, file_cache_path: []const u8) !?[]const u8 {
    const File = std.fs.File;
    const OpenError = File.OpenError;
    const file = std.fs.cwd().openFile(file_cache_path, .{ .mode = .read_only }) catch |e| {
        return switch (e) {
            OpenError.FileNotFound => null,
            OpenError.AccessDenied => error.AccessDeniedToCache,
            OpenError.NameTooLong => null,
            else => error.Unexpected,
        };
    };
    defer file.close();

    const GetSeekPosError = File.GetSeekPosError;
    const file_size = file.getEndPos() catch |e| {
        return switch (e) {
            GetSeekPosError.AccessDenied => error.AccessDeniedToCache,
            else => error.Unexpected,
        };
    };
    const buf = try allocator.alloc(u8, file_size);

    const ReadError = File.ReadError;
    _ = file.readAll(buf) catch |e| {
        return switch (e) {
            ReadError.AccessDenied => error.AccessDeniedToCache,
            else => error.Unexpected,
        };
    };
    return buf;
}

fn tryToGetCacheFile(allocator: Allocator, cache_file_query: CacheFileQuery) !?[]const u8 {
    const file_cache_path = switch (cache_file_query) {
        .compiler => |value| try getCompilerPath(allocator, value.version_name),
        else => @panic("FIX ME!!!!!!"),
    };
    defer allocator.free(file_cache_path);
    return try getCacheFile(allocator, file_cache_path);
}

fn create_test_cache_folder(allocator: Allocator) !testing.TmpDir {
    const tmp_dir = testing.tmpDir(.{});
    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);
    try setEnvVar(allocator, xdg_cache_home_var_name, path);
    return tmp_dir;
}

fn get_home_directory(allocator: Allocator) ![]u8 {
    return {
        switch (target_os) {
            .macos, .linux => {
                return try std.process.getEnvVarOwned(allocator, "HOME");
            },
            .windows => {
                return "%APPDATA%";
            },
            else => {
                @compileError("\"" ++ @tagName(target_os) ++ "\" is not supported in cache.zig");
            },
        }
    };
}

// SKILL_ISSUE: this seems like a really hack way to check if a file exists
fn file_exists(path: []const u8) bool {
    var fileExists = true;
    std.fs.cwd().access(path, .{}) catch |err| {
        fileExists = if (err == error.FileNotFound) false else true;
    };
    return fileExists;
}

test "Should save to XDG_CACHE_HOME if it is set for compiler cache" {
    const allocator = testing.allocator;
    var tmp_dir = try create_test_cache_folder(allocator);
    defer tmp_dir.cleanup();
    defer {
        unsetEnvVar(xdg_cache_home_var_name) catch {
            @panic("failed to unset " ++ xdg_cache_home_var_name);
        };
    }
    const cache_file = CacheFileData.Compiler{
        .version_name = "1.0.0",
        .binary = "A roc compiler binary",
    };
    try saveToCache(allocator, CacheFileData{ .compiler = cache_file });

    const binary = try tryToGetCacheFile(allocator, CacheFileQuery{ .compiler = CacheFileQuery.Compiler{
        .version_name = cache_file.version_name,
    } }) orelse @panic("Binary file should exist");
    defer allocator.free(binary);

    try testing.expectEqual(cache_file.binary.len, binary.len);
    try testing.expectEqualStrings(cache_file.binary, binary);

    const tmp_dir_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_dir_path);
    const expected_cache_dir = try std.fs.path.join(allocator, &[_][]const u8{
        tmp_dir_path,
        "roc",
        "compiler",
        cache_file.version_name,
    });
    defer allocator.free(expected_cache_dir);

    try testing.expect(file_exists(expected_cache_dir));
}

test "Should get correct base cache folder" {
    const allocator = testing.allocator;
    const home_dir = try get_home_directory(allocator);
    defer allocator.free(home_dir);

    const actual_cache_dir = try getBaseRocCacheFolder(allocator, "compiler");
    defer allocator.free(actual_cache_dir);
    const expected_cache_dir = try std.fs.path.join(allocator, &[_][]const u8{
        home_dir,
        ".cache",
        "roc",
        "compiler",
    });
    defer allocator.free(expected_cache_dir);

    try testing.expectEqualStrings(expected_cache_dir, actual_cache_dir);
}

test "Should return if null there is no cache file" {
    const allocator = testing.allocator;
    const home_dir = try get_home_directory(allocator);
    defer allocator.free(home_dir);
    const fake_path = try std.fs.path.join(allocator, &[_][]const u8{
        home_dir,
        "fake-cache",
    });
    defer allocator.free(fake_path);
    try setEnvVar(allocator, xdg_cache_home_var_name, fake_path);
    const cache_file_query = CacheFileQuery.Compiler{
        .version_name = "1.0.0",
    };
    const binary = try tryToGetCacheFile(allocator, CacheFileQuery{ .compiler = cache_file_query });
    if (binary) |b| {
        defer allocator.free(b);
    }

    try testing.expectEqual(null, binary);
}
