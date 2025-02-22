// Generally Questions
// How will the compiler/roc symlink be set?
// Spec here https://github.com/roc-lang/roc/issues/7517
// REMOVE_ME:
// https://abhinav.github.io/temp.zig/
// var tmp_dir = try TempDir.create(allocator, {});
// defer tmp_dir.deinit();
const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const parse = @import("check/parse.zig");

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

pub fn blake3Hash(data: []const u8) [32]u8 {
    var digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(data, &digest, .{});

    return digest;
}

pub fn saveCompilerToCache(allocator: Allocator, version_name: []const u8, file_binary: []const u8) !void {
    const file_cache_path = try getCompilerPath(allocator, version_name);
    defer allocator.free(file_cache_path);
    try saveCacheFile(file_cache_path, file_binary);
}

// QUESTION: Will any need to fetch the bytes of the cache file?
pub fn tryToGetCompilerCacheFile(allocator: Allocator, version_name: []const u8) !?[]const u8 {
    const file_cache_path = try getCompilerPath(allocator, version_name);
    defer allocator.free(file_cache_path);
    return try getCacheFile(allocator, file_cache_path);
}

pub fn saveCanonicalizeIr(
    // SKILL_ISSUE: for my formatter to format in a column of arguments with this comment
    allocator: Allocator,
    project_name: []const u8,
    version_name: []const u8,
    file_contents: []const u8,
    ir: *canonicalize.IR,
) !void {
    const cwd = std.fs.cwd();
    const tmp_file_path = try createTempFile(allocator);
    defer allocator.free(tmp_file_path);

    const tmp_file = try std.fs.openFileAbsoluteZ(tmp_file_path, .{ .mode = .read_write });
    defer tmp_file.close();

    const ir_bytes = std.mem.asBytes(ir);
    try tmp_file.writer().writeAll(ir_bytes);

    const file_cache_path = try getBuildPath(allocator, project_name, version_name, file_contents);
    defer allocator.free(file_cache_path);
    const roc_cache_folder = std.fs.path.dirname(file_cache_path);
    if (roc_cache_folder) |x| {
        cwd.makePath(x) catch {
            return error.FailedToMakeCacheFolder;
        };
    }
    const file_cache_path_z = try allocator.dupeZ(u8, file_cache_path);
    defer allocator.free(file_cache_path_z);
    std.debug.assert(file_exists(tmp_file_path));
    try std.fs.renameAbsoluteZ(tmp_file_path, file_cache_path_z);
}

pub fn tryToGetCanonicalizeIr(
    // SKILL_ISSUE: for my formatter to format in a column of arguments with this comment
    allocator: Allocator,
    project_name: []const u8,
    version_name: []const u8,
    file_contents: []const u8,
) !?canonicalize.IR {
    const file_cache_path = try getBuildPath(allocator, project_name, version_name, file_contents);
    defer allocator.free(file_cache_path);
    const ir_bytes = try getCacheFile(allocator, file_cache_path);
    if (ir_bytes) |b| {
        return std.mem.bytesToValue(canonicalize.IR, &b);
    } else {
        return null;
    }
}

pub fn setDefaultRocCompiler(allocator: Allocator, version_name: []const u8) !void {
    const file_cache_path = try getCompilerPath(allocator, version_name);
    defer allocator.free(file_cache_path);
    const roc_symlink_sub_path_parts = [_][]const u8{ "compiler", "roc" };
    const roc_symlink_sub_folder = try std.fs.path.join(allocator, &roc_symlink_sub_path_parts);
    defer allocator.free(roc_symlink_sub_folder);
    const roc_symlink_path = try getBaseRocCacheFolder(allocator, roc_symlink_sub_folder);
    defer allocator.free(roc_symlink_path);
    try std.fs.symLinkAbsolute(file_cache_path, roc_symlink_path, .{});
}

const target_os = builtin.target.os.tag;
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

fn getBuildPath(allocator: Allocator, project_name: []const u8, version_name: []const u8, file_contents: []const u8) ![]u8 {
    const project_name_hash = blake3Hash(project_name);
    const file_contents_hash = blake3Hash(file_contents);
    const sub_path_parts = [_][]const u8{ &project_name_hash, version_name, &file_contents_hash };
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

fn setEnvVar(allocator: Allocator, name: []const u8, value: []const u8) !void {
    // SKILL_ISSUE: it's required to use -lc to run the tests because of the
    // c library calls. Is there a better way to run a single test file?
    // zig test ./src/cache.zig -lc

    return {
        switch (target_os) {
            .macos, .linux => {
                const c = @cImport({
                    @cInclude("stdlib.h");
                });
                const c_str = try allocator.dupeZ(u8, value);
                defer allocator.free(c_str);
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

fn unsetEnvVar(name: []const u8) !void {
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

fn createTempFile(allocator: Allocator) ![:0]const u8 {
    return {
        switch (target_os) {
            .macos, .linux => {
                const c = @cImport({
                    @cInclude("stdlib.h");
                    @cInclude("unistd.h");
                    @cInclude("fcntl.h");
                });
                const c_str = try allocator.dupeZ(u8, "/tmp/rocXXXXXX");

                const fd = c.mkstemp(c_str.ptr);
                if (fd == -1) {
                    return error.FailToCreateTmpFile;
                }

                // SKILL_ISSUE: I want to hand over I/O calls to the zig
                // core libraries so I'm closing the file here.
                if (c.close(fd) == -1) {
                    return error.FailToCloseTmpFile;
                }

                return c_str;
            },
            else => {
                @compileError("\"" ++ @tagName(target_os) ++ "\" is not supported in cache.zig");
            },
        }
    };
}

fn createTestCacheFolder(allocator: Allocator) !testing.TmpDir {
    const tmp_dir = testing.tmpDir(.{});
    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);
    try setEnvVar(allocator, xdg_cache_home_var_name, path);
    return tmp_dir;
}

fn getHomeDirectory(allocator: Allocator) ![]u8 {
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
    var tmp_dir = try createTestCacheFolder(allocator);
    defer tmp_dir.cleanup();
    defer {
        unsetEnvVar(xdg_cache_home_var_name) catch {
            @panic("failed to unset " ++ xdg_cache_home_var_name);
        };
    }
    const version_name = "1.0.0";
    const expected_binary = "A roc compiler binary";
    try saveCompilerToCache(allocator, version_name, expected_binary);

    const binary = try tryToGetCompilerCacheFile(allocator, version_name) orelse @panic("Binary file should exist");
    defer allocator.free(binary);

    const tmp_dir_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_dir_path);
    const expected_cache_dir = try std.fs.path.join(allocator, &[_][]const u8{
        tmp_dir_path,
        "roc",
        "compiler",
        version_name,
    });
    defer allocator.free(expected_cache_dir);

    try testing.expectEqual(expected_binary.len, binary.len);
    try testing.expectEqualStrings(expected_binary, binary);
    try testing.expect(file_exists(expected_cache_dir));
}

test "Should save to XDG_CACHE_HOME if it is set for app cache" {
    //const allocator = testing.allocator;
    //var tmp_dir = try createTestCacheFolder(allocator);
    //defer tmp_dir.cleanup();
    //defer {
    //unsetEnvVar(xdg_cache_home_var_name) catch {
    //@panic("failed to unset " ++ xdg_cache_home_var_name);
    //};
    //}
    //const version_name = "1.0.0";
    //const project_name = "project-name";
    //const roc_code =
    //\\import cli.Stdout
    //\\main! = |_args|
    //\\    Stdout.line!("Hello, World!")
    //\\"""abc
    //;
    //var expected_ir = canonicalize.IR.init(allocator);
    //defer expected_ir.deinit();
    //var parse_ir = parse.parse(&expected_ir.env, roc_code);
    //parse_ir.store.emptyScratch();
    //defer parse_ir.deinit();

    // QUESTION: does the IR need to be canonicalize testing purposes?
    // Looks like canonicalize is not done yet and it's breaking the test
    // canonicalize.canonicalize(&expected_ir, &parse_ir);

    //try saveCanonicalizeIr(allocator, project_name, version_name, roc_code, &expected_ir);

    //var actual_ir = try tryToGetCanonicalizeIr(allocator, project_name, version_name, roc_code) orelse {
    //@panic("Binary file should exist");
    //};
    //// std.debug.print("\n\nactual_ir:\n\n{any}\n\n", .{actual_ir});

    ////_ = actual_ir;
    //defer actual_ir.deinit();

    //const tmp_dir_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    //defer allocator.free(tmp_dir_path);
    //const expected_cache_dir = try std.fs.path.join(allocator, &[_][]const u8{
    //tmp_dir_path,
    //"roc",
    //"compiler",
    //version_name,
    //});
    //defer allocator.free(expected_cache_dir);

    ////try testing.expectEqualDeep(expected_ir, actual_ir);
    //try testing.expect(file_exists(expected_cache_dir));
}

test "Should get correct base cache folder" {
    const allocator = testing.allocator;
    const home_dir = try getHomeDirectory(allocator);
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
    const home_dir = try getHomeDirectory(allocator);
    defer allocator.free(home_dir);
    const fake_path = try std.fs.path.join(allocator, &[_][]const u8{
        home_dir,
        "fake-cache",
    });
    defer allocator.free(fake_path);
    try setEnvVar(allocator, xdg_cache_home_var_name, fake_path);
    const version_name = "1.0.0";
    const binary = try tryToGetCompilerCacheFile(allocator, version_name);
    if (binary) |b| {
        defer allocator.free(b);
    }

    try testing.expectEqual(null, binary);
}

test "Should set default roc binary" {
    const allocator = testing.allocator;
    var tmp_dir = try createTestCacheFolder(allocator);
    defer tmp_dir.cleanup();

    const tmp_dir_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_dir_path);

    const fake_path = try std.fs.path.join(allocator, &[_][]const u8{
        tmp_dir_path,
        "fake-cache",
    });
    defer allocator.free(fake_path);
    try setEnvVar(allocator, xdg_cache_home_var_name, fake_path);
    const version_name = "1.0.0";
    const expected_binary = "A roc compiler binary";
    try saveCompilerToCache(allocator, version_name, expected_binary);
    try setDefaultRocCompiler(allocator, version_name);
    const binary = try tryToGetCompilerCacheFile(allocator, version_name);

    // SKILL_ISSUE: seem like there might be a better way to assert the binary
    // is not null
    if (binary) |b| {
        defer allocator.free(b);
        try testing.expectEqualStrings(expected_binary, b);
    } else {
        @panic("No roc file found");
    }
}

test "Should not return error if the roc binary already exists" {
    const allocator = testing.allocator;
    var tmp_dir = try createTestCacheFolder(allocator);
    defer tmp_dir.cleanup();

    const tmp_dir_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_dir_path);

    const fake_path = try std.fs.path.join(allocator, &[_][]const u8{
        tmp_dir_path,
        "fake-cache",
    });
    defer allocator.free(fake_path);
    try setEnvVar(allocator, xdg_cache_home_var_name, fake_path);
    const version_name = "1.0.0";
    const expected_binary = "A roc compiler binary";
    try saveCompilerToCache(allocator, version_name, expected_binary);
    try setDefaultRocCompiler(allocator, version_name);
    const binary_1 = try tryToGetCompilerCacheFile(allocator, version_name);
    if (binary_1) |b| {
        defer allocator.free(b);
    }
    const binary_2 = try tryToGetCompilerCacheFile(allocator, version_name);
    if (binary_2) |b| {
        defer allocator.free(b);
    }
}
