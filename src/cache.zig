// Spec here https://github.com/roc-lang/roc/issues/7517
const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");

const Package = base.Package;

// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []const u8, roc_version: []const u8) ?canonicalize.IR {
    _ = file_hash;
    _ = roc_version;

    return null;
}

pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator) []const u8 {
    _ = url_data;
    _ = gpa;

    @panic("not implemented");
}

pub const CacheFile = union(enum) {
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
pub fn setEnvVar(name: []const u8, value: []const u8) !void {
    // SKILL_ISSUE: it's required to use -lc to run the tests because of the
    // c library calls. Is there a better way to run a single test file?
    // zig test ./src/cache.zig -lc
    return {
        switch (target_os) {
            .macos, .linux => {
                const c = @cImport({
                    @cInclude("stdlib.h");
                });
                if (c.setenv(name.ptr, value.ptr, 1) != 0) {
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
                const home_dir = try std.process.getEnvVarOwned(std.testing.allocator, "HOME");
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
    const base_dir = if (xdg_cache_home) |x| try allocator.dupe(u8, x) else try getDefaultCacheFolder(allocator);
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

const SaveCacheFileError = error{
    FailedToMakeCacheFolder,
    FailedToMakeCacheFile,
    FailedToWriteToCacheFile,
};

fn saveCacheFile(file_cache_path: []const u8, file_binary: []const u8) SaveCacheFileError!void {
    const cwd = std.fs.cwd();
    const roc_cache_folder = std.fs.path.dirname(file_cache_path);
    if (roc_cache_folder) |x| {
        cwd.makePath(x) catch {
            return error.FailedToMakeCacheFolder;
        };
    }
    const file = cwd.createFile(file_cache_path, .{}) catch {
        return SaveCacheFileError.FailedToMakeCacheFile;
    };
    file.writeAll(file_binary) catch {
        return SaveCacheFileError.FailedToWriteToCacheFile;
    };
}

fn saveToCacheInternal(
    // SKILL_ISSUE: force my formatter to put the arguments in a vertical line
    allocator: Allocator,
    roc_cache_file: CacheFile,
    comptime save: fn (file_cache_path: []const u8, file_binary: []const u8) SaveCacheFileError!void,
) !void {
    const file_cache_path = switch (roc_cache_file) {
        .compiler => |value| try getCompilerPath(allocator, value.version_name),
        else => @panic("FIX ME!!!!!!"),
    };

    defer allocator.free(file_cache_path);

    const file_binary = switch (roc_cache_file) {
        .compiler => |value| value.binary,
        else => @panic("FIX ME!!!!!!"),
    };

    try save(file_cache_path, file_binary);
}

pub fn saveToCache(allocator: Allocator, roc_cache_file: CacheFile) !void {
    try saveToCacheInternal(allocator, roc_cache_file, saveCacheFile);
}

//pub fn setRocCompilerInternal(allocator: Allocator, version_name: []const u8) !void {
//}
//pub fn setRocCompiler(allocator: Allocator, version_name: []const u8) !void {
//const xdg_cache_home = getXdgCacheHome(allocator);
//defer allocator.free(xdg_cache_home);
//try setRocCompilerInternal(allocator, roc_cache_file, xdg_cache_home, saveCacheFile);
//}

const test_cache_folder = "./test-cache";
fn create_test_cache_folder() !void {
    try setEnvVar(xdg_cache_home_var_name, test_cache_folder);
    const cwd = std.fs.cwd();
    // remove folders temp cache folder in case a old test fail to clean it up
    delete_test_cache_folder();
    try cwd.makePath(test_cache_folder);
}

fn delete_test_cache_folder() void {
    const cwd = std.fs.cwd();
    cwd.deleteTree(test_cache_folder) catch @panic("Failed to delete test cache folder!");
}

fn make_test_cache_file_path(allocator: Allocator, relative_path: []const u8) ![]u8 {
    const parts = [_][]const u8{ test_cache_folder, "roc", relative_path };
    return try std.fs.path.join(allocator, &parts);
}

const SaveStub = struct {
    file_cache_path_list: std.ArrayList([]const u8),
    file_binary_list: std.ArrayList([]const u8),
    pub fn create_save_stub(self: *const SaveStub) fn (
        // SKILL_ISSUE: force my formatter to put the arguments in a vertical line
        file_cache_path: []const u8,
        file_binary: []const u8,
    ) SaveCacheFileError!void {
        return struct {
            fn closure(file_cache_path: []const u8, file_binary: []const u8) SaveCacheFileError!void {
                self.file_cache_path_list.append(file_cache_path);
                self.file_binary_list.append(file_binary);
            }
        }.closure;
    }

    pub fn init(allocator: std.mem.Allocator) SaveStub {
        return SaveStub{
            .file_cache_path_list = std.ArrayList([]const u8).init(allocator),
            .file_binary_list = std.ArrayList([]const u8).init(allocator),
        };
    }
    pub fn deinit(self: *SaveStub) void {
        self.list.deinit();
    }

    pub fn assert_save_called_with_correct_arguments(self: *SaveStub, file_cache_path: []const u8, file_binary: []const u8) !void {
        try testing.expectEqual(1, self.file_cache_path_list.items.len);
        try testing.expectEqual(1, self.file_binary_list.items.len);
        try testing.expectEqualStrings(file_cache_path, self.file_cache_path_list[0]);
        try testing.expectEqual(file_binary, self.file_binary_list[0]);
    }
};

test "Compiler cache should save in the correct place with the correct data" {
    const allocator = testing.allocator;
    try create_test_cache_folder();
    defer delete_test_cache_folder();
    const cache_file = CacheFile.Compiler{
        .version_name = "1.0.0",
        .binary = "A roc compiler binary",
    };
    try saveToCache(allocator, CacheFile{ .compiler = cache_file });
    const expected_cache_file_local = try make_test_cache_file_path(allocator, "compiler/1.0.0");
    defer allocator.free(expected_cache_file_local);

    const file = try std.fs.cwd().openFile(expected_cache_file_local, .{ .mode = .read_only });
    defer file.close();

    const file_size = try file.getEndPos();
    const buf = try allocator.alloc(u8, file_size);
    defer allocator.free(buf);
    _ = try file.readAll(buf);
    try testing.expectEqual(cache_file.binary.len, buf.len);
    try testing.expectEqualStrings(cache_file.binary, buf);
}

test "Compiler cache should save to .cache folder" {
    const allocator = testing.allocator;
    const cache_file = CacheFile.Compiler{
        .version_name = "1.0.0",
        .binary = "A roc compiler binary",
    };
    const save_stub_context = SaveStub.init(allocator);
    defer save_stub_context.deinit();

    const save_stub = save_stub_context.create_save_stub();

    try saveToCacheInternal(allocator, CacheFile{ .compiler = cache_file }, save_stub);
    const expected_cache_file_local = try make_test_cache_file_path(allocator, "compiler/1.0.0");
    defer allocator.free(expected_cache_file_local);
    save_stub_context.assert_save_called_with_correct_arguments("", "");
}
