// Spec here https://github.com/roc-lang/roc/issues/7517
const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

pub const CompilerCacheFile = struct {
    version_name: []const u8,
    binary: []const u8,
};

pub const RocCacheFileTag = enum {
    compiler,
    build,
    packages_src,
    packages_build,
};

pub const RocCacheFile = union(RocCacheFileTag) {
    compiler: CompilerCacheFile,
    build: void,
    packages_src: void,
    packages_build: void,
};

fn getDefaultCacheFolder(allocator: Allocator) ![]u8 {
    const target_os = builtin.target.os.tag;
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
                // SKILL_ISSUE: I want to include the os name in the error message, but
                // it seems like I need to include an allocator to concatenate strings.
                // I feel like that can't be the best approach.
                @compileLog(target_os);
                @compileError("Target OS is unsupported in cache");
            },
        }
    };
}

fn getBaseRocCacheFolder(allocator: Allocator, xdg_cache_home: ?[]const u8, sub_path: []const u8) ![]u8 {
    const base_dir = if (xdg_cache_home) |x| try allocator.dupe(u8, x) else try getDefaultCacheFolder(allocator);
    defer allocator.free(base_dir);
    const roc_cache_parts = [_][]const u8{
        base_dir,
        "roc",
        sub_path,
    };
    return try std.fs.path.join(allocator, &roc_cache_parts);
}

fn getCompilerPath(allocator: Allocator, xdg_cache_home: ?[]const u8, version_name: []const u8) ![]u8 {
    const sub_path_parts = [_][]const u8{ "compiler", version_name };
    const sub_path = try std.fs.path.join(allocator, &sub_path_parts);
    defer allocator.free(sub_path);
    return try getBaseRocCacheFolder(allocator, xdg_cache_home, sub_path);
}

fn getXdgCacheHome(allocator: Allocator) ?[]u8 {
    return std.process.getEnvVarOwned(allocator, "XDG_CACHE_HOME") catch null;
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
            return SaveCacheFileError.FailedToMakeCacheFolder;
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
    xdg_cache_home: ?[]const u8,
    roc_cache_file: RocCacheFile,
    comptime save: fn (file_cache_path: []const u8, file_binary: []const u8) SaveCacheFileError!void,
) !void {
    const file_cache_path = switch (roc_cache_file) {
        .compiler => |value| try getCompilerPath(allocator, xdg_cache_home, value.version_name),
        else => @panic("FIX ME!!!!!!"),
    };

    defer allocator.free(file_cache_path);

    const file_binary = switch (roc_cache_file) {
        .compiler => |value| value.binary,
        else => @panic("FIX ME!!!!!!"),
    };

    try save(file_cache_path, file_binary);
}

pub fn saveToCache(allocator: Allocator, roc_cache_file: RocCacheFile) !void {
    const xdg_cache_home = getXdgCacheHome(allocator);
    defer allocator.free(xdg_cache_home);
    try saveToCacheInternal(allocator, xdg_cache_home, roc_cache_file, saveCacheFile);
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

test "Compiler cache should save in the correct place with the correct data" {
    const allocator = testing.allocator;
    try create_test_cache_folder();
    defer delete_test_cache_folder();
    const cache_file = CompilerCacheFile{
        .version_name = "1.0.0",
        .binary = "A roc compiler binary",
    };
    // SKILL_ISSUE: I'm not sure how to set the environment variable in a
    // cross-platform way so my tests don't call this function
    try saveToCacheInternal(allocator, test_cache_folder, RocCacheFile{ .compiler = cache_file }, saveCacheFile);
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
