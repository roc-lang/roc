// Spec here https://github.com/roc-lang/roc/issues/7517
const builtin = @import("builtin");
const std = @import("std");
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
    return blk: {
        switch (target_os) {
            .macos, .linux => {
                const home_dir = try std.process.getEnvVarOwned(std.testing.allocator, "HOME");
                defer allocator.free(home_dir);
                const default_cache_folder_parts = [_][]const u8{
                    home_dir,
                    ".cache",
                };
                const default_cache_folder = try std.fs.path.join(allocator, &default_cache_folder_parts);
                break :blk default_cache_folder;
            },
            .windows => {
                const default_cache_folder_parts = [_][]const u8{"%APPDATA%"};
                const default_cache_folder = try std.fs.path.join(allocator, &default_cache_folder_parts);
                break :blk default_cache_folder;
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

const SaveCacheFileError = error{
    FailedToMakeCacheFolder,
    FailedToMakeCacheFile,
    FailedToWriteToCacheFile,
};

fn saveCacheFile(roc_cache_folder: []const u8, file_cache_path: []const u8, file_binary: []const u8) SaveCacheFileError!void {
    const cwd = std.fs.cwd();
    cwd.makePath(roc_cache_folder) catch {
        return SaveCacheFileError.FailedToMakeCacheFolder;
    };
    const file = cwd.createFile(file_cache_path, .{}) catch {
        return SaveCacheFileError.FailedToMakeCacheFile;
    };
    file.writeAll(file_binary) catch {
        return SaveCacheFileError.FailedToWriteToCacheFile;
    };
}

fn saveToCacheInternal(allocator: Allocator, roc_cache_file: RocCacheFile, xdg_cache_home: ?[]const u8, comptime save: fn (roc_cache_folder: []const u8, file_cache_path: []const u8, file_binary: []const u8) SaveCacheFileError!void) !void {
    const base_dir = xdg_cache_home orelse try getDefaultCacheFolder(allocator);
    defer {
        if (xdg_cache_home == null) {
            allocator.free(base_dir);
        }
    }
    const roc_cache_parts = [_][]const u8{ base_dir, "roc" };
    const roc_cache_folder = try std.fs.path.join(allocator, &roc_cache_parts);
    defer allocator.free(roc_cache_folder);

    const file_path_parts = switch (roc_cache_file) {
        .compiler => |value| [_][]const u8{ roc_cache_folder, value.version_name },
        else => @panic("FIX ME!!!!!!"),
    };

    const file_cache_path = try std.fs.path.join(allocator, &file_path_parts);
    defer allocator.free(file_cache_path);

    const file_binary = switch (roc_cache_file) {
        .compiler => |value| value.binary,
        else => @panic("FIX ME!!!!!!"),
    };

    try save(roc_cache_folder, file_cache_path, file_binary);
}

pub fn saveToCache(allocator: Allocator, roc_cache_file: RocCacheFile, xdg_cache_home: ?[]const u8) !void {
    try saveToCacheInternal(allocator, roc_cache_file, xdg_cache_home, saveCacheFile);
}

const test_cache_folder = "./test-cache";
fn create_test_cache_folder() !void {
    const cwd = std.fs.cwd();
    // remove folders temp cache folder in case a old test fail to clean it up
    delete_test_cache_folder();
    try cwd.makePath(test_cache_folder);
}

fn delete_test_cache_folder() void {
    //const cwd = std.fs.cwd();
    //cwd.deleteTree(test_cache_folder) catch @panic("Failed to delete test cache folder!");
}
test "Cache folder is correct" {
    try create_test_cache_folder();
    defer delete_test_cache_folder();
    try saveToCache(std.testing.allocator, RocCacheFile{ .compiler = .{
        .version_name = "1.0.0",
        .binary = "Str.concat(\"Hi \", \"there.\")",
    } }, test_cache_folder);
}
