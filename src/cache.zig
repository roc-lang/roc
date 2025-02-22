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

const target_os = builtin.target.os.tag;
const default_cache_folder = switch (target_os) {
    .macos, .linux => "~/.cache",
    .windows => "%APPDATA%",
    else => {
        // SKILL_ISSUE: I want to include the os name in the error message, but
        // it seems like I need to include an allocator to concatenate strings.
        // I feel like that can't be the best approach.
        @compileLog(target_os);
        @compileError("Target OS is unsupported in cache");
    },
};
// , comptime my_fun: fn (str: []const u8) void
fn saveToCacheInternal(allocator: Allocator, roc_cache_file: RocCacheFile, xdg_cache_home: ?[]const u8) !void {
    const base_dir = xdg_cache_home orelse default_cache_folder;
    const roc_cache_parts = [_][]const u8{ base_dir, "roc" };
    const roc_cache_folder = try std.fs.path.join(allocator, &roc_cache_parts);
    defer allocator.free(roc_cache_folder);

    const file_path_parts = switch (roc_cache_file) {
        .compiler => |value| [_][]const u8{ roc_cache_folder, value.version_name },
        else => @panic("FIX ME!!!!!!"),
    };

    const file_cache_path = try std.fs.path.join(allocator, &file_path_parts);
    defer allocator.free(file_cache_path);

    //try std.fs.cwd().makePath(file_cache_path);

    //const file = try std.fs.cwd().openFile(file_cache_path, .{
    //.mode = .read_write,
    ////.write = true,
    ////.create = true,
    ////.truncate = true,
    //});
    //defer file.close();

    std.debug.print("{s}\n", .{file_cache_path});
    //_ = try file.write("Hello, Zig!");
}

//fn s(allocator: Allocator, roc_cache_file: RocCacheFile, xdg_cache_home: ?[]u8) void {
//}

test "Cache folder is correct" {
    const cache_folder = "./temp";
    const cwd = std.fs.cwd();
    cwd.deleteDir(cache_folder);
    //try cwd.makePath(cache_folder);

    try saveToCacheInternal(std.testing.allocator, RocCacheFile{ .compiler = .{
        .version_name = "1.0.0",
        .binary = "Str.concat(\"Hi \", \"there.\")",
    } }, "./temp");
}
