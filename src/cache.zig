const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

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

fn saveToCache(allocator: Allocator, xdg_cache_home: ?[]u8) !void {
    const base_dir = xdg_cache_home orelse default_cache_folder;
    const file_parts = [_][]const u8{ base_dir, "roc" };
    const roc_cache_folder = try std.fs.path.join(allocator, &file_parts);
    defer allocator.free(roc_cache_folder);

    std.debug.print("{s}\n", .{roc_cache_folder});
}

test "Cache folder is correct" {
    try saveToCache(std.testing.allocator, null);
}
