const std = @import("std");
const builtin = @import("builtin");
const RocStr = @import("str.zig").RocStr;

// An optional debug impl to be called during `roc test`
pub fn dbg_impl(loc: *const RocStr, msg: *const RocStr, src: *const RocStr) callconv(.c) void {
    if (builtin.target.cpu.arch != .wasm32) {
        const stderr = std.io.getStdErr().writer();
        stderr.print("[{s}] {s} = {s}\n", .{ loc.asSlice(), src.asSlice(), msg.asSlice() }) catch unreachable;
    }
}
