const std = @import("std");
const builtin = @import("builtin");
const RocStr = @import("str.zig").RocStr;
const RocList = @import("list.zig").RocList;

pub const NameVariable = struct {
    name: RocStr,
    value: RocStr,
};

// An optional expect impl to be called during `roc test`
pub fn expect_impl(loc: *const RocStr, src: *const RocStr, variables: *RocList) callconv(.C) void {

    // TODO print the variables
    _ = variables;

    if (builtin.target.cpu.arch != .wasm32) {
        const stderr = std.io.getStdErr().writer();
        stderr.print("[{s}] {s} = TODO IMPLEMENT ME!\n", .{ loc.asSlice(), src.asSlice() }) catch unreachable;
    }
}
