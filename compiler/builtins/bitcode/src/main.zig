const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;

// Num Module
const num = @import("num.zig");
comptime {
    exportNumFn(num.atan, "atan");
    exportNumFn(num.isFinite, "is_finite");
    exportNumFn(num.powInt, "pow_int");
    exportNumFn(num.acos, "acos");
    exportNumFn(num.asin, "asin");
}

// Str Module
const str = @import("str.zig");
comptime {
    exportStrFn(str.strSplitInPlace, "str_split_in_place");
    exportStrFn(str.countSegments, "count_segments");
    exportStrFn(str.countGraphemeClusters, "count_grapheme_clusters");
    exportStrFn(str.startsWith, "starts_with");
    exportStrFn(str.endsWith, "ends_with");
    exportStrFn(str.strConcat, "concat");
    exportStrFn(str.strLen, "len");
}

// Export helpers - Must be run inside a comptime
fn exportBuiltinFn(comptime fn_target: anytype, comptime fn_name: []const u8) void {
    @export(fn_target, .{ .name = "roc_builtins." ++ fn_name, .linkage = .Strong });
}
fn exportNumFn(comptime fn_target: anytype, comptime fn_name: []const u8) void {
    exportBuiltinFn(fn_target, "num." ++ fn_name);
}
fn exportStrFn(comptime fn_target: anytype, comptime fn_name: []const u8) void {
    exportBuiltinFn(fn_target, "str." ++ fn_name);
}

// Run all tests in imported modules
// https://github.com/ziglang/zig/blob/master/lib/std/std.zig#L94
test "" {
    testing.refAllDecls(@This());
}
