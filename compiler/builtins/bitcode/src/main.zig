const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;

// Num Module
const num = @import("num.zig");
comptime { exportNumFn(num.atan, "atan"); }
comptime { exportNumFn(num.isFinite, "is_finite"); }
comptime { exportNumFn(num.powInt, "pow_int"); }
comptime { exportNumFn(num.acos, "acos"); }
comptime { exportNumFn(num.asin, "asin"); }

// Str Module
const str = @import("str.zig");
comptime { exportStrFn(str.strSplitInPlace, "str_split_in_place"); }
comptime { exportStrFn(str.countSegments, "count_segments"); }
comptime { exportStrFn(str.countGraphemeClusters, "count_grapheme_clusters"); }

// Export helpers - Must be run inside a comptime
fn exportBuiltinFn(comptime fn_target: anytype, comptime fn_name: []const u8) void {
    @export(fn_target, .{ .name = "roc_builtins." ++ fn_name, .linkage = .Strong  });
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
