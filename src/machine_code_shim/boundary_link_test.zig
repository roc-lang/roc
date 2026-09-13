//! Link a minimal host executable against each real shim archive. Symbol
//! references here and the archive exports share the canonical names, while
//! getOps traverses the shim-to-host direction of the boundary at runtime.
const std = @import("std");
const builtins = @import("builtins");
const symbols = builtins.shim_symbols;
const RocOps = builtins.host_abi.RocOps;
const is_interpreter = @import("boundary_test_options").is_interpreter;

test "canonical symbols link through the shim and back to the host" {
    const get_ops = @extern(*const fn () callconv(.c) *anyopaque, .{ .name = symbols.roc_shim_get_ops });
    const entrypoint = @extern(*const fn (u32, *RocOps, ?*anyopaque, ?*anyopaque) callconv(.c) void, .{ .name = symbols.roc_entrypoint });
    std.mem.doNotOptimizeAway(entrypoint);
    if (is_interpreter) {
        const from_image = @extern(*const fn (u32, *RocOps, ?*anyopaque, ?*anyopaque, ?*anyopaque, usize) callconv(.c) void, .{ .name = symbols.roc_entrypoint_from_image });
        std.mem.doNotOptimizeAway(from_image);
    } else {
        const default_main = @extern(*const fn (usize, [*][*:0]const u8) callconv(.c) usize, .{ .name = symbols.roc_shim_default_main });
        std.mem.doNotOptimizeAway(default_main);
    }
    inline for (symbols.runtime_set) |name| {
        std.mem.doNotOptimizeAway(@field(builtins.host_abi.extern_host, name));
    }
    const ops: *RocOps = @ptrCast(@alignCast(get_ops()));
    const count = @extern(*const usize, .{ .name = symbols.roc_shim_hosted_count });
    const fns = @extern(*const [*]const builtins.host_abi.HostedFn, .{ .name = symbols.roc_shim_hosted_fns });
    try std.testing.expectEqual(count.*, ops.hosted_fns.count);
    try std.testing.expectEqual(@intFromPtr(fns.*), @intFromPtr(ops.hosted_fns.fns));
    try std.testing.expectEqual(@as(?*anyopaque, null), ops.roc_alloc(ops, 1, 1));
    var byte: u8 = 0;
    try std.testing.expectEqual(@as(?*anyopaque, null), ops.roc_realloc(ops, &byte, 1, 1));
    ops.roc_dealloc(ops, &byte, 1);
    ops.roc_dbg(ops, "", 0);
    ops.roc_expect_failed(ops, "", 0);
}
