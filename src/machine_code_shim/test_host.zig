//! Minimal host ABI exports used by machine-code shim unit tests.

const std = @import("std");
const builtins = @import("builtins");
const shim_symbols = builtins.shim_symbols;

const empty_hosted_fns = [_]builtins.host_abi.HostedFn{};

const hosted_count: usize = 0;
const hosted_fns: [*]const builtins.host_abi.HostedFn = &empty_hosted_fns;

comptime {
    @export(&hosted_count, .{ .name = shim_symbols.roc_shim_hosted_count });
    @export(&hosted_fns, .{ .name = shim_symbols.roc_shim_hosted_fns });
    @export(&rocAlloc, .{ .name = shim_symbols.roc_alloc });
    @export(&rocDealloc, .{ .name = shim_symbols.roc_dealloc });
    @export(&rocRealloc, .{ .name = shim_symbols.roc_realloc });
    @export(&rocDbg, .{ .name = shim_symbols.roc_dbg });
    @export(&rocExpectFailed, .{ .name = shim_symbols.roc_expect_failed });
    @export(&rocCrashed, .{ .name = shim_symbols.roc_crashed });
}

fn rocAlloc(_: usize, _: usize) callconv(.c) ?*anyopaque {
    return null;
}

fn rocDealloc(_: *anyopaque, _: usize) callconv(.c) void {}

fn rocRealloc(_: *anyopaque, _: usize, _: usize) callconv(.c) ?*anyopaque {
    return null;
}

fn rocDbg(_: [*]const u8, _: usize) callconv(.c) void {}

fn rocExpectFailed(_: [*]const u8, _: usize) callconv(.c) void {}

fn rocCrashed(_: [*]const u8, _: usize) callconv(.c) void {
    std.process.abort();
}
