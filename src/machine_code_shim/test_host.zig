//! Minimal host ABI exports used by machine-code shim unit tests.

const std = @import("std");
const builtins = @import("builtins");

const empty_hosted_fns = [_]builtins.host_abi.HostedFn{};

export const roc_shim_hosted_count: usize = 0;
export const roc_shim_hosted_fns: [*]const builtins.host_abi.HostedFn = &empty_hosted_fns;

export fn roc_alloc(_: usize, _: usize) ?*anyopaque {
    return null;
}

export fn roc_dealloc(_: *anyopaque, _: usize) void {}

export fn roc_realloc(_: *anyopaque, _: usize, _: usize) ?*anyopaque {
    return null;
}

export fn roc_dbg(_: [*]const u8, _: usize) void {}

export fn roc_expect_failed(_: [*]const u8, _: usize) void {}

export fn roc_crashed(_: [*]const u8, _: usize) void {
    std.process.abort();
}
