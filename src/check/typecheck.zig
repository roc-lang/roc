const std = @import("std");
const base = @import("../base.zig");
const resolve = @import("resolve_imports.zig");

pub fn checkTypes(
    resolve_ir: resolve.IR,
    other_modules: []resolve.IR,
) void {
    _ = resolve_ir;
    _ = other_modules;

    @panic("not implemented");
}
