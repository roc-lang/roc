const std = @import("std");
const base = @import("../base.zig");
const can = @import("canonicalize.zig");

const resolve = @This();
pub const IR = @import("resolve_imports/IR.zig");

pub fn resolveImports(
    can_ir: can.IR,
    other_modules: std.HashMap(base.ModuleId, resolve.IR),
) resolve.IR {
    _ = can_ir;
    _ = other_modules;

    @panic("not implemented");
}
