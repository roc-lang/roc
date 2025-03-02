const std = @import("std");
const base = @import("../base.zig");
const can = @import("canonicalize.zig");

const Self = @This();
pub const IR = @import("resolve_imports/IR.zig");

pub fn resolveImports(
    ir: *IR,
    can_ir: *const can.IR,
    other_modules: *const base.ModuleWork(IR).Store,
) void {
    _ = ir;
    _ = can_ir;
    _ = other_modules;

    // TODO: implement
}
