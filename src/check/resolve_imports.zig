const std = @import("std");
const base = @import("../base.zig");
const can = @import("./canonicalize.zig");

const Self = @This();
pub const IR = @import("./resolve_imports/IR.zig");

pub fn resolveImports(
    can_ir: can.IR,
    other_modules: []Self.IR,
) Self.IR {
    _ = can_ir;
    _ = other_modules;

    @panic("not implemented");
}
