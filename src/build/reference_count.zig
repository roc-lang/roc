const std = @import("std");
const base = @import("../base.zig");
const lower_statements = @import("lower_statements.zig");

const reference_count = @This();
pub const IR = @import("reference_count/IR.zig");

/// Check ownership of function arguments and add refcounting instructions where necessary.
pub fn referenceCount(
    ir: lower_statements.IR,
    other_modules: std.HashMap(base.ModuleId, reference_count.IR),
) reference_count.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
