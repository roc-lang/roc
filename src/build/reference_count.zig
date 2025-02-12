const std = @import("std");
const base = @import("../base.zig");
const lower = @import("./lower_statements.zig");

const reference_count = @This();
pub const IR = @import("./reference_count/IR.zig");

/// Check ownership of function arguments and add refcounting instructions where necessary.
pub fn referenceCount(ir: lower.IR, other_modules: []reference_count.IR) reference_count.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
