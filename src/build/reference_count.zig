const std = @import("std");
const base = @import("../base.zig");
const lower = @import("./lower_statements.zig");

const ModuleWork = base.ModuleWork;

const Self = @This();

/// Represents the program intermediate representation after adding refcounting instructions.
pub const IR = @import("./reference_count/IR.zig");

/// Check ownership of function arguments and add refcounting instructions where necessary.
pub fn referenceCount(
    ir: *IR,
    lower_ir: *const lower.IR,
    other_modules: *const std.ArrayList(ModuleWork(IR)),
) void {
    _ = ir;
    _ = lower_ir;
    _ = other_modules;

    // TODO: implement
}
