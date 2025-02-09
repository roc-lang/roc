const std = @import("std");
const base = @import("../base.zig");
const func_spec = @import("specialize_functions.zig");

const lower_statements = @This();
pub const IR = @import("lower_statements/IR.zig");

/// Convert expressions into statements for consumption by codegen.
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/ayaz/compile-with-lambda-sets/0102-compiling-lambda-sets.md#lower_ir
pub fn lowerStatements(ir: func_spec.IR, other_modules: []lower_statements.IR) lower_statements.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
