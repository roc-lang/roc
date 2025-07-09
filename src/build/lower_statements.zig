const std = @import("std");
const base = @import("../base.zig");
const func_spec = @import("./specialize_functions.zig");

const ModuleWork = base.ModuleWork;

const Self = @This();

/// Represents the intermediate representation of the program after converting expressions into statements for consumption by codegen.
pub const IR = @import("./lower_statements/IR.zig");

/// Convert expressions into statements for consumption by codegen.
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/ayaz/compile-with-lambda-sets/0102-compiling-lambda-sets.md#lower_ir
pub fn lowerStatements(
    ir: *IR,
    func_spec_ir: *const func_spec.IR,
    other_modules: *const std.ArrayList(ModuleWork(IR)),
) void {
    _ = ir;
    _ = func_spec_ir;
    _ = other_modules;

    // TODO: implement
}
