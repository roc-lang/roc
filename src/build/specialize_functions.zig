const std = @import("std");
const base = @import("../base.zig");
const func_solve = @import("solve_functions.zig");

const specialize_functions = @This();
pub const IR = @import("specialize_functions/ir.zig");

/// Make each generic function concrete by representing each set of potential captures using a tag union as an argument.
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_specialize
pub fn specializeFunctions(
    ir: func_solve.IR,
    other_modules: std.HashMap(base.ModuleId, specialize_functions.IR),
) specialize_functions.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
