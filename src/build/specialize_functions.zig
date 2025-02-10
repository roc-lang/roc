const std = @import("std");
const base = @import("../base.zig");
const func_lift = @import("lift_functions.zig");
const func_solve = @import("solve_functions.zig");
const func_spec = @import("specialize_functions.zig");

const specialize_functions = @This();
pub const IR = @import("specialize_functions/IR.zig");

/// For every function that takes a function as an argument:
/// - replace each function arg with a new tag union based on the FunctionSet created during `solve_functions`
/// - where the function arg is called within the function body, match on it and call the original function optionally with its capture
///
/// Notes from Ayaz' doc: https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_specialize
pub fn specializeFunctions(
    ir: func_lift.IR,
    function_sets: func_solve.FunctionSet.List,
    other_modules: []specialize_functions.IR,
) specialize_functions.IR {
    _ = ir;
    _ = function_sets;
    _ = other_modules;

    @panic("not implemented");
}
