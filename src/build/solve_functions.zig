const std = @import("std");
const base = @import("../base.zig");
const func_lift = @import("lift_functions.zig");

const solve_functions = @This();
pub const IR = @import("solve_functions/IR.zig");

/// Annotate the generic-ness of each function at the top-level
///
/// infer all sets of functions passed to higher-order function (HOF) calls
/// after this step, every call to a HOF is assigned a variable with the set of functions passed (function set) or a generalized variable
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_solve
pub fn solveFunctions(
    ir: func_lift.IR,
    other_modules: std.HashMap(base.ModuleId, solve_functions.IR),
) solve_functions.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
