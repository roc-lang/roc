const std = @import("std");
const base = @import("../base.zig");
const func_lift = @import("./lift_functions.zig");
const func_solve = @import("./solve_functions.zig");

const ModuleWork = base.ModuleWork;

const Self = @This();

/// Represents the program intermediate representation after specializing functions.
pub const IR = @import("./specialize_functions/IR.zig");

/// For every function that takes a function as an argument:
/// - replace each function arg with a new tag union based on the FunctionSet created during `solve_functions`
/// - where the function arg is called within the function body, match on it and call the original function optionally with its capture
///
/// Notes from Ayaz' doc: https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_specialize
pub fn specializeFunctions(
    ir: *IR,
    func_lift_ir: *const func_lift.IR,
    function_sets: *const func_solve.IR,
    other_modules: *const std.ArrayList(ModuleWork(IR)),
) void {
    _ = ir;
    _ = func_lift_ir;
    _ = function_sets;
    _ = other_modules;

    // TODO: implement
}
