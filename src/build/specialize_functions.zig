const std = @import("std");
const base = @import("../base.zig");
const func_lift = @import("lift_functions.zig");

const specialize_functions = @This();
pub const IR = @import("specialize_functions/IR.zig");

/// For every function that takes a function as an argument:
/// - find all functions that can be called from that function
/// - create a tag union where each tag is the name of one of said functions, and the payload for the tag is the struct of captures for that function
/// - replace each function arg with that new tag union
/// - where the function arg is called within the function body, match on it and call the original function optionally with its capture
///
/// A combination of function solving and specialization from Ayaz' doc:
/// solving: https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_solve
/// specialization: https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_specialize
pub fn specializeFunctions(ir: func_lift.IR, other_modules: []specialize_functions.IR) specialize_functions.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
