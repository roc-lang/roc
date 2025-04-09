const std = @import("std");
const base = @import("../base.zig");
const type_spec = @import("./specialize_types.zig");

const ModuleWork = base.ModuleWork;

const Self = @This();

/// Represents the intermediate representation of the program IR after lifting functions.
/// All anonymous function bodies are lifted to the top level of the program.
pub const IR = @import("./lift_functions/IR.zig");

/// Lift all closures to the top-level and leave behind closure captures
///
/// after this step, the program has no more implicit closures
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_lift
pub fn liftFunctions(
    ir: *IR,
    type_spec_ir: *const type_spec.IR,
    other_modules: *const ModuleWork(IR).Store,
) void {
    _ = ir;
    _ = type_spec_ir;
    _ = other_modules;

    // TODO: not implemented
}
