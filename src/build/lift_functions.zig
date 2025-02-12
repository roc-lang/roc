const std = @import("std");
const base = @import("../base.zig");
const type_spec = @import("./specialize_types.zig");
pub const IR = @import("./lift_functions/IR.zig");

const lift_functions = @This();

/// Lift all closures to the top-level and leave behind closure captures
///
/// after this step, the program has no more implicit closures
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_lift
pub fn liftFunctions(ir: type_spec.IR, other_modules: []lift_functions.IR) lift_functions.IR {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}
