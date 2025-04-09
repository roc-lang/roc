const std = @import("std");
const base = @import("../base.zig");
const types = @import("../types.zig");
const resolve = @import("../check/resolve_imports.zig");

const Type = types.Type;
const ModuleWork = base.ModuleWork;

const Self = @This();

/// Represents the intermediate representation of the program after type specialization.
pub const IR = @import("specialize_types/IR.zig");

/// Create a copy of every function in the program, by walking from the entrypoint down the tree, replacing type variables with concrete types.
///
/// replace all calls to generic functions with concrete instances
/// after this step, the program has no generic types
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/ayaz/compile-with-lambda-sets/0102-compiling-lambda-sets.md#type_specialize
pub fn specializeTypes(
    ir: *IR,
    resolve_ir: *const resolve.IR,
    type_store: *const Type.Store,
    other_modules: *const ModuleWork(IR).Store,
) void {
    _ = ir;
    _ = resolve_ir;
    _ = type_store;
    _ = other_modules;

    // TODO: implement
}
