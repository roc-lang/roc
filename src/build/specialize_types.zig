const std = @import("std");
const base = @import("../base.zig");
const resolve = @import("../check/resolve_imports.zig");

const specialize_types = @This();
pub const IR = @import("specialize_types/IR.zig").IR;

/// Create a copy of every function in the program, by walking from the entrypoint down the tree, replacing type variables with concrete types.
///
/// replace all calls to generic functions with concrete instances
/// after this step, the program has no generic types
///
/// Implementation notes from Ayaz https://github.com/roc-lang/rfcs/blob/ayaz/compile-with-lambda-sets/0102-compiling-lambda-sets.md#type_specialize
pub fn specializeTypes(resolve_ir: resolve.IR, other_modules: []specialize_types.IR) specialize_types.IR {
    _ = resolve_ir;
    _ = other_modules;

    @panic("not implemented");
}
