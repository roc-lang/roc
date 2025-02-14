const std = @import("std");
const base = @import("../base.zig");
const resolve = @import("resolve_imports.zig");
const TypeStore = @import("../types/Store.zig");

/// Solves for the types of expressions in the ResolveIR and populates this
/// information in the module's type store.
pub fn checkTypes(
    resolve_ir: resolve.IR,
    other_modules: []resolve.IR,
) TypeStore {
    _ = resolve_ir;
    _ = other_modules;

    // constriain

    @panic("not implemented");
}
