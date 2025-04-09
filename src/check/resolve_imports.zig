const std = @import("std");
const base = @import("../base.zig");
const can = @import("canonicalize.zig");

const Self = @This();

/// The intermediate representation of a combined Roc program, including all of the
/// relevant modules such a builtins, or imported packages.
pub const IR = @import("resolve_imports/IR.zig");

/// Resolve modules that were nominally imported in silo-compiled modules to
/// real modules based on the file tree for the owning package.
pub fn resolveImports(
    ir: *IR,
    can_ir: *const can.IR,
    other_modules: *const base.ModuleWork(IR).Store,
) void {
    _ = ir;
    _ = can_ir;
    _ = other_modules;

    // TODO: implement
}
