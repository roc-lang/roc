const std = @import("std");
const base = @import("../base.zig");
const typecheck = @import("typecheck.zig");
const resolve = @import("resolve_imports.zig");

pub const TypeStore = struct {};

pub fn checkTypes(
    resolve_ir: *const resolve.IR,
    other_modules: *const base.ModuleWork(resolve.IR).Store,
    other_typestores: *const base.ModuleWork(TypeStore).Store,
) void {
    _ = resolve_ir;
    _ = other_modules;
    _ = other_typestores;

    @panic("not implemented");
}
