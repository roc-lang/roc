const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const types = @import("../types.zig");
const can = @import("canonicalize.zig");
const unify = @import("check_types/unify.zig");
const resolve = @import("resolve_imports.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const ModuleWork = base.ModuleWork;

/// Solves for the types of expressions in the ResolveIR and populates this
/// information in the module's type store.
pub fn checkTypes(
    type_store: *types.Store,
    resolve_ir: *const resolve.IR,
    other_modules: *const ModuleWork(resolve.IR).Store,
    other_typestores: *const ModuleWork(types.Store).Store,
) void {
    _ = type_store;
    _ = resolve_ir;
    _ = other_modules;
    _ = other_typestores;

    // TODO: implement
}

test "checkTypes - basic type unification" {
    const gpa = testing.allocator;

    var module_env = ModuleEnv.init(gpa);
    defer module_env.deinit();
    var can_irs = ModuleWork(can.IR).Store.fromCanIrs(
        gpa,
        &.{ModuleWork(can.IR){
            .package_idx = @enumFromInt(1),
            .module_idx = @enumFromInt(0),
            .work = can.IR.init(module_env),
        }},
    );
    defer can_irs.deinit(gpa);

    var type_stores = ModuleWork(types.Store).Store.initFromCanIrs(gpa, &can_irs);
    defer type_stores.deinit(gpa);

    var resolve_irs = ModuleWork(resolve.IR).Store.initFromCanIrs(gpa, &can_irs);
    defer resolve_irs.deinit(gpa);

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var env = can_irs.getWork(@enumFromInt(0)).env;
    const resolve_ir = resolve_irs.getWork(@enumFromInt(0));
    const type_store = type_stores.getWork(@enumFromInt(0));

    checkTypes(type_store, resolve_ir, &resolve_irs, &type_stores);

    // Test that we can perform basic type unification
    const flex = types.Content{ .flex_var = null };

    const rigid_name = env.idents.insert(env.gpa, Ident.for_text("b"), Region.zero());
    const rigid = types.Content{ .rigid_var = rigid_name };

    const a_type_var = type_store.freshFromContent(flex);
    const b_type_var = type_store.freshFromContent(rigid);

    // After unification, both variables should have the rigid type
    const result = unify.unify(type_store, &scratch, a_type_var, b_type_var);

    try testing.expectEqual(.ok, result);
    try testing.expectEqual(rigid, type_store.resolveVar(a_type_var).desc.content);
    try testing.expectEqual(rigid, type_store.resolveVar(b_type_var).desc.content);
}
