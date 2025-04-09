const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const type_mod = @import("../types/type.zig");
const can = @import("canonicalize.zig");
const unify = @import("check_types/unify.zig");
const resolve = @import("resolve_imports.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const ModuleWork = base.ModuleWork;
const Type = type_mod.Type;

/// Solves for the types of expressions in the ResolveIR and populates this
/// information in the module's type store.
pub fn checkTypes(
    type_store: *Type.Store,
    resolve_ir: *const resolve.IR,
    other_modules: *const ModuleWork(resolve.IR).Store,
    other_typestores: *const ModuleWork(Type.Store).Store,
) void {
    _ = type_store;
    _ = resolve_ir;
    _ = other_modules;
    _ = other_typestores;

    // TODO: implement
}

test "checkTypes - basic type unification" {
    const gpa = testing.allocator;

    var can_irs = ModuleWork(can.IR).Store.fromCanIrs(
        gpa,
        &.{ModuleWork(can.IR){
            .package_idx = @enumFromInt(1),
            .module_idx = @enumFromInt(0),
            .work = can.IR.init(gpa),
        }},
    );
    defer can_irs.deinit(gpa);

    var type_stores = ModuleWork(Type.Store).Store.initFromCanIrs(gpa, &can_irs);
    defer type_stores.deinit(gpa);

    var resolve_irs = ModuleWork(resolve.IR).Store.initFromCanIrs(gpa, &can_irs);
    defer resolve_irs.deinit(gpa);

    var env = &can_irs.getWork(@enumFromInt(0)).env;
    const resolve_ir = resolve_irs.getWork(@enumFromInt(0));
    const type_store = type_stores.getWork(@enumFromInt(0));

    const type_id_1 = type_store.fresh();
    const type_id_2 = type_store.fresh();

    checkTypes(type_store, resolve_ir, &resolve_irs, &type_stores);

    // Test that we can perform basic type unification
    const a_type = Type{ .flex_var = null };
    const int_name = env.idents.insert(env.gpa, Ident.for_text("Int"), Region.zero());
    const int_type = Type{ .rigid_var = int_name };

    type_store.set(type_id_1, a_type);
    type_store.set(type_id_2, int_type);

    try testing.expectEqual(type_store.get(type_id_1).*, a_type);
    try testing.expectEqual(type_store.get(type_id_2).*, int_type);

    // // After unification, both variables should have the rigid type
    // const result = try unify.unify(gpa, type_store, type_id_1, type_id_2);

    // try testing.expect(result.mismatches.items.len == 0);
    // try testing.expect(result.has_changed);
    // try testing.expectEqual(type_store.get(type_id_1).*, int_type);
    // try testing.expectEqual(type_store.get(type_id_2).*, int_type);
}
