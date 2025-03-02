const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const resolve = @import("./resolve_imports.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");
const Type = @import("../types/type.zig").Type;
const unify = @import("./check_types/unify.zig");

const ModuleWork = base.ModuleWork;

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

// test "checkTypes - basic type unification" {

//     // Create minimal ResolveIR
//     var module_env = ModuleEnv.init(testing.allocator);

//     const type_id_1 = module_env.newTypeId();
//     const type_id_2 = module_env.newTypeId();

//     const empty_ir = resolve.IR.init(&module_env, testing.allocator);
//     const empty_other_modules = &[_]resolve.IR{};

//     checkTypes(empty_ir, empty_other_modules);

//     // Test that we can perform basic type unification
//     const a_type = Type{ .flex_var = null };
//     const int_type = Type{ .rigid_var = "Int" };
//     try testing.expect(module_env.type_store.get(type_id_1).?.equal(a_type));
//     try testing.expect(module_env.type_store.get(type_id_2).?.equal(int_type));

//     // After unification, both variables should have the rigid type
//     const result = try unify.unify(testing.allocator, module_env, type_id_1, type_id_2);

//     try testing.expect(result.mismatches.items.len == 0);
//     try testing.expect(result.has_changed);
//     try testing.expect(module_env.type_store.get(type_id_1).?.* == .{ .RigidVar = "Int" });
//     try testing.expect(module_env.type_store.subs.get(type_id_2).?.* == .{ .RigidVar = "Int" });
// }
