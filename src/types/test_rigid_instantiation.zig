//! Tests for rigid variable instantiation in the type system.
//!
//! This module contains tests that verify the correct behavior of rigid type
//! variables during instantiation, particularly for polymorphic functions
//! where type variables need to be properly instantiated with concrete types.

const std = @import("std");
const base = @import("base");

// const Store = @import("store.zig").Store;
// const Instantiate = @import("instantiate.zig").Instantiate;
// const Content = @import("types.zig").Content;

// test "rigid variables need instantiation - multiple type parameters" {
//     const gpa = std.testing.allocator;

//     var store = try Store.init(gpa);
//     defer store.deinit();

//     var idents = try base.Ident.Store.initCapacity(gpa, 256);
//     defer idents.deinit(gpa);

//     var var_subs = Instantiate.SeenVars.init(gpa);
//     defer var_subs.deinit();

//     var rigid_var_subs = try Instantiate.RigidSubstitutions.init(gpa);
//     defer rigid_var_subs.deinit(gpa);

//     var instantiate = Instantiate.init(&store, &idents, &var_subs);
//     var instantiate_ctx = Instantiate.Ctx{ .rigid_var_subs = &rigid_var_subs };

//     const a_ident = try idents.insert(gpa, base.Ident.for_text("a"));
//     const b_ident = try idents.insert(gpa, base.Ident.for_text("a"));

//     // Create rigid type variables 'a' and 'b' (like in `swap : (a, b) -> (b, a)`)
//     const rigid_a = try store.freshFromContent(.{ .rigid_var = a_ident });
//     const rigid_b = try store.freshFromContent(.{ .rigid_var = b_ident });

//     // Create tuple types for the argument and return
//     const arg_elems_range = try store.appendVars(&.{ rigid_a, rigid_b });
//     const arg_tuple_var = try store.freshFromContent(Content{
//         .structure = .{ .tuple = .{ .elems = arg_elems_range } },
//     });

//     const ret_elems_range = try store.appendVars(&.{ rigid_b, rigid_a });
//     const ret_tuple_var = try store.freshFromContent(Content{
//         .structure = .{ .tuple = .{ .elems = ret_elems_range } },
//     });

//     // Create the polymorphic function type: (a, b) -> (b, a)
//     const func_content = try store.mkFuncPure(&.{arg_tuple_var}, ret_tuple_var);
//     const func_var = try store.freshFromContent(func_content);

//     // Verify the function needs instantiation
//     try std.testing.expect(store.needsInstantiation(func_var));

//     var empty_store = try base.Ident.Store.initCapacity(gpa, 10);
//     defer empty_store.deinit(gpa);

//     // Instantiate for first use
//     const inst1_var = try instantiate.instantiateVar(func_var, &instantiate_ctx);
//     try std.testing.expect(inst1_var != func_var);

//     // Clear maps between instantiation runs
//     var_subs.clearRetainingCapacity();
//     rigid_var_subs.items.clearRetainingCapacity();

//     // Instantiate for second use
//     const inst2_var = try instantiate.instantiateVar(func_var, &instantiate_ctx);
//     try std.testing.expect(inst2_var != func_var);
//     try std.testing.expect(inst2_var != inst1_var);

//     // TODO: Test different rigid instantiation behavior
// }

// test "needsInstantiation" {
//     const gpa = std.testing.allocator;

//     var store = try Store.init(gpa);
//     defer store.deinit();

//     var idents = try base.Ident.Store.initCapacity(gpa, 256);
//     defer idents.deinit(gpa);

//     var var_subs = Instantiate.SeenVars.init(gpa);
//     defer var_subs.deinit();

//     var rigid_var_subs = try Instantiate.RigidSubstitutions.init(gpa);
//     defer rigid_var_subs.deinit(gpa);

//     const a_ident = try idents.insert(gpa, base.Ident.for_text("a"));

//     // Test that both rigid and flex variables need instantiation
//     const rigid_var = try store.freshFromContent(.{ .rigid_var = a_ident });
//     try std.testing.expect(store.needsInstantiation(rigid_var));

//     const flex_var = try store.fresh();
//     try std.testing.expect(store.needsInstantiation(flex_var));

//     // Test that concrete types don't need instantiation
//     const str_var = try store.freshFromContent(Content{ .structure = .str });
//     try std.testing.expect(!store.needsInstantiation(str_var));

//     const int_var = try store.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .u32 } } });
//     try std.testing.expect(!store.needsInstantiation(int_var));
// }
