//! Tests for rigid variable instantiation in the type system.
//!
//! This module contains tests that verify the correct behavior of rigid type
//! variables during instantiation, particularly for polymorphic functions
//! where type variables need to be properly instantiated with concrete types.

const std = @import("std");
const types = @import("types.zig");
const Store = @import("store.zig").Store;
const instantiate = @import("../check/check_types/instantiate.zig");
const base = @import("../base.zig");

test "rigid variables need instantiation - polymorphic identity function" {
    var store = try Store.init(std.testing.allocator);
    defer store.deinit();

    var regions = try base.Region.List.initCapacity(std.testing.allocator, 256);
    defer regions.deinit(std.testing.allocator);

    // Create a rigid type variable 'a' (like in `identity : a -> a`)
    const rigid_var = try store.fresh();
    const rigid_content = types.Content{ .rigid_var = @bitCast(@as(u32, 1)) }; // arbitrary identifier
    try store.setVarContent(rigid_var, rigid_content);

    // Create the polymorphic function type: a -> a
    const func_content = try store.mkFuncPure(&.{rigid_var}, rigid_var);
    const func_var = try store.freshFromContent(func_content);

    // Verify the function needs instantiation (because it contains rigid vars)
    try std.testing.expect(store.needsInstantiation(func_var));

    // First call site: instantiate for use with integers
    const inst1_var = try instantiate.instantiateVarAlloc(&store, func_var, std.testing.allocator);

    // The instantiated function should still need instantiation
    // (because it has fresh type variables that could be instantiated again)
    try std.testing.expect(store.needsInstantiation(inst1_var));

    // Verify we got a different variable (not the same as original)
    try std.testing.expect(inst1_var != func_var);

    // Second call site: instantiate again for use with strings
    const inst2_var = try instantiate.instantiateVarAlloc(&store, func_var, std.testing.allocator);

    // Should also need instantiation
    try std.testing.expect(store.needsInstantiation(inst2_var));

    // Verify we got different variables for each instantiation
    try std.testing.expect(inst2_var != func_var);
    try std.testing.expect(inst2_var != inst1_var);

    // Now let's verify that if we had marked rigid vars as NOT needing instantiation,
    // we would have gotten the wrong behavior (same variable reused)
    // This is what we're preventing with the fix
}

test "rigid variables need instantiation - multiple type parameters" {
    var store = try Store.init(std.testing.allocator);
    defer store.deinit();

    var regions = try base.Region.List.initCapacity(std.testing.allocator, 256);
    defer regions.deinit(std.testing.allocator);

    // Create rigid type variables 'a' and 'b' (like in `swap : (a, b) -> (b, a)`)
    const rigid_a = try store.fresh();
    const rigid_a_content = types.Content{ .rigid_var = @bitCast(@as(u32, 1)) };
    try store.setVarContent(rigid_a, rigid_a_content);

    const rigid_b = try store.fresh();
    const rigid_b_content = types.Content{ .rigid_var = @bitCast(@as(u32, 2)) };
    try store.setVarContent(rigid_b, rigid_b_content);

    // Create tuple types for the argument and return
    const arg_tuple_var = try store.fresh();
    const arg_elems_range = try store.appendVars(&.{ rigid_a, rigid_b });
    const arg_tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = arg_elems_range } } };
    try store.setVarContent(arg_tuple_var, arg_tuple_content);

    const ret_tuple_var = try store.fresh();
    const ret_elems_range = try store.appendVars(&.{ rigid_b, rigid_a });
    const ret_tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = ret_elems_range } } };
    try store.setVarContent(ret_tuple_var, ret_tuple_content);

    // Create the polymorphic function type: (a, b) -> (b, a)
    const func_content = try store.mkFuncPure(&.{arg_tuple_var}, ret_tuple_var);
    const func_var = try store.freshFromContent(func_content);

    // Verify the function needs instantiation
    try std.testing.expect(store.needsInstantiation(func_var));

    // Instantiate for first use
    const inst1_var = try instantiate.instantiateVarAlloc(&store, func_var, std.testing.allocator);
    try std.testing.expect(inst1_var != func_var);

    // Instantiate for second use
    const inst2_var = try instantiate.instantiateVarAlloc(&store, func_var, std.testing.allocator);
    try std.testing.expect(inst2_var != func_var);
    try std.testing.expect(inst2_var != inst1_var);
}

test "rigid vs flex variable instantiation behavior" {
    var store = try Store.init(std.testing.allocator);
    defer store.deinit();

    // Test that both rigid and flex variables need instantiation
    const rigid_var = try store.fresh();
    try store.setVarContent(rigid_var, types.Content{ .rigid_var = @bitCast(@as(u32, 1)) });
    try std.testing.expect(store.needsInstantiation(rigid_var));

    const flex_var = try store.fresh();
    try store.setVarContent(flex_var, types.Content{ .flex_var = null });
    try std.testing.expect(store.needsInstantiation(flex_var));

    // Test that concrete types don't need instantiation
    const str_var = try store.fresh();
    try store.setVarContent(str_var, types.Content{ .structure = .str });
    try std.testing.expect(!store.needsInstantiation(str_var));

    const int_var = try store.fresh();
    try store.setVarContent(int_var, types.Content{ .structure = .{ .num = .{ .int_precision = .u32 } } });
    try std.testing.expect(!store.needsInstantiation(int_var));
}
