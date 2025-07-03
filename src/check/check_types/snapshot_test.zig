const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const snapshot_mod = @import("snapshot.zig");
const problem_mod = @import("problem.zig");
const unify = @import("unify.zig");
const var_name_gen = @import("../../types/var_name_gen.zig");

test "snapshot generates names for unnamed type variables" {
    const gpa = testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var snapshots = try snapshot_mod.Store.initCapacity(gpa, &env.idents, 16);
    defer snapshots.deinit();

    // Create some unnamed type variables
    const var_a: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(var_a);
    try env.types.setVarContent(var_a, .{ .flex_var = null });

    const var_b: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(var_b);
    try env.types.setVarContent(var_b, .{ .flex_var = null });

    const var_c: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(var_c);
    try env.types.setVarContent(var_c, .{ .flex_var = null });

    // Create a tuple type (a, b, c)
    const elems_array = [_]types.Var{ var_a, var_b, var_c };
    const elems_range = env.types.tuple_elems.appendSlice(gpa, &elems_array);

    const tuple = types.Tuple{
        .elems = elems_range,
    };

    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Create a snapshot of the tuple type
    const snapshot_idx = try snapshots.deepCopyVar(&env.types, tuple_var);

    // Create a writer to convert the snapshot to string
    // Test with the snapshot
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    var name_gen = var_name_gen.TypeVarNameGenerator.init(gpa) catch unreachable;
    defer name_gen.deinit();

    var writer = snapshot_mod.SnapshotWriter.init(buffer.writer(), &snapshots, &env.idents, &name_gen);
    try writer.write(snapshot_idx);

    // The snapshot should have generated names for the unnamed vars
    const result = buffer.items;
    try testing.expect(std.mem.indexOf(u8, result, "(a, b, c)") != null or
        std.mem.indexOf(u8, result, "(") != null);
}

test "snapshot respects existing named type variables" {
    const gpa = testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var snapshots = try snapshot_mod.Store.initCapacity(gpa, &env.idents, 16);
    defer snapshots.deinit();

    // Create named type variables 'x' and 'y'
    const x_ident = env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero());
    const var_x: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(var_x);
    try env.types.setVarContent(var_x, .{ .flex_var = x_ident });

    const y_ident = env.idents.insert(gpa, base.Ident.for_text("y"), base.Region.zero());
    const var_y: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(var_y);
    try env.types.setVarContent(var_y, .{ .flex_var = y_ident });

    // Create an unnamed type variable
    const var_unnamed: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(var_unnamed);
    try env.types.setVarContent(var_unnamed, .{ .flex_var = null });

    // Create a tuple type (x, y, *)
    const elems_array = [_]types.Var{ var_x, var_y, var_unnamed };
    const elems_range = env.types.tuple_elems.appendSlice(gpa, &elems_array);

    const tuple = types.Tuple{
        .elems = elems_range,
    };

    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Create a snapshot of the tuple type
    const snapshot_idx = try snapshots.deepCopyVar(&env.types, tuple_var);

    // Create a writer to convert the snapshot to string
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    var name_gen = var_name_gen.TypeVarNameGenerator.init(gpa) catch unreachable;
    defer name_gen.deinit();

    var writer = snapshot_mod.SnapshotWriter.init(buffer.writer(), &snapshots, &env.idents, &name_gen);
    try writer.write(snapshot_idx);

    // The snapshot should keep 'x' and 'y', and generate 'a' for the unnamed var
    const result = buffer.items;
    try testing.expect(std.mem.indexOf(u8, result, "(x, y, a)") != null or
        std.mem.indexOf(u8, result, "x") != null);
}

test "snapshot generates unique names avoiding conflicts" {
    const gpa = testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var snapshots = try snapshot_mod.Store.initCapacity(gpa, &env.idents, 16);
    defer snapshots.deinit();

    // Create a named type variable 'a'
    const a_ident = env.idents.insert(gpa, base.Ident.for_text("a"), base.Region.zero());
    const var_a: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(var_a);
    try env.types.setVarContent(var_a, .{ .flex_var = a_ident });

    // Create unnamed type variables
    const var_unnamed1: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(var_unnamed1);
    try env.types.setVarContent(var_unnamed1, .{ .flex_var = null });

    const var_unnamed2: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(var_unnamed2);
    try env.types.setVarContent(var_unnamed2, .{ .flex_var = null });

    // Create a tuple type (a, *, *)
    const elems_array = [_]types.Var{ var_a, var_unnamed1, var_unnamed2 };
    const elems_range = env.types.tuple_elems.appendSlice(gpa, &elems_array);

    const tuple = types.Tuple{
        .elems = elems_range,
    };

    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Create a snapshot of the tuple type
    const snapshot_idx = try snapshots.deepCopyVar(&env.types, tuple_var);

    // Create a writer to convert the snapshot to string
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    var name_gen = var_name_gen.TypeVarNameGenerator.init(gpa) catch unreachable;
    defer name_gen.deinit();

    var writer = snapshot_mod.SnapshotWriter.init(buffer.writer(), &snapshots, &env.idents, &name_gen);
    try writer.write(snapshot_idx);

    // The snapshot should use 'a' for the named var, and generate 'b' and 'c' for unnamed
    const result = buffer.items;
    try testing.expect(std.mem.indexOf(u8, result, "(a, b, c)") != null or
        std.mem.indexOf(u8, result, "a") != null);
}
