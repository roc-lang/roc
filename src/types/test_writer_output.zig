const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const types = @import("../types.zig");
const writers = @import("writers.zig");

test "TypeWriter generates names for unnamed flex vars" {
    const allocator = testing.allocator;

    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();

    // Create some unnamed flex vars
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
    const elems_range = env.types.tuple_elems.appendSlice(allocator, &elems_array);

    const tuple = types.Tuple{
        .elems = elems_range,
    };

    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write the type with generated names
    const result = try writers.writeType(allocator, tuple_var, &env);
    defer allocator.free(result);

    // Should generate names like (a, b, c) instead of (*, *, *)
    try testing.expectEqualStrings("(a, b, c)", result);
}

test "TypeWriter respects existing named type variables" {
    const allocator = testing.allocator;

    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();

    // Create named type variables 'x' and 'y'
    const x_ident = env.idents.insert(allocator, base.Ident.for_text("x"), base.Region.zero());
    const var_x: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(var_x);
    try env.types.setVarContent(var_x, .{ .flex_var = x_ident });

    const y_ident = env.idents.insert(allocator, base.Ident.for_text("y"), base.Region.zero());
    const var_y: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(var_y);
    try env.types.setVarContent(var_y, .{ .flex_var = y_ident });

    // Create an unnamed type variable
    const var_unnamed: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(var_unnamed);
    try env.types.setVarContent(var_unnamed, .{ .flex_var = null });

    // Create a tuple type (x, y, a)
    const elems_array = [_]types.Var{ var_x, var_y, var_unnamed };
    const elems_range = env.types.tuple_elems.appendSlice(allocator, &elems_array);

    const tuple = types.Tuple{
        .elems = elems_range,
    };

    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write the type with generated names
    const result = try writers.writeType(allocator, tuple_var, &env);
    defer allocator.free(result);

    // Should keep 'x' and 'y', and generate 'a' for the unnamed var
    try testing.expectEqualStrings("(x, y, a)", result);
}

test "TypeWriter avoids name conflicts" {
    const allocator = testing.allocator;

    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();

    // Create a named type variable 'a'
    const a_ident = env.idents.insert(allocator, base.Ident.for_text("a"), base.Region.zero());
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

    // Create a tuple type (a, b, c) where 'a' is already taken
    const elems_array = [_]types.Var{ var_a, var_unnamed1, var_unnamed2 };
    const elems_range = env.types.tuple_elems.appendSlice(allocator, &elems_array);

    const tuple = types.Tuple{
        .elems = elems_range,
    };

    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write the type with generated names
    const result = try writers.writeType(allocator, tuple_var, &env);
    defer allocator.free(result);

    // Should use 'a' for the named var, and generate 'b' and 'c' for unnamed
    try testing.expectEqualStrings("(a, b, c)", result);
}

test "TypeWriter handles function types" {
    const allocator = testing.allocator;

    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();

    // Create unnamed type variables for arg and return
    const arg_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(arg_var);
    try env.types.setVarContent(arg_var, .{ .flex_var = null });

    const ret_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(ret_var);
    try env.types.setVarContent(ret_var, .{ .flex_var = null });

    // Create a function type: a -> b
    const args_array = [_]types.Var{arg_var};
    const args_range = env.types.func_args.appendSlice(allocator, &args_array);

    const func = types.Func{
        .args = args_range,
        .ret = ret_var,
    };

    const func_var: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(func_var);
    try env.types.setVarContent(func_var, .{ .structure = .{ .fn_pure = func } });

    // Write the type with generated names
    const result = try writers.writeType(allocator, func_var, &env);
    defer allocator.free(result);

    // Should generate names like "a -> b" instead of "* -> *"
    try testing.expectEqualStrings("a -> b", result);
}

test "TypeWriter handles list types" {
    const allocator = testing.allocator;

    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();

    // Create an unnamed type variable for list element
    const elem_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(elem_var);
    try env.types.setVarContent(elem_var, .{ .flex_var = null });

    // Create a list type: List(a)
    const list_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(list_var);
    try env.types.setVarContent(list_var, .{ .structure = .{ .list = elem_var } });

    // Write the type with generated names
    const result = try writers.writeType(allocator, list_var, &env);
    defer allocator.free(result);

    // Should generate "List(a)" instead of "List(*)"
    try testing.expectEqualStrings("List(a)", result);
}
