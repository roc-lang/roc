const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const base = @import("../../base.zig");
const types = @import("../../types/types.zig");
const store = @import("../../types/store.zig");
const writers = @import("../../types/writers.zig");
const var_name_gen = @import("../../types/var_name_gen.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = base.ModuleEnv;
const TypeWriter = writers.TypeWriter;

test "writeVar with generated names - simple unnamed var" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create an unnamed flex var
    const unnamed_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(unnamed_var);
    try env.types.setVarContent(unnamed_var, .{ .flex_var = null });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, unnamed_var, &env);
    defer allocator.free(result);

    try expectEqualStrings("a", result);
}

test "writeVar with generated names - multiple unnamed vars" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create a function type with unnamed vars: * -> *
    const arg_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(arg_var);
    try env.types.setVarContent(arg_var, .{ .flex_var = null });

    const ret_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(ret_var);
    try env.types.setVarContent(ret_var, .{ .flex_var = null });

    const closure_var: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(closure_var);
    try env.types.setVarContent(closure_var, .{ .structure = .empty_record });

    var args = try store.List(types.Var).initWithCapacity(allocator, 1);
    try args.push(allocator, arg_var);

    const func = types.Func{
        .args = args,
        .ret = ret_var,
        .closure = closure_var,
    };

    const func_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(func_var);
    try env.types.setVarContent(func_var, .{ .structure = .{ .func = func } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, func_var, &env);
    defer allocator.free(result);

    try expectEqualStrings("a -> b", result);
}

test "writeVar with generated names - respects existing names" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create named vars 'a' and 'b'
    const a_ident = env.idents.insert(allocator, Ident.for_text("a"), base.Region.zero());
    const a_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(a_var);
    try env.types.setVarContent(a_var, .{ .flex_var = a_ident });

    const b_ident = env.idents.insert(allocator, Ident.for_text("b"), base.Region.zero());
    const b_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(b_var);
    try env.types.setVarContent(b_var, .{ .flex_var = b_ident });

    // Create unnamed var
    const unnamed_var: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(unnamed_var);
    try env.types.setVarContent(unnamed_var, .{ .flex_var = null });

    // Create a tuple (a, b, *)
    var elems = try store.List(types.Var).initWithCapacity(allocator, 3);
    try elems.push(allocator, a_var);
    try elems.push(allocator, b_var);
    try elems.push(allocator, unnamed_var);

    const tuple = types.Tuple{ .elems = elems };
    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, tuple_var, &env);
    defer allocator.free(result);

    // Should skip 'a' and 'b', use 'c' for the unnamed var
    try expectEqualStrings("(a, b, c)", result);
}

test "writeVar with generated names - record with unnamed field types" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create record { x: *, y: * }
    const x_type: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(x_type);
    try env.types.setVarContent(x_type, .{ .flex_var = null });

    const y_type: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(y_type);
    try env.types.setVarContent(y_type, .{ .flex_var = null });

    var fields = try store.List(types.RecordField).initWithCapacity(allocator, 2);
    try fields.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("x"), base.Region.zero()),
        .ty = x_type,
        .region = base.Region.zero(),
    });
    try fields.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("y"), base.Region.zero()),
        .ty = y_type,
        .region = base.Region.zero(),
    });

    const empty_record: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(empty_record);
    try env.types.setVarContent(empty_record, .{ .structure = .empty_record });
    const record = types.Record{
        .fields = fields,
        .ext = empty_record,
    };
    const record_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(record_var);
    try env.types.setVarContent(record_var, .{ .structure = .{ .record = record } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, record_var, &env);
    defer allocator.free(result);

    try expectEqualStrings("{ x: a, y: b }", result);
}

test "writeVar with generated names - Dict example" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create Dict(k, v) where k and v are named
    const k_ident = env.idents.insert(allocator, Ident.for_text("k"), base.Region.zero());
    const k_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(k_var);
    try env.types.setVarContent(k_var, .{ .flex_var = k_ident });

    const v_ident = env.idents.insert(allocator, Ident.for_text("v"), base.Region.zero());
    const v_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(v_var);
    try env.types.setVarContent(v_var, .{ .flex_var = v_ident });

    // Create an unnamed var that should become 'a'
    const unnamed_var: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(unnamed_var);
    try env.types.setVarContent(unnamed_var, .{ .flex_var = null });

    // Create tuple (k, v, *)
    var elems = try store.List(types.Var).initWithCapacity(allocator, 3);
    try elems.push(allocator, k_var);
    try elems.push(allocator, v_var);
    try elems.push(allocator, unnamed_var);

    const tuple = types.Tuple{ .elems = elems };
    const tuple_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, tuple_var, &env);
    defer allocator.free(result);

    // k and v are taken, so unnamed should get 'a'
    try expectEqualStrings("(k, v, a)", result);
}

test "writeVar with generated names - all single letters taken" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create vars for all letters a-z
    var letter_vars = std.ArrayList(types.Var).init(allocator);
    defer letter_vars.deinit();

    var letter: u8 = 'a';
    while (letter <= 'z') : (letter += 1) {
        var name_buf: [1]u8 = .{letter};
        const ident = env.idents.insert(allocator, Ident.for_text(&name_buf), base.Region.zero());
        const var_: types.Var = @enumFromInt(letter - 'a');
        try env.types.fillInSlotsThru(var_);
        try env.types.setVarContent(var_, .{ .flex_var = ident });
        try letter_vars.append(var_);
    }

    // Create two more unnamed vars
    const unnamed1: types.Var = @enumFromInt(26);
    try env.types.fillInSlotsThru(unnamed1);
    try env.types.setVarContent(unnamed1, .{ .flex_var = null });

    const unnamed2: types.Var = @enumFromInt(27);
    try env.types.fillInSlotsThru(unnamed2);
    try env.types.setVarContent(unnamed2, .{ .flex_var = null });

    // Create tuple with last few named vars and the unnamed ones
    var elems = try store.List(types.Var).initWithCapacity(allocator, 4);
    try elems.push(allocator, letter_vars.items[25]); // 'z'
    try elems.push(allocator, letter_vars.items[24]); // 'y'
    try elems.push(allocator, unnamed1);
    try elems.push(allocator, unnamed2);

    const tuple = types.Tuple{ .elems = elems };
    const tuple_var: types.Var = @enumFromInt(28);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, tuple_var, &env);
    defer allocator.free(result);

    // All single letters taken, should use a2, b2
    try expectEqualStrings("(z, y, a2, b2)", result);
}

test "writeVar with generated names - tag union" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create tag union: [Ok *, Err *]
    const ok_payload: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(ok_payload);
    try env.types.setVarContent(ok_payload, .{ .flex_var = null });

    const err_payload: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(err_payload);
    try env.types.setVarContent(err_payload, .{ .flex_var = null });

    var ok_payloads = try store.List(types.Var).initWithCapacity(allocator, 1);
    try ok_payloads.push(allocator, ok_payload);
    var err_payloads = try store.List(types.Var).initWithCapacity(allocator, 1);
    try err_payloads.push(allocator, err_payload);

    var tags = try store.List(types.Tag).initWithCapacity(allocator, 2);
    try tags.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("Ok"), base.Region.zero()),
        .payloads = ok_payloads,
    });
    try tags.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("Err"), base.Region.zero()),
        .payloads = err_payloads,
    });

    const empty_tag_union: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(empty_tag_union);
    try env.types.setVarContent(empty_tag_union, .{ .structure = .empty_tag_union });
    const tag_union = types.TagUnion{
        .tags = tags,
        .ext = empty_tag_union,
    };
    const tag_union_var: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(tag_union_var);
    try env.types.setVarContent(tag_union_var, .{ .structure = .{ .tag_union = tag_union } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, tag_union_var, &env);
    defer allocator.free(result);

    try expectEqualStrings("[Err a, Ok b]", result);
}

test "writeVar with generated names - nested types" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create List(List(*))
    const inner_elem: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(inner_elem);
    try env.types.setVarContent(inner_elem, .{ .flex_var = null });

    const inner_list: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(inner_list);
    try env.types.setVarContent(inner_list, .{ .structure = .{ .list = inner_elem } });

    const outer_list: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(outer_list);
    try env.types.setVarContent(outer_list, .{ .structure = .{ .list = inner_list } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, outer_list, &env);
    defer allocator.free(result);

    try expectEqualStrings("List(List(a))", result);
}

test "writeVar with generated names - mixed named and unnamed in function" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create function: a, *, b -> * where a and b are named
    const a_ident = env.idents.insert(allocator, Ident.for_text("a"), base.Region.zero());
    const a_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(a_var);
    try env.types.setVarContent(a_var, .{ .flex_var = a_ident });

    const b_ident = env.idents.insert(allocator, Ident.for_text("b"), base.Region.zero());
    const b_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(b_var);
    try env.types.setVarContent(b_var, .{ .flex_var = b_ident });

    const unnamed1: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(unnamed1);
    try env.types.setVarContent(unnamed1, .{ .flex_var = null });

    const unnamed2: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(unnamed2);
    try env.types.setVarContent(unnamed2, .{ .flex_var = null });

    var args = try store.List(types.Var).initWithCapacity(allocator, 3);
    try args.push(allocator, a_var);
    try args.push(allocator, unnamed1);
    try args.push(allocator, b_var);

    const closure_var: types.Var = @enumFromInt(4);
    try env.types.fillInSlotsThru(closure_var);
    try env.types.setVarContent(closure_var, .{ .structure = .empty_record });
    const func = types.Func{
        .args = args,
        .ret = unnamed2,
        .closure = closure_var,
    };

    const func_var: types.Var = @enumFromInt(5);
    try env.types.fillInSlotsThru(func_var);
    try env.types.setVarContent(func_var, .{ .structure = .{ .func = func } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, func_var, &env);
    defer allocator.free(result);

    // a and b are taken, so unnamed vars should be c and d
    try expectEqualStrings("a, c, b -> d", result);
}

test "writeVar with generated names - rigid vars remain unchanged" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create a rigid var 'T' and an unnamed flex var
    const t_ident = env.idents.insert(allocator, Ident.for_text("T"), base.Region.zero());
    const rigid_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(rigid_var);
    try env.types.setVarContent(rigid_var, .{ .rigid_var = t_ident });

    const flex_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(flex_var);
    try env.types.setVarContent(flex_var, .{ .flex_var = null });

    // Create tuple (T, *)
    var elems = try store.List(types.Var).initWithCapacity(allocator, 2);
    try elems.push(allocator, rigid_var);
    try elems.push(allocator, flex_var);

    const tuple = types.Tuple{ .elems = elems };
    const tuple_var: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, tuple_var, &env);
    defer allocator.free(result);

    // Rigid var T should stay as T, unnamed gets 'a'
    try expectEqualStrings("(T, a)", result);
}

test "writeVar with generated names - suffixes skip taken names" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create all single letter vars a-z
    var letter: u8 = 'a';
    while (letter <= 'z') : (letter += 1) {
        var name_buf: [1]u8 = .{letter};
        const ident = env.idents.insert(allocator, Ident.for_text(&name_buf), base.Region.zero());
        const var_: types.Var = @enumFromInt(letter - 'a');
        try env.types.fillInSlotsThru(var_);
        try env.types.setVarContent(var_, .{ .flex_var = ident });
    }

    // Also create a2 and b2
    const a2_ident = env.idents.insert(allocator, Ident.for_text("a2"), base.Region.zero());
    const a2_var: types.Var = @enumFromInt(26);
    try env.types.fillInSlotsThru(a2_var);
    try env.types.setVarContent(a2_var, .{ .flex_var = a2_ident });

    const b2_ident = env.idents.insert(allocator, Ident.for_text("b2"), base.Region.zero());
    const b2_var: types.Var = @enumFromInt(27);
    try env.types.fillInSlotsThru(b2_var);
    try env.types.setVarContent(b2_var, .{ .flex_var = b2_ident });

    // Create unnamed vars
    const unnamed1: types.Var = @enumFromInt(28);
    try env.types.fillInSlotsThru(unnamed1);
    try env.types.setVarContent(unnamed1, .{ .flex_var = null });

    const unnamed2: types.Var = @enumFromInt(29);
    try env.types.fillInSlotsThru(unnamed2);
    try env.types.setVarContent(unnamed2, .{ .flex_var = null });

    // Create tuple (a2, b2, *, *)
    var elems = try store.List(types.Var).initWithCapacity(allocator, 4);
    try elems.push(allocator, a2_var);
    try elems.push(allocator, b2_var);
    try elems.push(allocator, unnamed1);
    try elems.push(allocator, unnamed2);

    const tuple = types.Tuple{ .elems = elems };
    const tuple_var: types.Var = @enumFromInt(30);
    try env.types.fillInSlotsThru(tuple_var);
    try env.types.setVarContent(tuple_var, .{ .structure = .{ .tuple = tuple } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, tuple_var, &env);
    defer allocator.free(result);

    // a2 and b2 are taken, should use c2 and d2
    try expectEqualStrings("(a2, b2, c2, d2)", result);
}

test "writeVar with generated names - error type" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create an error type
    const err_var: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(err_var);
    try env.types.setVarContent(err_var, .err);

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, err_var, &env);
    defer allocator.free(result);

    try expectEqualStrings("Error", result);
}

test "writeVar with generated names - Box type" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create Box(*)
    const inner: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(inner);
    try env.types.setVarContent(inner, .{ .flex_var = null });

    const box_var: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(box_var);
    try env.types.setVarContent(box_var, .{ .structure = .{ .box = inner } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, box_var, &env);
    defer allocator.free(result);

    try expectEqualStrings("Box(a)", result);
}

test "writeVar with generated names - complex nested structure" {
    const allocator = std.testing.allocator;
    var env = ModuleEnv.init(allocator);
    defer env.deinit();

    // Create a complex type: { data: List(*), process: * -> Result(*, *) }
    // where Result is [Ok *, Err *]

    // Create unnamed vars
    const list_elem: types.Var = @enumFromInt(0);
    try env.types.fillInSlotsThru(list_elem);
    try env.types.setVarContent(list_elem, .{ .flex_var = null });

    const process_arg: types.Var = @enumFromInt(1);
    try env.types.fillInSlotsThru(process_arg);
    try env.types.setVarContent(process_arg, .{ .flex_var = null });

    const ok_payload: types.Var = @enumFromInt(2);
    try env.types.fillInSlotsThru(ok_payload);
    try env.types.setVarContent(ok_payload, .{ .flex_var = null });

    const err_payload: types.Var = @enumFromInt(3);
    try env.types.fillInSlotsThru(err_payload);
    try env.types.setVarContent(err_payload, .{ .flex_var = null });

    // Create List(*)
    const list_type: types.Var = @enumFromInt(4);
    try env.types.fillInSlotsThru(list_type);
    try env.types.setVarContent(list_type, .{ .structure = .{ .list = list_elem } });

    // Create Result(*, *) = [Ok *, Err *]
    var ok_payloads = try store.List(types.Var).initWithCapacity(allocator, 1);
    try ok_payloads.push(allocator, ok_payload);
    var err_payloads = try store.List(types.Var).initWithCapacity(allocator, 1);
    try err_payloads.push(allocator, err_payload);

    var tags = try store.List(types.Tag).initWithCapacity(allocator, 2);
    try tags.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("Ok"), base.Region.zero()),
        .payloads = ok_payloads,
    });
    try tags.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("Err"), base.Region.zero()),
        .payloads = err_payloads,
    });

    const empty_tag_union: types.Var = @enumFromInt(5);
    try env.types.fillInSlotsThru(empty_tag_union);
    try env.types.setVarContent(empty_tag_union, .{ .structure = .empty_tag_union });

    const result_type: types.Var = @enumFromInt(6);
    try env.types.fillInSlotsThru(result_type);
    try env.types.setVarContent(result_type, .{ .structure = .{ .tag_union = .{
        .tags = tags,
        .ext = empty_tag_union,
    } } });

    // Create function * -> Result(*, *)
    var args = try store.List(types.Var).initWithCapacity(allocator, 1);
    try args.push(allocator, process_arg);
    const closure_var: types.Var = @enumFromInt(7);
    try env.types.fillInSlotsThru(closure_var);
    try env.types.setVarContent(closure_var, .{ .structure = .empty_record });

    const func_type: types.Var = @enumFromInt(8);
    try env.types.fillInSlotsThru(func_type);
    try env.types.setVarContent(func_type, .{ .structure = .{ .func = .{
        .args = args,
        .ret = result_type,
        .closure = closure_var,
    } } });

    // Create record { data: List(*), process: * -> Result(*, *) }
    var fields = try store.List(types.RecordField).initWithCapacity(allocator, 2);
    try fields.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("data"), base.Region.zero()),
        .ty = list_type,
        .region = base.Region.zero(),
    });
    try fields.push(allocator, .{
        .name = env.idents.insert(allocator, Ident.for_text("process"), base.Region.zero()),
        .ty = func_type,
        .region = base.Region.zero(),
    });

    const empty_record: types.Var = @enumFromInt(9);
    try env.types.fillInSlotsThru(empty_record);
    try env.types.setVarContent(empty_record, .{ .structure = .empty_record });

    const record_var: types.Var = @enumFromInt(10);
    try env.types.fillInSlotsThru(record_var);
    try env.types.setVarContent(record_var, .{ .structure = .{ .record = .{
        .fields = fields,
        .ext = empty_record,
    } } });

    // Write with generated names
    const result = try writers.writeVarWithGeneratedNames(allocator, record_var, &env);
    defer allocator.free(result);

    // Should assign: a to list elem, b to process arg, c to err payload, d to ok payload
    try expectEqualStrings("{ data: List(a), process: b -> [Err c, Ok d] }", result);
}
