const std = @import("std");
const base = @import("../../base.zig");
const types_mod = @import("../../types.zig");
const can = @import("../canonicalize.zig");
const check_types = @import("../check_types.zig");
const unifier = @import("unify.zig");
const problem = @import("problem.zig");
const snapshot = @import("snapshot.zig");
const occurs = @import("occurs.zig");

const testing = std.testing;
const CIR = can.CIR;
const Var = types_mod.Var;
const Content = types_mod.Content;
const ModuleWork = base.ModuleWork;
const ModuleWorkIdx = base.ModuleWorkIdx;

test "cross-module type checking - monomorphic function" {
    const allocator = testing.allocator;

    // Create module A that exports a simple function
    var module_a_env = base.ModuleEnv.init(allocator);
    defer module_a_env.deinit();

    var module_a_cir = CIR.init(&module_a_env);
    defer module_a_cir.deinit();

    // Create a function: add : I32, I32 -> I32
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const arg1_var = module_a_env.types.freshFromContent(i32_content);
    const arg2_var = module_a_env.types.freshFromContent(i32_content);
    const ret_var = module_a_env.types.freshFromContent(i32_content);

    const func_content = module_a_env.types.mkFuncPure(&[_]Var{ arg1_var, arg2_var }, ret_var);
    _ = module_a_env.types.freshFromContent(func_content);

    // Store this as an integer literal expression (we'll use it as a placeholder)
    // The type of the expression is what matters for cross-module checking
    const func_expr_idx = module_a_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Set the type of this expression to our function type
    // Set the expression's type variable to the function type
    try module_a_env.types.setVarContent(@enumFromInt(@intFromEnum(func_expr_idx)), func_content);

    // Create module B that imports and uses the function from module A
    var module_b_env = base.ModuleEnv.init(allocator);
    defer module_b_env.deinit();

    var module_b_cir = CIR.init(&module_b_env);
    defer module_b_cir.deinit();

    // Register the import of module A
    const module_a_import_idx = try module_b_cir.imports.getOrPut(allocator, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = module_b_cir.store.addExpr(.{
        .e_lookup_external = .{
            .module_idx = module_a_import_idx,
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    });

    // Create a ModuleWork store with both modules
    var module_works = std.MultiArrayList(ModuleWork(CIR)){};
    defer module_works.deinit(allocator);

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(0),
        .work = module_a_cir,
    });

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(1),
        .work = module_b_cir,
    });

    const other_modules = ModuleWork(CIR).Store{ .items = module_works };

    // Type check module B
    var checker = try check_types.init(allocator, &module_b_env.types, &module_b_cir, &other_modules);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);

    // Verify that the external lookup has the correct type
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const resolved = module_b_env.types.resolveVar(external_var);

    try testing.expect(resolved.desc.content == .structure);
    try testing.expect(resolved.desc.content.structure == .fn_pure);

    const func = resolved.desc.content.structure.fn_pure;
    const args = module_b_env.types.getFuncArgsSlice(func.args);
    try testing.expectEqual(@as(usize, 2), args.len);

    // Check that all arguments and return are I32
    for (args) |arg| {
        const arg_resolved = module_b_env.types.resolveVar(arg);
        try testing.expect(arg_resolved.desc.content == .structure);
        try testing.expect(arg_resolved.desc.content.structure == .num);
        try testing.expect(arg_resolved.desc.content.structure.num == .int_precision);
        try testing.expectEqual(types_mod.Num.Int.Precision.i32, arg_resolved.desc.content.structure.num.int_precision);
    }

    const ret_resolved = module_b_env.types.resolveVar(func.ret);
    try testing.expect(ret_resolved.desc.content == .structure);
    try testing.expect(ret_resolved.desc.content.structure == .num);
    try testing.expect(ret_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, ret_resolved.desc.content.structure.num.int_precision);
}

test "cross-module type checking - polymorphic function" {
    const allocator = testing.allocator;

    // Create module A that exports a polymorphic identity function
    var module_a_env = base.ModuleEnv.init(allocator);
    defer module_a_env.deinit();

    var module_a_cir = CIR.init(&module_a_env);
    defer module_a_cir.deinit();

    // Create a function: identity : a -> a
    const type_var_a = module_a_env.types.fresh();
    const func_content = module_a_env.types.mkFuncPure(&[_]Var{type_var_a}, type_var_a);
    _ = module_a_env.types.freshFromContent(func_content);

    // Store this as an integer literal expression (placeholder)
    const func_expr_idx = module_a_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Set the type of this expression to our polymorphic function type
    // Set the expression's type variable to the function type
    try module_a_env.types.setVarContent(@enumFromInt(@intFromEnum(func_expr_idx)), func_content);

    // Create module B that imports and uses the polymorphic function
    var module_b_env = base.ModuleEnv.init(allocator);
    defer module_b_env.deinit();

    var module_b_cir = CIR.init(&module_b_env);
    defer module_b_cir.deinit();

    // Register the import of module A
    const module_a_import_idx = try module_b_cir.imports.getOrPut(allocator, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = module_b_cir.store.addExpr(.{
        .e_lookup_external = .{
            .module_idx = module_a_import_idx,
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    });

    // Create a ModuleWork store with both modules
    var module_works = std.MultiArrayList(ModuleWork(CIR)){};
    defer module_works.deinit(allocator);

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(0),
        .work = module_a_cir,
    });

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(1),
        .work = module_b_cir,
    });

    const other_modules = ModuleWork(CIR).Store{ .items = module_works };

    // Type check module B
    var checker = try check_types.init(allocator, &module_b_env.types, &module_b_cir, &other_modules);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);

    // Verify that the external lookup has a polymorphic type
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const resolved = module_b_env.types.resolveVar(external_var);

    try testing.expect(resolved.desc.content == .structure);
    try testing.expect(resolved.desc.content.structure == .fn_pure);

    const func = resolved.desc.content.structure.fn_pure;
    const args = module_b_env.types.getFuncArgsSlice(func.args);
    try testing.expectEqual(@as(usize, 1), args.len);

    // The function should still be polymorphic (flex var)
    const arg_resolved = module_b_env.types.resolveVar(args[0]);
    const ret_resolved = module_b_env.types.resolveVar(func.ret);

    // Both should resolve to the same flex var
    try testing.expectEqual(arg_resolved.var_, ret_resolved.var_);
    try testing.expect(arg_resolved.desc.content == .flex_var);
}

test "cross-module type checking - record type" {
    const allocator = testing.allocator;

    // Create module A that exports a record type
    var module_a_env = base.ModuleEnv.init(allocator);
    defer module_a_env.deinit();

    var module_a_cir = CIR.init(&module_a_env);
    defer module_a_cir.deinit();

    // Create a record: { x: I32, y: Bool }
    var record_fields = std.ArrayList(types_mod.RecordField).init(allocator);
    defer record_fields.deinit();

    const x_ident = module_a_env.idents.insert(allocator, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = module_a_env.idents.insert(allocator, base.Ident.for_text("y"), base.Region.zero());

    const i32_var = module_a_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .i32 } } });
    const bool_var = module_a_env.types.fresh(); // We'll set this to bool type

    try record_fields.append(.{ .name = x_ident, .var_ = i32_var });
    try record_fields.append(.{ .name = y_ident, .var_ = bool_var });

    const fields_range = module_a_env.types.appendRecordFields(record_fields.items);
    const ext_var = module_a_env.types.freshFromContent(Content{ .structure = .empty_record });

    const record_content = Content{
        .structure = .{
            .record = .{
                .fields = fields_range,
                .ext = ext_var,
            },
        },
    };
    _ = module_a_env.types.freshFromContent(record_content);

    // Store this as an integer literal expression (placeholder)
    const record_expr_idx = module_a_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Set the type of this expression to our record type
    // Set the expression's type variable to the record type
    try module_a_env.types.setVarContent(@enumFromInt(@intFromEnum(record_expr_idx)), record_content);

    // Create module B that imports and uses the record type
    var module_b_env = base.ModuleEnv.init(allocator);
    defer module_b_env.deinit();

    var module_b_cir = CIR.init(&module_b_env);
    defer module_b_cir.deinit();

    // Register the import of module A
    const module_a_import_idx = try module_b_cir.imports.getOrPut(allocator, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = module_b_cir.store.addExpr(.{
        .e_lookup_external = .{
            .module_idx = module_a_import_idx,
            .target_node_idx = @intCast(@intFromEnum(record_expr_idx)),
            .region = base.Region.zero(),
        },
    });

    // Create a ModuleWork store with both modules
    var module_works = std.MultiArrayList(ModuleWork(CIR)){};
    defer module_works.deinit(allocator);

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(0),
        .work = module_a_cir,
    });

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(1),
        .work = module_b_cir,
    });

    const other_modules = ModuleWork(CIR).Store{ .items = module_works };

    // Type check module B
    var checker = try check_types.init(allocator, &module_b_env.types, &module_b_cir, &other_modules);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);

    // Verify that the external lookup has the correct record type
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const resolved = module_b_env.types.resolveVar(external_var);

    try testing.expect(resolved.desc.content == .structure);
    try testing.expect(resolved.desc.content.structure == .record);

    const record = resolved.desc.content.structure.record;
    const fields = module_b_env.types.getRecordFieldsSlice(record.fields);
    try testing.expectEqual(@as(usize, 2), fields.len);

    // Check field names and types
    try testing.expectEqual(x_ident, fields.items(.name)[0]);
    try testing.expectEqual(y_ident, fields.items(.name)[1]);

    const x_resolved = module_b_env.types.resolveVar(fields.items(.var_)[0]);
    try testing.expect(x_resolved.desc.content == .structure);
    try testing.expect(x_resolved.desc.content.structure == .num);
    try testing.expect(x_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, x_resolved.desc.content.structure.num.int_precision);
}

test "cross-module type checking - type mismatch error" {
    const allocator = testing.allocator;

    // Create module A that exports an I32
    var module_a_env = base.ModuleEnv.init(allocator);
    defer module_a_env.deinit();

    var module_a_cir = CIR.init(&module_a_env);
    defer module_a_cir.deinit();

    // Create an I32 type
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };

    // Store this as an integer literal expression
    const i32_expr_idx = module_a_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{ 42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Set the expression's type to I32
    try module_a_env.types.setVarContent(@enumFromInt(@intFromEnum(i32_expr_idx)), i32_content);

    // Create module B that imports the I32 but tries to use it as a String
    var module_b_env = base.ModuleEnv.init(allocator);
    defer module_b_env.deinit();

    var module_b_cir = CIR.init(&module_b_env);
    defer module_b_cir.deinit();

    // Register the import of module A
    const module_a_import_idx = try module_b_cir.imports.getOrPut(allocator, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = module_b_cir.store.addExpr(.{
        .e_lookup_external = .{
            .module_idx = module_a_import_idx,
            .target_node_idx = @intCast(@intFromEnum(i32_expr_idx)),
            .region = base.Region.zero(),
        },
    });

    // Create a string literal expression
    const str_expr = module_b_cir.store.addExpr(.{
        .e_str_segment = .{
            .literal = @enumFromInt(0), // placeholder string literal
            .region = base.Region.zero(),
        },
    });

    // Create a ModuleWork store with both modules
    var module_works = std.MultiArrayList(ModuleWork(CIR)){};
    defer module_works.deinit(allocator);

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(0),
        .work = module_a_cir,
    });

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(1),
        .work = module_b_cir,
    });

    const other_modules = ModuleWork(CIR).Store{ .items = module_works };

    // Type check module B
    var checker = try check_types.init(allocator, &module_b_env.types, &module_b_cir, &other_modules);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);
    _ = try checker.checkExpr(str_expr);

    // Try to unify the imported I32 with a String - this should fail
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const string_var = @as(Var, @enumFromInt(@intFromEnum(str_expr)));

    const result = checker.unify(external_var, string_var);
    try testing.expect(result.isProblem());
}

test "cross-module type checking - polymorphic instantiation" {
    const allocator = testing.allocator;

    // Create module A that exports a polymorphic list function
    var module_a_env = base.ModuleEnv.init(allocator);
    defer module_a_env.deinit();

    var module_a_cir = CIR.init(&module_a_env);
    defer module_a_cir.deinit();

    // Create a function: listLength : List a -> I64
    const type_var_a = module_a_env.types.fresh();
    const list_content = Content{ .structure = .{ .list = type_var_a } };
    const list_var = module_a_env.types.freshFromContent(list_content);
    const i64_var = module_a_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .i64 } } });

    const func_content = module_a_env.types.mkFuncPure(&[_]Var{list_var}, i64_var);
    _ = module_a_env.types.freshFromContent(func_content);

    // Store this as an integer literal expression (placeholder)
    const func_expr_idx = module_a_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Set the type of this expression to our function type
    // Set the expression's type variable to the function type
    try module_a_env.types.setVarContent(@enumFromInt(@intFromEnum(func_expr_idx)), func_content);

    // Create module B that imports and uses the function with a specific type
    var module_b_env = base.ModuleEnv.init(allocator);
    defer module_b_env.deinit();

    var module_b_cir = CIR.init(&module_b_env);
    defer module_b_cir.deinit();

    // Register the import of module A
    const module_a_import_idx = try module_b_cir.imports.getOrPut(allocator, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = module_b_cir.store.addExpr(.{
        .e_lookup_external = .{
            .module_idx = module_a_import_idx,
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    });

    // Create a list of strings to pass to the function
    const str_var = module_b_env.types.freshFromContent(Content{ .structure = .str });
    const str_list_content = Content{ .structure = .{ .list = str_var } };

    const list_expr = module_b_cir.store.addExpr(.{
        .e_list = .{
            .elems = .{ .span = .{ .start = 0, .len = 0 } }, // empty list
            .elem_var = str_var,
            .region = base.Region.zero(),
        },
    });

    // Set the list expression's type
    try module_b_env.types.setVarContent(@enumFromInt(@intFromEnum(list_expr)), str_list_content);

    // Create a ModuleWork store with both modules
    var module_works = std.MultiArrayList(ModuleWork(CIR)){};
    defer module_works.deinit(allocator);

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(0),
        .work = module_a_cir,
    });

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(1),
        .work = module_b_cir,
    });

    const other_modules = ModuleWork(CIR).Store{ .items = module_works };

    // Type check module B
    var checker = try check_types.init(allocator, &module_b_env.types, &module_b_cir, &other_modules);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);
    _ = try checker.checkExpr(list_expr);

    // The polymorphic function should be usable with List(Str)
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const resolved = module_b_env.types.resolveVar(external_var);

    try testing.expect(resolved.desc.content == .structure);
    try testing.expect(resolved.desc.content.structure == .fn_pure);

    // The function should accept any list type
    const func = resolved.desc.content.structure.fn_pure;
    const args = module_b_env.types.getFuncArgsSlice(func.args);
    try testing.expectEqual(@as(usize, 1), args.len);

    // The argument should be a list with a flex var
    const arg_resolved = module_b_env.types.resolveVar(args[0]);
    try testing.expect(arg_resolved.desc.content == .structure);
    try testing.expect(arg_resolved.desc.content.structure == .list);
}

test "cross-module type checking - preserves module A types" {
    const allocator = testing.allocator;

    // Create module A
    var module_a_env = base.ModuleEnv.init(allocator);
    defer module_a_env.deinit();

    var module_a_cir = CIR.init(&module_a_env);
    defer module_a_cir.deinit();

    // Create a flex var in module A
    const flex_var_a = module_a_env.types.fresh();

    // Store this as an integer literal expression (placeholder)
    const flex_expr_idx = module_a_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Set the type of this expression to our flex var
    // Set the expression's type variable to the flex var
    try module_a_env.types.setVarContent(@enumFromInt(@intFromEnum(flex_expr_idx)), .{ .flex_var = null });

    // Remember the original state
    const original_content = module_a_env.types.resolveVar(flex_var_a).desc.content;
    try testing.expect(original_content == .flex_var);

    // Create module B
    var module_b_env = base.ModuleEnv.init(allocator);
    defer module_b_env.deinit();

    var module_b_cir = CIR.init(&module_b_env);
    defer module_b_cir.deinit();

    // Register the import of module A
    const module_a_import_idx = try module_b_cir.imports.getOrPut(allocator, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = module_b_cir.store.addExpr(.{
        .e_lookup_external = .{
            .module_idx = module_a_import_idx,
            .target_node_idx = @intCast(@intFromEnum(flex_expr_idx)),
            .region = base.Region.zero(),
        },
    });

    // Create a concrete type in module B to unify with
    const i32_var = module_b_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .i32 } } });
    const i32_expr = module_b_cir.store.addExpr(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{ 123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, .kind = .i128 },
        .region = base.Region.zero(),
    } });

    // Create a ModuleWork store
    var module_works = std.MultiArrayList(ModuleWork(CIR)){};
    defer module_works.deinit(allocator);

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(0),
        .work = module_a_cir,
    });

    try module_works.append(allocator, .{
        .package_idx = @enumFromInt(0),
        .module_idx = @enumFromInt(1),
        .work = module_b_cir,
    });

    const other_modules = ModuleWork(CIR).Store{ .items = module_works };

    // Type check module B
    var checker = try check_types.init(allocator, &module_b_env.types, &module_b_cir, &other_modules);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);
    _ = try checker.checkExpr(i32_expr);

    // Unify the imported type with I32
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const result = checker.unify(external_var, i32_var);
    try testing.expect(result.isOk());

    // Module A's type should remain unchanged (still a flex var)
    const module_a_after = module_a_env.types.resolveVar(flex_var_a).desc.content;
    try testing.expect(module_a_after == .flex_var);
    try testing.expectEqual(original_content, module_a_after);

    // Module B's imported type should be I32
    const module_b_resolved = module_b_env.types.resolveVar(external_var);
    try testing.expect(module_b_resolved.desc.content == .structure);
    try testing.expect(module_b_resolved.desc.content.structure == .num);
    try testing.expect(module_b_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, module_b_resolved.desc.content.structure.num.int_precision);
}
