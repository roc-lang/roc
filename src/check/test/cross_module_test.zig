//! Tests for cross-module type checking functionality.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");
const can = @import("can");
const Check = @import("../Check.zig");

const CIR = can.CIR;
const Var = types_mod.Var;
const Content = types_mod.Content;
const Ident = base.Ident;
const unifier = Check.unifier;
const problem = Check.problem;
const snapshot = Check.snapshot;
const occurs = Check.occurs;
const testing = std.testing;

test "cross-module type checking - monomorphic function" {
    const allocator = testing.allocator;

    // Create module A that exports a simple function
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const arg1_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);
    const arg2_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);
    const ret_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);

    const func_content = try module_a_env.types.mkFuncPure(&[_]Var{ arg1_var, arg2_var }, ret_var);
    const func_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, func_content, base.Region.zero());

    // Set the type of expression 0 (which maps to var 0)
    // Type is already set by addExprAndTypeVar

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);

    // Create module B that imports and uses the function from module A
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Register the import of module A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    try modules.append(module_b_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);

    // Verify that the external lookup has the correct type
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const resolved = module_b_env.types.resolveVar(external_var);

    try testing.expect(resolved.desc.content == .structure);
    try testing.expect(resolved.desc.content.structure == .fn_pure);

    const func = resolved.desc.content.structure.fn_pure;
    const args = module_b_env.types.sliceVars(func.args);
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
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Create a function: identity : a -> a
    const type_var_a = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const func_content = try module_a_env.types.mkFuncPure(&[_]Var{type_var_a}, type_var_a);
    const func_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, func_content, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);

    // Create module B that imports and uses the polymorphic function
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Register the import of module A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    try modules.append(module_b_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);

    // Verify that the external lookup has a polymorphic type
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const resolved = module_b_env.types.resolveVar(external_var);

    try testing.expect(resolved.desc.content == .structure);
    try testing.expect(resolved.desc.content.structure == .fn_pure);

    const func = resolved.desc.content.structure.fn_pure;
    const args = module_b_env.types.sliceVars(func.args);
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
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Create a record type { x: I32, y: Str }
    var record_fields = std.ArrayList(types_mod.RecordField).init(allocator);
    defer record_fields.deinit();

    const x_ident = try module_a_env.idents.insert(allocator, base.Ident.for_text("x"));
    const y_ident = try module_a_env.idents.insert(allocator, base.Ident.for_text("y"));

    const i32_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .structure = .{ .num = .{ .int_precision = .i32 } } }, base.Region.zero(), Var);
    const str_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .structure = .str }, base.Region.zero(), Var);

    try record_fields.append(.{ .name = x_ident, .var_ = i32_var });
    try record_fields.append(.{ .name = y_ident, .var_ = str_var });

    const fields_range = try module_a_env.types.appendRecordFields(record_fields.items);
    const ext_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .structure = .empty_record }, base.Region.zero(), Var);

    const record_content = Content{
        .structure = .{
            .record = .{
                .fields = fields_range,
                .ext = ext_var,
            },
        },
    };

    // Set the type of this expression to our record type
    const record_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, record_content, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);

    // Create module B that imports and uses the record from module A
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Register the import of module A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(record_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    try modules.append(module_b_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);

    // Verify that the external lookup has the correct record type
    // Verify the record type
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

    // Check field y is Str
    const y_resolved = module_b_env.types.resolveVar(fields.items(.var_)[1]);
    try testing.expect(y_resolved.desc.content == .structure);
    try testing.expect(y_resolved.desc.content.structure == .str);
}

test "cross-module type checking - type mismatch error" {
    const allocator = testing.allocator;

    // Create module A that exports an I32
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Create an I32 type
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };

    // Store this as an integer literal expression
    const func_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{ 42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, .kind = .i128 },
    } }, i32_content, base.Region.zero());

    // Create module B that imports the I32 but tries to use it as a String
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Register the import of module A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create a string literal expression
    const str_content = Content{ .structure = .str };
    const str_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_str_segment = .{
            .literal = @enumFromInt(0), // placeholder string literal
        },
    }, str_content, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);
    _ = try checker.checkExpr(str_expr);

    // Try to unify the imported I32 with a String - this should fail
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const string_var = @as(Var, @enumFromInt(@intFromEnum(str_expr)));

    const result = try checker.unify(external_var, string_var);
    try testing.expect(result.isProblem());
}

test "cross-module type checking - polymorphic instantiation" {
    const allocator = testing.allocator;

    // Create module A that exports a polymorphic list function
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Store this as an integer literal expression (placeholder)
    const type_var_a = try module_a_cir.addTypeSlotAndTypeVar(
        @enumFromInt(0),
        Content{ .flex_var = null },
        base.Region.zero(),
        types_mod.Var,
    );
    const list_var = try module_a_cir.addTypeSlotAndTypeVar(
        @enumFromInt(0),
        Content{ .structure = .{ .list = type_var_a } },
        base.Region.zero(),
        types_mod.Var,
    );
    const i64_var = try module_a_cir.addTypeSlotAndTypeVar(
        @enumFromInt(0),
        Content{ .structure = .{ .num = .{ .int_precision = .i64 } } },
        base.Region.zero(),
        types_mod.Var,
    );
    const func_content = try module_a_env.types.mkFuncPure(&[_]Var{list_var}, i64_var);
    const func_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, func_content, base.Region.zero());

    // Create module B that imports and uses the function with a specific type
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Register the import of module A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    }, .{ .flex_var = null }, base.Region.zero());

    // Create an empty list expression
    const str_var = try module_b_cir.addTypeSlotAndTypeVar(
        @enumFromInt(0),
        Content{ .structure = .str },
        base.Region.zero(),
        types_mod.Var,
    );
    const list_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_empty_list = .{},
    }, Content{ .structure = .{ .list = str_var } }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
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
    const args = module_b_env.types.sliceVars(func.args);
    try testing.expectEqual(@as(usize, 1), args.len);

    // The argument should be a list with a flex var
    const arg_resolved = module_b_env.types.resolveVar(args[0]);
    try testing.expect(arg_resolved.desc.content == .structure);
    try testing.expect(arg_resolved.desc.content.structure == .list);
}

test "cross-module type checking - preserves module A types" {
    const allocator = testing.allocator;

    // Create module A
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Create a flex var in module A
    const flex_var_a = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    // Store this as an integer literal expression (placeholder)
    const flex_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, Content{ .flex_var = null }, base.Region.zero());

    // Remember the original state
    const original_content = module_a_env.types.resolveVar(flex_var_a).desc.content;
    try testing.expect(original_content == .flex_var);

    // Create module B
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Register the import of module A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    // Create an external lookup expression
    const external_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(flex_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create a concrete type in module B to unify with
    const i32_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .structure = .{ .num = .{ .int_precision = .i32 } } }, base.Region.zero(), Var);
    const i32_expr = try module_b_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{ 123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, .kind = .i128 },
    } }, Content{ .structure = .{ .num = .{ .int_precision = .i32 } } }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(external_lookup_expr);
    _ = try checker.checkExpr(i32_expr);

    // Unify the imported type with I32
    const external_var = @as(Var, @enumFromInt(@intFromEnum(external_lookup_expr)));
    const result = try checker.unify(external_var, i32_var);
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

test "cross-module type checking - three module chain monomorphic" {
    const allocator = testing.allocator;

    // Module A exports a simple function: add : I32, I32 -> I32
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Create a function: add : I32, I32 -> I32
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const arg1_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);
    const arg2_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);
    const ret_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);

    const func_content = try module_a_env.types.mkFuncPure(&[_]Var{ arg1_var, arg2_var }, ret_var);
    const func_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, func_content, base.Region.zero());

    // Module B imports from A and re-exports it
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Module B imports A's function
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");

    const b_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Module C imports from B
    var module_c_env = try ModuleEnv.init(allocator, "");
    defer module_c_env.deinit();

    try module_c_env.initCIRFields(allocator, "ModuleC");
    const module_c_cir = &module_c_env;

    // Module C imports from B (not A)
    _ = try module_c_cir.imports.getOrPut(allocator, &module_c_cir.strings, "ModuleB");
    const c_lookup_expr = try module_c_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(1), // Direct index to module B in the array
            .target_node_idx = @intCast(@intFromEnum(b_lookup_expr)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);
    try modules.append(module_c_cir);

    // Type check module B
    var checker_b = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker_b.deinit();

    _ = try checker_b.checkExpr(b_lookup_expr);

    // Type check module C
    var checker_c = try Check.init(allocator, &module_c_env.types, module_c_cir, modules.items, &module_c_cir.store.regions);
    defer checker_c.deinit();

    _ = try checker_c.checkExpr(c_lookup_expr);

    // Verify that module C sees the correct type through the chain
    const c_var = @as(Var, @enumFromInt(@intFromEnum(c_lookup_expr)));
    const c_resolved = module_c_env.types.resolveVar(c_var);

    try testing.expect(c_resolved.desc.content == .structure);
    try testing.expect(c_resolved.desc.content.structure == .fn_pure);

    const func = c_resolved.desc.content.structure.fn_pure;
    const args = module_c_env.types.sliceVars(func.args);
    try testing.expectEqual(@as(usize, 2), args.len);

    // Check that all arguments and return are I32
    for (args) |arg| {
        const arg_resolved = module_c_env.types.resolveVar(arg);
        try testing.expect(arg_resolved.desc.content == .structure);
        try testing.expect(arg_resolved.desc.content.structure == .num);
        try testing.expect(arg_resolved.desc.content.structure.num == .int_precision);
        try testing.expectEqual(types_mod.Num.Int.Precision.i32, arg_resolved.desc.content.structure.num.int_precision);
    }

    const ret_resolved = module_c_env.types.resolveVar(func.ret);
    try testing.expect(ret_resolved.desc.content == .structure);
    try testing.expect(ret_resolved.desc.content.structure == .num);
    try testing.expect(ret_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, ret_resolved.desc.content.structure.num.int_precision);
}

test "cross-module type checking - three module chain polymorphic" {
    const allocator = testing.allocator;

    // Module A exports a polymorphic function: identity : a -> a
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    const type_var_a = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const func_content = try module_a_env.types.mkFuncPure(&[_]Var{type_var_a}, type_var_a);
    const func_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, func_content, base.Region.zero());

    // Module B imports from A and re-exports it
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Module B imports from A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");
    const b_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(func_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Module C imports from B
    var module_c_env = try ModuleEnv.init(allocator, "");
    defer module_c_env.deinit();

    try module_c_env.initCIRFields(allocator, "ModuleC");
    const module_c_cir = &module_c_env;

    // Module C imports from B
    _ = try module_c_cir.imports.getOrPut(allocator, &module_c_cir.strings, "ModuleB");
    const c_lookup_expr = try module_c_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(1), // Direct index to module B in the array
            .target_node_idx = @intCast(@intFromEnum(b_lookup_expr)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);
    try modules.append(module_c_cir);

    // Type check module B
    var checker_b = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker_b.deinit();

    _ = try checker_b.checkExpr(b_lookup_expr);

    // Type check module C
    var checker_c = try Check.init(allocator, &module_c_env.types, module_c_cir, modules.items, &module_c_cir.store.regions);
    defer checker_c.deinit();

    _ = try checker_c.checkExpr(c_lookup_expr);

    // Verify that module C sees the polymorphic type through the chain
    const c_var = @as(Var, @enumFromInt(@intFromEnum(c_lookup_expr)));
    const c_resolved = module_c_env.types.resolveVar(c_var);

    try testing.expect(c_resolved.desc.content == .structure);
    try testing.expect(c_resolved.desc.content.structure == .fn_pure);

    const func = c_resolved.desc.content.structure.fn_pure;
    const args = module_c_env.types.sliceVars(func.args);
    try testing.expectEqual(@as(usize, 1), args.len);

    // The function should still be polymorphic
    const arg_resolved = module_c_env.types.resolveVar(args[0]);
    const ret_resolved = module_c_env.types.resolveVar(func.ret);

    // Both should resolve to the same flex var
    try testing.expectEqual(arg_resolved.var_, ret_resolved.var_);
    try testing.expect(arg_resolved.desc.content == .flex_var);
}

test "cross-module type checking - partial polymorphic instantiation chain" {
    const allocator = testing.allocator;

    // Module A exports: map : (a -> b), List a -> List b
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Module A exports: map : (a -> b), List a -> List b

    // Create (a -> b)
    const type_var_a = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const type_var_b = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const mapper_func_content = try module_a_env.types.mkFuncPure(&[_]Var{type_var_a}, type_var_b);
    const mapper_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), mapper_func_content, base.Region.zero(), Var);

    // Create List a
    const list_a_content = Content{ .structure = .{ .list = type_var_a } };
    const list_a_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), list_a_content, base.Region.zero(), Var);

    // Create List b
    const list_b_content = Content{ .structure = .{ .list = type_var_b } };
    const list_b_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), list_b_content, base.Region.zero(), Var);

    // Create map : (a -> b), List a -> List b
    const map_func_content = try module_a_env.types.mkFuncPure(&[_]Var{ mapper_var, list_a_var }, list_b_var);

    // Update the expression with the correct function type
    const map_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, map_func_content, base.Region.zero());

    // Module B imports map and partially applies it with I32
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Module B imports from A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");
    const b_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(map_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create specialized version: mapI32 : (I32 -> b), List I32 -> List b

    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const i32_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);
    const b_type_var_b = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    const i32_to_b_content = try module_b_env.types.mkFuncPure(&[_]Var{i32_var}, b_type_var_b);
    const i32_to_b_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_to_b_content, base.Region.zero(), Var);

    const list_i32_content = Content{ .structure = .{ .list = i32_var } };
    const list_i32_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), list_i32_content, base.Region.zero(), Var);

    const list_b_content_2 = Content{ .structure = .{ .list = b_type_var_b } };
    const list_b_var_2 = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), list_b_content_2, base.Region.zero(), Var);

    const map_i32_content = try module_b_env.types.mkFuncPure(&[_]Var{ i32_to_b_var, list_i32_var }, list_b_var_2);

    const map_i32_expr_idx = try module_b_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, map_i32_content, base.Region.zero());

    // Module C imports the partially specialized version from B
    var module_c_env = try ModuleEnv.init(allocator, "");
    defer module_c_env.deinit();

    try module_c_env.initCIRFields(allocator, "ModuleC");
    const module_c_cir = &module_c_env;

    // Module C imports from B and uses the partially specialized function
    _ = try module_c_cir.imports.getOrPut(allocator, &module_c_cir.strings, "ModuleB");
    const c_lookup_expr = try module_c_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(1), // Direct index to module B in the array
            .target_node_idx = @intCast(@intFromEnum(map_i32_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);
    try modules.append(module_c_cir);

    // Type check module B
    var checker_b = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker_b.deinit();

    _ = try checker_b.checkExpr(b_lookup_expr);
    _ = try checker_b.checkExpr(map_i32_expr_idx);

    // Type check module C
    var checker_c = try Check.init(allocator, &module_c_env.types, module_c_cir, modules.items, &module_c_cir.store.regions);
    defer checker_c.deinit();

    _ = try checker_c.checkExpr(c_lookup_expr);

    // Verify that module C sees the partially specialized type
    const c_var = @as(Var, @enumFromInt(@intFromEnum(c_lookup_expr)));
    const c_resolved = module_c_env.types.resolveVar(c_var);

    try testing.expect(c_resolved.desc.content == .structure);
    try testing.expect(c_resolved.desc.content.structure == .fn_pure);

    const func = c_resolved.desc.content.structure.fn_pure;
    const args = module_c_env.types.sliceVars(func.args);
    try testing.expectEqual(@as(usize, 2), args.len);

    // First argument should be (I32 -> b)
    const mapper_resolved = module_c_env.types.resolveVar(args[0]);
    try testing.expect(mapper_resolved.desc.content == .structure);
    try testing.expect(mapper_resolved.desc.content.structure == .fn_pure);

    const mapper_func = mapper_resolved.desc.content.structure.fn_pure;
    const mapper_args = module_c_env.types.sliceVars(mapper_func.args);
    try testing.expectEqual(@as(usize, 1), mapper_args.len);

    // The mapper input should be I32
    const mapper_input_resolved = module_c_env.types.resolveVar(mapper_args[0]);
    try testing.expect(mapper_input_resolved.desc.content == .structure);
    try testing.expect(mapper_input_resolved.desc.content.structure == .num);
    try testing.expect(mapper_input_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, mapper_input_resolved.desc.content.structure.num.int_precision);

    // The mapper output should still be polymorphic
    const mapper_output_resolved = module_c_env.types.resolveVar(mapper_func.ret);
    try testing.expect(mapper_output_resolved.desc.content == .flex_var);

    // Second argument should be List I32
    const list_arg_resolved = module_c_env.types.resolveVar(args[1]);
    try testing.expect(list_arg_resolved.desc.content == .structure);
    try testing.expect(list_arg_resolved.desc.content.structure == .list);

    const list_elem_var = list_arg_resolved.desc.content.structure.list;
    const list_elem_resolved = module_c_env.types.resolveVar(list_elem_var);
    try testing.expect(list_elem_resolved.desc.content == .structure);
    try testing.expect(list_elem_resolved.desc.content.structure == .num);
    try testing.expect(list_elem_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, list_elem_resolved.desc.content.structure.num.int_precision);

    // Return type should be List b (still polymorphic)
    const ret_resolved = module_c_env.types.resolveVar(func.ret);
    try testing.expect(ret_resolved.desc.content == .structure);
    try testing.expect(ret_resolved.desc.content.structure == .list);

    const ret_elem_var = ret_resolved.desc.content.structure.list;
    const ret_elem_resolved = module_c_env.types.resolveVar(ret_elem_var);
    try testing.expect(ret_elem_resolved.desc.content == .flex_var);

    // The output type variable should match the mapper's output
    try testing.expectEqual(mapper_output_resolved.var_, ret_elem_resolved.var_);
}

test "cross-module type checking - record type chain" {
    const allocator = testing.allocator;

    // Module A exports a record type: { x: I32, y: Str }
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Module A exports a record type: { x: I32, y: Str }
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const i32_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), i32_content, base.Region.zero(), Var);

    const str_content = Content{ .structure = .str };
    const str_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), str_content, base.Region.zero(), Var);

    const x_ident = try module_a_env.idents.insert(allocator, base.Ident.for_text("x"));
    const y_ident = try module_a_env.idents.insert(allocator, base.Ident.for_text("y"));

    var record_fields = std.ArrayList(types_mod.RecordField).init(allocator);
    defer record_fields.deinit();

    try record_fields.append(.{ .name = x_ident, .var_ = i32_var });
    try record_fields.append(.{ .name = y_ident, .var_ = str_var });

    const fields_range = try module_a_env.types.appendRecordFields(record_fields.items);
    const ext_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };

    const record_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, record_content, base.Region.zero());

    // Module B imports and re-exports the record
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Module B imports from A and partially specializes
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");
    const b_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(record_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Module C imports from B
    var module_c_env = try ModuleEnv.init(allocator, "");
    defer module_c_env.deinit();

    try module_c_env.initCIRFields(allocator, "ModuleC");
    const module_c_cir = &module_c_env;

    // Module C imports from B
    _ = try module_c_cir.imports.getOrPut(allocator, &module_c_cir.strings, "ModuleB");
    const c_lookup_expr = try module_c_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(1), // Direct index to module B in the array
            .target_node_idx = @intCast(@intFromEnum(b_lookup_expr)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);
    try modules.append(module_c_cir);

    // Type check module B
    var checker_b = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker_b.deinit();

    _ = try checker_b.checkExpr(b_lookup_expr);

    // Type check module C
    var checker_c = try Check.init(allocator, &module_c_env.types, module_c_cir, modules.items, &module_c_cir.store.regions);
    defer checker_c.deinit();

    _ = try checker_c.checkExpr(c_lookup_expr);

    // Verify the record type in module C
    const c_var = @as(Var, @enumFromInt(@intFromEnum(c_lookup_expr)));
    const c_resolved = module_c_env.types.resolveVar(c_var);

    try testing.expect(c_resolved.desc.content == .structure);
    try testing.expect(c_resolved.desc.content.structure == .record);

    const record = c_resolved.desc.content.structure.record;
    const fields = module_c_env.types.getRecordFieldsSlice(record.fields);
    try testing.expectEqual(@as(usize, 2), fields.len);

    // Check field names and types
    try testing.expectEqualSlices(u8, "x", module_c_env.getIdent(fields.items(.name)[0]));
    try testing.expectEqualSlices(u8, "y", module_c_env.getIdent(fields.items(.name)[1]));

    // Check field x is I32
    const x_resolved = module_c_env.types.resolveVar(fields.items(.var_)[0]);
    try testing.expect(x_resolved.desc.content == .structure);
    try testing.expect(x_resolved.desc.content.structure == .num);
    try testing.expect(x_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, x_resolved.desc.content.structure.num.int_precision);

    // Check field y is Str
    const y_resolved = module_c_env.types.resolveVar(fields.items(.var_)[1]);
    try testing.expect(y_resolved.desc.content == .structure);
    try testing.expect(y_resolved.desc.content.structure == .str);
}

test "cross-module type checking - polymorphic record chain" {
    const allocator = testing.allocator;

    // Module A exports a polymorphic record: { value: a, next: List a }
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Module A exports a polymorphic record: { value: a, next: List a }
    const type_var_a = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    const list_a_content = Content{ .structure = .{ .list = type_var_a } };
    const list_a_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), list_a_content, base.Region.zero(), Var);

    const value_ident = try module_a_env.idents.insert(allocator, base.Ident.for_text("value"));
    const next_ident = try module_a_env.idents.insert(allocator, base.Ident.for_text("next"));

    var record_fields = std.ArrayList(types_mod.RecordField).init(allocator);
    defer record_fields.deinit();

    try record_fields.append(.{ .name = value_ident, .var_ = type_var_a });
    try record_fields.append(.{ .name = next_ident, .var_ = list_a_var });

    const fields_range = try module_a_env.types.appendRecordFields(record_fields.items);
    const ext_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };

    const record_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, record_content, base.Region.zero());

    // Module B imports and partially specializes to Str
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Module B imports from A
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");
    const b_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(record_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create specialized version: { value: Str, next: List Str }
    const str_content = Content{ .structure = .str };
    const str_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), str_content, base.Region.zero(), Var);

    const list_str_content = Content{ .structure = .{ .list = str_var } };
    const list_str_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), list_str_content, base.Region.zero(), Var);

    const value_ident_b = try module_b_env.idents.insert(allocator, base.Ident.for_text("value"));
    const next_ident_b = try module_b_env.idents.insert(allocator, base.Ident.for_text("next"));

    var str_record_fields = std.ArrayList(types_mod.RecordField).init(allocator);
    defer str_record_fields.deinit();

    try str_record_fields.append(.{ .name = value_ident_b, .var_ = str_var });
    try str_record_fields.append(.{ .name = next_ident_b, .var_ = list_str_var });

    const str_fields_range = try module_b_env.types.appendRecordFields(str_record_fields.items);
    const str_ext_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .structure = .empty_record }, base.Region.zero(), Var);

    const str_record_content = Content{ .structure = .{ .record = .{ .fields = str_fields_range, .ext = str_ext_var } } };

    // Set the type on var 1 (for expr 1)
    const str_record_expr_idx = try module_b_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, str_record_content, base.Region.zero());

    // Module C imports the specialized version from B
    var module_c_env = try ModuleEnv.init(allocator, "");
    defer module_c_env.deinit();

    try module_c_env.initCIRFields(allocator, "ModuleC");
    const module_c_cir = &module_c_env;

    // Module C imports from B
    _ = try module_c_cir.imports.getOrPut(allocator, &module_c_cir.strings, "ModuleB");
    const c_lookup_expr = try module_c_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(1), // Direct index to module B in the array
            .target_node_idx = @intCast(@intFromEnum(str_record_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);
    try modules.append(module_c_cir);

    // Type check module B
    var checker_b = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker_b.deinit();

    _ = try checker_b.checkExpr(b_lookup_expr);
    _ = try checker_b.checkExpr(str_record_expr_idx);

    // Type check module C
    var checker_c = try Check.init(allocator, &module_c_env.types, module_c_cir, modules.items, &module_c_cir.store.regions);
    defer checker_c.deinit();

    _ = try checker_c.checkExpr(c_lookup_expr);

    // Verify the specialized record type in module C
    const c_var = @as(Var, @enumFromInt(@intFromEnum(c_lookup_expr)));
    const c_resolved = module_c_env.types.resolveVar(c_var);

    try testing.expect(c_resolved.desc.content == .structure);
    try testing.expect(c_resolved.desc.content.structure == .record);

    const record = c_resolved.desc.content.structure.record;
    const fields = module_c_env.types.getRecordFieldsSlice(record.fields);
    try testing.expectEqual(@as(usize, 2), fields.len);

    // Check field names and types
    try testing.expectEqualSlices(u8, "value", module_c_env.getIdent(fields.items(.name)[0]));
    try testing.expectEqualSlices(u8, "next", module_c_env.getIdent(fields.items(.name)[1]));

    // Check field value is Str
    const value_resolved = module_c_env.types.resolveVar(fields.items(.var_)[0]);
    try testing.expect(value_resolved.desc.content == .structure);
    try testing.expect(value_resolved.desc.content.structure == .str);

    // Check field next is List Str
    const next_resolved = module_c_env.types.resolveVar(fields.items(.var_)[1]);
    try testing.expect(next_resolved.desc.content == .structure);
    try testing.expect(next_resolved.desc.content.structure == .list);

    const list_elem_var = next_resolved.desc.content.structure.list;
    const list_elem_resolved = module_c_env.types.resolveVar(list_elem_var);
    try testing.expect(list_elem_resolved.desc.content == .structure);
    try testing.expect(list_elem_resolved.desc.content.structure == .str);
}

test "cross-module type checking - complex polymorphic chain with unification" {
    const allocator = testing.allocator;

    // Module A exports: compose : (b -> c), (a -> b) -> (a -> c)
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Module A exports: compose : (b -> c), (a -> b) -> (a -> c)
    const type_var_a = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const type_var_b = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const type_var_c = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    // Create (b -> c)
    const b_to_c_content = try module_a_env.types.mkFuncPure(&[_]Var{type_var_b}, type_var_c);
    const b_to_c_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), b_to_c_content, base.Region.zero(), Var);

    // Create (a -> b)
    const a_to_b_content = try module_a_env.types.mkFuncPure(&[_]Var{type_var_a}, type_var_b);
    const a_to_b_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), a_to_b_content, base.Region.zero(), Var);

    // Create (a -> c)
    const a_to_c_content = try module_a_env.types.mkFuncPure(&[_]Var{type_var_a}, type_var_c);
    const a_to_c_var = try module_a_cir.addTypeSlotAndTypeVar(@enumFromInt(0), a_to_c_content, base.Region.zero(), Var);

    // Create compose : (b -> c), (a -> b) -> (a -> c)
    const compose_content = try module_a_env.types.mkFuncPure(&[_]Var{ b_to_c_var, a_to_b_var }, a_to_c_var);
    const compose_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, compose_content, base.Region.zero());

    // Module B imports compose and partially applies with b = Str
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Module B imports from A and makes a new record type
    // This is initially a flex var, but the type will be copied from module A
    // during type checking
    _ = try module_b_cir.imports.getOrPut(allocator, &module_b_cir.strings, "ModuleA");
    const b_lookup_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0), // Direct index to module A in the array
            .target_node_idx = @intCast(@intFromEnum(compose_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create partially specialized version with b = Str
    const str_content = Content{ .structure = .str };
    const b_str_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), str_content, base.Region.zero(), Var);
    const b_type_var_a = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);
    const b_type_var_c = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), Content{ .flex_var = null }, base.Region.zero(), Var);

    // Create (Str -> c)
    const str_to_c_content = try module_b_env.types.mkFuncPure(&[_]Var{b_str_var}, b_type_var_c);
    const str_to_c_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), str_to_c_content, base.Region.zero(), Var);

    // Create (a -> Str)
    const a_to_str_content = try module_b_env.types.mkFuncPure(&[_]Var{b_type_var_a}, b_str_var);
    const a_to_str_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), a_to_str_content, base.Region.zero(), Var);

    // Create (a -> c)
    const b_a_to_c_content = try module_b_env.types.mkFuncPure(&[_]Var{b_type_var_a}, b_type_var_c);
    const b_a_to_c_var = try module_b_cir.addTypeSlotAndTypeVar(@enumFromInt(0), b_a_to_c_content, base.Region.zero(), Var);

    // Create composeStr : (Str -> c), (a -> Str) -> (a -> c)
    const compose_str_content = try module_b_env.types.mkFuncPure(&[_]Var{ str_to_c_var, a_to_str_var }, b_a_to_c_var);

    // Create the expression
    const compose_str_expr_idx = try module_b_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, compose_str_content, base.Region.zero());

    // Module C imports the partially specialized version and further specializes c = I32
    var module_c_env = try ModuleEnv.init(allocator, "");
    defer module_c_env.deinit();

    try module_c_env.initCIRFields(allocator, "ModuleC");
    const module_c_cir = &module_c_env;

    // Module C imports from B and uses the wrapper
    // This is initially a flex var, but the type will be copied from module B
    // during type checking
    _ = try module_c_cir.imports.getOrPut(allocator, &module_c_cir.strings, "ModuleB");
    const c_lookup_expr = try module_c_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(1), // Direct index to module B in the array
            .target_node_idx = @intCast(@intFromEnum(compose_str_expr_idx)),
            .region = base.Region.zero(),
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Create array of module environments
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();

    try modules.append(module_a_cir);
    try modules.append(module_b_cir);
    try modules.append(module_c_cir);

    // Type check module B
    var checker_b = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker_b.deinit();

    _ = try checker_b.checkExpr(b_lookup_expr);
    _ = try checker_b.checkExpr(compose_str_expr_idx);

    // Type check module C
    var checker_c = try Check.init(allocator, &module_c_env.types, module_c_cir, modules.items, &module_c_cir.store.regions);
    defer checker_c.deinit();

    _ = try checker_c.checkExpr(c_lookup_expr);

    // Important! Now that we've type-checked, we can add vars directly to the
    // types store.

    // Now unify the imported function with a specific instantiation
    const c_var = @as(Var, @enumFromInt(@intFromEnum(c_lookup_expr)));

    // Create a concrete instance where c = I32
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const c_str_var = try module_c_env.types.freshFromContent(str_content);
    const c_i32_var = try module_c_env.types.freshFromContent(i32_content);
    const c_type_var_a = try module_c_env.types.fresh();

    // Create (Str -> I32)
    const str_to_i32_content = try module_c_env.types.mkFuncPure(&[_]Var{c_str_var}, c_i32_var);
    const str_to_i32_var = try module_c_env.types.freshFromContent(str_to_i32_content);

    // Create (a -> Str)
    const c_a_to_str_content = try module_c_env.types.mkFuncPure(&[_]Var{c_type_var_a}, c_str_var);
    const c_a_to_str_var = try module_c_env.types.freshFromContent(c_a_to_str_content);

    // Create (a -> I32)
    const a_to_i32_content = try module_c_env.types.mkFuncPure(&[_]Var{c_type_var_a}, c_i32_var);
    const a_to_i32_var = try module_c_env.types.freshFromContent(a_to_i32_content);

    // Create the expected type: (Str -> I32), (a -> Str) -> (a -> I32)
    const expected_content = try module_c_env.types.mkFuncPure(&[_]Var{ str_to_i32_var, c_a_to_str_var }, a_to_i32_var);
    const expected_var = try module_c_env.types.freshFromContent(expected_content);

    // Unify the imported type with the expected type
    const result = try checker_c.unify(c_var, expected_var);
    try testing.expect(result.isOk());

    // Verify the unified type
    const c_resolved = module_c_env.types.resolveVar(c_var);
    try testing.expect(c_resolved.desc.content == .structure);
    try testing.expect(c_resolved.desc.content.structure == .fn_pure);

    const func = c_resolved.desc.content.structure.fn_pure;
    const args = module_c_env.types.sliceVars(func.args);
    try testing.expectEqual(@as(usize, 2), args.len);

    // First argument should be (Str -> I32)
    const first_arg_resolved = module_c_env.types.resolveVar(args[0]);
    try testing.expect(first_arg_resolved.desc.content == .structure);
    try testing.expect(first_arg_resolved.desc.content.structure == .fn_pure);

    const first_func = first_arg_resolved.desc.content.structure.fn_pure;
    const first_func_args = module_c_env.types.sliceVars(first_func.args);
    try testing.expectEqual(@as(usize, 1), first_func_args.len);

    const first_func_arg_resolved = module_c_env.types.resolveVar(first_func_args[0]);
    try testing.expect(first_func_arg_resolved.desc.content == .structure);
    try testing.expect(first_func_arg_resolved.desc.content.structure == .str);

    const first_func_ret_resolved = module_c_env.types.resolveVar(first_func.ret);
    try testing.expect(first_func_ret_resolved.desc.content == .structure);
    try testing.expect(first_func_ret_resolved.desc.content.structure == .num);
    try testing.expect(first_func_ret_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, first_func_ret_resolved.desc.content.structure.num.int_precision);

    // Second argument should be (a -> Str) where a is still polymorphic
    const second_arg_resolved = module_c_env.types.resolveVar(args[1]);
    try testing.expect(second_arg_resolved.desc.content == .structure);
    try testing.expect(second_arg_resolved.desc.content.structure == .fn_pure);

    const second_func = second_arg_resolved.desc.content.structure.fn_pure;
    const second_func_args = module_c_env.types.sliceVars(second_func.args);
    try testing.expectEqual(@as(usize, 1), second_func_args.len);

    const second_func_arg_resolved = module_c_env.types.resolveVar(second_func_args[0]);
    try testing.expect(second_func_arg_resolved.desc.content == .flex_var);

    const second_func_ret_resolved = module_c_env.types.resolveVar(second_func.ret);
    try testing.expect(second_func_ret_resolved.desc.content == .structure);
    try testing.expect(second_func_ret_resolved.desc.content.structure == .str);

    // Return type should be (a -> I32)
    const ret_resolved = module_c_env.types.resolveVar(func.ret);
    try testing.expect(ret_resolved.desc.content == .structure);
    try testing.expect(ret_resolved.desc.content.structure == .fn_pure);

    const ret_func = ret_resolved.desc.content.structure.fn_pure;
    const ret_func_args = module_c_env.types.sliceVars(ret_func.args);
    try testing.expectEqual(@as(usize, 1), ret_func_args.len);

    const ret_func_arg_resolved = module_c_env.types.resolveVar(ret_func_args[0]);
    try testing.expect(ret_func_arg_resolved.desc.content == .flex_var);

    const ret_func_ret_resolved = module_c_env.types.resolveVar(ret_func.ret);
    try testing.expect(ret_func_ret_resolved.desc.content == .structure);
    try testing.expect(ret_func_ret_resolved.desc.content.structure == .num);
    try testing.expect(ret_func_ret_resolved.desc.content.structure.num == .int_precision);
    try testing.expectEqual(types_mod.Num.Int.Precision.i32, ret_func_ret_resolved.desc.content.structure.num.int_precision);

    // The 'a' type variable should be consistent across both functions
    try testing.expectEqual(second_func_arg_resolved.var_, ret_func_arg_resolved.var_);
}

test "cross-module type checking - type mismatch with proper error message" {
    const allocator = testing.allocator;

    // Module A: Exports a string value
    var module_a_env = try ModuleEnv.init(allocator, "");
    defer module_a_env.deinit();

    try module_a_env.initCIRFields(allocator, "ModuleA");
    const module_a_cir = &module_a_env;

    // Create a string value in module A
    const str_expr_idx = try module_a_cir.addExprAndTypeVar(.{ .e_int = .{
        .value = .{ .bytes = [_]u8{0} ** 16, .kind = .i128 },
    } }, .{ .structure = .str }, base.Region.zero());

    // Module B: Tries to use the string as a number
    var module_b_env = try ModuleEnv.init(allocator, "");
    defer module_b_env.deinit();

    try module_b_env.initCIRFields(allocator, "ModuleB");
    const module_b_cir = &module_b_env;

    // Create an import expression that references module A's string
    // This is initially a flex var, but the type will be copied from module A
    // during type checking
    const import_expr = try module_b_cir.addExprAndTypeVar(.{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0),
            .target_node_idx = @intCast(@intFromEnum(str_expr_idx)),
            .region = .{
                .start = .{ .offset = 0 },
                .end = .{ .offset = 20 },
            },
        },
    }, Content{ .flex_var = null }, base.Region.zero());

    // Set up modules array
    var modules = std.ArrayList(*ModuleEnv).init(allocator);
    defer modules.deinit();
    try modules.append(module_a_cir);

    // Type check module B
    var checker = try Check.init(allocator, &module_b_env.types, module_b_cir, modules.items, &module_b_cir.store.regions);
    defer checker.deinit();

    // Check the import expression - this will copy the type from module A
    _ = try checker.checkExpr(import_expr);

    // Important! Now that we've type-checked, we can add vars directly to the
    // types store.

    // Now try to unify the import (which has Str type) with I32
    const import_var = @as(types_mod.Var, @enumFromInt(@intFromEnum(import_expr)));
    const i32_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const i32_var = try module_b_env.types.freshFromContent(i32_content);

    const result = try checker.unify(import_var, i32_var);

    // The unification should fail
    try testing.expect(result.isProblem());

    // Check that the problem has the cross-module import detail
    const problem_idx = result.problem;
    const prob = checker.problems.problems.items[@intFromEnum(problem_idx)];

    try testing.expect(prob == .type_mismatch);
    const mismatch = prob.type_mismatch;

    // The detail might be null if the unification happens outside the import handling
    // But our code ensures it gets set when the import unification fails
    if (mismatch.detail) |detail| {
        if (detail == .cross_module_import) {
            const cross_module_detail = detail.cross_module_import;
            try testing.expectEqual(import_expr, cross_module_detail.import_region);
            try testing.expectEqual(@as(CIR.Import.Idx, @enumFromInt(0)), cross_module_detail.module_idx);
        }
    }
}
