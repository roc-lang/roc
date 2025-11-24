//! Tests for e_low_level_lambda runtime evaluation in the interpreter
//!
//! These tests verify that low-level operations (like Str.is_empty, List.concat) that are defined
//! as e_low_level_lambda nodes correctly dispatch to their builtin implementations
//! when called at compile-time, producing the correct runtime values.

const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");

const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;
const builtin_loading = @import("../builtin_loading.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const helpers = @import("helpers.zig");

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const testing = std.testing;
const test_allocator = testing.allocator;

fn parseCheckAndEvalModule(src: []const u8) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    builtin_module: builtin_loading.LoadedModule,
} {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = "TestModule";
    try module_env.common.calcLineStarts(module_env.gpa);

    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    parse_ast.store.emptyScratch();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    errdefer builtin_module.deinit();

    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .@"try" = try module_env.insertIdent(base.Ident.for_text("Try")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
    };

    // Create module_envs map for canonicalization (enables qualified calls to Str, List, etc.)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs_map.deinit();

    // Use shared function to populate ALL builtin types - ensures Builtin.roc is single source of truth
    try Can.populateModuleEnvs(
        &module_envs_map,
        module_env,
        builtin_module.env,
        builtin_indices,
    );

    var czer = try Can.init(module_env, &parse_ast, &module_envs_map);
    defer czer.deinit();

    try czer.canonicalizeFile();

    const imported_envs = [_]*const ModuleEnv{builtin_module.env};
    var checker = try Check.init(gpa, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &imported_envs, problems, builtin_types, null);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .builtin_module = builtin_module,
    };
}

fn cleanupEvalModule(result: anytype) void {
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);

    var builtin_module_mut = result.builtin_module;
    builtin_module_mut.deinit();
}

/// Helper to evaluate multi-declaration modules and get the integer value of a specific declaration
fn evalModuleAndGetInt(src: []const u8, decl_index: usize) !i128 {
    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    // Get all declarations
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    if (decl_index >= defs.len) {
        return error.DeclarationIndexOutOfBounds;
    }

    const ops = result.evaluator.get_ops();

    // Evaluate all declarations up to and including the one we want, in order
    // This ensures earlier declarations (like x = ...) are available when evaluating later ones (like len = List.len(x))
    var i: usize = 0;
    while (i <= decl_index) : (i += 1) {
        const def = result.module_env.store.getDef(defs[i]);
        const stack_value = try result.evaluator.interpreter.evalMinimal(def.expr, ops);

        // Store the value in bindings so later declarations can reference it
        try result.evaluator.interpreter.bindings.append(.{
            .pattern_idx = def.pattern,
            .value = stack_value,
            .expr_idx = def.expr,
            .source_env = result.module_env,
        });

        // Return the value if this is the declaration we want
        if (i == decl_index) {
            defer stack_value.decref(&result.evaluator.interpreter.runtime_layout_store, ops);
            return stack_value.asI128();
        }
    }

    unreachable;
}

/// Helper to evaluate multi-declaration modules and get the string representation of a specific declaration
fn evalModuleAndGetString(src: []const u8, decl_index: usize, _: std.mem.Allocator) ![]u8 {
    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    // Get all declarations
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    if (decl_index >= defs.len) {
        return error.DeclarationIndexOutOfBounds;
    }

    const ops = result.evaluator.get_ops();

    // Evaluate all declarations up to and including the one we want, in order
    var i: usize = 0;
    while (i <= decl_index) : (i += 1) {
        const def = result.module_env.store.getDef(defs[i]);
        const stack_value = try result.evaluator.interpreter.evalMinimal(def.expr, ops);

        // Store the value in bindings so later declarations can reference it
        try result.evaluator.interpreter.bindings.append(.{
            .pattern_idx = def.pattern,
            .value = stack_value,
            .expr_idx = def.expr,
            .source_env = result.module_env,
        });

        // Return the rendered value if this is the declaration we want
        if (i == decl_index) {
            defer stack_value.decref(&result.evaluator.interpreter.runtime_layout_store, ops);
            const rt_var = try result.evaluator.interpreter.translateTypeVar(result.module_env, can.ModuleEnv.varFrom(def.expr));
            return try result.evaluator.interpreter.renderValueRocWithType(stack_value, rt_var);
        }
    }

    unreachable;
}

test "e_low_level_lambda - Str.is_empty returns True for empty string" {
    const src =
        \\x = Str.is_empty("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.is_empty returns False for non-empty string" {
    const src =
        \\x = Str.is_empty("hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.is_empty in conditional" {
    const src =
        \\x = if True {
        \\    Str.is_empty("")
        \\} else {
        \\    False
        \\}
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - List.concat with two non-empty lists" {
    const src =
        \\x = List.concat([1, 2], [3, 4])
        \\len = List.len(x)
    ;

    // Get the value of the second declaration (len), which should be 4
    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 4), len_value);
}

test "e_low_level_lambda - List.concat with empty and non-empty list" {
    const src =
        \\x = List.concat([], [1, 2, 3])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 3), len_value);
}

test "e_low_level_lambda - List.concat with two empty lists" {
    const src =
        \\x : List(U64)
        \\x = List.concat([], [])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.concat preserves order" {
    const src =
        \\x = List.concat([10, 20], [30, 40, 50])
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(10)", first_value);
}

test "e_low_level_lambda - List.concat with strings (refcounted elements)" {
    const src =
        \\x = List.concat(["hello", "world"], ["foo", "bar"])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 4), len_value);
}

test "e_low_level_lambda - List.concat with nested lists (refcounted elements)" {
    const src =
        \\x = List.concat([[1, 2], [3]], [[4, 5, 6]])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 3), len_value);
}

test "e_low_level_lambda - List.concat with empty string list" {
    const src =
        \\x = List.concat([], ["a", "b", "c"])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 3), len_value);
}

test "e_low_level_lambda - Dec.to_str returns string representation of decimal" {
    const src =
        \\a : Dec
        \\a = 123.45dec
        \\x = Dec.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"123.45\"", value);
}

test "e_low_level_lambda - Dec.to_str with negative decimal" {
    const src =
        \\a : Dec
        \\a = -456.78dec
        \\x = Dec.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-456.78\"", value);
}

test "e_low_level_lambda - Dec.to_str with zero" {
    const src =
        \\a : Dec
        \\a = 0.0dec
        \\x = Dec.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"0.0\"", value);
}

// Integer to_str tests

test "e_low_level_lambda - U8.to_str" {
    const src =
        \\a : U8
        \\a = 42u8
        \\x = U8.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"42\"", value);
}

test "e_low_level_lambda - I8.to_str with negative" {
    const src =
        \\a : I8
        \\a = -42i8
        \\x = I8.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-42\"", value);
}

test "e_low_level_lambda - U16.to_str" {
    const src =
        \\a : U16
        \\a = 1000u16
        \\x = U16.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"1000\"", value);
}

test "e_low_level_lambda - I16.to_str with negative" {
    const src =
        \\a : I16
        \\a = -500i16
        \\x = I16.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-500\"", value);
}

test "e_low_level_lambda - U32.to_str" {
    const src =
        \\a : U32
        \\a = 100000u32
        \\x = U32.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"100000\"", value);
}

test "e_low_level_lambda - I32.to_str with negative" {
    const src =
        \\a : I32
        \\a = -12345i32
        \\x = I32.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-12345\"", value);
}

test "e_low_level_lambda - U64.to_str" {
    const src =
        \\a : U64
        \\a = 9876543210u64
        \\x = U64.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"9876543210\"", value);
}

test "e_low_level_lambda - I64.to_str with negative" {
    const src =
        \\a : I64
        \\a = -9876543210i64
        \\x = I64.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-9876543210\"", value);
}

test "e_low_level_lambda - U128.to_str" {
    const src =
        \\a : U128
        \\a = 12345678901234567890u128
        \\x = U128.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"12345678901234567890\"", value);
}

test "e_low_level_lambda - I128.to_str with negative" {
    const src =
        \\a : I128
        \\a = -12345678901234567890i128
        \\x = I128.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-12345678901234567890\"", value);
}

// Float to_str tests

test "e_low_level_lambda - F32.to_str" {
    const src =
        \\a : F32
        \\a = 3.14f32
        \\x = F32.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    // F32 has limited precision, so we just check it starts correctly
    try testing.expect(std.mem.startsWith(u8, value, "\"3.14"));
}

test "e_low_level_lambda - F64.to_str" {
    const src =
        \\a : F64
        \\a = 3.14159265359f64
        \\x = F64.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    // F64 has more precision than F32
    try testing.expect(std.mem.startsWith(u8, value, "\"3.141592"));
}

test "e_low_level_lambda - F32.to_str with negative" {
    const src =
        \\a : F32
        \\a = -2.5f32
        \\x = F32.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expect(std.mem.startsWith(u8, value, "\"-2.5"));
}

test "e_low_level_lambda - F64.to_str with negative" {
    const src =
        \\a : F64
        \\a = -123.456f64
        \\x = F64.to_str(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expect(std.mem.startsWith(u8, value, "\"-123.456"));
}
