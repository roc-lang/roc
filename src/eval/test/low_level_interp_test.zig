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
const layout_mod = @import("layout");

const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;
const builtin_loading = @import("../builtin_loading.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const helpers = @import("helpers.zig");

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const Layout = layout_mod.Layout;
const LayoutTag = layout_mod.LayoutTag;
const ScalarTag = layout_mod.ScalarTag;
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
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &imported_envs, problems, builtin_types);

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

test "tag union payload layout should match list element type" {
    // This test verifies that when List.first is called on List U64,
    // the Result type's payload has U64 layout, not I128 layout.
    const src =
        \\x = [10u64, 20u64]
        \\first = List.first(x)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    // Get the type of 'first' declaration
    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    const first_def = result.module_env.store.getDef(defs[1]); // second declaration

    // Get the runtime type
    const rt_var = try result.evaluator.interpreter.translateTypeVar(result.module_env, can.ModuleEnv.varFrom(first_def.expr));
    const rt_type = result.evaluator.interpreter.runtime_types.resolveVar(rt_var);

    // The type should be a tag union (Result/Try)
    try testing.expect(rt_type.desc.content == .structure);
    try testing.expect(rt_type.desc.content.structure == .tag_union);

    // Get the tag union layout
    var temp_scope = types.TypeScope.init(test_allocator);
    defer temp_scope.deinit();
    const layout_idx = try result.evaluator.interpreter.runtime_layout_store.addTypeVar(rt_var, &temp_scope);
    const layout = result.evaluator.interpreter.runtime_layout_store.getLayout(layout_idx);

    // Tag unions are tuples: (payload, tag)
    try testing.expectEqual(LayoutTag.tuple, layout.tag);

    const tuple_data = result.evaluator.interpreter.runtime_layout_store.getTupleData(layout.data.tuple.idx);
    const fields = result.evaluator.interpreter.runtime_layout_store.tuple_fields.sliceRange(tuple_data.getFields());

    // Should have 2 fields: payload and tag
    try testing.expectEqual(@as(usize, 2), fields.len);

    // Find the payload field (original index 0)
    var payload_layout: ?Layout = null;
    for (0..fields.len) |i| {
        const field = fields.get(i);
        if (field.index == 0) { // payload is at original index 0
            payload_layout = result.evaluator.interpreter.runtime_layout_store.getLayout(field.layout);
            break;
        }
    }

    try testing.expect(payload_layout != null);
    const pl = payload_layout.?;

    // Payload should be U64, not I128
    try testing.expectEqual(LayoutTag.scalar, pl.tag);
    try testing.expectEqual(ScalarTag.int, pl.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u64, pl.data.scalar.data.int);
}

test "e_low_level_lambda - List.first on literal list" {
    const src =
        \\x = [10, 20, 30, 40, 50]
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok 10", first_value);
}

test "e_low_level_lambda - List.concat preserves order" {
    const src =
        \\x = List.concat([10, 20], [30, 40, 50])
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok 10", first_value);
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
