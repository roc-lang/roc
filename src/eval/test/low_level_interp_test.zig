//! Tests for e_low_level_lambda runtime evaluation in the interpreter
//!
//! These tests verify that low-level operations (like Str.is_empty, List.concat) that are defined
//! as e_low_level_lambda nodes correctly dispatch to their builtin implementations
//! when called at compile-time, producing the correct runtime values.

const std = @import("std");
const parse = @import("parse");
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
    checker: *Check,
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

    try module_env.initCIRFields("test");
    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
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

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.resolveImports(module_env, &imported_envs);

    const checker = try gpa.create(Check);
    errdefer gpa.destroy(checker);
    checker.* = try Check.init(gpa, &module_env.types, module_env, &imported_envs, null, &module_env.store.regions, builtin_ctx);
    errdefer checker.deinit();

    try checker.checkFile();

    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &imported_envs, problems, builtin_types, builtin_module.env, &checker.import_mapping);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .builtin_module = builtin_module,
        .checker = checker,
    };
}

fn cleanupEvalModule(result: anytype) void {
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);

    // Deinit checker (must happen after evaluator since evaluator holds pointer to import_mapping)
    var checker_mut = result.checker;
    checker_mut.deinit();
    test_allocator.destroy(result.checker);

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
        const stack_value = try result.evaluator.interpreter.eval(def.expr, ops);

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
        const stack_value = try result.evaluator.interpreter.eval(def.expr, ops);

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
            return try result.evaluator.interpreter.renderValueRocWithType(stack_value, rt_var, ops);
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

test "e_low_level_lambda - Str.concat with two non-empty strings" {
    const src =
        \\x = Str.concat("hello", "world")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"helloworld\"", value);
}

test "e_low_level_lambda - Str.concat with empty and non-empty string" {
    const src =
        \\x = Str.concat("", "test")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"test\"", value);
}

test "e_low_level_lambda - Str.concat with non-empty and empty string" {
    const src =
        \\x = Str.concat("test", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"test\"", value);
}

test "e_low_level_lambda - Str.concat with two empty strings" {
    const src =
        \\x = Str.concat("", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.concat with special characters" {
    const src =
        \\x = Str.concat("hello ", "world!")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello world!\"", value);
}

test "e_low_level_lambda - Str.concat with longer strings" {
    const src =
        \\x = Str.concat("This is a longer string that contains about one hundred characters for testing concatenation.", " This is the second string that also has many characters in it for testing longer string operations.")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"This is a longer string that contains about one hundred characters for testing concatenation. This is the second string that also has many characters in it for testing longer string operations.\"", value);
}

test "e_low_level_lambda - Str.contains with substring in middle" {
    const src =
        \\x = Str.contains("foobarbaz", "bar")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.contains with non-matching strings" {
    const src =
        \\x = Str.contains("apple", "orange")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.contains with empty needle" {
    const src =
        \\x = Str.contains("anything", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.contains with substring at start" {
    const src =
        \\x = Str.contains("hello world", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.contains with substring at end" {
    const src =
        \\x = Str.contains("hello world", "world")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.contains with empty haystack" {
    const src =
        \\x = Str.contains("", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.contains with identical strings" {
    const src =
        \\x = Str.contains("test", "test")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals with equal strings" {
    const src =
        \\x = Str.caseless_ascii_equals("hello", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals with different case" {
    const src =
        \\x = Str.caseless_ascii_equals("hello", "HELLO")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals with different strings" {
    const src =
        \\x = Str.caseless_ascii_equals("hello", "world")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals with empty strings" {
    const src =
        \\x = Str.caseless_ascii_equals("", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals with empty and non-empty string" {
    const src =
        \\x = Str.caseless_ascii_equals("", "test")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals with longer strings" {
    const src =
        \\x = Str.caseless_ascii_equals("This is a longer string that contains about one hundred characters for testing purposes.", "THIS IS A LONGER STRING THAT CONTAINS ABOUT ONE HUNDRED CHARACTERS FOR TESTING purposes.")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals long and small strings" {
    const src =
        \\x = Str.caseless_ascii_equals("THIS IS A LONGER STRING THAT CONTAINS ABOUT ONE HUNDRED CHARACTERS FOR TESTING purposes.", "This")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals small and long strings" {
    const src =
        \\x = Str.caseless_ascii_equals("This", "THIS IS A LONGER STRING THAT CONTAINS ABOUT ONE HUNDRED CHARACTERS FOR TESTING purposes.")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals eq with non-ascii chars" {
    const src =
        \\x = Str.caseless_ascii_equals("COFFÉ", "coffÉ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.caseless_ascii_equals non-ascii casing difference" {
    const src =
        \\x = Str.caseless_ascii_equals("coffé", "coffÉ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.with_ascii_lowercased with mixed case" {
    const src =
        \\x = Str.with_ascii_lowercased("HeLLo")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.with_ascii_lowercased with already lowercase" {
    const src =
        \\x = Str.with_ascii_lowercased("hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.with_ascii_lowercased with empty string" {
    const src =
        \\x = Str.with_ascii_lowercased("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.with_ascii_lowercased with non-ascii chars" {
    const src =
        \\x = Str.with_ascii_lowercased("COFFÉ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"coffÉ\"", value);
}

test "e_low_level_lambda - Str.with_ascii_uppercased with mixed case" {
    const src =
        \\x = Str.with_ascii_uppercased("HeLLo")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"HELLO\"", value);
}

test "e_low_level_lambda - Str.with_ascii_uppercased with already uppercase" {
    const src =
        \\x = Str.with_ascii_uppercased("HELLO")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"HELLO\"", value);
}

test "e_low_level_lambda - Str.with_ascii_uppercased with empty string" {
    const src =
        \\x = Str.with_ascii_uppercased("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.with_ascii_uppercased with non-ascii chars" {
    const src =
        \\x = Str.with_ascii_uppercased("coffÉ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"COFFÉ\"", value);
}

test "e_low_level_lambda - Str.with_ascii_uppercased long text" {
    const src =
        \\x = Str.with_ascii_uppercased("coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ COFFÉ\"", value);
}

test "e_low_level_lambda - Str.trim with an empty string" {
    const src =
        \\x = Str.trim("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.trim with a whitespace string" {
    const src =
        \\x = Str.trim("   ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.trim with a non-whitespace string" {
    const src =
        \\x = Str.trim("  hello  ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.trim_start with an empty string" {
    const src =
        \\x = Str.trim_start("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.trim_start with a whitespace string" {
    const src =
        \\x = Str.trim_start("   ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.trim_start with a non-whitespace string" {
    const src =
        \\x = Str.trim_start("  hello  ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello  \"", value);
}

test "e_low_level_lambda - Str.trim_end with an empty string" {
    const src =
        \\x = Str.trim_end("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.trim_end with a whitespace string" {
    const src =
        \\x = Str.trim_end("   ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.trim_end with a non-whitespace string" {
    const src =
        \\x = Str.trim_end("  hello  ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"  hello\"", value);
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

test "e_low_level_lambda - List.concat with zero-sized type" {
    const src =
        \\x : List({})
        \\x = List.concat([{}, {}], [{}, {}, {}])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 5), len_value);
}

test "e_low_level_lambda - List.with_capacity of non refcounted elements creates empty list" {
    const src =
        \\x : List(U64)
        \\x = List.with_capacity(10)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.with_capacity of str (refcounted elements) creates empty list" {
    const src =
        \\x : List(Str)
        \\x = List.with_capacity(10)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.with_capacity of non refcounted elements can concat" {
    const src =
        \\y : List(U64)
        \\y = List.with_capacity(10)
        \\x = List.concat(y, [1])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 1), len_value);
}

test "e_low_level_lambda - List.with_capacity of str (refcounted elements) can concat" {
    const src =
        \\y : List(Str)
        \\y = List.with_capacity(10)
        \\x = List.concat(y, ["hello", "world"])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 2), len_value);
}

test "e_low_level_lambda - List.with_capacity without capacity, of str (refcounted elements) can concat" {
    const src =
        \\y : List(Str)
        \\y = List.with_capacity(0)
        \\x = List.concat(y, ["hello", "world"])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 2), len_value);
}

test "e_low_level_lambda - List.with_capacity of zero-sized type creates empty list" {
    const src =
        \\x : List({})
        \\x = List.with_capacity(10)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.append on non-empty list" {
    const src =
        \\x = List.append([0, 1, 2, 3], 4)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 5), len_value);
}

test "e_low_level_lambda - List.append on empty list" {
    const src =
        \\x = List.append([], 0)
        \\got = List.get(x, 0)
    ;

    const get_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(get_value);
    try testing.expectEqualStrings("Ok(0)", get_value);
}

test "e_low_level_lambda - List.append a list on empty list" {
    const src =
        \\x = List.append([], [])
        \\len = List.len(x)
        \\got = List.get(x, 0)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1), len_value);
}

test "e_low_level_lambda - List.append for strings" {
    const src =
        \\x = List.append(["cat", "chases"], "rat")
        \\len = List.len(x)
        \\got = List.get(x, 2)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 3), len_value);

    const get_value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(get_value);
    try testing.expectEqualStrings("Ok(\"rat\")", get_value);
}

test "e_low_level_lambda - List.append for list of lists" {
    const src =
        \\x = List.append([[0, 1], [2, 3, 4], [5, 6, 7]], [8,9])
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 4), len_value);
}

test "e_low_level_lambda - List.append for list of tuples" {
    const src =
        \\x = List.append([(-1, 0, 1), (2, 3, 4), (5, 6, 7)], (-2, -3, -4))
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 4), len_value);
}

test "e_low_level_lambda - List.append for list of records" {
    const src =
        \\x = List.append([{x:"1", y: "1"}, {x: "2", y: "4"}, {x: "5", y: "7"}], {x: "2", y: "4"})
        \\len = List.len(x)
        \\tail = match List.get(x, 3) { Ok(rec) => rec.x, _ => "wrong"}
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 4), len_value);

    const get_value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(get_value);
    try testing.expectEqualStrings("\"2\"", get_value);
}

test "e_low_level_lambda - List.append for already refcounted elt" {
    const src =
        \\new = [8, 9]
        \\w = [new, new, new, [10, 11]]
        \\x = List.append([[0, 1], [2, 3, 4], [5, 6, 7]], new)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 3);
    try testing.expectEqual(@as(i128, 4), len_value);
}

test "e_low_level_lambda - List.append for list of tuples with strings (issue 8650)" {
    // This test reproduces issue #8650 - use-after-free when appending tuples containing strings.
    // The bug was that isRefcounted() returns false for tuples, so strings inside tuples
    // weren't being increffed before the append, leading to use-after-free.
    const src =
        \\x = List.append([("a", "b")], ("hello", "world"))
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 2), len_value);
}

test "e_low_level_lambda - List.drop_at on an empty list at index 0" {
    const src =
        \\x = List.drop_at([], 0)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.drop_at on an empty list at index >0" {
    const src =
        \\x = List.drop_at([], 10)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.drop_at on non-empty list" {
    const src =
        \\x = List.drop_at([1, 2, 3], 0)
        \\len = List.len(x)
        \\first = List.get(x, 0)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 2), len_value);

    const value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(2)", value);
}

test "e_low_level_lambda - List.drop_at out of bounds on non-empty list" {
    const src =
        \\x = List.drop_at([1, 2, 3, 4, 5], 10)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 5), len_value);
}

test "e_low_level_lambda - List.drop_at on refcounted List(Str)" {
    const src =
        \\x = List.drop_at(["cat", "chases", "rat"], 1)
        \\len = List.len(x)
        \\second = List.get(x, 1)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 2), len_value);

    const value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(\"rat\")", value);
}

test "e_low_level_lambda - List.drop_at on refcounted List(List(Str))" {
    const src =
        \\x = List.drop_at([["two", "words"], [], ["a", "four", "word", "list"]], 1)
        \\len = List.len(x)
        \\second = Try.ok_or(List.get(x, 1), [])
        \\elt_len =  List.len(second)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 2), len_value);

    const elt_len_value = try evalModuleAndGetInt(src, 3);
    try testing.expectEqual(@as(i128, 4), elt_len_value);
}

test "e_low_level_lambda - List.sublist on empty list" {
    const src =
        \\x = List.sublist([], {start: 0, len: 10})
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.sublist on non-empty list" {
    const src =
        \\x = List.sublist([0, 1, 2, 3, 4], {start: 1, len: 3})
        \\len = List.len(x)
        \\slice_start = List.get(x, 0)
        \\slice_end = List.get(x, 2)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 3), len_value);

    const head_value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(head_value);
    try testing.expectEqualStrings("Ok(1)", head_value);

    const tail_value = try evalModuleAndGetString(src, 3, test_allocator);
    defer test_allocator.free(tail_value);
    try testing.expectEqualStrings("Ok(3)", tail_value);
}

test "e_low_level_lambda - List.sublist start out of bounds" {
    const src =
        \\x = List.sublist([0, 1, 2, 3, 4], {start: 100, len: 3})
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.sublist requesting beyond end of list gives you input list" {
    const src =
        \\x = List.sublist([0, 1, 2, 3, 4], {start: 0, len: 10000})
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 5), len_value);
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

// Str.starts_with tests

test "e_low_level_lambda - Str.starts_with returns True for matching prefix" {
    const src =
        \\x = Str.starts_with("hello world", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.starts_with returns False for non-matching prefix" {
    const src =
        \\x = Str.starts_with("hello world", "world")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.starts_with with empty prefix" {
    const src =
        \\x = Str.starts_with("hello", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.starts_with with empty string and empty prefix" {
    const src =
        \\x = Str.starts_with("", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.starts_with with prefix longer than string" {
    const src =
        \\x = Str.starts_with("hi", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

// Str.ends_with tests

test "e_low_level_lambda - Str.ends_with returns True for matching suffix" {
    const src =
        \\x = Str.ends_with("hello world", "world")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.ends_with returns False for non-matching suffix" {
    const src =
        \\x = Str.ends_with("hello world", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

test "e_low_level_lambda - Str.ends_with with empty suffix" {
    const src =
        \\x = Str.ends_with("hello", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.ends_with with empty string and empty suffix" {
    const src =
        \\x = Str.ends_with("", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value);
}

test "e_low_level_lambda - Str.ends_with with suffix longer than string" {
    const src =
        \\x = Str.ends_with("hi", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("False", value);
}

// Str.repeat tests

test "e_low_level_lambda - Str.repeat basic repetition" {
    const src =
        \\x = Str.repeat("ab", 3)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"ababab\"", value);
}

test "e_low_level_lambda - Str.repeat with zero count" {
    const src =
        \\x = Str.repeat("hello", 0)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.repeat with one count" {
    const src =
        \\x = Str.repeat("hello", 1)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.repeat empty string" {
    const src =
        \\x = Str.repeat("", 5)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

// Str.with_prefix tests

test "e_low_level_lambda - Str.with_prefix basic" {
    const src =
        \\x = Str.with_prefix("world", "hello ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello world\"", value);
}

test "e_low_level_lambda - Str.with_prefix empty prefix" {
    const src =
        \\x = Str.with_prefix("hello", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.with_prefix empty string" {
    const src =
        \\x = Str.with_prefix("", "prefix")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"prefix\"", value);
}

test "e_low_level_lambda - Str.with_prefix both empty" {
    const src =
        \\x = Str.with_prefix("", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

// Str.drop_prefix tests

test "e_low_level_lambda - Str.drop_prefix removes matching prefix" {
    const src =
        \\x = Str.drop_prefix("hello world", "hello ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"world\"", value);
}

test "e_low_level_lambda - Str.drop_prefix returns original when no match" {
    const src =
        \\x = Str.drop_prefix("hello world", "goodbye ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello world\"", value);
}

test "e_low_level_lambda - Str.drop_prefix with empty prefix" {
    const src =
        \\x = Str.drop_prefix("hello", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.drop_prefix removes entire string" {
    const src =
        \\x = Str.drop_prefix("hello", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.drop_prefix prefix longer than string" {
    const src =
        \\x = Str.drop_prefix("hi", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hi\"", value);
}

// Str.drop_suffix tests

test "e_low_level_lambda - Str.drop_suffix removes matching suffix" {
    const src =
        \\x = Str.drop_suffix("hello world", " world")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.drop_suffix returns original when no match" {
    const src =
        \\x = Str.drop_suffix("hello world", " goodbye")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello world\"", value);
}

test "e_low_level_lambda - Str.drop_suffix with empty suffix" {
    const src =
        \\x = Str.drop_suffix("hello", "")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.drop_suffix removes entire string" {
    const src =
        \\x = Str.drop_suffix("hello", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.drop_suffix suffix longer than string" {
    const src =
        \\x = Str.drop_suffix("hi", "hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hi\"", value);
}
// U8 conversion tests

test "e_low_level_lambda - U8.to_i16 safe widening" {
    const src =
        \\a : U8
        \\a = 200u8
        \\x = U8.to_i16(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 200), value);
}

test "e_low_level_lambda - U8.to_i32 safe widening" {
    const src =
        \\a : U8
        \\a = 255u8
        \\x = U8.to_i32(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 255), value);
}

test "e_low_level_lambda - U8.to_i64 safe widening" {
    const src =
        \\a : U8
        \\a = 128u8
        \\x = U8.to_i64(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 128), value);
}

test "e_low_level_lambda - U8.to_i128 safe widening" {
    const src =
        \\a : U8
        \\a = 100u8
        \\x = U8.to_i128(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 100), value);
}

test "e_low_level_lambda - U8.to_u16 safe widening" {
    const src =
        \\a : U8
        \\a = 200u8
        \\x = U8.to_u16(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 200), value);
}

test "e_low_level_lambda - U8.to_u32 safe widening" {
    const src =
        \\a : U8
        \\a = 255u8
        \\x = U8.to_u32(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 255), value);
}

test "e_low_level_lambda - U8.to_u64 safe widening" {
    const src =
        \\a : U8
        \\a = 128u8
        \\x = U8.to_u64(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 128), value);
}

test "e_low_level_lambda - U8.to_u128 safe widening" {
    const src =
        \\a : U8
        \\a = 50u8
        \\x = U8.to_u128(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 50), value);
}

test "e_low_level_lambda - U8.to_i8_wrap in range" {
    const src =
        \\a : U8
        \\a = 100u8
        \\x = U8.to_i8_wrap(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 100), value);
}

test "e_low_level_lambda - U8.to_i8_wrap out of range wraps" {
    const src =
        \\a : U8
        \\a = 200u8
        \\x = U8.to_i8_wrap(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    // 200 as u8 wraps to -56 as i8 (200 - 256 = -56)
    try testing.expectEqual(@as(i128, -56), value);
}

test "e_low_level_lambda - U8.to_i8_try in range returns Ok" {
    const src =
        \\a : U8
        \\a = 100u8
        \\x = U8.to_i8_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(100)", value);
}

test "e_low_level_lambda - U8.to_i8_try out of range returns Err" {
    const src =
        \\a : U8
        \\a = 200u8
        \\x = U8.to_i8_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Err(OutOfRange)", value);
}

test "e_low_level_lambda - U8.to_f32" {
    const src =
        \\a : U8
        \\a = 42u8
        \\x = U8.to_f32(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expect(std.mem.startsWith(u8, value, "42"));
}

test "e_low_level_lambda - U8.to_f64" {
    const src =
        \\a : U8
        \\a = 255u8
        \\x = U8.to_f64(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expect(std.mem.startsWith(u8, value, "255"));
}

test "e_low_level_lambda - U8.to_dec" {
    const src =
        \\a : U8
        \\a = 123u8
        \\x = U8.to_dec(a)
        \\y = Dec.to_str(x)
    ;
    const value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"123.0\"", value);
}

// I8 conversion tests

test "e_low_level_lambda - I8.to_i16 safe widening positive" {
    const src =
        \\a : I8
        \\a = 100i8
        \\x = I8.to_i16(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 100), value);
}

test "e_low_level_lambda - I8.to_i16 safe widening negative" {
    const src =
        \\a : I8
        \\a = -50i8
        \\x = I8.to_i16(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -50), value);
}

test "e_low_level_lambda - I8.to_i32 safe widening" {
    const src =
        \\a : I8
        \\a = -128i8
        \\x = I8.to_i32(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -128), value);
}

test "e_low_level_lambda - I8.to_i64 safe widening" {
    const src =
        \\a : I8
        \\a = 127i8
        \\x = I8.to_i64(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 127), value);
}

test "e_low_level_lambda - I8.to_i128 safe widening" {
    const src =
        \\a : I8
        \\a = -1i8
        \\x = I8.to_i128(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -1), value);
}

test "e_low_level_lambda - I8.to_u8_wrap in range" {
    const src =
        \\a : I8
        \\a = 50i8
        \\x = I8.to_u8_wrap(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 50), value);
}

test "e_low_level_lambda - I8.to_u8_wrap negative wraps" {
    const src =
        \\a : I8
        \\a = -1i8
        \\x = I8.to_u8_wrap(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    // -1 as i8 wraps to 255 as u8
    try testing.expectEqual(@as(i128, 255), value);
}

test "e_low_level_lambda - I8.to_u8_try in range returns Ok" {
    const src =
        \\a : I8
        \\a = 100i8
        \\x = I8.to_u8_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(100)", value);
}

test "e_low_level_lambda - I8.to_u8_try negative returns Err" {
    const src =
        \\a : I8
        \\a = -10i8
        \\x = I8.to_u8_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Err(OutOfRange)", value);
}

test "e_low_level_lambda - I8.to_u16_wrap positive" {
    const src =
        \\a : I8
        \\a = 100i8
        \\x = I8.to_u16_wrap(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 100), value);
}

test "e_low_level_lambda - I8.to_u16_wrap negative wraps" {
    const src =
        \\a : I8
        \\a = -1i8
        \\x = I8.to_u16_wrap(a)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    // -1 as i8 sign-extends to u16 as 65535
    try testing.expectEqual(@as(i128, 65535), value);
}

test "e_low_level_lambda - I8.to_u16_try in range returns Ok" {
    const src =
        \\a : I8
        \\a = 50i8
        \\x = I8.to_u16_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(50)", value);
}

test "e_low_level_lambda - I8.to_u16_try negative returns Err" {
    const src =
        \\a : I8
        \\a = -5i8
        \\x = I8.to_u16_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Err(OutOfRange)", value);
}

test "e_low_level_lambda - I8.to_u32_try negative returns Err" {
    const src =
        \\a : I8
        \\a = -100i8
        \\x = I8.to_u32_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Err(OutOfRange)", value);
}

test "e_low_level_lambda - I8.to_u64_try positive returns Ok" {
    const src =
        \\a : I8
        \\a = 127i8
        \\x = I8.to_u64_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(127)", value);
}

test "e_low_level_lambda - I8.to_u128_try zero returns Ok" {
    const src =
        \\a : I8
        \\a = 0i8
        \\x = I8.to_u128_try(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(0)", value);
}

test "e_low_level_lambda - I8.to_f32 positive" {
    const src =
        \\a : I8
        \\a = 42i8
        \\x = I8.to_f32(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expect(std.mem.startsWith(u8, value, "42"));
}

test "e_low_level_lambda - I8.to_f64 negative" {
    const src =
        \\a : I8
        \\a = -100i8
        \\x = I8.to_f64(a)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expect(std.mem.startsWith(u8, value, "-100"));
}

test "e_low_level_lambda - I8.to_dec positive" {
    const src =
        \\a : I8
        \\a = 50i8
        \\x = I8.to_dec(a)
        \\y = Dec.to_str(x)
    ;
    const value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"50.0\"", value);
}

test "e_low_level_lambda - I8.to_dec negative" {
    const src =
        \\a : I8
        \\a = -25i8
        \\x = I8.to_dec(a)
        \\y = Dec.to_str(x)
    ;
    const value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"-25.0\"", value);
}

// count_utf8_bytes tests
test "e_low_level_lambda - Str.count_utf8_bytes empty string" {
    const src =
        \\x = Str.count_utf8_bytes("")
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 0), value);
}

test "e_low_level_lambda - Str.count_utf8_bytes ASCII string" {
    const src =
        \\x = Str.count_utf8_bytes("hello")
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 5), value);
}

test "e_low_level_lambda - Str.count_utf8_bytes multi-byte UTF-8" {
    const src =
        \\x = Str.count_utf8_bytes("é")
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 2), value);
}

test "e_low_level_lambda - Str.count_utf8_bytes emoji" {
    const src =
        \\x = Str.count_utf8_bytes("🎉")
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 4), value);
}

// with_capacity tests
test "e_low_level_lambda - Str.with_capacity returns empty string" {
    const src =
        \\x = Str.with_capacity(0)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.with_capacity with capacity returns empty string" {
    const src =
        \\x = Str.with_capacity(100)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

// reserve tests
test "e_low_level_lambda - Str.reserve preserves content" {
    const src =
        \\x = Str.reserve("hello", 100)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.reserve empty string" {
    const src =
        \\x = Str.reserve("", 50)
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

// release_excess_capacity tests
test "e_low_level_lambda - Str.release_excess_capacity preserves content" {
    const src =
        \\x = Str.release_excess_capacity("hello")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.release_excess_capacity empty string" {
    const src =
        \\x = Str.release_excess_capacity("")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

// to_utf8 tests (using List.len to verify)
test "e_low_level_lambda - Str.to_utf8 empty string" {
    const src =
        \\x = List.len(Str.to_utf8(""))
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 0), value);
}

test "e_low_level_lambda - Str.to_utf8 ASCII string" {
    const src =
        \\x = List.len(Str.to_utf8("hello"))
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 5), value);
}

test "e_low_level_lambda - Str.to_utf8 multi-byte UTF-8" {
    const src =
        \\x = List.len(Str.to_utf8("é"))
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 2), value);
}

// from_utf8_lossy tests (roundtrip through to_utf8)
test "e_low_level_lambda - Str.from_utf8_lossy roundtrip ASCII" {
    const src =
        \\x = Str.from_utf8_lossy(Str.to_utf8("hello"))
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.from_utf8_lossy roundtrip empty" {
    const src =
        \\x = Str.from_utf8_lossy(Str.to_utf8(""))
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.from_utf8_lossy roundtrip UTF-8" {
    const src =
        \\x = Str.from_utf8_lossy(Str.to_utf8("hello 🎉 world"))
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello 🎉 world\"", value);
}

// split_on tests
test "e_low_level_lambda - Str.split_on basic split count" {
    const src =
        \\x = List.len(Str.split_on("hello world", " "))
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 2), value);
}

test "e_low_level_lambda - Str.split_on basic split first element" {
    const src =
        \\parts = Str.split_on("hello world", " ")
        \\first = List.first(parts)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(\"hello\")", value);
}

test "e_low_level_lambda - Str.split_on multiple delimiters count" {
    const src =
        \\x = List.len(Str.split_on("a,b,c,d", ","))
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 4), value);
}

test "e_low_level_lambda - Str.split_on multiple delimiters first element" {
    const src =
        \\parts = Str.split_on("a,b,c,d", ",")
        \\first = List.first(parts)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(\"a\")", value);
}

test "e_low_level_lambda - Str.split_on no match" {
    const src =
        \\parts = Str.split_on("hello", "x")
        \\first = List.first(parts)
    ;
    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("Ok(\"hello\")", value);
}

test "e_low_level_lambda - Str.split_on empty string" {
    const src =
        \\x = List.len(Str.split_on("", ","))
    ;
    const value = try evalModuleAndGetInt(src, 0);
    try testing.expectEqual(@as(i128, 1), value);
}

// join_with tests
test "e_low_level_lambda - Str.join_with basic join" {
    const src =
        \\x = Str.join_with(["hello", "world"], " ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello world\"", value);
}

test "e_low_level_lambda - Str.join_with multiple elements" {
    const src =
        \\x = Str.join_with(["a", "b", "c", "d"], ",")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"a,b,c,d\"", value);
}

test "e_low_level_lambda - Str.join_with single element" {
    const src =
        \\x = Str.join_with(["hello"], "-")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello\"", value);
}

test "e_low_level_lambda - Str.join_with empty list" {
    const src =
        \\x = Str.join_with([], ",")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"\"", value);
}

test "e_low_level_lambda - Str.join_with roundtrip with split_on" {
    const src =
        \\x = Str.join_with(Str.split_on("hello world", " "), " ")
    ;
    const value = try evalModuleAndGetString(src, 0, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("\"hello world\"", value);
}

test "e_low_level_lambda - U8.plus basic" {
    const src =
        \\a : U8
        \\a = 5
        \\b : U8
        \\b = 3
        \\x : U8
        \\x = U8.plus(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 8), value);
}

test "e_low_level_lambda - U8.plus method call syntax" {
    const src =
        \\a : U8
        \\a = 5
        \\b : U8
        \\b = 3
        \\x : U8
        \\x = a.plus(b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 8), value);
}

// mod_by tests for integer types

test "e_low_level_lambda - U8.mod_by basic" {
    const src =
        \\a : U8
        \\a = 10
        \\b : U8
        \\b = 3
        \\x : U8
        \\x = U8.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 1), value);
}

test "e_low_level_lambda - U8.mod_by zero remainder" {
    const src =
        \\a : U8
        \\a = 10
        \\b : U8
        \\b = 5
        \\x : U8
        \\x = U8.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 0), value);
}

test "e_low_level_lambda - I8.mod_by positive positive" {
    const src =
        \\a : I8
        \\a = 10
        \\b : I8
        \\b = 3
        \\x : I8
        \\x = I8.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 1), value);
}

test "e_low_level_lambda - I8.mod_by negative positive" {
    const src =
        \\a : I8
        \\a = -10
        \\b : I8
        \\b = 3
        \\x : I8
        \\x = I8.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    // -10 mod 3 = 2 (Euclidean modulo: result has sign of divisor)
    try testing.expectEqual(@as(i128, 2), value);
}

test "e_low_level_lambda - I8.mod_by positive negative" {
    const src =
        \\a : I8
        \\a = 10
        \\b : I8
        \\b = -3
        \\x : I8
        \\x = I8.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    // 10 mod -3 = -2 (Euclidean modulo: result has sign of divisor)
    try testing.expectEqual(@as(i128, -2), value);
}

test "e_low_level_lambda - I8.mod_by negative negative" {
    const src =
        \\a : I8
        \\a = -10
        \\b : I8
        \\b = -3
        \\x : I8
        \\x = I8.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    // -10 mod -3 = -1 (Euclidean modulo: result has sign of divisor)
    try testing.expectEqual(@as(i128, -1), value);
}

test "e_low_level_lambda - U64.mod_by large numbers" {
    const src =
        \\a : U64
        \\a = 1000000
        \\b : U64
        \\b = 7
        \\x : U64
        \\x = U64.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 1), value);
}

test "e_low_level_lambda - I64.mod_by with zero result" {
    const src =
        \\a : I64
        \\a = 100
        \\b : I64
        \\b = 10
        \\x : I64
        \\x = I64.mod_by(a, b)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 0), value);
}

// List.sort_with tests

test "e_low_level_lambda - List.sort_with basic ascending sort" {
    const src =
        \\x = List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(1)", first_value);
}

test "e_low_level_lambda - List.sort_with preserves length" {
    const src =
        \\x = List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 5), len_value);
}

test "e_low_level_lambda - List.sort_with with larger list" {
    const src =
        \\x = List.sort_with([5, 2, 8, 1, 9, 3, 7, 4, 6], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(1)", first_value);
}

test "e_low_level_lambda - List.sort_with with two elements" {
    const src =
        \\x = List.sort_with([2, 1], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(1)", first_value);
}

test "e_low_level_lambda - List.sort_with descending order" {
    const src =
        \\x = List.sort_with([1, 3, 2], |a, b| if a > b LT else if a < b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    // Descending sort of [1, 3, 2] should give [3, 2, 1], first = 3
    try testing.expectEqualStrings("Ok(3)", first_value);
}

test "e_low_level_lambda - List.sort_with empty list" {
    const src =
        \\x : List(U64)
        \\x = List.sort_with([], |a, b| if a < b LT else if a > b GT else EQ)
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), len_value);
}

test "e_low_level_lambda - List.sort_with single element" {
    const src =
        \\x = List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(42)", first_value);
}

test "e_low_level_lambda - List.sort_with with duplicates" {
    // TODO: This test is skipped due to stack memory accumulation across eval() calls.
    // The interpreter stores return values in stack memory, and bindings hold references
    // to this memory. When evaluating multiple declarations (x, then first, then len),
    // the stack grows without being freed, eventually causing overflow.
    // See: https://github.com/roc-lang/roc/issues/XXXX (architectural issue)
    if (true) return error.SkipZigTest;

    const src =
        \\x = List.sort_with([3, 1, 2, 1, 3], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
        \\len = List.len(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(1)", first_value);

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 5), len_value);
}

test "e_low_level_lambda - List.sort_with already sorted" {
    const src =
        \\x = List.sort_with([1, 2, 3, 4, 5], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(1)", first_value);
}

test "e_low_level_lambda - List.sort_with reverse sorted" {
    const src =
        \\x = List.sort_with([5, 4, 3, 2, 1], |a, b| if a < b LT else if a > b GT else EQ)
        \\first = List.first(x)
    ;

    const first_value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(1)", first_value);
}
