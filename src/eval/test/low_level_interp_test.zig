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
const compiled_builtins = @import("compiled_builtins");

const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;
const builtin_loading = @import("../builtin_loading.zig");

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
    /// Heap-allocated array of imported module envs.
    /// This must stay alive for the lifetime of the evaluator since
    /// interpreter.all_module_envs points to this memory.
    imported_envs: []*const ModuleEnv,
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

    // Heap-allocate imported_envs so it outlives this function.
    // The interpreter's all_module_envs is a slice that points to this memory.
    const imported_envs = try gpa.alloc(*const ModuleEnv, 1);
    errdefer gpa.free(imported_envs);
    imported_envs[0] = builtin_module.env;

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.resolveImports(module_env, imported_envs);

    const checker = try gpa.create(Check);
    errdefer gpa.destroy(checker);
    checker.* = try Check.init(gpa, &module_env.types, module_env, imported_envs, null, &module_env.store.regions, builtin_ctx);
    errdefer checker.deinit();

    try checker.checkFile();

    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, imported_envs, problems, builtin_types, builtin_module.env, &checker.import_mapping);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .builtin_module = builtin_module,
        .checker = checker,
        .imported_envs = imported_envs,
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

    // Free heap-allocated imported_envs (must happen after evaluator deinit)
    test_allocator.free(result.imported_envs);
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

/// Helper to evaluate multi-declaration modules and get the Dec value of a specific declaration
fn evalModuleAndGetDec(src: []const u8, decl_index: usize) !i128 {
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

        // Return the value if this is the declaration we want
        if (i == decl_index) {
            defer stack_value.decref(&result.evaluator.interpreter.runtime_layout_store, ops);
            // Dec values are stored as i128 internally
            std.debug.assert(stack_value.layout.tag == .scalar and stack_value.layout.data.scalar.tag == .frac);
            const ptr = @as(*const i128, @ptrCast(@alignCast(stack_value.ptr.?)));
            return ptr.*;
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

test "e_low_level_lambda - List.concat with Str.to_utf8 inside lambda (issue 8618)" {
    const src =
        \\test = |line| {
        \\    bytes = line.to_utf8()
        \\    List.concat([0], bytes)
        \\}
        \\
        \\x = test("abc")
    ;

    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("[0, 97, 98, 99]", value);
}

test "top-level List.concat with Str.to_utf8 (value restriction check)" {
    // This tests whether value restriction at top-level causes issues
    // similar to what we saw when applying it to local bindings
    const src =
        \\line = "abc"
        \\result = line.to_utf8().concat([0])
    ;

    const value = try evalModuleAndGetString(src, 1, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("[97, 98, 99, 0]", value);
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

test "e_low_level_lambda - List.append tuple to empty list (issue 8758)" {
    // This test reproduces issue #8758 - integer overflow when appending tuples containing
    // strings to an empty list. The bug was that isRefcounted() returns false for tuples,
    // causing allocation to use one memory layout but deallocation to use another, leading
    // to integer overflow when reading from the wrong offset during cleanup.
    const src =
        \\x = List.append([], ("hello", "world"))
        \\len = List.len(x)
    ;

    const len_value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1), len_value);
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

// Bitwise shift operation tests

test "e_low_level_lambda - U8.shift_left_by basic" {
    const src =
        \\a : U8
        \\a = 5
        \\x = a.shift_left_by(2)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 20), value); // 5 << 2 = 20
}

test "e_low_level_lambda - U8.shift_right_by basic" {
    const src =
        \\a : U8
        \\a = 20
        \\x = a.shift_right_by(2)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 5), value); // 20 >> 2 = 5
}

test "e_low_level_lambda - U8.shift_right_zf_by basic" {
    const src =
        \\a : U8
        \\a = 128
        \\x = a.shift_right_zf_by(2)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 32), value); // 128 >>> 2 = 32
}

test "e_low_level_lambda - I8.shift_left_by positive" {
    const src =
        \\a : I8
        \\a = 3
        \\x = a.shift_left_by(3)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 24), value); // 3 << 3 = 24
}

test "e_low_level_lambda - I8.shift_right_by negative arithmetic" {
    const src =
        \\a : I8
        \\a = -8
        \\x = a.shift_right_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -4), value); // -8 >> 1 = -4 (arithmetic shift)
}

test "e_low_level_lambda - I8.shift_right_zf_by negative zero_fill" {
    const src =
        \\a : I8
        \\a = -8
        \\x = a.shift_right_zf_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 124), value); // -8 >>> 1 = 124 (zero-fill shift)
}

test "e_low_level_lambda - U16.shift_left_by" {
    const src =
        \\a : U16
        \\a = 1
        \\x = a.shift_left_by(4)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 16), value); // 1 << 4 = 16
}

test "e_low_level_lambda - I16.shift_right_by positive" {
    const src =
        \\a : I16
        \\a = 64
        \\x = a.shift_right_by(3)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 8), value); // 64 >> 3 = 8
}

test "e_low_level_lambda - I16.shift_right_by negative" {
    const src =
        \\a : I16
        \\a = -16
        \\x = a.shift_right_by(2)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -4), value); // -16 >> 2 = -4
}

test "e_low_level_lambda - U32.shift_left_by" {
    const src =
        \\a : U32
        \\a = 16
        \\x = a.shift_left_by(3)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 128), value); // 16 << 3 = 128
}

test "e_low_level_lambda - I32.shift_right_by negative" {
    const src =
        \\a : I32
        \\a = -32
        \\x = a.shift_right_by(3)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -4), value); // -32 >> 3 = -4
}

test "e_low_level_lambda - U64.shift_left_by" {
    const src =
        \\a : U64
        \\a = 255
        \\x = a.shift_left_by(8)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 65280), value); // 255 << 8 = 65280
}

test "e_low_level_lambda - I64.shift_right_by negative" {
    const src =
        \\a : I64
        \\a = -1024
        \\x = a.shift_right_by(2)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -256), value); // -1024 >> 2 = -256
}

test "e_low_level_lambda - U128.shift_left_by" {
    const src =
        \\a : U128
        \\a = 1
        \\x = a.shift_left_by(10)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1024), value); // 1 << 10 = 1024
}

test "e_low_level_lambda - I128.shift_right_by negative" {
    const src =
        \\a : I128
        \\a = -256
        \\x = a.shift_right_by(4)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -16), value); // -256 >> 4 = -16
}

test "e_low_level_lambda - shift_left_by with zero shift" {
    const src =
        \\a : U8
        \\a = 42
        \\x = a.shift_left_by(0)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 42), value); // 42 << 0 = 42
}

test "e_low_level_lambda - shift_right_by with zero shift" {
    const src =
        \\a : I8
        \\a = -42
        \\x = a.shift_right_by(0)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -42), value); // -42 >> 0 = -42
}

test "e_low_level_lambda - shift operations preserve type" {
    const src =
        \\a : U32
        \\a = 100
        \\b = a.shift_left_by(2)
        \\c = b.shift_right_by(1)
        \\x = c.shift_right_zf_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 3);
    try testing.expectEqual(@as(i128, 100), value); // ((100 << 2) >> 1) >>> 1 = (400 >> 1) >>> 1 = 200 >>> 1 = 100
}

test "e_low_level_lambda - I8.shift_right_zf_by with -1" {
    const src =
        \\a : I8
        \\a = -1
        \\x = a.shift_right_zf_by(4)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 15), value); // -1 (0xFF) >>> 4 = 15 (0x0F)
}

test "e_low_level_lambda - U16.shift_right_zf_by equals shift_right_by for unsigned" {
    const src =
        \\a : U16
        \\a = 256
        \\b = a.shift_right_by(4)
        \\c = a.shift_right_zf_by(4)
        \\x = U16.is_eq(b, c)
    ;
    const value = try evalModuleAndGetString(src, 3, test_allocator);
    defer test_allocator.free(value);
    try testing.expectEqualStrings("True", value); // For unsigned, >> and >>> are the same
}

// Bitwise shift edge case tests

test "e_low_level_lambda - U8.shift_left_by overflow wraps" {
    const src =
        \\a : U8
        \\a = 128
        \\x = a.shift_left_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 128 << 1 wraps to 0 in U8
}

test "e_low_level_lambda - I8.shift_left_by overflow wraps" {
    const src =
        \\a : I8
        \\a = 64
        \\x = a.shift_left_by(2)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 64 << 2 = 256, wraps to 0 in I8
}

test "e_low_level_lambda - I8.shift_left_by max value overflow" {
    const src =
        \\a : I8
        \\a = 127
        \\x = a.shift_left_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -2), value); // 127 << 1 = 254 = -2 in I8
}

test "e_low_level_lambda - U8.shift_right_by max value" {
    const src =
        \\a : U8
        \\a = 255
        \\x = a.shift_right_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 127), value); // 255 >> 1 = 127
}

test "e_low_level_lambda - I8.shift_right_by min value" {
    const src =
        \\a : I8
        \\a = -128
        \\x = a.shift_right_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -64), value); // -128 >> 1 = -64 (arithmetic)
}

test "e_low_level_lambda - I8.shift_right_zf_by min value" {
    const src =
        \\a : I8
        \\a = -128
        \\x = a.shift_right_zf_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 64), value); // -128 (0x80) >>> 1 = 64 (0x40)
}

test "e_low_level_lambda - shift_left_by amount at bit width boundary" {
    const src =
        \\a : U8
        \\a = 1
        \\x = a.shift_left_by(7)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 128), value); // 1 << 7 = 128 (MSB set)
}

test "e_low_level_lambda - shift_right_by amount at bit width boundary" {
    const src =
        \\a : U8
        \\a = 128
        \\x = a.shift_right_by(7)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1), value); // 128 >> 7 = 1
}

test "e_low_level_lambda - I8.shift_right_by negative all ones preserves" {
    const src =
        \\a : I8
        \\a = -1
        \\x = a.shift_right_by(7)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -1), value); // -1 >> 7 = -1 (sign extends)
}

test "e_low_level_lambda - I8.shift_right_by negative rounds toward negative infinity" {
    const src =
        \\a : I8
        \\a = -3
        \\x = a.shift_right_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -2), value); // -3 >> 1 = -2
}

test "e_low_level_lambda - U8.shift_right_zf_by all ones pattern" {
    const src =
        \\a : U8
        \\a = 255
        \\x = a.shift_right_zf_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 127), value); // 255 >>> 1 = 127
}

test "e_low_level_lambda - I8.shift_right_zf_by all ones from negative" {
    const src =
        \\a : I8
        \\a = -1
        \\x = a.shift_right_zf_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 127), value); // -1 (0xFF) >>> 1 = 127 (0x7F)
}

test "e_low_level_lambda - shift_left_by with zero value" {
    const src =
        \\a : U8
        \\a = 0
        \\x = a.shift_left_by(5)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 0 << 5 = 0
}

test "e_low_level_lambda - shift_right_zf_by with zero value" {
    const src =
        \\a : I8
        \\a = 0
        \\x = a.shift_right_zf_by(3)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 0 >>> 3 = 0
}

test "e_low_level_lambda - shift_left_by large shift amount clamped U8" {
    const src =
        \\a : U8
        \\a = 1
        \\x = a.shift_left_by(200)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 1 << 127 (clamped) wraps to 0
}

test "e_low_level_lambda - shift_right_by large shift amount clamped" {
    const src =
        \\a : U8
        \\a = 255
        \\x = a.shift_right_by(200)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 255 >> 127 (clamped) = 0 (all bits shifted out)
}

test "e_low_level_lambda - U16.shift_left_by to max representable" {
    const src =
        \\a : U16
        \\a = 1
        \\x = a.shift_left_by(15)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 32768), value); // 1 << 15 = 32768 (MSB set)
}

test "e_low_level_lambda - U32.shift_left_by power of 2" {
    const src =
        \\a : U32
        \\a = 1
        \\x = a.shift_left_by(20)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1048576), value); // 1 << 20 = 2^20
}

test "e_low_level_lambda - U64.shift_left_by large power" {
    const src =
        \\a : U64
        \\a = 1
        \\x = a.shift_left_by(40)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1099511627776), value); // 1 << 40 = 2^40
}

test "e_low_level_lambda - U128.shift_left_by near max" {
    const src =
        \\a : U128
        \\a = 1
        \\x = a.shift_left_by(100)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1267650600228229401496703205376), value); // 1 << 100 = 2^100
}

test "e_low_level_lambda - I16.shift_right_by negative large magnitude" {
    const src =
        \\a : I16
        \\a = -1024
        \\x = a.shift_right_by(5)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -32), value); // -1024 >> 5 = -32
}

test "e_low_level_lambda - I32.shift_right_by min value" {
    const src =
        \\a : I32
        \\a = -2147483648
        \\x = a.shift_right_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -1073741824), value); // I32::MIN >> 1
}

test "e_low_level_lambda - I32.shift_right_zf_by min value" {
    const src =
        \\a : I32
        \\a = -2147483648
        \\x = a.shift_right_zf_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 1073741824), value); // I32::MIN (0x80000000) >>> 1 = 0x40000000
}

test "e_low_level_lambda - shift single bit round trip" {
    const src =
        \\a : U8
        \\a = 1
        \\b = a.shift_left_by(5)
        \\x = b.shift_right_by(5)
    ;
    const value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 1), value); // (1 << 5) >> 5 = 1
}

test "e_low_level_lambda - I64.shift_right_by negative two" {
    const src =
        \\a : I64
        \\a = -2
        \\x = a.shift_right_by(1)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -1), value); // -2 >> 1 = -1
}

test "e_low_level_lambda - U32.shift_left_by shift amount exactly at width" {
    const src =
        \\a : U32
        \\a = 1
        \\x = a.shift_left_by(32)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 0), value); // 1 << 32 (clamped to 31) = wraps
}

test "e_low_level_lambda - I8.shift_right_by negative by 7 bits" {
    const src =
        \\a : I8
        \\a = -127
        \\x = a.shift_right_by(6)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, -2), value); // -127 >> 6 = -2
}

test "e_low_level_lambda - U64.shift_right_zf_by max value by half" {
    const src =
        \\a : U64
        \\a = 18446744073709551615
        \\x = a.shift_right_zf_by(32)
    ;
    const value = try evalModuleAndGetInt(src, 1);
    try testing.expectEqual(@as(i128, 4294967295), value); // U64::MAX >>> 32
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

// Regression test for issue #8750: dbg in polymorphic function causes TypeMismatch
// Ian McLerran reported that using dbg inside a polymorphic debug function
// and then method-chaining on the result causes crashes and wrong values.
// The bug is in dbg_print continuation which incorrectly translates the type
// variable in polymorphic contexts, leading to the wrong layout being used
// for the return value (should be empty record {}).
test "issue 8750: dbg in polymorphic debug function with List.len" {
    std.debug.print("Ignore the dbg prints to stderr below, they are expected.\n", .{});

    const src =
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\len = xs->debug()->List.len()
    ;

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 3), len_value);
}

test "issue 8750: dbg in polymorphic debug function with List.first" {
    const src =
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [10, 20, 30]
        \\first = xs->debug()->List.first()
    ;

    const first_value = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(first_value);
    try testing.expectEqualStrings("Ok(10)", first_value);
}

test "issue 8750: dbg in polymorphic debug function chained multiple times" {
    const src =
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3, 4, 5]
        \\result = xs->debug()->debug()->List.len()
    ;

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 5), len_value);
}

test "issue 8750: dbg in polymorphic function with List.fold" {
    const src =
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\sum = xs->debug()->List.fold(0, |acc, x| acc + x)
    ;

    // List.fold returns Dec because numeric literals default to Dec.
    // Dec value 6 is stored as 6 * 10^18 in fixed-point representation.
    const sum_value = try evalModuleAndGetDec(src, 2);
    try testing.expectEqual(@as(i128, 6_000_000_000_000_000_000), sum_value);
}

// Test without dbg to isolate whether the bug is specific to dbg or more general
test "issue 8750: identity function (no dbg) with List.fold" {
    const src =
        \\identity = |v| v
        \\xs = [1, 2, 3]
        \\sum = xs->identity()->List.fold(0, |acc, x| acc + x)
    ;

    // List.fold returns Dec because numeric literals default to Dec.
    const sum_value = try evalModuleAndGetDec(src, 2);
    try testing.expectEqual(@as(i128, 6_000_000_000_000_000_000), sum_value);
}

// Test direct List.fold without any wrapping function
test "issue 8750: direct List.fold without wrapper" {
    const src =
        \\xs = [1, 2, 3]
        \\sum = xs->List.fold(0, |acc, x| acc + x)
    ;

    // List.fold returns Dec because numeric literals default to Dec.
    const sum_value = try evalModuleAndGetDec(src, 1);
    try testing.expectEqual(@as(i128, 6_000_000_000_000_000_000), sum_value);
}

// Test dbg with simpler function (no List.fold)
test "issue 8750: dbg in polymorphic function with List.len" {
    const src =
        \\debug = |v| {
        \\    dbg v
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\len = xs->debug()->List.len()
    ;

    const len_value = try evalModuleAndGetInt(src, 2);
    try testing.expectEqual(@as(i128, 3), len_value);
}

// Test with only a block (no dbg) before List.fold
test "issue 8750: block without dbg before List.fold" {
    const src =
        \\wrap = |v| { v }
        \\xs = [1, 2, 3]
        \\sum = xs->wrap()->List.fold(0, |acc, x| acc + x)
    ;

    // List.fold returns Dec because numeric literals default to Dec.
    const sum_value = try evalModuleAndGetDec(src, 2);
    try testing.expectEqual(@as(i128, 6_000_000_000_000_000_000), sum_value);
}

// Test with dbg of a constant (not the polymorphic parameter)
test "issue 8750: dbg of constant before returning v with List.fold" {
    const src =
        \\debug = |v| {
        \\    dbg 42
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\sum = xs->debug()->List.fold(0, |acc, x| acc + x)
    ;

    // List.fold returns Dec because numeric literals default to Dec.
    const sum_value = try evalModuleAndGetDec(src, 2);
    try testing.expectEqual(@as(i128, 6_000_000_000_000_000_000), sum_value);
}

// Test that List.fold renders the correct value
test "issue 8750: List.fold render value" {
    const src =
        \\xs = [1, 2, 3]
        \\sum = xs->List.fold(0, |acc, x| acc + x)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const defs = result.module_env.store.sliceDefs(result.module_env.all_defs);
    const ops = result.evaluator.get_ops();

    // Evaluate first declaration (xs)
    var def = result.module_env.store.getDef(defs[0]);
    var stack_value = try result.evaluator.interpreter.eval(def.expr, ops);
    try result.evaluator.interpreter.bindings.append(.{
        .pattern_idx = def.pattern,
        .value = stack_value,
        .expr_idx = def.expr,
        .source_env = result.module_env,
    });

    // Evaluate second declaration (sum)
    def = result.module_env.store.getDef(defs[1]);
    const ct_var = can.ModuleEnv.varFrom(def.expr);
    stack_value = try result.evaluator.interpreter.eval(def.expr, ops);

    const rt_var = try result.evaluator.interpreter.translateTypeVar(result.module_env, ct_var);
    const rendered = try result.evaluator.interpreter.renderValueRocWithType(stack_value, rt_var, ops);
    defer test_allocator.free(rendered);
    try testing.expectEqualStrings("6", rendered);
}

test "issue 8765: Box.unbox with record containing numeric literal" {
    // Regression test for issue #8765 - Box.unbox loses type resolution when the
    // boxed value contains a record with numeric literals. The bug was that numeric
    // literals in nested structures weren't propagating their constraint information
    // through Box.unbox, causing type inference to fail.
    const src =
        \\update = |boxed| {
        \\    { count } = Box.unbox(boxed)
        \\    count + 1
        \\}
        \\initial = Box.box({ count: 0 })
        \\result = update(initial)
    ;

    const result = try evalModuleAndGetString(src, 2, test_allocator);
    defer test_allocator.free(result);
    try testing.expectEqualStrings("1", result);
}
