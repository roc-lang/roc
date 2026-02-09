//! Tests for e_anno_only expression evaluation in the interpreter
//!
//! These tests verify that standalone type annotations (which don't have implementations)
//! are handled correctly by the interpreter - they crash immediately when accessed or called,
//! regardless of whether they are function types or value types.

const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const compiled_builtins = @import("compiled_builtins");

const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;
const builtin_loading = @import("../builtin_loading.zig");
const roc_target = @import("roc_target");

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;
const testing = std.testing;
// Use page_allocator for interpreter tests (doesn't track leaks)
const test_allocator = std.heap.page_allocator;

fn parseCheckAndEvalModule(src: []const u8) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    builtin_module: builtin_loading.LoadedModule,
    checker: *Check,
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

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const parse_ast = try parse.parse(&allocators, &module_env.common);
    defer parse_ast.deinit();

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

    // Create module_envs map for canonicalization (enables qualified calls to List, Str, etc.)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs_map.deinit();

    // Use shared function to populate ALL builtin types - ensures Builtin.roc is single source of truth
    try Can.populateModuleEnvs(
        &module_envs_map,
        module_env,
        builtin_module.env,
        builtin_indices,
    );

    var czer = try Can.init(&allocators, module_env, parse_ast, &module_envs_map);
    defer czer.deinit();

    try czer.canonicalizeFile();

    // Heap-allocate imported_envs so it outlives this function.
    // Order must match all_module_envs in the interpreter (self module first, then imports).
    // evalLookupExternal uses all_module_envs[resolved_idx], so resolveImports indices
    // must match this array. The interpreter detects other_envs[0]==env and uses it directly.
    const imported_envs = try gpa.alloc(*const ModuleEnv, 2);
    errdefer gpa.free(imported_envs);
    imported_envs[0] = module_env;
    imported_envs[1] = builtin_module.env;

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.resolveImports(module_env, imported_envs);

    const checker = try gpa.create(Check);
    errdefer gpa.destroy(checker);
    checker.* = try Check.init(gpa, &module_env.types, module_env, imported_envs, null, &module_env.store.regions, builtin_ctx);
    errdefer checker.deinit();

    try checker.checkFile();

    const problems = try gpa.create(check.problem.Store);
    errdefer gpa.destroy(problems);
    problems.* = try check.problem.Store.init(gpa);

    const builtin_types = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, imported_envs, problems, builtin_types, builtin_module.env, &checker.import_mapping, roc_target.RocTarget.detectNative());

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

    var checker_mut = result.checker;
    checker_mut.deinit();
    test_allocator.destroy(result.checker);

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);

    test_allocator.free(result.imported_envs);

    var builtin_module_mut = result.builtin_module;
    builtin_module_mut.deinit();
}

test "e_anno_only - function crashes when called directly" {
    const src =
        \\foo : Str -> Str
        \\x = foo("test")
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (the call to foo should crash)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - non-function crashes when accessed" {
    const src =
        \\bar : Str
        \\x = bar
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (accessing bar should crash)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - function only crashes when called (True branch)" {
    const src =
        \\foo : Str -> Str
        \\x = if True {
        \\    foo("test")
        \\} else {
        \\    "not called"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (foo is called in True branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - function only crashes when called (False branch)" {
    const src =
        \\foo : Str -> Str
        \\x = if False {
        \\    foo("test")
        \\} else {
        \\    "not called"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 0 crashes (foo is NOT called in False branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "e_anno_only - value only crashes when accessed (True branch)" {
    const src =
        \\bar : Str
        \\x = if True {
        \\    bar
        \\} else {
        \\    "not accessed"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash (bar is accessed in True branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "e_anno_only - value only crashes when accessed (False branch)" {
    const src =
        \\bar : Str
        \\x = if False {
        \\    bar
        \\} else {
        \\    "not accessed"
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 0 crashes (bar is NOT accessed in False branch)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.first on nonempty list" {
    const src =
        \\result = List.first([1, 2, 3])
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with 0 crashes (List.first should succeed)
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.get with valid index returns Ok" {
    const src =
        \\result = List.get([1, 2, 3], 1)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with 0 crashes (List.get should succeed)
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.get with invalid index returns Err" {
    const src =
        \\result = List.get([1, 2, 3], 10)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with 0 crashes (List.get should return Err but not crash)
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.get on empty list returns Err" {
    const src =
        \\empty : List(U64)
        \\empty = []
        \\result = List.get(empty, 0)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 0 crashes (List.get should return Err but not crash)
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.get with different element types - Str" {
    const src =
        \\result = List.get(["foo", "bar", "baz"], 1)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with 0 crashes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.get with different element types - Bool" {
    const src =
        \\result = List.get([True, False, True], 2)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with 0 crashes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "List.get with nested lists" {
    const src =
        \\result = List.get([[1, 2], [3, 4], [5, 6]], 1)
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with 0 crashes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}
