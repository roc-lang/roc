//! Tests for compile-time evaluation of top-level declarations
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const eval = @import("../mod.zig");
const compiled_builtins = @import("compiled_builtins");

const helpers = @import("helpers.zig");
const builtin_loading = eval.builtin_loading;
const ComptimeEvaluator = @import("../comptime_evaluator.zig").ComptimeEvaluator;
const BuiltinTypes = eval.BuiltinTypes;

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const testing = std.testing;
const test_allocator = testing.allocator;

/// Helper to parse, canonicalize, type-check, and run comptime evaluation on a full module
fn parseCheckAndEvalModule(src: []const u8) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    bool_module: builtin_loading.LoadedModule,
    result_module: builtin_loading.LoadedModule,
} {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = "TestModule";
    try module_env.common.calcLineStarts(module_env.gpa);

    // Parse the source code
    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Load real builtins (these will be returned and cleaned up by the caller)
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = builtin_indices.bool_type,
        .result_stmt = builtin_indices.result_type,
    };

    // Create canonicalizer
    var czer = try Can.init(module_env, &parse_ast, null);
    defer czer.deinit();

    // Canonicalize the module
    try czer.canonicalizeFile();

    // Type check the module
    var checker = try Check.init(gpa, &module_env.types, module_env, &.{}, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    // Create problem store for comptime evaluation
    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    // Create and run comptime evaluator with real builtins
    const builtin_types = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, &.{}, problems, builtin_types);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .bool_module = bool_module,
        .result_module = result_module,
    };
}

/// Helper to parse, canonicalize, type-check, and run comptime evaluation with imported modules
fn parseCheckAndEvalModuleWithImport(src: []const u8, import_name: []const u8, imported_module: *const ModuleEnv) !struct {
    module_env: *ModuleEnv,
    evaluator: ComptimeEvaluator,
    problems: *check.problem.Store,
    other_envs: []const *const ModuleEnv,
    bool_module: builtin_loading.LoadedModule,
    result_module: builtin_loading.LoadedModule,
} {
    const gpa = test_allocator;

    const module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);
    module_env.* = try ModuleEnv.init(gpa, src);
    errdefer module_env.deinit();

    module_env.common.source = src;
    module_env.module_name = "TestModule";
    try module_env.common.calcLineStarts(module_env.gpa);

    // Set up imports
    var module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    defer module_envs.deinit();
    try module_envs.put(import_name, imported_module);

    // Parse the source code
    var parse_ast = try parse.parse(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Load real builtins (these will be returned and cleaned up by the caller)
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    errdefer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    errdefer result_module.deinit();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(gpa, "test");
    const common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = builtin_indices.bool_type,
        .result_stmt = builtin_indices.result_type,
    };

    // Create canonicalizer with imports
    // Pass null since we don't use auto-imported types in this test
    var czer = try Can.init(module_env, &parse_ast, null);
    defer czer.deinit();

    // Canonicalize the module
    try czer.canonicalizeFile();

    // Set up other_envs for type checking
    var other_envs_list = std.array_list.Managed(*const ModuleEnv).init(gpa);
    defer other_envs_list.deinit();
    try other_envs_list.append(imported_module);

    // Type check the module
    var checker = try Check.init(gpa, &module_env.types, module_env, other_envs_list.items, &module_env.store.regions, common_idents);
    defer checker.deinit();

    try checker.checkFile();

    // Create problem store for comptime evaluation
    const problems = try gpa.create(check.problem.Store);
    problems.* = .{};

    // Keep other_envs alive
    const other_envs_slice = try gpa.dupe(*const ModuleEnv, other_envs_list.items);

    // Create and run comptime evaluator with real builtins
    const builtin_types = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    const evaluator = try ComptimeEvaluator.init(gpa, module_env, other_envs_slice, problems, builtin_types);

    return .{
        .module_env = module_env,
        .evaluator = evaluator,
        .problems = problems,
        .other_envs = other_envs_slice,
        .bool_module = bool_module,
        .result_module = result_module,
    };
}

fn cleanupEvalModule(result: anytype) void {
    // ComptimeEvaluator deinit frees crash message strings
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);

    // Clean up builtin modules
    var bool_module_mut = result.bool_module;
    bool_module_mut.deinit();
    var result_module_mut = result.result_module;
    result_module_mut.deinit();
}

fn cleanupEvalModuleWithImport(result: anytype) void {
    // ComptimeEvaluator deinit frees crash message strings
    var evaluator_mut = result.evaluator;
    evaluator_mut.deinit();

    var problems_mut = result.problems;
    problems_mut.deinit(test_allocator);
    test_allocator.destroy(result.problems);
    test_allocator.free(result.other_envs);
    result.module_env.deinit();
    test_allocator.destroy(result.module_env);

    // Clean up builtin modules
    var bool_module_mut = result.bool_module;
    bool_module_mut.deinit();
    var result_module_mut = result.result_module;
    result_module_mut.deinit();
}

test "comptime eval - simple constant" {
    const src = "x = 42";

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 1 declaration with no crashes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "comptime eval - crash in constant" {
    const src =
        \\x = 42
        \\bad = {
        \\    crash "this crashes at compile time"
        \\    0
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with 1 crash
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);

    // Should have 1 problem reported
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - crash in if branch not taken" {
    const src =
        \\x = if True 42 else {
        \\    crash "not taken"
        \\    0
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate successfully - crash branch not taken
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "comptime eval - crash in if branch taken" {
    const src =
        \\x = if False 42 else {
        \\    crash "this branch is taken"
        \\    0
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should crash - else branch is taken
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
}

test "comptime eval - lambda is skipped" {
    const src =
        \\add = |x, y| x + y
        \\value = 42
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate 2 declarations with no crashes
    // The lambda should be skipped, not evaluated
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
}

test "comptime eval - multiple declarations with mixed results" {
    const src =
        \\good1 = 42
        \\bad1 = {
        \\    crash "first crash"
        \\    0
        \\}
        \\good2 = 100
        \\bad2 = {
        \\    crash "second crash"
        \\    0
        \\}
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 5 declarations with 2 crashes
    // All defs are evaluated regardless of crashes in other defs
    try testing.expectEqual(@as(u32, 5), summary.evaluated);
    try testing.expectEqual(@as(u32, 2), summary.crashed);

    // Should have 2 problems reported (one for each crash)
    try testing.expectEqual(@as(usize, 2), result.problems.len());
}

// Cross-module tests

test "comptime eval - cross-module constant works" {
    // Module A exports a constant
    const src_a =
        \\module [value]
        \\
        \\value = 42
    ;

    var result_a = try parseCheckAndEvalModule(src_a);
    defer cleanupEvalModule(&result_a);

    const summary_a = try result_a.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 1), summary_a.evaluated);
    try testing.expectEqual(@as(u32, 0), summary_a.crashed);

    // Module B imports and uses the constant
    const src_b =
        \\module []
        \\
        \\import A
        \\
        \\doubled = A.value + A.value
    ;

    var result_b = try parseCheckAndEvalModuleWithImport(src_b, "A", result_a.module_env);
    defer cleanupEvalModuleWithImport(&result_b);

    const summary_b = try result_b.evaluator.evalAll();

    // Cross-module comptime evaluation is now supported
    // The constant in module B should evaluate successfully using module A's value
    try testing.expectEqual(@as(u32, 1), summary_b.evaluated);
    try testing.expectEqual(@as(u32, 0), summary_b.crashed);
}

test "comptime eval - cross-module crash is detected" {
    // Module A exports a constant that crashes
    const src_a =
        \\module [crashy]
        \\
        \\crashy = {
        \\    crash "crash from module A"
        \\    0
        \\}
    ;

    var result_a = try parseCheckAndEvalModule(src_a);
    defer cleanupEvalModule(&result_a);

    const summary_a = try result_a.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 1), summary_a.evaluated);
    try testing.expectEqual(@as(u32, 1), summary_a.crashed);

    // Module B imports and uses the crashing constant
    const src_b =
        \\module []
        \\
        \\import A
        \\
        \\usesCrashy = A.crashy + 1
    ;

    var result_b = try parseCheckAndEvalModuleWithImport(src_b, "A", result_a.module_env);
    defer cleanupEvalModuleWithImport(&result_b);

    const summary_b = try result_b.evaluator.evalAll();

    // The expression in module B should crash because it evaluates A.crashy + 1
    // Cross-module comptime evaluation is now supported
    try testing.expectEqual(@as(u32, 1), summary_b.evaluated);
    try testing.expectEqual(@as(u32, 1), summary_b.crashed);
}

test "comptime eval - unexposed constant cannot be accessed" {
    // Module A has an unexposed constant
    const src_a =
        \\module [value]
        \\
        \\value = 42
        \\secret = 100
    ;

    var result_a = try parseCheckAndEvalModule(src_a);
    defer cleanupEvalModule(&result_a);

    const summary_a = try result_a.evaluator.evalAll();
    try testing.expectEqual(@as(u32, 2), summary_a.evaluated);
    try testing.expectEqual(@as(u32, 0), summary_a.crashed);

    // Module B tries to use exposing syntax to import the unexposed constant
    // This should generate a diagnostic during canonicalization because secret is not in A's exposure list
    const src_b =
        \\module []
        \\
        \\import A exposing [value, secret]
        \\
        \\x = value + secret
    ;

    // This should succeed (no error thrown) but generate a diagnostic
    var result_b = try parseCheckAndEvalModuleWithImport(src_b, "A", result_a.module_env);
    defer cleanupEvalModuleWithImport(&result_b);

    // Check that a value_not_exposed diagnostic was generated
    const diagnostics = try result_b.module_env.getDiagnostics();
    defer test_allocator.free(diagnostics);

    var found_value_not_exposed = false;
    for (diagnostics) |diagnostic| {
        if (diagnostic == .value_not_exposed) {
            const value_name = result_b.module_env.getIdent(diagnostic.value_not_exposed.value_name);
            if (std.mem.eql(u8, value_name, "secret")) {
                found_value_not_exposed = true;
            }
        }
    }

    try testing.expect(found_value_not_exposed);
}

test "comptime eval - expect success does not report" {
    const src =
        \\x = {
        \\    expect 1 == 1
        \\    42
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate successfully - expect passes
    try testing.expectEqual(@as(u32, 1), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - expect failure is reported but does not halt within def" {
    const src =
        \\x = {
        \\    expect 1 == 2
        \\    42
        \\}
        \\y = {
        \\    _before = 1
        \\    expect True == False
        \\    _after = 2
        \\    100
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both declarations with no crashes but 2 expect failures
    // expect never halts execution - even within the same def
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 2 problems reported (expect failures)
    try testing.expectEqual(@as(usize, 2), result.problems.len());

    // Verify both are expect_failed problems
    try testing.expect(result.problems.problems.items[0] == .comptime_expect_failed);
    try testing.expect(result.problems.problems.items[1] == .comptime_expect_failed);
}

test "comptime eval - multiple expect failures are reported" {
    const src =
        \\x = {
        \\    expect 1 == 2
        \\    42
        \\}
        \\y = {
        \\    expect True == False
        \\    100
        \\}
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both declarations with no crashes but 2 expect failures
    // All defs are evaluated regardless of expect failures in other defs
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 2 problems reported (one for each expect failure)
    try testing.expectEqual(@as(usize, 2), result.problems.len());

    // Verify both are expect_failed problems
    try testing.expect(result.problems.problems.items[0] == .comptime_expect_failed);
    try testing.expect(result.problems.problems.items[1] == .comptime_expect_failed);
}

test "comptime eval - crash does not halt other defs" {
    const src =
        \\good1 = 42
        \\bad = {
        \\    crash "this crashes"
        \\    0
        \\}
        \\good2 = 100
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 4 declarations even though one crashes
    // Crashes only halt within a single def, not across defs
    try testing.expectEqual(@as(u32, 4), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);

    // Should have 1 problem reported
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - expect failure does not halt evaluation" {
    const src =
        \\good1 = 42
        \\bad = {
        \\    expect 1 == 2
        \\    0
        \\}
        \\good2 = 100
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 4 declarations even though one has expect failure
    // expect never halts evaluation - not within defs, not across defs
    try testing.expectEqual(@as(u32, 4), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);

    // Should have 1 problem reported (expect failure)
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - dbg does not halt evaluation" {
    const src =
        \\good1 = 42
        \\good2 = 100
        \\good3 = 200
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // All declarations should be evaluated - no crashes or halts
    try testing.expectEqual(@as(u32, 3), summary.evaluated);
    try testing.expectEqual(@as(u32, 0), summary.crashed);
    try testing.expectEqual(@as(usize, 0), result.problems.len());
}

test "comptime eval - crash in first def does not halt other defs" {
    const src =
        \\bad = crash "immediate crash"
        \\good1 = 42
        \\good2 = 100
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate all 3 declarations even though the first one crashes
    // Crashes only halt within a single def, not across defs
    try testing.expectEqual(@as(u32, 3), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}

test "comptime eval - crash halts within single def" {
    // This test verifies that when a crash occurs inside a def,
    // evaluation of the rest of that def stops (within-def halting),
    // but other defs continue to be evaluated
    const src =
        \\x = {
        \\    _beforeCrash = 1
        \\    crash "halt here"
        \\    _afterCrash = 2  # This should never be evaluated
        \\    42
        \\}
        \\y = 100  # But this should still be evaluated
    ;

    var result = try parseCheckAndEvalModule(src);
    defer cleanupEvalModule(&result);

    const summary = try result.evaluator.evalAll();

    // Should evaluate both defs even though x crashes
    try testing.expectEqual(@as(u32, 2), summary.evaluated);
    try testing.expectEqual(@as(u32, 1), summary.crashed);
    try testing.expectEqual(@as(usize, 1), result.problems.len());
}
